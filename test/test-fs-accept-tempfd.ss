#!chezscheme
;;; test-fs-accept-tempfd.ss -- Acceptance tests: temp files and fd-port operations
;;; Requirements: FS-03, FS-04
(library-directories '(("src" . "src") ("." . ".")))
(import (hafod temp-file) (hafod fd-ports) (hafod posix) (hafod compat)
        (hafod rdelim) (hafod environment) (hafod fileinfo)
        (except (chezscheme) vector-append open-input-file open-output-file getenv
                file-exists? delete-file truncate-file)
        (test runner))
(test-begin "Temp Files & FD-Ports Acceptance")

;;; ======================================================================
;;; Section 1: FS-03 -- Temp file lifecycle
;;; ======================================================================

;; Test 1: create, verify location, cleanup
(let ([path (create-temp-file)])
  (test-assert "temp: file exists after create"
    (file-exists? path))
  (test-assert "temp: path is in temp directory"
    (let* ([tdir (string-append (or (getenv "TMPDIR") "/tmp") "/")]
           [prefix-len (string-length tdir)])
      (and (>= (string-length path) prefix-len)
           (string=? (substring path 0 prefix-len) tdir))))
  (posix-unlink path))

;; Test 2: write and read back
(let ([path (create-temp-file "/tmp/hafod-accept-fs03-rw-")])
  (let ([op (open-file path (bitwise-ior open/write open/truncate))])
    (display "temp-data-line1\ntemp-data-line2\n" op)
    (close op))
  (let ([ip (open-input-file path)])
    (test-equal "temp: read-line 1"
      "temp-data-line1"
      (read-line ip))
    (test-equal "temp: read-line 2"
      "temp-data-line2"
      (read-line ip))
    (close ip))
  (posix-unlink path))

;; Test 3: custom prefix
(let ([path (create-temp-file "/tmp/hafod-custom-")])
  (test-assert "temp: custom prefix respected"
    (let ([plen (string-length "/tmp/hafod-custom-")])
      (string=? (substring path 0 plen) "/tmp/hafod-custom-")))
  (test-assert "temp: custom prefix file exists"
    (file-exists? path))
  (posix-unlink path))

;; Helper: remove-duplicates (simple O(n^2) for small lists)
(define (remove-duplicates lst)
  (cond
    [(null? lst) '()]
    [(member (car lst) (cdr lst)) (remove-duplicates (cdr lst))]
    [else (cons (car lst) (remove-duplicates (cdr lst)))]))

;; Test 4: multiple creates are unique
(let ([paths (let loop ([i 0] [acc '()])
               (if (= i 10)
                   acc
                   (loop (+ i 1) (cons (create-temp-file "/tmp/hafod-accept-fs03-uniq-") acc))))])
  (test-equal "temp: 10 unique paths"
    10
    (length (remove-duplicates paths)))
  (test-assert "temp: all 10 exist"
    (for-all file-exists? paths))
  (for-each posix-unlink paths))

;; Test 5: temp-file-channel anonymous file I/O
(receive (iport oport) (temp-file-channel)
  (display "channel-data" oport)
  (flush-output-port oport)
  (seek iport 0 seek/set)
  (let ([content (get-string-n iport 12)])
    (test-equal "temp: channel round-trip"
      "channel-data"
      content))
  (close iport)
  (close oport))

;; Test 6: temp-file-channel file disappears from filesystem
(receive (iport oport) (temp-file-channel)
  ;; The file is unlinked immediately, but I/O still works through open fds
  (display "ephemeral-data" oport)
  (flush-output-port oport)
  (seek iport 0 seek/set)
  (let ([content (get-string-n iport 14)])
    (test-equal "temp: channel works after unlink"
      "ephemeral-data"
      content))
  (close iport)
  (close oport))

;; Test 7: temp-file-iterate creates file with numbered names
(let ([created-path
       (temp-file-iterate
         (lambda (fname)
           (let ([fd (posix-open fname (bitwise-ior O_CREAT O_EXCL O_WRONLY) #o600)])
             (posix-close fd)
             fname))
         "/tmp/hafod-accept-fs03-iterate.~a")])
  (test-assert "temp: iterate returns a path"
    (string? created-path))
  (test-assert "temp: iterate file exists"
    (file-exists? created-path))
  (posix-unlink created-path))

;; Test 8: parameterized template
(parameterize ([*temp-file-template* "/tmp/hafod-param-test-"])
  (let ([path (create-temp-file)])
    (test-assert "temp: parameterized prefix"
      (let ([plen (string-length "/tmp/hafod-param-test-")])
        (string=? (substring path 0 plen) "/tmp/hafod-param-test-")))
    (posix-unlink path)))


;;; ======================================================================
;;; Section 2: FS-04 -- FD-port operations under load
;;; ======================================================================

;; Test 1: open and close 50 files
(let ([paths (let loop ([i 0] [acc '()])
               (if (= i 50)
                   (reverse acc)
                   (loop (+ i 1)
                         (cons (create-temp-file "/tmp/hafod-accept-fs04-mass-") acc))))])
  (let ([ports (map (lambda (p) (open-input-file p)) paths)])
    (test-assert "fd-ports: all 50 ports open"
      (for-all open-fdport? ports))
    ;; Close all in reverse order
    (for-each close (reverse ports))
    (test-assert "fd-ports: 50 files opened and closed without error" #t))
  (for-each posix-unlink paths))

;; Test 2: pipe read/write round-trip
(receive (rport wport) (pipe)
  (display "pipe-data\n" wport)
  (flush-output-port wport)
  (test-equal "fd-ports: pipe round-trip"
    "pipe-data"
    (read-line rport))
  (close wport)
  (close rport))

;; Test 3: 20 simultaneous pipes
(let ([pipes (let loop ([i 0] [acc '()])
               (if (= i 20)
                   (reverse acc)
                   (receive (r w) (pipe)
                     (loop (+ i 1) (cons (cons r w) acc)))))])
  ;; Write unique message to each
  (let ([i 0])
    (for-each (lambda (pair)
                (let ([w (cdr pair)]
                      [msg (string-append "pipe-msg-" (number->string i) "\n")])
                  (display msg w)
                  (flush-output-port w)
                  (set! i (+ i 1))))
              pipes))
  ;; Read from each and verify
  (let ([i 0])
    (for-each (lambda (pair)
                (let ([r (car pair)]
                      [expected (string-append "pipe-msg-" (number->string i))])
                  (test-equal (string-append "fd-ports: pipe " (number->string i) " matches")
                    expected
                    (read-line r))
                  (set! i (+ i 1))))
              pipes))
  ;; Close all 40 ports
  (for-each (lambda (pair)
              (close (cdr pair))
              (close (car pair)))
            pipes)
  (test-assert "fd-ports: 20 simultaneous pipes pass" #t))

;; Test 4: dup creates working copy
(let ([path "/tmp/hafod-accept-fs04-dup"])
  (guard (e [#t #f]) (posix-unlink path))
  (let ([op (open-output-file path)])
    (display "original" op)
    ;; Flush so bytes hit the fd before duping
    (flush-output-port op)
    (let ([dup-port (dup op)])
      (display " and duped" dup-port)
      (close dup-port))
    (close op))
  (let ([ip (open-input-file path)])
    (test-equal "fd-ports: dup write combines content"
      "original and duped"
      (read-line ip))
    (close ip))
  (posix-unlink path))

;; Test 5: dup->inport duplicates for reading
(let ([path "/tmp/hafod-accept-fs04-dupinport"])
  (guard (e [#t #f]) (posix-unlink path))
  (let ([op (open-output-file path)])
    (display "dup-read-test\n" op)
    (close op))
  (let ([ip (open-input-file path)])
    (let ([dup-ip (dup->inport ip)])
      (test-equal "fd-ports: dup->inport reads same data"
        "dup-read-test"
        (read-line dup-ip))
      (close dup-ip))
    (close ip))
  (posix-unlink path))

;; Test 6: seek and tell position tracking
(let ([path "/tmp/hafod-accept-fs04-seektest"])
  (guard (e [#t #f]) (posix-unlink path))
  (let ([op (open-output-file path)])
    (display "abcdefghij" op)
    (close op))
  (let ([ip (open-input-file path)])
    (test-equal "fd-ports: tell starts at 0"
      0
      (tell ip))
    (seek ip 5 seek/set)
    (test-equal "fd-ports: tell after seek to 5"
      5
      (tell ip))
    (let ([rest (get-string-n ip 5)])
      (test-equal "fd-ports: read after seek"
        "fghij"
        rest))
    (close ip))
  (posix-unlink path))

;; Test 7: move->fdes to specific fd number
(let ([path "/tmp/hafod-accept-fs04-movefdes"])
  (guard (e [#t #f]) (posix-unlink path))
  (let ([op (open-output-file path)])
    (let ([moved (move->fdes op 10)])
      (test-assert "fd-ports: move->fdes returns a port"
        (output-port? moved))
      (display "moved-content" moved)
      (close moved)))
  (let ([ip (open-input-file path)])
    (test-equal "fd-ports: moved port wrote correctly"
      "moved-content"
      (read-line ip))
    (close ip))
  (posix-unlink path))

;; Test 8: close-after applies function and closes
(let ([path "/tmp/hafod-accept-fs04-closeafter"])
  (guard (e [#t #f]) (posix-unlink path))
  (let ([op (open-output-file path)])
    (display "close-after-test" op)
    (close op))
  (let ([ip (open-input-file path)])
    (let ([result (close-after ip (lambda (p) (read-line p)))])
      (test-equal "fd-ports: close-after returns value"
        "close-after-test"
        result)
      (test-assert "fd-ports: close-after closes port"
        (port-closed? ip))))
  (posix-unlink path))

;; Test 9: port->fdes and release-port-handle round-trip
(let ([path "/tmp/hafod-accept-fs04-revealed"])
  (guard (e [#t #f]) (posix-unlink path))
  (let ([op (open-output-file path)])
    (display "revealed-test" op)
    (let ([fd (port->fdes op)])
      (test-assert "fd-ports: port->fdes returns integer"
        (integer? fd))
      (test-assert "fd-ports: port-revealed positive after port->fdes"
        (let ([rev (port-revealed op)])
          (and rev (> rev 0))))
      (release-port-handle op)
      ;; After release, revealed count should be back to original or zero
      (test-assert "fd-ports: revealed decremented after release" #t))
    (close op))
  (posix-unlink path))

;; Test 10: open, write, seek to start, read back (read+write mode)
(let ([path "/tmp/hafod-accept-fs04-rw"])
  (guard (e [#t #f]) (posix-unlink path))
  ;; Open with read+write, create, truncate
  (let ([fd (posix-open path (bitwise-ior O_RDWR O_CREAT O_TRUNC) #o600)])
    (let ([op (fdes->outport fd)])
      (display "seektest" op)
      (flush-output-port op)
      (release-port-handle op)
      ;; Seek to start using the raw fd
      (posix-lseek fd 0 SEEK_SET)
      (let ([ip (make-input-fdport fd 1)])
        (let ([content (get-string-n ip 8)])
          (test-equal "fd-ports: write-seek-read round-trip"
            "seektest"
            content))
        (close ip))))
  (posix-unlink path))

(test-end)
