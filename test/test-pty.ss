(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod pty)
        (hafod fd-ports)
        (hafod posix)
        (only (hafod procobj) proc? wait)
        (except (chezscheme) open-input-file open-output-file))

(test-begin "Pseudo-Terminals")

;; ======================================================================
;; open-pty
;; ======================================================================

(test-assert "open-pty returns two values"
  (let-values ([(master slave-name) (open-pty)])
    (let ([result (and (port? master) (string? slave-name))])
      (close master)
      result)))

(test-assert "open-pty master is an input port"
  (let-values ([(master slave-name) (open-pty)])
    (let ([result (input-port? master)])
      (close master)
      result)))

(test-assert "open-pty slave name starts with /dev/"
  (let-values ([(master slave-name) (open-pty)])
    (let ([result (and (>= (string-length slave-name) 5)
                       (string=? "/dev/" (substring slave-name 0 5)))])
      (close master)
      result)))

(test-assert "open-pty slave device file exists"
  (let-values ([(master slave-name) (open-pty)])
    (let ([result (file-exists? slave-name)])
      (close master)
      result)))

(test-assert "open-pty master fd is valid (can dup->outport for writing)"
  (let-values ([(master slave-name) (open-pty)])
    (let ([out (dup->outport master)])
      (let ([result (output-port? out)])
        (close out)
        (close master)
        result))))

;; ======================================================================
;; PTY communication
;; ======================================================================

(test-assert "data written to master can be read from slave"
  (let-values ([(master slave-name) (open-pty)])
    (let ([master-out (dup->outport master)]
          ;; Open the slave read-only so it is a genuine input port: writing to
          ;; the master delivers the bytes to the slave's input queue, and
          ;; reading the slave reads them straight back.
          [slave-in (open-file slave-name open/read)])
      ;; Write to master side
      (display "hello\n" master-out)
      (flush-output-port master-out)
      ;; Read from the slave side.  Writing "hello\n" to the master delivers
      ;; those bytes to the slave's input queue, and reading the slave returns
      ;; them directly -- the first byte is #\h.  Echo is not involved on this
      ;; read path: echo would affect the slave's output (the master read side),
      ;; not the slave's input that we read here, so the read behaves the same
      ;; with echo on or off.
      ;; A dead or broken PTY now fails loudly: any read error or EOF yields #f,
      ;; and only that first byte (#\h), seen within a bounded number of reads,
      ;; counts as success.  Read latency is tolerated up to that bound (the
      ;; suite kill-timeout caps any residual stall); reading stops the instant
      ;; the sentinel arrives so no further read-char can block.
      (let ([result
             (guard (e [#t #f])
               (let loop ([count 0])
                 (if (>= count 16)
                     #f
                     (let ([ch (read-char slave-in)])
                       (cond
                         [(eof-object? ch) #f]
                         [(char=? ch #\h) #t]
                         [else (loop (+ count 1))])))))])
        (close master-out)
        (close slave-in)
        (close master)
        result))))

;; Helper for string-contains
(define (string-contains haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

;; ======================================================================
;; fork-pty-session
;; ======================================================================

(test-assert "fork-pty-session returns two values (pty-port and proc)"
  (let-values ([(pty-port proc) (fork-pty-session
                                  (lambda ()
                                    (display "test\n")
                                    (flush-output-port (current-output-port))))])
    (let ([result (and (port? pty-port) (proc? proc))])
      (wait proc)
      (close pty-port)
      result)))

(test-assert "fork-pty-session pty-port is an input port"
  (let-values ([(pty-port proc) (fork-pty-session
                                  (lambda ()
                                    (display "x")
                                    (flush-output-port (current-output-port))))])
    (let ([result (input-port? pty-port)])
      (wait proc)
      (close pty-port)
      result)))

(test-assert "fork-pty-session child output readable from master"
  (let-values ([(pty-port proc) (fork-pty-session
                                  (lambda ()
                                    (display "PTY-OUTPUT")
                                    (flush-output-port (current-output-port))))])
    ;; Read from the master port -- child's stdout goes to PTY slave,
    ;; which is readable from master
    (let ([result (guard (e [#t #f])
                    (let loop ([chars '()] [count 0])
                      (if (>= count 10)
                          (let ([str (list->string (reverse chars))])
                            (string=? str "PTY-OUTPUT"))
                          (let ([ch (read-char pty-port)])
                            (if (eof-object? ch)
                                #f
                                (loop (cons ch chars) (+ count 1)))))))])
      (wait proc)
      (close pty-port)
      result)))

(test-assert "fork-pty-session child can run a shell command"
  (let-values ([(pty-port proc) (fork-pty-session
                                  (lambda ()
                                    (display "MARKER123")
                                    (flush-output-port (current-output-port))))])
    ;; Read output from master port looking for our marker.
    ;; When the child exits, the slave side closes and read-char may
    ;; raise an I/O error (EIO) rather than returning EOF -- this is
    ;; normal for PTYs. We accumulate what we can and check the buffer.
    (let ([result (let loop ([buf ""] [count 0])
                    (if (>= count 50)
                        (string-contains buf "MARKER123")
                        (guard (e [#t (string-contains buf "MARKER123")])
                          (let ([ch (read-char pty-port)])
                            (if (eof-object? ch)
                                (string-contains buf "MARKER123")
                                (loop (string-append buf (string ch))
                                      (+ count 1)))))))])
      (wait proc)
      (close pty-port)
      result)))

;; ======================================================================
;; pty-name->tty-name / tty-name->pty-name
;; ======================================================================

(test-equal "pty-name->tty-name converts /dev/ptyXX to /dev/ttyXX"
  "/dev/ttyp0"
  (pty-name->tty-name "/dev/ptyp0"))

(test-equal "tty-name->pty-name converts /dev/ttyXX to /dev/ptyXX"
  "/dev/ptyp0"
  (tty-name->pty-name "/dev/ttyp0"))

(test-assert "pty-name->tty-name and tty-name->pty-name are inverses"
  (let ([name "/dev/ptyq5"])
    (string=? name (tty-name->pty-name (pty-name->tty-name name)))))

(test-equal "pty-name->tty-name handles /dev/ptya3"
  "/dev/ttya3"
  (pty-name->tty-name "/dev/ptya3"))

(test-equal "tty-name->pty-name handles /dev/ttya3"
  "/dev/ptya3"
  (tty-name->pty-name "/dev/ttya3"))

;; ======================================================================
;; make-pty-generator
;; ======================================================================

(test-assert "make-pty-generator returns a procedure"
  (procedure? (make-pty-generator)))

(test-assert "make-pty-generator thunk returns two values (master-port and slave-name)"
  (let ([gen (make-pty-generator)])
    (let-values ([(master slave-name) (gen)])
      (let ([result (and (port? master) (string? slave-name))])
        (close master)
        result))))

(test-assert "make-pty-generator can produce multiple PTY pairs"
  (let ([gen (make-pty-generator)])
    (let-values ([(m1 s1) (gen)])
      (let-values ([(m2 s2) (gen)])
        (let ([result (and (port? m1) (port? m2)
                           (not (string=? s1 s2)))])
          (close m1)
          (close m2)
          result)))))

(test-assert "make-pty-generator slave names start with /dev/"
  (let ([gen (make-pty-generator)])
    (let-values ([(master slave-name) (gen)])
      (let ([result (and (>= (string-length slave-name) 5)
                         (string=? "/dev/" (substring slave-name 0 5)))])
        (close master)
        result))))

(test-end)
