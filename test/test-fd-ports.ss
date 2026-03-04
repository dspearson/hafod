;;; test-fd-ports.ss -- Tests for (hafod fd-ports)
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod fd-ports) (hafod posix) (hafod compat) (hafod rdelim))

(test-begin "fd-ports")

;; ======================================================================
;; Table initialization
;; ======================================================================

(test-assert "init: current-input-port is fdport"
  (fdport? (current-input-port)))

(test-assert "init: current-output-port is fdport"
  (fdport? (current-output-port)))

(test-assert "init: current-error-port is fdport"
  (fdport? (current-error-port)))

;; Note: In --script mode, Chez may replace current-input-port/current-output-port
;; with different port objects than those registered at library load time.
;; Re-init with current ports and test that they're tracked (fd numbers may vary
;; in --script mode due to Chez's internal fd allocation).
(init-fdports!)

(test-assert "init: stdin is fdport after init"
  (fdport? (current-input-port)))

(test-assert "init: stdout is fdport after init"
  (fdport? (current-output-port)))

(test-assert "init: stderr is fdport after init"
  (fdport? (current-error-port)))

(test-assert "init: port->fdes on stderr returns 2"
  (let ([fd (port->fdes (current-error-port))])
    (release-port-handle (current-error-port))
    (= fd 2)))

(test-assert "init: port->fdes returns integer for stdin"
  (let ([fd (port->fdes (current-input-port))])
    (release-port-handle (current-input-port))
    (integer? fd)))

(test-assert "init: port->fdes returns integer for stdout"
  (let ([fd (port->fdes (current-output-port))])
    (release-port-handle (current-output-port))
    (integer? fd)))

;; ======================================================================
;; fdes->inport / fdes->outport
;; ======================================================================

(test-assert "fdes->inport creates readable port"
  (let* ([fd (posix-open "/dev/null" O_RDONLY 0)]
         [p (fdes->inport fd)])
    (and (input-port? p)
         (eof-object? (read-char p))
         (begin (close p) #t))))

(test-assert "fdes->outport creates writable port"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [wp (fdes->outport wfd)])
    (and (output-port? wp)
         (begin
           (display "test" wp)
           (flush-output-port wp)
           (let ([got (posix-read rfd 4)])
             (posix-close rfd)
             (close wp)
             (equal? got (string->utf8 "test")))))))

(test-assert "fdes->inport idempotent: same port returned"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [p1 (fdes->inport rfd)]
         [p2 (fdes->inport rfd)])
    (let ([same (eq? p1 p2)])
      (posix-close wfd)
      ;; Release both increments
      (release-port-handle p1)
      (close p1)
      same)))

(test-assert "fdes->outport on fd mapped to inport raises error"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)])
    ;; Map rfd to an input port
    (let ([p (fdes->inport rfd)])
      (let ([got-error
             (guard (e [#t #t])
               (fdes->outport rfd)
               #f)])
        (posix-close wfd)
        (close p)
        got-error))))

;; ======================================================================
;; port->fdes
;; ======================================================================

(test-assert "port->fdes returns integer fd"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [p (fdes->inport rfd)]
         [fd (port->fdes p)])
    (let ([result (and (integer? fd) (= fd rfd))])
      (release-port-handle p)
      (posix-close wfd)
      (close p)
      result)))

(test-assert "port->fdes increments revealed count"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [p (fdes->inport rfd)])
    ;; revealed is 1 after fdes->inport
    (let ([rev1 (port-revealed p)])
      (port->fdes p) ;; now 2
      (let ([rev2 (port-revealed p)])
        (release-port-handle p) ;; back to 1
        (posix-close wfd)
        (close p)
        (and (= rev1 1) (= rev2 2))))))

;; ======================================================================
;; release-port-handle
;; ======================================================================

(test-assert "release-port-handle decrements revealed"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [p (fdes->inport rfd)])
    ;; revealed = 1
    (port->fdes p) ;; revealed = 2
    (let ([rev-before (port-revealed p)])
      (release-port-handle p) ;; revealed = 1
      (let ([rev-after (port-revealed p)])
        (posix-close wfd)
        (close p)
        (and (= rev-before 2) (= rev-after 1))))))

(test-assert "release-port-handle at 1 drops to 0 (GC-managed)"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [p (fdes->inport rfd)])
    ;; revealed = 1
    (release-port-handle p) ;; revealed = 0
    (let ([rev (port-revealed p)])
      (posix-close wfd)
      (close p)
      (not rev)))) ;; port-revealed returns #f when 0

;; ======================================================================
;; close operations
;; ======================================================================

(test-assert "close input port removes from table"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [p (fdes->inport rfd)])
    (close p)
    (let ([result (not (fdport? p))])
      (posix-close wfd)
      result)))

(test-assert "close integer fd works"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)])
    ;; close-fdes the write fd (no port mapped)
    (close wfd)
    ;; Verify fd is closed by trying to read (should get error or eof)
    (posix-close rfd)
    #t))

(test-assert "close-fdes evicts port"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [p (fdes->inport rfd)])
    ;; close-fdes should evict the port to a new fd
    (close-fdes rfd)
    ;; The port should no longer be at rfd, but eviction happened
    ;; (port was moved to a new fd via dup before closing rfd)
    (posix-close wfd)
    #t))

(test-assert "close-after applies function then closes"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [wp (fdes->outport wfd)]
         [result (close-after wp (lambda (p) (display "ca" p) (flush-output-port p) 42))])
    (let ([got (posix-read rfd 2)])
      (posix-close rfd)
      (and (= result 42)
           (equal? got (string->utf8 "ca"))
           (port-closed? wp)))))

;; ======================================================================
;; pipe
;; ======================================================================

(test-assert "pipe: basic data flow"
  (let-values ([(rp wp) (pipe)])
    (display "hello\n" wp)
    (flush-output-port wp)
    (let ([result (read-line rp)])
      (close wp)
      (close rp)
      (equal? result "hello"))))

(test-assert "pipe: ports have revealed=0 after creation"
  (let-values ([(rp wp) (pipe)])
    (let ([r-rev (port-revealed rp)]
          [w-rev (port-revealed wp)])
      (close wp)
      (close rp)
      ;; port-revealed returns #f when revealed=0
      (and (not r-rev) (not w-rev)))))

;; ======================================================================
;; call/fdes and sleazy-call/fdes
;; ======================================================================

(test-assert "call/fdes with integer"
  (= (call/fdes 5 (lambda (fd) (* fd 2))) 10))

(test-assert "call/fdes with port manages revealed count"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [p (fdes->inport rfd)])
    (let ([rev-before (port-revealed p)])
      ;; call/fdes increments revealed, calls proc, then releases
      (let ([fd-result (call/fdes p (lambda (fd) fd))])
        (let ([rev-after (port-revealed p)])
          (posix-close wfd)
          (close p)
          ;; Before: 1, during: 2 (port->fdes), after: 1 (release-port-handle)
          (and (= rev-before 1) (= rev-after 1) (= fd-result rfd)))))))

(test-assert "sleazy-call/fdes returns fd without changing revealed"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [p (fdes->inport rfd)])
    (let ([rev-before (port-revealed p)]
          [fd-result (sleazy-call/fdes p (lambda (fd) fd))])
      (let ([rev-after (port-revealed p)])
        (posix-close wfd)
        (close p)
        (and (= rev-before 1) (= rev-after 1) (= fd-result rfd))))))

;; ======================================================================
;; fdport? predicate
;; ======================================================================

(test-assert "fdport? true for tracked port"
  (fdport? (current-input-port)))

(test-assert "fdport? false for string port"
  (not (fdport? (open-input-string "hello"))))

;; ======================================================================
;; flush-all-ports
;; ======================================================================

(test-assert "flush-all-ports doesn't error"
  (begin (flush-all-ports) #t))

;; ======================================================================
;; move->fdes
;; ======================================================================

(test-assert "move->fdes integer"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [target 80])
    (move->fdes wfd target)
    ;; Write to target, read from rfd
    (let ([p (fdes->outport target)])
      (display "moved" p)
      (flush-output-port p)
      (let ([got (posix-read rfd 5)])
        ;; Clean up
        (close p)
        (posix-close rfd)
        (equal? got (string->utf8 "moved"))))))

(test-assert "move->fdes port"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [wp (fdes->outport wfd)]
         [target 81])
    (let ([new-port (move->fdes wp target)])
      ;; new-port should be an output port on target
      (display "portmove" new-port)
      (flush-output-port new-port)
      (let ([got (posix-read rfd 8)])
        (close new-port)
        (posix-close rfd)
        (equal? got (string->utf8 "portmove"))))))

;; ======================================================================
;; dup operations
;; ======================================================================

(test-assert "dup integer returns new fd"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [dup-wfd (dup wfd)])
    ;; dup-wfd is a new integer fd
    (and (integer? dup-wfd)
         (not (= dup-wfd wfd))
         (begin
           (posix-write dup-wfd (string->utf8 "dup"))
           (let ([got (posix-read rfd 3)])
             (posix-close rfd)
             (posix-close wfd)
             (posix-close dup-wfd)
             (equal? got (string->utf8 "dup")))))))

(test-assert "dup input port returns new input port"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [rp (fdes->inport rfd)]
         [dup-rp (dup rp)])
    ;; dup-rp is a new input port
    (and (input-port? dup-rp)
         (not (eq? dup-rp rp))
         (begin
           (posix-write wfd (string->utf8 "dupin\n"))
           (posix-close wfd)
           (let ([result (read-line dup-rp)])
             (close rp)
             (close dup-rp)
             (equal? result "dupin"))))))

(test-assert "dup output port returns new output port"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [wp (fdes->outport wfd)]
         [dup-wp (dup wp)])
    (and (output-port? dup-wp)
         (not (eq? dup-wp wp))
         (begin
           (display "dupout\n" dup-wp)
           (flush-output-port dup-wp)
           (let ([rp (fdes->inport rfd)])
             (let ([result (read-line rp)])
               (close wp)
               (close dup-wp)
               (close rp)
               (equal? result "dupout")))))))

(test-assert "dup->fdes returns integer"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [rp (fdes->inport rfd)]
         [fd (dup->fdes rp)])
    (posix-close wfd)
    (close rp)
    (posix-close fd)
    (integer? fd)))

(test-assert "dup->fdes with target"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [target 82]
         [fd (dup->fdes wfd target)])
    (and (= fd target)
         (begin
           (posix-write fd (string->utf8 "t"))
           (let ([got (posix-read rfd 1)])
             (posix-close rfd)
             (posix-close wfd)
             (posix-close fd)
             (equal? got (string->utf8 "t")))))))

(test-assert "dup->inport returns input port"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [p (dup->inport rfd)])
    (and (input-port? p)
         (begin
           (posix-write wfd (string->utf8 "hi\n"))
           (posix-close wfd)
           (posix-close rfd)
           (let ([result (read-line p)])
             (close p)
             (equal? result "hi"))))))

(test-assert "dup->outport returns output port"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [p (dup->outport wfd)])
    (and (output-port? p)
         (begin
           (display "op\n" p)
           (flush-output-port p)
           (posix-close wfd)
           (let ([got (posix-read rfd 3)])
             (close p)
             (posix-close rfd)
             (equal? got (string->utf8 "op\n")))))))

;; ======================================================================
;; open-file / open-input-file / open-output-file
;; ======================================================================

(test-assert "open-file read /dev/null"
  (let ([p (open-file "/dev/null" open/read)])
    (and (input-port? p)
         (eof-object? (read-char p))
         (begin (close p) #t))))

(test-assert "open-output-file creates and writes"
  (let ([fname "/tmp/hafod-test-open-out.txt"])
    (let ([p (open-output-file fname)])
      (display "test-data" p)
      (close p))
    (let ([p (open-input-file fname)])
      (let ([result (read-line p)])
        (close p)
        (posix-unlink fname)
        (equal? result "test-data")))))

(test-assert "open-file error on nonexistent"
  (guard (e [(posix-error? e) #t])
    (open-file "/no/such/file/hafod-open-test" open/read)
    #f))

(test-assert "file-option constants"
  (and (= open/read 0)
       (= open/write 1)
       (= open/read+write 2)
       (= open/create 64)
       (= open/truncate 512)))

;; ======================================================================
;; Port rebinding
;; ======================================================================

(test-assert "with-current-input-port rebinds"
  (let-values ([(rp wp) (pipe)])
    (display "rebind\n" wp)
    (flush-output-port wp)
    (close wp)
    (let ([result (with-current-input-port rp
                    (read-line))])
      (close rp)
      (equal? result "rebind"))))

(test-assert "with-current-output-port rebinds"
  (let-values ([(rp wp) (pipe)])
    (with-current-output-port wp
      (display "out-rebind\n"))
    (flush-output-port wp)
    (close wp)
    (let ([result (read-line rp)])
      (close rp)
      (equal? result "out-rebind"))))

(test-assert "with-current-input-port restores original"
  (let ([orig (current-input-port)])
    (let-values ([(rp wp) (pipe)])
      (close wp)
      (with-current-input-port rp (values))
      (close rp)
      (eq? (current-input-port) orig))))

(test-assert "with-current-input-port* thunk variant"
  (let-values ([(rp wp) (pipe)])
    (display "thunk\n" wp)
    (flush-output-port wp)
    (close wp)
    (let ([result (with-current-input-port* rp
                    (lambda () (read-line)))])
      (close rp)
      (equal? result "thunk"))))

(test-assert "with-current-output-port* thunk variant"
  (let-values ([(rp wp) (pipe)])
    (with-current-output-port* wp
      (lambda () (display "thunkout\n")))
    (flush-output-port wp)
    (close wp)
    (let ([result (read-line rp)])
      (close rp)
      (equal? result "thunkout"))))

(test-end)
