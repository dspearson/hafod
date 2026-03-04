;;; Gap-fill tests for untested fd-ports symbols
;;; Phase 24 Plan 02 Task 1
;;; Copyright (c) 2026 Dominic Pearson.
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod fd-ports)
        (hafod posix)
        (hafod compat)
        (except (chezscheme) vector-append open-input-file open-output-file getenv))

(test-begin "coverage-fdports")

;; ======================================================================
;; Internal tables/guardians (3 symbols)
;; ======================================================================
(test-assert "*fd-table* is a hashtable" (hashtable? *fd-table*))
(test-assert "*port-table* is a hashtable" (hashtable? *port-table*))
(test-assert "*port-guardian* is a procedure (guardian)" (procedure? *port-guardian*))

;; ======================================================================
;; Port management functions (6 symbols)
;; ======================================================================
(test-assert "delete-fdport! is a procedure" (procedure? delete-fdport!))
(test-assert "drain-port-guardian! is a procedure" (procedure? drain-port-guardian!))
(test-assert "evict-ports is a procedure" (procedure? evict-ports))
(test-assert "set-fdport! is a procedure" (procedure? set-fdport!))
(test-assert "maybe-ref-fdport is a procedure" (procedure? maybe-ref-fdport))
(test-assert "maybe-ref-fdport returns #f for unused fd"
  (not (maybe-ref-fdport 999)))

;; make-output-fdport: create a temp file, open for writing, wrap as fdport
(let-values ([(path fd) (posix-mkstemp "/tmp/hafod-cov-mofp-XXXXXX")])
  (posix-close fd)
  ;; Open fresh fd for writing
  (let ([wfd (posix-open path (bitwise-ior O_WRONLY O_TRUNC) #o644)])
    (let ([port (make-output-fdport wfd 0)])
      (test-assert "make-output-fdport returns output port" (output-port? port))
      (display "hello" port)
      (close port)))
  (posix-unlink path))

;; ======================================================================
;; Predicates (2 symbols)
;; ======================================================================
(test-assert "fd/port? on integer returns #t" (fd/port? 0))
(test-assert "fd/port? on port returns #t" (fd/port? (current-input-port)))
(test-assert "fd/port? on string returns #f" (not (fd/port? "hello")))

;; open-fdport? test
(let-values ([(path fd) (posix-mkstemp "/tmp/hafod-cov-ofp-XXXXXX")])
  (posix-close fd)
  (let ([wfd (posix-open path (bitwise-ior O_WRONLY O_TRUNC) #o644)])
    (let ([port (make-output-fdport wfd 0)])
      (test-assert "open-fdport? on active fdport returns #t" (open-fdport? port))
      (close port)))
  (posix-unlink path))

;; ======================================================================
;; Open flag constants (2 symbols)
;; ======================================================================
(test-assert "open/exclusive is an integer" (integer? open/exclusive))
(test-assert "open/non-blocking is an integer" (integer? open/non-blocking))

;; ======================================================================
;; Low-level operations (3 symbols)
;; ======================================================================

;; %set-cloexec
(test-assert "%set-cloexec is a procedure" (procedure? %set-cloexec))
(let-values ([(r w) (pipe)])
  (let ([rfd (port->fdes r)])
    (%set-cloexec rfd #t)
    (let ([flags (posix-fcntl rfd F_GETFD)])
      (test-assert "%set-cloexec sets FD_CLOEXEC"
        (not (zero? (bitwise-and flags FD_CLOEXEC)))))
    (close r)
    (close w)))

;; shell-open -- opens file and moves to target fd
;; Cannot safely test by stealing fd 0/1/2, so just verify it's a procedure
(test-assert "shell-open is a procedure" (procedure? shell-open))

;; flush-all-ports-no-threads
(test-assert "flush-all-ports-no-threads is a procedure" (procedure? flush-all-ports-no-threads))
(test-assert "flush-all-ports-no-threads runs without error"
  (begin (flush-all-ports-no-threads) #t))

;; ======================================================================
;; Error port redirectors (3 symbols)
;; ======================================================================

;; with-current-error-port (syntax)
(let ([sp (open-output-string)])
  (with-current-error-port sp
    (display "err-test" (current-error-port)))
  (test-equal "with-current-error-port redirects error output"
    "err-test" (get-output-string sp)))

;; with-current-error-port* (procedure)
(let ([sp (open-output-string)])
  (with-current-error-port* sp
    (lambda () (display "err-test2" (current-error-port))))
  (test-equal "with-current-error-port* redirects error output"
    "err-test2" (get-output-string sp)))

;; with-error-output-port* (procedure)
(let ([sp (open-output-string)])
  (with-error-output-port* sp
    (lambda () (display "err-test3" (current-error-port))))
  (test-equal "with-error-output-port* redirects error output"
    "err-test3" (get-output-string sp)))

(test-end)
