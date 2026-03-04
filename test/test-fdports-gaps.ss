;;; Tests for fd-ports gap filling: seek/tell, error-output-port, file-option aliases
;;; Part of hafod v2.0 Phase 8 Plan 01

(import (chezscheme)
        (hafod fd-ports)
        (hafod posix)
        (hafod compat)
        (test runner))

(test-begin "fd-ports-gaps")

;;; ============================================================
;;; seek/tell tests
;;; ============================================================

(test-assert "seek/set and tell work on a file"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-seek-XXXXXX")])
    (let ([bv (string->utf8 "hello world")])
      (posix-write fd bv)
      ;; Seek to beginning
      (seek fd 0 seek/set)
      (let ([pos (tell fd)])
        (posix-close fd)
        (posix-unlink path)
        (= pos 0)))))

(test-assert "seek to position 5 and tell returns 5"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-seek2-XXXXXX")])
    (let ([bv (string->utf8 "hello world")])
      (posix-write fd bv)
      (seek fd 5 seek/set)
      (let ([pos (tell fd)])
        (posix-close fd)
        (posix-unlink path)
        (= pos 5)))))

(test-assert "seek/delta moves relative to current position"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-seekd-XXXXXX")])
    (let ([bv (string->utf8 "abcdefghij")])
      (posix-write fd bv)
      ;; Position is at end (10), seek back 5
      (seek fd -5 seek/delta)
      (let ([pos1 (tell fd)])
        ;; Now seek forward 2
        (seek fd 2 seek/delta)
        (let ([pos2 (tell fd)])
          (posix-close fd)
          (posix-unlink path)
          (and (= pos1 5) (= pos2 7)))))))

(test-assert "seek/end moves relative to end of file"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-seeke-XXXXXX")])
    (let ([bv (string->utf8 "abcdefghij")])
      (posix-write fd bv)
      ;; Seek to end
      (seek fd 0 seek/end)
      (let ([pos1 (tell fd)])
        ;; Seek to 3 before end
        (seek fd -3 seek/end)
        (let ([pos2 (tell fd)])
          (posix-close fd)
          (posix-unlink path)
          (and (= pos1 10) (= pos2 7)))))))

;;; ============================================================
;;; error-output-port
;;; ============================================================

(test-assert "error-output-port returns a port"
  (output-port? (error-output-port)))

(test-assert "error-output-port returns current-error-port"
  (eq? (current-error-port) (error-output-port)))

;;; ============================================================
;;; with-stdio-ports*
;;; ============================================================

(test-assert "with-stdio-ports* rebinds stdio to fds 0/1/2"
  (with-stdio-ports*
    (lambda ()
      (and (port? (current-input-port))
           (port? (current-output-port))
           (port? (current-error-port))))))

;;; ============================================================
;;; File option aliases
;;; ============================================================

(test-equal "create+trunc is correct bitmask"
  (bitwise-ior open/write open/create open/truncate)
  create+trunc)

(test-equal "write+append+create is correct bitmask"
  (bitwise-ior open/write open/append open/create)
  write+append+create)

(test-equal "read-only equals open/read"
  open/read
  read-only)

(test-assert "seek/set seek/delta seek/end are distinct numeric constants"
  (and (integer? seek/set)
       (integer? seek/delta)
       (integer? seek/end)
       (not (= seek/set seek/delta))
       (not (= seek/delta seek/end))))

(test-end)
