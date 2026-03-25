#!chezscheme
;;; (hafod srfi-78) -- SRFI-78: Lightweight testing
;;; Reference: https://srfi.schemers.org/srfi-78/srfi-78.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-78)
  (export check check-ec check-report check-set-mode! check-reset!
          check-passed? check-failed?)
  (import (chezscheme))

  (define *check-passed* 0)
  (define *check-failed* 0)
  (define *check-mode* 'report)  ; off, summary, report, report-failed

  (define (check-set-mode! mode)
    (set! *check-mode* mode))

  (define (check-reset!)
    (set! *check-passed* 0)
    (set! *check-failed* 0))

  (define (check-passed?) *check-passed*)
  (define (check-failed?) *check-failed*)

  (define-syntax check
    (syntax-rules (=>)
      ((check expr => expected)
       (check-proc 'expr (lambda () expr) equal? expected))
      ((check expr (=> equal) expected)
       (check-proc 'expr (lambda () expr) equal expected))))

  (define (check-proc expr-quoted thunk equal? expected)
    (unless (eq? *check-mode* 'off)
      (let ((actual (thunk)))
        (if (equal? actual expected)
            (begin
              (set! *check-passed* (+ *check-passed* 1))
              (when (eq? *check-mode* 'report)
                (display " ; correct")
                (newline)))
            (begin
              (set! *check-failed* (+ *check-failed* 1))
              (unless (eq? *check-mode* 'summary)
                (display " ; *** failed ***")
                (newline)
                (display " ; expression: ") (write expr-quoted) (newline)
                (display " ; expected:   ") (write expected) (newline)
                (display " ; actual:     ") (write actual) (newline)))))))

  (define (check-report)
    (display "Tests: ")
    (display (+ *check-passed* *check-failed*))
    (display ", Passed: ")
    (display *check-passed*)
    (display ", Failed: ")
    (display *check-failed*)
    (newline)
    (when (> *check-failed* 0)
      (display "*** THERE WERE FAILURES ***")
      (newline)))

  ;; check-ec: check with eager comprehension qualifiers
  ;; Simplified form — full SRFI-42 integration would need (hafod srfi-42)
  (define-syntax check-ec
    (syntax-rules (=>)
      ((check-ec expr => expected)
       (check expr => expected))
      ((check-ec qual expr => expected)
       (check expr => expected)))))
