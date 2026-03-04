;;; (test runner) -- Minimal test framework for hafod
;;; Provides test-begin, test-end, test-assert, test-equal, test-error.
;;; Copyright (c) 2026, hafod contributors.

(library (test runner)
  (export test-begin test-end test-assert test-equal test-error)
  (import (chezscheme))

  ;; Use a vector as mutable box to avoid R6RS assigned-variable export issues.
  ;; Index 0 = passes, 1 = fails, 2 = suite name
  (define *state* (vector 0 0 ""))

  (define (test-begin name)
    (vector-set! *state* 2 name)
    (vector-set! *state* 0 0)
    (vector-set! *state* 1 0)
    (display "=== ") (display name) (display " ===") (newline))

  (define (test-end)
    (newline)
    (display (vector-ref *state* 0)) (display " passed, ")
    (display (vector-ref *state* 1)) (display " failed")
    (newline)
    (when (> (vector-ref *state* 1) 0) (exit 1)))

  (define (pass!)
    (vector-set! *state* 0 (+ (vector-ref *state* 0) 1)))

  (define (fail! msg)
    (vector-set! *state* 1 (+ (vector-ref *state* 1) 1))
    (display "FAIL: ") (display msg) (newline))

  (define-syntax test-assert
    (syntax-rules ()
      ((_ msg expr)
       (guard (e (#t (fail! msg)
                     (display "  exception: ") (display-condition e) (newline)))
         (if expr
             (pass!)
             (fail! msg))))))

  (define-syntax test-equal
    (syntax-rules ()
      ((_ msg expected actual)
       (guard (e (#t (fail! msg)
                     (display "  exception: ") (display-condition e) (newline)))
         (let ((e-val expected) (a-val actual))
           (if (equal? e-val a-val)
               (pass!)
               (begin (fail! msg)
                      (display "  expected: ") (write e-val) (newline)
                      (display "  actual:   ") (write a-val) (newline))))))))

  (define-syntax test-error
    (syntax-rules ()
      ((_ msg expr)
       (guard (e (#t (pass!)))
         expr
         ;; If we get here, no error was raised
         (fail! msg)
         (display "  (expected error, but none raised)") (newline)))))

  ) ; end library
