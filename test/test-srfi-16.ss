;;; test-srfi-16.ss -- Spot-check tests for the case-lambda library (SRFI 16).
;;;
;;; SRFI 16's case-lambda builds a procedure that dispatches on the number of
;;; arguments it is called with, including a dotted final clause for variable
;;; arity. hafod re-exports Chez's native case-lambda. This suite spot-checks
;;; dispatch across zero, one, two and many arguments. The audit dispositioned
;;; this library conformant.
;;; Copyright (c) 2026, hafod contributors.

;; case-lambda is a Chez native the library re-exports; take it from the
;; library under test and everything else from chezscheme.
(import (test runner) (except (chezscheme) case-lambda) (hafod srfi-16))

(test-begin "srfi-16")

(define arity
  (case-lambda
    (() 'none)
    ((x) (list 'one x))
    ((x y) (list 'two x y))
    ((x . rest) (list 'many x rest))))

(test-equal "case-lambda dispatches the nullary clause" 'none (arity))
(test-equal "case-lambda dispatches the unary clause" '(one 1) (arity 1))
(test-equal "case-lambda dispatches the binary clause" '(two 1 2) (arity 1 2))
(test-equal "case-lambda dispatches the variable-arity clause"
            '(many 1 (2 3 4)) (arity 1 2 3 4))

(test-end)
