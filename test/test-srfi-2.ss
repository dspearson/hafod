;;; test-srfi-2.ss -- Spot-check tests for the and-let* library (SRFI 2).
;;;
;;; and-let* chains bindings and guard clauses left-to-right, short-circuiting
;;; to #f the moment a binding is #f or a guard fails, and evaluating its body
;;; only when every clause held. This suite spot-checks a value-binding chained
;;; into a guard, a failing guard, a #f binding, and the bound-variable clause.
;;; The audit dispositioned this library conformant.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-2) (chezscheme))

(test-begin "srfi-2")

;; A binding whose value passes a following guard runs the body.
(test-equal "and-let* binds then passes a guard, running the body"
            10 (and-let* ((x 5) ((> x 3))) (* x 2)))

;; A failing guard short-circuits to #f (the body is never reached).
(test-equal "and-let* returns #f when a guard clause fails"
            #f (and-let* ((x 5) ((> x 10))) (* x 2)))

;; A useful binding: keep the pair found by assv, then take its cdr.
(test-equal "and-let* binds a searched pair and uses it in the body"
            'b (and-let* ((p (assv 2 '((1 . a) (2 . b))))) (cdr p)))

;; A binding to #f short-circuits before the body.
(test-equal "and-let* returns #f when a binding is #f"
            #f (and-let* ((x (assv 9 '((1 . a))))) (cdr x)))

;; The bound-variable clause tests an already-bound identifier for truth.
(test-equal "and-let* tests a bound variable then chains a guard"
            70 (let ((y 7)) (and-let* (y ((> y 3))) (* y 10))))

;; An empty clause list simply runs the body.
(test-equal "and-let* with no clauses runs its body"
            42 (and-let* () 42))

(test-end)
