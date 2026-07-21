;;; test-srfi-31.ss -- Spot-check tests for the rec library (SRFI 31).
;;;
;;; SRFI 31's rec evaluates an expression in a scope where a name is bound to
;;; the value being computed, allowing self-reference. hafod provides its own
;;; two-clause rec: the value form (rec var expr) binds a self-referential
;;; value, and the procedure form (rec (name . formals) body ...) defines a
;;; self-recursive procedure directly. Both surfaces are pinned below.
;;; Copyright (c) 2026, hafod contributors.

;; rec is defined by the library under test (shadowing Chez's native rec);
;; take it from there and everything else from chezscheme.
(import (test runner) (except (chezscheme) rec) (hafod srfi-31))

(test-begin "srfi-31")

;; (rec var expr): the lambda bound to fact can call itself by name.
(test-equal "rec binds a self-referential factorial (variable form)"
            120 ((rec fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) 5))

;; A second self-referential lambda: a recursive sum 1..n.
(test-equal "rec binds a self-referential recursive sum"
            55 ((rec sum (lambda (n) (if (= n 0) 0 (+ n (sum (- n 1)))))) 10))

;; (rec (name . formals) body ...): the procedure form defines a self-recursive
;; procedure directly, without an explicit lambda.
(test-equal "rec procedure form binds a self-referential factorial"
            120 ((rec (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) 5))

(test-end)
