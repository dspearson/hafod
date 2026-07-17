;;; test-srfi-31.ss -- Spot-check tests for the rec library (SRFI 31).
;;;
;;; SRFI 31's rec evaluates an expression in a scope where a name is bound to
;;; the value being computed, allowing self-reference. hafod provides Chez's
;;; native rec, which supports the variable form (rec var expr) -- the working
;;; surface this suite pins by binding a self-referential recursive lambda.
;;;
;;; Deferred capability (recorded in Future, NOT asserted here): the procedure
;;; form (rec (name . formals) body) is not provided by Chez's rec and raises
;;; invalid syntax; adding it is out of scope for this point release.
;;; Copyright (c) 2026, hafod contributors.

;; rec is a Chez native the library re-exports; take it from the library under
;; test and everything else from chezscheme.
(import (test runner) (except (chezscheme) rec) (hafod srfi-31))

(test-begin "srfi-31")

;; (rec var expr): the lambda bound to fact can call itself by name.
(test-equal "rec binds a self-referential factorial (variable form)"
            120 ((rec fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) 5))

;; A second self-referential lambda: a recursive sum 1..n.
(test-equal "rec binds a self-referential recursive sum"
            55 ((rec sum (lambda (n) (if (= n 0) 0 (+ n (sum (- n 1)))))) 10))

(test-end)
