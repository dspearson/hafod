;;; test-srfi-71.ss -- Spot-check tests for the extended let library (SRFI 71).
;;;
;;; SRFI 71 extends let / let* so a binding may name several identifiers before
;;; its init expression, distributing that expression's multiple values across
;;; them. hafod provides the two-value shorthand; this suite pins it (plain,
;;; named and mixed bindings included) alongside let*.
;;;
;;; Deferred capability (recorded in Future, NOT asserted here): the
;;; N-greater-than-two shorthand (let ((a b c (values 1 2 3))) …) raises invalid
;;; syntax, and the uncons / unlist / values->list family is absent. Adding
;;; them is out of scope for this point release. The audit dispositioned the
;;; two-value surface conformant with that note.
;;; Copyright (c) 2026, hafod contributors.

;; The library redefines let/let*/letrec/letrec* with the multiple-value
;; shorthand; take them from the library and everything else from chezscheme.
(import (test runner) (except (chezscheme) let let* letrec letrec*) (hafod srfi-71))

(test-begin "srfi-71")

;; ===========================================================================
;; Section A -- plain and named let still work
;; ===========================================================================

(test-equal "plain let binds and evaluates its body" 3 (let ((x 1) (y 2)) (+ x y)))
(test-equal "named let iterates"
            10 (let loop ((i 0) (acc 0)) (if (= i 5) acc (loop (+ i 1) (+ acc i)))))

;; ===========================================================================
;; Section B -- the two-value shorthand distributes a values expression
;; ===========================================================================

(test-equal "let binds two identifiers from a two-value expression"
            '(1 2) (let ((a b (values 1 2))) (list a b)))
(test-equal "let mixes a two-value binding with a single-value binding"
            '(10 20 5) (let ((a b (values 10 20)) (c 5)) (list a b c)))

;; ===========================================================================
;; Section C -- let* threads earlier bindings, including the shorthand
;; ===========================================================================

(test-equal "let* sees a two-value binding's names in a later init"
            '(1 2 3) (let* ((a b (values 1 2)) (c (+ a b))) (list a b c)))

(test-end)
