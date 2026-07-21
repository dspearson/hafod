;;; test-srfi-71.ss -- Spot-check tests for the extended let library (SRFI 71).
;;;
;;; SRFI 71 extends let / let* / letrec / letrec* so a binding may name several
;;; identifiers -- for arbitrary arity and with an optional rest identifier --
;;; before its init expression, distributing that expression's multiple values
;;; across them. This suite pins the full shorthand (plain, named, mixed, the
;;; N-greater-than-two positional form, the rest form, and multiple-value
;;; letrec / letrec*) alongside the value-destructuring procedures
;;; uncons / uncons-2 / uncons-3 / uncons-4, unlist, unvector, and the
;;; values->list / values->vector family.
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

;; ===========================================================================
;; Section D -- N>=3 positional shorthand and a rest binding
;; ===========================================================================

(test-equal "let binds three identifiers from a three-value expression"
            '(1 2 3) (let ((a b c (values 1 2 3))) (list a b c)))
(test-equal "let binds a rest identifier from the surplus values"
            '(1 (2 3 4)) (let (((values a . r) (values 1 2 3 4))) (list a r)))
(test-equal "a zero-variable binding evaluates its expression for effect"
            7 (let ((acc 0)) (let (((values) (set! acc 7))) acc)))

;; ===========================================================================
;; Section E -- multiple-value letrec / letrec*
;; ===========================================================================

(test-equal "letrec binds several identifiers from a values expression"
            '(1 2) (letrec ((a b (values 1 2))) (list a b)))
(test-equal "letrec* threads a multi-value binding into a later one"
            '(1 2 3) (letrec* ((a b (values 1 2)) (c (+ a b))) (list a b c)))

;; ===========================================================================
;; Section F -- value-destructuring procedures
;; ===========================================================================

(test-equal "uncons splits a pair into its car and cdr"
            '(1 (2 3)) (let ((a d (uncons '(1 2 3)))) (list a d)))
(test-equal "uncons-2 yields the first two elements and the tail"
            '(1 2 (3 4)) (let ((a b t (uncons-2 '(1 2 3 4)))) (list a b t)))
(test-equal "uncons-3 yields the first three elements and the tail"
            '(1 2 3 (4 5)) (let ((a b c t (uncons-3 '(1 2 3 4 5)))) (list a b c t)))
(test-equal "uncons-4 yields the first four elements and the tail"
            '(1 2 3 4 (5)) (let ((a b c d t (uncons-4 '(1 2 3 4 5)))) (list a b c d t)))
(test-equal "unlist spreads a list into multiple values"
            '(1 2 3) (let ((a b c (unlist '(1 2 3)))) (list a b c)))
(test-equal "unvector spreads a vector into multiple values"
            '(1 2 3) (let ((a b c (unvector '#(1 2 3)))) (list a b c)))
(test-equal "values->list collects a values expression into a list"
            '(1 2 3) (values->list (values 1 2 3)))
(test-equal "values->vector collects a values expression into a vector"
            '#(1 2 3) (values->vector (values 1 2 3)))

(test-end)
