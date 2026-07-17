;;; test-srfi-26.ss -- Conformance tests for the cut library (SRFI 26).
;;;
;;; SRFI 26's cut and cute build specialised procedures from a template with
;;; <> slots and a trailing <...> rest slot. The two differ in evaluation of
;;; the non-slot expressions: cut re-evaluates them on every call, cute
;;; evaluates them ONCE at construction. This suite pins the slot filling, the
;;; rest slot, and -- with side-effecting counters -- the once-vs-per-call
;;; distinction. The audit dispositioned this library conformant.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-26) (chezscheme))

(test-begin "srfi-26")

;; ===========================================================================
;; Section A -- cut fills <> slots positionally
;; ===========================================================================

(test-equal "cut with a fixed argument and one slot: ((cut + 1 <>) 4) is 5"
            5 ((cut + 1 <>) 4))
(test-equal "cut with two slots fills them left-to-right"
            '(a b) ((cut list <> <>) 'a 'b))
(test-equal "cut interleaves fixed arguments and slots"
            '(1 2 3 4) ((cut list 1 <> 3 <>) 2 4))
(test-equal "cut with no slots ignores its call arguments' positions"
            7 ((cut + 3 4)))

;; ===========================================================================
;; Section B -- the <...> rest slot collects the remaining arguments
;; ===========================================================================

(test-equal "cut with a rest slot: ((cut list 1 <...>) 2 3 4) is (1 2 3 4)"
            '(1 2 3 4) ((cut list 1 <...>) 2 3 4))
(test-equal "cut with only a rest slot forwards every argument"
            6 ((cut + <...>) 1 2 3))

;; ===========================================================================
;; Section C -- cute evaluates non-slot expressions ONCE at construction
;; ===========================================================================
;; A side-effecting counter proves the difference: cute runs the counter once
;; when the procedure is built; cut runs it on every application.

(define cute-count 0)
(define (cute-bump) (set! cute-count (+ cute-count 1)) 100)
(define cute-proc (cute + (cute-bump) <>))

(test-equal "cute evaluated its non-slot expression once at construction time"
            1 cute-count)
(test-equal "cute-proc applies the once-captured value" 105 (cute-proc 5))
(test-equal "cute-proc reuses the captured value" 106 (cute-proc 6))
(test-equal "cute did not re-evaluate its non-slot expression across two calls"
            1 cute-count)

(define cut-count 0)
(define (cut-bump) (set! cut-count (+ cut-count 1)) 100)
(define cut-proc (cut + (cut-bump) <>))

(test-equal "cut did not evaluate its non-slot expression at construction time"
            0 cut-count)
(test-equal "cut-proc evaluates the expression on the first call" 105 (cut-proc 5))
(test-equal "cut-proc evaluates the expression again on the second call"
            106 (cut-proc 6))
(test-equal "cut re-evaluated its non-slot expression on every call"
            2 cut-count)

(test-end)
