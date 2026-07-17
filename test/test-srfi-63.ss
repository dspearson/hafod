;;; test-srfi-63.ss -- Spot-check tests for the array library (SRFI 63).
;;;
;;; SRFI 63 provides heterogeneous arrays: make-array over dimensions, then
;;; positional array-ref / array-set!, with array-rank / array-dimensions and
;;; list<->array conversion. This suite spot-checks a 2-D round-trip, the rank-1
;;; list conversion, and pins that make-shared-array COPIES rather than shares.
;;;
;;; Deliberate/deferred: make-shared-array copies storage instead of sharing it
;;; (a later cell change in the source does not show through), and the typed
;;; array constructors (numeric prototype tags) are not provided (Future). The
;;; audit dispositioned the positional surface conformant with those notes.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-63) (chezscheme))

(test-begin "srfi-63")

;; ===========================================================================
;; Section A -- a 2-D array round-trip
;; ===========================================================================
;; The prototype argument is ignored for typing here (generic vector backing);
;; #f yields an unspecified fill, which we overwrite via array-set!.

(define a (make-array #f 2 3))

(test-assert "make-array builds an array" (array? a))
(test-equal "array-rank reports the dimension count" 2 (array-rank a))
(test-equal "array-dimensions lists the extents" '(2 3) (array-dimensions a))

(array-set! a 42 1 2)
(array-set! a 7 0 0)
(test-equal "array-set! then array-ref round-trips at [1 2]" 42 (array-ref a 1 2))
(test-equal "array-set! then array-ref round-trips at [0 0]" 7 (array-ref a 0 0))

;; ===========================================================================
;; Section B -- list <-> array (rank 1)
;; ===========================================================================

(define c (list->array 1 '(5 6 7)))
(test-equal "array->list recovers the stored elements" '(5 6 7) (array->list c))
(test-equal "array-ref indexes a rank-1 array" 6 (array-ref c 1))

;; ===========================================================================
;; Section C -- make-shared-array copies, it does not share (deliberate)
;; ===========================================================================
;; A shared array built from base holds base's values, but a later change to a
;; base cell does NOT show through -- proving the storage was copied.

(define base (make-array #f 3))
(array-set! base 10 0)
(array-set! base 20 1)
(array-set! base 30 2)

(define shared (make-shared-array base (lambda (i) (list i)) 3))
(test-equal "the shared array initially reflects base" 10 (array-ref shared 0))

(array-set! base 999 0)
(test-equal "a later change to base does NOT show through (copy, not share)"
            10 (array-ref shared 0))

(test-end)
