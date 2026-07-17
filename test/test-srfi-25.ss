;;; test-srfi-25.ss -- Spot-check tests for the array library (SRFI 25).
;;;
;;; SRFI 25 provides multi-dimensional arrays: make-array over a shape, then
;;; positional array-ref / array-set!, with array-rank / array-start /
;;; array-end reporting the bounds. This suite spot-checks a 2-D array
;;; round-trip and the row-major array constructor.
;;;
;;; Deliberate/deferred: shape returns a raw vector (not a first-class array),
;;; and share-array plus the index-vector form of array-ref/array-set! are not
;;; provided (recorded in Future). The audit dispositioned the positional
;;; surface conformant with those notes.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-25) (chezscheme))

(test-begin "srfi-25")

;; ===========================================================================
;; Section A -- a 2-D array: make, predicate, rank, bounds, ref/set! round-trip
;; ===========================================================================

(define a (make-array (shape 0 2 0 3) 0))

(test-assert "make-array builds an array" (array? a))
(test-equal "array-rank reports the dimension count" 2 (array-rank a))
(test-equal "array-start reports a dimension's lower bound" 0 (array-start a 0))
(test-equal "array-end reports the first dimension's upper bound" 2 (array-end a 0))
(test-equal "array-end reports the second dimension's upper bound" 3 (array-end a 1))
(test-equal "a freshly made array holds its fill value" 0 (array-ref a 0 0))

(array-set! a 42 1 2)
(test-equal "array-set! then array-ref round-trips at a position"
            42 (array-ref a 1 2))
(test-equal "setting one cell leaves another untouched" 0 (array-ref a 0 1))

;; ===========================================================================
;; Section B -- the row-major array constructor
;; ===========================================================================

(define b (array (shape 0 2 0 2) 1 2 3 4))    ; row-major: [0 0]=1 [0 1]=2 [1 0]=3 [1 1]=4

(test-equal "array fills row-major, element [0 1]" 2 (array-ref b 0 1))
(test-equal "array fills row-major, element [1 0]" 3 (array-ref b 1 0))

;; ===========================================================================
;; Section C -- shape returns a raw vector (deliberate)
;; ===========================================================================
;; Pinned so a later move to a first-class array-valued shape would redden.

(test-assert "shape returns a raw vector, not a first-class array (deliberate)"
             (vector? (shape 0 3 0 2)))

(test-end)
