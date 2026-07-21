;;; test-srfi-63.ss -- Spot-check tests for the array library (SRFI 63).
;;;
;;; SRFI 63 provides heterogeneous arrays: make-array over dimensions, then
;;; positional array-ref / array-set!, with array-rank / array-dimensions and
;;; list<->array conversion. This suite spot-checks a 2-D round-trip, the rank-1
;;; list conversion, true make-shared-array sharing, the typed A:* prototype
;;; constructors, and the SLIB array-mapping procedures.
;;;
;;; make-shared-array shares the parent's store: a later cell change in the
;;; source shows through the shared view. Storage stays vector-backed, so the
;;; typed prototypes are conformant at the API/fill level rather than unboxed.
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
;; Section C -- make-shared-array truly shares the parent's store
;; ===========================================================================
;; A shared array holds the parent's store object, so a later change to a base
;; cell shows through the shared view -- proving the storage is shared, not
;; copied.

(define base (make-array #f 3))
(array-set! base 10 0)
(array-set! base 20 1)
(array-set! base 30 2)

(define shared (make-shared-array base (lambda (i) (list i)) 3))
(test-equal "the shared array initially reflects base" 10 (array-ref shared 0))

(array-set! base 999 0)
(test-equal "a later change to base shows through (true sharing)"
            999 (array-ref shared 0))

;; ===========================================================================
;; Section D -- typed array prototypes (vector-backed; type-zero default fills)
;; ===========================================================================
;; Each A:* prototype seeds make-array's fill with the type-appropriate zero:
;; 0.0 for inexact real, 0.0+0.0i for inexact complex, 0 for the exact numeric
;; families, and #f for booleans. Storage stays boxed Scheme vectors.

(test-assert "A:floR64b prototype makes an array"
             (array? (make-array (A:floR64b) 2 2)))
(test-equal "A:floR64b seeds the flonum zero"
            0.0 (array-ref (make-array (A:floR64b) 2 2) 0 0))
(test-equal "A:floC64b seeds the inexact complex zero"
            (make-rectangular 0.0 0.0) (array-ref (make-array (A:floC64b) 2) 0))
(test-equal "A:floQ64d seeds the exact zero"
            0 (array-ref (make-array (A:floQ64d) 2) 0))
(test-equal "A:fixZ32b seeds the exact zero"
            0 (array-ref (make-array (A:fixZ32b) 2 2) 0 0))
(test-equal "A:fixN8b seeds the exact zero"
            0 (array-ref (make-array (A:fixN8b) 2) 0))
(test-equal "A:bool seeds #f"
            #f (array-ref (make-array (A:bool) 2) 0))
;; an optional argument overrides the default fill:
(test-equal "A:floR64b takes an optional fill override"
            3.5 (array-ref (make-array (A:floR64b 3.5) 2) 0))
;; every one of the 20 prototypes builds an array (non-vacuous coverage):
(test-assert "all 20 A:* prototypes build arrays"
             (for-all array?
                      (list (make-array (A:floC128b) 1) (make-array (A:floC64b) 1)
                            (make-array (A:floC32b) 1)  (make-array (A:floC16b) 1)
                            (make-array (A:floR128b) 1) (make-array (A:floR64b) 1)
                            (make-array (A:floR32b) 1)  (make-array (A:floR16b) 1)
                            (make-array (A:floQ128d) 1) (make-array (A:floQ64d) 1)
                            (make-array (A:floQ32d) 1)
                            (make-array (A:fixZ64b) 1)  (make-array (A:fixZ32b) 1)
                            (make-array (A:fixZ16b) 1)  (make-array (A:fixZ8b) 1)
                            (make-array (A:fixN64b) 1)  (make-array (A:fixN32b) 1)
                            (make-array (A:fixN16b) 1)  (make-array (A:fixN8b) 1)
                            (make-array (A:bool) 1))))

;; ===========================================================================
;; Section E -- SLIB array mapping (array-index-map!, array-for-each, array-map!)
;; ===========================================================================
;; These three procedures are SLIB "Array Mapping" companions to SRFI 63, not
;; part of the SRFI 63 document itself. array-set! is object-first throughout.

;; array-index-map! stores proc(indices...) at each cell (row-major walk):
(test-equal "array-index-map! stored 10*i+j at [1 2]"
            12 (let ((a (make-array (A:fixZ32b) 2 3)))
                 (array-index-map! a (lambda (i j) (+ (* 10 i) j)))
                 (array-ref a 1 2)))
(test-equal "array-index-map! stored 10*i+j at [0 0]"
            0 (let ((a (make-array (A:fixZ32b) 2 3)))
                (array-index-map! a (lambda (i j) (+ (* 10 i) j)))
                (array-ref a 0 0)))

;; array-for-each visits every element (row-major), for effect:
(test-equal "array-for-each visited all elements"
            10 (let ((sum 0))
                 (array-for-each (lambda (x) (set! sum (+ sum x)))
                                 (list->array 1 '(1 2 3 4)))
                 sum))

;; array-map! writes proc(sources...) into the destination (dest first):
(test-equal "array-map! wrote proc(sources) into dest at 0"
            11 (let ((dest (make-array (A:fixZ32b) 3)))
                 (array-map! dest + (list->array 1 '(1 2 3)) (list->array 1 '(10 20 30)))
                 (array-ref dest 0)))
(test-equal "array-map! wrote proc(sources) into dest at 2"
            33 (let ((dest (make-array (A:fixZ32b) 3)))
                 (array-map! dest + (list->array 1 '(1 2 3)) (list->array 1 '(10 20 30)))
                 (array-ref dest 2)))

(test-end)
