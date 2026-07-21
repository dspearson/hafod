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

(array-set! a 1 2 42)                          ; SRFI 25: the stored value comes LAST
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
;; Section D -- the index-object accessor form (value LAST)
;; ===========================================================================
;; SRFI 25 accepts a single index object -- a vector, or a rank-1 array -- in
;; place of the separate subscripts, and puts the stored value LAST.

;; index-vector array-ref agrees with the multi-index form (non-vacuous: the
;; cell holds a distinctive value written through the multi-index form first).
(test-equal "index-vector array-ref equals the multi-index form"
            7 (let ((p (make-array (shape 0 2 0 2) 0)))
                (array-set! p 1 0 7)                 ; value LAST
                (array-ref p (vector 1 0))))

;; index-vector array-set! (value LAST) writes the mapped cell.
(test-equal "index-vector array-set! (value last) writes the mapped cell"
            5 (let ((p (make-array (shape 0 2 0 2) 0)))
                (array-set! p (vector 0 1) 5)        ; value LAST
                (array-ref p 0 1)))

;; a rank-1 array is also a valid index object.
(test-equal "rank-1 array index object equals the multi-index form"
            9 (let ((p (make-array (shape 0 2 0 3) 0))
                    (ix (array (shape 0 2) 1 2)))    ; index object holding (1 2)
                (array-set! p 1 2 9)
                (array-ref p ix)))

;; ===========================================================================
;; Section C -- shape returns a raw vector (deliberate)
;; ===========================================================================
;; Pinned so a later move to a first-class array-valued shape would redden.

(test-assert "shape returns a raw vector, not a first-class array (deliberate)"
             (vector? (shape 0 3 0 2)))

;; ===========================================================================
;; Section E -- share-array: a shared view with true aliasing
;; ===========================================================================
;; A shared view holds its parent's store, so a write through the view lands at
;; the parent's mapped coordinate (aliasing, not a copy). PROC maps the new
;; indices to the parent's and returns them as multiple values.

;; Transpose view: writing view[0 1] shows at the parent's mapped cell [1 0].
;; A copy would fail this.
(test-equal "share-array aliases: a write through the view shows at the parent"
            9 (let* ((p (make-array (shape 0 2 0 2) 0))
                     (v (share-array p (shape 0 2 0 2) (lambda (r c) (values c r)))))
                (array-set! v 0 1 9)                 ; value LAST
                (array-ref p 1 0)))

;; Diagonal view (the verbatim SRFI 25 example): setting each diagonal cell
;; through the view leaves the parent's main diagonal all 1.
(test-equal "share-array diagonal: writes through the view fill the parent's diagonal"
            '(1 1 1 1)
            (let* ((i (make-array (shape 0 4 0 4) 0))
                   (d (share-array i (shape 0 4) (lambda (k) (values k k)))))
              (do ((k 0 (+ k 1))) ((= k 4))
                (array-set! d k 1))                  ; value LAST: (array-set! d k 1)
              (list (array-ref i 0 0) (array-ref i 1 1)
                    (array-ref i 2 2) (array-ref i 3 3))))

;; Share of a share: a write through the second view still shows at the root
;; parent's store (affine maps compose through the parent's flat-index).
(test-equal "share-array composes: a write through a share-of-a-share reaches the root"
            8 (let* ((p (make-array (shape 0 2 0 2) 0))
                     (v (share-array p (shape 0 2 0 2) (lambda (r c) (values c r))))
                     (w (share-array v (shape 0 2 0 2) (lambda (r c) (values c r)))))
                (array-set! w 1 0 8)                 ; w = transpose of transpose = identity to p
                (array-ref p 1 0)))

(test-end)
