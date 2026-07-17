;;; test-srfi-43.ss -- Conformance tests for the vector library (SRFI 43).
;;;
;;; SRFI 43's iteration procedures thread the index as the FIRST argument: a
;;; fold applies its kons in (kons i state elt) order and map/for-each pass
;;; (i elem …). This suite pins that ordering, the sub-range copy!, the
;;; in-place and copying reverses, and a binary search hit/miss. The vector
;;; library was dispositioned conformant in the audit; these assertions prove
;;; it by exercise and guard against a future ordering regression.
;;; Copyright (c) 2026, hafod contributors.

;; The vector library redefines vector-map/-for-each/-copy/-copy! with the
;; SRFI 43 (index-first) contract; take those from the library and everything
;; else from chezscheme.
(import (test runner)
        (except (chezscheme) vector-map vector-for-each vector-copy vector-copy!)
        (hafod srfi-43))

(test-begin "srfi-43")

;; ===========================================================================
;; Section A -- vector-fold / vector-fold-right kons order (kons i state elt)
;; ===========================================================================
;; Collecting (index . element) pairs proves the index is the first argument
;; and the element the last: index 0 must pair with element 10, and so on.

(test-equal "vector-fold applies kons as (kons i state elt): indices pair with elements"
            '((0 . 10) (1 . 20) (2 . 30))
            (reverse (vector-fold (lambda (i acc elt) (cons (cons i elt) acc))
                                  '() '#(10 20 30))))
(test-equal "vector-fold threads the state (a left sum)"
            10 (vector-fold (lambda (i acc elt) (+ acc elt)) 0 '#(1 2 3 4)))
(test-equal "vector-fold-right also passes the index first, visiting right-to-left"
            '(0 1 2)
            (vector-fold-right (lambda (i acc elt) (cons i acc)) '() '#(10 20 30)))
(test-equal "vector-fold-right rebuilds a vector's elements in order"
            '(1 2 3)
            (vector-fold-right (lambda (i acc elt) (cons elt acc)) '() '#(1 2 3)))

;; ===========================================================================
;; Section B -- vector-copy! copies a (sub)range into a target
;; ===========================================================================

(test-equal "vector-copy! copies a whole source at a target offset"
            '#(0 7 8 9 0)
            (let ((t (vector 0 0 0 0 0)))
              (vector-copy! t 1 (vector 7 8 9))
              t))
(test-equal "vector-copy! copies a source sub-range [sstart,send)"
            '#(3 4 0 0 0)
            (let ((t (vector 0 0 0 0 0)))
              (vector-copy! t 0 (vector 1 2 3 4 5) 2 4)
              t))

;; ===========================================================================
;; Section C -- reversing: in-place vector-reverse! and copying reverse
;; ===========================================================================

(test-equal "vector-reverse! reverses in place"
            '#(4 3 2 1)
            (let ((v (vector 1 2 3 4)))
              (vector-reverse! v)
              v))
(test-equal "vector-reverse-copy returns a reversed copy"
            '#(4 3 2 1) (vector-reverse-copy '#(1 2 3 4)))
(test-equal "vector-reverse-copy leaves its argument untouched"
            '#(1 2 3 4)
            (let ((v (vector 1 2 3 4)))
              (vector-reverse-copy v)
              v))

;; ===========================================================================
;; Section D -- vector-binary-search hit and miss
;; ===========================================================================
;; The comparator is called (cmp element key) and returns -1/0/1.

(define (num-cmp a b) (cond ((< a b) -1) ((> a b) 1) (else 0)))

(test-equal "vector-binary-search finds a present element and returns its index"
            3 (vector-binary-search '#(1 3 5 7 9) 7 num-cmp))
(test-equal "vector-binary-search misses an absent element and returns #f"
            #f (vector-binary-search '#(1 3 5 7 9) 4 num-cmp))

;; ===========================================================================
;; Section E -- assorted index-first spot-checks
;; ===========================================================================

(test-equal "vector-map passes (i elem) and builds a new vector"
            '#(1 4 9) (vector-map (lambda (i x) (* x x)) '#(1 2 3)))
(test-equal "vector-count counts matching (i elem) pairs"
            2 (vector-count (lambda (i x) (even? x)) '#(1 2 3 4)))
(test-assert "vector= compares element-wise under a predicate"
             (vector= = '#(1 2 3) '#(1 2 3)))
(test-equal "vector-index returns the first matching index"
            1 (vector-index (lambda (i x) (> x 10)) '#(5 20 30)))

(test-end)
