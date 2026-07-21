;;; test-srfi-95.ss -- Conformance tests for the sort library (SRFI 95).
;;;
;;; SRFI 95 orders its arguments (sort sequence less? [key]) and
;;; (merge list1 list2 less? [key]), works over both lists and vectors, takes
;;; an optional key extractor, and its sort is stable across equal keys. The
;;; audit dispositioned this library conformant; this suite proves it by
;;; exercise, including the stability guarantee.
;;; Copyright (c) 2026, hafod contributors.

;; The sort library redefines sort and merge with the SRFI 95 argument order;
;; take those from the library and everything else from chezscheme.
(import (test runner)
        (except (chezscheme) sort merge sort! merge!)
        (hafod srfi-95))

(test-begin "srfi-95")

;; ===========================================================================
;; Section A -- sort over a list and a vector
;; ===========================================================================

(test-equal "sort orders a list ascending under <"
            '(1 2 3 4 5) (sort '(3 1 4 5 2) <))
(test-equal "sort orders a vector, returning a vector"
            '#(1 2 3 4 5) (sort '#(3 1 4 5 2) <))
(test-equal "sort with > orders descending"
            '(5 4 3 2 1) (sort '(3 1 4 5 2) >))

;; ===========================================================================
;; Section B -- merge two ordered sequences
;; ===========================================================================

(test-equal "merge combines two sorted lists into one sorted list"
            '(1 2 3 4 5 6) (merge '(1 3 5) '(2 4 6) <))
(test-equal "merge preserves duplicates across both inputs"
            '(1 2 2 3 3 4) (merge '(2 3) '(1 2 3 4) <))

;; ===========================================================================
;; Section C -- sorted? predicate on lists and vectors
;; ===========================================================================

(test-assert "sorted? is #t for an ordered list" (sorted? '(1 2 3) <))
(test-assert "sorted? is #f for an unordered list" (not (sorted? '(3 1 2) <)))
(test-assert "sorted? is #t for an ordered vector" (sorted? '#(1 2 3) <))
(test-assert "sorted? is #t for an empty list" (sorted? '() <))

;; ===========================================================================
;; Section D -- key-extracting sort
;; ===========================================================================

(test-equal "sort with a key orders by the extracted key"
            '((1 . b) (2 . c) (3 . a))
            (sort '((3 . a) (1 . b) (2 . c)) < car))

;; ===========================================================================
;; Section E -- stability: equal keys keep their input order
;; ===========================================================================
;; Sorting these pairs by car alone must leave the a/c/e (key 1) and b/d
;; (key 2) records in their original relative order.

(test-equal "sort is stable: equal-key records keep input order"
            '((1 . a) (1 . c) (1 . e) (2 . b) (2 . d))
            (sort '((1 . a) (2 . b) (1 . c) (2 . d) (1 . e)) < car))

;; ===========================================================================
;; Section F -- destructive sort! and merge!
;; ===========================================================================
;; sort! sorts in place and MUST return its sequence argument. For a vector the
;; wrapper returns the very vector passed in (a naive wrapper that returned
;; Chez vector-sort!'s result would fail the eq? witness). Inputs are freshly
;; allocated so the destructive operation is well-defined.

(test-equal "sort! orders a list ascending" '(1 2 3 4 5) (sort! (list 3 1 4 5 2) <))
(let ((v (vector 3 1 4 5 2)))
  (test-assert "sort! returns the same vector object it was given" (eq? v (sort! v <)))
  (test-equal "sort! left that vector ordered in place" '#(1 2 3 4 5) v))
(test-equal "sort! with a key orders by the extracted key"
            '((1 . b) (2 . c) (3 . a))
            (sort! (list '(3 . a) '(1 . b) '(2 . c)) < car))
(test-equal "sort! is stable across equal keys"
            '((1 . a) (1 . c) (2 . b) (2 . d))
            (sort! (list '(1 . a) '(2 . b) '(1 . c) '(2 . d)) < car))

(test-equal "merge! combines two sorted lists into one sorted list"
            '(1 2 3 4 5 6) (merge! (list 1 3 5) (list 2 4 6) <))
(test-equal "merge! preserves duplicates across both inputs"
            '(1 2 2 3 3 4) (merge! (list 2 3) (list 1 2 3 4) <))

(test-end)
