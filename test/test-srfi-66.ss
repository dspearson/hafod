;;; test-srfi-66.ss -- Conformance tests for the octet-vector library (SRFI 66).
;;;
;;; The specification defines u8vector-compare as LENGTH-FIRST: "Shorter vectors
;;; are always smaller than longer ones, and vectors of equal length are compared
;;; lexicographically." (srfi.schemers.org/srfi-66; the reference implementation
;;; branches on length before walking elements.) hafod compared element-first
;;; with length only as a tiebreak, so (u8vector-compare #u8(2) #u8(1 9)) was 1
;;; where the spec requires -1 (the shorter vector is smaller regardless of its
;;; first element). The fix aligns with the spec; the decisive assertions below
;;; reddened on the pre-fix tree.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-66) (chezscheme))

(test-begin "srfi-66")

;; ===========================================================================
;; Section A -- u8vector-compare is length-first (the decisive cases)
;; ===========================================================================
;; A one-octet vector is smaller than a two-octet vector even though its only
;; element (2) is larger than the other's first element (1): length wins.

(test-equal "a shorter vector is smaller than a longer one (2 vs 1,9 is -1)"
            -1 (u8vector-compare (u8vector 2) (u8vector 1 9)))
(test-equal "a longer vector is greater than a shorter one (1,9 vs 2 is 1)"
            1 (u8vector-compare (u8vector 1 9) (u8vector 2)))

;; Equal length: compare lexicographically.
(test-equal "equal-length vectors compare lexicographically (1,2 vs 1,3 is -1)"
            -1 (u8vector-compare (u8vector 1 2) (u8vector 1 3)))
(test-equal "equal-length vectors compare lexicographically (1,3 vs 1,2 is 1)"
            1 (u8vector-compare (u8vector 1 3) (u8vector 1 2)))
(test-equal "identical vectors compare equal (0)"
            0 (u8vector-compare (u8vector 1 2 3) (u8vector 1 2 3)))
(test-equal "two empty vectors compare equal (0)"
            0 (u8vector-compare (u8vector) (u8vector)))

;; ===========================================================================
;; Section B -- round-trip, copy, and equality spot-checks
;; ===========================================================================

(test-equal "list->u8vector then u8vector->list is the identity"
            '(1 2 3) (u8vector->list (list->u8vector '(1 2 3))))
(test-equal "u8vector-copy duplicates the contents"
            '(5 6 7) (u8vector->list (u8vector-copy (u8vector 5 6 7))))
(test-equal "u8vector-length reports the octet count" 3 (u8vector-length (u8vector 4 5 6)))
(test-equal "u8vector-ref reads an element" 6 (u8vector-ref (u8vector 4 5 6) 2))
(test-assert "u8vector=? is #t for equal contents" (u8vector=? (u8vector 1 2) (u8vector 1 2)))
(test-assert "u8vector=? is #f for differing contents" (not (u8vector=? (u8vector 1 2) (u8vector 1 3))))

;; u8vector-copy! copies a run into a destination.
(define dest (make-u8vector 3 0))
(u8vector-copy! (u8vector 7 8 9) 0 dest 0 3)
(test-equal "u8vector-copy! copies the source run into the destination"
            '(7 8 9) (u8vector->list dest))

(test-end)
