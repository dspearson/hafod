;;; test-srfi-67.ss -- Conformance tests for the compare library (SRFI 67).
;;;
;;; SRFI 67's three-way compare procedures return -1, 0 or 1 for
;;; less/equal/greater. This suite pins the type-specific compares
;;; (integer/real/string/char/boolean), the compare-by< family that builds a
;;; three-way result from a strict less-than, and the =?/<? predicates and
;;; if3 dispatch built on top of a compare. The audit dispositioned the
;;; three-way forms conformant; these assertions prove it by exercise.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-67) (chezscheme))

(test-begin "srfi-67")

;; ===========================================================================
;; Section A -- type-specific three-way compares return -1 / 0 / 1
;; ===========================================================================

(test-equal "integer-compare less is -1" -1 (integer-compare 1 2))
(test-equal "integer-compare equal is 0"  0 (integer-compare 2 2))
(test-equal "integer-compare greater is 1" 1 (integer-compare 3 2))

(test-equal "real-compare less is -1" -1 (real-compare 1.5 2.5))
(test-equal "real-compare equal is 0"  0 (real-compare 2.5 2.5))
(test-equal "real-compare greater is 1" 1 (real-compare 3.5 2.5))

(test-equal "string-compare less is -1" -1 (string-compare "abc" "abd"))
(test-equal "string-compare equal is 0"  0 (string-compare "abc" "abc"))
(test-equal "string-compare greater is 1" 1 (string-compare "abd" "abc"))
(test-equal "string-compare treats a shorter prefix as less"
            -1 (string-compare "ab" "abc"))

(test-equal "char-compare less is -1" -1 (char-compare #\a #\b))
(test-equal "char-compare equal is 0"  0 (char-compare #\b #\b))
(test-equal "char-compare greater is 1" 1 (char-compare #\c #\b))

(test-equal "boolean-compare orders #f before #t" -1 (boolean-compare #f #t))
(test-equal "boolean-compare equal is 0" 0 (boolean-compare #t #t))
(test-equal "boolean-compare orders #t after #f" 1 (boolean-compare #t #f))

;; ===========================================================================
;; Section B -- compare-by< and friends build a three-way result from an order
;; ===========================================================================

(test-equal "compare-by< with < returns -1 for less" -1 (compare-by< < 1 2))
(test-equal "compare-by< with < returns 0 for equal"  0 (compare-by< < 2 2))
(test-equal "compare-by< with < returns 1 for greater" 1 (compare-by< < 3 2))
(test-equal "compare-by> with > reports a greater first argument as 1"
            1 (compare-by> > 3 2))
(test-equal "compare-by> with > reports a lesser first argument as -1"
            -1 (compare-by> > 2 3))
(test-equal "compare-by= yields 0 when equal, -1 otherwise"
            0 (compare-by= = 5 5))

;; ===========================================================================
;; Section C -- predicates and if3 dispatch built on a compare
;; ===========================================================================

(test-assert "<? is true when the compare reports less"
             (<? integer-compare 1 2))
(test-assert "=? is true when the compare reports equal"
             (=? integer-compare 2 2))
(test-assert ">? is true when the compare reports greater"
             (>? integer-compare 3 2))
(test-equal "if3 dispatches on the sign of a compare result"
            'less (if3 (integer-compare 1 2) 'less 'equal 'greater))
(test-equal "if3 selects the equal branch on 0"
            'equal (if3 (integer-compare 2 2) 'less 'equal 'greater))

(test-end)
