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

;; ===========================================================================
;; Section H -- default-compare (cross-type total order) and symbol-compare
;; ===========================================================================
;; The type order is null < pair < boolean < char < string < symbol < number
;; < vector; within a type it compares recursively.

(test-equal "default-compare orders null before a pair" -1 (default-compare '() '(1)))
(test-equal "default-compare orders a boolean before a char" -1 (default-compare #t #\a))
(test-equal "default-compare orders a string before a symbol" -1 (default-compare "z" 'a))
(test-equal "default-compare orders a number before a vector" -1 (default-compare 1 '#(0)))
(test-equal "default-compare recurses into pairs"
            -1 (default-compare '(1 2) '(1 3)))
(test-equal "default-compare reports equal for equal values" 0 (default-compare '(1 2) '(1 2)))
(test-equal "symbol-compare orders by symbol name" -1 (symbol-compare 'abc 'abd))

;; ===========================================================================
;; Section I -- 2-arg optional-compare arity + compare-by< constructor
;; ===========================================================================
;; =?/<?/… gain a 2-arg arity defaulting the compare to default-compare, while
;; the existing 3-arg (explicit-compare) arity is unchanged. compare-by< gains a
;; no-x-y constructor arity returning a compare procedure.

(test-assert "=? with two arguments uses default-compare" (=? 2 2))
(test-assert "<? with two arguments uses default-compare" (<? 1 2))
(test-assert ">? with two arguments uses default-compare" (>? 3 2))
(test-assert "the explicit-compare arity of =? still works" (=? integer-compare 2 2))
(test-equal "compare-by< with one argument returns a compare procedure"
            -1 ((compare-by< <) 1 2))
(test-equal "compare-by< with three arguments still computes directly"
            1 (compare-by< < 2 1))

;; ===========================================================================
;; Section J -- min/max-compare and the chain comparison family
;; ===========================================================================

(test-equal "min-compare returns the minimum per the compare"
            1 (min-compare integer-compare 3 1 2))
(test-equal "max-compare returns the maximum per the compare"
            3 (max-compare integer-compare 3 1 2))
(test-assert "chain<? is #t for a strictly increasing chain"
             (chain<? integer-compare 1 2 3))
(test-assert "chain<? is #f when not strictly increasing"
             (not (chain<? integer-compare 1 3 2)))
(test-assert "chain<=? allows equal adjacent elements"
             (chain<=? integer-compare 1 1 2))
(test-assert "chain>? is #t for a strictly decreasing chain"
             (chain>? integer-compare 3 2 1))
(test-assert "chain>=? allows equal adjacent elements"
             (chain>=? integer-compare 3 3 1))
(test-assert "chain=? is #t when all elements are equal"
             (chain=? integer-compare 2 2 2))
(test-assert "chain=? is #f when an element differs"
             (not (chain=? integer-compare 2 2 3)))

(test-end)
