;;; test-srfi-42.ss -- Conformance tests for the comprehension library (SRFI 42).
;;;
;;; fold-ec and fold3-ec had the expression and combiner arguments swapped
;;; against the spec, which orders the expression BEFORE the combiner(s):
;;; (fold-ec x0 qualifier* expression f). The spec-order call therefore tried to
;;; apply the range value as a procedure and errored; only hafod's reversed order
;;; worked. The fix moves the macro pattern positions (the bodies already apply
;;; the combiners correctly). The spec-order assertions reddened on the pre-fix
;;; tree.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-42) (chezscheme))

(test-begin "srfi-42")

;; ===========================================================================
;; Section A -- fold-ec / fold3-ec take the spec argument order
;; ===========================================================================
;; (fold-ec 0 (:range i 5) i +) folds + over 0..4 starting at 0 -> 10.
;; Pre-fix the spec order raised "attempt to apply non-procedure 0".

(test-equal "fold-ec folds in spec argument order (expression before combiner)"
            10 (fold-ec 0 (:range i 5) i +))

;; fold3-ec applies f1 to the first value then f2 to each subsequent value and
;; the accumulator; a minimum over 1..5 is 1.
(test-equal "fold3-ec folds a minimum over a range in spec argument order"
            1 (fold3-ec 'empty (:range i 1 6) i
                        (lambda (x) x)
                        (lambda (v acc) (min v acc))))
(test-equal "fold3-ec returns its default for an empty range"
            'empty (fold3-ec 'empty (:range i 0) i
                             (lambda (x) x)
                             (lambda (v acc) (min v acc))))

;; ===========================================================================
;; Section B -- high-value comprehension spot-checks
;; ===========================================================================

(test-equal "list-ec collects a range" '(0 1 2 3) (list-ec (:range i 4) i))
(test-equal "sum-ec sums a range" 10 (sum-ec (:range i 5) i))
(test-equal "list-ec with a nested qualifier filters" '(0 2 4)
            (list-ec (:range i 6) (if (even? i)) i))

(test-end)
