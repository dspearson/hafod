;;; test-srfi-1.ss -- Conformance tests for the list library (SRFI 1).
;;;
;;; delete-duplicates called its comparator with the arguments reversed: the
;;; spec fixes that when x precedes y in the list the comparison is (= x y),
;;; i.e. the EARLIER element is the first argument. hafod passed the later
;;; (current) element first. This is invisible to a symmetric comparator such as
;;; equal? but wrong for an asymmetric one; the recording comparator below is the
;;; non-vacuous proof (it saw the later element first on the pre-fix tree).
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-1) (chezscheme))

(test-begin "srfi-1")

;; ===========================================================================
;; Section A -- delete-duplicates comparator argument order (spec: earlier first)
;; ===========================================================================
;; A comparator that records every (a . b) pair it is called with, in call
;; order, and answers equality. For '(10 20 10) the first comparison pits the
;; earlier 10 against the later 20, so spec order records (10 . 20). Pre-fix it
;; recorded (20 . 10).

(define dd-calls '())
(define (dd-recorder a b)
  (set! dd-calls (cons (cons a b) dd-calls))
  (equal? a b))

(define dd-result (delete-duplicates '(10 20 10) dd-recorder))
(define dd-order (reverse dd-calls))            ; chronological call order

(test-equal "delete-duplicates passes the earlier element as the first comparator argument"
            '(10 . 20) (car dd-order))
(test-equal "delete-duplicates still returns the distinct elements (result unchanged)"
            '(10 20) dd-result)

;; ===========================================================================
;; Section B -- symmetric-comparator callers are unaffected (no regression)
;; ===========================================================================

(test-equal "delete-duplicates under equal? yields distinct elements in first-seen order"
            '(1 2 3) (delete-duplicates '(1 2 1 3 2)))
(test-equal "delete-duplicates! delegates and matches delete-duplicates under equal?"
            '(1 2 3) (delete-duplicates! (list 1 2 1 3 2)))

;; ===========================================================================
;; Section C -- high-value list library spot-checks
;; ===========================================================================

(test-equal "fold-right conses right-to-left" '(1 2 3) (fold-right cons '() '(1 2 3)))
(test-equal "fold sums with the SRFI-1 (elt acc) kons order" 6 (fold + 0 '(1 2 3)))
(test-equal "take returns the leading n elements" '(1 2) (take '(1 2 3 4 5) 2))
(test-equal "drop returns the trailing elements" '(3 4 5) (drop '(1 2 3 4 5) 2))
(test-equal "partition splits by predicate into in-list then out-list"
            '((1 3 5) (2 4))
            (call-with-values (lambda () (partition odd? '(1 2 3 4 5))) list))
(test-equal "iota builds a counted range" '(0 1 2 3) (iota 4))

(test-end)
