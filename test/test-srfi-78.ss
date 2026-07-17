;;; test-srfi-78.ss -- Conformance tests for the lightweight-testing library (SRFI 78).
;;;
;;; check-passed? is specified as UNARY returning a boolean: #t iff there were no
;;; failures AND exactly the given number of checks passed. hafod's was nullary
;;; and returned the raw count, so any argument raised "incorrect number of
;;; arguments" -- the unary assertions below reddened on the pre-fix tree. The
;;; non-standard nullary check-failed? count-getter is retained unchanged.
;;;
;;; The suite drives its own check forms in 'summary mode so their output stays
;;; quiet while the pass/fail counters still advance (a POSITIVE count exercises
;;; the unary semantics genuinely, not merely the zero case).
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-78) (chezscheme))

(test-begin "srfi-78")

;; ===========================================================================
;; Section A -- check-passed? is unary and boolean over a positive pass count
;; ===========================================================================
(check-set-mode! 'summary)
(check-reset!)
(check (+ 1 1) => 2)                 ; pass
(check (* 2 3) => 6)                 ; pass

(test-assert "check-passed? is #t for the exact number of passes (2)"
             (check-passed? 2))
(test-assert "check-passed? is #f for a wrong pass count (3)"
             (not (check-passed? 3)))
(test-equal "check-failed? still reports the nullary failure count (zero here)"
            0 (check-failed?))

;; ===========================================================================
;; Section B -- check-passed? is failure-sensitive; check-failed? counts
;; ===========================================================================
(check-reset!)
(check (+ 1 1) => 2)                 ; pass
(check (+ 1 1) => 3)                 ; fail (summary mode suppresses the detail)

(test-assert "check-passed? is #f when a check failed, even though one check passed"
             (not (check-passed? 1)))
(test-equal "check-failed? reports the single failure" 1 (check-failed?))

;; ===========================================================================
;; Section C -- check-set-mode! honours its mode symbols
;; ===========================================================================
;; In 'off mode a check is not run and nothing is counted, so exactly zero
;; passed and zero failed -- check-passed? of 0 is #t.
(check-set-mode! 'off)
(check-reset!)
(check (+ 1 1) => 2)                 ; suppressed: not counted
(test-assert "in 'off mode no check is counted (check-passed? 0 is #t)"
             (check-passed? 0))
(test-equal "in 'off mode no failure is counted" 0 (check-failed?))
(check-set-mode! 'summary)

;; ===========================================================================
;; Section D -- check-report runs and check ... => expected increments passes
;; ===========================================================================
(test-assert "check-report runs without error on a clean pass"
             (begin (check-reset!) (check (+ 2 2) => 4) (check-report) #t))

(test-end)
