;;; test-srfi-61.ss -- Conformance tests for the general cond library (SRFI 61).
;;;
;;; hafod re-exports Chez's R6RS cond, which provides ordinary clauses, else, and
;;; the two-part (test => receiver) clause. The SRFI 61 three-part
;;; (generator guard => receiver) clause is a DELIBERATE DIVERGENCE deferred to a
;;; future release: hafod does not rewrite the pervasively-used cond macro in this
;;; point release (see the source header). This suite pins the provided behaviour
;;; and deliberately adds NO assertion that depends on the three-part arm.
;;; Copyright (c) 2026, hafod contributors.

;; cond is the library under test; take it from there, the rest from chezscheme.
(import (test runner) (except (chezscheme) cond) (hafod srfi-61))

(test-begin "srfi-61")

;; Ordinary clause: the first true test's body is the value.
(test-equal "an ordinary cond clause returns its body"
            'yes (cond ((= 1 1) 'yes) (else 'no)))

;; else clause: taken when no test matches.
(test-equal "the else clause is taken when no test matches"
            'fallback (cond ((= 1 2) 'no) (else 'fallback)))

;; Multiple clauses: the first match wins.
(test-equal "the first matching clause wins"
            'second (cond ((= 1 2) 'first) ((= 2 2) 'second) (else 'no)))

;; The two-part (test => receiver) clause applies the receiver to the test value.
(test-equal "a two-part (test => receiver) clause applies the receiver to the test value"
            'a (cond ((assv 1 '((1 . a) (2 . b))) => cdr) (else 'no)))
(test-equal "a two-part => clause falls through when the test is #f"
            'none (cond ((assv 9 '((1 . a))) => cdr) (else 'none)))

(test-end)
