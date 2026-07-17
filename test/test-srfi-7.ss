;;; test-srfi-7.ss -- Spot-check tests for the program library (SRFI 7).
;;;
;;; hafod's program is a documented niche stub: rather than the full SRFI 7
;;; feature-configuration language (requires / files / feature-cond), it simply
;;; SPLICES its clauses as a begin, per the note in the library header (R6RS
;;; library forms are hafod's real mechanism). This suite pins that begin-splice
;;; behaviour: as an expression it returns its last clause, and at top level it
;;; splices definitions into the enclosing scope.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-7) (chezscheme))

(test-begin "srfi-7")

;; As an expression, (program e ...) behaves as (begin e ...) -- the value is
;; that of the last clause.
(test-equal "program as an expression yields its last clause"
            7 (program 1 2 7))

;; At top level, program splices its definition clauses via begin, so the
;; bindings are visible afterwards.
(program
  (define prog-a 10)
  (define prog-b 20))

(test-equal "program splices top-level definitions through begin"
            30 (+ prog-a prog-b))

(test-end)
