#!chezscheme
;;; (hafod srfi-61) -- SRFI-61: A more general cond clause
;;; Reference: https://srfi.schemers.org/srfi-61/srfi-61.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Chez Scheme's cond provides the R6RS clauses, including the two-part
;;; (test => receiver) clause.
;;;
;;; DELIBERATE DIVERGENCE (deferred): the SRFI 61 three-part clause
;;; (generator guard => receiver) is not provided. Supplying it means replacing
;;; the cond macro hafod uses pervasively with a custom one that reproduces all
;;; R6RS cond behaviour plus the extra arm -- real regression surface for a point
;;; release -- so it is deferred to a future release rather than rewritten here.

(library (hafod srfi-61)
  (export cond)
  (import (chezscheme)))
