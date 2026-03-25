#!chezscheme
;;; (hafod srfi-61) -- SRFI-61: A more general cond clause
;;; Reference: https://srfi.schemers.org/srfi-61/srfi-61.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Chez Scheme's cond already supports => clauses.

(library (hafod srfi-61)
  (export cond)
  (import (chezscheme)))
