#!chezscheme
;;; (hafod srfi-16) -- SRFI-16: Syntax for procedures of variable arity
;;; Reference: https://srfi.schemers.org/srfi-16/srfi-16.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Chez Scheme provides case-lambda natively.

(library (hafod srfi-16)
  (export case-lambda)
  (import (chezscheme)))
