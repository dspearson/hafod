#!chezscheme
;;; (hafod srfi-11) -- SRFI-11: Syntax for receiving multiple values
;;; Reference: https://srfi.schemers.org/srfi-11/srfi-11.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Chez Scheme provides let-values and let*-values natively.

(library (hafod srfi-11)
  (export let-values let*-values)
  (import (chezscheme)))
