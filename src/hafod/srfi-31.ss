#!chezscheme
;;; (hafod srfi-31) -- SRFI-31: A special form rec for recursive evaluation
;;; Reference: https://srfi.schemers.org/srfi-31/srfi-31.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Chez Scheme provides rec natively.

(library (hafod srfi-31)
  (export rec)
  (import (chezscheme)))
