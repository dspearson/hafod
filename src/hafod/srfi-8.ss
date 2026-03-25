#!chezscheme
;;; (hafod srfi-8) -- SRFI-8: receive: Binding to formals of multiple values
;;; Reference: https://srfi.schemers.org/srfi-8/srfi-8.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Re-exports receive from (hafod compat).

(library (hafod srfi-8)
  (export receive)
  (import (only (hafod compat) receive)))
