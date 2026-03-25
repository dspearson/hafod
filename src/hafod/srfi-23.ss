#!chezscheme
;;; (hafod srfi-23) -- SRFI-23: Error reporting mechanism
;;; Reference: https://srfi.schemers.org/srfi-23/srfi-23.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Chez Scheme provides error natively.

(library (hafod srfi-23)
  (export error)
  (import (chezscheme)))
