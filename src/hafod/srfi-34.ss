#!chezscheme
;;; (hafod srfi-34) -- SRFI-34: Exception Handling for Programs
;;; Reference: https://srfi.schemers.org/srfi-34/srfi-34.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Chez Scheme provides guard, with-exception-handler, raise natively.

(library (hafod srfi-34)
  (export guard with-exception-handler raise raise-continuable)
  (import (chezscheme)))
