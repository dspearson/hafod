#!chezscheme
;;; (hafod srfi-6) -- SRFI-6: Basic String Ports
;;; Reference: https://srfi.schemers.org/srfi-6/srfi-6.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Chez Scheme provides these natively; this is a thin re-export.

(library (hafod srfi-6)
  (export open-input-string open-output-string get-output-string)
  (import (chezscheme)))
