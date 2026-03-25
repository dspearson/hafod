#!chezscheme
;;; (hafod srfi-39) -- SRFI-39: Parameter objects
;;; Reference: https://srfi.schemers.org/srfi-39/srfi-39.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Chez Scheme provides make-parameter and parameterize natively.

(library (hafod srfi-39)
  (export make-parameter parameterize)
  (import (chezscheme)))
