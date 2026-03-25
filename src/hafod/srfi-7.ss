#!chezscheme
;;; (hafod srfi-7) -- SRFI-7: Feature-based program configuration language
;;; Reference: https://srfi.schemers.org/srfi-7/srfi-7.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; This SRFI is very niche and not commonly used in scsh code.
;;; Provided as a stub for completeness; use R6RS library forms instead.

(library (hafod srfi-7)
  (export program)
  (import (chezscheme))

  (define-syntax program
    (syntax-rules ()
      ((_ clause ...)
       (begin clause ...)))))
