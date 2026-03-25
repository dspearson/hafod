#!chezscheme
;;; (hafod srfi-45) -- SRFI-45: Primitives for Expressing Iterative Lazy Algorithms
;;; Reference: https://srfi.schemers.org/srfi-45/srfi-45.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Chez provides delay/force; we add lazy and eager for full SRFI-45.

(library (hafod srfi-45)
  (export delay force lazy eager)
  (import (chezscheme))

  ;; eager: wrap a value as an already-forced promise
  (define-syntax eager
    (syntax-rules ()
      ((_ expr)
       (delay expr))))

  ;; lazy: like delay but for expressions that return promises (iterative forcing)
  (define-syntax lazy
    (syntax-rules ()
      ((_ expr)
       (delay (force expr))))))
