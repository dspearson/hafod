#!chezscheme
;;; (hafod srfi-2) -- SRFI-2: AND-LET*: an AND with local bindings
;;; Reference: https://srfi.schemers.org/srfi-2/srfi-2.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-2)
  (export and-let*)
  (import (chezscheme))

  (define-syntax and-let*
    (syntax-rules ()
      ;; No clauses, just body
      ((and-let* () body ...)
       (begin body ...))
      ;; No body — return result of last clause
      ((and-let* ((expr)))
       expr)
      ((and-let* ((var expr)))
       expr)
      ((and-let* (bound-variable))
       bound-variable)
      ;; Clause: bare expression (no binding)
      ((and-let* ((expr) clause ...) body ...)
       (let ((t expr))
         (if t (and-let* (clause ...) body ...) #f)))
      ;; Clause: (variable expression)
      ((and-let* ((var expr) clause ...) body ...)
       (let ((var expr))
         (if var (and-let* (clause ...) body ...) #f)))
      ;; Clause: bound-variable
      ((and-let* (bound-variable clause ...) body ...)
       (if bound-variable (and-let* (clause ...) body ...) #f)))))
