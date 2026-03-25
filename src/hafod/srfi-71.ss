#!chezscheme
;;; (hafod srfi-71) -- SRFI-71: Extended LET-syntax for multiple values
;;; Reference: https://srfi.schemers.org/srfi-71/srfi-71.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-71)
  (export let let* letrec letrec*)
  (import (rename (chezscheme)
            (let chez:let)
            (let* chez:let*)
            (letrec chez:letrec)
            (letrec* chez:letrec*)))

  ;; Extended let: process bindings one by one
  (define-syntax let
    (syntax-rules (values)
      ;; Named let: keep as is
      ((let name ((var init) ...) body ...)
       (chez:let name ((var init) ...) body ...))
      ;; No bindings
      ((let () body ...)
       (begin body ...))
      ;; Multi-value with values keyword
      ((let (((values v ...) expr) . more) body ...)
       (call-with-values (lambda () expr)
         (lambda (v ...)
           (let more body ...))))
      ;; Detect multi-value: two vars before last expr
      ((let ((v1 v2 expr) . more) body ...)
       (call-with-values (lambda () expr)
         (lambda (v1 v2)
           (let more body ...))))
      ;; Normal single-value binding
      ((let ((var init) . more) body ...)
       (chez:let ((var init))
         (let more body ...)))))

  (define-syntax let*
    (syntax-rules (values)
      ((let* () body ...)
       (begin body ...))
      ((let* (((values v ...) expr) . more) body ...)
       (call-with-values (lambda () expr)
         (lambda (v ...)
           (let* more body ...))))
      ((let* ((v1 v2 expr) . more) body ...)
       (call-with-values (lambda () expr)
         (lambda (v1 v2)
           (let* more body ...))))
      ((let* ((var init) . more) body ...)
       (chez:let ((var init))
         (let* more body ...)))))

  (define-syntax letrec
    (syntax-rules (values)
      ((letrec () body ...)
       (begin body ...))
      ((letrec ((var init) ...) body ...)
       (chez:letrec ((var init) ...) body ...))))

  (define-syntax letrec*
    (syntax-rules ()
      ((letrec* () body ...)
       (begin body ...))
      ((letrec* ((var init) ...) body ...)
       (chez:letrec* ((var init) ...) body ...)))))
