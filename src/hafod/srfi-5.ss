#!chezscheme
;;; (hafod srfi-5) -- SRFI-5: A compatible let form with signatures and rest arguments
;;; Reference: https://srfi.schemers.org/srfi-5/srfi-5.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-5)
  (export let)
  (import (rename (chezscheme) (let chez:let)))

  (define-syntax let
    (lambda (stx)
      (syntax-case stx ()
        ;; Regular let: (let ((var init) ...) body ...)
        [(_ ((var init) ...) body ...)
         #'(chez:let ((var init) ...) body ...)]
        ;; Standard named let: (let name ((var init) ...) body ...)
        [(_ name ((var init) ...) body ...)
         (identifier? #'name)
         #'(chez:let name ((var init) ...) body ...)]
        ;; SRFI-5 signature form: (let (name (var init) ...) body ...)
        [(_ (name clause ...) body ...)
         (identifier? #'name)
         (chez:let ([clauses (syntax->list #'(clause ...))])
           (chez:let loop ([cs clauses] [vars '()] [inits '()])
             (cond
               [(null? cs)
                (with-syntax ([(v ...) (reverse vars)]
                              [(i ...) (reverse inits)])
                  #'(chez:let name ((v i) ...) body ...))]
               ;; Rest argument: bare identifier at the end
               [(and (null? (cdr cs)) (identifier? (car cs)))
                (with-syntax ([(v ...) (reverse vars)]
                              [(i ...) (reverse inits)]
                              [rest (car cs)])
                  #'(letrec ([name (lambda (v ... . rest) body ...)])
                      (name i ...)))]
               [else
                (syntax-case (car cs) ()
                  [(v i)
                   (loop (cdr cs) (cons #'v vars) (cons #'i inits))])])))]
        ;; No bindings
        [(_ () body ...)
         #'(chez:let () body ...)]))))
