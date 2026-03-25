#!chezscheme
;;; (hafod srfi-26) -- SRFI-26: Notation for Specializing Parameters
;;; Reference: https://srfi.schemers.org/srfi-26/srfi-26.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-26)
  (export cut cute <> <...>)
  (import (chezscheme))

  ;; Define <> and <...> as auxiliary syntax
  (define-syntax <> (lambda (x) (syntax-violation '<> "misuse of <> outside cut/cute" x)))
  (define-syntax <...> (lambda (x) (syntax-violation '<...> "misuse of <...> outside cut/cute" x)))

  ;; cut implementation using syntax-case for proper hygiene
  (define-syntax cut
    (lambda (stx)
      (define (process-slots slots)
        ;; Returns (params body) where params is list of generated ids
        ;; and body is list of expressions
        (let loop ((slots slots) (params '()) (body '()) (rest-arg #f))
          (syntax-case slots (<> <...>)
            (() (list (reverse params) (reverse body) #f))
            ((<...>) (list (reverse params) (reverse body) #t))
            ((<> . more)
             (with-syntax ((x (car (generate-temporaries '(x)))))
               (loop #'more (cons #'x params) (cons #'x body) #f)))
            ((expr . more)
             (loop #'more params (cons #'expr body) #f)))))
      (syntax-case stx ()
        ((_ slot-or-expr ...)
         (let ((result (process-slots #'(slot-or-expr ...))))
           (let ((params (car result))
                 (body (cadr result))
                 (has-rest (caddr result)))
             (with-syntax (((p ...) params)
                           ((b ...) body))
               (if has-rest
                   #'(lambda (p ... . rest-args)
                       (apply b ... rest-args))
                   #'(lambda (p ...) (b ...))))))))))

  ;; cute: like cut but evaluates non-slot expressions once
  (define-syntax cute
    (lambda (stx)
      (define (process-slots slots)
        (let loop ((slots slots) (params '()) (binds '()) (body '()) (has-rest #f))
          (syntax-case slots (<> <...>)
            (() (list (reverse params) (reverse binds) (reverse body) #f))
            ((<...>) (list (reverse params) (reverse binds) (reverse body) #t))
            ((<> . more)
             (with-syntax ((x (car (generate-temporaries '(x)))))
               (loop #'more (cons #'x params) binds (cons #'x body) #f)))
            ((expr . more)
             (with-syntax ((t (car (generate-temporaries '(t)))))
               (loop #'more params
                     (cons #'(t expr) binds)
                     (cons #'t body) #f))))))
      (syntax-case stx ()
        ((_ slot-or-expr ...)
         (let ((result (process-slots #'(slot-or-expr ...))))
           (let ((params (car result))
                 (binds (cadr result))
                 (body (caddr result))
                 (has-rest (cadddr result)))
             (with-syntax (((p ...) params)
                           ((bind ...) binds)
                           ((b ...) body))
               (if has-rest
                   #'(let (bind ...)
                       (lambda (p ... . rest-args)
                         (apply b ... rest-args)))
                   #'(let (bind ...)
                       (lambda (p ...) (b ...))))))))))))
