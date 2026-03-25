#!chezscheme
;;; (hafod srfi-9) -- SRFI-9: Defining Record Types
;;; Reference: https://srfi.schemers.org/srfi-9/srfi-9.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Implements SRFI-9 syntax on top of Chez Scheme's R6RS records.

(library (hafod srfi-9)
  (export define-record-type)
  (import (except (chezscheme) define-record-type))

  (define-syntax define-record-type
    (lambda (stx)
      (define (index-of sym lst)
        (let loop ((l lst) (i 0))
          (cond ((null? l) #f)
                ((eq? (car l) sym) i)
                (else (loop (cdr l) (+ i 1))))))

      (syntax-case stx ()
        ((_ type-name
            (constructor-name constructor-arg ...)
            predicate-name
            field-spec ...)
         (let* ((fields (syntax->list #'(field-spec ...)))
                (field-names
                 (map (lambda (fs)
                        (syntax->datum (syntax-case fs () ((n . _) #'n))))
                      fields))
                (n-fields (length field-names))
                (ctor-arg-names (map syntax->datum
                                    (syntax->list #'(constructor-arg ...))))
                (field-ctor-indices
                 (map (lambda (fn) (index-of fn ctor-arg-names))
                      field-names)))
           (with-syntax
             ((nf n-fields)
              (type-name-sym (datum->syntax #'type-name
                               (syntax->datum #'type-name)))
              ((field-sym ...)
               (map (lambda (fs) (syntax-case fs () ((n . _) #'n))) fields))
              ((accessor-name ...)
               (map (lambda (fs)
                      (syntax-case fs ()
                        ((_ a) #'a) ((_ a _) #'a)))
                    fields))
              ((accessor-idx ...)
               (iota n-fields))
              (((mutator-clause ...) ...)
               (map (lambda (fs idx)
                      (syntax-case fs ()
                        ((_ _) '())
                        ((_ _ mut)
                         (list #`(define mut
                                   (record-mutator rtd #,idx))))))
                    fields (iota n-fields)))
              ((init-expr ...)
               (map (lambda (fn fci)
                      (if fci
                          (list-ref (syntax->list #'(constructor-arg ...)) fci)
                          #'(void)))
                    field-names field-ctor-indices)))
             #'(begin
                 (define rtd
                   (make-record-type-descriptor
                    'type-name-sym #f #f #f #f
                    (vector (list 'mutable 'field-sym) ...)))
                 (define rcd
                   (make-record-constructor-descriptor rtd #f #f))
                 (define predicate-name (record-predicate rtd))
                 (define constructor-name
                   (let ((make (record-constructor rcd)))
                     (lambda (constructor-arg ...)
                       (make init-expr ...))))
                 (define accessor-name
                   (record-accessor rtd accessor-idx))
                 ...
                 mutator-clause ... ...))))))))
