#!chezscheme
;;; (hafod srfi-17) -- SRFI-17: Generalized set!
;;; Reference: https://srfi.schemers.org/srfi-17/srfi-17.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-17)
  (export setter set! getter-with-setter)
  (import (except (chezscheme) set!))

  ;; Table mapping getters to setters
  (define *setter-table* (make-eq-hashtable))

  (define (setter proc)
    (or (eq-hashtable-ref *setter-table* proc #f)
        (error 'setter "no setter defined for procedure" proc)))

  (define (getter-with-setter getter setter-proc)
    (eq-hashtable-set! *setter-table* getter setter-proc)
    getter)

  ;; Generalized set!
  (define-syntax set!
    (syntax-rules ()
      ;; (set! (proc args ...) value) => ((setter proc) args ... value)
      ((set! (proc args ...) value)
       ((setter proc) args ... value))
      ;; (set! var value) => normal set!
      ((set! var value)
       (chez:set! var value))))

  (define-syntax chez:set!
    (syntax-rules ()
      ((_ var val)
       (let ()
         (import (only (chezscheme) set!))
         (set! var val)))))

  ;; Register standard setters
  (eq-hashtable-set! *setter-table* car set-car!)
  (eq-hashtable-set! *setter-table* cdr set-cdr!)
  (eq-hashtable-set! *setter-table* vector-ref vector-set!)
  (eq-hashtable-set! *setter-table* string-ref string-set!)
  (eq-hashtable-set! *setter-table* bytevector-u8-ref bytevector-u8-set!)
  (eq-hashtable-set! *setter-table* bytevector-s8-ref bytevector-s8-set!)
  (eq-hashtable-set! *setter-table* unbox set-box!))
