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

  ;; Composed-accessor setter helper.  Spec rule:
  ;; (set! (caXXr x) v) == (set-car! (cXXr x) v) and
  ;; (set! (cdXXr x) v) == (set-cdr! (cXXr x) v) -- the inner accessor is the
  ;; native c[ad]+r of all-but-the-first letter, and the mutator is set-car!
  ;; when the first letter is a, else set-cdr!.
  (define (register-cxr! accessor inner mutate!)
    (eq-hashtable-set! *setter-table* accessor
      (lambda (obj v) (mutate! (inner obj) v))))

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
  (eq-hashtable-set! *setter-table* unbox set-box!)

  ;; Composed-accessor setters (caar … cddddr) -- see register-cxr! above.
  (register-cxr! caar   car   set-car!)  (register-cxr! cadr   cdr   set-car!)
  (register-cxr! cdar   car   set-cdr!)  (register-cxr! cddr   cdr   set-cdr!)
  (register-cxr! caaar  caar  set-car!)  (register-cxr! caadr  cadr  set-car!)
  (register-cxr! cadar  cdar  set-car!)  (register-cxr! caddr  cddr  set-car!)
  (register-cxr! cdaar  caar  set-cdr!)  (register-cxr! cdadr  cadr  set-cdr!)
  (register-cxr! cddar  cdar  set-cdr!)  (register-cxr! cdddr  cddr  set-cdr!)
  (register-cxr! caaaar caaar set-car!)  (register-cxr! caaadr caadr set-car!)
  (register-cxr! caadar cadar set-car!)  (register-cxr! caaddr caddr set-car!)
  (register-cxr! cadaar cdaar set-car!)  (register-cxr! cadadr cdadr set-car!)
  (register-cxr! caddar cddar set-car!)  (register-cxr! cadddr cdddr set-car!)
  (register-cxr! cdaaar caaar set-cdr!)  (register-cxr! cdaadr caadr set-cdr!)
  (register-cxr! cdadar cadar set-cdr!)  (register-cxr! cdaddr caddr set-cdr!)
  (register-cxr! cddaar cdaar set-cdr!)  (register-cxr! cddadr cdadr set-cdr!)
  (register-cxr! cdddar cddar set-cdr!)  (register-cxr! cddddr cdddr set-cdr!)

  ;; Make setter itself settable: (set! (setter proc) new) expands (via the
  ;; generalised set! macro) to ((setter setter) proc new); registering setter's
  ;; own setter makes that call write proc's setter into the table.
  ;; Spec: "(set! (setter proc) setter) sets the setter associated with proc."
  (eq-hashtable-set! *setter-table* setter
    (lambda (proc new-setter)
      (eq-hashtable-set! *setter-table* proc new-setter))))
