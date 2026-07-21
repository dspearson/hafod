#!chezscheme
;;; (hafod srfi-71) -- SRFI-71: Extended LET-syntax for multiple values
;;; Reference: https://srfi.schemers.org/srfi-71/srfi-71.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; The binding engine is adapted from the SRFI 71 reference implementation
;;; (Sebastian Egner, 2005): a binding may name several identifiers -- for
;;; arbitrary arity and with an optional rest identifier -- before its init
;;; expression, distributing that expression's multiple values across them,
;;; over let / let* / letrec / letrec*.

(library (hafod srfi-71)
  (export (rename (srfi-let let) (srfi-let* let*)
                  (srfi-letrec letrec) (srfi-letrec* letrec*))
          uncons uncons-2 uncons-3 uncons-4 unlist unvector
          values->list values->vector)
  (import (rename (chezscheme)
            (let chez:let)
            (letrec chez:letrec)))

  ;; --- SRFI 71 binding engine (internal names only) -------------------------

  ;; letrec variables bind to i:undefined until their consumer fills them; there
  ;; is no portable way to bind a variable to a "raises when read" value.
  (define i:undefined 'undefined)

  (define-syntax srfi-letrec*                 ; sequential nesting of srfi-letrec
    (syntax-rules ()
      ((srfi-letrec* () body1 body ...) (srfi-letrec () body1 body ...))
      ((srfi-letrec* (bs) body1 body ...) (srfi-letrec (bs) body1 body ...))
      ((srfi-letrec* (bs1 bs2 bs ...) body1 body ...)
       (srfi-letrec (bs1) (srfi-letrec* (bs2 bs ...) body1 body ...)))))

  (define-syntax srfi-letrec                  ; i:let with the rec flag set
    (syntax-rules ()
      ((srfi-letrec ((b1 b2 b ...) ...) body1 body ...)
       (i:let "bs" #t () () (body1 body ...) ((b1 b2 b ...) ...)))))

  (define-syntax srfi-let*                     ; sequential nesting of srfi-let
    (syntax-rules ()
      ((srfi-let* () body1 body ...) (srfi-let () body1 body ...))
      ((srfi-let* (bs) body1 body ...) (srfi-let (bs) body1 body ...))
      ((srfi-let* (bs1 bs2 bs ...) body1 body ...)
       (srfi-let (bs1) (srfi-let* (bs2 bs ...) body1 body ...)))))

  (define-syntax srfi-let                      ; i:let or i:named-let
    (syntax-rules ()
      ((srfi-let ((b1 b2 b ...) ...) body1 body ...)
       (i:let "bs" #f () () (body1 body ...) ((b1 b2 b ...) ...)))
      ((srfi-let tag ((b1 b2 b ...) ...) body1 body ...)
       (i:named-let tag () (body1 body ...) ((b1 b2 b ...) ...)))))

  ;; i:let accumulates call-with-values skeletons (cwv) and plain bindings (vx),
  ;; then wraps the skeletons around the payload (chez:let/chez:letrec (vx ...) body).
  (define-syntax i:let
    (syntax-rules (values)
      ;; no more binding specs -> wrap the accumulated skeletons
      ((i:let "bs" rec (cwv ...) vxs body ())
       (i:let "wrap" rec vxs body cwv ...))
      ;; ((values) x) -> dummy binding, evaluate x for its side-effect only
      ((i:let "bs" rec cwvs (vx ...) body (((values) x) bs ...))
       (i:let "bs" rec cwvs (vx ... (dummy (begin x #f))) body (bs ...)))
      ;; ((values v) x) single variable -> ordinary binding
      ((i:let "bs" rec cwvs (vx ...) body (((values v) x) bs ...))
       (i:let "bs" rec cwvs (vx ... (v x)) body (bs ...)))
      ;; ((values v ...) x) no rest -> generate a cwv skeleton
      ((i:let "bs" rec cwvs vxs body (((values v ...) x) bs ...))
       (i:let "form1" rec cwvs vxs body (bs ...) (x ()) (values v ...)))
      ;; ((values . vs) x) with rest -> generate a cwv skeleton
      ((i:let "bs" rec cwvs vxs body (((values . vs) x) bs ...))
       (i:let "form1+" rec cwvs vxs body (bs ...) (x ()) (values . vs)))
      ;; (v x) positional single variable -> ordinary binding
      ((i:let "bs" rec cwvs (vx ...) body ((v x) bs ...))
       (i:let "bs" rec cwvs (vx ... (v x)) body (bs ...)))
      ;; (b1 b2 b3 b ...) positional, >=2 variables -> rewrite to form1
      ((i:let "bs" rec cwvs vxs body ((b1 b2 b3 b ...) bs ...))
       (i:let "form2" rec cwvs vxs body (bs ...) (b1 b2) (b3 b ...)))

      ;; form1: consume the variables of a no-rest values form into temporaries
      ((i:let "form1" rec (cwv ...) vxs body bss (x ts) (values))
       (i:let "bs" rec (cwv ... (x ts)) vxs body bss))
      ((i:let "form1" rec cwvs (vx ...) body bss (x (t ...)) (values v1 v ...))
       (i:let "form1" rec cwvs (vx ... (v1 t1)) body bss (x (t ... t1)) (values v ...)))

      ;; form1+: consume the variables of a values form that has a rest argument
      ((i:let "form1+" rec cwvs (vx ...) body bss (x (t ...)) (values v1 v2 . vs))
       (i:let "form1+" rec cwvs (vx ... (v1 t1)) body bss (x (t ... t1)) (values v2 . vs)))
      ((i:let "form1+" rec (cwv ...) (vx ...) body bss (x (t ...)) (values v1 . vr))
       (i:let "bs" rec (cwv ... (x (t ... t1 . tr))) (vx ... (v1 t1) (vr tr)) body bss))
      ((i:let "form1+" rec (cwv ...) (vx ...) body bss (x ()) (values . vr))
       (i:let "bs" rec (cwv ... (x tr)) (vx ... (vr tr)) body bss))

      ;; form2: fold positional (v ... b ... x) into ((values v ... b ...) x)
      ((i:let "form2" rec cwvs vxs body (bs ...) (v ...) (x))
       (i:let "bs" rec cwvs vxs body (((values v ...) x) bs ...)))
      ((i:let "form2" rec cwvs vxs body bss (v ...) (b1 b2 b ...))
       (i:let "form2" rec cwvs vxs body bss (v ... b1) (b2 b ...)))

      ;; wrap: emit the actual code (let for #f, letrec for #t)
      ((i:let "wrap" #f vxs body)
       (chez:let vxs . body))
      ((i:let "wrap" #f vxs body (x formals) cwv ...)
       (call-with-values
         (lambda () x)
         (lambda formals (i:let "wrap" #f vxs body cwv ...))))
      ((i:let "wrap" #t vxs body)
       (chez:letrec vxs . body))
      ((i:let "wrap" #t ((v t) ...) body cwv ...)
       (chez:let ((v i:undefined) ...)
         (i:let "wraprec" ((v t) ...) body cwv ...)))

      ;; wraprec: the letrec case -- fill user vars from the consumer temporaries
      ((i:let "wraprec" ((v t) ...) (body ...))
       (begin (set! v t) ... (chez:let () body ...)))
      ((i:let "wraprec" vxs body (x formals) cwv ...)
       (call-with-values
         (lambda () x)
         (lambda formals (i:let "wraprec" vxs body cwv ...))))))

  (define-syntax i:named-let
    (syntax-rules (values)
      ((i:named-let tag vxs body ())
       (chez:let tag vxs . body))
      ((i:named-let tag (vx ...) body (((values v) x) bs ...))
       (i:named-let tag (vx ... (v x)) body (bs ...)))
      ((i:named-let tag (vx ...) body ((v x) bs ...))
       (i:named-let tag (vx ... (v x)) body (bs ...)))))

  ;; --- SRFI 71 value-destructuring procedures --------------------------------

  (define (uncons pair)  (values (car pair) (cdr pair)))
  (define (uncons-2 lst) (values (car lst) (cadr lst) (cddr lst)))
  (define (uncons-3 lst) (values (car lst) (cadr lst) (caddr lst) (cdddr lst)))
  (define (uncons-4 lst) (values (car lst) (cadr lst) (caddr lst) (cadddr lst) (cddddr lst)))
  (define (unlist lst)   (apply values lst))
  (define (unvector vec) (apply values (vector->list vec)))

  ;; values->list / values->vector are macros: they thunk their argument so its
  ;; multiple values survive to the consumer (a procedure would collapse them).
  (define-syntax values->list
    (syntax-rules () ((values->list x) (call-with-values (lambda () x) list))))
  (define-syntax values->vector
    (syntax-rules () ((values->vector x) (call-with-values (lambda () x) vector))))
)
