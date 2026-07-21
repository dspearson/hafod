;;; test-srfi-17.ss -- Spot-check tests for the generalised set! library (SRFI 17).
;;;
;;; SRFI 17 generalises set! so that (set! (getter args …) value) invokes the
;;; getter's registered setter, and getter-with-setter pairs a getter with its
;;; setter. This suite spot-checks the working surface: the built-in
;;; getter/setter pairs (car, cdr, vector-ref, unbox), a user getter/setter
;;; pair, plain variable set!, the composed-accessor setters (caar … cddddr),
;;; and the settable-setter idiom (set! (setter proc) new-setter) -- registering
;;; a setter THROUGH set! -- so that setter is itself a settable location.
;;; Copyright (c) 2026, hafod contributors.

;; The library redefines set! as the generalised form; take it from the
;; library under test and everything else from chezscheme.
(import (test runner) (except (chezscheme) set!) (hafod srfi-17))

(test-begin "srfi-17")

;; ===========================================================================
;; Section A -- generalised set! through the built-in getter/setter pairs
;; ===========================================================================

(define pr (cons 1 2))
(set! (car pr) 10)
(set! (cdr pr) 20)
(test-equal "set! on (car pair) invokes set-car!" 10 (car pr))
(test-equal "set! on (cdr pair) invokes set-cdr!" 20 (cdr pr))

(define vv (vector 0 0 0))
(set! (vector-ref vv 1) 99)
(test-equal "set! on (vector-ref v i) invokes vector-set!" 99 (vector-ref vv 1))

(define bx (box 5))
(set! (unbox bx) 42)
(test-equal "set! on (unbox b) invokes set-box!" 42 (unbox bx))

;; ===========================================================================
;; Section B -- a user getter-with-setter round-trips
;; ===========================================================================
;; getter-with-setter returns the getter after registering its setter, so the
;; accessor below both reads storage and, through set!, writes it.

(define storage (vector #f))
(define accessor
  (getter-with-setter (lambda () (vector-ref storage 0))
                      (lambda (v) (vector-set! storage 0 v))))

(set! (accessor) 'hello)
(test-equal "a user getter-with-setter writes through generalised set!"
            'hello (accessor))
(test-assert "getter-with-setter returns a callable getter" (procedure? accessor))

;; ===========================================================================
;; Section C -- plain variable set! still works
;; ===========================================================================

(test-equal "plain variable set! is unaffected by the generalisation"
            2 (let ((n 1)) (set! n 2) n))

;; ===========================================================================
;; Section D -- composed-accessor setters (caar … cddddr)
;; ===========================================================================
;; SRFI 17 settable composed accessors: (set! (caXXr x) v) == (set-car! (cXXr x) v)
;; and (set! (cdXXr x) v) == (set-cdr! (cXXr x) v), for every 2-to-4-deep c[ad]+r.

(test-equal "set! on (cadr p) mutates via the composed setter"
            '(1 9 3) (let ((p (list 1 2 3))) (set! (cadr p) 9) p))
(test-equal "set! on (caddr p) mutates the third car"
            '(1 2 9) (let ((p (list 1 2 3))) (set! (caddr p) 9) p))

;; ===========================================================================
;; Section E -- the settable-setter idiom (set! (setter g) s)
;; ===========================================================================
;; SRFI 17 makes setter itself settable: (set! (setter proc) new) registers
;; new as proc's setter, so a subsequent (set! (proc …) v) writes through it.

(define store (vector #f))
(define g (lambda () (vector-ref store 0)))
(set! (setter g) (lambda (v) (vector-set! store 0 v)))
(set! (g) 'wired)
(test-equal "a setter registered through (set! (setter g) s) then writes"
            'wired (g))

(test-end)
