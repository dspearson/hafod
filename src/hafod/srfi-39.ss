#!chezscheme
;;; (hafod srfi-39) -- SRFI-39: Parameter objects
;;; Reference: https://srfi.schemers.org/srfi-39/srfi-39.html
;;; Copyright (c) 2026 Dominic Pearson.
;;;
;;; Chez provides make-parameter and parameterize natively, but its parameterize
;;; re-runs the converter when it restores the outer value on exit. SRFI 39
;;; restores the saved value verbatim: the converter fires only when a value is
;;; installed, never again on restore. A non-idempotent converter therefore does
;;; not compound across a parameterize (5 with (* x 10) is 50 before, 30 inside,
;;; and 50 again after -- not 500). This library supplies conformant forms of its
;;; own so callers of (hafod srfi-39) get that behaviour; the rest of hafod uses
;;; Chez's native parameters directly and is unaffected.
;;;
;;; A parameter is a procedure over an underlying Chez parameter used as a raw
;;; storage cell -- created WITHOUT a converter, so the cell itself never
;;; converts. This library owns the converter: it is applied once to the initial
;;; value and once to each publicly set value. Two private, un-exported tokens
;;; select the internal operations parameterize needs -- reading the converter's
;;; result for a candidate value, and setting the cell raw -- so restore can
;;; reinstall a saved value without re-converting it.

(library (hafod srfi-39)
  (export make-parameter parameterize)
  (import (rename (chezscheme)
                  (make-parameter chez:make-parameter)
                  (parameterize   chez:parameterize)))

  ;; eq?-unique operation selectors. Never exported and never a legal user value
  ;; (a fresh list), so they cannot collide with anything passed to a parameter.
  (define param-set!    (list 'srfi-39-set!))     ; (p param-set! v)    -> store v raw
  (define param-convert (list 'srfi-39-convert))  ; (p param-convert v) -> (converter v)

  ;; (make-parameter init) / (make-parameter init converter)
  ;;   0 args           -> current value
  ;;   1 arg  x         -> public set: store (converter x)
  ;;   2 args op v      -> internal: raw set, or convert-without-store
  (define make-parameter
    (case-lambda
      ((init) (make-parameter init (lambda (x) x)))
      ((init converter)
       (let ((cell (chez:make-parameter (converter init))))
         (case-lambda
           (() (cell))
           ((x) (cell (converter x)))
           ((op v)
            (cond
              ((eq? op param-set!)    (cell v))
              ((eq? op param-convert) (converter v))
              (else
               (assertion-violation 'parameter
                 "not a valid parameter operation" op)))))))))

  ;; parameterize converts each new value ONCE, on entry, then installs and
  ;; restores raw via dynamic-wind so escapes and re-entries are handled and the
  ;; converter never re-fires. Values and parameters are evaluated once, up front.
  (define-syntax parameterize
    (syntax-rules ()
      ((_ () body1 body2 ...)
       (let () body1 body2 ...))
      ((_ ((param val) ...) body1 body2 ...)
       (let* ((params (list param ...))
              (news   (map (lambda (p v) (p param-convert v))
                           params (list val ...)))
              (olds   '()))
         (dynamic-wind
           (lambda ()
             (set! olds (map (lambda (p) (p)) params))
             (for-each (lambda (p n) (p param-set! n)) params news))
           (lambda () body1 body2 ...)
           (lambda ()
             (for-each (lambda (p o) (p param-set! o)) params olds))))))))
