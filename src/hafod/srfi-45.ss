#!chezscheme
;;; (hafod srfi-45) -- SRFI-45: Primitives for Expressing Iterative Lazy Algorithms
;;; Reference: https://srfi.schemers.org/srfi-45/srfi-45.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Chez provides delay/force; we add lazy and eager for full SRFI-45.

(library (hafod srfi-45)
  (export delay force lazy eager)
  (import (chezscheme))

  ;; eager: evaluate the argument eagerly, then wrap the value as a promise.
  ;; The spec makes (eager expr) equivalent to (let ((value expr)) (delay value)),
  ;; so the argument's side effects run at construction rather than at force.
  (define-syntax eager
    (syntax-rules ()
      ((_ expr)
       (let ((v expr)) (delay v)))))

  ;; lazy: like delay but for expressions that return promises (iterative forcing).
  ;;
  ;; DELIBERATE DIVERGENCE: hafod's lazy is built on Chez promises and does not
  ;; provide the specification's bounded-space representation (the boxed-promise
  ;; type with a trampolining force that keeps iterative lazy algorithms in
  ;; constant space). Providing that needs a full replacement promise type, which
  ;; is out of scope for a behaviour-preserving point release; lazy is retained
  ;; as-is. eager, above, is corrected because it is a one-line semantic fix.
  (define-syntax lazy
    (syntax-rules ()
      ((_ expr)
       (delay (force expr))))))
