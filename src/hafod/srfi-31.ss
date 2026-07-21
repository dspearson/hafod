#!chezscheme
;;; (hafod srfi-31) -- SRFI-31: A special form rec for recursive evaluation
;;; Reference: https://srfi.schemers.org/srfi-31/srfi-31.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; hafod defines its own two-clause rec (value form and procedure form),
;;; shadowing Chez's native value-only rec.

(library (hafod srfi-31)
  (export rec)
  (import (except (chezscheme) rec))

  ;; rec binds a name to the value being computed so it can refer to itself.
  ;; The procedure-form clause must be listed first: otherwise the value-form
  ;; clause would capture (name . formals) as the bound variable and emit an
  ;; invalid letrec. The value form stays letrec-based, identical in behaviour
  ;; to Chez's native rec.
  (define-syntax rec
    (syntax-rules ()
      ((rec (name . formals) body ...)
       (letrec ((name (lambda formals body ...))) name))
      ((rec name expr)
       (letrec ((name expr)) name)))))
