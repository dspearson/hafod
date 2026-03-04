;;; (hafod exit-hooks) -- Exit hook registration and execution
;;; Users register thunks that run before process exit.
;;; Ported from scsh/scheme/scsh-package.scm lines 842-853.
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026 Dominic Pearson.

(library (hafod exit-hooks)
  (export add-exit-hook! call-exit-hooks-and-run call-exit-hooks!)
  (import (chezscheme))

  ;; Vector-box pattern for mutable state in R6RS libraries.
  ;; Index 0 holds the list of exit hook thunks.
  (define *hooks* (vector '()))

  ;; Register a zero-argument thunk to be called before exit.
  ;; Hooks are prepended (LIFO order: last registered runs first).
  (define (add-exit-hook! thunk)
    (vector-set! *hooks* 0 (cons thunk (vector-ref *hooks* 0))))

  ;; Run all registered exit hooks in order (LIFO).
  (define (call-exit-hooks!)
    (for-each (lambda (thunk) (thunk)) (vector-ref *hooks* 0)))

  ;; Run all exit hooks, then execute the given thunk.
  ;; Used by exit to run hooks before the actual _exit call.
  (define (call-exit-hooks-and-run thunk)
    (call-exit-hooks!)
    (thunk))

) ;; end library
