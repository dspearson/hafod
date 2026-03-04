;;; (hafod environment) -- Environment variable management for hafod
;;; Provides getenv, setenv, env->alist, alist->env, with-env*, with-total-env*,
;;; and environ-resource for resource alignment.
;;; Ported from scsh/scheme/environment.scm
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod environment)
  (export
    getenv setenv env->alist alist->env
    with-env* with-total-env* with-env with-total-env
    environ-resource
    ;; Alist utilities (now public)
    alist-update alist-delete alist->env-list alist-compress
    add-before add-after
    ;; Internal, for resource alignment
    align-env! read-environ-fresh)

  (import (hafod internal base) (hafod posix) (hafod compat))

  ;; ======================================================================
  ;; Internal helpers
  ;; ======================================================================

  ;; alist-update: add or replace key in alist
  (define (alist-update key val alist)
    (cons (cons key val)
          (alist-delete key alist)))

  ;; alist-delete: remove all entries with given key
  (define (alist-delete key alist)
    (filter (lambda (pair) (not (string=? (car pair) key))) alist))

  ;; ======================================================================
  ;; Additional alist utilities
  ;; ======================================================================

  ;; alist->env-list: convert alist to list of "KEY=VALUE" strings.
  (define (alist->env-list alist)
    (map (lambda (p) (string-append (car p) "=" (cdr p))) alist))

  ;; alist-compress: remove duplicate keys from alist, keeping first occurrence.
  (define (alist-compress alist)
    (let loop ([rest alist] [seen '()] [acc '()])
      (if (null? rest)
          (reverse acc)
          (let ([key (caar rest)])
            (if (member key seen)
                (loop (cdr rest) seen acc)
                (loop (cdr rest) (cons key seen) (cons (car rest) acc)))))))

  ;; Split a colon-separated string into a list of strings.
  (define (env-split-colon str)
    (let ([len (string-length str)])
      (if (zero? len) '()
          (let loop ([i 0])
            (let scan ([j i])
              (cond
                [(= j len) (list (substring str i len))]
                [(char=? (string-ref str j) #\:)
                 (cons (substring str i j) (loop (+ j 1)))]
                [else (scan (+ j 1))]))))))

  ;; Join a list of strings with colon separator.
  (define (env-join-colon lst)
    (if (null? lst) ""
        (let loop ([rest (cdr lst)] [acc (car lst)])
          (if (null? rest) acc
              (loop (cdr rest) (string-append acc ":" (car rest)))))))

  ;; add-before: insert NEW before REF in a colon-separated string.
  ;; If REF not found, append NEW at the end.
  (define (add-before new ref str)
    (let ([parts (env-split-colon str)])
      (let loop ([rest parts] [acc '()])
        (cond
          [(null? rest)
           ;; ref not found -- append new
           (env-join-colon (append (reverse acc) (list new)))]
          [(string=? (car rest) ref)
           (env-join-colon (append (reverse acc) (list new) rest))]
          [else
           (loop (cdr rest) (cons (car rest) acc))]))))

  ;; add-after: insert NEW after REF in a colon-separated string.
  ;; If REF not found, append NEW at the end.
  (define (add-after new ref str)
    (let ([parts (env-split-colon str)])
      (let loop ([rest parts] [acc '()])
        (cond
          [(null? rest)
           ;; ref not found -- append new
           (env-join-colon (append (reverse acc) (list new)))]
          [(string=? (car rest) ref)
           (env-join-colon (append (reverse acc) (list (car rest) new) (cdr rest)))]
          [else
           (loop (cdr rest) (cons (car rest) acc))]))))

  ;; ======================================================================
  ;; Environment parameter
  ;; ======================================================================

  ;; The Scheme-side environment is stored as an alist in a parameter.
  ;; Initialized from the C environ global at library load time.
  (define %environ (make-parameter (read-environ)))

  ;; Fresh read from OS (for alignment checking)
  (define (read-environ-fresh) (read-environ))

  ;; ======================================================================
  ;; Public API
  ;; ======================================================================

  ;; getenv: look up a variable in the Scheme-side alist.
  (define (getenv var)
    (let ([pair (assoc var (%environ))])
      (and pair (cdr pair))))

  ;; setenv: update both the Scheme-side alist and the OS.
  ;; If val is #f, delete the variable.
  (define (setenv var val)
    (if val
        (begin
          (%environ (alist-update var val (%environ)))
          (posix-setenv var val #t))
        (begin
          (%environ (alist-delete var (%environ)))
          (posix-unsetenv var))))

  ;; env->alist: return the current Scheme-side environment as an alist.
  (define (env->alist)
    (%environ))

  ;; alist->env: replace the Scheme-side environment with the given alist.
  ;; Does NOT immediately sync to OS -- use align-env! or with-resources-aligned.
  (define (alist->env alist)
    (%environ alist))

  ;; ======================================================================
  ;; Dynamic scoping
  ;; ======================================================================

  ;; with-env*: merge delta alist into current environment, run thunk, restore.
  ;; Delta is an alist of (var . val) pairs to add/override.
  (define (with-env* alist-delta thunk)
    (let ([new-env (fold-left (lambda (env pair)
                                (alist-update (car pair) (cdr pair) env))
                              (env->alist)
                              alist-delta)])
      (with-total-env* new-env thunk)))

  ;; with-total-env*: replace entire environment, run thunk, restore.
  (define (with-total-env* alist thunk)
    (let ([saved (%environ)])
      (dynamic-wind
        (lambda ()
          (%environ alist)
          (sync-env-to-os! alist))
        thunk
        (lambda ()
          (%environ saved)
          (sync-env-to-os! saved)))))

  ;; ======================================================================
  ;; OS synchronization
  ;; ======================================================================

  ;; sync-env-to-os!: make the OS environment match the given alist exactly.
  ;; 1. Read the current OS environment
  ;; 2. For each var in OS but not in alist: unsetenv
  ;; 3. For each var in alist: setenv
  (define (sync-env-to-os! alist)
    (let ([os-env (read-environ)])
      ;; Remove vars that are in OS but not in alist
      (for-each
        (lambda (os-pair)
          (unless (assoc (car os-pair) alist)
            (posix-unsetenv (car os-pair))))
        os-env)
      ;; Set all vars from alist
      (for-each
        (lambda (pair)
          (posix-setenv (car pair) (cdr pair) #t))
        alist)))

  ;; ======================================================================
  ;; Resource alignment
  ;; ======================================================================

  ;; align-env!: sync OS environment to match Scheme-side alist.
  ;; Called by with-resources-aligned before fork/exec.
  (define (align-env!)
    (sync-env-to-os! (%environ)))

  ;; Resource descriptor for use with with-resources-aligned.
  ;; This is a cons pair (name . align-thunk) for v1 simplicity.
  ;; Phase 4 Plan 02 defines the resource record type in process-state.ss
  ;; and with-resources-aligned handles both resource records and cons pairs.
  (define environ-resource
    (cons 'environ align-env!))

  ;; ======================================================================
  ;; Syntax sugar
  ;; ======================================================================

  ;; with-env sugar: delta should be a quoted or literal alist expression.
  ;; (with-env ((var . val) ...) body ...) expands to
  ;; (with-env* (list (cons var val) ...) (lambda () body ...))
  ;; For simplicity, user passes a quoted alist:
  ;; (with-env '(("K" . "V")) body ...)
  (define-simple-syntax (with-env delta body ...)
    (with-env* delta (lambda () body ...)))

  (define-simple-syntax (with-total-env env body ...)
    (with-total-env* env (lambda () body ...)))

  ) ; end library
