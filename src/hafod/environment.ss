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
    align-env! read-environ-fresh
    ;; Internal, exposed for the O(n) sync-diff equivalence/timing test only
    ;; (deliberately absent from the (hafod) umbrella).
    env-keys-to-unset
    ;; envp cache accessor + rebuild-count seam, for process.ss and the cache
    ;; test only (also deliberately absent from the (hafod) umbrella).
    cached-env-strings envp-rebuild-observer)

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

  ;; O(1) getenv index: variable name -> value.  A parallel lookup structure;
  ;; the %environ alist above remains the ordering source of truth, because Chez
  ;; hashtable enumeration is hash order, not insertion order.  env->alist keeps
  ;; returning the alist verbatim, so the child envp byte order is unchanged.
  (define %env-index (make-hashtable string-hash string=?))

  ;; rebuild-index!: repopulate the whole index from ALIST.  Reverse-iterate so
  ;; the FRONTMOST (assoc-winning) value survives when the alist carries
  ;; duplicate keys -- matching getenv's assoc-first semantics exactly.
  (define (rebuild-index! alist)
    (hashtable-clear! %env-index)
    (for-each (lambda (p) (hashtable-set! %env-index (car p) (cdr p)))
              (reverse alist)))

  ;; ======================================================================
  ;; envp cache: the KEY=VALUE string list handed to posix_spawnp
  ;; ======================================================================

  ;; A DEDICATED dirty flag, kept strictly separate from %env-dirty? below.
  ;; %env-dirty? tracks OS-sync and is reset by align-env! after a sync -- a
  ;; different event from "the environment changed since the last envp build".
  ;; Reusing it would serve a child a stale envp.  This flag is set at EVERY
  ;; %environ write (the set-environ! write-path plus both setenv branches) and
  ;; is cleared only when cached-env-strings rebuilds.  It starts dirty so the
  ;; first spawn builds.
  (define %envp-dirty? #t)

  ;; The cached KEY=VALUE string list (the spawn-env-strings result), or #f
  ;; before the first build.  Only this Scheme list is cached; the C char** envp
  ;; is still built and freed per spawn inside posix-spawnp*.
  (define %envp-cache #f)

  ;; Injectable rebuild counter, fired once per real rebuild.  Defaults to #f so
  ;; the real spawn path is byte-for-byte unchanged, mirroring
  ;; spawn-release-observer.  A test parameterizes it to count rebuilds.
  (define envp-rebuild-observer (make-parameter #f))

  ;; cached-env-strings: rebuild the KEY=VALUE list only when %envp-dirty?, clear
  ;; the flag, and fire the observer; otherwise return the cached list.  An
  ;; unchanged-environment spawn loop rebuilds once, not once per spawn, with the
  ;; child environment byte-identical to (alist->env-list (env->alist)).
  (define (cached-env-strings)
    (when %envp-dirty?
      (set! %envp-cache (alist->env-list (%environ)))
      (set! %envp-dirty? #f)
      (let ([obs (envp-rebuild-observer)]) (when obs (obs))))
    %envp-cache)

  ;; set-environ!: the single write-path for a WHOLESALE environment
  ;; replacement.  It keeps %environ and %env-index consistent by construction,
  ;; so a wholesale write cannot leave the index stale.  alist->env and both
  ;; with-total-env* writes route through here; setenv updates incrementally.
  (define (set-environ! alist)
    (%environ alist)
    (rebuild-index! alist)
    (set! %envp-dirty? #t))         ; invalidate the envp cache (dedicated flag)

  ;; Dirty flag: #t when the Scheme alist has diverged from the OS
  ;; environment via alist->env or with-total-env*.  setenv/getenv
  ;; keep the OS in sync directly, so alignment is only needed when
  ;; the alist was replaced wholesale.
  (define %env-dirty? #f)

  ;; Fresh read from OS (for alignment checking)
  (define (read-environ-fresh) (read-environ))

  ;; ======================================================================
  ;; Public API
  ;; ======================================================================

  ;; getenv: O(1) lookup in the index.  Env values are always strings, so the
  ;; #f default is an unambiguous "absent" -- identical to the old assoc scan.
  (define (getenv var)
    (hashtable-ref %env-index var #f))

  ;; setenv: update the Scheme-side alist, the O(1) index, and the OS.
  ;; If val is #f, delete the variable.  The incremental single-key index update
  ;; mirrors the wholesale rebuild: on set the key takes the new value (the front
  ;; of the alist wins); on delete the single cell is removed.  The
  ;; posix-setenv/posix-unsetenv calls are left exactly as they were.
  (define (setenv var val)
    (if val
        (begin
          (%environ (alist-update var val (%environ)))
          (hashtable-set! %env-index var val)
          (set! %envp-dirty? #t)         ; invalidate the envp cache (dedicated flag)
          (posix-setenv var val #t))
        (begin
          (%environ (alist-delete var (%environ)))
          (hashtable-delete! %env-index var)
          (set! %envp-dirty? #t)         ; invalidate the envp cache (dedicated flag)
          (posix-unsetenv var))))

  ;; env->alist: return the current Scheme-side environment as an alist.
  (define (env->alist)
    (%environ))

  ;; alist->env: replace the Scheme-side environment with the given alist.
  ;; Routes through set-environ! so the index is rebuilt with it.
  ;; Does NOT immediately sync to OS -- use align-env! or with-resources-aligned.
  (define (alist->env alist)
    (set-environ! alist)
    (set! %env-dirty? #t))

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
  ;; Both the enter and restore writes route through set-environ! so the index
  ;; tracks the wholesale replacement in each direction.
  (define (with-total-env* alist thunk)
    (let ([saved (%environ)])
      (dynamic-wind
        (lambda ()
          (set-environ! alist)
          (sync-env-to-os! alist)
          (set! %env-dirty? #f))
        thunk
        (lambda ()
          (set-environ! saved)
          (sync-env-to-os! saved)
          (set! %env-dirty? #f)))))

  ;; ======================================================================
  ;; OS synchronization
  ;; ======================================================================

  ;; env-keys-to-unset: pure diff.  Given the OS environment alist OS-ALIST and
  ;; the WANTED alist WANT-ALIST, return the OS pairs whose key is absent from
  ;; WANT-ALIST -- the vars to unset so the OS matches WANT-ALIST.  A membership
  ;; hashtable over the wanted keys replaces the per-OS-var (assoc key want)
  ;; scan, making the diff O(n+m) rather than O(n*m).  Pure: it touches no OS
  ;; state, so it yields the identical unset-set as the naive assoc reference and
  ;; can be exercised on large synthetic inputs by the timing witness.
  (define (env-keys-to-unset os-alist want-alist)
    (let ([want (make-hashtable string-hash string=?)])
      (for-each (lambda (p) (hashtable-set! want (car p) #t)) want-alist)
      (filter (lambda (op) (not (hashtable-contains? want (car op)))) os-alist)))

  ;; sync-env-to-os!: make the OS environment match the given alist exactly.
  ;; 1. Read the current OS environment
  ;; 2. Unset every OS var absent from the alist (O(n+m) membership diff)
  ;; 3. Set every alist pair, in alist order
  ;; No clearenv (absent on macOS); no new FFI -- the same unset-set and the same
  ;; set order as the prior per-var assoc scan, so the OS end-state is identical.
  (define (sync-env-to-os! alist)
    (for-each
      (lambda (os-pair) (posix-unsetenv (car os-pair)))
      (env-keys-to-unset (read-environ) alist))
    (for-each
      (lambda (pair) (posix-setenv (car pair) (cdr pair) #t))
      alist))

  ;; ======================================================================
  ;; Resource alignment
  ;; ======================================================================

  ;; align-env!: sync OS environment to match Scheme-side alist.
  ;; Called by with-resources-aligned before fork/exec.
  ;; Skips the expensive sync when the OS is already in sync (setenv/getenv
  ;; update both the alist and the OS directly).
  (define (align-env!)
    (when %env-dirty?
      (sync-env-to-os! (%environ))
      (set! %env-dirty? #f)))

  ;; Resource descriptor for use with with-resources-aligned.
  ;; This is a cons pair (name . align-thunk) for v1 simplicity.
  ;; The resource record type is defined in process-state.ss
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

  ;; Populate the index once from the environment captured at load, mirroring
  ;; fd-ports.ss's (init-fdports!) auto-initialisation.  Placed at the end of the
  ;; body so it runs after every definition above; nothing mutates %environ
  ;; before this, so (%environ) is still the load-time (read-environ) value.
  (rebuild-index! (%environ))

  ) ; end library
