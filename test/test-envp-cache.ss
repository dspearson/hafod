;;; test/test-envp-cache.ss -- The marshalled envp (the KEY=VALUE string list
;;; handed to posix_spawnp) is cached and rebuilt only when the environment
;;; actually changed, not once per spawn.
;;;
;;; Two witnesses, both deterministic and PTY-free:
;;;
;;;   1. A routed rebuild count through envp-rebuild-observer (the proven
;;;      spawn-release-observer idiom). Two consecutive spawns with an
;;;      unchanged environment must rebuild the list exactly ONCE -- the second
;;;      reuses the cache. A naive per-spawn rebuild counts two, so this is RED
;;;      before the cache lands and records the before/after (2 -> 1). A setenv
;;;      between the two spawns is a real change and forces a second rebuild,
;;;      proving the cache invalidates rather than silently going stale.
;;;
;;;   2. A byte-identity oracle: after every environment-mutation path (setenv
;;;      add, setenv delete, a wholesale alist->env replace, and both the enter
;;;      and the restore of a with-total-env*) the cached list must equal the
;;;      naive (alist->env-list (env->alist)) recompute. A stale cache -- a
;;;      mutation path that forgot to invalidate -- would hand a child the wrong
;;;      environment (a leaked or hidden variable); this oracle is the security
;;;      regression guard for that trust boundary.
;;;
;;; The observer and cached-env-strings are imported DIRECTLY from
;;; (hafod environment), never through the (hafod) umbrella -- they are
;;; sub-library-only bindings, exactly like spawn-release-observer.
;;;
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (chezscheme)
        (only (hafod environment)
              cached-env-strings envp-rebuild-observer
              setenv env->alist alist->env alist->env-list with-total-env*))

(test-begin "envp-cache")

;; The cached KEY=VALUE list must always agree with a fresh naive recompute
;; from the current Scheme environment. Calling cached-env-strings here also
;; settles the cache (clears the dirty flag), so each caller below re-dirties
;; via its own mutation before asserting.
(define (cache-matches-naive?)
  (equal? (cached-env-strings) (alist->env-list (env->alist))))

;; --- Witness 1: routed rebuild count (records the before/after 2 -> 1) ---

;; A setenv forces the dirty flag, so the first spawn rebuilds and the second
;; reuses: exactly one rebuild across two unchanged-environment spawns. A
;; per-spawn rebuild (the pre-cache behaviour) would fire the observer twice.
(test-assert "two spawns with an unchanged environment trigger exactly one envp rebuild"
  (let ([rebuilds 0])
    (parameterize ([envp-rebuild-observer (lambda () (set! rebuilds (+ rebuilds 1)))])
      (setenv "HAFOD_ENVP_PROBE_A" "1")   ; a real change: cache now dirty
      (cached-env-strings)                 ; spawn 1: rebuild (fires once)
      (cached-env-strings)                 ; spawn 2: unchanged env -> reuse
      (= rebuilds 1))))

;; A setenv between the two spawns is a genuine change and must invalidate the
;; cache, so the second spawn rebuilds again: two rebuilds, not one.
(test-assert "a setenv between two spawns forces a second rebuild"
  (let ([rebuilds 0])
    (parameterize ([envp-rebuild-observer (lambda () (set! rebuilds (+ rebuilds 1)))])
      (setenv "HAFOD_ENVP_PROBE_B" "1")   ; dirty
      (cached-env-strings)                 ; rebuild 1
      (setenv "HAFOD_ENVP_PROBE_B" "2")   ; changed again -> dirty
      (cached-env-strings)                 ; rebuild 2
      (= rebuilds 2))))

;; --- Witness 2: byte-identity oracle across every mutation path ---

(test-assert "cached list matches the naive recompute after a setenv add"
  (begin
    (setenv "HAFOD_ENVP_ADD" "added")
    (cache-matches-naive?)))

(test-assert "cached list matches the naive recompute after a setenv delete"
  (begin
    (setenv "HAFOD_ENVP_ADD" #f)
    (cache-matches-naive?)))

;; A wholesale alist->env replace routes through set-environ!; snapshot and
;; restore the Scheme environment so the replace does not leak into later
;; assertions.
(test-assert "cached list matches the naive recompute after a wholesale alist->env"
  (let ([saved (env->alist)])
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (alist->env '(("HAFOD_ENVP_WHOLE" . "w")))
        (cache-matches-naive?))
      (lambda () (alist->env saved)))))

;; with-total-env* mutates on enter and restores on exit; the cache must be
;; correct in BOTH directions. Keep the rest of the environment in the delta so
;; the sync-to-os on enter/exit stays gentle.
(test-assert "cached list matches the naive recompute inside a with-total-env* body"
  (with-total-env* (cons '("HAFOD_ENVP_TOTAL" . "t") (env->alist))
    (lambda () (cache-matches-naive?))))

(test-assert "cached list matches the naive recompute after a with-total-env* restores"
  (begin
    (with-total-env* (cons '("HAFOD_ENVP_TOTAL2" . "t2") (env->alist))
      (lambda () #t))
    (cache-matches-naive?)))

(test-end)
