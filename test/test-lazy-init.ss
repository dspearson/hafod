;;; test/test-lazy-init.ss -- The two interactive-only load-time side effects are
;;; deferred to first use.  Importing the two leaf libraries must NOT dlopen
;;; libsqlite3 or register the built-in completers; the first real use of each
;;; wrapper flips its probe.  A naive tree that resolves the FFI bindings or
;;; registers the completers at library instantiation makes the first two
;;; assertions fail, so this suite is non-vacuous by construction.
;;; Entirely in-process: an in-memory database and a registry lookup, no
;;; terminal, so it runs identically on every platform.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod editor sqlite3) sqlite3-loaded? sqlite3-open)
        (only (hafod shell completers) completers-registered? lookup-completer)
        ;; Importing (hafod editor editor) instantiates its body -- the same
        ;; boot path bin/hafod takes for every invocation.  A binding is pulled
        ;; in so the library is genuinely invoked, not merely visited.
        (only (hafod editor editor) read-expression))

(test-begin "lazy-init")

;; Importing the two leaf libraries instantiates their bodies.  After the
;; deferral neither body performs its side effect, so both probes read false.
(test-assert "sqlite3 not loaded at import" (not (sqlite3-loaded?)))
(test-assert "completers not registered at import" (not (completers-registered?)))

;; The interactive editor's history handle is opened lazily too.  bin/hafod
;; imports (hafod editor editor) for EVERY invocation -- including a non-
;; interactive -c/-s that only evaluates an expression -- so instantiating that
;; library must NOT open the history database, because that would dlopen
;; libsqlite3 at boot.  Referencing read-expression above forced the body to
;; run; the boot probe must still read both-false.  A tree that opens history
;; at editor instantiation loads sqlite here, flipping the pair to (#t #f), so
;; this assertion is non-vacuous.  Kept ahead of the sqlite3-open below, which
;; loads the shared object for the rest of the process.
(test-assert "editor library instantiates without a live history handle"
  (procedure? read-expression))
(test-equal "a non-interactive boot leaves sqlite unloaded and completers unregistered"
  '(#f #f)
  (list (sqlite3-loaded?) (completers-registered?)))

;; First real use of a sqlite3 wrapper resolves the shared object on demand.
(let ([db (sqlite3-open ":memory:")])
  (test-assert "opening a db loads sqlite3" (sqlite3-loaded?)))

;; First lookup registers the built-in completers exactly once, on demand.
(lookup-completer "git")
(test-assert "first lookup registers built-ins" (completers-registered?))
(test-assert "git completer is now present" (procedure? (lookup-completer "git")))

(test-end)
