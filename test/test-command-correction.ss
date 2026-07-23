;;; test/test-command-correction.ss -- Suggest-only command-head correction.
;;; A new toggle command-correction? (default off) makes a Tab on an UNRESOLVABLE
;;; command head surface the nearest known command as a "did you mean X?" hint,
;;; ranked by the transposition-aware bounded edit distance over the in-memory
;;; command universe (PATH-cache keys + builtins + aliases).  It is SUGGEST-ONLY:
;;; the hint never rewrites the buffer and never runs anything -- pressing Enter
;;; runs the typed command exactly as typed, and only a deliberate explicit accept
;;; (a second Tab / the accept command) replaces the head.  The suite is PTY-free:
;;;   * the ranking + non-vacuity rows call nearest-commands / fuzzy-filter over a
;;;     seeded universe directly -- deterministic on every platform;
;;;   * the surface rows drive the real Tab command (cmd-complete) on an editor
;;;     state, reading the buffer and the pending-hint accessor back, with the
;;;     working directory pinned to a fresh EMPTY temp dir so filename completion
;;;     contributes nothing and the correction fallback is reached deterministically.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod editor editor)
              nearest-commands command-universe current-correction clear-correction!
              cmd-accept-correction cmd-complete command-correction?
              make-editor-state editor-state-gb)
        (only (hafod editor gap-buffer)
              make-gap-buffer gap-buffer-insert-string! gap-buffer->string)
        (only (hafod shell classifier) path-cache alias-set!)
        (only (hafod fuzzy) fuzzy-filter)
        (only (hafod fileinfo) create-directory delete-directory)
        (only (hafod process-state) pid)
        (chezscheme))

(test-begin "command-correction")

;; ======================================================================
;; Harness
;; ======================================================================

;; Seed the in-memory command universe with a known, deterministic set so the
;; ranking is independent of whatever the real PATH holds.  path-cache starts
;; empty (rebuild-path-cache! is a startup call, never triggered on load), so the
;; universe is exactly what is seeded here plus the fixed builtins.
(for-each (lambda (name) (hashtable-set! (path-cache) name #t))
          '("git" "grep" "python" "python3"
            ;; a family that all sit one edit from "xxxx", for the cap-at-three row
            "xxxa" "xxax" "xaxx" "axxx" "xxxy"))
(alias-set! "gs" "git status")

;; A fresh editor state whose buffer holds TEXT with the cursor at the end, ready
;; for a Tab on the trailing head token.  cmd-complete reads the buffer and cursor
;; itself; the kill-ring and output port are unused on this path, so #f suffices.
(define (state-with text)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert-string! gb text)
    (make-editor-state gb #f "" #f 0 #f #f 'insert #f)))

(define (buffer-of es) (gap-buffer->string (editor-state-gb es)))

;; ======================================================================
;; The command universe unions the three in-memory sources.
;; ======================================================================

(test-assert "the command universe includes a PATH-cache command"
  (member "git" (command-universe)))
(test-assert "the command universe includes a shell builtin"
  (member "cd" (command-universe)))
(test-assert "the command universe includes an alias name"
  (member "gs" (command-universe)))

;; ======================================================================
;; Ranking: nearest-first, capped at three, within the threshold.
;; ======================================================================

;; "gti" is one adjacent transposition from "git", and nothing else is nearer, so
;; git leads the results.
(test-equal "the transposition typo's target ranks first (gti -> git)"
  "git" (car (nearest-commands "gti")))

;; A double-transposition slip still lands on its target within the threshold.
(test-equal "a transposition typo ranks its target first (pyhton -> python)"
  "python" (car (nearest-commands "pyhton")))

;; A head far from every known command -- longer than any by more than the
;; threshold -- yields nothing (the length pre-filter alone clears the universe).
(test-equal "a far head yields no suggestions"
  '() (nearest-commands "qwertyuiopas"))

;; Five names sit one edit from "xxxx"; the result is capped at three.
(test-equal "suggestions are capped at three"
  3 (length (nearest-commands "xxxx")))

;; "grep" is the same length as "xxxx" (so the length pre-filter keeps it) but four
;; edits away -- beyond the threshold -- so the distance check must drop it.  This
;; proves the tight threshold, not merely the free length filter.
(test-assert "a same-length but beyond-threshold candidate is excluded"
  (not (member "grep" (nearest-commands "xxxx"))))

;; ======================================================================
;; Non-vacuity: the shipped subsequence fuzzy engine cannot rank this typo.
;; A tree that reused fuzzy for the correction would fail this row -- fuzzy-filter
;; returns nothing for the commonest transposition, while nearest-commands finds
;; the target.  This is exactly why the transposition-aware metric is used.
;; ======================================================================

(test-assert "the subsequence fuzzy engine cannot match gti against git"
  (null? (fuzzy-filter "gti" (list "git"))))
(test-assert "the transposition-aware search does find git for gti"
  (member "git" (nearest-commands "gti")))

;; ======================================================================
;; The suggest-only surface, off-by-default proof, and the resolvable no-op.
;; These drive the real Tab command in a pinned EMPTY working directory so the
;; filename-completion arm contributes nothing and the correction fallback is the
;; deterministic outcome.
;; ======================================================================

(define (temp-root)
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/hafod-command-correction-"
                 (number->string (pid)) "-"
                 (number->string (random 1000000))))

(let ([root (temp-root)]
      [saved (current-directory)])
  (create-directory root)
  (dynamic-wind
    (lambda () (current-directory root))
    (lambda ()

      ;; (1) Toggle ON, an unresolvable head: a Tab surfaces the nearest command as
      ;; a suggestion WITHOUT touching the buffer.  The buffer stays exactly the
      ;; typed text -- which is what Enter would run -- so the correction is never
      ;; auto-applied and never auto-run.
      (clear-correction!)
      (let ([es (state-with "gti")])
        (parameterize ([command-correction? #t])
          (cmd-complete es))
        (test-equal "a Tab on an unresolvable head leaves the buffer untouched"
          "gti" (buffer-of es))
        (test-equal "the nearest command is surfaced as a suggestion"
          "git" (current-correction))
        ;; The buffer -- what a following Enter submits verbatim -- is unchanged.
        (test-equal "Enter would run the typed command exactly as typed"
          "gti" (buffer-of es)))

      ;; (2) The explicit accept affordance replaces the head token -- and only when
      ;; it is invoked.  The first Tab merely surfaces the hint (buffer intact); the
      ;; explicit accept then rewrites the head to the suggestion and clears it.
      (clear-correction!)
      (let ([es (state-with "gti")])
        (parameterize ([command-correction? #t])
          (cmd-complete es))
        (test-equal "before the accept, the buffer is still the typed text"
          "gti" (buffer-of es))
        (cmd-accept-correction es)
        (test-equal "the explicit accept replaces the head token with the suggestion"
          "git" (buffer-of es))
        (test-assert "accepting clears the pending suggestion"
          (not (current-correction))))

      ;; (3) A deliberate SECOND Tab is that explicit accept gesture in the live
      ;; keymap: the first Tab surfaces, the second accepts.
      (clear-correction!)
      (let ([es (state-with "gti")])
        (parameterize ([command-correction? #t])
          (cmd-complete es)     ; first Tab: surface
          (cmd-complete es))    ; second Tab: accept
        (test-equal "a deliberate second Tab accepts the suggestion"
          "git" (buffer-of es)))

      ;; (4) Off by default: with the toggle unset, an unresolvable head on Tab
      ;; surfaces nothing and leaves the buffer untouched -- the shipped behaviour.
      (clear-correction!)
      (let ([es (state-with "gti")])
        (cmd-complete es)
        (test-assert "off by default, an unresolvable head yields no suggestion"
          (not (current-correction)))
        (test-equal "off by default, the buffer is untouched"
          "gti" (buffer-of es)))

      ;; (5) A resolvable head never triggers a correction, even with the toggle on.
      ;; "car" resolves as a bound top-level identifier, so the head-resolves
      ;; predicate suppresses any suggestion -- exercising that gate directly.
      (clear-correction!)
      (let ([es (state-with "car")])
        (parameterize ([command-correction? #t])
          (cmd-complete es))
        (test-assert "a resolvable head yields no suggestion with the toggle on"
          (not (current-correction)))))

    (lambda ()
      (current-directory saved)
      (guard (e [#t (void)]) (delete-directory root)))))

(test-end)
