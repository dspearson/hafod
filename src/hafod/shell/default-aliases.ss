;;; (hafod shell default-aliases) -- The curated default interactive alias set.
;;;
;;; A fresh interactive session installs a small, portable set of conveniences a
;;; user coming from zsh + prezto expects: a colourised `ls` (and its long-listing
;;; variants), colourised `grep`, human-readable sizes, and prompt-before-clobber
;;; safety on `rm`/`cp`/`mv`.  These are ordinary command aliases -- the same table
;;; `alias`/`unalias` and the classifier already use -- so they expand on execution
;;; exactly as a hand-typed alias would, and the classifier's loop guard keeps a
;;; self-referential `ls -> ls --color=auto` finite.
;;;
;;; Installation is interactive-only (the REPL entry calls it, never program
;;; top-level) and it never clobbers a user alias: init.ss is loaded BEFORE the
;;; REPL enables these, so an init.ss `(alias "ls" ...)` is already in the table
;;; and is left untouched.  `default-aliases?` (default #t) gates the whole set,
;;; and the master `interactive-enhancements?` switch gates it at the REPL.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod shell default-aliases)
  (export default-aliases? default-alias-list install-default-aliases!)
  (import (chezscheme)
          (only (hafod internal platform) os-family)
          (only (hafod shell classifier) alias-set! alias-ref))

  ;; Gate for the default alias set.  #t installs it on an interactive session;
  ;; set #f in init.ss for the bare command surface.  Coerces any non-#f to #t.
  (define default-aliases?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; The colourised-`ls` command, per platform: GNU coreutils takes --color=auto,
  ;; the BSD / macOS ls takes -G.  The long-listing aliases reference bare `ls`, so
  ;; this colour flag composes onto them through the classifier's chained head
  ;; expansion (a single --color, the loop guard stopping the second `ls`).
  (define (ls-command)
    (if (eq? os-family 'linux) "ls --color=auto" "ls -G"))

  ;; The curated set as (name . value) pairs.  Portable across Linux and macOS:
  ;; grep --color, df/du -h and the -i safety flags are accepted by both toolchains
  ;; (Linux-only `free` is deliberately omitted).
  (define (default-alias-list)
    (list
      (cons "ls" (ls-command))
      (cons "ll" "ls -lh")
      (cons "la" "ls -lAh")
      (cons "l"  "ls -lah")
      (cons "grep"  "grep --color=auto")
      (cons "egrep" "egrep --color=auto")
      (cons "fgrep" "fgrep --color=auto")
      (cons "df" "df -h")
      (cons "du" "du -h")
      (cons "rm" "rm -i")
      (cons "cp" "cp -i")
      (cons "mv" "mv -i")))

  ;; Install each default alias the user has NOT already defined, so an init.ss
  ;; alias (set before the REPL runs this) always wins.  Idempotent: a second call
  ;; re-sets nothing, since the first pass has already populated the table.
  (define (install-default-aliases!)
    (for-each
      (lambda (pair)
        (unless (alias-ref (car pair))
          (alias-set! (car pair) (cdr pair))))
      (default-alias-list)))
)
