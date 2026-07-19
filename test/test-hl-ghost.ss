;;; test/test-hl-ghost.ss -- The inline ghost suggestion draws from more than
;;; history prefixes.
;;;
;;; The dim autosuggestion was history-PREFIX only: it offered the tail of the
;;; most recent past line that BEGAN with the typed text, and nothing when none
;;; did.  It now falls through a cascade of sources, most relevant first: the
;;; history prefix stays the first source and is preserved byte-for-byte, then --
;;; only when no line heads with the text -- a history SUBSTRING (a real past
;;; command matched mid-line), then a single unambiguous PATH-cache COMPLETION of
;;; a bare command head.  A real past command always outranks a synthesised
;;; completion, so the substring source is tried before the completion one.
;;;
;;; This suite seeds a known corpus and drives history-ghost-suffix directly, no
;;; terminal.  A naive prefix-only implementation returns "" for the substring
;;; and completion needles below, so it reddens the pre-change tree on those two
;;; arms while the prefix assertion still passes; a precedence assertion holds in
;;; both trees, proving the prefix source is preserved.  Degrades to a printed
;;; note where libsqlite3 cannot be loaded, mirroring the other history suites.
;;; Copyright (c) 2026, hafod contributors.

;; Resolve the compiled libraries by ABSOLUTE path from the launch directory, so
;; library resolution stays independent of any later cd -- the idiom the other
;; navigation suites use.
(library-directories
  (let ([root (current-directory)])
    (list (cons (string-append root "/src") (string-append root "/src"))
          (cons root root))))
(import (test runner)
        (only (hafod editor editor) history-ghost-suffix)
        (hafod editor history)
        (only (hafod editor sqlite3) sqlite3-loaded?)
        (only (hafod shell classifier) path-cache)
        (chezscheme))

(define (note . parts)
  (display "  note: ")
  (for-each display parts)
  (newline))

(test-begin "hl-ghost")

;; open-history goes through the sqlite3 wrappers, whose shared object is
;; resolved on first use -- so this first open is what loads it, and
;; sqlite3-loaded? can be trusted from there on.  An in-memory history needs no
;; file and leaves nothing behind; history-ghost-suffix reads the in-memory
;; entry vector, so the corpus below drives it with no disk at all.
(define h (open-history ":memory:"))

(if (not (sqlite3-loaded?))
    (begin
      (note "libsqlite3 is unavailable; skipping the ghost-source assertions")
      (history-close! h))
    (begin
      ;; A known corpus, oldest first (history-add! appends, most recent last):
      ;;   0  git status     -- a genuine PREFIX entry (headed by "git s"/"git")
      ;;   1  ls | grep foo   -- a MID-LINE entry for "grep" (does NOT begin "grep")
      ;;   2  run git push    -- a MID-LINE "git" entry (for the precedence control)
      (history-add! h "git status")
      (history-add! h "ls | grep foo")
      (history-add! h "run git push")

      ;; Arm 1 (history prefix) -- unchanged and byte-identical to before.  "git s"
      ;; heads "git status", so the ghost offers the tail that continues exactly
      ;; where the typing stopped.  The after-cursor text is "" throughout (the
      ;; cursor sits at the end of the typed region), which passes the outer gate.
      (test-equal "a history prefix still yields its byte-identical suffix"
        "tatus" (history-ghost-suffix "git s" "" h))

      ;; Arm 2 (history substring) -- the headline change.  "grep" heads no entry,
      ;; so the prefix source offers nothing and the ghost falls through to the
      ;; substring source: the tail AFTER the matched needle in "ls | grep foo" is
      ;; " foo".  A prefix-only ghost returns "" here, so this assertion reddens
      ;; the pre-change tree.
      (test-equal "a mid-line needle yields the after-needle tail via substring"
        " foo" (history-ghost-suffix "grep" "" h))

      ;; Precedence control -- the prefix source is preserved.  "git" both heads
      ;; "git status" (a prefix match) and sits mid-line in "run git push" (a
      ;; substring match).  The prefix source must win, so the suffix is " status"
      ;; from the head match, never " push" from the mid-line one.  This holds in
      ;; both the pre- and post-change trees: it proves the richer cascade never
      ;; displaces a genuine prefix ghost.
      (test-equal "the prefix source wins over an also-present substring source"
        " status" (history-ghost-suffix "git" "" h))

      ;; Arm 3 (PATH-cache completion) -- the last resort.  "gitf" heads no entry
      ;; and sits in none, so both history sources return "" and the ghost reaches
      ;; the completion source.  The PATH cache is empty in a fresh test process,
      ;; so it is seeded deterministically here (no PATH scan).  With exactly one
      ;; key carrying "gitf" as a LITERAL prefix, the ghost completes it to the
      ;; after-prefix tail "oo".  A prefix-only ghost returns "" here too.
      (hashtable-set! (path-cache) "gitfoo" #t)
      (test-equal "a single unambiguous PATH-cache completion yields its tail"
        "oo" (history-ghost-suffix "gitf" "" h))

      ;; ... and a SECOND key sharing that prefix makes the completion ambiguous,
      ;; so the arm withholds a suffix rather than guess between them -- exactly
      ;; one candidate is the whole point of "unambiguous".  (A key that does not
      ;; share the "gitf" prefix would be neither a fuzzy candidate nor a literal
      ;; continuation, so a prefix-sharing key is what actually creates the
      ;; contest this asserts is refused.)
      (hashtable-set! (path-cache) "gitfun" #t)
      (test-equal "an ambiguous PATH-cache completion contributes nothing"
        "" (history-ghost-suffix "gitf" "" h))

      (history-close! h)))

;; The editor's update-suggestion! seam reads history-ghost-suffix on every
;; render and shows its result as the dim ghost; only the SOURCE of that suffix
;; changed here, not the render path.  The substring and completion sources are
;; computed only on a prefix miss, so the common per-keystroke path costs exactly
;; what it did before.

(test-end)
