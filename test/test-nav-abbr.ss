;;; test/test-nav-abbr.ss -- Coverage for command-head abbreviations: the pure
;;; (text cursor) -> (values text' cursor') expansion core and its Space/Enter
;;; hooks in the line editor. The pure core is driven directly over flat strings
;;; so every gate (shell context, command position, cursor-at-token-end, table
;;; membership) is exercised with no gap buffer or I/O. The Space and Enter paths
;;; run through editor-drive-keys!, the PTY-free key harness the dot-repeat suite
;;; also uses, so a test exercises the real insert-keymap dispatch. The
;;; single-undo case proves the expansion and its trailing space are one undoable
;;; step; the toggle case proves the off switch falls back to a literal space.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor editor)
        (hafod editor gap-buffer)
        (hafod editor kill-ring)
        (hafod editor input-decode))

;; Build an INSERT-mode editor-state around TEXT with the cursor at INDEX, drive
;; the keystroke script INPUTS through the real record/dispatch loop, and return
;; the resulting buffer string. Mirrors the dot-repeat harness's drive helper but
;; starts in insert mode, where the Space abbreviation binding lives.
(define (drive text index inputs)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert-string! gb text)
    (gap-buffer-move-cursor! gb (- index (string-length text)))
    (let ([es (make-editor-state gb (make-kill-ring) "> "
                                 (open-output-string) 0 #f #f 'insert #f)])
      (editor-drive-keys! es inputs)
      (gap-buffer->string gb))))

;; Seed one abbreviation for the whole suite (the module-global table persists
;; across drives within this process).
(abbr-set! "gco" "git checkout")

(test-begin "nav-abbr")

;; ---- Pure core: exhaustive gate coverage over flat strings ----

;; A lone command-head token that names an abbreviation, cursor at its end,
;; expands to (values expansion end-of-expansion).
(let-values ([(t c) (expand-abbr-head "gco" 3)])
  (test-equal "the head abbreviation expands to its value" "git checkout" t)
  (test-equal "the cursor lands at the end of the expansion" 12 c))

;; An unknown head does not expand.
(let-values ([(t c) (expand-abbr-head "xyz" 3)])
  (test-assert "an unknown head does not expand" (and (not t) (not c))))

;; The cursor is past a second word rather than at the head-token end, so no
;; expansion -- abbreviations fire in command-head, cursor-at-end position only.
(let-values ([(t c) (expand-abbr-head "ls -l" 5)])
  (test-assert "a cursor past the head token does not expand" (and (not t) (not c))))

;; A Scheme-context buffer (first non-whitespace char is a scheme-prefix char)
;; is never rewritten, even when its head names an abbreviation.
(let-values ([(t c) (expand-abbr-head "(gco" 4)])
  (test-assert "a Scheme-context buffer does not expand" (and (not t) (not c))))

;; The Enter core expands the first-word head regardless of cursor position.
(let-values ([(t c) (expand-first-abbr "gco")])
  (test-equal "the submit core expands the first-word head" "git checkout" t)
  (test-equal "the submit core reports the cursor at the expansion end" 12 c))

;; ---- Space in the insert keymap: expand-then-insert ----

(test-equal "Space after a head abbreviation expands the buffer and inserts the space"
  "git checkout "
  (drive "gco" 3 (list " ")))

;; With arguments already present and the cursor at the head-token end, Space
;; expands the head but must not add a second separator: the existing space
;; already separates the head from its args, so the result carries a single
;; space, not a doubled one.
(test-equal "Space expanding a head with args present keeps a single space"
  "git checkout arg"
  (drive "gco arg" 3 (list " ")))

(test-equal "Space mid-line inserts a literal space (no expansion)"
  "ls -l "
  (drive "ls -l" 5 (list " ")))

(test-equal "Space after an unknown head inserts a literal space (no expansion)"
  "xyz "
  (drive "xyz" 3 (list " ")))

;; A Scheme-context buffer is untouched by the Space binding too: the head is not
;; rewritten, only the literal space is inserted.
(test-equal "Space in a Scheme-context buffer inserts a literal space (no expansion)"
  "(gco "
  (drive "(gco" 4 (list " ")))

;; ---- Single-step undo: expansion + trailing space is one undoable edit ----
;; Run on the expansion's OWN editor state (the undo stack is module-global, so
;; one undo pops exactly the snapshot this expansion opened). One undo restores
;; the original abbreviation, NOT the expansion: had the trailing space gone
;; through cmd-self-insert, its whitespace snapshot would have split the edit and
;; a single undo would have stopped at "git checkout".
(let ([gb (make-gap-buffer)])
  (gap-buffer-insert-string! gb "gco")
  (let ([es (make-editor-state gb (make-kill-ring) "> "
                               (open-output-string) 0 #f #f 'insert #f)])
    (editor-drive-keys! es (list " "))
    (test-equal "the Space expansion produced the full expanded buffer"
      "git checkout "
      (gap-buffer->string gb))
    (cmd-undo es)
    (test-equal "one undo restores the original abbreviation, not the expansion"
      "gco"
      (gap-buffer->string gb))))

;; ---- Enter (submit): fish expand-on-execute ----
;; A pending head abbreviation is expanded into the submitted line before it is
;; taken, so the editor result carries the expansion. Driving a newline exercises
;; the real insert-keymap submit binding, not just the pure core.
(let ([gb (make-gap-buffer)])
  (gap-buffer-insert-string! gb "gco")
  (let ([es (make-editor-state gb (make-kill-ring) "> "
                               (open-output-string) 0 #f #f 'insert #f)])
    (editor-drive-keys! es (list "\n"))
    (test-equal "Enter expands the pending head abbreviation on submit"
      "git checkout"
      (editor-state-result es))))

;; ---- Toggle off: a literal space, no expansion ----
(test-equal "with expansion toggled off Space inserts a literal space"
  "gco "
  (parameterize ([abbr-expand? #f])
    (drive "gco" 3 (list " "))))

;; ---- Toggle off on Enter: the buffer is submitted verbatim ----
;; The submit path (fish expand-on-execute) must honour the same off switch as
;; Space: with abbr-expand? off, pressing Enter on a pending head abbreviation
;; submits it unchanged rather than silently rewriting the command head.
(let ([gb (make-gap-buffer)])
  (gap-buffer-insert-string! gb "gco")
  (let ([es (make-editor-state gb (make-kill-ring) "> "
                               (open-output-string) 0 #f #f 'insert #f)])
    (parameterize ([abbr-expand? #f])
      (editor-drive-keys! es (list "\n")))
    (test-equal "with expansion toggled off Enter submits the buffer verbatim"
      "gco"
      (editor-state-result es))))

(test-end)
