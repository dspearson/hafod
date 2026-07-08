;;; test/test-sexp-transpose.ss -- Coverage for the s-expression transpose
;;; commands: cmd-transpose-sexp swaps the sexp at point with its next sibling
;;; (emacs transpose-sexps), cmd-transpose-sexp-backward swaps it with the
;;; previous sibling.  Every assertion drives the command directly on an
;;; editor-state and reads the buffer via gap-buffer->string and the point via
;;; gap-buffer-cursor-pos, so the suite is PTY-free and can never block.  The
;;; genuine last-child / first-child no-ops and the undo-integrity case are the
;;; teeth: they prove the anchor is bound-by-end (a non-first-child cursor swaps
;;; the correct form) and that a no-op pushes no undo snapshot.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor editor)
        (hafod editor gap-buffer)
        (hafod editor kill-ring)
        (hafod editor input-decode)
        (hafod editor vi)
        (hafod editor history)
        (hafod editor sexp-tracker))

;; Build a gap-buffer holding TEXT with the cursor at INDEX.
(define (buffer-with-cursor text index)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert-string! gb text)
    ;; insert leaves the cursor at the end; step it back to the chosen index
    (gap-buffer-move-cursor! gb (- index (string-length text)))
    gb))

;; An editor-state around a buffer in MODE; the trailing #f is the initial mark
;; (no region active).
(define (state-for gb kr mode)
  (make-editor-state gb kr "> " (open-output-string) 0 #f #f mode #f))

(test-begin "sexp-transpose")

;; --- Forward: cmd-transpose-sexp ----------------------------------------

;; 1. Two atoms in a list swap forward; point lands after the moved form.  The
;; cursor sits on the first child `a`, and the anchor is bound-by-end, so A is
;; `a` (not something before it) and B is its next sibling `b`.
(let ()
  (define gb (buffer-with-cursor "(a b c)" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (cmd-transpose-sexp es)
  (test-equal "two atoms in a list swap forward"
    "(b a c)" (gap-buffer->string gb))
  (test-equal "point lands after the moved form"
    4 (gap-buffer-cursor-pos gb)))

;; 2. Top-level atoms swap; the whitespace gap is carried across and point ends
;; after the moved atom at the buffer end.
(let ()
  (define gb (buffer-with-cursor "foo bar" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (cmd-transpose-sexp es)
  (test-equal "top-level atoms swap forward"
    "bar foo" (gap-buffer->string gb))
  (test-equal "point lands after the moved top-level atom"
    7 (gap-buffer-cursor-pos gb)))

;; 3. GENUINE last-child no-op: cursor on the LAST child `b`.  A is `b`;
;; forward-sexp-end past it hits the closer, so end-b is #f, the guard fails,
;; and nothing happens.  This case would WRONGLY swap to `(b a)` under the old
;; `(or (backward-sexp-start text pos) pos)` anchor (which anchors A on `a`), so
;; it is the regression guard for the bound-by-end fix.
(let ()
  (define gb (buffer-with-cursor "(a b)" 3))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (cmd-transpose-sexp es)
  (test-equal "a last child is a safe no-op, buffer byte-for-byte unchanged"
    "(a b)" (gap-buffer->string gb))
  (test-equal "the no-op leaves the cursor where it was"
    3 (gap-buffer-cursor-pos gb)))

;; 4. Single-child / buffer-edge no-op: `foo` is the only child, so end-b is #f
;; and the command no-ops.  A no-op never moves the cursor, so point stays at the
;; original index 1 and the buffer is byte-for-byte unchanged.
(let ()
  (define gb (buffer-with-cursor "(foo)" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (cmd-transpose-sexp es)
  (test-equal "a single child is a safe no-op, buffer byte-for-byte unchanged"
    "(foo)" (gap-buffer->string gb))
  (test-equal "the single-child no-op leaves the cursor where it was"
    1 (gap-buffer-cursor-pos gb)))

;; 5. Multi-line gap preserved: the embedded newline between the two atoms is
;; carried across verbatim as the inter-form gap.
(let ()
  (define gb (buffer-with-cursor "a\nb" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (cmd-transpose-sexp es)
  (test-equal "the newline gap is preserved across a multi-line swap"
    "b\na" (gap-buffer->string gb))
  (test-equal "point lands after the moved atom across the newline"
    3 (gap-buffer-cursor-pos gb)))

;; --- Backward: cmd-transpose-sexp-backward ------------------------------

;; 6. Cursor on the non-first child `b`: A is `b` (bound-by-end), P is the
;; previous sibling `a`; `b` drags up past `a` and point lands after the moved
;; `b`.  This case is a NO-OP under the old anchor (which anchors A on `a`, whose
;; previous sibling is none), so it is the second regression guard for the fix.
(let ()
  (define gb (buffer-with-cursor "(a b c)" 3))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (cmd-transpose-sexp-backward es)
  (test-equal "a non-first child swaps backward past its previous sibling"
    "(b a c)" (gap-buffer->string gb))
  (test-equal "point lands after the moved form in its earlier position"
    2 (gap-buffer-cursor-pos gb)))

;; 7. GENUINE first-child no-op: cursor in the FIRST top-level atom `foo`.  A is
;; `foo`; backward-sexp-start before A returns #f, so start-p is #f and there is
;; no previous sibling -- a safe no-op, cursor untouched.
(let ()
  (define gb (buffer-with-cursor "foo bar" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (cmd-transpose-sexp-backward es)
  (test-equal "a first child is a safe backward no-op, buffer unchanged"
    "foo bar" (gap-buffer->string gb))
  (test-equal "the first-child no-op leaves the cursor where it was"
    1 (gap-buffer-cursor-pos gb)))

;; --- Undo integrity: guard-then-snapshot / no snapshot on a no-op -------

;; 8. The discriminating case.  A real swap pushes exactly ONE snapshot; a
;; following no-op pushes none.  A single cmd-undo must therefore reach the
;; ORIGINAL buffer, not the post-swap intermediate -- had the no-op pushed a
;; phantom snapshot of `(b a c)`, one undo would only reach `(b a c)`.
(let ()
  (define gb (buffer-with-cursor "(a b c)" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (cmd-transpose-sexp es)                 ; real swap -> (b a c), point 4
  (test-equal "the real swap moved the form"
    "(b a c)" (gap-buffer->string gb))
  (gap-buffer-move-cursor! gb 1)          ; point 4 -> 5, onto the last child c
  (cmd-transpose-sexp es)                 ; no-op: c has no next sibling
  (test-equal "the trailing no-op left the buffer unchanged"
    "(b a c)" (gap-buffer->string gb))
  (cmd-undo es)                           ; a single undo pops exactly the real swap
  (test-equal "one undo returns to the original -- the no-op pushed no snapshot"
    "(a b c)" (gap-buffer->string gb)))

(test-end)
