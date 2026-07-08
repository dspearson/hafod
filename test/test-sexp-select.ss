;;; test/test-sexp-select.ss -- Coverage for cmd-select-sexp: selecting the
;;; innermost s-expression at point.  The atom under point, the whole list on a
;;; delimiter or on inner whitespace, a top-level atom with no enclosing list,
;;; and the empty-buffer no-op.  The command is called directly on an
;;; editor-state and the resolved selection asserted through
;;; editor-selection-range, so the suite needs no terminal and can never block.
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

;; A normal-mode editor-state around a buffer; the trailing #f is the initial
;; mark (no region active).
(define (state-for gb kr)
  (make-editor-state gb kr "> " (open-output-string) 0 #f #f 'normal #f))

(test-begin "sexp-select")

;; Atom under point: the cursor on foo (index 1) selects just the atom.
(let ()
  (define gb (buffer-with-cursor "(foo bar)" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (reset-sexp-stack!)
  (cmd-select-sexp es)
  (test-equal "the atom under point is selected"
    '(1 . 4) (editor-selection-range es)))

;; On an opener: the cursor on the ( (index 0) selects the whole list.
(let ()
  (define gb (buffer-with-cursor "(foo bar)" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (reset-sexp-stack!)
  (cmd-select-sexp es)
  (test-equal "an opener selects the whole list"
    '(0 . 9) (editor-selection-range es)))

;; On a closer: the cursor on the ) (index 8) selects the list it closes.
(let ()
  (define gb (buffer-with-cursor "(foo bar)" 8))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (reset-sexp-stack!)
  (cmd-select-sexp es)
  (test-equal "a closer selects the list it closes"
    '(0 . 9) (editor-selection-range es)))

;; On inner whitespace: the cursor on the space (index 4) selects the enclosing
;; form, not the neighbouring atom.
(let ()
  (define gb (buffer-with-cursor "(foo bar)" 4))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (reset-sexp-stack!)
  (cmd-select-sexp es)
  (test-equal "inner whitespace selects the enclosing form"
    '(0 . 9) (editor-selection-range es)))

;; A top-level atom with no enclosing list: the cursor on foo (index 1) selects
;; the atom, and a second select re-seeds exactly the same span.
(let ()
  (define gb (buffer-with-cursor "foo bar" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (reset-sexp-stack!)
  (cmd-select-sexp es)
  (test-equal "a top-level atom is selected"
    '(0 . 3) (editor-selection-range es))
  (cmd-select-sexp es)
  (test-equal "a second select re-seeds the same span"
    '(0 . 3) (editor-selection-range es)))

;; Empty buffer: nothing to select, so the command is a no-op -- no selection
;; range surfaces and it must not crash.
(let ()
  (define gb (buffer-with-cursor "" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (reset-sexp-stack!)
  (cmd-select-sexp es)
  (test-assert "an empty buffer leaves no selection"
    (eq? #f (editor-selection-range es))))

(test-end)
