;;; test/test-sexp-expand.ss -- Coverage for cmd-expand-region /
;;; cmd-contract-region: the retrace expansion stack.  Progressive expand grows
;;; the selection through each enclosing form and no-ops at the outermost;
;;; contract retraces exactly inward and never past the seed; a cursor move
;;; re-seeds the stack from the new point; and a span is computed correctly
;;; across an embedded newline.  The commands are called directly on an
;;; editor-state and the selection asserted through editor-selection-range, so
;;; the suite needs no terminal and can never block.
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
    (gap-buffer-move-cursor! gb (- index (string-length text)))
    gb))

;; A normal-mode editor-state around a buffer; the trailing #f is the initial
;; mark (no region active).
(define (state-for gb kr)
  (make-editor-state gb kr "> " (open-output-string) 0 #f #f 'normal #f))

;; Reposition the cursor to an absolute INDEX (the move primitive takes a delta).
(define (move-cursor-to! gb index)
  (gap-buffer-move-cursor! gb (- index (gap-buffer-cursor-pos gb))))

(test-begin "sexp-expand")

;; Progressive expand then retrace on a nested form.  "(a (b c))" with the cursor
;; on b (index 4): expand walks b -> (b c) -> (a (b c)) -> no-op, and contract
;; steps back through each, never past the seed atom.
(let ()
  (define gb (buffer-with-cursor "(a (b c))" 4))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (reset-sexp-stack!)
  (cmd-expand-region es)
  (test-equal "the first expand selects the atom at point"
    '(4 . 5) (editor-selection-range es))
  (cmd-expand-region es)
  (test-equal "the second expand grows to the inner list"
    '(3 . 8) (editor-selection-range es))
  (cmd-expand-region es)
  (test-equal "the third expand grows to the outer list"
    '(0 . 9) (editor-selection-range es))
  (cmd-expand-region es)
  (test-equal "expand no-ops at the outermost form"
    '(0 . 9) (editor-selection-range es))
  (cmd-contract-region es)
  (test-equal "contract retraces to the inner list"
    '(3 . 8) (editor-selection-range es))
  (cmd-contract-region es)
  (test-equal "contract retraces to the atom"
    '(4 . 5) (editor-selection-range es))
  (cmd-contract-region es)
  (test-equal "contract never steps past the seed"
    '(4 . 5) (editor-selection-range es)))

;; Stale re-seed on a cursor move.  After expanding to the atom b, moving the
;; cursor onto a (index 1) makes the stack stale, so the next expand re-seeds the
;; atom a rather than growing the old span.
(let ()
  (define gb (buffer-with-cursor "(a (b c))" 4))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (reset-sexp-stack!)
  (cmd-expand-region es)
  (test-equal "the first expand selects the atom b"
    '(4 . 5) (editor-selection-range es))
  (move-cursor-to! gb 1)
  (cmd-expand-region es)
  (test-equal "a cursor move re-seeds the atom at the new point"
    '(1 . 2) (editor-selection-range es)))

;; Multi-line span: a form split across an embedded newline.  "(a\n b)" with the
;; cursor on b (index 4): select the atom, then expand across the newline to the
;; whole form.
(let ()
  (define gb (buffer-with-cursor "(a\n b)" 4))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (reset-sexp-stack!)
  (cmd-select-sexp es)
  (test-equal "the atom is selected on the second line"
    '(4 . 5) (editor-selection-range es))
  (cmd-expand-region es)
  (test-equal "expand grows across the newline to the whole form"
    '(0 . 6) (editor-selection-range es)))

(test-end)
