;;; test/test-vi-visual.ss -- Coverage for vi visual-mode state and the visual
;;; selection range mapping. Everything is driven through vi-process-key with
;;; bounded string ports, so the suite needs no terminal and can never block.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor gap-buffer)
        (hafod editor kill-ring)
        (hafod editor input-decode)
        (hafod editor vi)
        ;; Importing the editor library installs vi.ss's procedure hooks
        ;; (vi-snapshot!-proc and friends) at load, so vi-process-key runs
        ;; without a null-hook error.
        (hafod editor editor))

;; Build a single-line gap-buffer holding TEXT with the cursor at INDEX.
(define (buffer-with-cursor text index)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert-string! gb text)
    ;; insert leaves the cursor at the end; step it back to the chosen index
    (gap-buffer-move-cursor! gb (- index (string-length text)))
    gb))

;; Construct a normal-mode editor-state around a buffer for a scenario.
;; The trailing #f is the initial mark (no region active).
(define (state-for gb kr)
  (make-editor-state gb kr "> " (open-output-string) 0 #f #f 'normal #f))

;; Feed one character key event through the vi state machine. The input port is
;; bounded and empty: none of the keys exercised here read a follow-up key.
(define (press! es gb kr ch)
  (vi-process-key es (make-key-event 'char ch 0) (open-input-string "") (open-output-string) gb kr))

(test-begin "vi-visual")

;; --- Characterwise: v then a rightward motion ---------------------------
;; "hello" with the cursor on 'e' (index 1). Enter characterwise visual, then
;; move right one with l. The anchor stays at the entry index, the end tracks
;; the live cursor, and the resolved range is the inclusive min..max+1 span
;; (indices 1 and 2, so end is 3).
(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\v)
  (press! es gb kr #\l)
  (test-equal "characterwise visual stays 'char through a rightward motion"
    'char (vi-visual-mode))
  (test-equal "characterwise visual keeps the anchor at the entry index"
    1 (vi-visual-anchor))
  (test-equal "vi-visual-end tracks the live cursor position"
    2 (vi-visual-end gb))
  (test-equal "vi-visual-range is the inclusive characterwise span (min..max+1)"
    '(1 . 3) (vi-visual-range gb)))

;; --- Mode indicator: characterwise --------------------------------------
(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\v)
  (test-equal "the mode indicator reads VISUAL in characterwise visual"
    "-- VISUAL --" (vi-mode-indicator)))

;; --- Linewise: V sets 'line, indicator + whole-line range ---------------
;; V selects linewise regardless of the cursor: the indicator reads VISUAL LINE
;; and the resolved range spans the whole single-line buffer, 0..len.
(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\V)
  (test-equal "linewise visual sets 'line mode"
    'line (vi-visual-mode))
  (test-equal "the mode indicator reads VISUAL LINE in linewise visual"
    "-- VISUAL LINE --" (vi-mode-indicator))
  (test-equal "linewise visual selects the whole single-line buffer 0..len"
    '(0 . 5) (vi-visual-range gb)))

;; --- No active visual selection -----------------------------------------
;; After a session reset there is no selection: both the mode and the range
;; report #f.
(let ()
  (define gb (buffer-with-cursor "hello" 3))
  (vi-reset-session!)
  (test-assert "with no visual mode, vi-visual-mode is #f"
    (eq? #f (vi-visual-mode)))
  (test-assert "with no visual mode, vi-visual-range is #f"
    (eq? #f (vi-visual-range gb))))

(test-end)
