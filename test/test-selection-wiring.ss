;;; test/test-selection-wiring.ss -- Coverage for the selection-range and mode-
;;; indicator helpers the redisplay feeds to the selection-aware renderer, and
;;; for the mark reset that stops a region bleeding into the next line. Pure unit
;;; assertions over editor-state and the vi visual accessors, driven with bounded
;;; string ports -- no terminal, no PTY, so the suite can never block.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor editor)
        (hafod editor gap-buffer)
        (hafod editor kill-ring)
        (hafod editor input-decode)
        ;; The vi accessors under test, plus vi-process-key to drive visual mode.
        (hafod editor vi)
        (hafod editor history)
        (hafod editor sexp-tracker))

;; Build a single-line gap-buffer holding TEXT with the cursor at INDEX.
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

;; Feed one character key event through the vi state machine. The input port is
;; bounded and empty: none of the keys exercised here read a follow-up key.
(define (press! es gb kr ch)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string "") (open-output-string) gb kr))

(test-begin "selection-wiring")

;; --- Mode indicator: insert / normal / visual ---------------------------
;; The text indicator is OFF by default (the mode stays visible via the cursor
;; shape), so its strings are asserted with it switched ON.  #f while typing
;; (insert mode) regardless.  In plain normal mode vi-mode-indicator is #f, so
;; editor.ss supplies the "-- NORMAL --" fallback.  Once a characterwise visual
;; is entered vi-mode-indicator drives the "-- VISUAL --" string.
(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (vi-reset-session!)
  (parameterize ([show-mode-indicator? #t])
    (test-assert "the mode indicator is #f in insert mode"
      (eq? #f (editor-mode-indicator es)))
    (editor-state-mode-set! es 'normal)
    (test-equal "plain normal mode shows the editor NORMAL fallback"
      "-- NORMAL --" (editor-mode-indicator es))
    (press! es gb kr #\v)
    (test-equal "characterwise visual shows the vi VISUAL indicator"
      "-- VISUAL --" (editor-mode-indicator es))))

;; --- Selection range: the half-open emacs region ------------------------
;; With no visual mode active but an emacs mark set at 0 and point at 3, the
;; resolved span is the half-open min..max -- (0 . 3), with NO +1 (the region
;; excludes point), unlike the inclusive vi span.
(let ()
  (define gb (buffer-with-cursor "hello" 3))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (vi-reset-session!)
  (editor-state-mark-set! es 0)
  (test-equal "the emacs region is the half-open mark..point span (no +1)"
    '(0 . 3) (editor-selection-range es)))

;; --- Selection range: vi visual wins over the emacs mark ----------------
;; When BOTH a mark is set and a vi characterwise visual is active, the vi span
;; takes priority: the range is vi-visual-range (1 . 3 here), not the emacs
;; mark..point that would otherwise be (0 . 2).
(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'normal))
  (vi-reset-session!)
  (editor-state-mark-set! es 0)     ; the emacs mark alone would give (0 . 2)
  (press! es gb kr #\v)             ; enter characterwise visual
  (press! es gb kr #\l)             ; extend right -> cursor 2, vi range (1 . 3)
  (test-equal "vi visual takes priority over the emacs mark"
    (vi-visual-range gb) (editor-selection-range es))
  (test-equal "the resolved span is the vi range, not the emacs mark..point"
    '(1 . 3) (editor-selection-range es)))

;; --- Mark reset makes the region inactive -------------------------------
;; The reset finish! performs on submit -- (editor-state-mark-set! es #f) --
;; must leave no selection, so a mark set on one line never highlights the next
;; prompt. With the mark cleared and no visual mode, the range resolves to #f.
(let ()
  (define gb (buffer-with-cursor "hello" 3))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (vi-reset-session!)
  (editor-state-mark-set! es 0)
  (test-assert "a set mark yields an active selection range"
    (pair? (editor-selection-range es)))
  (editor-state-mark-set! es #f)    ; the very reset finish! performs on submit
  (test-assert "clearing the mark leaves no selection range"
    (eq? #f (editor-selection-range es))))

;; --- Phantom selection must not bleed into insert mode ------------------
;; The redisplay resolves its highlight span from the GLOBAL vi visual state,
;; and production never clears that state between submitting one line and
;; opening the next prompt.  A visual left active on one line is therefore
;; still active when the next read starts -- in insert mode -- so
;; editor-selection-range MUST refuse to surface a vi span while the buffer is
;; in insert mode, or a highlight the user never asked for paints itself as
;; they type.  This case deliberately does NOT call vi-reset-session! first:
;; the cases above reset per test, which is exactly what hides this leak.
(let ()
  ;; First line (es1): a characterwise visual, extended one cell.  Next prompt
  ;; (es2): a fresh insert-mode state that shares nothing but the still-active
  ;; GLOBAL vi visual -- editor-selection-range reads that global state at call
  ;; time, so es2 built up front is fine; the presses below arm it.
  (define gb1 (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es1 (state-for gb1 kr 'normal))
  (define gb2 (buffer-with-cursor "world" 3))
  (define es2 (state-for gb2 kr 'insert))
  ;; Enter a characterwise visual and extend it, leaving the global vi visual
  ;; active (as a submit without an intervening Escape would).
  (press! es1 gb1 kr #\v)
  (press! es1 gb1 kr #\l)
  (test-assert "the visual span is active on the first line (normal mode)"
    (pair? (editor-selection-range es1)))
  ;; The next prompt is in insert mode with the global vi visual still set.  No
  ;; highlight may leak into the line being typed.
  (test-assert "the stale vi visual is still active globally"
    (and (vi-visual-mode) #t))
  (test-assert "no phantom selection span leaks into the fresh insert prompt"
    (eq? #f (editor-selection-range es2)))
  ;; Restore a clean global vi state for any case added later.
  (vi-reset-session!))

(test-end)
