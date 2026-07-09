;;; test/test-mode-indicator.ss -- The optional text mode indicator.
;;; The active editing mode is always visible through the cursor shape and
;;; colour, so the textual "-- NORMAL --" / "-- VISUAL --" row is redundant and
;;; OFF by default; a user's init may switch it on with (show-mode-indicator?
;;; #t).  Four PTY-free layers:
;;;   (A) the gate: editor-mode-indicator is #f in every mode by default, and
;;;       yields the mode strings only when the parameter is switched on;
;;;   (B) geometry: what the default feeds the renderer -- a #f indicator --
;;;       draws no extra row and is byte-identical to a plain render-line on a
;;;       non-tty string port;
;;;   (C) the (test vterm) harness (ansi-ok?/colour-ok? forced on): two frames
;;;       fold into one virtual-terminal capture -- frame 1 draws the indicator
;;;       row while editing, frame 2 is the suppressed submit re-render whose
;;;       \r ESC[J climbs over and erases it -- and the folded grid collapses to
;;;       the single edit row with no "-- NORMAL --" residue to collide with the
;;;       printed result;
;;;   (D) regression: the toggle governs only the indicator -- an emacs region
;;;       and a vi visual still resolve through editor-selection-range with it on.
;;; Entirely PTY-free: it opens no terminal and needs no platform gate, so it
;;; runs identically on every platform.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (hafod editor editor)
        (hafod editor gap-buffer)
        (hafod editor kill-ring)
        (hafod editor input-decode)
        (hafod editor vi)
        (hafod editor history)
        (hafod editor sexp-tracker)
        (only (hafod editor render)
              render-line render-line/selection
              cursor-visual-row ansi-display-width)
        (only (hafod environment) setenv)
        (chezscheme))

;; ======================================================================
;; Helpers
;; ======================================================================

;; Build a single-line gap-buffer holding TEXT with the cursor at INDEX.
(define (buffer-with-cursor text index)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert-string! gb text)
    (gap-buffer-move-cursor! gb (- index (string-length text)))
    gb))

;; Build a gap-buffer holding TEXT with the cursor left at end-of-buffer.
(define (buffer-from text)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb text)
    gb))

;; An editor-state around a buffer in MODE; the trailing #f is the initial mark
;; (no region active).
(define (state-for gb kr mode)
  (make-editor-state gb kr "> " (open-output-string) 0 #f #f mode #f))

;; Feed one character key event through the vi state machine.  The input port is
;; bounded and empty: none of the keys exercised here read a follow-up key.
(define (press! es gb kr ch)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string "") (open-output-string) gb kr))

;; Does the string carry any ESC (\x1b) byte at all?
(define (has-esc? s)
  (let ([n (string-length s)])
    (let scan ([i 0])
      (and (< i n)
           (or (char=? (string-ref s i) #\x1b)
               (scan (+ i 1)))))))

;; Naive substring search (no dependency on srfi-13 in this suite).
(define (contains-substring? hay needle)
  (let ([hl (string-length hay)]
        [nl (string-length needle)])
    (let outer ([i 0])
      (cond
        [(> (+ i nl) hl) #f]
        [(string=? (substring hay i (+ i nl)) needle) #t]
        [else (outer (+ i 1))]))))

;; Does ANY row of the folded grid contain NEEDLE?
(define (any-line-has? scr needle)
  (let loop ([lines (vterm-lines scr)])
    (cond
      [(null? lines) #f]
      [(contains-substring? (car lines) needle) #t]
      [else (loop (cdr lines))])))

(test-begin "mode-indicator")

;; Pitfall pin: section (B) renders through the live colour-ok? on a plain
;; string port and asserts zero escape bytes.  colour-ok? now reads
;; CLICOLOR_FORCE, so an ambient CLICOLOR_FORCE=1 would force colour on that
;; non-tty sink and flip the assert.  Pin it unset for the whole suite (no
;; with-env* delta here sets it, so this baseline persists).
(setenv "CLICOLOR_FORCE" #f)

;; ======================================================================
;; (A) The gate: default OFF, opt-in ON.
;; ======================================================================

;; Default (parameter OFF): #f in EVERY mode -- the mode stays visible via the
;; cursor shape, so no text row is ever drawn out of the box.
(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (vi-reset-session!)
  (test-assert "default off: #f in insert mode"
    (eq? #f (editor-mode-indicator es)))
  (editor-state-mode-set! es 'normal)
  (test-assert "default off: #f in normal mode (no -- NORMAL -- drawn)"
    (eq? #f (editor-mode-indicator es)))
  (press! es gb kr #\v)                 ; enter characterwise visual
  (test-assert "default off: #f in visual mode"
    (eq? #f (editor-mode-indicator es)))
  (vi-reset-session!))

;; Parameter ON: the insert gate still yields #f while typing, normal mode shows
;; the "-- NORMAL --" fallback, and a characterwise visual shows the vi string.
(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (vi-reset-session!)
  (parameterize ([show-mode-indicator? #t])
    (test-assert "on: still #f in insert mode (typing shows nothing)"
      (eq? #f (editor-mode-indicator es)))
    (editor-state-mode-set! es 'normal)
    (test-equal "on: normal mode shows the -- NORMAL -- fallback"
      "-- NORMAL --" (editor-mode-indicator es))
    (press! es gb kr #\v)
    (test-equal "on: characterwise visual shows the vi -- VISUAL -- string"
      "-- VISUAL --" (editor-mode-indicator es)))
  (vi-reset-session!))

;; The coercion mirrors the sibling toggles: any truthy value normalises to #t.
(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'normal))
  (vi-reset-session!)
  (parameterize ([show-mode-indicator? 'yes])
    (test-equal "on: a truthy value coerces to enabled"
      "-- NORMAL --" (editor-mode-indicator es)))
  (vi-reset-session!))

;; ======================================================================
;; (B) Geometry: a #f indicator draws no extra row (non-tty string port).
;; ======================================================================

;; What the DEFAULT feeds the renderer is indicator #f.  On a plain string port
;; (a non-tty) render-line/selection with a #f indicator and no selection must
;; emit exactly what render-line emits and return the same logical row -- i.e.
;; no extra indicator row, identical geometry to a plain edit line.
(let* ([prompt "> "]
       [text "(+ 9 5 4)"]
       [sp1 (open-output-string)]
       [gb1 (buffer-from text)]
       [row1 (render-line/selection sp1 prompt gb1 0 #f #f 80)]
       [out1 (get-output-string sp1)]
       [sp2 (open-output-string)]
       [gb2 (buffer-from text)]
       [row2 (render-line sp2 prompt gb2 0 80)]
       [out2 (get-output-string sp2)])
  (test-assert "indicator #f: emits zero ESC bytes to a non-tty sink"
    (not (has-esc? out1)))
  (test-equal "indicator #f: same logical row as a plain render-line"
    row2 row1)
  (test-equal "indicator #f: byte-identical to a plain render-line"
    out2 out1)
  (test-assert "indicator #f: no -- NORMAL -- row is drawn"
    (not (contains-substring? out1 "-- NORMAL --"))))

;; ======================================================================
;; (C) Harness: the indicator draws, and the submit re-render clears it.
;; ======================================================================

;; Two frames folded into ONE virtual-terminal capture.  render->screen forces
;; the capability verdict on, so a plain string port emits its full escape
;; stream (ansi-ok?/colour-ok? #t) with no terminal:
;;   frame 1: render-line/selection WITH the indicator draws "-- NORMAL --" on
;;            its own row below the edit line (the pre-submit state);
;;   frame 2: the bare render-line the submit path (finish!) performs -- passing
;;            frame 1's returned cursor row as prev-lines, it climbs to the
;;            prompt line and clears to end of screen (\r ESC[J), wiping the
;;            indicator row, then redraws with the indicator suppressed.
;; Reading the folded grid proves the residue is absent from the SCREEN itself,
;; strictly stronger than the old scan for "absent after the last ESC[J": the
;; grid collapses to the single edit row, so the printed result cannot collide
;; with a leftover indicator.

;; Sanity first: a single frame WITH the indicator really does draw it on its
;; own row, so the erase assertion below is not vacuously true.
(let* ([scr (render->screen 80
              (lambda (p)
                (render-line/selection p "> " (buffer-from "(+ 9 5 4)")
                                       0 #f "-- NORMAL --" 80)))])
  (test-assert "harness: frame 1 has at least the edit row plus an indicator row"
    (>= (vterm-rows scr) 2))
  (test-equal "harness: the indicator is drawn on its own row"
    "-- NORMAL --" (vterm-row-text scr 1)))

;; The two-frame redraw: frame 2's \r ESC[J climbs over and erases the indicator
;; row.  render-line (indicator suppressed), passed frame 1's returned row as
;; prev-lines, is exactly the call finish! makes on submit.
(let* ([gb (buffer-from "(+ 9 5 4)")]
       [scr (render->screen 80
              (lambda (p)
                (let ([r (render-line/selection p "> " gb 0 #f "-- NORMAL --" 80)])
                  (render-line p "> " gb r 80))))])
  (test-equal "harness: the final grid collapses to the single edit row"
    1 (vterm-rows scr))
  (test-equal "harness: the edit line survives the redraw"
    "> (+ 9 5 4)" (vterm-row-text scr 0))
  (test-assert "harness: no -- NORMAL -- residue remains in any row"
    (not (any-line-has? scr "-- NORMAL --"))))

;; ======================================================================
;; (D) Regression: the toggle governs only the indicator, never the selection.
;; ======================================================================

;; editor-selection-range is independent of show-mode-indicator?: with the
;; indicator ON an emacs mark still resolves to the half-open mark..point span.
(let ()
  (define gb (buffer-with-cursor "hello" 3))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (vi-reset-session!)
  (parameterize ([show-mode-indicator? #t])
    (editor-state-mark-set! es 0)
    (test-equal "on: the emacs region still resolves to the half-open span"
      '(0 . 3) (editor-selection-range es))))

;; With the indicator ON a vi characterwise visual still resolves, and its
;; "-- VISUAL --" string shows alongside the highlighted span.
(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'normal))
  (vi-reset-session!)
  (parameterize ([show-mode-indicator? #t])
    (press! es gb kr #\v)               ; enter characterwise visual
    (press! es gb kr #\l)               ; extend right -> cursor 2, vi range (1 . 3)
    (test-equal "on: the vi visual span still resolves"
      '(1 . 3) (editor-selection-range es))
    (test-equal "on: the visual indicator shows alongside the selection"
      "-- VISUAL --" (editor-mode-indicator es)))
  (vi-reset-session!))

(test-end)
