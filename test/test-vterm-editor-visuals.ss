;;; test/test-vterm-editor-visuals.ss -- The selection / mode / surround visuals
;;; asserted through the (test vterm) screen model, PTY-free.
;;;
;;; The sibling test-vterm-attrs.ss proves a cell's pen -- its colour and bold;
;;; this suite proves the editor visuals that resolve to no colour but to reverse
;;; video, a dim indicator row, or a buffer rewrite -- the ones the
;;; ghost-geometry and completion-overlay suites deliberately leave alone:
;;;
;;;   (1) the selection highlight.  The vi characterwise visual, the vi linewise
;;;       visual, the emacs mark/region and the s-expression selection all funnel
;;;       through the ONE editor-selection-range -> render-line/selection ->
;;;       display-with-selection path, which draws the span in SGR-7 reverse
;;;       video (never a colour, never a background), so each is asserted per
;;;       cell with cell-reverse? #t on the span and #f just outside it.  The
;;;       span glyph stays the literal buffer character at the default
;;;       foreground, because the selection branch draws plain rather than
;;;       colourising.
;;;
;;;   (2) the optional "-- VISUAL --" mode indicator, drawn dim on its own grid
;;;       row below the edit line when show-mode-indicator? is on and the buffer
;;;       is in a visual mode.
;;;
;;;   (3) the surround (ys) result: a buffer rewrite whose rewritten text -- the
;;;       inner-spaced "( alpha )" for an opening bracket, the tight "(alpha)"
;;;       for a closing one -- is read back as plain grid text.
;;;
;;; Every fixture is driven through a REAL render entry point under the harness's
;;; forced capability verdict (render->screen), never a hand-built escape string,
;;; and normal/visual/surround state is reached PTY-free through the vi state
;;; machine and editor-drive-keys! (an isolated Escape element decodes to
;;; 'escape, not Meta).  Prompt "> " shifts a buffer index i to grid column i+2.
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
        (only (hafod editor render) render-line render-line/selection)
        (chezscheme))

;; ======================================================================
;; Helpers
;; ======================================================================

;; A gap buffer holding TEXT with the cursor left at end-of-buffer.
(define (buffer-from text)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb text)
    gb))

;; A single-line gap-buffer holding TEXT with the cursor seated at INDEX.
;; gap-buffer-insert-string! leaves the cursor at the end, so a negative move of
;; (INDEX - length) walks it back to INDEX.
(define (buffer-with-cursor text index)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert-string! gb text)
    (gap-buffer-move-cursor! gb (- index (string-length text)))
    gb))

;; An editor-state around a buffer in MODE; the trailing #f is the initial mark
;; (no region active).  The 0 / #f / #f are the last-yank length, done? and
;; result fields.
(define (state-for gb kr mode)
  (make-editor-state gb kr "> " (open-output-string) 0 #f #f mode #f))

;; Feed one character key event through the vi state machine.  The input port is
;; bounded and empty: none of the single keys pressed here read a follow-up key.
(define (press! es gb kr ch)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string "") (open-output-string) gb kr))

;; Naive substring search (no srfi-13 dependency in this suite).
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

;; Assert the selection highlight on ROW of the folded screen SCR: every grid
;; column in the half-open range [LO,HI) reads reverse-video, and the single
;; column OUTSIDE reads plain.  One helper for all three highlights, since the
;; vi visual, the emacs region and the sexp selection share the one reverse-video
;; render path.  LABEL prefixes each generated message.
(define (assert-reverse-span scr row lo hi outside label)
  (let loop ([col lo])
    (when (< col hi)
      (test-assert (string-append label ": grid column "
                                  (number->string col) " is reverse-video")
        (let ([c (vterm-cell scr row col)])
          (and c (cell-reverse? c))))
      (loop (+ col 1))))
  (test-assert (string-append label ": grid column "
                              (number->string outside) " is NOT reverse-video")
    (let ([c (vterm-cell scr row outside)])
      (and c (not (cell-reverse? c))))))

(test-begin "vterm-editor-visuals")

;; ======================================================================
;; (1) The pure render-path proof: a LITERAL (start . end) span, no editor-state.
;; render-line/selection given a pair sel-range draws the buffer through
;; display-with-selection -- reverse video (ESC[7m..ESC[27m) over the span,
;; PLAIN everywhere else.  Over "(abc)" with prompt "> ", the span (1 . 4) covers
;; buffer indices 1,2,3 (a,b,c), which land at grid columns 3,4,5; the parens at
;; indices 0 and 4 (columns 2 and 6) and the prompt (column 0) sit outside it.
;; The span glyph must read the literal buffer character at the DEFAULT
;; foreground -- the selection branch draws plain, it does not colourise.
;; ======================================================================

(let* ([scr (render->screen 80
              (lambda (p)
                (render-line/selection p "> " (buffer-from "(abc)") 0 '(1 . 4) #f 80)))]
       [in-a  (vterm-cell scr 0 3)]      ; buffer index 1, 'a'
       [in-b  (vterm-cell scr 0 4)]      ; buffer index 2, 'b'
       [in-c  (vterm-cell scr 0 5)]      ; buffer index 3, 'c'
       [open-paren  (vterm-cell scr 0 2)] ; buffer index 0, '('
       [close-paren (vterm-cell scr 0 6)] ; buffer index 4, ')'
       [prompt-cell (vterm-cell scr 0 0)]) ; the prompt '>'
  ;; In-span: the three covered glyphs are reverse-video, and the first reads the
  ;; literal 'a' at the default foreground (drawn plain, not colourised).
  (test-assert "literal span: buffer index 1 (column 3) is reverse-video"
    (cell-reverse? in-a))
  (test-equal "literal span: the in-span glyph is the literal buffer char a"
    #\a (cell-glyph in-a))
  (test-equal "literal span: the in-span glyph reads the default foreground (plain)"
    'default (cell-fg in-a))
  (test-assert "literal span: buffer index 2 (column 4) is reverse-video"
    (cell-reverse? in-b))
  (test-assert "literal span: buffer index 3 (column 5) is reverse-video"
    (cell-reverse? in-c))
  ;; Out-of-span: the enclosing parens and the prompt are not reverse-video, so
  ;; the span really is bounded and the proof is not vacuous.
  (test-assert "literal span: the open paren (index 0, column 2) is NOT reverse"
    (not (cell-reverse? open-paren)))
  (test-assert "literal span: the close paren (index 4, column 6) is NOT reverse"
    (not (cell-reverse? close-paren)))
  (test-assert "literal span: the prompt cell (column 0) is NOT reverse"
    (not (cell-reverse? prompt-cell))))

;; ======================================================================
;; (2) End-to-end selection highlights: the PRODUCT resolves each span through
;; editor-selection-range, and the renderer draws it reverse-video.  Each block
;; sets the state (a vi visual, an emacs mark), resolves the span with the SAME
;; entry point the editor feeds the renderer, renders it and reads the grid.  The
;; vi session is reset around each block so a left-over visual cannot bleed into
;; the next assertion.
;; ======================================================================

;; vi characterwise visual: v then l over "hello" from index 1 covers indices
;; 1,2 -> the (1 . 3) span -> grid columns 3,4 reverse, index 0 (column 2) not.
(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'normal))
  (vi-reset-session!)
  (press! es gb kr #\v)                 ; enter characterwise visual
  (press! es gb kr #\l)                 ; extend right -> cursor 2, range (1 . 3)
  (let* ([sel (editor-selection-range es)]
         [scr (render->screen 80
                (lambda (p) (render-line/selection p "> " gb 0 sel #f 80)))])
    (test-equal "vi-visual char: editor-selection-range resolves (1 . 3)"
      '(1 . 3) sel)
    (assert-reverse-span scr 0 3 5 2 "vi-visual char"))
  (vi-reset-session!))

;; vi linewise visual: V over "hello" covers the whole line -> (0 . 5) -> grid
;; columns 2..6 reverse, the column just past the text (7) not.
(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'normal))
  (vi-reset-session!)
  (press! es gb kr #\V)                 ; enter linewise visual -> (0 . 5)
  (let* ([sel (editor-selection-range es)]
         [scr (render->screen 80
                (lambda (p) (render-line/selection p "> " gb 0 sel #f 80)))])
    (test-equal "vi-visual line: editor-selection-range is the whole-line span"
      '(0 . 5) sel)
    (assert-reverse-span scr 0 2 7 7 "vi-visual line"))
  (vi-reset-session!))

;; emacs mark/region: mark at 0 with point at 3 over "hello" (insert mode) -> the
;; half-open (0 . 3) span -> grid columns 2,3,4 reverse, index 3 (column 5) not.
(let ()
  (define gb (buffer-with-cursor "hello" 3))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (vi-reset-session!)
  (editor-state-mark-set! es 0)
  (let* ([sel (editor-selection-range es)]
         [scr (render->screen 80
                (lambda (p) (render-line/selection p "> " gb 0 sel #f 80)))])
    (test-equal "emacs region: editor-selection-range is the half-open (0 . 3)"
      '(0 . 3) sel)
    (assert-reverse-span scr 0 2 5 5 "emacs region"))
  (vi-reset-session!))

;; ======================================================================
;; (3) The "-- VISUAL --" mode indicator as its own grid row.  With
;; show-mode-indicator? on, a characterwise visual draws "-- VISUAL --" dim on
;; the row below the edit line; the folded grid keeps the edit row above it.
;; ("-- NORMAL --" as a grid row is already covered by the mode-indicator suite;
;; this adds the visual-mode string.)
;; ======================================================================

(let ()
  (define gb (buffer-with-cursor "hello" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'normal))
  (vi-reset-session!)
  (parameterize ([show-mode-indicator? #t])
    (press! es gb kr #\v)               ; characterwise visual -> "-- VISUAL --"
    (let* ([sel (editor-selection-range es)]
           [ind (editor-mode-indicator es)]
           [scr (render->screen 80
                  (lambda (p) (render-line/selection p "> " gb 0 sel ind 80)))])
      (test-equal "visual indicator: editor-mode-indicator yields the visual string"
        "-- VISUAL --" ind)
      (test-assert "visual indicator: the edit row and an indicator row are drawn"
        (>= (vterm-rows scr) 2))
      (test-equal "visual indicator: it draws on its own row below the edit line"
        "-- VISUAL --" (vterm-row-text scr 1))
      (test-assert "visual indicator: the indicator row renders dim"
        (cell-dim? (vterm-cell scr 1 0)))))
  (vi-reset-session!))

;; ======================================================================
;; (4) s-expression selection + expand.  With the cursor on the inner list's
;; open paren of "(a (b) c)" (index 3), cmd-select-sexp selects the inner "(b)"
;; via apply-span!'s emacs mark -- the SAME reverse-video path -- and
;; cmd-expand-region grows the span to the enclosing form.  Rendering each
;; resolved span reads back a reverse-video run over its grid columns
;; (buffer index i -> grid column i+2), and the expanded run is strictly wider.
;; ======================================================================

(let ()
  (define gb (buffer-with-cursor "(a (b) c)" 3))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'normal))
  (vi-reset-session!)
  (cmd-select-sexp es)
  (let* ([inner (editor-selection-range es)]
         [scr (render->screen 80
                (lambda (p) (render-line/selection p "> " gb 0 inner #f 80)))])
    (test-assert "sexp select: a non-empty inner span is resolved"
      (and (pair? inner) (< (car inner) (cdr inner))))
    ;; Pin the span concretely so a wrong-but-non-empty selection cannot pass:
    ;; the cursor at index 3 of "(a (b) c)" selects the "(b)" form, span (3 . 6).
    (test-equal "sexp select: inner span is the (b) form" '(3 . 6) inner)
    ;; the resolved span renders reverse-video across its grid columns; the
    ;; column just past its end (a half-open boundary) does not.
    (assert-reverse-span scr 0 (+ (car inner) 2) (+ (cdr inner) 2)
                         (+ (cdr inner) 2) "sexp select")
    ;; expand: the span grows to the enclosing form and stays reverse-video.
    (cmd-expand-region es)
    (let* ([outer (editor-selection-range es)]
           [scr2 (render->screen 80
                   (lambda (p) (render-line/selection p "> " gb 0 outer #f 80)))])
      (test-assert "sexp expand: the span grows to the enclosing form"
        (and (pair? outer)
             (<= (car outer) (car inner))
             (>= (cdr outer) (cdr inner))
             (> (- (cdr outer) (car outer)) (- (cdr inner) (car inner)))))
      (test-equal "sexp expand: outer span is the whole (a (b) c) form"
        '(0 . 9) outer)
      (assert-reverse-span scr2 0 (+ (car outer) 2) (+ (cdr outer) 2)
                           (+ (cdr outer) 2) "sexp expand")))
  (vi-reset-session!))

;; ======================================================================
;; (5) Surround (ys): an edit that rewrites the buffer, so the RESULT is read
;; back as plain grid text through render-line.  editor-drive-keys! reaches
;; normal mode PTY-free via an isolated Escape element ((string #\x1b) as its own
;; list element decodes to 'escape, not Meta), then ysiw wraps the word.  An
;; opening bracket adds one inner space per side; a closing bracket adds none.
;; ======================================================================

;; ysiw( on "alpha": the opening paren wraps with inner spaces -> "( alpha )".
(let ()
  (define gb (buffer-from "alpha"))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (vi-reset-session!)
  (editor-drive-keys! es (list (string #\x1b) "ysiw("))
  (let ([scr (render->screen 80 (lambda (p) (render-line p "> " gb 0 80)))])
    (test-equal "surround ysiw(: the opening paren wraps with inner spaces"
      "> ( alpha )" (vterm-row-text scr 0)))
  (vi-reset-session!))

;; ysiw) on "alpha": the closing paren wraps tight -> "(alpha)".
(let ()
  (define gb (buffer-from "alpha"))
  (define kr (make-kill-ring))
  (define es (state-for gb kr 'insert))
  (vi-reset-session!)
  (editor-drive-keys! es (list (string #\x1b) "ysiw)"))
  (let ([scr (render->screen 80 (lambda (p) (render-line p "> " gb 0 80)))])
    (test-equal "surround ysiw): the closing paren wraps tight"
      "> (alpha)" (vterm-row-text scr 0)))
  (vi-reset-session!))

(test-end)
