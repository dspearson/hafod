;;; test/test-vterm-attrs.ss -- Per-cell colour/attribute assertions folded
;;; through the virtual terminal, plus the two-frame erase-to-end-of-screen
;;; property.
;;;
;;; The sibling test-vterm-render.ss proves the harness recovers the grid text
;;; and the cursor geometry from a real render frame; this suite proves the
;;; other half of a screen cell -- its pen.  It drives the genuine render-line
;;; and render-completion-grid into the (test vterm) screen model via
;;; render->screen (which forces the capability verdict on so a plain string
;;; port emits its full escape stream), then reads back the SGR attributes the
;;; renderer wrote onto individual cells: the 24-bit foreground of a rainbow
;;; paren, the bold of a matched paren, and the 24-bit background of a selected
;;; completion cell -- each at a named (row, column), each on real renderer
;;; bytes rather than a hand-built escape string.  A non-coloured cell is
;;; asserted to read the default foreground so the colour checks are not
;;; vacuous.
;;;
;;; The erase section drives render-line/selection twice into ONE capture: a
;;; first frame that draws a mode-indicator row below the edit line, then a
;;; second frame that suppresses it -- so the second frame's ESC[<n>A / \r /
;;; ESC[J redraw climbs over and erases the indicator row.  The folded grid is
;;; asserted to carry the edit line and no indicator residue, mirroring the
;;; submit-time redraw the editor performs.
;;;
;;; The final section is the harness's headline use: it drives the real
;;; read-expression editor loop with an injected key string over an
;;; open-input-string -- no pty -- under the forced capability verdict, so the
;;; loop emits its full escape stream (entry cursor shape/colour, the Ctrl-L
;;; clear, the prompt frame, and the submit reset) into a plain string port.
;;; The total parser folds that stream into a clean grid, skipping the OSC and
;;; cursor-shape escapes rather than rendering their payload, and end-of-input
;;; submits so the drive terminates deterministically with no drain loop.
;;;
;;; Entirely PTY-free -- it opens no terminal and needs no platform gate, so it
;;; runs identically on every platform.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor render)
              render-line render-line/selection render-completion-grid)
        (only (hafod editor editor) read-expression)
        (hafod editor gap-buffer)
        ;; Pin $LS_COLORS around the colour-target grid drive so a non-selected
        ;; candidate's colour is deterministic (dev box vs CI).
        (only (hafod environment) with-env* setenv)
        (chezscheme))

;; ======================================================================
;; Helpers
;; ======================================================================

;; A gap buffer holding TEXT with the cursor seated at INDEX.
;; gap-buffer-set-from-string! leaves the gap at the end (index = length), so a
;; negative move of (INDEX - length) walks it back to INDEX.  Seating the cursor
;; INSIDE a pair of parens is what makes the renderer bold that matched pair.
(define (buffer-cursor-at text index)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb text)
    (gap-buffer-move-cursor! gb (- index (string-length text)))
    gb))

;; A gap buffer holding TEXT with the cursor left at end-of-buffer.
(define (buffer-from text)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb text)
    gb))

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

;; Pin $LS_COLORS around a colour-target grid drive so a candidate's colour is
;; the palette under test, never the box's ambient value.  A #f pin UNSETS via
;; setenv (the built-in default palette resolves) -- NOT a with-env* #f delta,
;; which would route the unset through the C setenv, whose string arg cannot take
;; #f.  A string pin installs an override.  The ambient value is restored either
;; way.  The grid here passes no threaded type, so the pin only neutralises the
;; ambient value: the selected cell takes the selection colour regardless.
(define (with-ls-colors pin thunk)
  (if pin
      (with-env* (list (cons "LS_COLORS" pin)) thunk)
      (let ([saved (getenv "LS_COLORS")])
        (dynamic-wind
          (lambda () (setenv "LS_COLORS" #f))
          thunk
          (lambda () (setenv "LS_COLORS" saved))))))

(test-begin "vterm-attrs")

;; ======================================================================
;; (1) Per-cell 24-bit foreground and bold on a real colourised render frame.
;; Driving render-line on "(+ 1 2)" with the cursor seated INSIDE the parens
;; colours the depth-0 parens with the rainbow palette's first entry and bolds
;; the matched open/close pair.  The harness reads those attributes back off the
;; exact cells the glyphs landed on -- "> (+ 1 2)" places "(" at column 2 and
;; ")" at column 8 -- so the assertion is glyph AND colour AND bold at a
;; (row, column), never a restatement of the escape bytes.
;; ======================================================================

;; Cursor at index 3 (on the "1"), enclosed by the outer parens, so
;; find-enclosing-parens bolds the pair at indices 0 and 6.
(let* ([gb (buffer-cursor-at "(+ 1 2)" 3)]
       [scr (render->screen 80 (lambda (p) (render-line p "> " gb 0 80)))]
       [open-cell  (vterm-cell scr 0 2)]
       [close-cell (vterm-cell scr 0 8)]
       [prompt-cell (vterm-cell scr 0 1)])
  ;; The depth-0 open paren: glyph, 24-bit foreground, and bold, all per-cell.
  (test-equal "colour: the open paren glyph is ("
    #\( (cell-glyph open-cell))
  (test-equal "colour: the depth-0 paren carries the rainbow depth-0 foreground"
    '(255 153 94) (cell-fg open-cell))
  (test-equal "colour: the paren cell has no background colour (default)"
    'default (cell-bg open-cell))
  (test-assert "colour: the matched open paren is bold"
    (cell-bold? open-cell))
  ;; The matched close paren carries the same depth-0 colour and its own bold.
  (test-equal "colour: the close paren glyph is )"
    #\) (cell-glyph close-cell))
  (test-equal "colour: the close paren shares the depth-0 foreground"
    '(255 153 94) (cell-fg close-cell))
  (test-assert "colour: the matched close paren is bold per-cell"
    (cell-bold? close-cell))
  ;; Non-vacuous: a prompt cell the renderer never colours reads the default
  ;; foreground and no bold, so the colour equalities above are meaningful.
  (test-equal "colour: an uncoloured prompt cell reads the default foreground"
    'default (cell-fg prompt-cell))
  (test-assert "colour: the uncoloured prompt cell is not bold"
    (not (cell-bold? prompt-cell))))

;; ======================================================================
;; (2) Per-cell 24-bit BACKGROUND on a real render frame.  render-completion-grid
;; paints the selected candidate with a truecolour background (and foreground);
;; the harness reads that background back on the selected cell, while a blank
;; cell outside the selection reads the default background -- so a per-cell
;; 24-bit background is assertable, not just a foreground.
;; ======================================================================

;; Two candidates, the first selected: its cell gets sel-bg/sel-fg.  "foo"
;; truncates to "fo…" after a two-space indent, so the "f" lands at column 2.
(let* ([scr (with-ls-colors #f
              (lambda ()
                (render->screen 40
                  (lambda (p)
                    (render-completion-grid p '(("foo" ()) ("bar" ())) 0 40 10)))))]
       [sel-cell (vterm-cell scr 1 2)]
       [blank-cell (vterm-cell scr 0 0)])
  (test-equal "background: the selected candidate glyph is f"
    #\f (cell-glyph sel-cell))
  (test-equal "background: the selected cell carries a 24-bit background"
    '(56 62 87) (cell-bg sel-cell))
  (test-equal "background: the selected cell also carries a 24-bit foreground"
    '(195 202 230) (cell-fg sel-cell))
  (test-equal "background: a cell outside the selection reads the default background"
    'default (cell-bg blank-cell)))

;; ======================================================================
;; (3) ESC[J erase-to-end-of-screen leaves no residue.  Two frames of
;; render-line/selection fold into ONE capture: frame 1 draws a "-- NORMAL --"
;; indicator row below the edit line; frame 2, passing frame 1's returned
;; cursor row as prev-lines with the indicator suppressed, climbs back and emits
;; \r ESC[J -- erasing the indicator row.  The final grid keeps the edit line and
;; drops the indicator, exactly as the editor's submit-time redraw does.
;; ======================================================================

;; Sanity first: a single frame WITH the indicator really does draw it on its
;; own row, so the erase assertion below is not vacuously true.
(let* ([scr (render->screen 80
              (lambda (p)
                (render-line/selection p "> " (buffer-from "(+ 1 2)")
                                       0 #f "-- NORMAL --" 80)))])
  (test-assert "erase sanity: frame 1 has at least the edit row plus an indicator row"
    (>= (vterm-rows scr) 2))
  (test-equal "erase sanity: the indicator is drawn on its own row"
    "-- NORMAL --" (vterm-row-text scr 1)))

;; The two-frame redraw: the second frame's \r ESC[J erases the indicator row.
(let* ([gb (buffer-from "(+ 1 2)")]
       [scr (render->screen 80
              (lambda (p)
                (let ([r (render-line/selection p "> " gb 0 #f "-- NORMAL --" 80)])
                  (render-line/selection p "> " gb r #f #f 80))))])
  (test-equal "erase: the final grid collapses to the single edit row"
    1 (vterm-rows scr))
  (test-equal "erase: the edit line survives the redraw"
    "> (+ 1 2)" (vterm-row-text scr 0))
  (test-assert "erase: no -- NORMAL -- residue remains in any row"
    (not (any-line-has? scr "-- NORMAL --"))))

;; ======================================================================
;; (4) Injected-key read-expression, folded through the harness PTY-free.
;; Driving the real editor loop with keys over an open-input-string (no pty)
;; under render->screen's forced verdict makes read-expression emit its whole
;; escape stream -- entry cursor shape/colour, the Ctrl-L clear, the prompt
;; frame, and the submit reset -- into the string port.  End-of-input submits,
;; so the drive returns at once with no unbounded read.  The parser must fold
;; that stream into a clean prompt grid, skipping the OSC cursor-colour and
;; cursor-shape escapes rather than leaking their payload as glyphs.
;; ======================================================================

;; The mode-independent Ctrl-L drive; end-of-input then submits (no drain loop).
(define editor-drive-keys (string #\x0c))

(let* ([scr (render->screen 80
              (lambda (p)
                (read-expression "> " (open-input-string editor-drive-keys) p)))]
       [c0 (vterm-cell scr 0 0)]
       [c1 (vterm-cell scr 0 1)])
  ;; The prompt survived onto row 0: its first two cells are "> ".
  (test-equal "injected-key: row 0 begins with the prompt glyph >"
    #\> (cell-glyph c0))
  (test-equal "injected-key: the prompt's second cell is a space"
    #\space (cell-glyph c1))
  ;; No OSC / cursor-shape payload leaked into the grid: the row is EXACTLY the
  ;; prompt (trailing blank trimmed), with no stray "12"/"#"/digits from the
  ;; OSC-12 cursor-colour string or the cursor-shape CSI.
  (test-equal "injected-key: row 0 is exactly the prompt (no escape payload leaked)"
    ">" (vterm-row-text scr 0))
  ;; The drive ran the WHOLE loop including submit: end-of-input submits, and the
  ;; editor's submit path emits a trailing newline to open the result line, so
  ;; the deterministic terminus is the fresh row below the prompt -- row 1,
  ;; column 0 -- not the pre-submit prompt column.  Asserting it proves the
  ;; injected-key loop parsed to a clean grid and a deterministic cursor,
  ;; PTY-free.
  (test-equal "injected-key: the cursor comes to rest on the submit row"
    1 (vterm-cursor-row scr))
  (test-equal "injected-key: the submit newline reset the cursor to column 0"
    0 (vterm-cursor-col scr)))

(test-end)
