;;; test/test-vterm-render.ss -- The real render path folded through the virtual
;;; terminal, cross-checked against the renderer's own geometry oracle.
;;;
;;; This suite drives the genuine render-line / render-line/suggestion into the
;;; (test vterm) screen model via render->screen (which forces the capability
;;; verdict on so a plain string port emits its full escape stream), then reads
;;; the folded grid back.  The cursor the harness DERIVES from those escapes is
;;; cross-checked against cursor-visual-row plus the renderer's column formula,
;;; and the grid row count against count-visual-lines -- never against a
;;; restatement of the escape bytes.  Because the driven output is the real
;;; renderer and the oracle is render.ss's own pure helper, the agreement is
;;; non-circular: it proves the harness parses the renderer's real bytes and
;;; recovers a (row, column) cursor, not the raw buffer offset.
;;;
;;; Entirely PTY-free -- it opens no terminal and needs no platform gate, so it
;;; runs identically on every platform.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor render)
              render-line render-line/suggestion ghost-visible-suffix
              ansi-display-width cursor-visual-row+col count-visual-lines
              paren-colours)
        (hafod editor gap-buffer)
        (chezscheme))

;; ======================================================================
;; Helpers
;; ======================================================================

;; Build a gap buffer holding text with the cursor left at end-of-buffer
;; (gap-buffer-set-from-string! leaves the gap at the string length, so the whole
;; text is "before" and "after" is empty -- the same end-of-buffer case the
;; ghost renderer draws for).
(define (buffer-from text)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb text)
    gb))

;; A gap buffer holding TEXT with the cursor seated at INDEX, so the after-cursor
;; string is non-empty -- the case buffer-from cannot build, and the one the
;; editor actually produces once paredit has auto-inserted a closer.
;; gap-buffer-set-from-string! leaves the gap at the end (index = length), so a
;; negative move of (INDEX - length) walks it back to INDEX.
(define (buffer-cursor-at text index)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb text)
    (gap-buffer-move-cursor! gb (- index (string-length text)))
    gb))

;; The 256-colour index the renderer paints the ghost with (ESC[38;5;240m).
(define ghost-grey 240)

;; Does any cell on ROW carry the ghost's dim grey?  Reads the pen the renderer
;; actually left on each cell, so an un-drawn ghost is provable, not assumed.
(define (row-has-ghost-grey? scr row)
  (let loop ([col 0])
    (cond
      [(>= col (vterm-cols scr)) #f]
      [(eqv? (cell-fg (vterm-cell scr row col)) ghost-grey) #t]
      [else (loop (+ col 1))])))

;; The (row, 0-based col) the renderer's own geometry oracle predicts for a
;; cursor at the end of `before` under this prompt and terminal width.  Both come
;; from the SAME wrap-aware walk the renderer uses (cursor-visual-row+col): the
;; visual row, and the 0-based column WITHIN the final visual row -- which on a
;; line that wraps beyond the terminal width is the wrapped column, not the
;; wrap-blind width since the last newline.  render.ss emits the 1-based
;; cursor-col as that column + 1; the 0-based grid column the harness reads back
;; drops the +1, so the oracle column is the walk's col directly.
(define (expected-cursor prompt before term-cols)
  (let ([rc (cursor-visual-row+col (ansi-display-width prompt) before term-cols)])
    (values (car rc) (cdr rc))))

;; Drive render-line for `before` under `prompt`/`cols` into a fresh screen.
(define (screen-of prompt before cols)
  (render->screen cols
    (lambda (p) (render-line p prompt (buffer-from before) 0 cols))))

(test-begin "vterm-render")

;; ======================================================================
;; (1) Cursor (row, col) cross-check -- the derived cursor, PARSED from the
;; escapes a real render-line emits, agrees with cursor-visual-row and the
;; column formula across single-line, multi-line, wrapped, and ghost inputs.
;; The cursor is read from the escape stream, never taken from the buffer offset.
;; ======================================================================

;; (a) Single line: the cursor sits after the prompt plus the whole buffer width.
(let* ([prompt "> "] [before "(+ 1 2)"] [cols 80]
       [scr (screen-of prompt before cols)])
  (let-values ([(er ec) (expected-cursor prompt before cols)])
    (test-equal "single-line: cursor row agrees with cursor-visual-row"
      er (vterm-cursor-row scr))
    (test-equal "single-line: cursor col agrees with the render formula"
      ec (vterm-cursor-col scr))))

;; (b) Multi-line (embedded newline): the cursor drops to the newline's row and
;; the column is the width AFTER the newline -- provably not the raw buffer
;; offset, so the cross-check cannot pass by echoing the cursor index.
(let* ([prompt "> "] [before "(a\n(b"] [cols 80]
       [scr (screen-of prompt before cols)])
  (let-values ([(er ec) (expected-cursor prompt before cols)])
    (test-equal "multi-line: cursor row agrees with cursor-visual-row"
      er (vterm-cursor-row scr))
    (test-equal "multi-line: cursor col agrees with the render formula"
      ec (vterm-cursor-col scr))
    ;; The derived column is the post-newline width (2), whereas the raw
    ;; buffer offset (the end-of-buffer cursor index) is (string-length before)
    ;; = 5.  Guarding derived-col /= offset stops a coincidental pass.
    (test-equal "multi-line: derived column is the render geometry (2), not the offset"
      2 (vterm-cursor-col scr))
    (test-assert "multi-line: derived column is not the raw buffer offset"
      (not (= (vterm-cursor-col scr) (string-length before))))))

;; (c) Width-wrapped: a line longer than a narrow terminal wraps onto further
;; rows; the derived cursor still tracks the renderer's geometry oracle.
(let* ([prompt "> "] [before "abcdefghijklmnopqrst"] [cols 10]  ; 20 glyphs, width 10
       [scr (screen-of prompt before cols)])
  (let-values ([(er ec) (expected-cursor prompt before cols)])
    (test-equal "wrapped: cursor row agrees with cursor-visual-row"
      er (vterm-cursor-row scr))
    (test-equal "wrapped: cursor col agrees with the render formula"
      ec (vterm-cursor-col scr))
    ;; The wrap genuinely descended below the first row, so the row assertion is
    ;; not a vacuous "row 0 equals row 0".
    (test-assert "wrapped: the cursor is below the first row (a wrap happened)"
      (> (vterm-cursor-row scr) 0))))

;; (c2) Wrapped-column regression: a line that overruns a narrow terminal must
;; leave the cursor at the WRAPPED column -- its position within the final
;; wrapped row -- not at the wrap-blind width-since-the-newline that overshoots
;; the terminal's right edge.  Thirty glyphs after a two-column prompt at width
;; 20 wrap once, so the cursor rests twelve columns into the second row, well
;; inside the terminal.  The concrete 12 (not the pre-fix overrun of 32) gives the
;; assertion teeth: it fails on a renderer that emits the absolute column.
(let* ([prompt "> "] [before (make-string 30 #\a)] [cols 20]
       [scr (screen-of prompt before cols)])
  (let-values ([(er ec) (expected-cursor prompt before cols)])
    (test-equal "wrapped column: cursor row agrees with the geometry oracle"
      er (vterm-cursor-row scr))
    (test-equal "wrapped column: cursor col agrees with the geometry oracle"
      ec (vterm-cursor-col scr)))
  (test-equal "wrapped column: the cursor rests at the wrapped column (12), not the overrun"
    12 (vterm-cursor-col scr))
  (test-assert "wrapped column: the wrapped cursor stays within the terminal width"
    (<= (vterm-cursor-col scr) cols))
  (test-assert "wrapped column: the cursor descended below the first row (a wrap happened)"
    (> (vterm-cursor-row scr) 0)))

;; (d) Ghost suggestion: render-line/suggestion draws a multi-line ghost past the
;; cursor, then climbs back over its rows.  The cursor must land on the USER's
;; logical typing row (row 0), never stranded on the suggestion's bottom row --
;; the property the whole ghost row-counting machinery exists to guarantee.
(let* ([prompt "> "] [before "abc"] [suggestion "X\nY\nZ"] [cols 80]
       [scr (render->screen cols
              (lambda (p)
                (render-line/suggestion
                  p prompt (buffer-from before) 0 suggestion cols)))])
  (let-values ([(er ec) (expected-cursor prompt before cols)])
    (test-equal "ghost: cursor row agrees with cursor-visual-row"
      er (vterm-cursor-row scr))
    (test-equal "ghost: cursor col agrees with the render formula"
      ec (vterm-cursor-col scr))
    ;; The suggestion added rows below (its bottom row is 2); the cursor must
    ;; rest on the logical typing row 0, not down on the ghost's last row.
    (test-equal "ghost: cursor is on the logical typing row, not the ghost's bottom row"
      0 (vterm-cursor-row scr))
    (test-assert "ghost: the suggestion genuinely added rows below the cursor"
      (> (vterm-rows scr) 1))))

;; ======================================================================
;; (2) Grid-text fidelity and row count -- the folded grid shows what a real
;; terminal of that width would show: the prompt plus the buffer glyphs on the
;; prompt row, a fresh grid row on an embedded newline, and a wrap at the column
;; boundary whose total row count matches count-visual-lines.  Every fixture is
;; driven through the REAL render-line (no hand-built escape strings), so this
;; proves agreement with the renderer rather than with a restatement of it.
;; ======================================================================

;; (1) Single line: row 0 is the prompt followed by the buffer glyphs.  SGR is
;; stripped from the cell model, so the row TEXT is plain glyphs even though the
;; colourised render carries colour attributes on those very cells.
(let* ([prompt "> "] [before "(+ 1 2)"] [cols 80]
       [scr (screen-of prompt before cols)])
  (test-equal "grid single-line: row 0 is the prompt plus the buffer glyphs"
    "> (+ 1 2)" (vterm-row-text scr 0))
  (test-equal "grid single-line: a single grid row" 1 (vterm-rows scr)))

;; (2) Embedded newline: the newline starts a new physical grid row with the
;; column reset to 0 -- "a" rests on the prompt row and "b" on row 1.
(let* ([prompt "> "] [before "a\nb"] [cols 80]
       [scr (screen-of prompt before cols)])
  (test-equal "grid newline: the prompt row holds the pre-newline glyph"
    "> a" (vterm-row-text scr 0))
  (test-equal "grid newline: the post-newline glyph starts a new row"
    "b" (vterm-row-text scr 1))
  (test-equal "grid newline: two grid rows" 2 (vterm-rows scr)))

;; (3) Wrap at the column boundary: a single logical line wider than a narrow
;; terminal spills the overflowing glyph onto the next grid row, and the total
;; row count equals 1 + count-visual-lines for that prompt/text/width.  Row-text
;; equalities pin the split exactly where the oracle puts it: six glyphs fit
;; after the two-column prompt, then "g" begins the next row.
(let* ([prompt "> "] [before "abcdefghijklmno"] [cols 8]  ; 15 glyphs, width 8
       [scr (screen-of prompt before cols)])
  (test-equal "grid wrap: total rows equal 1 + count-visual-lines"
    (+ 1 (count-visual-lines (ansi-display-width prompt) before cols))
    (vterm-rows scr))
  (test-equal "grid wrap: row 0 is the prompt plus the glyphs that fit before the boundary"
    "> abcdef" (vterm-row-text scr 0))
  (test-equal "grid wrap: the overflow continues on the next row"
    "ghijklmn" (vterm-row-text scr 1))
  (test-equal "grid wrap: the boundary glyph starts row 1"
    #\g (cell-glyph (vterm-cell scr 1 0))))

;; ======================================================================
;; (3) The ghost drawn INSIDE the auto-inserted closers -- the case an
;; end-of-buffer cursor cannot reach.  Every fixture above seats the cursor at the
;; end of the buffer, so the after-cursor string is empty and the ghost has
;; nothing to be drawn in front of.  But that is not the shape the editor hands
;; the renderer: type "(" and paredit leaves the buffer "()" with the cursor
;; parked BETWEEN the parens, so a real closer sits past the cursor while the
;; suggestion expands at it.  The ghost must therefore be drawn AT the cursor,
;; ahead of that closer -- and the closer the ghost would duplicate must be
;; elided, or the line shows it twice.  These fixtures pin both, per-cell: the
;; ghost's glyphs read the dim grey, the buffer's closer reads its own paren
;; colour, and the cursor stays on the user's typing row.
;; ======================================================================

;; The elision rule, pure.  The ghost is drawn minus the trailing run it shares
;; with the after-cursor text, because the buffer is already holding those
;; characters -- they are on the screen once already.
(test-equal "elision: the closer the buffer already holds is dropped from the ghost"
  "+ 1 2" (ghost-visible-suffix "+ 1 2)" ")"))
(test-equal "elision: a ghost that is nothing but the shared closer draws nothing"
  "" (ghost-visible-suffix ")" ")"))
;; Nested: only ONE closer is genuinely shared -- the two-character tails "1)" and
;; "))" already differ -- so exactly one is elided and the ghost keeps the other.
;; The extra closer it then shows on screen is ACCEPTED: it is the honest
;; continuation of the matched entry, and eliding further would mean guessing
;; which of the buffer's closers the ghost's own closers correspond to.
(test-equal "elision: only the trailing run the two genuinely share is dropped"
  "lambda (x) x) 1" (ghost-visible-suffix "lambda (x) x) 1)" "))"))
(test-equal "elision: with nothing after the cursor the ghost is drawn whole"
  "-world" (ghost-visible-suffix "-world" ""))

;; (a) The paredit fixture: buffer "()" with the cursor between the parens (what
;; typing "(" leaves), ghost "+ 1 2)" from the history entry "(+ 1 2)".  The row
;; must read "> (+ 1 2)" -- the grey suggestion INSIDE the user's real closer.
;; The closer stays real: it keeps the renderer's depth-0 paren colour, so the
;; screen distinguishes what is typed from what is merely suggested.
(let* ([prompt "> "] [cols 80]
       [scr (render->screen cols
              (lambda (p)
                (render-line/suggestion
                  p prompt (buffer-cursor-at "()" 1) 0 "+ 1 2)" cols)))])
  (test-equal "ghost in parens: the row reads the expansion, the closer last"
    "> (+ 1 2)" (vterm-row-text scr 0))
  (test-equal "ghost in parens: a single grid row" 1 (vterm-rows scr))
  ;; The ghost's own glyphs, "+" at column 3 through "2" at column 7, are grey.
  (test-equal "ghost in parens: the ghost's first glyph is +"
    #\+ (cell-glyph (vterm-cell scr 0 3)))
  (test-equal "ghost in parens: the ghost's first glyph is dim grey"
    ghost-grey (cell-fg (vterm-cell scr 0 3)))
  (test-equal "ghost in parens: the ghost's last glyph is 2"
    #\2 (cell-glyph (vterm-cell scr 0 7)))
  (test-equal "ghost in parens: the ghost's last glyph is dim grey"
    ghost-grey (cell-fg (vterm-cell scr 0 7)))
  ;; The closer past the cursor is REAL buffer text, not ghost.
  (test-equal "ghost in parens: the trailing closer is the buffer's own )"
    #\) (cell-glyph (vterm-cell scr 0 8)))
  (test-equal "ghost in parens: the closer keeps the depth-0 paren colour"
    (vector-ref paren-colours 0) (cell-fg (vterm-cell scr 0 8)))
  (test-assert "ghost in parens: the closer is not painted as ghost grey"
    (not (eqv? (cell-fg (vterm-cell scr 0 8)) ghost-grey)))
  ;; The cursor rests where the user is typing: just past the "(" it typed.
  (test-equal "ghost in parens: the cursor is on the logical typing row"
    0 (vterm-cursor-row scr))
  (test-equal "ghost in parens: the cursor rests just past the open paren"
    3 (vterm-cursor-col scr)))

;; (b) The fully-elided fixture: type on to "(+ 1 2" and the buffer is "(+ 1 2)"
;; with the cursor before the auto-inserted closer, so the entry's remaining
;; suffix is exactly the ")" already on the screen.  Drawing it would double the
;; closer ("> (+ 1 2))"), so nothing is drawn -- the row is the buffer alone and
;; no cell carries the ghost's grey.
(let* ([prompt "> "] [cols 80]
       [scr (render->screen cols
              (lambda (p)
                (render-line/suggestion
                  p prompt (buffer-cursor-at "(+ 1 2)" 6) 0 ")" cols)))])
  (test-equal "fully-elided ghost: the row shows one closer, not two"
    "> (+ 1 2)" (vterm-row-text scr 0))
  (test-equal "fully-elided ghost: a single grid row" 1 (vterm-rows scr))
  (test-assert "fully-elided ghost: no cell is painted in the ghost's grey"
    (not (row-has-ghost-grey? scr 0)))
  (test-equal "fully-elided ghost: the cursor still rests before the closer"
    8 (vterm-cursor-col scr)))

;; (c) The end-of-buffer regression: with nothing past the cursor there is
;; nothing to elide and nothing to draw around, so the ghost lands straight after
;; the buffer in grey, exactly as it always did.  The buffer's own glyphs stay
;; ungreyed, so the grey assertions above are not vacuous.
(let* ([prompt "> "] [cols 80]
       [scr (render->screen cols
              (lambda (p)
                (render-line/suggestion
                  p prompt (buffer-cursor-at "abc" 3) 0 "-world" cols)))])
  (test-equal "end-of-buffer ghost: the ghost trails the buffer"
    "> abc-world" (vterm-row-text scr 0))
  (test-equal "end-of-buffer ghost: the ghost's first glyph is dim grey"
    ghost-grey (cell-fg (vterm-cell scr 0 5)))
  (test-equal "end-of-buffer ghost: the ghost's last glyph is dim grey"
    ghost-grey (cell-fg (vterm-cell scr 0 10)))
  (test-assert "end-of-buffer ghost: the buffer's own glyphs are not grey"
    (not (eqv? (cell-fg (vterm-cell scr 0 2)) ghost-grey))))

(test-end)
