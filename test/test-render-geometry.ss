;;; test/test-render-geometry.ss -- Geometry of the ghost-suggestion renderer.
;;; Proves render-line/suggestion counts the ghost suggestion's own screen rows,
;;; so the terminal cursor is left on the user's logical typing row rather than
;;; stranded on the suggestion's bottom row.  Three layers:
;;;   (a) the pure wrap-aware helpers (count-visual-lines / cursor-visual-row /
;;;       ansi-display-width), asserted PTY-free and deterministically;
;;;   (b) a plain string-port sink, which emits zero escape bytes, draws no
;;;       ghost, and returns the LOGICAL cursor row (not the drawn total);
;;;   (c) the real ghost frame folded through the (test vterm) virtual terminal:
;;;       render->screen forces the capability verdict on so the renderer emits
;;;       its full escape stream, and the cursor read back from the folded grid
;;;       rests on the logical typing row, above the ghost's own drawn rows.
;;; Entirely PTY-free -- it opens no terminal and needs no platform gate, so it
;;; runs identically on every platform.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor render)
              count-visual-lines cursor-visual-row cursor-visual-row+col
              ansi-display-width
              render-line/suggestion)
        (hafod editor gap-buffer)
        (only (hafod environment) setenv)
        (chezscheme))

;; ======================================================================
;; Helpers
;; ======================================================================

;; Build a gap buffer holding text with the cursor left at end-of-buffer
;; (gap-buffer-set-from-string! leaves the gap-start at the string length, so
;; the whole text is "before" and "after" is empty -- the ghost-drawing case).
(define (buffer-from text)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb text)
    gb))

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

(test-begin "render-geometry")

;; Pitfall pin: the plain-string-port render asserts in section (b) gate on the
;; live colour-ok?, which now reads CLICOLOR_FORCE; an ambient CLICOLOR_FORCE=1
;; would force colour on the non-tty sink and flip the zero-escape asserts.  Pin
;; it unset for the whole suite.
(setenv "CLICOLOR_FORCE" #f)

;; ======================================================================
;; (a) Pure geometry -- PTY-free, deterministic.
;; ======================================================================

;; ansi-display-width measures the prompt, stripping any SGR it carries.
(test-equal "ansi-display-width plain prompt" 2 (ansi-display-width "> "))
(test-equal "ansi-display-width ignores a CSI colour run" 2
  (ansi-display-width "\x1b;[31m> \x1b;[0m"))
(test-equal "ansi-display-width of the empty string" 0 (ansi-display-width ""))

;; count-visual-lines: a line that fits is zero extra rows; each embedded
;; newline is one extra row; a narrow width wraps into extra rows.
(test-equal "count-visual-lines: single line fits (no extra rows)" 0
  (count-visual-lines 2 "abc" 80))
(test-equal "count-visual-lines: one embedded newline is one extra row" 1
  (count-visual-lines 2 "a\nb" 80))
(test-equal "count-visual-lines: two embedded newlines are two extra rows" 2
  (count-visual-lines 2 "a\nb\nc" 80))
(test-equal "count-visual-lines: wraps at a narrow terminal width" 1
  (count-visual-lines 0 "abcdef" 3))

;; cursor-visual-row: the row of the cursor within the text before it.
(test-equal "cursor-visual-row: cursor before the newline is row 0" 0
  (cursor-visual-row 2 "ab" 80))
(test-equal "cursor-visual-row: cursor after the newline is row 1" 1
  (cursor-visual-row 2 "ab\ncd" 80))

;; cursor-visual-row+col: one wrap-aware walk yields BOTH the visual row and the
;; 0-based column WITHIN the final visual row.  On a line that overruns the
;; terminal width the column is the wrapped position, not the wrap-blind width
;; since the last newline; the common non-wrapping case is unchanged.
(test-equal "cursor-visual-row+col: a fitting line is (row 0 . column past the prompt)"
  '(0 . 5) (cursor-visual-row+col 2 "abc" 80))
(test-equal "cursor-visual-row+col: a line wider than the terminal reports the wrapped column"
  '(1 . 12) (cursor-visual-row+col 2 (make-string 30 #\a) 20))
(test-assert "cursor-visual-row+col: the wrapped column stays within the terminal width"
  (<= (cdr (cursor-visual-row+col 2 (make-string 30 #\a) 20)) 20))
;; Edge cases the walk must handle without a second traversal: an unknown width
;; (term-cols <= 0) falls back to the wrap-blind column, an empty buffer rests at
;; the prompt, a glyph that exactly fills the final column sits at a pending wrap
;; (column = width), and the next glyph past the boundary wraps to a new row.
(test-equal "cursor-visual-row+col: unknown width falls back to the wrap-blind column"
  '(0 . 5) (cursor-visual-row+col 2 "abc" 0))
(test-equal "cursor-visual-row+col: unknown width past a newline drops the prompt"
  '(1 . 2) (cursor-visual-row+col 2 "ab\ncd" 0))
(test-equal "cursor-visual-row+col: an empty buffer rests at the prompt column"
  '(0 . 2) (cursor-visual-row+col 2 "" 20))
(test-equal "cursor-visual-row+col: a glyph filling the last column is a pending wrap"
  '(0 . 5) (cursor-visual-row+col 0 "abcde" 5))
(test-equal "cursor-visual-row+col: one glyph past the boundary wraps to the next row"
  '(1 . 1) (cursor-visual-row+col 0 "abcdef" 5))

;; The KEY ghost arithmetic: measuring the buffer alone vs. the buffer with the
;; suggestion appended.  Worked case P=2, T="abc" (cursor at end), S="X\nY\nZ",
;; W=80 -> buffer-only 0 rows, ghost-inclusive 2 rows, so the renderer must
;; climb (2 - 0) = 2 rows back to the typing row.
(test-equal "ghost arithmetic: buffer-only rows (no ghost counted)" 0
  (count-visual-lines 2 "abc" 80))
(test-equal "ghost arithmetic: buffer + suggestion rows (ghost counted)" 2
  (count-visual-lines 2 (string-append "abc" "X\nY\nZ") 80))
(test-equal "ghost arithmetic: climb = drawn rows - cursor row" 2
  (- (count-visual-lines 2 (string-append "abc" "X\nY\nZ") 80)
     (cursor-visual-row 2 "abc" 80)))

;; ======================================================================
;; (b) Plain string-port sink -- zero escapes, no ghost, logical row returned.
;; A string port is a non-tty, so ansi-ok?/colour-ok? are both #f: no motion
;; escapes and no ghost are emitted, and the returned prev-lines is the logical
;; cursor row, never the ghost-inclusive drawn total.
;; ======================================================================

(let* ([sp (open-output-string)]
       [gb (buffer-from "(+ 1 2)")]
       [row (render-line/suggestion sp "> " gb 0 "X\nY\nZ" 80)]
       [out (get-output-string sp)])
  (test-assert "plain sink: render-line/suggestion emits zero ESC bytes"
    (not (has-esc? out)))
  (test-assert "plain sink: the buffer text is still drawn"
    (contains-substring? out "(+ 1 2)"))
  (test-assert "plain sink: the ghost suggestion is not drawn"
    (not (contains-substring? out "X\nY\nZ")))
  (test-equal "plain sink: returns the logical cursor row (0 for a single line)"
    0 row))

;; A multi-line buffer confirms the return value is the LOGICAL cursor row (1),
;; not the ghost-inclusive drawn total (which would be 3 here) -- so the value
;; threaded as prev-lines tracks where the cursor is physically left.
(let* ([sp (open-output-string)]
       [gb (buffer-from "a\nb")]
       [row (render-line/suggestion sp "> " gb 0 "X\nY\nZ" 80)])
  (test-equal "plain sink: multi-line buffer returns the logical row, not the drawn total"
    1 row))

;; ======================================================================
;; (c) The real ghost frame folded through the virtual terminal -- the cursor
;; climbs back over the ghost's own rows onto the user's logical typing row.
;; render->screen forces the capability verdict on, so render-line/suggestion
;; emits its full escape stream (the ghost draw plus the cursor-up climb) to a
;; string port; the harness folds that climb into vterm-cursor-row.  The derived
;; cursor must rest on the logical typing row (cursor-visual-row of the buffer
;; alone), never stranded on the suggestion's bottom row, and the grid must have
;; genuinely grown extra rows below it, so the assertion is not vacuous.
;; ======================================================================

(let* ([prompt "> "]
       [before "(a\n(b"]               ; multi-line buffer, cursor left at end
       [suggestion "X\nY\nZ"]          ; multi-line ghost suggestion
       [cols 80]
       [typing-row (cursor-visual-row (ansi-display-width prompt) before cols)]
       [scr (render->screen cols
              (lambda (p)
                (render-line/suggestion
                  p prompt (buffer-from before) 0 suggestion cols)))])
  ;; The derived cursor -- folded from the renderer's real climb -- rests on the
  ;; logical typing row the geometry oracle predicts, not the ghost's bottom row.
  (test-equal "ghost: cursor climbs back to the logical typing row (oracle-agreed)"
    typing-row (vterm-cursor-row scr))
  (test-equal "ghost: the logical typing row is row 1 for the two-line buffer"
    1 (vterm-cursor-row scr))
  ;; The suggestion genuinely added rows BELOW the typing row (row 1), so the
  ;; total drawn rows must exceed typing-row + 1.  This fails -- with teeth -- if
  ;; the ghost drew nothing (rows would then equal the 2-line buffer's 2).
  (test-assert "ghost: the suggestion genuinely added rows below the typing row"
    (> (vterm-rows scr) (+ 1 (vterm-cursor-row scr)))))

(test-end)
