;;; test/test-vterm.ss -- Parser-unit self-tests for the (test vterm) harness.
;;; Feeds hand-built escape fixtures (ESC spelled \x1b;) straight into a screen
;;; and asserts the folded grid: glyph placement and wrap, independent SGR
;;; attributes, every cursor motion, the two erases, and totality over OSC,
;;; cursor-shape, and truncated input.  Entirely PTY-free -- it opens no pty and
;;; needs no platform gate, so it runs identically on every platform.  The escape
;;; vocabulary here is exactly what (hafod editor render) emits.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor render) count-visual-lines)
        (chezscheme))

;; Fold a fixture into a fresh screen of the given width.
(define (feed cols s)
  (let ([vt (make-vterm cols)])
    (vterm-feed! vt s)
    vt))

(test-begin "vterm")

;; ----------------------------------------------------------------------
;; (1) Plain baseline -- glyphs land, and no attribute appears without an
;; escape (checked on real, populated cells so the assertion is non-vacuous).
;; ----------------------------------------------------------------------
(let ([vt (feed 80 "abc")])
  (test-equal "plain: row 0 text" "abc" (vterm-row-text vt 0))
  (test-equal "plain: one row" 1 (vterm-rows vt))
  (test-equal "plain: cursor col follows the text" 3 (vterm-cursor-col vt))
  (test-equal "plain: cell 0 glyph" #\a (cell-glyph (vterm-cell vt 0 0)))
  (test-equal "plain: cell 0 fg default" 'default (cell-fg (vterm-cell vt 0 0)))
  (test-equal "plain: cell 1 fg default" 'default (cell-fg (vterm-cell vt 0 1)))
  (test-equal "plain: cell 2 fg default" 'default (cell-fg (vterm-cell vt 0 2)))
  (test-equal "plain: cell 0 bg default" 'default (cell-bg (vterm-cell vt 0 0)))
  (test-assert "plain: cell 0 not bold" (not (cell-bold? (vterm-cell vt 0 0))))
  (test-assert "plain: cell 1 not bold" (not (cell-bold? (vterm-cell vt 0 1))))
  (test-assert "plain: cell 2 not underlined"
    (not (cell-underline? (vterm-cell vt 0 2)))))

;; ----------------------------------------------------------------------
;; (2) SGR independence -- each code touches exactly one pen attribute.
;; ----------------------------------------------------------------------
;; 24-bit fg then a 39m fg-reset
(let ([vt (feed 80 "\x1b;[38;2;255;0;0mA\x1b;[39mB")])
  (test-equal "sgr: A carries truecolour fg" '(255 0 0) (cell-fg (vterm-cell vt 0 0)))
  (test-equal "sgr: 39m resets fg to default" 'default (cell-fg (vterm-cell vt 0 1))))

;; a 39m fg-reset must NOT wipe the background
(let ([vt (feed 80 "\x1b;[48;2;0;0;255mA\x1b;[39mB")])
  (test-equal "sgr: A carries truecolour bg" '(0 0 255) (cell-bg (vterm-cell vt 0 0)))
  (test-equal "sgr: background survives the fg reset" '(0 0 255)
    (cell-bg (vterm-cell vt 0 1)))
  (test-equal "sgr: B fg still default after 39m" 'default (cell-fg (vterm-cell vt 0 1))))

;; bold on, then 22m bold-off
(let ([vt (feed 80 "\x1b;[1mA\x1b;[22mB")])
  (test-assert "sgr: 1m sets bold" (cell-bold? (vterm-cell vt 0 0)))
  (test-assert "sgr: 22m clears bold" (not (cell-bold? (vterm-cell vt 0 1)))))

;; 0m clears every attribute at once
(let ([vt (feed 80 "\x1b;[1;4;38;2;1;2;3mA\x1b;[0mB")])
  (test-assert "sgr: A bold before reset" (cell-bold? (vterm-cell vt 0 0)))
  (test-assert "sgr: A underlined before reset" (cell-underline? (vterm-cell vt 0 0)))
  (test-equal "sgr: A fg before reset" '(1 2 3) (cell-fg (vterm-cell vt 0 0)))
  (test-assert "sgr: 0m clears bold" (not (cell-bold? (vterm-cell vt 0 1))))
  (test-assert "sgr: 0m clears underline" (not (cell-underline? (vterm-cell vt 0 1))))
  (test-equal "sgr: 0m clears fg" 'default (cell-fg (vterm-cell vt 0 1))))

;; 256-colour fg, dim, underline, reverse -- each independent
(let ([vt (feed 80 "\x1b;[38;5;240m\x1b;[2m\x1b;[4m\x1b;[7mZ")])
  (test-equal "sgr: 256-colour fg index" 240 (cell-fg (vterm-cell vt 0 0)))
  (test-assert "sgr: 2m sets dim" (cell-dim? (vterm-cell vt 0 0)))
  (test-assert "sgr: 4m sets underline" (cell-underline? (vterm-cell vt 0 0)))
  (test-assert "sgr: 7m sets reverse" (cell-reverse? (vterm-cell vt 0 0))))

;; ----------------------------------------------------------------------
;; (3) Cursor motion -- derived from the escapes.
;; ----------------------------------------------------------------------
;; ESC[<n>G is 1-based; the grid column is n-1.
(let ([vt (feed 80 "\x1b;[5G")])
  (test-equal "motion: 5G -> 0-based column 4" 4 (vterm-cursor-col vt)))

;; ESC[<n>A / ESC[<n>B move the row.
(let ([vt (feed 80 "a\nb\nc\x1b;[2A")])
  (test-equal "motion: 2A moves up two rows" 0 (vterm-cursor-row vt)))
(let ([vt (feed 80 "a\x1b;[1B")])
  (test-equal "motion: 1B moves down one row" 1 (vterm-cursor-row vt)))
;; A clamps at the top row.
(let ([vt (feed 80 "a\x1b;[9A")])
  (test-equal "motion: A clamps at row 0" 0 (vterm-cursor-row vt)))

;; Carriage return returns to column 0.
(let ([vt (feed 80 "abc\r")])
  (test-equal "motion: CR -> column 0" 0 (vterm-cursor-col vt)))

;; Newline advances the row and resets the column.
(let ([vt (feed 80 "a\nb")])
  (test-equal "motion: LF -> row 1" 1 (vterm-cursor-row vt))
  (test-equal "motion: LF resets the column" 1 (vterm-cursor-col vt))
  (test-equal "motion: row 1 holds the post-newline glyph" "b" (vterm-row-text vt 1)))

;; ESC[H homes the cursor to (0,0).
(let ([vt (feed 80 "abc\ndef\x1b;[H")])
  (test-equal "motion: H homes row" 0 (vterm-cursor-row vt))
  (test-equal "motion: H homes col" 0 (vterm-cursor-col vt)))

;; ----------------------------------------------------------------------
;; (4) Wrap -- geometry mirrors count-visual-lines exactly.
;; ----------------------------------------------------------------------
(let ([vt (feed 3 "abcdef")])
  (test-equal "wrap: row count matches count-visual-lines"
    (+ 1 (count-visual-lines 0 "abcdef" 3)) (vterm-rows vt))
  (test-equal "wrap: the overflowing glyph starts the next row"
    #\d (cell-glyph (vterm-cell vt 1 0)))
  (test-equal "wrap: row 0 holds the first three glyphs" "abc" (vterm-row-text vt 0))
  (test-equal "wrap: row 1 holds the wrapped glyphs" "def" (vterm-row-text vt 1)))

;; ----------------------------------------------------------------------
;; (5) Erase -- ESC[J (cursor to end-of-screen) and ESC[2J (whole grid).
;; ----------------------------------------------------------------------
;; The renderer's redraw idiom: climb, CR, clear-to-end-of-screen.
(let ([vt (feed 80 "hello\nworld\x1b;[1A\r\x1b;[J")])
  (test-equal "erase: current row blanked to end-of-screen" "" (vterm-row-text vt 0))
  (test-equal "erase: rows below the cursor are gone" 1 (vterm-rows vt)))

;; ESC[J from mid-row keeps the text before the cursor.
(let ([vt (feed 80 "abcdef\x1b;[4G\x1b;[J")])
  (test-equal "erase: keeps text before the cursor column" "abc" (vterm-row-text vt 0)))

;; ESC[2J blanks the whole grid but keeps the cursor.
(let ([vt (feed 80 "abc\x1b;[2J")])
  (test-equal "erase: 2J blanks the grid" "" (vterm-row-text vt 0))
  (test-equal "erase: 2J keeps the cursor column" 3 (vterm-cursor-col vt)))

;; ----------------------------------------------------------------------
;; (6) Totality -- the parser is total over arbitrary/unknown bytes.
;; ----------------------------------------------------------------------
;; An OSC string (here a cursor-colour set) is skipped; its payload is not glyphs.
(let ([vt (feed 80 "\x1b;]12;#51afef\x07;abc")])
  (test-equal "totality: OSC skipped, only real glyphs land" "abc"
    (vterm-row-text vt 0)))

;; An OSC terminated by ST (ESC backslash) rather than BEL.
(let ([vt (feed 80 "\x1b;]0;title\x1b;\\xy")])
  (test-equal "totality: ST-terminated OSC skipped" "xy" (vterm-row-text vt 0)))

;; A cursor-shape CSI (a space intermediate then q) is skipped.
(let ([vt (feed 80 "\x1b;[6 qX")])
  (test-equal "totality: cursor-shape CSI skipped" "X" (vterm-row-text vt 0)))

;; A private-parameter CSI (bracketed-paste enable) is skipped.
(let ([vt (feed 80 "\x1b;[?2004hY")])
  (test-equal "totality: private CSI skipped" "Y" (vterm-row-text vt 0)))

;; DECSC/DECRC save and restore the cursor.
(let ([vt (feed 80 "\x1b;7ab\x1b;8Z")])
  (test-equal "totality: DECRC restores the saved cursor column" "Zb"
    (vterm-row-text vt 0)))

;; Truncated / degenerate tails return without error or hang.
(test-assert "totality: truncated CSI tail is safe"
  (let ([vt (make-vterm 80)]) (vterm-feed! vt "\x1b;[") #t))
(test-assert "totality: lone ESC at end is safe"
  (let ([vt (make-vterm 80)]) (vterm-feed! vt "abc\x1b;") #t))
(test-assert "totality: unterminated OSC is safe"
  (let ([vt (make-vterm 80)]) (vterm-feed! vt "\x1b;]12;partial") #t))
(test-assert "totality: unknown two-byte ESC is skipped"
  (let ([vt (feed 80 "\x1b;Xhi")]) (string=? "hi" (vterm-row-text vt 0))))

(test-end)
