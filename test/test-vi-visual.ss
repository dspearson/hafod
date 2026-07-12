;;; test/test-vi-visual.ss -- Coverage for vi visual-mode state, the visual
;;; selection range mapping, and the line-relative operators (dd/cc/yy) and line
;;; motions (0/^/$/gg/G, visual-line V) on multi-line buffers. Everything is
;;; driven through vi-process-key with bounded string ports, so the suite needs
;;; no terminal and can never block.
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

;; As press!, but the follow-up key(s) a two-key command reads (gg's second g,
;; for instance) are supplied through the bounded input port REST.
(define (press-with! es gb kr ch rest)
  (vi-process-key es (make-key-event 'char ch 0) (open-input-string rest) (open-output-string) gb kr))

;; Feed each character of KEYS through the vi state machine in order, each on an
;; empty follow-up port -- none of the operator+motion sequences below read a
;; follow-up key. Used to drive multi-key edits such as 2d3w and cw.
(define (press-keys! es gb kr keys)
  (for-each (lambda (ch) (press! es gb kr ch)) (string->list keys)))

;; Count the occurrences of CH in S (used to assert a counted paste's total).
(define (char-count ch s)
  (let lp ([i 0] [n 0])
    (cond [(= i (string-length s)) n]
          [(char=? (string-ref s i) ch) (lp (+ i 1) (+ n 1))]
          [else (lp (+ i 1) n)])))

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

;; ========================================================================
;; Line-relative operators and motions on genuinely multi-line buffers
;; ========================================================================
;; The buffers below hold three logical lines separated by embedded newlines,
;; e.g. "a\nb\nc" -> indices 0:a 1:\n 2:b 3:\n 4:c. The line operators and line
;; motions must act on the CURRENT logical line only, never the whole buffer.

;; --- dd on the middle line removes only that line -----------------------
;; Point on line two ('b', index 2). A single dd takes line two and its
;; trailing newline; lines one and three survive.
(let ()
  (define gb (buffer-with-cursor "a\nb\nc" 2))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press! es gb kr #\d)
  (test-equal "dd on the middle line removes only that line"
    "a\nc" (gap-buffer->string gb)))

;; --- dd on the last line takes the preceding newline --------------------
;; Point on line three ('c', index 4). dd on the final line removes the
;; PRECEDING newline, so no dangling blank line is left behind.
(let ()
  (define gb (buffer-with-cursor "a\nb\nc" 4))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press! es gb kr #\d)
  (test-equal "dd on the last line leaves no trailing blank line"
    "a\nb" (gap-buffer->string gb)))

;; --- dd on a single-line buffer empties it ------------------------------
;; With no newline to remove, dd empties the line and leaves an empty buffer
;; (length zero) rather than crashing.
(let ()
  (define gb (buffer-with-cursor "hello" 2))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press! es gb kr #\d)
  (test-equal "dd on a single-line buffer empties it"
    "" (gap-buffer->string gb)))

;; --- cc clears only the current line and enters insert ------------------
;; cc on line two clears just that line's text -- the newline stays, so the
;; line survives as an empty line -- and switches to insert mode.
(let ()
  (define gb (buffer-with-cursor "a\nb\nc" 2))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\c)
  (press! es gb kr #\c)
  (test-equal "cc clears only the current line's text, keeping the line"
    "a\n\nc" (gap-buffer->string gb))
  (test-equal "cc enters insert mode"
    'insert (editor-state-mode es)))

;; --- yy then p round-trips only the current line ------------------------
;; yy on line two copies just that line's text into the unnamed register; a
;; following p re-inserts only that line, leaving lines one and three intact.
(let ()
  (define gb (buffer-with-cursor "a\nb\nc" 2))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\y)
  (press! es gb kr #\y)
  (test-equal "yy copies only the current line into the unnamed register"
    "b" (vi-reg-fetch #\"))
  (cmd-paste-after es)
  (test-equal "yy then p re-inserts only the current line"
    "a\nbb\nc" (gap-buffer->string gb)))

;; --- 0 / ^ / $ stay within the current line -----------------------------
;; "ax\n  by\ncz": line two is "  by" spanning indices 3..6 (two leading
;; blanks). With point on 'b' (index 5): 0 -> line start (3), ^ -> first
;; non-blank (5), $ -> last column (6). None crosses a newline.
(let ()
  (define gb (buffer-with-cursor "ax\n  by\ncz" 5))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\0)
  (test-equal "0 moves to the current line's first column"
    3 (gap-buffer-cursor-pos gb)))
(let ()
  (define gb (buffer-with-cursor "ax\n  by\ncz" 5))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\^)
  (test-equal "^ moves to the current line's first non-blank"
    5 (gap-buffer-cursor-pos gb)))
(let ()
  (define gb (buffer-with-cursor "ax\n  by\ncz" 5))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\$)
  (test-equal "$ moves to the current line's last column"
    6 (gap-buffer-cursor-pos gb)))

;; --- gg / G land on the first / last line -------------------------------
;; On the same three-line buffer, gg reaches the first non-blank of the first
;; line (index 0) and G the first non-blank of the last line ('c', index 8) --
;; line-aware, not raw buffer offset 0 or buffer length.
(let ()
  (define gb (buffer-with-cursor "ax\n  by\ncz" 5))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press-with! es gb kr #\g "g")
  (test-equal "gg lands on the first line"
    0 (gap-buffer-cursor-pos gb)))
(let ()
  (define gb (buffer-with-cursor "ax\n  by\ncz" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\G)
  (test-equal "G lands on the last line"
    8 (gap-buffer-cursor-pos gb)))

;; --- Vd deletes only the selected line ----------------------------------
;; Visual-line V selects the whole logical line under the cursor; d then
;; deletes exactly that line (line two), leaving lines one and three.
(let ()
  (define gb (buffer-with-cursor "a\nb\nc" 2))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\V)
  (press! es gb kr #\d)
  (test-equal "Vd deletes exactly the selected line"
    "a\nc" (gap-buffer->string gb)))

;; ========================================================================
;; Inner text objects resolved from INSIDE the delimiters
;; ========================================================================
;; di"/ci"/yi" (and the ' and ` kinds) must act with the cursor anywhere
;; between the delimiters, not only when it sits on the opening one. The
;; buffer 'foo "bar baz" qux' puts the opening quote at index 4 and the
;; closing quote at index 12, so the inner span is indices 5..11 and index 6
;; is the 'a' of "bar" -- strictly inside the quotes.

;; --- di" from inside deletes the inner text -----------------------------
(let ()
  (define gb (buffer-with-cursor "foo \"bar baz\" qux" 6))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-with! es gb kr #\i "\"")
  (test-equal "di\" from inside the quotes deletes the inner text"
    "foo \"\" qux" (gap-buffer->string gb)))

;; --- ci" from inside clears the inner text and enters insert ------------
(let ()
  (define gb (buffer-with-cursor "foo \"bar baz\" qux" 6))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\c)
  (press-with! es gb kr #\i "\"")
  (test-equal "ci\" from inside the quotes clears the inner text"
    "foo \"\" qux" (gap-buffer->string gb))
  (test-equal "ci\" from inside the quotes enters insert mode"
    'insert (editor-state-mode es)))

;; --- yi" from inside yanks the inner text, buffer intact ----------------
(let ()
  (define gb (buffer-with-cursor "foo \"bar baz\" qux" 6))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\y)
  (press-with! es gb kr #\i "\"")
  (test-equal "yi\" from inside yanks the inner text to the unnamed register"
    "bar baz" (vi-reg-fetch #\"))
  (test-equal "yi\" leaves the buffer unchanged"
    "foo \"bar baz\" qux" (gap-buffer->string gb)))

;; --- di' from inside: the single-quote kind -----------------------------
(let ()
  (define gb (buffer-with-cursor "foo 'bar baz' qux" 6))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-with! es gb kr #\i "'")
  (test-equal "di' from inside the single quotes deletes the inner text"
    "foo '' qux" (gap-buffer->string gb)))

;; --- di` from inside: the backtick kind ---------------------------------
(let ()
  (define gb (buffer-with-cursor "foo `bar baz` qux" 6))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-with! es gb kr #\i "`")
  (test-equal "di` from inside the backticks deletes the inner text"
    "foo `` qux" (gap-buffer->string gb)))

;; --- An internal ESCAPED quote does not terminate the span --------------
;; Buffer 'foo "a\"b" qux': the opening quote is index 4, the escaped quote
;; is index 7 (preceded by the backslash at 6) and the real closer is index
;; 9. With point on 'b' (index 8) the escaped quote must be skipped in both
;; directions, so di" deletes the whole inner a\"b and leaves 'foo "" qux'.
(let ()
  (define gb (buffer-with-cursor "foo \"a\\\"b\" qux" 8))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-with! es gb kr #\i "\"")
  (test-equal "di\" skips an internal escaped quote and deletes the whole span"
    "foo \"\" qux" (gap-buffer->string gb)))

;; --- Cursor outside any pair is a safe no-op ----------------------------
;; Point on 'q' of qux (index 10), past the closed "bar": there is an opener
;; before it but no closer after, so the text object is a no-op -- the buffer
;; is untouched and nothing is deleted.
(let ()
  (define gb (buffer-with-cursor "foo \"bar\" qux" 10))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-with! es gb kr #\i "\"")
  (test-equal "di\" with the cursor outside any pair leaves the buffer unchanged"
    "foo \"bar\" qux" (gap-buffer->string gb)))

;; --- The on-opening-quote case still resolves ---------------------------
;; With point ON the opening quote (index 4) di" still deletes the inner
;; text, so the inside-aware walk does not regress the original behaviour.
(let ()
  (define gb (buffer-with-cursor "foo \"bar\" qux" 4))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-with! es gb kr #\i "\"")
  (test-equal "di\" with point on the opening quote still deletes the inner text"
    "foo \"\" qux" (gap-buffer->string gb)))

;; ========================================================================
;; Numeric counts: keymap dispatch, operator×motion multiply, cw→ce, w-clamp
;; ========================================================================
;; These behaviours split across two dispatch paths. A count in front of a
;; keymap-dispatched command (3x / 5p / 4a) is surfaced by vi-process-key and
;; consumed by the editor's keymap dispatch, so those cases run through the real
;; editor drive harness (editor-drive-keys!). The operator+motion behaviours --
;; the count multiply, the cw->ce special case and the w-motion newline clamp --
;; live wholly inside vi-process-key and are driven straight through press-keys!.

;; --- 3x deletes three characters ----------------------------------------
;; The count reaches the keymap-dispatched x: three characters go, not one.
(let ()
  (define gb (buffer-with-cursor "abcdef" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (editor-drive-keys! es (list "3x"))
  (test-equal "3x deletes three characters through the keymap dispatch"
    "def" (gap-buffer->string gb)))

;; --- a keymap command with no count runs exactly once -------------------
;; The count-absent path is unchanged: a bare x deletes a single character.
(let ()
  (define gb (buffer-with-cursor "abcdef" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (editor-drive-keys! es (list "x"))
  (test-equal "x with no count deletes exactly one character"
    "bcdef" (gap-buffer->string gb)))

;; --- 5p pastes the register five times ----------------------------------
;; With "X" on the kill ring, 5p inserts it five times; asserting the count of
;; X's is robust to the paste's cursor interleaving. A bare p inserts one.
(let ()
  (define gb (buffer-with-cursor "ab" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (kill-ring-push! kr "X")
  (editor-drive-keys! es (list "5p"))
  (test-equal "5p pastes the yanked text five times"
    5 (char-count #\X (gap-buffer->string gb))))
(let ()
  (define gb (buffer-with-cursor "ab" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (kill-ring-push! kr "X")
  (editor-drive-keys! es (list "p"))
  (test-equal "p with no count pastes exactly once"
    1 (char-count #\X (gap-buffer->string gb))))

;; --- 4a applies the count to the append ---------------------------------
;; A counted keymap command runs the bound command N times, so 4a runs the
;; append-entry four times: the cursor advances four columns and the editor ends
;; in insert mode. Without the surfaced count the append runs once (column 1).
(let ()
  (define gb (buffer-with-cursor "abcdef" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (editor-drive-keys! es (list "4a"))
  (test-equal "4a runs the append four times, advancing the cursor"
    4 (gap-buffer-cursor-pos gb))
  (test-equal "4a leaves the editor in insert mode"
    'insert (editor-state-mode es)))

;; --- 2d3w / 3d2w delete six words (operator count × motion count) -------
;; The seven-word line "one two three four five six seven": from its start,
;; deleting six words leaves only "seven". Both 2d3w and 3d2w multiply to six;
;; the pre-fix digit-concatenation (2 then 3 -> 23) would run off the end and
;; empty the buffer.
(let ()
  (define gb (buffer-with-cursor "one two three four five six seven" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press-keys! es gb kr "2d3w")
  (test-equal "2d3w deletes six words (two by three), not twenty-three"
    "seven" (gap-buffer->string gb)))
(let ()
  (define gb (buffer-with-cursor "one two three four five six seven" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press-keys! es gb kr "3d2w")
  (test-equal "3d2w also deletes six words"
    "seven" (gap-buffer->string gb)))

;; --- d2w / 2dw delete two words (a single count on either side) ---------
(let ()
  (define gb (buffer-with-cursor "one two three four five six seven" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press-keys! es gb kr "d2w")
  (test-equal "d2w deletes two words"
    "three four five six seven" (gap-buffer->string gb)))
(let ()
  (define gb (buffer-with-cursor "one two three four five six seven" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press-keys! es gb kr "2dw")
  (test-equal "2dw deletes two words"
    "three four five six seven" (gap-buffer->string gb)))

;; --- cw changes to the word end (like ce); dw deletes to the next word --
;; On "foo bar" from the 'f': cw changes just "foo" and keeps the trailing
;; space (vim's cw->ce special case), so the buffer becomes " bar" and the
;; editor enters insert; dw instead deletes "foo " through to the next word.
(let ()
  (define gb (buffer-with-cursor "foo bar" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press-keys! es gb kr "cw")
  (test-equal "cw changes to the word end and keeps the trailing space"
    " bar" (gap-buffer->string gb))
  (test-equal "cw enters insert mode"
    'insert (editor-state-mode es)))
(let ()
  (define gb (buffer-with-cursor "foo bar" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press-keys! es gb kr "dw")
  (test-equal "dw deletes through to the start of the next word"
    "bar" (gap-buffer->string gb)))
;; ce matches cw: the same change to the word end, confirming the substitution.
(let ()
  (define gb (buffer-with-cursor "foo bar" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press-keys! es gb kr "ce")
  (test-equal "ce changes to the word end, matching cw"
    " bar" (gap-buffer->string gb)))

;; --- The w-motion operator does not cross the line's trailing newline ---
;; On "foo\nbar" with point on the last (only) word of line one, dw stops at the
;; line end: the newline and the whole second line survive. Pre-fix, motion-w's
;; whitespace skip crossed the newline and dw ate it, joining "bar" up. cw on
;; the same word likewise stops at the line end.
(let ()
  (define gb (buffer-with-cursor "foo\nbar" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press-keys! es gb kr "dw")
  (test-equal "dw on the last word of a line leaves the newline and next line"
    "\nbar" (gap-buffer->string gb)))
(let ()
  (define gb (buffer-with-cursor "foo\nbar" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press-keys! es gb kr "cw")
  (test-equal "cw on the last word of a line also stops at the line end"
    "\nbar" (gap-buffer->string gb)))

;; --- A mid-line dw is unchanged by the clamp ----------------------------
;; When w stays within the line the clamp is a no-op: dw on "foo bar baz" from
;; the start still deletes "foo " through to "bar".
(let ()
  (define gb (buffer-with-cursor "foo bar baz" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press-keys! es gb kr "dw")
  (test-equal "a mid-line dw still deletes through to the next word"
    "bar baz" (gap-buffer->string gb)))

;; --- The plain w motion (no operator) is unaffected ---------------------
;; The clamp and cw->ce apply only in operator context; a bare w still steps the
;; cursor to the start of the next word.
(let ()
  (define gb (buffer-with-cursor "foo bar" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\w)
  (test-equal "a plain w motion moves the cursor to the next word start"
    4 (gap-buffer-cursor-pos gb)))

(test-end)
