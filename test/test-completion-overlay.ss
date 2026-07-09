;;; test/test-completion-overlay.ss -- The reusable terminal-overlay primitive.
;;; Pins the escape geometry of the overlay draw/clear helpers and the gating of
;;; the completion grid they wrap.  The overlay clears a tracked N-row span with
;;; RELATIVE motion (cursor-up over the span, a carriage return, clear-to-end-of-
;;; screen) and never a cursor save/restore -- an absolute save cannot survive a
;;; scroll near the terminal bottom, which is what makes the old menu stack.  The
;;; layers:
;;;   (a) the anchor arithmetic the overlay is handed (rows below the edit cursor),
;;;       asserted PTY-free through the shared wrap-aware helpers;
;;;   (b) overlay-clear! geometry on a plain string port -- one cursor-up equal to
;;;       the tracked span, exactly one clear-to-end-of-screen, no save/restore,
;;;       and zero bytes when the target is non-terminal or the span is empty;
;;;   (c) overlay-draw direction on a plain string port -- a dropdown descends and
;;;       climbs back, a drop-up anchors above, both with relative motion only, and
;;;       a non-terminal target emits no motion of its own;
;;;   (d) render-completion-grid's bounded return values + zero-escape collapse on
;;;       a plain string port, and, through the virtual-terminal harness, the
;;;       per-cell truecolour selection highlight and the height-cap pager it
;;;       renders on a colour target.
;;; Entirely PTY-free: string ports and the virtual-terminal harness everywhere,
;;; so it opens no terminal and needs no platform gate, running identically on
;;; every platform.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor render)
              overlay-clear! overlay-draw
              render-completion-grid
              count-visual-lines cursor-visual-row)
        (only (hafod editor editor)
              read-expression menu-anchor-place)
        (only (hafod environment) with-env* setenv)
        (chezscheme))

;; ======================================================================
;; Helpers
;; ======================================================================

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

;; Does ANY row of the folded harness grid contain NEEDLE?  Folds vterm-lines
;; through contains-substring? so a plain-glyph row (the height-cap pager) is
;; findable in the rendered grid text rather than in a raw byte stream.
(define (any-line-has? scr needle)
  (let loop ([lines (vterm-lines scr)])
    (cond
      [(null? lines) #f]
      [(contains-substring? (car lines) needle) #t]
      [else (loop (cdr lines))])))

;; Count non-overlapping occurrences of needle in hay -- how many times a stable
;; menu marker was painted, so its growth (or lack of it) across cycles is
;; assertable.
(define (count-substring hay needle)
  (let ([hl (string-length hay)]
        [nl (string-length needle)])
    (if (= nl 0)
        0
        (let loop ([i 0] [n 0])
          (cond
            [(> (+ i nl) hl) n]
            [(string=? (substring hay i (+ i nl)) needle) (loop (+ i nl) (+ n 1))]
            [else (loop (+ i 1) n)])))))

;; Scan a byte stream for every ESC[<digits><final> escape and return the parsed
;; counts, in order.  The run between '[' and the final must be digits only and
;; the terminator must match `final`, so an SGR colour (ESC[..m), the column move
;; (ESC[<n>G) and the bare clear (ESC[J) never match.  Generalised over the final
;; byte so cursor-up (A) and cursor-down (B) share one scanner.
(define (collect-csi s final)
  (let ([len (string-length s)]
        [esc #\x1b])
    (let scan ([i 0] [acc '()])
      (cond
        [(>= i len) (reverse acc)]
        [(and (char=? (string-ref s i) esc)
              (< (+ i 1) len)
              (char=? (string-ref s (+ i 1)) #\[))
         (let digits ([j (+ i 2)] [n 0] [saw? #f])
           (cond
             [(>= j len) (reverse acc)]
             [(and (char<=? #\0 (string-ref s j)) (char<=? (string-ref s j) #\9))
              (digits (+ j 1)
                      (+ (* n 10)
                         (- (char->integer (string-ref s j)) (char->integer #\0)))
                      #t)]
             [(and saw? (char=? (string-ref s j) final))
              (scan (+ j 1) (cons n acc))]
             [else (scan (+ j 1) acc)]))]
        [else (scan (+ i 1) acc)]))))

(define (collect-cursor-ups s) (collect-csi s #\A))
(define (collect-cursor-downs s) (collect-csi s #\B))

;; Count clear-to-end-of-screen escapes: ESC [ J with no numeric parameter (the
;; overlay clear's wipe).  ESC[<n>J does not match -- the byte right after '[' must
;; be 'J'.
(define (count-clear-to-eos s)
  (let ([len (string-length s)]
        [esc #\x1b])
    (let scan ([i 0] [n 0])
      (cond
        [(>= (+ i 2) len) n]
        [(and (char=? (string-ref s i) esc)
              (char=? (string-ref s (+ i 1)) #\[)
              (char=? (string-ref s (+ i 2)) #\J))
         (scan (+ i 3) (+ n 1))]
        [else (scan (+ i 1) n)]))))

;; Does the stream carry a cursor save (ESC 7 / DECSC) or restore (ESC 8 / DECRC)?
;; These are the absolute save/restore the overlay must NEVER emit: ESC followed
;; immediately by 7 or 8.  A CSI cursor-up whose count contains 7/8 (ESC[7A) does
;; not match -- the byte after ESC there is '[', not the digit.
(define (has-save-restore? s)
  (let ([len (string-length s)]
        [esc #\x1b])
    (let scan ([i 0])
      (cond
        [(>= (+ i 1) len) #f]
        [(and (char=? (string-ref s i) esc)
              (let ([c (string-ref s (+ i 1))])
                (or (char=? c #\7) (char=? c #\8))))
         #t]
        [else (scan (+ i 1))]))))

;; A counting draw-thunk: paints no content but reports the rows it "drew", so
;; overlay-draw's geometry is isolated from any real content renderer.
(define (counting-thunk rows) (lambda () rows))

;; A known completion list: six equal-width candidates, no descriptions, built as
;; single-element entries so render-completion-grid reads empty highlight
;; positions.  At term-cols 20 the grid lays out a single column of six rows.
(define overlay-names
  '("candidate0" "candidate1" "candidate2" "candidate3" "candidate4" "candidate5"))
(define overlay-entries (map list overlay-names))
(define overlay-cols 20)
(define overlay-max-visible 4)

(test-begin "completion-overlay")

;; Pitfall pin: the render-completion-grid asserts (section (d)) and the plain-
;; sink cycle guard (section (g)) render through the live colour-ok? on a plain
;; string port and check for zero escape bytes.  colour-ok? now reads
;; CLICOLOR_FORCE, so an ambient CLICOLOR_FORCE=1 would force colour on that
;; non-tty sink and flip those asserts.  Pin it unset for the whole suite; the
;; TERM-only with-env* block in (g) restores back to this baseline.
(setenv "CLICOLOR_FORCE" #f)

;; ======================================================================
;; (a) Anchor arithmetic -- PTY-free.
;; The overlay consumer chooses a dropdown or a drop-up from the room BELOW the
;; edit cursor: lines-after = (total wrapped rows) - (cursor's own row).  Pinning
;; that arithmetic documents the anchor offset overlay-draw is handed.  Worked
;; case: prompt width 2, buffer "(a\n(b" with the cursor at end, width 80 -> one
;; extra physical row, cursor on the lower one, so zero rows remain below it (a
;; bottom anchor -- the drop-up candidate).
;; ======================================================================

(let* ([pw 2] [text "(a\n(b"] [cols 80]
       [total (count-visual-lines pw text cols)]
       [crow (cursor-visual-row pw text cols)]
       [lines-after (- total crow)])
  (test-equal "anchor: a two-line buffer has one extra wrapped row" 1 total)
  (test-equal "anchor: the cursor sits on the lower row" 1 crow)
  (test-equal "anchor: no rows remain below the cursor (a bottom anchor)"
    0 lines-after))

;; ======================================================================
;; (b) overlay-clear! geometry -- PTY-free, on a plain string port.
;; ======================================================================

(let ([sp (open-output-string)])
  (overlay-clear! sp 3 #t)
  (let ([out (get-output-string sp)])
    (test-equal "overlay-clear!: exact byte sequence (up N, CR, clear-to-EOS)"
      "\x1b;[3A\r\x1b;[J" out)
    (test-equal "overlay-clear!: the only cursor-up equals the tracked span"
      (list 3) (collect-cursor-ups out))
    (test-equal "overlay-clear!: exactly one clear-to-end-of-screen"
      1 (count-clear-to-eos out))
    (test-assert "overlay-clear!: no cursor save/restore (DECSC/DECRC)"
      (not (has-save-restore? out)))))

;; ansi? #f -> a pipe/file sink stays byte-for-byte plain.
(let ([sp (open-output-string)])
  (overlay-clear! sp 3 #f)
  (test-assert "overlay-clear!: ansi? #f emits zero ESC bytes"
    (not (has-esc? (get-output-string sp)))))

;; A zero-row span -> nothing to clear, zero bytes even with ansi? #t.
(let ([sp (open-output-string)])
  (overlay-clear! sp 0 #t)
  (test-assert "overlay-clear!: a zero-row span emits zero ESC bytes"
    (not (has-esc? (get-output-string sp)))))

;; ======================================================================
;; (c) overlay-draw direction -- PTY-free, on a plain string port.
;; ======================================================================

;; Dropdown: descend `offset` rows to the anchor, then climb back over the drawn
;; span, its single leading transition row and the descent -- relative only.
(let* ([sp (open-output-string)]
       [ret (overlay-draw sp 'down 2 #t (counting-thunk 3))]
       [out (get-output-string sp)])
  (test-equal "overlay-draw 'down: returns the draw-thunk's row count" 3 ret)
  (test-equal "overlay-draw 'down: descends to the anchor (one cursor-down = offset)"
    (list 2) (collect-cursor-downs out))
  (test-equal "overlay-draw 'down: climbs back over span + leading row + offset"
    (list (+ 2 3 1)) (collect-cursor-ups out))
  (test-assert "overlay-draw 'down: no cursor save/restore"
    (not (has-save-restore? out))))

;; Drop-up: anchor the span above the edit line with an up-move.  With the offset
;; chosen larger than the span the cursor is left above the edit row, so the
;; return is a downward move -- still relative, still no save/restore.
(let* ([sp (open-output-string)]
       [ret (overlay-draw sp 'up 5 #t (counting-thunk 2))]
       [out (get-output-string sp)])
  (test-equal "overlay-draw 'up: returns the draw-thunk's row count" 2 ret)
  (test-equal "overlay-draw 'up: anchors the span above with an up-move (= offset)"
    (list 5) (collect-cursor-ups out))
  ;; net = span + leading row - offset = 2 + 1 - 5 = -2, i.e. two rows below.
  (test-equal "overlay-draw 'up: returns to the edit row with a down-move"
    (list 2) (collect-cursor-downs out))
  (test-assert "overlay-draw 'up: no cursor save/restore"
    (not (has-save-restore? out))))

;; ansi? #f: the thunk still runs for its content and return value, but
;; overlay-draw contributes no motion escapes of its own.
(let* ([sp (open-output-string)]
       [marker "candidate-row"]
       [ret (overlay-draw sp 'down 2 #f
              (lambda () (display marker sp) 3))]
       [out (get-output-string sp)])
  (test-equal "overlay-draw: ansi? #f still returns the thunk's row count" 3 ret)
  (test-assert "overlay-draw: ansi? #f still runs the draw-thunk (content present)"
    (contains-substring? out marker))
  (test-assert "overlay-draw: ansi? #f emits no escape bytes of its own"
    (not (has-esc? out))))

;; ======================================================================
;; (d) render-completion-grid geometry + gating.
;; Six candidates, single column, max-visible 4: the list is capped to four rows
;; and a pager row is counted (menu-lines 5), and the scroll window keeps the
;; selected row in view.  On a plain string port the whole thing collapses to
;; zero escape bytes; through the virtual-terminal harness the selection highlight
;; and the pager are read back per cell on a colour target.
;; ======================================================================

;; No selection: bounded return values, zero escapes.
(let ([sp (open-output-string)])
  (let-values ([(menu-lines grid-cols grid-rows scroll-off)
                (render-completion-grid sp overlay-entries -1
                                        overlay-cols overlay-max-visible)])
    (test-equal "grid: a single column for six wide candidates at term-cols 20"
      1 grid-cols)
    (test-equal "grid: six rows for six single-column candidates" 6 grid-rows)
    (test-equal "grid: menu-lines is the capped rows plus one pager row"
      (+ overlay-max-visible 1) menu-lines)
    (test-equal "grid: no scroll while the top of the list is in view" 0 scroll-off)
    (test-assert "grid: a plain string port carries zero ESC bytes"
      (not (has-esc? (get-output-string sp))))))

;; A selection inside the first window: still no scroll, and the highlight
;; collapses to zero escapes on the non-terminal sink.
(let ([sp (open-output-string)])
  (let-values ([(menu-lines grid-cols grid-rows scroll-off)
                (render-completion-grid sp overlay-entries 2
                                        overlay-cols overlay-max-visible)])
    (test-equal "grid: an in-view selection does not scroll the window" 0 scroll-off)
    (test-assert "grid: the selection highlight collapses to zero ESC on a plain sink"
      (not (has-esc? (get-output-string sp))))))

;; A selection past the first window scrolls to keep it visible: the scroll
;; offset advances so the selected row stays inside the capped window.
(let ([sp (open-output-string)])
  (let-values ([(menu-lines grid-cols grid-rows scroll-off)
                (render-completion-grid sp overlay-entries 5
                                        overlay-cols overlay-max-visible)])
    (test-equal "grid: the last row scrolls the window to keep it in view"
      (- 5 (- overlay-max-visible 1)) scroll-off)
    (test-assert "grid: the scrolled render still carries zero ESC on a plain sink"
      (not (has-esc? (get-output-string sp))))))

;; Through the virtual-terminal harness the same render-completion-grid is driven
;; under a forced capability verdict, so its selection highlight is read back per
;; cell on a colour target -- identically on every platform, no gate.  Selected
;; index 2 sits inside the first visible window (max-visible 4, no scroll), so its
;; row renders alongside the height-cap pager (menu-lines 5).  The leading newline
;; the grid emits puts grid row 0 on folded row 1, so the selected grid row 2
;; lands on folded row 3; the two-space indent puts the candidate's first glyph at
;; column 2 (mirrors the per-cell truecolour-background sibling in test-vterm-attrs).
(let* ([scr (render->screen overlay-cols
              (lambda (p)
                (render-completion-grid p overlay-entries 2
                                        overlay-cols overlay-max-visible)))]
       [sel-cell (vterm-cell scr 3 2)]
       [blank-cell (vterm-cell scr 0 0)])
  (test-equal "grid: the selected candidate's first glyph lands at row 3 column 2"
    #\c (cell-glyph sel-cell))
  (test-equal "grid: the selected cell carries the truecolour selection background"
    '(56 62 87) (cell-bg sel-cell))
  (test-equal "grid: the selected cell carries the truecolour selection foreground"
    '(195 202 230) (cell-fg sel-cell))
  (test-equal "grid: a cell outside the selection reads the default background"
    'default (cell-bg blank-cell))
  (test-assert "grid: the height-cap pager row renders in the folded grid"
    (any-line-has? scr " rows")))

;; ======================================================================
;; (e) Cycle redraws in place -- the behavioural no-stacking proof, on a
;; forced-caps STRING capture.  Drive the real read-expression over an
;; open-input-string with a Scheme completion prefix and repeated Tab: the first
;; Tab opens the menu, each further Tab cycles it.  A forced capability verdict
;; makes read-expression emit its full escape stream to a plain string port -- no
;; terminal, no TERM/NO_COLOR setup -- and end-of-input submits, so the drive
;; returns at once.  The rewired repaint uses relative motion and a tracked-span
;; clear, never a cursor save/restore, so the captured bytes carry ZERO DECSC and
;; ZERO DECRC.  The selection highlight marks each cycled redraw exactly once, so
;; its occurrence count stays bounded to the handful of cycles rather than growing
;; -- the menu does not stack.  A "menu did render" sanity check keeps the
;; no-stacking assertion non-vacuous.  These stay raw-byte assertions because the
;; virtual terminal CONSUMES ESC7/ESC8 and folds the highlight into cells, so a
;; folded grid cannot report their raw presence or count -- only the SOURCE of the
;; bytes changes (a forced-caps string capture), not the assertions.
;; ======================================================================

;; "(ca" opens a Scheme context (the paren) whose prefix "ca" has many stable Chez
;; completions (car, case, caar, ...); the trailing Tabs open then cycle the menu.
(define cycle-keys
  (string-append "(ca" (string #\tab) (string #\tab) (string #\tab)))

;; Forced-caps capture: read-expression emits its full escape stream to a string
;; port because assume-terminal-caps is forced 'on -- no terminal, no
;; TERM/NO_COLOR setup.  The existing byte helpers then run unchanged on it.
(let* ([sp (open-output-string)]
       [_ (parameterize ([assume-terminal-caps 'on])
            (read-expression "> " (open-input-string cycle-keys) sp))]
       [captured (get-output-string sp)]
       [sel-count (count-substring captured "\x1b;[48;2;")])
  (test-assert "cycle: the menu actually rendered (selection highlight present)"
    (>= sel-count 1))
  (test-assert "cycle: the repaint carries no cursor save/restore (no DECSC/DECRC)"
    (not (has-save-restore? captured)))
  (test-assert "cycle: the menu marker stays bounded across cycles (no stacking)"
    (<= sel-count 4)))

;; ======================================================================
;; (f) Drop-up decision -- PTY-free, the pure anchor predicate.
;; menu-anchor-place chooses 'up when the room below the edit line cannot hold the
;; menu and 'down otherwise.  Asserted at both sides of the boundary, and with
;; rows-below derived from the terminal height and the shared wrap-aware geometry
;; (count-visual-lines / cursor-visual-row) for a constructed buffer, so the
;; arithmetic is pinned to those helpers.
;; ======================================================================

(test-equal "anchor: room below equal to the menu height drops down" 'down
  (menu-anchor-place 6 6))
(test-equal "anchor: room below one short of the menu drops up" 'up
  (menu-anchor-place 5 6))
(test-equal "anchor: ample room below drops down" 'down
  (menu-anchor-place 20 4))
(test-equal "anchor: no room below drops up" 'up
  (menu-anchor-place 0 3))

;; rows-below from the shared geometry: a three-line buffer on a short terminal.
;; total wrapped rows via count-visual-lines; rows-below = (term-rows - 1) - total.
(let* ([pw 2] [text "(a\n(b\n(c"] [cols 80] [term-rows 6]
       [total (count-visual-lines pw text cols)]
       [crow (cursor-visual-row pw text cols)]
       [rows-below (max 0 (- (- term-rows 1) total))])
  (test-equal "anchor: a three-line buffer has two extra wrapped rows" 2 total)
  (test-equal "anchor: the cursor sits on the last wrapped row" 2 crow)
  (test-equal "anchor: rows-below from the terminal height and the geometry" 3 rows-below)
  (test-equal "anchor: a five-row menu will not fit below -> drop-up" 'up
    (menu-anchor-place rows-below 5))
  (test-equal "anchor: a three-row menu fits below -> dropdown" 'down
    (menu-anchor-place rows-below 3)))

;; ======================================================================
;; (g) Rewired-path plain-sink guard -- PTY-free, the escape-gating check.
;; The same completion cycle driven on a plain string out-port (a non-tty) must
;; carry ZERO escape bytes: the rewired menu still degrades to plain text off a
;; terminal, so a pipe or file target sees no cursor motion, no clear, no colour --
;; even under a capable TERM, so the absence is the sink's doing, not a dumb TERM.
;; ======================================================================

(let ([out (open-output-string)])
  (with-env* '(("TERM" . "xterm"))
    (lambda ()
      (read-expression "> " (open-input-string cycle-keys) out)))
  (test-assert "cycle on a plain string sink carries zero ESC bytes"
    (not (has-esc? (get-output-string out)))))

(test-end)
