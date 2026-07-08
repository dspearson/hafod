;;; test/test-mode-indicator.ss -- The optional text mode indicator.
;;; The active editing mode is always visible through the cursor shape and
;;; colour, so the textual "-- NORMAL --" / "-- VISUAL --" row is redundant and
;;; OFF by default; a user's init may switch it on with (show-mode-indicator?
;;; #t).  Four PTY-free-or-bounded layers:
;;;   (A) the gate: editor-mode-indicator is #f in every mode by default, and
;;;       yields the mode strings only when the parameter is switched on;
;;;   (B) geometry: what the default feeds the renderer -- a #f indicator --
;;;       draws no extra row and is byte-identical to a plain render-line on a
;;;       non-tty string port;
;;;   (C) a real pty slave (ansi-ok?/colour-ok? #t): the indicator row IS drawn
;;;       while editing, and the suppressed re-render the submit path performs
;;;       clears it, so no "-- NORMAL --" residue remains to collide with the
;;;       printed result;
;;;   (D) regression: the toggle governs only the indicator -- an emacs region
;;;       and a vi visual still resolve through editor-selection-range with it on.
;;; Hang-free by construction: string ports everywhere plus a single BOUNDED
;;; master drain (EOF or a hard char cap stops it; a closed-pty read is caught).
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
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
        (only (hafod environment) with-env* setenv)
        (only (hafod pty) open-pty)
        (only (hafod fd-ports) fdes->outport close)
        (only (hafod posix) posix-open O_WRONLY)
        (only (hafod internal platform) os-family)
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

;; Start index of the LAST occurrence of NEEDLE in HAY, or #f.
(define (last-index-of hay needle)
  (let ([hl (string-length hay)]
        [nl (string-length needle)])
    (let outer ([i (- hl nl)])
      (cond
        [(< i 0) #f]
        [(string=? (substring hay i (+ i nl)) needle) i]
        [else (outer (- i 1))]))))

;; Remove every CSI SGR run (ESC '[' ... 'm') from S, leaving the plain text.
;; The colourised buffer redraw interleaves SGR runs between tokens, so the
;; buffer text is only contiguous once the SGR is stripped.
(define (strip-sgr s)
  (let ([n (string-length s)]
        [op (open-output-string)])
    (let loop ([i 0])
      (cond
        [(>= i n) (get-output-string op)]
        [(and (char=? (string-ref s i) #\x1b)
              (< (+ i 1) n)
              (char=? (string-ref s (+ i 1)) #\[))
         (let skip ([j (+ i 2)])
           (cond
             [(>= j n) (get-output-string op)]
             [(char=? (string-ref s j) #\m) (loop (+ j 1))]
             [else (skip (+ j 1))]))]
        [else
         (write-char (string-ref s i) op)
         (loop (+ i 1))]))))

(test-begin "mode-indicator")

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
;; (C) Real pty slave: the indicator draws, and the submit re-render clears it.
;; ======================================================================

;; Two frames on a pty slave (ansi-ok?/colour-ok? #t):
;;   frame 1: render-line/selection WITH the indicator -- draws "-- NORMAL --"
;;            on its own row below the edit line (the pre-submit state);
;;   frame 2: render-line with the indicator suppressed -- the exact call the
;;            submit path (finish!) makes: it climbs to the prompt line and
;;            clears to end of screen, wiping the indicator row, then redraws.
;; After the LAST clear-to-end-of-screen (ESC[J) no "-- NORMAL --" remains, so
;; the result printed next cannot collide with it.
;; LINUX-ONLY pty round-trip: closing the slave write-end makes the master read
;; drain then EOF on Linux, but on macOS/BSD the master read is unreliable (blocks
;; or returns partial data), so this fails/hangs off Linux.  The render path is
;; platform-agnostic and its escape-gating is covered by the string-port
;; assertions above on every OS; see test-completion-overlay for the rationale.
(unless (eq? os-family 'linux)
  (display "  mode-indicator: pty round-trip test skipped on non-Linux (EOF-on-slave-close is Linux-specific)\n"))
(when (eq? os-family 'linux)
 (let* ([prompt "> "]
       [text "(+ 9 5 4)"]
       [cols 80])
  (let-values ([(master slave-name) (open-pty)])
    (setenv "NO_COLOR" #f)              ; unset so colour-ok? is not vetoed
    (let ([slave-out (fdes->outport (posix-open slave-name O_WRONLY 0))])
      (let ([captured
             (with-env* '(("TERM" . "xterm"))
               (lambda ()
                 (let* ([gb (buffer-from text)]
                        [r (render-line/selection slave-out prompt gb 0
                                                  #f "-- NORMAL --" cols)])
                   ;; The suppressed re-render finish! performs on submit.
                   (render-line slave-out prompt gb r cols))
                 (flush-output-port slave-out)
                 (close slave-out)       ; last write-end -> master drains then EOFs
                 (let ([op (open-output-string)])
                   (guard (e [#t (void)])   ; a closed-pty read may signal; stop cleanly
                     (let drain ([count 0])
                       (when (< count 8192)
                         (let ([ch (read-char master)])
                           (unless (eof-object? ch)
                             (write-char ch op)
                             (drain (+ count 1)))))))
                   (get-output-string op))))])
        (close master)
        ;; Sanity: the indicator really was drawn (frame 1), else the clear
        ;; assertion below would be vacuous.
        (test-assert "pty: the indicator row is drawn while editing"
          (contains-substring? captured "-- NORMAL --"))
        ;; The submit re-render clears to end of screen; after that final ESC[J
        ;; the buffer is redrawn but the indicator is gone.
        (let* ([j (last-index-of captured "\x1b;[J")]
               [tail (if j (substring captured (+ j 3) (string-length captured))
                         captured)])
          (test-assert "pty: a clear-to-end-of-screen (ESC[J) is emitted on the submit re-render"
            (and j #t))
          (test-assert "pty: the buffer is redrawn after the final clear"
            (contains-substring? (strip-sgr tail) text))
          (test-assert "pty: no -- NORMAL -- residue remains after the final clear"
            (not (contains-substring? tail "-- NORMAL --")))))))))

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
