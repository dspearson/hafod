;;; test/test-selection-highlight.ss -- Unit suite for the selection-highlight
;;; render surface.  Two layers, both PTY-free:
;;;   (a) display-with-selection, the reverse-video span leaf.  Driven through a
;;;       string port with an explicit colour? boolean, it emits ESC[7m…ESC[27m
;;;       around exactly the selected span when colour? is #t and zero escape
;;;       bytes when colour? is #f, when the range is #f, or when the range is
;;;       empty after clamping.  An out-of-range end is clamped and the text
;;;       still round-trips intact.
;;;   (b) render-line/selection, the parallel entry point.  On a plain string
;;;       port (a non-tty, so colour-ok?/ansi-ok? are both #f) it draws neither
;;;       the highlight nor the mode indicator and emits zero escape bytes, yet
;;;       still returns the LOGICAL cursor row -- identical to render-line -- so
;;;       prev-lines tracking is unchanged.
;;; No real terminal is needed: every assertion runs over an in-memory string
;;; port, and the colour? boolean is threaded directly rather than derived from
;;; the port, so the highlight bytes are provable off a tty.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor gap-buffer)
        (only (hafod editor render)
              display-with-selection render-line/selection
              tokenize cursor-visual-row ansi-display-width))

;; ======================================================================
;; Helpers (each suite keeps its own copies).
;; ======================================================================

;; Build a gap buffer holding text with the cursor left at end-of-buffer, so the
;; whole text is "before" and "after" is empty.
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

;; Does the string carry an ESC '[' (the CSI introducer every SGR uses)?
(define (has-csi? s)
  (let ([n (string-length s)])
    (let scan ([i 0])
      (and (< (+ i 1) n)
           (or (and (char=? (string-ref s i) #\x1b)
                    (char=? (string-ref s (+ i 1)) #\[))
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

;; Remove every CSI SGR run (ESC '[' ... 'm') from s, leaving the plain text.
;; Used to prove a highlighted string still carries the original text verbatim.
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

;; Render text through display-with-selection and return the emitted bytes.
(define (render-sel text sel-range colour?)
  (let ([sp (open-output-string)])
    (display-with-selection sp text sel-range colour?)
    (get-output-string sp)))

(test-begin "selection-highlight")

;; ======================================================================
;; display-with-selection: the reverse-video span leaf.
;; ======================================================================

(define sample "hello world")

;; (1) colour? #t: the selected span [0,5) = "hello" sits between ESC[7m and
;; ESC[27m, and the output carries a CSI.
(test-assert "display-with-selection with colour? #t emits SGR (ESC [)"
  (has-csi? (render-sel sample '(0 . 5) #t)))
(test-assert "display-with-selection wraps exactly the selected span in [7m…[27m"
  (contains-substring? (render-sel sample '(0 . 5) #t)
                       "\x1b;[7mhello\x1b;[27m"))

;; (2) colour? #f: the same call emits zero ESC bytes and the plain text verbatim.
(test-assert "display-with-selection with colour? #f emits zero ESC bytes"
  (not (has-esc? (render-sel sample '(0 . 5) #f))))
(test-equal "display-with-selection with colour? #f renders the text verbatim"
  sample (render-sel sample '(0 . 5) #f))

;; (3) A #f range with colour? #t still emits zero ESC bytes (nothing to select).
(test-assert "display-with-selection with a #f range emits zero ESC bytes"
  (not (has-esc? (render-sel sample #f #t))))
(test-equal "display-with-selection with a #f range renders the text verbatim"
  sample (render-sel sample #f #t))

;; (4) An out-of-range end is clamped to the string length: the span runs to the
;; end ("world"), the closing [27m is emitted after the last char, and stripping
;; the SGR round-trips the original text with no crash.
(test-assert "display-with-selection clamps an out-of-range span to end-of-string"
  (contains-substring? (render-sel sample '(6 . 99) #t)
                       "\x1b;[7mworld\x1b;[27m"))
(test-equal "display-with-selection round-trips the text after a clamped span"
  sample (strip-sgr (render-sel sample '(6 . 99) #t)))

;; ======================================================================
;; render-line/selection: degradation on a non-tty string port.
;; ======================================================================

;; (5) A string port is a non-tty, so colour-ok?/ansi-ok? are both #f: no
;; highlight, no indicator, zero escape bytes -- yet the return value is the
;; LOGICAL cursor row (identical to render-line's), so prev-lines tracking is
;; unchanged.
(let* ([sp (open-output-string)]
       [prompt "> "]
       [text "(+ 1 2)"]
       [gb (buffer-from text)]
       [row (render-line/selection sp prompt gb 0 '(1 . 4) "-- VISUAL --")]
       [out (get-output-string sp)])
  (test-equal "render-line/selection returns the logical cursor row on a string port"
    (cursor-visual-row (ansi-display-width prompt) text 0)
    row)
  (test-assert "render-line/selection emits zero ESC bytes to a non-tty sink"
    (not (has-esc? out)))
  (test-assert "render-line/selection still draws the buffer text"
    (contains-substring? out text))
  (test-assert "render-line/selection draws no highlight off a terminal"
    (not (contains-substring? out "\x1b;[7m")))
  (test-assert "render-line/selection draws no mode indicator off a terminal"
    (not (contains-substring? out "-- VISUAL --"))))

;; A multi-line buffer confirms the return value tracks the buffer's own shape
;; (the logical row), never a highlight- or indicator-inflated total.
(let* ([sp (open-output-string)]
       [prompt "> "]
       [text "a\nb"]
       [gb (buffer-from text)]
       [row (render-line/selection sp prompt gb 0 '(0 . 1) "-- VISUAL --")])
  (test-equal "render-line/selection returns the logical row for a multi-line buffer"
    (cursor-visual-row (ansi-display-width prompt) text 0)
    row))

(test-end)
