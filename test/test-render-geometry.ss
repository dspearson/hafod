;;; test/test-render-geometry.ss -- Geometry of the ghost-suggestion renderer.
;;; Proves render-line/suggestion counts the ghost suggestion's own screen rows,
;;; so the terminal cursor is left on the user's logical typing row rather than
;;; stranded on the suggestion's bottom row.  Three layers:
;;;   (a) the pure wrap-aware helpers (count-visual-lines / cursor-visual-row /
;;;       ansi-display-width), asserted PTY-free and deterministically;
;;;   (b) a plain string-port sink, which emits zero escape bytes, draws no
;;;       ghost, and returns the LOGICAL cursor row (not the drawn total);
;;;   (c) a real pty slave, where the cursor-up climb spans exactly the ghost-
;;;       inclusive drawn rows above the logical typing row.
;;; Hang-free by construction: string ports everywhere plus a single BOUNDED
;;; master drain (EOF or a hard char cap stops it; a closed-pty read signal is
;;; caught).  The whole suite runs with stdin redirected from /dev/null.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod editor render)
              count-visual-lines cursor-visual-row ansi-display-width
              render-line/suggestion render-line)
        (hafod editor gap-buffer)
        (only (hafod environment) with-env* setenv)
        (only (hafod pty) open-pty)
        (only (hafod fd-ports) fdes->outport close)
        (only (hafod posix) posix-open O_WRONLY)
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

;; Scan a byte stream for every cursor-up escape ESC[<digits>A and return the
;; list of parsed counts, in order.  A CSI that is not a bare digits-then-A
;; sequence (an SGR colour like ESC[38;2;..m, the column move ESC[<n>G, the
;; clear ESC[J) never matches: the run between '[' and the terminator must be
;; digits only, and the terminator must be 'A'.
(define (collect-cursor-ups s)
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
             [(and saw? (char=? (string-ref s j) #\A))
              (scan (+ j 1) (cons n acc))]
             [else (scan (+ j 1) acc)]))]
        [else (scan (+ i 1) acc)]))))

(test-begin "render-geometry")

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
;; (c) Real pty slave -- the cursor climbs back over the ghost's own rows.
;; On a pty slave ansi-ok?/colour-ok? are #t, so the ghost is drawn and the
;; cursor repositioned.  With prev-lines 0 the ONLY ESC[<n>A in the stream is
;; that reposition climb, whose n must equal the ghost-inclusive drawn rows
;; minus the logical cursor row.  The master is drained with a single bounded
;; loop after the slave write-end is closed, so it can never block.
;; ======================================================================

(let* ([pty-prompt "> "]
       [pty-before "(a\n(b"]           ; buffer text, cursor left at end
       [pty-sugg "X\nY\nZ"]            ; multi-line ghost suggestion
       [pty-cols 80]
       [pty-pw (ansi-display-width pty-prompt)]
       [expected-climb
        (- (count-visual-lines pty-pw (string-append pty-before pty-sugg) pty-cols)
           (cursor-visual-row pty-pw pty-before pty-cols))])
  (let-values ([(master slave-name) (open-pty)])
    (setenv "NO_COLOR" #f)             ; unset baseline so colour-ok? is not vetoed
    (let ([slave-out (fdes->outport (posix-open slave-name O_WRONLY 0))])
      (let ([captured
             (with-env* '(("TERM" . "xterm"))
               (lambda ()
                 (let ([gb (buffer-from pty-before)])
                   (render-line/suggestion slave-out pty-prompt gb 0 pty-sugg pty-cols))
                 (flush-output-port slave-out)
                 (close slave-out)     ; last slave write-end -> master drains then EOFs
                 (let ([op (open-output-string)])
                   (guard (e [#t (void)])   ; a closed-pty read may signal; stop cleanly
                     (let drain ([count 0])
                       (when (< count 4096)
                         (let ([ch (read-char master)])
                           (unless (eof-object? ch)
                             (write-char ch op)
                             (drain (+ count 1)))))))
                   (get-output-string op))))])
        (close master)
        ;; Sanity: the ghost genuinely adds rows, so the assertion below is not
        ;; a vacuous "() equals ()".
        (test-assert "pty: drawn total exceeds the logical cursor row (ghost owns rows)"
          (> expected-climb 0))
        (test-equal "pty: cursor climbs exactly the ghost-inclusive rows above the typing row"
          (list expected-climb)
          (collect-cursor-ups captured))))))

(test-end)
