;;; (hafod editor vi) -- Full vim emulation: motions, operators, text objects,
;;; visual mode, search, registers, marks, count prefixes, repeat.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor vi)
  (export
    ;; Main entry point for normal-mode key processing
    vi-process-key
    ;; State management
    vi-reset-state! vi-reset-session! vi-clear-visual!
    ;; State accessors (for rendering)
    vi-visual-mode vi-visual-anchor vi-visual-end vi-visual-range
    vi-mode-indicator vi-pending-state
    ;; Search
    vi-search-pattern vi-search-direction
    ;; Register read + paste seam, plus the keymap-count seam (white-box; read
    ;; by editor.ss paste / keymap dispatch and the tests — deliberately not
    ;; re-exported by the (hafod) umbrella)
    vi-reg-fetch vi-take-paste-register! vi-take-keymap-count!
    ;; Dot-repeat: mid-command signal + replay delegation (white-box; the hook
    ;; is set by editor.ss — deliberately not re-exported by the (hafod) umbrella)
    vi-mid-command? vi-replay!-proc
    ;; For editor.ss integration (parameters — call with value to set)
    vi-snapshot!-proc vi-undo!-proc vi-redo!-proc
    vi-enter-insert!-proc vi-enter-normal!-proc
    vi-history-prev!-proc vi-history-next!-proc
    vi-submit!-proc
    ;; Configurable surround trigger keys (settable; read live by the dispatch)
    surround-op-key surround-visual-key)
  (import (chezscheme)
          (hafod editor gap-buffer)
          (hafod editor kill-ring)
          (hafod editor input-decode)
          (hafod editor sexp-tracker))

  ;; ======================================================================
  ;; Procedure hooks (set by editor.ss to avoid circular deps)
  ;; ======================================================================

  (define vi-snapshot!-proc (make-parameter #f))
  (define vi-undo!-proc (make-parameter #f))
  (define vi-redo!-proc (make-parameter #f))
  (define vi-enter-insert!-proc (make-parameter #f))
  (define vi-enter-normal!-proc (make-parameter #f))
  (define vi-history-prev!-proc (make-parameter #f))
  (define vi-history-next!-proc (make-parameter #f))
  (define vi-submit!-proc (make-parameter #f))
  ;; Dot-repeat replay delegation: set by editor.ss to the driver that re-feeds
  ;; the recorded keystrokes of the last change. The default #f keeps the . arm
  ;; a safe no-op until the engine is wired.
  (define vi-replay!-proc (make-parameter #f))

  ;; ======================================================================
  ;; Configurable surround trigger keys
  ;; ======================================================================
  ;; The surround operator (ys / cs / ds) and the visual surround (S) are plain
  ;; character keys the vi dispatch consumes directly, so — unlike the keymap
  ;; sexp bindings — their trigger characters live here and are read live on
  ;; every key. Rebind by setting the parameter; the guard keeps a character,
  ;; falling back to the default when handed anything else.
  (define surround-op-key
    (make-parameter #\s (lambda (v) (if (char? v) v #\s))))
  (define surround-visual-key
    (make-parameter #\S (lambda (v) (if (char? v) v #\S))))

  ;; ======================================================================
  ;; Vi state — single mutable record
  ;; ======================================================================

  (define-record-type vi-state
    (fields (mutable count)
            (mutable operator)          ;; #f | 'd | 'c | 'y
            (mutable register-char)     ;; current register
            (mutable last-find)         ;; (dir . char) where dir = f|F|t|T
            (mutable search-pat)
            (mutable search-dir)
            (mutable vis-mode)          ;; #f | 'char | 'line
            (mutable vis-anchor)
            (mutable marks-ht)
            (mutable registers-ht)
            (mutable pending)           ;; 'normal|'operator|'replace|'find|'register
            (mutable find-dir)          ;; 'f|'F|'t|'T
            (mutable last-edit)         ;; (keys . initial-state) for . repeat
            (mutable recording-edit)    ;; list of key events being recorded
            (mutable operator-count)    ;; count stashed on operator entry (2 in 2d3w)
            (mutable keymap-count)))    ;; count surfaced to the editor keymap (3 in 3x)

  (define *vi*
    (make-vi-state
      0              ;; count
      #f             ;; operator
      #\"            ;; register-char
      #f             ;; last-find
      ""             ;; search-pat
      'forward       ;; search-dir
      #f             ;; vis-mode
      0              ;; vis-anchor
      (make-hashtable char->integer char=?)   ;; marks-ht
      (make-hashtable char->integer char=?)   ;; registers-ht
      'normal        ;; pending
      #f             ;; find-dir
      #f             ;; last-edit
      #f             ;; recording-edit
      0              ;; operator-count
      0))            ;; keymap-count

  ;; Public accessors
  (define (vi-visual-mode) (vi-state-vis-mode *vi*))
  (define (vi-visual-anchor) (vi-state-vis-anchor *vi*))
  (define (vi-visual-end gb) (gap-buffer-cursor-pos gb))
  ;; Resolve the active visual selection into a (start . end) index span in the
  ;; buffer's coordinate space, or #f when no visual mode is active. The span is
  ;; the highlighted text: characterwise is inclusive of the cursor character
  ;; (min..max+1, clamped to the text length); linewise covers the WHOLE logical
  ;; line(s) the selection spans -- from the start of the lower endpoint's line
  ;; to the end of the upper endpoint's line -- via the shared line-bounds helper
  ;; (so on a single-line buffer this is still 0..len). The highlight is the line
  ;; content; the linewise delete/yank in the operator arm additionally takes the
  ;; bounding newline so the line is actually removed.
  (define (vi-visual-range gb)
    (let ([mode (vi-state-vis-mode *vi*)])
      (and mode
           (let* ([text (gap-buffer->string gb)]
                  [len (string-length text)]
                  [pos (gap-buffer-cursor-pos gb)]
                  [anchor (vi-state-vis-anchor *vi*)])
             (if (eq? mode 'line)
                 (let-values ([(lo-start lo-end) (current-line-bounds text (min pos anchor))]
                              [(hi-start hi-end) (current-line-bounds text (max pos anchor))])
                   (cons lo-start hi-end))
                 ;; characterwise: min..max+1, clamped into [0, len], start <= end
                 (let* ([start (max 0 (min (min pos anchor) len))]
                        [end (max start (min (+ (max pos anchor) 1) len))])
                   (cons start end)))))))
  (define (vi-mode-indicator)
    (cond
      [(eq? (vi-state-pending *vi*) 'operator)
       (string-append "-- "
         (case (vi-state-operator *vi*)
           [(d) "DELETE"] [(c) "CHANGE"] [(y) "YANK"] [else "?"])
         " --")]
      [(vi-state-vis-mode *vi*)
       (case (vi-state-vis-mode *vi*)
         [(char) "-- VISUAL --"]
         [(line) "-- VISUAL LINE --"]
         [else "-- VISUAL --"])]
      [else #f]))
  (define (vi-pending-state) (vi-state-pending *vi*))
  ;; Dot-repeat mid-command signal: #t while the state machine is part-way
  ;; through a command — an operator is pending, OR a find/replace/register
  ;; prefix is waiting (pending is not 'normal), OR a count is being accumulated
  ;; (count > 0). #f from a fully-idle normal state. This lets the main loop tell
  ;; an in-flight change from an idle one without reaching into the vi internals.
  (define (vi-mid-command?)
    (or (and (vi-state-operator *vi*) #t)
        (not (eq? (vi-state-pending *vi*) 'normal))
        (> (vi-state-count *vi*) 0)))
  (define (vi-search-pattern) (vi-state-search-pat *vi*))
  (define (vi-search-direction) (vi-state-search-dir *vi*))

  (define (vi-reset-state!)
    (vi-state-count-set! *vi* 0)
    (vi-state-operator-set! *vi* #f)
    (vi-state-operator-count-set! *vi* 0)
    (vi-state-keymap-count-set! *vi* 0)
    (vi-state-register-char-set! *vi* #\")
    (vi-state-pending-set! *vi* 'normal)
    (vi-state-find-dir-set! *vi* #f)
    (vi-state-recording-edit-set! *vi* #f))

  (define (vi-reset-session!)
    (vi-reset-state!)
    (vi-state-vis-mode-set! *vi* #f)
    (vi-state-vis-anchor-set! *vi* 0)
    (vi-state-last-find-set! *vi* #f)
    (vi-state-search-pat-set! *vi* "")
    (vi-state-search-dir-set! *vi* 'forward)
    (hashtable-clear! (vi-state-marks-ht *vi*))
    (vi-state-last-edit-set! *vi* #f))

  ;; Dismiss any active visual selection -- clear the mode and its anchor while
  ;; leaving the rest of the vi session (marks, search, registers) intact.  This
  ;; is the narrow reset used when entering insert mode: a visual open in normal
  ;; mode must not survive into typing, but a search pattern or mark the user set
  ;; should.  Mirrors the visual-clear the Escape handler performs.
  (define (vi-clear-visual!)
    (vi-state-vis-mode-set! *vi* #f)
    (vi-state-vis-anchor-set! *vi* 0))

  ;; ======================================================================
  ;; Character classification
  ;; ======================================================================

  (define (word-char? ch)
    (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)
        (char=? ch #\-) (char=? ch #\?) (char=? ch #\!)))

  (define (blank? ch) (or (char=? ch #\space) (char=? ch #\tab)))

  ;; ======================================================================
  ;; Logical-line bounds
  ;; ======================================================================

  ;; The half-open span [start, end) of the logical line containing POS: start is
  ;; one past the newline at-or-before POS (0 when there is none), end is the
  ;; index of the next newline at-or-after POS (the text length when there is
  ;; none -- i.e. the final line). POS is clamped into [0, len] first and both
  ;; results stay within [0, len], so a malformed position or an empty buffer can
  ;; never yield a negative or overrunning index, nor an unbounded scan. This is
  ;; the single definition of where a logical line begins and ends, shared by the
  ;; line operators (dd/cc/yy), the line-relative motions (0/^/$/gg/G) and the
  ;; visual-line span so all three agree on the same line.
  (define (current-line-bounds text pos)
    (let* ([len (string-length text)]
           [p (max 0 (min pos len))]
           [start (let scan ([i (- p 1)])
                    (cond
                      [(< i 0) 0]
                      [(char=? (string-ref text i) #\newline) (+ i 1)]
                      [else (scan (- i 1))]))]
           [end (let scan ([i p])
                  (cond
                    [(>= i len) len]
                    [(char=? (string-ref text i) #\newline) i]
                    [else (scan (+ i 1))]))])
      (values start end)))

  ;; ======================================================================
  ;; Motion functions: (text pos) -> new-pos or #f
  ;; ======================================================================

  ;; Forward word (vim w): to start of next word
  (define (motion-w text pos)
    (let ([len (string-length text)])
      (if (>= pos len) #f
          (let* ([ch (string-ref text pos)]
                 [skip-class
                   (cond [(word-char? ch) word-char?]
                         [(char-whitespace? ch) char-whitespace?]
                         [else (lambda (c) (and (not (word-char? c))
                                                (not (char-whitespace? c))))])])
              ;; Skip current class
              (let lp1 ([i pos])
                (cond
                  [(>= i len) len]
                  [(not (skip-class (string-ref text i)))
                   ;; Skip whitespace
                   (let lp2 ([j i])
                     (cond [(>= j len) len]
                           [(char-whitespace? (string-ref text j)) (lp2 (+ j 1))]
                           [else j]))]
                  [else (lp1 (+ i 1))]))))))

  ;; Forward WORD (vim W): to start of next WORD (non-blank sequence)
  (define (motion-W text pos)
    (let ([len (string-length text)])
      (if (>= pos len) #f
          ;; Skip non-blanks
          (let lp1 ([i pos])
            (cond
              [(>= i len) len]
              [(not (blank? (string-ref text i))) (lp1 (+ i 1))]
              [else
               ;; Skip blanks
               (let lp2 ([j i])
                 (cond [(>= j len) len]
                       [(blank? (string-ref text j)) (lp2 (+ j 1))]
                       [else j]))])))))

  ;; Backward word (vim b)
  (define (motion-b text pos)
    (if (<= pos 0) #f
        ;; Skip whitespace backwards
        (let lp1 ([i (- pos 1)])
          (cond
            [(< i 0) 0]
            [(char-whitespace? (string-ref text i)) (lp1 (- i 1))]
            [else
             ;; Now at last char of previous word — find its start
             (let* ([ch (string-ref text i)]
                    [same-class?
                      (if (word-char? ch) word-char?
                          (lambda (c) (and (not (word-char? c))
                                           (not (char-whitespace? c)))))])
               (let lp2 ([j i])
                 (cond
                   [(< j 0) 0]
                   [(same-class? (string-ref text j)) (lp2 (- j 1))]
                   [else (+ j 1)])))]))))

  ;; Backward WORD (vim B)
  (define (motion-B text pos)
    (if (<= pos 0) #f
        ;; Skip blanks backwards
        (let lp1 ([i (- pos 1)])
          (cond
            [(< i 0) 0]
            [(blank? (string-ref text i)) (lp1 (- i 1))]
            [else
             ;; Find start of WORD
             (let lp2 ([j i])
               (cond
                 [(< j 0) 0]
                 [(blank? (string-ref text j)) (+ j 1)]
                 [else (lp2 (- j 1))]))]))))

  ;; End of word (vim e)
  (define (motion-e text pos)
    (let ([len (string-length text)])
      (if (>= (+ pos 1) len) (if (>= pos len) #f (- len 1))
          ;; Move at least one position, skip whitespace
          (let lp1 ([i (+ pos 1)])
            (cond
              [(>= i len) (- len 1)]
              [(char-whitespace? (string-ref text i)) (lp1 (+ i 1))]
              [else
               ;; Find end of word
               (let* ([ch (string-ref text i)]
                      [same-class?
                        (if (word-char? ch) word-char?
                            (lambda (c) (and (not (word-char? c))
                                             (not (char-whitespace? c)))))])
                 (let lp2 ([j i])
                   (cond
                     [(>= j len) (- len 1)]
                     [(not (same-class? (string-ref text j))) (- j 1)]
                     [else (lp2 (+ j 1))])))])))))

  ;; End of WORD (vim E)
  (define (motion-E text pos)
    (let ([len (string-length text)])
      (if (>= (+ pos 1) len) (if (>= pos len) #f (- len 1))
          ;; Skip at least one, then blanks
          (let lp1 ([i (+ pos 1)])
            (cond
              [(>= i len) (- len 1)]
              [(blank? (string-ref text i)) (lp1 (+ i 1))]
              [else
               ;; Find end of WORD
               (let lp2 ([j i])
                 (cond
                   [(>= j len) (- len 1)]
                   [(blank? (string-ref text j)) (- j 1)]
                   [else (lp2 (+ j 1))]))])))))

  ;; Find char forward (vim f)
  (define (motion-find-char text pos ch)
    (let ([len (string-length text)])
      (let lp ([i (+ pos 1)])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref text i) ch) i]
          [else (lp (+ i 1))]))))

  ;; Find char backward (vim F)
  (define (motion-find-char-back text pos ch)
    (let lp ([i (- pos 1)])
      (cond
        [(< i 0) #f]
        [(char=? (string-ref text i) ch) i]
        [else (lp (- i 1))])))

  ;; Till char forward (vim t) — one before the found char
  (define (motion-till-char text pos ch)
    (let ([found (motion-find-char text pos ch)])
      (and found (> found pos) (- found 1))))

  ;; Till char backward (vim T) — one after the found char
  (define (motion-till-char-back text pos ch)
    (let ([found (motion-find-char-back text pos ch)])
      (and found (< found pos) (+ found 1))))

  ;; Match paren (vim %)
  (define (motion-match-paren text pos)
    (let ([len (string-length text)])
      (and (< pos len)
           (let ([ch (string-ref text pos)])
             (cond
               [(or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
                ;; Find matching close
                (let ([close (case ch [(#\() #\)] [(#\[) #\]] [(#\{) #\}])])
                  (let lp ([i (+ pos 1)] [d 1])
                    (cond
                      [(>= i len) #f]
                      [(char=? (string-ref text i) ch) (lp (+ i 1) (+ d 1))]
                      [(char=? (string-ref text i) close)
                       (if (= d 1) i (lp (+ i 1) (- d 1)))]
                      [else (lp (+ i 1) d)])))]
               [(or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
                ;; Find matching open
                (let ([open (case ch [(#\)) #\(] [(#\]) #\[] [(#\}) #\{])])
                  (let lp ([i (- pos 1)] [d 1])
                    (cond
                      [(< i 0) #f]
                      [(char=? (string-ref text i) ch) (lp (- i 1) (+ d 1))]
                      [(char=? (string-ref text i) open)
                       (if (= d 1) i (lp (- i 1) (- d 1)))]
                      [else (lp (- i 1) d)])))]
               ;; If not on a paren, search forward for one
               [else
                (let lp ([i (+ pos 1)])
                  (cond
                    [(>= i len) #f]
                    [(memv (string-ref text i) '(#\( #\) #\[ #\] #\{ #\}))
                     (motion-match-paren text i)]
                    [else (lp (+ i 1))]))])))))

  ;; First non-blank of the current line (vim ^). Scans forward from the line
  ;; start, staying within the current logical line, and falls back to the line
  ;; start when the whole line is blank -- so ^ never crosses a newline into a
  ;; neighbouring line.
  (define (motion-first-non-blank text pos)
    (let-values ([(start end) (current-line-bounds text pos)])
      (let lp ([i start])
        (cond
          [(>= i end) start]
          [(char-whitespace? (string-ref text i)) (lp (+ i 1))]
          [else i]))))

  ;; ======================================================================
  ;; Text objects: (text pos) -> (values start end) inclusive start, exclusive end
  ;; ======================================================================

  (define (text-obj-inner-word text pos)
    (let ([len (string-length text)])
      (if (or (>= pos len) (= len 0)) (values #f #f)
          (let* ([ch (string-ref text pos)]
                 [same?
                   (cond [(word-char? ch) word-char?]
                         [(char-whitespace? ch) char-whitespace?]
                         [else (lambda (c) (and (not (word-char? c))
                                                (not (char-whitespace? c))))])]
                 [start (let lp ([i pos])
                           (if (and (> i 0) (same? (string-ref text (- i 1))))
                               (lp (- i 1)) i))]
                 [end (let lp ([i pos])
                         (if (and (< i len) (same? (string-ref text i)))
                             (lp (+ i 1)) i))])
              (values start end)))))

  (define (text-obj-around-word text pos)
    (let-values ([(start end) (text-obj-inner-word text pos)])
      (if (not start) (values #f #f)
          (let ([len (string-length text)])
            ;; Include trailing whitespace, or leading if no trailing
            (let ([end2 (let lp ([i end])
                          (if (and (< i len) (char-whitespace? (string-ref text i)))
                              (lp (+ i 1)) i))])
              (if (> end2 end)
                  (values start end2)
                  ;; Include leading whitespace
                  (let ([start2 (let lp ([i start])
                                  (if (and (> i 0)
                                           (char-whitespace? (string-ref text (- i 1))))
                                      (lp (- i 1)) i))])
                    (values start2 end))))))))

  ;; Inner/around WORD
  (define (text-obj-inner-WORD text pos)
    (let ([len (string-length text)])
      (if (or (>= pos len) (blank? (string-ref text pos))) (values #f #f)
          (let ([start (let lp ([i pos])
                         (if (and (> i 0) (not (blank? (string-ref text (- i 1)))))
                             (lp (- i 1)) i))]
                [end (let lp ([i pos])
                       (if (and (< i len) (not (blank? (string-ref text i))))
                           (lp (+ i 1)) i))])
            (values start end)))))

  (define (text-obj-around-WORD text pos)
    (let-values ([(start end) (text-obj-inner-WORD text pos)])
      (if (not start) (values #f #f)
          (let ([len (string-length text)])
            (let ([end2 (let lp ([i end])
                          (if (and (< i len) (blank? (string-ref text i)))
                              (lp (+ i 1)) i))])
              (if (> end2 end)
                  (values start end2)
                  (let ([start2 (let lp ([i start])
                                  (if (and (> i 0) (blank? (string-ref text (- i 1))))
                                      (lp (- i 1)) i))])
                    (values start2 end))))))))

  ;; A quote/tick delimiter at index I is escaped when it is preceded by an ODD
  ;; run of backslashes -- \" is an escaped quote, \\" is a literal backslash
  ;; then a live quote. This mirrors the forward backslash-skip string-bounds-at
  ;; uses, so the backward opener walk and the forward closer walk agree on which
  ;; delimiters are live; an escaped quote never opens or closes the span.
  (define (delimiter-escaped? text i)
    (let lp ([j (- i 1)] [n 0])
      (if (and (>= j 0) (char=? (string-ref text j) #\\))
          (lp (- j 1) (+ n 1))
          (odd? n))))

  ;; Inner/around delimited pair: parens, brackets, braces, quotes
  (define (text-obj-inner-pair text pos open close)
    (let ([len (string-length text)])
      ;; Find enclosing open
      (let ([op (if (char=? open close)
                    ;; For quotes/ticks: resolve the pair ENCLOSING the cursor,
                    ;; mirroring string-bounds-at. A live delimiter under the
                    ;; cursor is taken as the opener (the on-opening-quote case);
                    ;; otherwise walk BACKWARD from just before the cursor to the
                    ;; nearest UNescaped delimiter -- an escaped one is skipped so
                    ;; it never opens the span. No opener before the cursor leaves
                    ;; op #f (a safe no-op). Every index stays within [0, len].
                    (if (and (< pos len)
                             (char=? (string-ref text pos) open)
                             (not (delimiter-escaped? text pos)))
                        pos
                        (let lp ([i (- pos 1)])
                          (cond
                            [(< i 0) #f]
                            [(and (char=? (string-ref text i) open)
                                  (not (delimiter-escaped? text i)))
                             i]
                            [else (lp (- i 1))])))
                    ;; For paired delimiters: find matching open with nesting
                    (let lp ([i (if (and (< pos len) (char=? (string-ref text pos) open))
                                   pos (- pos 1))]
                             [d 0])
                      (cond
                        [(< i 0) #f]
                        [(char=? (string-ref text i) close) (lp (- i 1) (+ d 1))]
                        [(char=? (string-ref text i) open)
                         (if (= d 0) i (lp (- i 1) (- d 1)))]
                        [else (lp (- i 1) d)])))])
        (if (not op)
            (values #f #f)
            ;; Find matching close
            (let ([cl (if (char=? open close)
                          ;; For quotes/ticks: the nearest UNescaped delimiter
                          ;; forward of the opener, skipping a backslash-escaped
                          ;; one (\X consumes two chars) so an internal escaped
                          ;; delimiter never closes the span.
                          (let lp ([i (+ op 1)])
                            (cond
                              [(>= i len) #f]
                              [(char=? (string-ref text i) close) i]
                              [(char=? (string-ref text i) #\\) (lp (+ i 2))]
                              [else (lp (+ i 1))]))
                          ;; For paired: find matching close
                          (let lp ([i (+ op 1)] [d 0])
                            (cond
                              [(>= i len) #f]
                              [(char=? (string-ref text i) open) (lp (+ i 1) (+ d 1))]
                              [(char=? (string-ref text i) close)
                               (if (= d 0) i (lp (+ i 1) (- d 1)))]
                              [else (lp (+ i 1) d)])))])
              (if (not cl)
                  (values #f #f)
                  (values (+ op 1) cl)))))))

  (define (text-obj-around-pair text pos open close)
    (let-values ([(start end) (text-obj-inner-pair text pos open close)])
      (if (not start) (values #f #f)
          (values (- start 1) (+ end 1)))))

  ;; ======================================================================
  ;; Surround target-family locator (shared by the change verb)
  ;; ======================================================================
  ;; A change target names a delimiter FAMILY, not a literal char: the verb must
  ;; find the pair enclosing the cursor and normalise every family to two
  ;; delimiter indices. open-mark? drives the inner-whitespace trim (only the
  ;; four opening brackets trim); surround-target-chars resolves a typed char
  ;; (including the b/B/r/a aliases) to its canonical open/close pair;
  ;; enclosing-pair-of-type is the lexer- and type-aware bracket walk; and
  ;; surround-locate dispatches by family, guarding a non-delimiter char up front
  ;; so a typo can never reach char=? with a #f.

  ;; True only for the four opening brackets — the marks whose change form trims
  ;; the inner whitespace. Every closer, alias (b/B/r/a) and quote preserves it.
  (define (open-mark? ch) (memv ch '(#\( #\[ #\{ #\<)))

  ;; The canonical (open close) delimiter chars for any typed target char,
  ;; resolving the aliases; (values #f #f) for anything unrecognised. Used both
  ;; to choose the locator family and to hand the raw pair scanner its chars.
  (define (surround-target-chars ch)
    (case ch
      [(#\( #\) #\b) (values #\( #\))]
      [(#\[ #\] #\r) (values #\[ #\])]
      [(#\{ #\} #\B) (values #\{ #\})]
      [(#\< #\> #\a) (values #\< #\>)]
      [(#\") (values #\" #\")]
      [(#\') (values #\' #\')]
      [(#\`) (values #\` #\`)]
      [else (values #f #f)]))

  ;; The innermost enclosing pair of a GIVEN bracket type around pos, lexer-aware
  ;; via scan-lexer's open stack and find-matching-close. Walks the stack
  ;; innermost-first and returns the first opener whose char is open-ch, together
  ;; with its match — so a change on ([a]) finds the nearest ( skipping the inner
  ;; [, and a ( inside a string or comment is never treated as structure.
  ;; Returns (values open-idx close-idx) or (values #f #f).
  (define (enclosing-pair-of-type text pos open-ch)
    (let-values ([(state stack bc) (scan-lexer text pos)])
      (cond
        [(not (eq? state 'normal)) (values #f #f)]
        ;; Cursor sitting ON the opener: scan-lexer only stacks openers in
        ;; [0, pos), so an opener at pos itself is invisible to the stack walk.
        ;; Take it as the open index directly -- it is genuine structure, the
        ;; 'normal guard above having ruled out an opener inside a string or
        ;; comment -- matching how the raw-scan brace/angle/quote families and
        ;; ci( already accept the cursor being on the opening delimiter.
        [(and (fx< pos (string-length text))
              (char=? (string-ref text pos) open-ch))
         (let ([cl (find-matching-close text pos)])
           (if cl (values pos cl) (values #f #f)))]
        [else
         (let loop ([s stack])
           (cond
             [(null? s) (values #f #f)]
             [(char=? (string-ref text (car s)) open-ch)
              (let ([cl (find-matching-close text (car s))])
                (if cl (values (car s) cl) (values #f #f)))]
             [else (loop (cdr s))]))])))

  ;; Locate the enclosing pair a change target refers to, normalised to the two
  ;; delimiter indices (so the inner content is always
  ;; (substring text (+ open-idx 1) close-idx)). The leading #f guard is
  ;; MANDATORY and must precede every char=? branch: a non-delimiter char
  ;; (surround-target-chars → #f) returns (values #f #f) here, so a stray key
  ;; never reaches a (char=? #f …) further down and the change simply no-ops.
  ;;  - ( or [   → the lexer- and type-aware bracket walk.
  ;;  - "        → string-bounds-at, guarded on the cursor being in a string (its
  ;;               close is already the closing-quote index).
  ;;  - {}/<>/'/` → the raw nested-pair scan; normalise the half-open (start end)
  ;;               that text-obj-around-pair yields to (start, end-1).
  (define (surround-locate text pos ch)
    (let-values ([(open-char close-char) (surround-target-chars ch)])
      (cond
        [(not open-char) (values #f #f)]
        [(or (char=? open-char #\() (char=? open-char #\[))
         (enclosing-pair-of-type text pos open-char)]
        [(char=? open-char #\")
         (if (eq? (lexer-state-at text pos) 'in-string)
             (string-bounds-at text pos)
             (values #f #f))]
        [else
         (let-values ([(s e) (text-obj-around-pair text pos open-char close-char)])
           (if (and s e) (values s (fx- e 1)) (values #f #f)))])))

  ;; ======================================================================
  ;; Surround delimiter table and wrap transform
  ;; ======================================================================
  ;; The single source of truth for the surround pairs. Given the delimiter
  ;; character the user typed, yield the opening and closing strings with the
  ;; spacing convention baked in: an opening bracket ( [ { < adds one inner
  ;; space per side; a closing bracket ) ] } > and its aliases b B r a add none;
  ;; the symmetric quotes " ' ` add none. Any other character is not a
  ;; recognised delimiter and yields (values #f #f), so the caller treats it as
  ;; a safe no-op. Note the deliberate divergence from vim-surround: < is the
  ;; space-adding open-angle here (vim opens a tag prompt), since this editor
  ;; has no markup and defers tags.
  (define (surround-strings ch)
    (case ch
      [(#\() (values "( " " )")]
      [(#\) #\b) (values "(" ")")]
      [(#\[) (values "[ " " ]")]
      [(#\] #\r) (values "[" "]")]
      [(#\{) (values "{ " " }")]
      [(#\} #\B) (values "{" "}")]
      [(#\<) (values "< " " >")]
      [(#\> #\a) (values "<" ">")]
      [(#\") (values "\"" "\"")]
      [(#\') (values "'" "'")]
      [(#\`) (values "`" "`")]
      [else (values #f #f)]))

  ;; Pure wrap transform: splice the opening/closing strings around the span
  ;; [s, e) of text and return the new text together with the cursor position on
  ;; the new opening delimiter. No mutation; the caller commits the result.
  (define (wrap-span text s e open-str close-str)
    (values (string-append (substring text 0 s) open-str
                           (substring text s e) close-str
                           (substring text e (string-length text)))
            s))

  ;; Read a single raw delimiter character from the port, returning the
  ;; character or #f. This only guards against a truncated read (eof) or a
  ;; non-character event; whether the character is a recognised delimiter is
  ;; decided by the caller through surround-strings, so a malformed sequence
  ;; never acts on the buffer.
  (define (vi-read-delim in-port)
    (let ([e (read-key-event in-port)])
      (and (not (eof-object? e))
           (eq? (key-event-type e) 'char)
           (key-event-value e))))

  ;; ======================================================================
  ;; Lexer-aware s-expression text object
  ;; ======================================================================
  ;; Unlike text-obj-inner-pair, which counts raw parens and therefore
  ;; miscounts a ( inside a string, a comment, or a #\( character literal, the
  ;; s object resolves through the sexp-tracker primitives. Those gate on the
  ;; lexer state 'normal, so only genuine structure is ever matched.

  ;; The enclosing form around pos: inner drops the delimiters, around keeps
  ;; them. Returns a half-open (values start end), or (values #f #f).
  (define (form-object text pos inner?)
    (let-values ([(open close) (find-enclosing-parens text pos)])
      (cond
        [(not (and open close)) (values #f #f)]
        [inner? (values (fx+ open 1) close)]
        [else (values open (fx+ close 1))])))

  ;; Bounds of the string enclosing pos. Precondition: pos is inside a string.
  ;; Scan forward to the closing quote, honouring backslash escapes; derive the
  ;; opening quote by handing the position just past the close quote to
  ;; backward-sexp-start, whose close-quote branch scans from the start of the
  ;; text to find the matching open. close is the index of the closing quote.
  (define (string-bounds-at text pos)
    (let* ([len (string-length text)]
           [close (let scan ([i pos])
                    (cond
                      [(fx>= i len) #f]
                      [(char=? (string-ref text i) #\\) (scan (fx+ i 2))]
                      [(char=? (string-ref text i) #\") i]
                      [else (scan (fx+ i 1))]))]
           [open (and close (backward-sexp-start text (fx+ close 1)))])
      (if (and open close) (values open close) (values #f #f))))

  ;; Resolve the s-expression at pos into a half-open (values start end).
  ;; Dispatches on the lexer state so strings and comments are handled as whole
  ;; units and never scanned as structure.
  (define (resolve-sexp-object text pos inner?)
    (let ([state (lexer-state-at text pos)]
          [len (string-length text)])
      (cond
        ;; Inside a string: the string is the unit (quotes dropped for inner).
        [(eq? state 'in-string)
         (let-values ([(open close) (string-bounds-at text pos)])
           (cond
             [(not open) (values #f #f)]
             [inner? (values (fx+ open 1) close)]
             [else (values open (fx+ close 1))]))]
        ;; Inside a comment: no structural sub-unit; take the enclosing form.
        [(memq state '(in-line-comment in-block-comment))
         (form-object text pos inner?)]
        [else
         (let ([ch (and (fx< pos len) (string-ref text pos))])
           (cond
             ;; On an opener: this list.
             [(and ch (memv ch '(#\( #\[)))
              (let ([close (find-matching-close text pos)])
                (cond
                  [(not close) (values #f #f)]
                  [inner? (values (fx+ pos 1) close)]
                  [else (values pos (fx+ close 1))]))]
             ;; On a closer: the list it closes.
             [(and ch (memv ch '(#\) #\])))
              (let ([open (find-matching-paren text pos)])
                (cond
                  [(not open) (values #f #f)]
                  [inner? (values (fx+ open 1) pos)]
                  [else (values open (fx+ pos 1))]))]
             ;; On an atom: bound it by scanning forward, then back from the
             ;; end (so the first atom in a list, where a backward scan from
             ;; pos would stop at the opener, is still bounded). Inner and
             ;; around are identical for an atom -- it has no delimiters.
             [(and ch (not (char-whitespace? ch)))
              (let* ([end (forward-sexp-end text pos)]
                     [start (and end (backward-sexp-start text end))])
                (if (and start end) (values start end) (values #f #f)))]
             ;; On whitespace inside a list: the enclosing form.
             [else (form-object text pos inner?)]))])))

  ;; ======================================================================
  ;; Register management
  ;; ======================================================================

  ;; Store TEXT into register REG, always mirroring into the unnamed register.
  ;; An upper-case A..Z register appends to its lower-case sibling and the
  ;; unnamed register then mirrors the accumulated text; any other named register
  ;; overwrites (and mirrors); the unnamed default writes only itself. When YANK?
  ;; is true the text also lands in the "0 yank register (the last-yank slot), so
  ;; a later delete — which passes YANK? false — cannot clobber the last yank.
  (define (vi-reg-store! reg text yank?)
    (let ([ht (vi-state-registers-ht *vi*)])
      (cond
        [(and (char? reg) (char<=? #\A reg #\Z))
         (let* ([lc (char-downcase reg)]
                [joined (string-append (hashtable-ref ht lc "") text)])
           (hashtable-set! ht lc joined)
           (hashtable-set! ht #\" joined))]
        [(and (char? reg) (not (char=? reg #\")))
         (hashtable-set! ht reg text)
         (hashtable-set! ht #\" text)]
        [else
         (hashtable-set! ht #\" text)])
      (when yank?
        (hashtable-set! ht #\0 text))))

  (define (vi-reg-fetch reg)
    (hashtable-ref (vi-state-registers-ht *vi*) reg ""))

  ;; Read the text of the currently-selected register for a paste, then reset the
  ;; selection back to the unnamed default. Returns #f when the unnamed register
  ;; is selected — the signal that the caller should fall back to the kill-ring
  ;; (today's p / P behaviour) — otherwise the register's text, downcasing an
  ;; A..Z selection to its lower-case sibling. The reset happens AFTER the read
  ;; so the next paste starts afresh from the unnamed default.
  (define (vi-take-paste-register!)
    (let ([reg (vi-state-register-char *vi*)])
      (vi-state-register-char-set! *vi* #\")
      (if (char=? reg #\")
          #f
          (vi-reg-fetch (if (char<=? #\A reg #\Z) (char-downcase reg) reg)))))

  ;; Hand the count typed before a keymap-dispatched command to the editor, then
  ;; reset it. The fall-through arm parks the pending count here (before zeroing
  ;; the running count) whenever vi-process-key returns #f, so editor.ss can read
  ;; it and repeat the bound command that many times -- 3x deletes three chars,
  ;; 5p pastes five times. Defaults to 1 when no count was given, and clears the
  ;; slot on the read so the next uncounted command runs exactly once. Mirrors
  ;; vi-take-paste-register!, which hands the pending register to a keymap paste.
  (define (vi-take-keymap-count!)
    (let ([n (vi-state-keymap-count *vi*)])
      (vi-state-keymap-count-set! *vi* 0)
      (max 1 n)))

  ;; ======================================================================
  ;; Operator application
  ;; ======================================================================

  ;; Delete range [start, end) from gap buffer, storing the deleted text under
  ;; the captured register REG (a delete is not a yank, so it never touches "0).
  (define (vi-delete-range! gb start end kr reg)
    (let* ([text (gap-buffer->string gb)]
           [len (string-length text)]
           [s (max 0 (min start len))]
           [e (max s (min end len))])
      (when (< s e)
        (let* ([deleted (substring text s e)]
               [new-text (string-append (substring text 0 s)
                                        (substring text e len))])
          (vi-reg-store! reg deleted #f)
          (kill-ring-push! kr deleted)
          (gap-buffer-set-from-string! gb new-text)
          (gap-buffer-move-cursor! gb (- (min s (string-length new-text))
                                         (gap-buffer-cursor-pos gb)))))))

  ;; Yank range [start, end) — copy to the captured register REG without
  ;; deleting; a yank also fills the "0 last-yank register (via YANK? true).
  (define (vi-yank-range! gb start end kr reg)
    (let* ([text (gap-buffer->string gb)]
           [len (string-length text)]
           [s (max 0 (min start len))]
           [e (max s (min end len))])
      (when (< s e)
        (let ([yanked (substring text s e)])
          (vi-reg-store! reg yanked #t)
          (kill-ring-push! kr yanked)))))

  ;; Apply operator to range. The selected register is captured BEFORE the
  ;; reset: vi-reset-state! sets register-char back to the unnamed default, so a
  ;; named "ayy / "add would otherwise lose its target before the range procs
  ;; read it. The captured register is threaded into the delete/yank procs.
  (define (vi-apply-operator! es gb kr start end)
    (let ([op (vi-state-operator *vi*)]
          [reg (vi-state-register-char *vi*)])
      (vi-reset-state!)
      (case op
        [(d)
         (when (vi-snapshot!-proc) ((vi-snapshot!-proc) es))
         (vi-delete-range! gb start end kr reg)]
        [(c)
         (when (vi-snapshot!-proc) ((vi-snapshot!-proc) es))
         (vi-delete-range! gb start end kr reg)
         (when (vi-enter-insert!-proc) ((vi-enter-insert!-proc) es))]
        [(y)
         (vi-yank-range! gb start end kr reg)])))

  ;; ======================================================================
  ;; Surround: add a delimiter pair around a span
  ;; ======================================================================

  ;; Commit a surround: wrap the span [start, end) of the buffer in the given
  ;; opening/closing strings. The span is clamped into [0, len] with start <=
  ;; end; an empty span or an unrecognised delimiter (open-str #f) is a safe
  ;; no-op that leaves the buffer and the undo stack untouched. The snapshot is
  ;; taken only inside the change guard, and the whole edit commits as one
  ;; gap-buffer-set-from-string! -- never character by character, so paredit
  ;; auto-pairing cannot fire mid-edit -- leaving the cursor on the new opening
  ;; delimiter.
  (define (vi-surround-add! es gb start end open-str close-str)
    (let* ([text (gap-buffer->string gb)]
           [len (string-length text)]
           [s (max 0 (min start len))]
           [e (max s (min end len))])
      (when (and open-str (< s e))
        (when (vi-snapshot!-proc) ((vi-snapshot!-proc) es))
        (let-values ([(nt np) (wrap-span text s e open-str close-str)])
          (gap-buffer-set-from-string! gb nt)
          (gap-buffer-move-cursor! gb (- np (gap-buffer-cursor-pos gb)))))))

  ;; Stage the ys operand and then the delimiter, both read directly from the
  ;; port so neither reaches the keymap. The operand is one of: a second
  ;; surround key (yss -- the whole line from first-non-blank to end); an i/a
  ;; text object (iw, i(, is/as, ...); or a motion, whose span copies the motion
  ;; arm's inclusive-end fixup. Any eof, non-character, unresolved operand or
  ;; unrecognised delimiter at any stage is a safe no-op.
  (define (vi-surround-read-add! es gb in-port text pos)
    (let ([k1 (read-key-event in-port)])
      (when (and (not (eof-object? k1)) (eq? (key-event-type k1) 'char))
        (let ([opc (key-event-value k1)])
          (let-values ([(start end)
                        (cond
                          ;; yss: the whole line, leading blanks preserved
                          [(char=? opc (surround-op-key))
                           (values (motion-first-non-blank text pos)
                                   (string-length text))]
                          ;; i/a text object (covers iw, i(, is/as, ...)
                          [(memv opc '(#\i #\a))
                           (let ([k2 (read-key-event in-port)])
                             (if (and (not (eof-object? k2))
                                      (eq? (key-event-type k2) 'char))
                                 (resolve-text-object text pos
                                   (char=? opc #\i) (key-event-value k2))
                                 (values #f #f)))]
                          ;; motion operand, inclusive-end for e E $ %
                          [(resolve-motion k1)
                           => (lambda (motion)
                                (let ([new-pos (run-motion-n motion text pos
                                                 (max 1 (vi-state-count *vi*)))])
                                  (values (min pos new-pos)
                                          (if (memv opc '(#\e #\E #\$ #\%))
                                              (+ (max pos new-pos) 1)
                                              (max pos new-pos)))))]
                          [else (values #f #f)])])
            ;; Drain the trailing delimiter from the port on EVERY operand form,
            ;; whether or not the operand resolved to a span. Reading it here is
            ;; what keeps it off the keymap: were it left in the port an
            ;; unresolved operand -- a garbage operand, or an iw/ib that finds no
            ;; span at end-of-buffer or outside any block -- would let the
            ;; delimiter surface as a fresh keystroke on the next loop pass and
            ;; self-insert or auto-pair. The edit itself stays gated on a
            ;; resolved span and a recognised delimiter, so an unresolved operand
            ;; remains a clean no-op.
            (let ([ch (vi-read-delim in-port)])
              (when (and start end ch)
                (let-values ([(open close) (surround-strings ch)])
                  (when open
                    (vi-surround-add! es gb start end open close))))))))))

  ;; ======================================================================
  ;; Surround: change one delimiter pair to another
  ;; ======================================================================

  ;; Strip the leading and trailing whitespace from the extracted inner content,
  ;; leaving any interior whitespace intact. This is the reversible open-mark
  ;; trim: it round-trips the single-space add (( x ) → x), while a close-mark,
  ;; alias or quote target skips it so the inner spaces are preserved.
  (define (surround-trim-inner str)
    (let* ([len (string-length str)]
           [start (let lp ([i 0])
                    (if (and (fx< i len) (char-whitespace? (string-ref str i)))
                        (lp (fx+ i 1)) i))]
           [end (let lp ([i len])
                  (if (and (fx> i start)
                           (char-whitespace? (string-ref str (fx- i 1))))
                      (lp (fx- i 1)) i))])
      (substring str start end)))

  ;; Change the enclosing OLD pair to the NEW pair. surround-locate finds the
  ;; pair of the old family (a missing pair, or a non-delimiter old char, yields
  ;; #f indices and no-ops); surround-strings gives the new pair with its spacing
  ;; baked in (an unrecognised new delimiter is #f and no-ops). The inner content
  ;; is trimmed of its outer whitespace only when the old char is an open-mark,
  ;; then re-emitted wrapped in the new strings. The snapshot is taken only
  ;; inside the change guard and the whole edit commits as one
  ;; gap-buffer-set-from-string!, leaving the cursor on the new opening delimiter.
  (define (vi-surround-change! es gb text pos old-ch new-ch)
    (let-values ([(oi ci) (surround-locate text pos old-ch)])
      (when (and oi ci)
        (let-values ([(no nc) (surround-strings new-ch)])
          (when no
            (let* ([raw (substring text (fx+ oi 1) ci)]
                   [inner (if (open-mark? old-ch) (surround-trim-inner raw) raw)])
              (when (vi-snapshot!-proc) ((vi-snapshot!-proc) es))
              (let ([new-text (string-append (substring text 0 oi) no inner nc
                                             (substring text (fx+ ci 1)
                                                        (string-length text)))])
                (gap-buffer-set-from-string! gb new-text)
                (gap-buffer-move-cursor! gb
                  (- oi (gap-buffer-cursor-pos gb))))))))))

  ;; ======================================================================
  ;; Surround: delete a delimiter pair
  ;; ======================================================================

  ;; Delete the enclosing pair of the requested family, keeping the inner
  ;; content. This is the change verb minus the re-emitted delimiters:
  ;; surround-locate finds the pair (a missing pair, a lone quote, or a
  ;; non-delimiter char all yield #f indices and no-op), the inner content is
  ;; trimmed of its outer whitespace only when the requested char is an
  ;; open-mark -- so ds( on ( x ) gives x while ds) on ( x ) preserves the inner
  ;; spaces -- and the prefix, inner and suffix are spliced with both delimiters
  ;; dropped. The snapshot is taken only inside the delete guard and the whole
  ;; edit commits as one gap-buffer-set-from-string!, leaving the cursor where
  ;; the opener was.
  (define (vi-surround-delete! es gb text pos ch)
    (let-values ([(oi ci) (surround-locate text pos ch)])
      (when (and oi ci)
        (let* ([raw (substring text (fx+ oi 1) ci)]
               [inner (if (open-mark? ch) (surround-trim-inner raw) raw)])
          (when (vi-snapshot!-proc) ((vi-snapshot!-proc) es))
          (let ([new-text (string-append (substring text 0 oi) inner
                                         (substring text (fx+ ci 1)
                                                    (string-length text)))])
            (gap-buffer-set-from-string! gb new-text)
            (gap-buffer-move-cursor! gb
              (- oi (gap-buffer-cursor-pos gb))))))))

  ;; ======================================================================
  ;; Search
  ;; ======================================================================

  (define (vi-search-forward text pos pattern)
    (let ([plen (string-length pattern)]
          [tlen (string-length text)])
      (and (> plen 0)
           (let lp ([i (+ pos 1)])
             (cond
               [(> (+ i plen) tlen)
                ;; Wrap around
                (let lp2 ([i 0])
                  (cond
                    [(> (+ i plen) (+ pos 1)) #f]
                    [(string=? (substring text i (+ i plen)) pattern) i]
                    [else (lp2 (+ i 1))]))]
               [(string=? (substring text i (+ i plen)) pattern) i]
               [else (lp (+ i 1))])))))

  (define (vi-search-backward text pos pattern)
    (let ([plen (string-length pattern)]
          [tlen (string-length text)])
      (and (> plen 0)
           (let lp ([i (- pos 1)])
             (cond
               [(< i 0)
                ;; Wrap around
                (let lp2 ([i (- tlen plen)])
                  (cond
                    [(< i pos) #f]
                    [(string=? (substring text i (+ i plen)) pattern) i]
                    [else (lp2 (- i 1))]))]
               [(and (<= (+ i plen) tlen)
                     (string=? (substring text i (+ i plen)) pattern)) i]
               [else (lp (- i 1))])))))

  ;; Read a search pattern from the user (mini-prompt at current position)
  ;; Returns the pattern string or "" if cancelled
  (define (vi-read-search-pattern in-port out-port direction)
    (let ([prompt-ch (if (eq? direction 'forward) "/" "?")])
      (display prompt-ch out-port)
      (flush-output-port out-port)
      (let lp ([chars '()])
        (let ([evt (read-key-event in-port)])
          (cond
            [(eof-object? evt) ""]
            [(and (eq? (key-event-type evt) 'special)
                  (eq? (key-event-value evt) 'return))
             (list->string (reverse chars))]
            [(and (eq? (key-event-type evt) 'special)
                  (eq? (key-event-value evt) 'escape))
             ""]
            [(and (eq? (key-event-type evt) 'special)
                  (eq? (key-event-value evt) 'backspace))
             (if (null? chars)
                 ""
                 (begin
                   (display "\b \b" out-port)
                   (flush-output-port out-port)
                   (lp (cdr chars))))]
            [(and (eq? (key-event-type evt) 'char)
                  (char? (key-event-value evt)))
             (let ([ch (key-event-value evt)])
               (display ch out-port)
               (flush-output-port out-port)
               (lp (cons ch chars)))]
            [else (lp chars)])))))

  ;; Word under cursor (for * and #)
  (define (word-at-pos text pos)
    (let ([len (string-length text)])
      (if (or (>= pos len) (not (word-char? (string-ref text pos))))
          #f
          (let ([start (let lp ([i pos])
                         (if (and (> i 0) (word-char? (string-ref text (- i 1))))
                             (lp (- i 1)) i))]
                [end (let lp ([i pos])
                       (if (and (< i len) (word-char? (string-ref text i)))
                           (lp (+ i 1)) i))])
            (substring text start end)))))

  ;; ======================================================================
  ;; Execute motion N times, return final position
  ;; ======================================================================

  (define (run-motion-n motion text pos n)
    (let lp ([i 0] [p pos])
      (if (>= i n) p
          (let ([np (motion text p)])
            (if np (lp (+ i 1) np) p)))))

  ;; ======================================================================
  ;; Resolve a motion key event to a motion function
  ;; Returns: motion-fn or #f
  ;; ======================================================================

  (define (resolve-motion evt)
    (and (eq? (key-event-type evt) 'char)
         (let ([ch (key-event-value evt)])
           (case ch
             [(#\w) motion-w]
             [(#\W) motion-W]
             [(#\b) motion-b]
             [(#\B) motion-B]
             [(#\e) motion-e]
             [(#\E) motion-E]
             [(#\0) (lambda (text pos)
                      (let-values ([(start end) (current-line-bounds text pos)])
                        start))]
             [(#\^) motion-first-non-blank]
             [(#\$) (lambda (text pos)
                      (let-values ([(start end) (current-line-bounds text pos)])
                        (if (> end start) (- end 1) start)))]
             [(#\h) (lambda (text pos)
                      (if (> pos 0) (- pos 1) #f))]
             [(#\l) (lambda (text pos)
                      (let ([len (string-length text)])
                        (if (< pos (- len 1)) (+ pos 1) #f)))]
             [(#\%) (lambda (text pos) (motion-match-paren text pos))]
             [else #f]))))

  ;; Resolve a text object: i/a prefix + object key
  ;; Returns: (values start end) or (values #f #f)
  (define (resolve-text-object text pos inner? obj-char)
    (case obj-char
      [(#\w) (if inner? (text-obj-inner-word text pos)
                        (text-obj-around-word text pos))]
      [(#\W) (if inner? (text-obj-inner-WORD text pos)
                        (text-obj-around-WORD text pos))]
      [(#\( #\) #\b)
       (if inner? (text-obj-inner-pair text pos #\( #\))
                  (text-obj-around-pair text pos #\( #\)))]
      [(#\[ #\])
       (if inner? (text-obj-inner-pair text pos #\[ #\])
                  (text-obj-around-pair text pos #\[ #\]))]
      [(#\{ #\})
       (if inner? (text-obj-inner-pair text pos #\{ #\})
                  (text-obj-around-pair text pos #\{ #\}))]
      [(#\" ) (if inner? (text-obj-inner-pair text pos #\" #\")
                         (text-obj-around-pair text pos #\" #\"))]
      [(#\') (if inner? (text-obj-inner-pair text pos #\' #\')
                        (text-obj-around-pair text pos #\' #\'))]
      [(#\`) (if inner? (text-obj-inner-pair text pos #\` #\`)
                        (text-obj-around-pair text pos #\` #\`))]
      [(#\s) (resolve-sexp-object text pos inner?)]
      [else (values #f #f)]))

  ;; ======================================================================
  ;; Main state machine
  ;; ======================================================================

  ;; Returns #t if the key was handled, #f to fall through to keymap
  (define (vi-process-key es evt in-port out-port gb kr)
    (let ([type (key-event-type evt)]
          [val (key-event-value evt)]
          [mods (key-event-mods evt)])
      (case (vi-state-pending *vi*)
        ;; ------- REPLACE: waiting for replacement char -------
        [(replace)
         (when (and (eq? type 'char) (char? val))
           (let ([text (gap-buffer->string gb)]
                 [pos (gap-buffer-cursor-pos gb)])
             (when (< pos (string-length text))
               (when (vi-snapshot!-proc) ((vi-snapshot!-proc) es))
               (let ([cnt (max 1 (vi-state-count *vi*))])
                 (let lp ([i 0])
                   (when (and (< i cnt)
                              (< (+ pos i) (string-length (gap-buffer->string gb))))
                     (gap-buffer-delete-forward! gb)
                     (gap-buffer-insert! gb val)
                     (lp (+ i 1))))
                 ;; Move back to last replaced char
                 (let ([final-pos (gap-buffer-cursor-pos gb)])
                   (when (> final-pos 0)
                     (gap-buffer-move-cursor! gb -1)))))))
         (vi-reset-state!)
         #t]

        ;; ------- FIND: waiting for target char -------
        [(find)
         (when (and (eq? type 'char) (char? val))
           (let* ([text (gap-buffer->string gb)]
                  [pos (gap-buffer-cursor-pos gb)]
                  [cnt (max 1 (vi-state-count *vi*))]
                  [motion (case (vi-state-find-dir *vi*)
                            [(f) (lambda (t p) (motion-find-char t p val))]
                            [(F) (lambda (t p) (motion-find-char-back t p val))]
                            [(t) (lambda (t p) (motion-till-char t p val))]
                            [(T) (lambda (t p) (motion-till-char-back t p val))])]
                  [new-pos (run-motion-n motion text pos cnt)])
             (vi-state-last-find-set! *vi* (cons (vi-state-find-dir *vi*) val))
             (if (vi-state-operator *vi*)
                 ;; Apply operator to range
                 (let ([start (min pos new-pos)]
                       [end (+ (max pos new-pos) 1)])
                   (vi-apply-operator! es gb kr start end))
                 ;; Just move cursor
                 (when (and new-pos (not (= new-pos pos)))
                   (gap-buffer-move-cursor! gb (- new-pos pos))))))
         (when (not (vi-state-operator *vi*))
           (vi-reset-state!))
         (when (vi-state-operator *vi*) (vi-reset-state!))
         #t]

        ;; ------- REGISTER: waiting for register char -------
        [(register)
         (when (and (eq? type 'char) (char? val))
           (vi-state-register-char-set! *vi* val))
         (vi-state-pending-set! *vi* 'normal)
         #t]

        ;; ------- OPERATOR PENDING or NORMAL with possible count -------
        [else
         (cond
           ;; Escape always cancels
           [(and (eq? type 'special) (eq? val 'escape))
            (cond
              [(vi-state-vis-mode *vi*)
               (vi-state-vis-mode-set! *vi* #f)
               (vi-reset-state!)]
              [(vi-state-operator *vi*) (vi-reset-state!)]
              [else (vi-reset-state!)])
            #t]

           ;; Digit: count accumulation (1-9 start, 0 continues)
           [(and (eq? type 'char) (char? val) (char-numeric? val)
                 (or (not (char=? val #\0)) (> (vi-state-count *vi*) 0)))
            (vi-state-count-set! *vi* (+ (* (vi-state-count *vi*) 10)
                              (- (char->integer val) (char->integer #\0))))
            #t]

           ;; Operator keys: d c y. Skipped while a visual selection is active so
           ;; d/c/y fall through to the visual-operator arm below (which deletes
           ;; or changes the selection); otherwise a visual d would start an
           ;; operator-pending instead of acting on the selection.
           [(and (eq? type 'char) (memv val '(#\d #\c #\y))
                 (not (vi-state-operator *vi*))
                 (not (vi-state-vis-mode *vi*)))
            (vi-state-operator-set! *vi* (case val [(#\d) 'd] [(#\c) 'c] [(#\y) 'y]))
            ;; Stash the prefix count as the operator's count and reset the
            ;; running count, so the following motion's digits accumulate a
            ;; FRESH motion count. The motion arm then multiplies the two
            ;; (2d3w = 2x3 = 6 words) instead of the old digit-concatenation
            ;; (2 then 3 -> 23). A bare operator (no prefix) stashes 0, which
            ;; the arm reads as 1, so d3w still deletes 3.
            (vi-state-operator-count-set! *vi* (vi-state-count *vi*))
            (vi-state-count-set! *vi* 0)
            (vi-state-pending-set! *vi* 'operator)
            #t]

           ;; Double operator: dd cc yy → the CURRENT logical line (never the
           ;; whole buffer). The span is derived from the shared line-bounds
           ;; helper and shaped per operator:
           ;;   dd  the line plus its trailing newline; on the final line take the
           ;;       PRECEDING newline instead (so no blank line is left behind);
           ;;       a single-line buffer has no newline and is simply emptied.
           ;;   cc  the line's text only -- the newline stays, so the line
           ;;       survives as an empty line for the insert that follows.
           ;;   yy  the line's text only (no newline), so a charwise p re-inserts
           ;;       just the line -- matching the current single-line yy/p.
           [(and (eq? type 'char) (vi-state-operator *vi*)
                 (case (vi-state-operator *vi*)
                   [(d) (char=? val #\d)]
                   [(c) (char=? val #\c)]
                   [(y) (char=? val #\y)]
                   [else #f]))
            (let* ([text (gap-buffer->string gb)]
                   [len (string-length text)]
                   [pos (gap-buffer-cursor-pos gb)]
                   [op (vi-state-operator *vi*)])
              (let-values ([(ls le) (current-line-bounds text pos)])
                (let-values ([(start end)
                              (case op
                                ;; cc / yy: the line's text only (no newline).
                                ;; cc then keeps the now-empty line for the
                                ;; insert; yy copies just the line content, so a
                                ;; charwise p re-inserts the line's text -- the
                                ;; current single-line yy/p behaviour carried to
                                ;; multi-line buffers.
                                [(c y) (values ls le)]
                                ;; dd: the line plus a bounding newline so the
                                ;; line is removed outright.
                                [else (cond
                                        [(< le len) (values ls (+ le 1))]
                                        [(> ls 0)   (values (- ls 1) le)]
                                        [else       (values ls le)])])])
                  (vi-apply-operator! es gb kr start end))))
            #t]

           ;; Surround operator: ys (the surround key after y). The change and
           ;; delete operators reach this arm too, so the operator symbol is
           ;; captured before the reset -- their clauses land here later. Guarded
           ;; on an operator pending AND the surround key, so yiw, dw, ci( and
           ;; the other operator forms are untouched.
           [(and (vi-state-operator *vi*) (eq? type 'char)
                 (char=? val (surround-op-key)))
            (let ([op (vi-state-operator *vi*)])
              (case op
                [(y) (vi-surround-read-add! es gb in-port
                       (gap-buffer->string gb) (gap-buffer-cursor-pos gb))]
                ;; cs{old}{new}: read the two raw delimiters left-to-right. let*
                ;; (NOT let) is load-bearing -- Chez leaves a plain let's init
                ;; order unspecified (right-to-left in practice), which would bind
                ;; old to the SECOND char and new to the FIRST, so cs)] would hunt
                ;; for a [] that is not there and no-op on every input. let* forces
                ;; the sequential reads: old = the first delimiter, new = the
                ;; second. An eof or non-char at either read leaves old or new #f
                ;; and the change no-ops.
                [(c) (let* ([old (vi-read-delim in-port)]
                            [new (vi-read-delim in-port)])
                       (when (and old new)
                         (vi-surround-change! es gb (gap-buffer->string gb)
                           (gap-buffer-cursor-pos gb) old new)))]
                ;; ds{char}: read one raw delimiter naming the family to delete;
                ;; an eof or non-char leaves ch #f and the delete no-ops.
                [(d) (let ([ch (vi-read-delim in-port)])
                       (when ch
                         (vi-surround-delete! es gb (gap-buffer->string gb)
                           (gap-buffer-cursor-pos gb) ch)))]))
            (vi-reset-state!)
            #t]

           ;; Text object: i/a prefix in operator-pending
           [(and (vi-state-operator *vi*) (eq? type 'char) (memv val '(#\i #\a)))
            ;; Read next char for object type
            (let ([next (read-key-event in-port)])
              (when (and (not (eof-object? next))
                         (eq? (key-event-type next) 'char))
                (let* ([text (gap-buffer->string gb)]
                       [pos (gap-buffer-cursor-pos gb)])
                  (let-values ([(start end)
                                (resolve-text-object text pos
                                  (char=? val #\i)
                                  (key-event-value next))])
                    (when (and start end)
                      (vi-apply-operator! es gb kr start end))))))
            (when (vi-state-operator *vi*) (vi-reset-state!))
            #t]

           ;; Motion keys (work standalone or with operator)
           [(and (eq? type 'char)
                 (resolve-motion evt))
            => (lambda (motion)
                 (let* ([text (gap-buffer->string gb)]
                        [len (string-length text)]
                        [pos (gap-buffer-cursor-pos gb)]
                        [op (vi-state-operator *vi*)]
                        ;; cw / cW act as ce / cE: with the change operator and
                        ;; a w/W motion on a non-blank, vim changes to the END
                        ;; of the current word (not the start of the next), and
                        ;; so keeps the trailing whitespace. Substitute the e/E
                        ;; motion and take its inclusive end. dw / yw keep w.
                        [cw->ce? (and (eq? op 'c) (memv val '(#\w #\W))
                                      (< pos len)
                                      (not (char-whitespace? (string-ref text pos))))]
                        [eff-key (cond [(not cw->ce?) val]
                                       [(char=? val #\w) #\e]
                                       [else #\E])]
                        [eff-motion (cond [(not cw->ce?) motion]
                                          [(char=? val #\w) motion-e]
                                          [else motion-E])]
                        ;; Operator count (stashed on operator entry) multiplies
                        ;; the motion count: 2d3w = 6. Outside operator context
                        ;; the stash is 0 -> 1, so a plain motion count is
                        ;; unchanged.
                        [cnt (* (max 1 (vi-state-operator-count *vi*))
                                (max 1 (vi-state-count *vi*)))]
                        [new-pos (run-motion-n eff-motion text pos cnt)])
                   (cond
                     [op
                      ;; Apply operator over range
                      (when new-pos
                        (let* ([start (min pos new-pos)]
                               [raw-end (if (memv eff-key '(#\e #\E #\$ #\%))
                                            (+ (max pos new-pos) 1)
                                            (max pos new-pos))]
                               ;; w/W-motion operator clamp: never delete across
                               ;; the current line's trailing newline. Clamp the
                               ;; end back to the line end so dw / cw on the last
                               ;; word of a line stop at end-of-line and leave the
                               ;; newline (and the next line) intact. Operator
                               ;; context only -- the plain w/W cursor motion
                               ;; above is untouched. Indices stay within [0, len].
                               [end (if (memv val '(#\w #\W))
                                        (let-values ([(ls le) (current-line-bounds text pos)])
                                          (min raw-end le))
                                        raw-end)]
                               [s (max 0 (min start len))]
                               [e (max s (min end len))])
                          (vi-apply-operator! es gb kr s e)))]
                     [(vi-state-vis-mode *vi*)
                      ;; Extend visual selection
                      (when new-pos
                        (gap-buffer-move-cursor! gb (- new-pos pos)))]
                     [else
                      ;; Simple motion
                      (when (and new-pos (not (= new-pos pos)))
                        (gap-buffer-move-cursor! gb (- new-pos pos)))]))
                 (when (not (vi-state-vis-mode *vi*)) (vi-reset-state!))
                 #t)]

           ;; j/k: history navigation
           [(and (eq? type 'char) (char=? val #\j) (not (vi-state-operator *vi*)))
            (when (vi-history-next!-proc) ((vi-history-next!-proc) es))
            (vi-reset-state!)
            #t]
           [(and (eq? type 'char) (char=? val #\k) (not (vi-state-operator *vi*)))
            (when (vi-history-prev!-proc) ((vi-history-prev!-proc) es))
            (vi-reset-state!)
            #t]

           ;; G: last line — the first non-blank of the final logical line, via
           ;; the shared line-bounds helper (not raw buffer length).
           [(and (eq? type 'char) (char=? val #\G) (not (vi-state-operator *vi*)))
            (let* ([text (gap-buffer->string gb)]
                   [len (string-length text)]
                   [pos (gap-buffer-cursor-pos gb)]
                   [target (motion-first-non-blank text len)])
              (gap-buffer-move-cursor! gb (- target pos)))
            (vi-reset-state!)
            #t]

           ;; gg: first line — the first non-blank of the first logical line, via
           ;; the shared line-bounds helper (not raw buffer offset 0).
           [(and (eq? type 'char) (char=? val #\g) (not (vi-state-operator *vi*)))
            (let ([next (read-key-event in-port)])
              (when (and (not (eof-object? next))
                         (eq? (key-event-type next) 'char)
                         (char=? (key-event-value next) #\g))
                (let* ([text (gap-buffer->string gb)]
                       [pos (gap-buffer-cursor-pos gb)]
                       [target (motion-first-non-blank text 0)])
                  (gap-buffer-move-cursor! gb (- target pos)))))
            (vi-reset-state!)
            #t]

           ;; r: replace char
           [(and (eq? type 'char) (char=? val #\r) (not (vi-state-operator *vi*)))
            (vi-state-pending-set! *vi* 'replace)
            #t]

           ;; f/F/t/T: find char
           [(and (eq? type 'char) (memv val '(#\f #\F #\t #\T))
                 (= mods 0))
            (vi-state-find-dir-set! *vi* (case val
                                [(#\f) 'f] [(#\F) 'F]
                                [(#\t) 't] [(#\T) 'T]))
            (vi-state-pending-set! *vi* 'find)
            #t]

           ;; ; repeat last find, , reverse last find
           [(and (eq? type 'char) (char=? val #\;) (vi-state-last-find *vi*))
            (let* ([text (gap-buffer->string gb)]
                   [pos (gap-buffer-cursor-pos gb)]
                   [dir (car (vi-state-last-find *vi*))]
                   [ch (cdr (vi-state-last-find *vi*))]
                   [cnt (max 1 (vi-state-count *vi*))]
                   [motion (case dir
                             [(f) (lambda (t p) (motion-find-char t p ch))]
                             [(F) (lambda (t p) (motion-find-char-back t p ch))]
                             [(t) (lambda (t p) (motion-till-char t p ch))]
                             [(T) (lambda (t p) (motion-till-char-back t p ch))])]
                   [new-pos (run-motion-n motion text pos cnt)])
              (when (and new-pos (not (= new-pos pos)))
                (gap-buffer-move-cursor! gb (- new-pos pos))))
            (vi-reset-state!)
            #t]
           [(and (eq? type 'char) (char=? val #\,) (vi-state-last-find *vi*))
            (let* ([text (gap-buffer->string gb)]
                   [pos (gap-buffer-cursor-pos gb)]
                   [dir (car (vi-state-last-find *vi*))]
                   [ch (cdr (vi-state-last-find *vi*))]
                   [cnt (max 1 (vi-state-count *vi*))]
                   [rev-dir (case dir
                              [(f) 'F] [(F) 'f] [(t) 'T] [(T) 't])]
                   [motion (case rev-dir
                             [(f) (lambda (t p) (motion-find-char t p ch))]
                             [(F) (lambda (t p) (motion-find-char-back t p ch))]
                             [(t) (lambda (t p) (motion-till-char t p ch))]
                             [(T) (lambda (t p) (motion-till-char-back t p ch))])]
                   [new-pos (run-motion-n motion text pos cnt)])
              (when (and new-pos (not (= new-pos pos)))
                (gap-buffer-move-cursor! gb (- new-pos pos))))
            (vi-reset-state!)
            #t]

           ;; ~ toggle case
           [(and (eq? type 'char) (char=? val #\~) (not (vi-state-operator *vi*)))
            (let* ([pos (gap-buffer-cursor-pos gb)]
                   [len (gap-buffer-length gb)]
                   [cnt (min (max 1 (vi-state-count *vi*)) (- len pos))])
              (when (> cnt 0)
                (when (vi-snapshot!-proc) ((vi-snapshot!-proc) es))
                (let ([text (gap-buffer->string gb)])
                  (let lp ([i 0])
                    (when (< i cnt)
                      (let* ([p (+ pos i)]
                             [ch (string-ref text p)]
                             [toggled (if (char-upper-case? ch)
                                         (char-downcase ch)
                                         (char-upcase ch))])
                        (gap-buffer-delete-forward! gb)
                        (gap-buffer-insert! gb toggled)
                        (lp (+ i 1))))))))
            (vi-reset-state!)
            #t]

           ;; J join lines
           [(and (eq? type 'char) (char=? val #\J) (not (vi-state-operator *vi*)))
            (let* ([text (gap-buffer->string gb)]
                   [pos (gap-buffer-cursor-pos gb)]
                   [len (string-length text)]
                   ;; Find next newline
                   [nl (let lp ([i pos])
                         (cond [(>= i len) #f]
                               [(char=? (string-ref text i) #\newline) i]
                               [else (lp (+ i 1))]))])
              (when nl
                (when (vi-snapshot!-proc) ((vi-snapshot!-proc) es))
                ;; Remove newline and surrounding whitespace, insert single space
                (let* ([start (let lp ([i nl])
                                (if (and (> i 0)
                                         (char-whitespace? (string-ref text (- i 1)))
                                         (not (char=? (string-ref text (- i 1)) #\newline)))
                                    (lp (- i 1)) i))]
                       [end (let lp ([i (+ nl 1)])
                              (if (and (< i len)
                                       (char-whitespace? (string-ref text i))
                                       (not (char=? (string-ref text i) #\newline)))
                                  (lp (+ i 1)) i))]
                       [new-text (string-append
                                   (substring text 0 start)
                                   " "
                                   (substring text end len))])
                  (gap-buffer-set-from-string! gb new-text)
                  (gap-buffer-move-cursor! gb (- start (gap-buffer-cursor-pos gb))))))
            (vi-reset-state!)
            #t]

           ;; u undo
           [(and (eq? type 'char) (char=? val #\u) (not (vi-state-operator *vi*)))
            (let ([cnt (max 1 (vi-state-count *vi*))])
              (let lp ([i 0])
                (when (< i cnt)
                  (when (vi-undo!-proc) ((vi-undo!-proc) es))
                  (lp (+ i 1)))))
            (vi-reset-state!)
            #t]

           ;; Ctrl+R redo (in normal mode)
           [(and (eq? type 'ctrl) (eqv? val #\r))
            (let ([cnt (max 1 (vi-state-count *vi*))])
              (let lp ([i 0])
                (when (< i cnt)
                  (when (vi-redo!-proc) ((vi-redo!-proc) es))
                  (lp (+ i 1)))))
            (vi-reset-state!)
            #t]

           ;; " register selection
           [(and (eq? type 'char) (char=? val #\") (not (vi-state-operator *vi*)))
            (vi-state-pending-set! *vi* 'register)
            #t]

           ;; v/V visual mode
           [(and (eq? type 'char) (char=? val #\v) (not (vi-state-operator *vi*)))
            (if (eq? (vi-state-vis-mode *vi*) 'char)
                (vi-state-vis-mode-set! *vi* #f)  ;; toggle off
                (begin
                  (vi-state-vis-mode-set! *vi* 'char)
                  (vi-state-vis-anchor-set! *vi* (gap-buffer-cursor-pos gb))))
            (vi-reset-state!)
            #t]
           [(and (eq? type 'char) (char=? val #\V) (not (vi-state-operator *vi*)))
            (if (eq? (vi-state-vis-mode *vi*) 'line)
                (vi-state-vis-mode-set! *vi* #f)
                (begin
                  (vi-state-vis-mode-set! *vi* 'line)
                  (vi-state-vis-anchor-set! *vi* (gap-buffer-cursor-pos gb))))
            (vi-reset-state!)
            #t]

           ;; Visual mode operator application
           [(and (vi-state-vis-mode *vi*) (eq? type 'char) (memv val '(#\d #\x #\c #\y #\s)))
            (let* ([pos (gap-buffer-cursor-pos gb)]
                   [start (min pos (vi-state-vis-anchor *vi*))]
                   [end (+ (max pos (vi-state-vis-anchor *vi*)) 1)]
                   [text (gap-buffer->string gb)]
                   [len (string-length text)])
              ;; For line visual, extend to WHOLE logical lines: from the start
              ;; of the line holding the lower endpoint to the end of the line
              ;; holding the upper endpoint, plus a bounding newline (the trailing
              ;; one, or the preceding one when the final line is selected) so Vd
              ;; removes the line(s) without leaving a blank line behind. Reuses
              ;; the shared line-bounds helper so V agrees with dd on the line.
              (let-values ([(s e)
                            (if (eq? (vi-state-vis-mode *vi*) 'line)
                                (let-values ([(lo-start lo-end) (current-line-bounds text start)]
                                             [(hi-start hi-end)
                                              (current-line-bounds text
                                                (max pos (vi-state-vis-anchor *vi*)))])
                                  (cond
                                    [(< hi-end len) (values lo-start (+ hi-end 1))]
                                    [(> lo-start 0) (values (- lo-start 1) hi-end)]
                                    [else           (values lo-start hi-end)]))
                                (values start (min end len)))])
                (vi-state-operator-set! *vi* (case val
                                    [(#\d #\x) 'd] [(#\c #\s) 'c] [(#\y) 'y]))
                (vi-state-vis-mode-set! *vi* #f)
                (vi-apply-operator! es gb kr s e)))
            #t]

           ;; Visual-mode surround: capital S wraps the current selection in a
           ;; delimiter pair. Lowercase s stays the visual substitute above, so
           ;; S is free. The span comes from vi-visual-range; an unrecognised
           ;; delimiter leaves the buffer untouched.
           [(and (vi-state-vis-mode *vi*) (eq? type 'char)
                 (char=? val (surround-visual-key)))
            (let ([span (vi-visual-range gb)]
                  [ch (vi-read-delim in-port)])
              ;; Clear the selection once, on every path: an aborted S (an
              ;; unrecognised or truncated delimiter) leaves visual mode exactly
              ;; as a resolved one does. span is already captured above, so
              ;; clearing here does not disturb the wrap.
              (vi-state-vis-mode-set! *vi* #f)
              (when (and span ch)
                (let-values ([(open close) (surround-strings ch)])
                  (when open
                    (vi-surround-add! es gb (car span) (cdr span) open close)))))
            (vi-reset-state!)
            #t]

           ;; Visual-mode i/a text object: read the following object key and
           ;; set the visual selection to the resolved span. This is what makes
           ;; vis / vas (and the other visual i/a objects) select -- the visual
           ;; operator arm above only applies an operator to the existing
           ;; anchor..cursor span and has no i/a entry of its own. Guarded on
           ;; vis-mode so a normal-mode i/a still falls through to the keymap
           ;; (insert entry), and on the absence of an operator so the
           ;; operator-pending i/a path is untouched.
           [(and (vi-state-vis-mode *vi*) (eq? type 'char) (memv val '(#\i #\a))
                 (not (vi-state-operator *vi*)))
            (let ([next (read-key-event in-port)])
              (when (and (not (eof-object? next))
                         (eq? (key-event-type next) 'char))
                (let* ([text (gap-buffer->string gb)]
                       [pos (gap-buffer-cursor-pos gb)])
                  (let-values ([(start end)
                                (resolve-text-object text pos
                                  (char=? val #\i)
                                  (key-event-value next))])
                    (when (and start end (< start end))
                      ;; anchor = span start, cursor = span end - 1, so
                      ;; vi-visual-range resolves to exactly (start . end).
                      (vi-state-vis-anchor-set! *vi* start)
                      (gap-buffer-move-cursor! gb
                        (- (- end 1) (gap-buffer-cursor-pos gb))))))))
            (vi-reset-state!)
            #t]

           ;; / ? search
           [(and (eq? type 'char) (or (char=? val #\/) (char=? val #\?))
                 (not (vi-state-operator *vi*)))
            (let* ([dir (if (char=? val #\/) 'forward 'backward)]
                   [pattern (vi-read-search-pattern in-port out-port dir)])
              (when (> (string-length pattern) 0)
                (vi-state-search-pat-set! *vi* pattern)
                (vi-state-search-dir-set! *vi* dir)
                (let* ([text (gap-buffer->string gb)]
                       [pos (gap-buffer-cursor-pos gb)]
                       [found (if (eq? dir 'forward)
                                  (vi-search-forward text pos pattern)
                                  (vi-search-backward text pos pattern))])
                  (when found
                    (gap-buffer-move-cursor! gb (- found pos))))))
            (vi-reset-state!)
            #t]

           ;; n/N next/prev search result
           [(and (eq? type 'char) (char=? val #\n) (not (vi-state-operator *vi*))
                 (> (string-length (vi-state-search-pat *vi*)) 0))
            (let* ([text (gap-buffer->string gb)]
                   [pos (gap-buffer-cursor-pos gb)]
                   [found (if (eq? (vi-state-search-dir *vi*) 'forward)
                              (vi-search-forward text pos (vi-state-search-pat *vi*))
                              (vi-search-backward text pos (vi-state-search-pat *vi*)))])
              (when found
                (gap-buffer-move-cursor! gb (- found pos))))
            (vi-reset-state!)
            #t]
           [(and (eq? type 'char) (char=? val #\N) (not (vi-state-operator *vi*))
                 (> (string-length (vi-state-search-pat *vi*)) 0))
            (let* ([text (gap-buffer->string gb)]
                   [pos (gap-buffer-cursor-pos gb)]
                   [found (if (eq? (vi-state-search-dir *vi*) 'forward)
                              (vi-search-backward text pos (vi-state-search-pat *vi*))
                              (vi-search-forward text pos (vi-state-search-pat *vi*)))])
              (when found
                (gap-buffer-move-cursor! gb (- found pos))))
            (vi-reset-state!)
            #t]

           ;; * search word under cursor forward
           [(and (eq? type 'char) (char=? val #\*) (not (vi-state-operator *vi*)))
            (let* ([text (gap-buffer->string gb)]
                   [pos (gap-buffer-cursor-pos gb)]
                   [word (word-at-pos text pos)])
              (when word
                (vi-state-search-pat-set! *vi* word)
                (vi-state-search-dir-set! *vi* 'forward)
                (let ([found (vi-search-forward text pos word)])
                  (when found
                    (gap-buffer-move-cursor! gb (- found pos))))))
            (vi-reset-state!)
            #t]

           ;; # search word under cursor backward
           [(and (eq? type 'char) (char=? val #\#) (not (vi-state-operator *vi*)))
            (let* ([text (gap-buffer->string gb)]
                   [pos (gap-buffer-cursor-pos gb)]
                   [word (word-at-pos text pos)])
              (when word
                (vi-state-search-pat-set! *vi* word)
                (vi-state-search-dir-set! *vi* 'backward)
                (let ([found (vi-search-backward text pos word)])
                  (when found
                    (gap-buffer-move-cursor! gb (- found pos))))))
            (vi-reset-state!)
            #t]

           ;; m set mark
           [(and (eq? type 'char) (char=? val #\m) (not (vi-state-operator *vi*)))
            (let ([next (read-key-event in-port)])
              (when (and (not (eof-object? next))
                         (eq? (key-event-type next) 'char)
                         (char? (key-event-value next)))
                (hashtable-set! (vi-state-marks-ht *vi*) (key-event-value next)
                                (gap-buffer-cursor-pos gb))))
            (vi-reset-state!)
            #t]

           ;; ' or ` jump to mark
           [(and (eq? type 'char) (or (char=? val #\') (char=? val #\`))
                 (not (vi-state-operator *vi*)))
            (let ([next (read-key-event in-port)])
              (when (and (not (eof-object? next))
                         (eq? (key-event-type next) 'char)
                         (char? (key-event-value next)))
                (let ([mark-pos (hashtable-ref (vi-state-marks-ht *vi*)
                                  (key-event-value next) #f)])
                  (when mark-pos
                    (let ([pos (gap-buffer-cursor-pos gb)]
                          [len (gap-buffer-length gb)])
                      (gap-buffer-move-cursor! gb
                        (- (min mark-pos len) pos)))))))
            (vi-reset-state!)
            #t]

           ;; . repeat last edit — delegate to the replay hook with the count
           ;; typed before the dot. The count is read BEFORE vi-reset-state!
           ;; zeroes it (the u / C-r read-before-reset idiom); a raw 0 means no
           ;; count was given. A #f hook (no engine wired) makes this a safe
           ;; no-op that just resets — pressing . never crashes.
           [(and (eq? type 'char) (char=? val #\.) (not (vi-state-operator *vi*)))
            (let ([cnt (vi-state-count *vi*)])
              (when (vi-replay!-proc) ((vi-replay!-proc) es cnt)))
            (vi-reset-state!)
            #t]

           ;; Not handled by vi — fall through to the keymap.  Clear the transient
           ;; operator / count / pending / find state, but LEAVE register-char: a
           ;; keymap paste (cmd-paste-after / cmd-paste-before) still reads the
           ;; pending "reg selection through vi-take-paste-register!, which resets
           ;; it to the unnamed default on that read.  A blanket vi-reset-state!
           ;; here would zero the register before the paste ran, stranding "ap /
           ;; "aP back on the kill-ring instead of the named register.
           [else
            ;; Surface the pending count to the editor's keymap dispatch BEFORE
            ;; zeroing the running count: 3x / 5p / 4a fall through here, and
            ;; editor.ss reads the parked count via vi-take-keymap-count! to
            ;; repeat the bound command. register-char is still left intact so a
            ;; keymap paste (cmd-paste-after / -before) reads the pending "reg.
            (vi-state-keymap-count-set! *vi* (vi-state-count *vi*))
            (vi-state-count-set! *vi* 0)
            (vi-state-operator-set! *vi* #f)
            (vi-state-pending-set! *vi* 'normal)
            (vi-state-find-dir-set! *vi* #f)
            (vi-state-recording-edit-set! *vi* #f)
            #f])])))

) ; end library
