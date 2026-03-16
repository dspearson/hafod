;;; (hafod editor vi) -- Full vim emulation: motions, operators, text objects,
;;; visual mode, search, registers, marks, count prefixes, repeat.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor vi)
  (export
    ;; Main entry point for normal-mode key processing
    vi-process-key
    ;; State management
    vi-reset-state! vi-reset-session!
    ;; State accessors (for rendering)
    vi-visual-mode vi-visual-anchor vi-visual-end
    vi-mode-indicator vi-pending-state
    ;; Search
    vi-search-pattern vi-search-direction
    ;; For editor.ss integration (parameters — call with value to set)
    vi-snapshot!-proc vi-undo!-proc vi-redo!-proc
    vi-enter-insert!-proc vi-enter-normal!-proc
    vi-history-prev!-proc vi-history-next!-proc
    vi-submit!-proc)
  (import (chezscheme)
          (hafod editor gap-buffer)
          (hafod editor kill-ring)
          (hafod editor input-decode))

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
            (mutable recording-edit)))  ;; list of key events being recorded

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
      #f))           ;; recording-edit

  ;; Public accessors
  (define (vi-visual-mode) (vi-state-vis-mode *vi*))
  (define (vi-visual-anchor) (vi-state-vis-anchor *vi*))
  (define (vi-visual-end gb) (gap-buffer-cursor-pos gb))
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
  (define (vi-search-pattern) (vi-state-search-pat *vi*))
  (define (vi-search-direction) (vi-state-search-dir *vi*))

  (define (vi-reset-state!)
    (vi-state-count-set! *vi* 0)
    (vi-state-operator-set! *vi* #f)
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

  ;; ======================================================================
  ;; Character classification
  ;; ======================================================================

  (define (word-char? ch)
    (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)
        (char=? ch #\-) (char=? ch #\?) (char=? ch #\!)))

  (define (blank? ch) (or (char=? ch #\space) (char=? ch #\tab)))

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

  ;; First non-blank (vim ^)
  (define (motion-first-non-blank text _pos)
    (let ([len (string-length text)])
      (let lp ([i 0])
        (cond
          [(>= i len) 0]
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

  ;; Inner/around delimited pair: parens, brackets, braces, quotes
  (define (text-obj-inner-pair text pos open close)
    (let ([len (string-length text)])
      ;; Find enclosing open
      (let ([op (if (char=? open close)
                    ;; For quotes: scan backward for nearest odd quote
                    (let lp ([i (- pos 1)] [count 0])
                      (cond
                        [(< i 0) (if (odd? count) #f
                                     ;; Maybe pos is on the open quote
                                     (and (< pos len)
                                          (char=? (string-ref text pos) open)
                                          pos))]
                        [(char=? (string-ref text i) open) (lp (- i 1) (+ count 1))]
                        [else (lp (- i 1) count)]))
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
                          ;; For quotes: find next quote after open
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
  ;; Register management
  ;; ======================================================================

  (define (vi-reg-store! reg text)
    (hashtable-set! (vi-state-registers-ht *vi*) reg text)
    ;; Also store in unnamed register
    (hashtable-set! (vi-state-registers-ht *vi*) #\" text))

  (define (vi-reg-fetch reg)
    (hashtable-ref (vi-state-registers-ht *vi*) reg ""))

  ;; ======================================================================
  ;; Operator application
  ;; ======================================================================

  ;; Delete range [start, end) from gap buffer, store in register
  (define (vi-delete-range! gb start end kr)
    (let* ([text (gap-buffer->string gb)]
           [len (string-length text)]
           [s (max 0 (min start len))]
           [e (max s (min end len))])
      (when (< s e)
        (let* ([deleted (substring text s e)]
               [new-text (string-append (substring text 0 s)
                                        (substring text e len))])
          (vi-reg-store! (vi-state-register-char *vi*) deleted)
          (kill-ring-push! kr deleted)
          (gap-buffer-set-from-string! gb new-text)
          (gap-buffer-move-cursor! gb (- (min s (string-length new-text))
                                         (gap-buffer-cursor-pos gb)))))))

  ;; Yank range [start, end) — copy to register without deleting
  (define (vi-yank-range! gb start end kr)
    (let* ([text (gap-buffer->string gb)]
           [len (string-length text)]
           [s (max 0 (min start len))]
           [e (max s (min end len))])
      (when (< s e)
        (let ([yanked (substring text s e)])
          (vi-reg-store! (vi-state-register-char *vi*) yanked)
          (kill-ring-push! kr yanked)))))

  ;; Apply operator to range
  (define (vi-apply-operator! es gb kr start end)
    (let ([op (vi-state-operator *vi*)])
      (vi-reset-state!)
      (case op
        [(d)
         (when (vi-snapshot!-proc) ((vi-snapshot!-proc) es))
         (vi-delete-range! gb start end kr)]
        [(c)
         (when (vi-snapshot!-proc) ((vi-snapshot!-proc) es))
         (vi-delete-range! gb start end kr)
         (when (vi-enter-insert!-proc) ((vi-enter-insert!-proc) es))]
        [(y)
         (vi-yank-range! gb start end kr)])))

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
             [(#\0) (lambda (text _pos) 0)]
             [(#\^) motion-first-non-blank]
             [(#\$) (lambda (text _pos)
                      (let ([len (string-length text)])
                        (if (> len 0) (- len 1) 0)))]
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

           ;; Operator keys: d c y
           [(and (eq? type 'char) (memv val '(#\d #\c #\y))
                 (not (vi-state-operator *vi*)))
            (vi-state-operator-set! *vi* (case val [(#\d) 'd] [(#\c) 'c] [(#\y) 'y]))
            (vi-state-pending-set! *vi* 'operator)
            #t]

           ;; Double operator: dd cc yy → whole line
           [(and (eq? type 'char) (vi-state-operator *vi*)
                 (case (vi-state-operator *vi*)
                   [(d) (char=? val #\d)]
                   [(c) (char=? val #\c)]
                   [(y) (char=? val #\y)]
                   [else #f]))
            (let* ([text (gap-buffer->string gb)]
                   [len (string-length text)])
              (vi-apply-operator! es gb kr 0 len))
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
                        [pos (gap-buffer-cursor-pos gb)]
                        [cnt (max 1 (vi-state-count *vi*))]
                        [new-pos (run-motion-n motion text pos cnt)])
                   (cond
                     [(vi-state-operator *vi*)
                      ;; Apply operator over range
                      (when new-pos
                        (let ([start (min pos new-pos)]
                              [end (if (memv val '(#\e #\E #\$ #\%))
                                       (+ (max pos new-pos) 1)
                                       (max pos new-pos))])
                          (vi-apply-operator! es gb kr start end)))]
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

           ;; G: end of line (in single-line context)
           [(and (eq? type 'char) (char=? val #\G) (not (vi-state-operator *vi*)))
            (let ([len (gap-buffer-length gb)]
                  [pos (gap-buffer-cursor-pos gb)])
              (when (< pos len)
                (gap-buffer-move-cursor! gb (- (max 0 (- len 1)) pos))))
            (vi-reset-state!)
            #t]

           ;; gg: beginning of buffer
           [(and (eq? type 'char) (char=? val #\g) (not (vi-state-operator *vi*)))
            (let ([next (read-key-event in-port)])
              (when (and (not (eof-object? next))
                         (eq? (key-event-type next) 'char)
                         (char=? (key-event-value next) #\g))
                (let ([pos (gap-buffer-cursor-pos gb)])
                  (gap-buffer-move-cursor! gb (- 0 pos)))))
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
              ;; For line visual, extend to line boundaries
              (let-values ([(s e)
                            (if (eq? (vi-state-vis-mode *vi*) 'line)
                                (values 0 len)
                                (values start (min end len)))])
                (vi-state-operator-set! *vi* (case val
                                    [(#\d #\x) 'd] [(#\c #\s) 'c] [(#\y) 'y]))
                (vi-state-vis-mode-set! *vi* #f)
                (vi-apply-operator! es gb kr s e)))
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

           ;; . repeat last edit
           [(and (eq? type 'char) (char=? val #\.) (not (vi-state-operator *vi*)))
            ;; TODO: replay recorded edit
            (vi-reset-state!)
            #t]

           ;; Not handled by vi — fall through to keymap
           [else
            (vi-reset-state!)
            #f])])))

) ; end library
