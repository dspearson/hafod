;;; (hafod editor editor) -- Line editor core: raw mode, emacs bindings, read-expression
;;; Combines gap-buffer, kill-ring, sexp-tracker, input-decode, keymap, and render
;;; into a working line editor with emacs keybindings.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor editor)
  (export read-expression with-raw-mode editor-default-keymap
          editor-insert-keymap editor-normal-keymap
          ;; Exported for testing
          make-editor-state editor-state-gb editor-state-done? editor-state-result
          editor-state-mode editor-state-mode-set!
          ;; Completion helpers (exported for testing)
          word-at-cursor symbol-completions filename-completions
          longest-common-prefix path-at-cursor
          ;; Shell-mode completion
          shell-completions
          ;; Keymap layers
          bind-base-keys! bind-paredit-keys! unbind-paredit-keys!
          ;; Paredit toggle
          toggle-paredit! paredit-enabled? enable-paredit! disable-paredit!)
  (import (chezscheme)
          (hafod editor gap-buffer)
          (hafod editor kill-ring)
          (hafod editor sexp-tracker)
          (hafod editor input-decode)
          (hafod editor keymap)
          (hafod editor render)
          (hafod editor history)
          (hafod tty)
          (only (hafod shell classifier) path-cache scheme-prefix-chars))

  ;; ======================================================================
  ;; Raw mode
  ;; ======================================================================

  ;; with-raw-mode: enter raw mode on fd, run thunk, restore on exit.
  ;; Uses dynamic-wind so terminal is restored even on non-local exit (SIGINT, exceptions).
  ;; Keeps ttyl/enable-signals so SIGINT is delivered as a signal, not raw byte.
  (define (with-raw-mode fd thunk)
    (let ([saved (tty-info fd)])
      (dynamic-wind
        (lambda ()
          (let ([raw (copy-tty-info saved)])
            (set-tty-info:local-flags raw
              (bitwise-and (tty-info:local-flags raw)
                           (bitwise-not (bitwise-ior ttyl/canonical ttyl/echo))))
            (set-tty-info:input-flags raw
              (bitwise-and (tty-info:input-flags raw)
                           (bitwise-not ttyin/cr->nl)))
            (set-tty-info:min raw 1)
            (set-tty-info:time raw 0)
            (set-tty-info/now fd raw))
          ;; Enable bracketed paste mode
          (display "\x1b;[?2004h" (console-output-port))
          (flush-output-port (console-output-port)))
        thunk
        (lambda ()
          ;; Disable bracketed paste mode
          (display "\x1b;[?2004l" (console-output-port))
          (flush-output-port (console-output-port))
          (set-tty-info/now fd saved)))))

  ;; ======================================================================
  ;; Editor state record
  ;; ======================================================================

  ;; Internal state passed to command procedures.
  (define-record-type editor-state
    (nongenerative)
    (fields
      gb            ;; gap-buffer
      kr            ;; kill-ring
      prompt        ;; prompt string
      out-port      ;; output port for rendering
      (mutable last-yank-len)  ;; length of last yanked text (for M-y replace)
      (mutable done?)          ;; #t when user pressed Return or C-d on empty
      (mutable result)         ;; string or eof-object when done
      (mutable mode)))         ;; 'insert or 'normal

  ;; ======================================================================
  ;; Word boundary helpers
  ;; ======================================================================

  (define (char-word-constituent? ch)
    (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_) (char=? ch #\-)))

  ;; Find next word boundary (forward) from position pos in string s.
  (define (next-word-boundary s pos)
    (let ([len (string-length s)])
      ;; Skip non-word chars first
      (let skip-non ([i pos])
        (cond
          [(>= i len) len]
          [(char-word-constituent? (string-ref s i))
           ;; Now skip word chars
           (let skip-word ([j i])
             (cond
               [(>= j len) len]
               [(char-word-constituent? (string-ref s j))
                (skip-word (+ j 1))]
               [else j]))]
          [else (skip-non (+ i 1))]))))

  ;; Find previous word boundary (backward) from position pos in string s.
  (define (prev-word-boundary s pos)
    ;; Skip non-word chars backwards
    (let skip-non ([i (- pos 1)])
      (cond
        [(< i 0) 0]
        [(char-word-constituent? (string-ref s i))
         ;; Now skip word chars backwards
         (let skip-word ([j i])
           (cond
             [(< j 0) 0]
             [(char-word-constituent? (string-ref s j))
              (skip-word (- j 1))]
             [else (+ j 1)]))]
        [else (skip-non (- i 1))])))

  ;; ======================================================================
  ;; Editor commands
  ;; ======================================================================

  ;; Movement commands
  (define (cmd-beginning-of-line es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)])
      (gap-buffer-move-cursor! gb (- pos))))

  (define (cmd-end-of-line es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [len (gap-buffer-length gb)])
      (gap-buffer-move-cursor! gb (- len pos))))

  (define (cmd-forward-char es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [len (gap-buffer-length gb)])
      (when (< pos len)
        (gap-buffer-move-cursor! gb 1))))

  (define (cmd-backward-char es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)])
      (when (> pos 0)
        (gap-buffer-move-cursor! gb -1))))

  (define (cmd-forward-word es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)]
           [target (next-word-boundary text pos)])
      (gap-buffer-move-cursor! gb (- target pos))))

  (define (cmd-backward-word es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)]
           [target (prev-word-boundary text pos)])
      (gap-buffer-move-cursor! gb (- target pos))))

  ;; ======================================================================
  ;; Structural deletion helpers
  ;; ======================================================================

  (define (opening-delimiter? ch)
    (or (char=? ch #\() (char=? ch #\[)))

  (define (closing-delimiter? ch)
    (or (char=? ch #\)) (char=? ch #\])))

  (define (matching-pair? open close)
    (or (and (char=? open #\() (char=? close #\)))
        (and (char=? open #\[) (char=? close #\]))
        (and (char=? open #\") (char=? close #\"))))

  ;; Splice-delete helper: remove delimiter at del-idx and its match, keep contents.
  ;; Returns new cursor position.
  (define (splice-delimiter! gb text del-idx)
    (let* ([len (string-length text)]
           [ch (string-ref text del-idx)]
           [pos (gap-buffer-cursor-pos gb)])
      (cond
        ;; Deleting an opener: find matching closer
        [(opening-delimiter? ch)
         (let ([close-idx (find-matching-close text del-idx)])
           (if close-idx
               ;; Remove both: build new text without opener and closer
               (let* ([new-text (string-append
                                  (substring text 0 del-idx)
                                  (substring text (+ del-idx 1) close-idx)
                                  (substring text (+ close-idx 1) len))]
                      ;; Adjust cursor: if after opener, shift left 1; if after closer, shift left 2
                      [new-pos (cond
                                 [(<= pos del-idx) pos]
                                 [(<= pos close-idx) (- pos 1)]
                                 [else (- pos 2)])])
                 (editor-replace-text! gb new-text new-pos))
               ;; No match found: just delete the single char
               (begin
                 (gap-buffer-move-cursor! gb (- del-idx pos))
                 (gap-buffer-delete-forward! gb))))]
        ;; Deleting a closer: find matching opener
        [(closing-delimiter? ch)
         (let ([open-idx (find-matching-paren text del-idx)])
           (if open-idx
               (let* ([new-text (string-append
                                  (substring text 0 open-idx)
                                  (substring text (+ open-idx 1) del-idx)
                                  (substring text (+ del-idx 1) len))]
                      [new-pos (cond
                                 [(<= pos open-idx) pos]
                                 [(<= pos del-idx) (- pos 1)]
                                 [else (- pos 2)])])
                 (editor-replace-text! gb new-text new-pos))
               (begin
                 (gap-buffer-move-cursor! gb (- del-idx pos))
                 (gap-buffer-delete-forward! gb))))])))

  ;; Delete forward (C-d / Delete key) — structural: splices non-empty delimiters.
  (define (cmd-delete-char es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [len (gap-buffer-length gb)])
      (cond
        [(= len 0)
         (editor-state-done?-set! es #t)
         (editor-state-result-set! es (eof-object))]
        [(>= pos len) (void)]
        [else
         (let* ([text (gap-buffer->string gb)]
                [next-ch (string-ref text pos)]
                [prev-ch (and (> pos 0) (string-ref text (- pos 1)))]
                [state (lexer-state-at text pos)])
           (cond
             ;; Empty pair: delete both
             [(and prev-ch (matching-pair? prev-ch next-ch)
                   (or (and (opening-delimiter? prev-ch) (closing-delimiter? next-ch))
                       (and (char=? prev-ch #\") (char=? next-ch #\")
                            (eq? state 'in-string)
                            (eq? (lexer-state-at text (- pos 1)) 'normal))))
              (gap-buffer-move-cursor! gb -1)
              (gap-buffer-delete-forward! gb)
              (gap-buffer-delete-forward! gb)]
             ;; Inside string/comment: normal deletion except closing quote
             [(memq state '(in-string in-string-escape in-line-comment in-block-comment))
              (if (and (char=? next-ch #\")
                       (eq? (lexer-state-at text (+ pos 1)) 'normal))
                  (void)
                  (gap-buffer-delete-forward! gb))]
             ;; Non-empty paren/bracket: splice (remove both opener and closer)
             [(or (opening-delimiter? next-ch) (closing-delimiter? next-ch))
              (splice-delimiter! gb text pos)]
             ;; Quote at boundary: don't delete
             [(char=? next-ch #\") (void)]
             ;; Normal character
             [else (gap-buffer-delete-forward! gb)]))])))

  ;; Delete backward (Backspace) — structural: splices non-empty delimiters.
  (define (cmd-delete-backward-char es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [len (gap-buffer-length gb)])
      (when (> pos 0)
        (let* ([text (gap-buffer->string gb)]
               [prev-ch (string-ref text (- pos 1))]
               [next-ch (and (< pos len) (string-ref text pos))]
               [state (lexer-state-at text pos)])
          (cond
            ;; Empty pair: delete both
            [(and next-ch (matching-pair? prev-ch next-ch)
                  (or (and (opening-delimiter? prev-ch) (closing-delimiter? next-ch))
                      (and (char=? prev-ch #\") (char=? next-ch #\")
                           (eq? state 'in-string)
                           (eq? (lexer-state-at text (- pos 1)) 'normal))))
             (gap-buffer-move-cursor! gb -1)
             (gap-buffer-delete-forward! gb)
             (gap-buffer-delete-forward! gb)]
            ;; Inside string: allow content deletion, not opening quote
            [(memq state '(in-string in-string-escape))
             (if (and (char=? prev-ch #\")
                      (eq? (lexer-state-at text (- pos 1)) 'normal))
                 (void)
                 (gap-buffer-delete-backward! gb))]
            ;; Inside comment: allow deletion
            [(memq state '(in-line-comment in-block-comment))
             (gap-buffer-delete-backward! gb)]
            ;; Non-empty paren/bracket: splice (remove both opener and closer)
            [(or (opening-delimiter? prev-ch) (closing-delimiter? prev-ch))
             (splice-delimiter! gb text (- pos 1))]
            ;; Closing quote: don't delete
            [(and (char=? prev-ch #\") (eq? state 'normal))
             (void)]
            ;; Normal character
            [else (gap-buffer-delete-backward! gb)])))))

  ;; Kill commands
  ;; Structural kill-line: if inside a list, kill to the closing delimiter
  ;; (not including it). If at top level, kill to end of buffer.
  (define (cmd-kill-line es)
    (let* ([gb (editor-state-gb es)]
           [kr (editor-state-kr es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)]
           [len (string-length text)])
      (let-values ([(open-idx close-idx) (find-enclosing-parens text pos)])
        (if (and open-idx close-idx (< pos close-idx))
            ;; Inside a list: kill from cursor to the closing delimiter (exclusive)
            (let ([killed (substring text pos close-idx)])
              (when (> (string-length killed) 0)
                ;; Rebuild text: before cursor + closing delimiter onward
                (let* ([new-text (string-append
                                   (substring text 0 pos)
                                   (substring text close-idx len))])
                  (editor-replace-text! gb new-text pos)
                  (kill-ring-push! kr killed))))
            ;; Top level or at/past closer: kill to end of buffer
            (let ([killed (gap-buffer-kill-to-end! gb)])
              (when (> (string-length killed) 0)
                (kill-ring-push! kr killed)))))
      (editor-state-last-yank-len-set! es 0)))

  (define (cmd-kill-region es)
    ;; For now, same as kill-line (full region support deferred)
    (cmd-kill-line es))

  (define (cmd-kill-word es)
    (let* ([gb (editor-state-gb es)]
           [kr (editor-state-kr es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)]
           [target (next-word-boundary text pos)]
           [deleted-len (- target pos)])
      (when (> deleted-len 0)
        ;; Extract the text that will be deleted
        (let ([killed (substring text pos target)])
          (gap-buffer-delete-word-forward! gb)
          (kill-ring-push! kr killed)
          (editor-state-last-yank-len-set! es 0)))))

  (define (cmd-backward-kill-word es)
    (let* ([gb (editor-state-gb es)]
           [kr (editor-state-kr es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)]
           [target (prev-word-boundary text pos)]
           [deleted-len (- pos target)])
      (when (> deleted-len 0)
        (let ([killed (substring text target pos)])
          ;; Move cursor back then delete forward
          (gap-buffer-move-cursor! gb (- deleted-len))
          (do ([i 0 (+ i 1)])
              ((= i deleted-len))
            (gap-buffer-delete-forward! gb))
          (kill-ring-push! kr killed)
          (editor-state-last-yank-len-set! es 0)))))

  (define (cmd-kill-whole-line es)
    (let* ([gb (editor-state-gb es)]
           [kr (editor-state-kr es)]
           [text (gap-buffer->string gb)])
      (when (> (string-length text) 0)
        (kill-ring-push! kr text)
        ;; Move to start, delete everything
        (let ([pos (gap-buffer-cursor-pos gb)])
          (gap-buffer-move-cursor! gb (- pos))
          (do ([i 0 (+ i 1)])
              ((= i (string-length text)))
            (gap-buffer-delete-forward! gb)))
        (editor-state-last-yank-len-set! es 0))))

  (define (cmd-clear-screen es)
    (let ([port (editor-state-out-port es)])
      ;; Clear screen and move cursor to top-left
      (display "\x1b;[2J\x1b;[H" port)
      (flush-output-port port)))

  ;; Yank commands
  (define (cmd-yank es)
    (let* ([gb (editor-state-gb es)]
           [kr (editor-state-kr es)]
           [text (kill-ring-yank kr)])
      (when text
        (let ([len (string-length text)])
          (do ([i 0 (+ i 1)])
              ((= i len))
            (gap-buffer-insert! gb (string-ref text i)))
          (editor-state-last-yank-len-set! es len)))))

  (define (cmd-yank-pop es)
    (let* ([gb (editor-state-gb es)]
           [kr (editor-state-kr es)]
           [prev-len (editor-state-last-yank-len es)])
      (when (> prev-len 0)
        ;; Delete the previously yanked text
        (gap-buffer-move-cursor! gb (- prev-len))
        (do ([i 0 (+ i 1)])
            ((= i prev-len))
          (gap-buffer-delete-forward! gb))
        ;; Rotate kill ring and yank new text
        (kill-ring-rotate! kr)
        (let ([text (kill-ring-yank kr)])
          (when text
            (let ([len (string-length text)])
              (do ([i 0 (+ i 1)])
                  ((= i len))
                (gap-buffer-insert! gb (string-ref text i)))
              (editor-state-last-yank-len-set! es len)))))))

  ;; Transpose
  (define (cmd-transpose-chars es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [len (gap-buffer-length gb)])
      (when (and (>= pos 2) (<= pos len))
        ;; If cursor is at end of buffer, transpose the two chars before it
        ;; If cursor is in middle, transpose char before cursor with char at cursor
        (let* ([swap-pos (if (= pos len) (- pos 2) (- pos 1))]
               [text (gap-buffer->string gb)]
               [ch1 (string-ref text swap-pos)]
               [ch2 (string-ref text (+ swap-pos 1))])
          ;; Move to swap-pos, delete both chars, insert swapped
          (let ([delta (- swap-pos pos)])
            (gap-buffer-move-cursor! gb delta)
            (gap-buffer-delete-forward! gb)
            (gap-buffer-delete-forward! gb)
            (gap-buffer-insert! gb ch2)
            (gap-buffer-insert! gb ch1))))))

  ;; ======================================================================
  ;; Evil-mode: modal editing (normal / insert)
  ;; ======================================================================

  ;; Cursor shape + colour via ANSI (shape) and OSC 12 (colour)
  ;; Doom Emacs palette: insert=#51afef (blue), normal=#ECBE7B (yellow)
  (define (set-cursor-block port)
    (display "\x1b;[2 q" port)            ; steady block
    (display "\x1b;]12;#ECBE7B\x7;" port))  ; yellow cursor colour
  (define (set-cursor-bar port)
    (display "\x1b;[6 q" port)            ; steady bar
    (display "\x1b;]12;#51afef\x7;" port))  ; blue cursor colour
  (define (reset-cursor port)
    (display "\x1b;]112\x7;" port)           ; reset cursor colour to terminal default
    (display "\x1b;[0 q" port))              ; reset cursor shape to terminal default

  ;; Enter normal mode (Escape from insert)
  (define (cmd-enter-normal-mode es)
    (editor-state-mode-set! es 'normal)
    ;; Vim convention: cursor moves left 1 when leaving insert
    (let ([gb (editor-state-gb es)])
      (when (> (gap-buffer-cursor-pos gb) 0)
        (gap-buffer-move-cursor! gb -1)))
    (set-cursor-block (editor-state-out-port es)))

  ;; Enter insert mode at cursor (i)
  (define (cmd-enter-insert-mode es)
    (editor-state-mode-set! es 'insert)
    (set-cursor-bar (editor-state-out-port es)))

  ;; Enter insert mode after cursor (a)
  (define (cmd-enter-insert-after es)
    (let ([gb (editor-state-gb es)])
      (when (< (gap-buffer-cursor-pos gb) (gap-buffer-length gb))
        (gap-buffer-move-cursor! gb 1)))
    (cmd-enter-insert-mode es))

  ;; Enter insert mode at beginning of line (I)
  (define (cmd-enter-insert-bol es)
    (cmd-beginning-of-line es)
    (cmd-enter-insert-mode es))

  ;; Enter insert mode at end of line (A)
  (define (cmd-enter-insert-eol es)
    (cmd-end-of-line es)
    (cmd-enter-insert-mode es))

  ;; Open line below and enter insert mode (o)
  (define (cmd-open-below es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)]
           [pos (gap-buffer-cursor-pos gb)]
           [indent (compute-indent text pos)])
      (cmd-end-of-line es)
      (gap-buffer-insert! gb #\newline)
      (do ([i 0 (+ i 1)])
          ((= i (string-length indent)))
        (gap-buffer-insert! gb (string-ref indent i)))
      (cmd-enter-insert-mode es)))

  ;; Open line above and enter insert mode (O)
  (define (cmd-open-above es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)]
           [pos (gap-buffer-cursor-pos gb)]
           [indent (compute-indent text pos)])
      (cmd-beginning-of-line es)
      (gap-buffer-insert! gb #\newline)
      (gap-buffer-move-cursor! gb -1)
      (do ([i 0 (+ i 1)])
          ((= i (string-length indent)))
        (gap-buffer-insert! gb (string-ref indent i)))
      (cmd-enter-insert-mode es)))

  ;; Substitute char: delete char at cursor, enter insert (s)
  (define (cmd-substitute-char es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [len (gap-buffer-length gb)])
      (when (< pos len)
        (gap-buffer-delete-forward! gb))
      (cmd-enter-insert-mode es)))

  ;; Substitute line: kill whole line, enter insert (S)
  (define (cmd-substitute-line es)
    (cmd-kill-whole-line es)
    (cmd-enter-insert-mode es))

  ;; Always insert newline + auto-indent (Enter in insert mode)
  (define (cmd-insert-newline es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)]
           [pos (gap-buffer-cursor-pos gb)]
           [indent (compute-indent text pos)])
      (gap-buffer-insert! gb #\newline)
      (do ([i 0 (+ i 1)])
          ((= i (string-length indent)))
        (gap-buffer-insert! gb (string-ref indent i)))))

  ;; Smart Return: submit balanced, insert newline for unbalanced, no-op for empty.
  (define (cmd-smart-return es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)]
           [len (string-length text)])
      (cond
        ;; Empty buffer: no-op
        [(= len 0) (void)]
        [else
         (let ([depth (sexp-depth text)]
               [state (lexer-state-at text len)])
           (cond
             ;; Balanced and in normal state: submit
             [(and (= depth 0) (eq? state 'normal))
              (cmd-submit es)]
             ;; Unbalanced or in string/comment: insert newline
             [else
              (cmd-insert-newline es)]))])))

  ;; Always submit for eval (Enter in normal mode)
  (define (cmd-submit es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)])
      (history-add! editor-history text)
      (history-reset-nav! editor-history)
      (editor-state-done?-set! es #t)
      (editor-state-result-set! es text)))

  ;; History: navigate to previous (older) entry with optional prefix filtering
  (define (cmd-history-prev es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)]
           [pos (gap-buffer-cursor-pos gb)])
      ;; On first upward press, save input and determine if prefix search
      (when (= (history-cursor editor-history) -1)
        (history-save-input! editor-history text)
        ;; If buffer has content and cursor > 0, use prefix search
        (if (and (> (string-length text) 0) (> pos 0))
            (set! history-prefix (substring text 0 pos))
            (set! history-prefix #f)))
      ;; Navigate
      (if history-prefix
          ;; Prefix-filtered search
          (let* ([entries (history-entries editor-history)]
                 [cur (history-cursor editor-history)]
                 [start (if (= cur -1) (- (vector-length entries) 1) (- cur 1))]
                 [idx (history-prefix-search-backward editor-history history-prefix start)])
            (when idx
              (history-cursor-set! editor-history idx)
              (gap-buffer-set-from-string! gb (vector-ref entries idx))))
          ;; Normal history navigation
          (let ([entry (history-prev editor-history)])
            (when entry
              (gap-buffer-set-from-string! gb entry))))))

  ;; History: navigate to next (newer) entry with optional prefix filtering
  (define (cmd-history-next es)
    (let* ([gb (editor-state-gb es)]
           [cur (history-cursor editor-history)])
      (if (and history-prefix (not (= cur -1)))
          ;; Prefix-filtered forward search
          (let* ([entries (history-entries editor-history)]
                 [len (vector-length entries)])
            (let loop ([i (+ cur 1)])
              (cond
                [(>= i len)
                 ;; Past most recent: return to saved input
                 (history-cursor-set! editor-history -1)
                 (set! history-prefix #f)
                 (gap-buffer-set-from-string! gb (history-saved-input editor-history))]
                [(string-prefix? history-prefix (vector-ref entries i))
                 (history-cursor-set! editor-history i)
                 (gap-buffer-set-from-string! gb (vector-ref entries i))]
                [else (loop (+ i 1))])))
          ;; Normal history navigation
          (let ([entry (history-next editor-history)])
            (when entry
              (gap-buffer-set-from-string! gb entry))))))

  ;; Reverse incremental search: Ctrl-R
  (define (cmd-reverse-search es)
    (let ([gb (editor-state-gb es)])
      (cond
        [(not search-active?)
         ;; Start search: save current buffer, activate
         (set! search-saved-buffer (gap-buffer->string gb))
         (set! search-active? #t)
         (set! search-query "")
         (set! search-match-idx (- (vector-length (history-entries editor-history)) 1))
         ;; Perform initial search (empty query matches everything)
         (void)]
        [else
         ;; Already in search: cycle to next older match
         (when (> search-match-idx 0)
           (let ([idx (history-search-backward editor-history search-query
                        (- search-match-idx 1))])
             (when idx
               (set! search-match-idx idx)
               (let ([entry (vector-ref (history-entries editor-history) idx)])
                 (gap-buffer-set-from-string! gb entry)))))])))

  ;; Exit search mode. If cancel? is #t, restore original buffer.
  (define (exit-search-mode! es cancel?)
    (let ([gb (editor-state-gb es)])
      (when cancel?
        (gap-buffer-set-from-string! gb search-saved-buffer))
      (reset-search-state!)))

  ;; Handle a character typed during search mode: append to query, re-search
  (define (search-self-insert! es ch)
    (let ([gb (editor-state-gb es)])
      (set! search-query (string-append search-query (string ch)))
      (let ([idx (history-search-backward editor-history search-query search-match-idx)])
        (cond
          [idx
           (set! search-match-idx idx)
           (gap-buffer-set-from-string! gb (vector-ref (history-entries editor-history) idx))]
          [else (void)]))))  ; no match -- keep current display

  ;; Paste after cursor (p)
  (define (cmd-paste-after es)
    (let ([gb (editor-state-gb es)])
      (when (< (gap-buffer-cursor-pos gb) (gap-buffer-length gb))
        (gap-buffer-move-cursor! gb 1))
      (cmd-yank es)))

  ;; Paste before cursor (P) - same as yank
  (define (cmd-paste-before es)
    (cmd-yank es))

  ;; ======================================================================
  ;; Intelligent auto-indentation
  ;; ======================================================================

  ;; Scheme special forms: body indented 2 from opening paren column.
  (define indent-special-forms
    '("define" "define-syntax" "define-record-type" "define-condition-type"
      "define-values" "define-property"
      "lambda" "case-lambda"
      "let" "let*" "letrec" "letrec*" "let-values" "let*-values"
      "fluid-let" "parameterize" "with-mutex"
      "if" "when" "unless" "cond" "case"
      "begin" "do" "delay" "delay-force"
      "guard" "dynamic-wind" "with-exception-handler"
      "call-with-values" "call/cc" "call-with-current-continuation"
      "call-with-port" "call-with-input-file" "call-with-output-file"
      "with-input-from-file" "with-output-to-file"
      "syntax-case" "syntax-rules" "with-syntax"
      "library" "module" "import" "export"
      "record-case" "exclusive-cond" "critical-section"
      "trace-lambda" "trace-define" "trace-let"
      "match" "match-let" "match-let*"
      "for-each" "map" "and" "or"))

  (define (special-form? sym)
    (memp (lambda (s) (string=? s sym)) indent-special-forms))

  ;; Column of position pos (0-based): chars from last newline or start.
  (define (column-of text pos)
    (let loop ([i (fx- pos 1)] [col 0])
      (cond
        [(fx< i 0) col]
        [(char=? (string-ref text i) #\newline) col]
        [else (loop (fx- i 1) (fx+ col 1))])))

  ;; Extract symbol immediately after open paren at paren-pos.
  ;; Returns string or #f if no symbol follows on the same line.
  (define (extract-head-symbol text paren-pos)
    (let ([len (string-length text)])
      (let skip-ws ([i (fx+ paren-pos 1)])
        (cond
          [(fx>= i len) #f]
          [(char=? (string-ref text i) #\newline) #f]
          [(char-whitespace? (string-ref text i)) (skip-ws (fx+ i 1))]
          [(memv (string-ref text i) '(#\( #\) #\[ #\] #\" #\;)) #f]
          [else
           (let read-sym ([j i] [acc '()])
             (cond
               [(fx>= j len)
                (if (null? acc) #f (list->string (reverse acc)))]
               [(or (char-whitespace? (string-ref text j))
                    (memv (string-ref text j) '(#\( #\) #\[ #\] #\" #\;)))
                (if (null? acc) #f (list->string (reverse acc)))]
               [else (read-sym (fx+ j 1) (cons (string-ref text j) acc))]))]))))

  ;; Find column of first argument after head symbol, on same line as paren.
  ;; Returns column number or #f.
  (define (find-first-arg-column text paren-pos)
    (let ([len (string-length text)])
      ;; Skip whitespace after paren
      (let skip-ws1 ([i (fx+ paren-pos 1)])
        (cond
          [(fx>= i len) #f]
          [(char=? (string-ref text i) #\newline) #f]
          [(char-whitespace? (string-ref text i)) (skip-ws1 (fx+ i 1))]
          [else
           ;; Skip over head token (symbol or other atom)
           (let skip-head ([j i])
             (cond
               [(fx>= j len) #f]
               [(char=? (string-ref text j) #\newline) #f]
               [(or (char-whitespace? (string-ref text j))
                    (memv (string-ref text j) '(#\( #\) #\[ #\] #\" #\;)))
                ;; Find first arg (skip whitespace after head)
                (let skip-ws2 ([k j])
                  (cond
                    [(fx>= k len) #f]
                    [(char=? (string-ref text k) #\newline) #f]
                    [(char-whitespace? (string-ref text k)) (skip-ws2 (fx+ k 1))]
                    [else (column-of text k)]))]
               [else (skip-head (fx+ j 1))]))]))))

  ;; Compute indentation for a new line using Scheme conventions.
  ;; Special forms: indent 2 from opening paren column.
  ;; Regular calls: align with first argument, or indent 2 if no arg on same line.
  ;; Nested lists (no symbol head): align at paren column + 1.
  (define (compute-indent text pos)
    (let-values ([(state stack bc-depth) (scan-lexer text pos)])
      (cond
        [(not (eq? state 'normal)) ""]
        [(null? stack) ""]
        [else
         (let* ([paren-pos (car stack)]
                [paren-col (column-of text paren-pos)]
                [head (extract-head-symbol text paren-pos)]
                [indent-col
                 (cond
                   [(and head (special-form? head))
                    (fx+ paren-col 2)]
                   [head
                    (or (find-first-arg-column text paren-pos)
                        (fx+ paren-col 2))]
                   [else
                    (fx+ paren-col 1)])])
           (make-string indent-col #\space))])))

  ;; Helper: replace gap-buffer contents and set cursor position.
  (define (editor-replace-text! gb new-text new-pos)
    (gap-buffer-set-from-string! gb new-text)
    (gap-buffer-move-cursor! gb (- new-pos (string-length new-text))))

  ;; Self-insert with paredit-style auto-pairing and skip-close.
  (define (cmd-self-insert es ch)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [len (gap-buffer-length gb)]
           [text (gap-buffer->string gb)]
           [state (if (> len 0) (lexer-state-at text pos) 'normal)])
      (cond
        ;; Skip-close: typing ) or ] when cursor is at that char (paredit only)
        [(and (paredit-enabled?)
              (or (char=? ch #\)) (char=? ch #\]))
              (eq? state 'normal)
              (< pos len)
              (char=? (gap-buffer-char-at gb pos) ch))
         (gap-buffer-move-cursor! gb 1)]
        ;; Auto-pair: ( -> (), [ -> [] (paredit only)
        ;; Insert preceding space if needed (e.g. (cd|) + ( -> (cd ()))
        [(and (paredit-enabled?)
              (or (char=? ch #\() (char=? ch #\[))
              (eq? state 'normal))
         (let ([close (if (char=? ch #\() #\) #\])])
           ;; Insert space before if prev char is not space/opener/start-of-input
           (when (and (> pos 0)
                      (let ([prev (gap-buffer-char-at gb (- pos 1))])
                        (not (or (char-whitespace? prev)
                                 (char=? prev #\()
                                 (char=? prev #\[)))))
             (gap-buffer-insert! gb #\space))
           (gap-buffer-insert! gb ch)
           (gap-buffer-insert! gb close)
           (gap-buffer-move-cursor! gb -1))]
        ;; Double-quote handling (paredit auto-pair only when enabled)
        [(char=? ch #\")
         (cond
           ;; In string and next char is ": skip over (paredit only)
           [(and (paredit-enabled?)
                 (eq? state 'in-string)
                 (< pos len)
                 (char=? (gap-buffer-char-at gb pos) #\"))
            (gap-buffer-move-cursor! gb 1)]
           ;; Not in string: auto-pair "" (paredit only)
           [(and (paredit-enabled?)
                 (eq? state 'normal))
            (gap-buffer-insert! gb #\")
            (gap-buffer-insert! gb #\")
            (gap-buffer-move-cursor! gb -1)]
           ;; In string or paredit disabled: just insert
           [else (gap-buffer-insert! gb ch)])]
        ;; Normal insert
        [else (gap-buffer-insert! gb ch)])))

  ;; ======================================================================
  ;; Structural editing commands (paredit-style)
  ;; ======================================================================

  ;; Forward sexp: move cursor past the next sexp
  (define (cmd-forward-sexp es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)]
           [target (forward-sexp-end text pos)])
      (when target
        (gap-buffer-move-cursor! gb (- target pos)))))

  ;; Backward sexp: move cursor to start of previous sexp
  (define (cmd-backward-sexp es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)]
           [target (backward-sexp-start text pos)])
      (when target
        (gap-buffer-move-cursor! gb (- target pos)))))

  ;; Kill sexp: kill from cursor to end of next sexp
  (define (cmd-kill-sexp es)
    (let* ([gb (editor-state-gb es)]
           [kr (editor-state-kr es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)]
           [target (forward-sexp-end text pos)])
      (when target
        (let ([killed (substring text pos target)])
          (do ([i 0 (+ i 1)])
              ((= i (- target pos)))
            (gap-buffer-delete-forward! gb))
          (kill-ring-push! kr killed)
          (editor-state-last-yank-len-set! es 0)))))

  ;; Splice sexp: remove enclosing parens, keeping contents
  (define (cmd-splice-sexp es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)])
      (let-values ([(open-idx close-idx) (find-enclosing-parens text pos)])
        (when (and open-idx close-idx)
          (let* ([new-text (string-append
                             (substring text 0 open-idx)
                             (substring text (+ open-idx 1) close-idx)
                             (substring text (+ close-idx 1) (string-length text)))]
                 [new-pos (- pos 1)])
            (editor-replace-text! gb new-text new-pos))))))

  ;; Raise sexp: replace enclosing list with sexp at point
  (define (cmd-raise-sexp es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)])
      (let-values ([(open-idx close-idx) (find-enclosing-parens text pos)])
        (when (and open-idx close-idx)
          (let ([end (forward-sexp-end text pos)])
            (when end
              ;; Find actual start (skip whitespace from pos)
              (let* ([start (let skip ([i pos])
                              (if (and (< i end) (char-whitespace? (string-ref text i)))
                                  (skip (+ i 1))
                                  i))]
                     [sexp-text (substring text start end)]
                     [new-text (string-append
                                 (substring text 0 open-idx)
                                 sexp-text
                                 (substring text (+ close-idx 1) (string-length text)))]
                     [new-pos open-idx])
                (editor-replace-text! gb new-text new-pos))))))))

  ;; Wrap round: wrap next sexp in parens
  (define (cmd-wrap-round es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)]
           [end (forward-sexp-end text pos)])
      (if end
          (let* ([new-text (string-append
                             (substring text 0 pos)
                             "("
                             (substring text pos end)
                             ")"
                             (substring text end (string-length text)))]
                 [new-pos (+ pos 1)])
            (editor-replace-text! gb new-text new-pos))
          ;; No sexp found, just insert ()
          (begin
            (gap-buffer-insert! gb #\()
            (gap-buffer-insert! gb #\))
            (gap-buffer-move-cursor! gb -1)))))

  ;; Forward slurp: pull next sexp into current list
  ;; (a b|) c → (a b| c)
  (define (cmd-forward-slurp es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)])
      (let-values ([(open-idx close-idx) (find-enclosing-parens text pos)])
        (when (and open-idx close-idx)
          (let ([target (forward-sexp-end text (+ close-idx 1))])
            (when target
              (let* ([close-ch (string-ref text close-idx)]
                     [new-text (string-append
                                 (substring text 0 close-idx)
                                 (substring text (+ close-idx 1) target)
                                 (string close-ch)
                                 (substring text target (string-length text)))])
                (editor-replace-text! gb new-text pos))))))))

  ;; Forward barf: push last element out of current list
  ;; (a b| c) → (a b|) c
  ;; Trims whitespace: the space before the barfed element stays outside.
  (define (cmd-forward-barf es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)])
      (let-values ([(open-idx close-idx) (find-enclosing-parens text pos)])
        (when (and open-idx close-idx)
          (let ([sexp-start (backward-sexp-start text close-idx)])
            (when (and sexp-start (> sexp-start open-idx))
              ;; Trim whitespace before the barfed sexp (it goes outside)
              (let* ([trim-start (let skip ([i (- sexp-start 1)])
                                   (if (and (> i open-idx) (char-whitespace? (string-ref text i)))
                                       (skip (- i 1))
                                       (+ i 1)))]
                     [close-ch (string-ref text close-idx)]
                     [new-text (string-append
                                 (substring text 0 trim-start)
                                 (string close-ch)
                                 " "
                                 (substring text sexp-start close-idx)
                                 (substring text (+ close-idx 1) (string-length text)))]
                     [new-pos (if (>= pos sexp-start) trim-start pos)])
                (editor-replace-text! gb new-text new-pos))))))))

  ;; Backward slurp: pull previous sexp into current list
  ;; a (|b c) → (a |b c)
  (define (cmd-backward-slurp es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)])
      (let-values ([(open-idx close-idx) (find-enclosing-parens text pos)])
        (when (and open-idx close-idx)
          (let ([target (backward-sexp-start text open-idx)])
            (when target
              (let* ([open-ch (string-ref text open-idx)]
                     [new-text (string-append
                                 (substring text 0 target)
                                 (string open-ch)
                                 (substring text target open-idx)
                                 (substring text (+ open-idx 1) (string-length text)))])
                ;; Cursor stays at same position (removed one before, inserted one before = net 0)
                (editor-replace-text! gb new-text pos))))))))

  ;; Backward barf: push first element out of current list
  ;; (a |b c) → a (|b c)
  (define (cmd-backward-barf es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)])
      (let-values ([(open-idx close-idx) (find-enclosing-parens text pos)])
        (when (and open-idx close-idx)
          (let ([sexp-end (forward-sexp-end text (+ open-idx 1))])
            (when (and sexp-end (< sexp-end close-idx))
              (let* ([open-ch (string-ref text open-idx)]
                     [new-text (string-append
                                 (substring text 0 open-idx)
                                 (substring text (+ open-idx 1) sexp-end)
                                 (string open-ch)
                                 (substring text sexp-end (string-length text)))]
                     ;; Cursor: removal of open_idx shifts left 1, insert at sexp-end-1 shifts right 1 for after
                     ;; For pos > open-idx and pos < sexp-end: cursor barfed out, clamp inside new list
                     [new-pos (if (< pos sexp-end)
                                  (max (- pos 1) (- sexp-end 1))
                                  pos)])
                (editor-replace-text! gb new-text new-pos))))))))

  ;; Up list: move to enclosing open paren (C-M-u)
  (define (cmd-up-list es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)])
      (let-values ([(open-idx close-idx) (find-enclosing-parens text pos)])
        (when open-idx
          (gap-buffer-move-cursor! gb (- open-idx pos))))))

  ;; Down list: move into next nested list (C-M-d)
  (define (cmd-down-list es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)]
           [target (forward-down-list text pos)])
      (when target
        (gap-buffer-move-cursor! gb (- target pos)))))

  ;; Split sexp: split enclosing list at cursor (M-S)
  ;; (foo |bar baz) → (foo) (bar baz)
  ;; Trims whitespace at the split boundary.
  (define (cmd-split-sexp es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)])
      (let-values ([(open-idx close-idx) (find-enclosing-parens text pos)])
        (when (and open-idx close-idx)
          (let* ([open-ch (string-ref text open-idx)]
                 [close-ch (string-ref text close-idx)]
                 ;; Trim trailing whitespace before split point
                 [left-end (let skip ([i (- pos 1)])
                             (if (and (> i open-idx) (char-whitespace? (string-ref text i)))
                                 (skip (- i 1))
                                 (+ i 1)))]
                 ;; Trim leading whitespace after split point
                 [right-start (let skip ([i pos])
                                (if (and (< i close-idx) (char-whitespace? (string-ref text i)))
                                    (skip (+ i 1))
                                    i))]
                 [new-text (string-append
                             (substring text 0 left-end)
                             (string close-ch)
                             " "
                             (string open-ch)
                             (substring text right-start (string-length text)))]
                 ;; Cursor after the inserted open-ch
                 [new-pos (+ left-end 3)])
            (editor-replace-text! gb new-text new-pos))))))

  ;; Join sexps: join two adjacent lists at cursor (M-J)
  ;; (foo) |(bar) → (foo bar)
  (define (cmd-join-sexp es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)]
           [len (string-length text)])
      ;; Find previous close-paren and next open-paren, skipping whitespace
      (let* ([pe (let skip ([i (- pos 1)])
                   (cond
                     [(< i 0) #f]
                     [(char-whitespace? (string-ref text i)) (skip (- i 1))]
                     [else i]))]
             [ns (let skip ([i pos])
                   (cond
                     [(>= i len) #f]
                     [(char-whitespace? (string-ref text i)) (skip (+ i 1))]
                     [else i]))])
        (when (and pe ns
                   (closing-delimiter? (string-ref text pe))
                   (opening-delimiter? (string-ref text ns)))
          (let* ([new-text (string-append
                             (substring text 0 pe)
                             " "
                             (substring text (+ ns 1) len))]
                 [new-pos (+ pe 1)])
            (editor-replace-text! gb new-text new-pos))))))

  ;; ======================================================================
  ;; Default keymap
  ;; ======================================================================

  ;; Base emacs-style bindings (movement, kill, yank, history, search, screen).
  (define (bind-base-keys! km)
    ;; Movement (Ctrl + Emacs)
    (keymap-bind! km (list (make-key-event 'ctrl #\a 0)) cmd-beginning-of-line)
    (keymap-bind! km (list (make-key-event 'ctrl #\e 0)) cmd-end-of-line)
    (keymap-bind! km (list (make-key-event 'ctrl #\f 0)) cmd-forward-char)
    (keymap-bind! km (list (make-key-event 'ctrl #\b 0)) cmd-backward-char)
    (keymap-bind! km (list (make-key-event 'meta #\f 0)) cmd-forward-word)
    (keymap-bind! km (list (make-key-event 'meta #\b 0)) cmd-backward-word)

    ;; Arrow keys and Home/End
    (keymap-bind! km (list (make-key-event 'special 'left 0)) cmd-backward-char)
    (keymap-bind! km (list (make-key-event 'special 'right 0)) cmd-forward-char)
    (keymap-bind! km (list (make-key-event 'special 'home 0)) cmd-beginning-of-line)
    (keymap-bind! km (list (make-key-event 'special 'end 0)) cmd-end-of-line)
    (keymap-bind! km (list (make-key-event 'special 'left MOD_ALT)) cmd-backward-word)
    (keymap-bind! km (list (make-key-event 'special 'right MOD_ALT)) cmd-forward-word)

    ;; Kill (Emacs)
    (keymap-bind! km (list (make-key-event 'ctrl #\k 0)) cmd-kill-line)
    (keymap-bind! km (list (make-key-event 'ctrl #\w 0)) cmd-backward-kill-word)
    (keymap-bind! km (list (make-key-event 'meta #\d 0)) cmd-kill-word)
    (keymap-bind! km (list (make-key-event 'special 'backspace MOD_ALT)) cmd-backward-kill-word)

    ;; Yank
    (keymap-bind! km (list (make-key-event 'ctrl #\y 0)) cmd-yank)
    (keymap-bind! km (list (make-key-event 'meta #\y 0)) cmd-yank-pop)

    ;; History navigation
    (keymap-bind! km (list (make-key-event 'special 'up 0)) cmd-history-prev)
    (keymap-bind! km (list (make-key-event 'special 'down 0)) cmd-history-next)
    (keymap-bind! km (list (make-key-event 'ctrl #\p 0)) cmd-history-prev)
    (keymap-bind! km (list (make-key-event 'ctrl #\n 0)) cmd-history-next)

    ;; Reverse incremental search
    (keymap-bind! km (list (make-key-event 'ctrl #\r 0)) cmd-reverse-search)

    ;; Screen
    (keymap-bind! km (list (make-key-event 'ctrl #\l 0)) cmd-clear-screen))

  ;; Paredit key sequences -- stored for unbind iteration
  (define paredit-key-sequences
    (list
      (list (make-key-event 'ctrl #\f MOD_ALT))
      (list (make-key-event 'ctrl #\b MOD_ALT))
      (list (make-key-event 'ctrl #\u MOD_ALT))
      (list (make-key-event 'ctrl #\d MOD_ALT))
      (list (make-key-event 'ctrl #\k MOD_ALT))
      (list (make-key-event 'special 'right MOD_CTRL))
      (list (make-key-event 'special 'left MOD_CTRL))
      (list (make-key-event 'special 'right (bitwise-ior MOD_CTRL MOD_ALT)))
      (list (make-key-event 'special 'left (bitwise-ior MOD_CTRL MOD_ALT)))
      (list (make-key-event 'meta #\) 0))
      (list (make-key-event 'meta #\} 0))
      (list (make-key-event 'meta #\{ 0))
      (list (make-key-event 'meta #\s 0))
      (list (make-key-event 'meta #\r 0))
      (list (make-key-event 'meta #\( 0))
      (list (make-key-event 'meta #\S 0))
      (list (make-key-event 'meta #\J 0))))

  ;; Paredit commands corresponding to paredit-key-sequences (same order)
  (define paredit-commands
    (list
      cmd-forward-sexp
      cmd-backward-sexp
      cmd-up-list
      cmd-down-list
      cmd-kill-sexp
      cmd-forward-slurp
      cmd-forward-barf
      cmd-backward-barf
      cmd-backward-slurp
      cmd-forward-slurp
      cmd-forward-barf
      cmd-backward-barf
      cmd-splice-sexp
      cmd-raise-sexp
      cmd-wrap-round
      cmd-split-sexp
      cmd-join-sexp))

  ;; Structural editing bindings (paredit / smartparens / Doom Emacs).
  (define (bind-paredit-keys! km)
    (for-each
      (lambda (seq cmd) (keymap-bind! km seq cmd))
      paredit-key-sequences
      paredit-commands))

  ;; Remove all paredit bindings from a keymap.
  (define (unbind-paredit-keys! km)
    (for-each
      (lambda (seq) (keymap-unbind! km seq))
      paredit-key-sequences))

  ;; Shared structural and emacs-style bindings applied to both keymaps.
  ;; Backward-compatible wrapper calling both base and paredit layers.
  (define (bind-common-keys! km)
    (bind-base-keys! km)
    (bind-paredit-keys! km))

  ;; ======================================================================
  ;; Paredit toggle state
  ;; ======================================================================

  (define *paredit-enabled* #t)

  (define (paredit-enabled?) *paredit-enabled*)

  (define (enable-paredit!)
    (unless *paredit-enabled*
      (bind-paredit-keys! editor-insert-keymap)
      (bind-paredit-keys! editor-normal-keymap)
      (set! *paredit-enabled* #t)))

  (define (disable-paredit!)
    (when *paredit-enabled*
      (unbind-paredit-keys! editor-insert-keymap)
      (unbind-paredit-keys! editor-normal-keymap)
      (set! *paredit-enabled* #f)))

  (define (toggle-paredit!)
    (if *paredit-enabled* (disable-paredit!) (enable-paredit!)))

  ;; Build the insert-mode keymap (emacs bindings + auto-pair self-insert).
  ;; Enter always inserts newline. Escape switches to normal mode.
  (define (make-insert-keymap)
    (let ([km (make-keymap)])
      (bind-common-keys! km)

      ;; Delete
      (keymap-bind! km (list (make-key-event 'ctrl #\d 0)) cmd-delete-char)
      (keymap-bind! km (list (make-key-event 'special 'backspace 0)) cmd-delete-backward-char)
      (keymap-bind! km (list (make-key-event 'special 'delete 0)) cmd-delete-char)

      ;; Kill whole line
      (keymap-bind! km (list (make-key-event 'ctrl #\u 0)) cmd-kill-whole-line)

      ;; Transpose
      (keymap-bind! km (list (make-key-event 'ctrl #\t 0)) cmd-transpose-chars)

      ;; Smart Return: submit balanced, newline for unbalanced, no-op for empty
      (keymap-bind! km (list (make-key-event 'special 'return 0)) cmd-smart-return)

      ;; C-j (LF) submits from insert mode (like readline accept-line)
      (keymap-bind! km (list (make-key-event 'special 'newline 0)) cmd-submit)

      ;; Escape -> normal mode
      (keymap-bind! km (list (make-key-event 'special 'escape 0)) cmd-enter-normal-mode)

      km))

  ;; Build the normal-mode keymap (vim motions + lispy structural editing).
  ;; Enter always submits for eval. Single-letter keys are commands, not self-insert.
  (define (make-normal-keymap)
    (let ([km (make-keymap)])
      (bind-common-keys! km)

      ;; ---- Vim motions ----
      (keymap-bind! km (list (make-key-event 'char #\h 0)) cmd-backward-char)
      (keymap-bind! km (list (make-key-event 'char #\l 0)) cmd-forward-char)
      (keymap-bind! km (list (make-key-event 'char #\w 0)) cmd-forward-word)
      (keymap-bind! km (list (make-key-event 'char #\b 0)) cmd-backward-word)
      (keymap-bind! km (list (make-key-event 'char #\e 0)) cmd-forward-word)
      (keymap-bind! km (list (make-key-event 'char #\0 0)) cmd-beginning-of-line)
      (keymap-bind! km (list (make-key-event 'char #\^ 0)) cmd-beginning-of-line)
      (keymap-bind! km (list (make-key-event 'char #\$ 0)) cmd-end-of-line)

      ;; ---- Sexp navigation (lispy-style) ----
      (keymap-bind! km (list (make-key-event 'char #\) 0)) cmd-forward-sexp)
      (keymap-bind! km (list (make-key-event 'char #\( 0)) cmd-backward-sexp)
      (keymap-bind! km (list (make-key-event 'char #\{ 0)) cmd-up-list)
      (keymap-bind! km (list (make-key-event 'char #\} 0)) cmd-down-list)

      ;; ---- Insert mode entry ----
      (keymap-bind! km (list (make-key-event 'char #\i 0)) cmd-enter-insert-mode)
      (keymap-bind! km (list (make-key-event 'char #\a 0)) cmd-enter-insert-after)
      (keymap-bind! km (list (make-key-event 'char #\I 0)) cmd-enter-insert-bol)
      (keymap-bind! km (list (make-key-event 'char #\A 0)) cmd-enter-insert-eol)
      (keymap-bind! km (list (make-key-event 'char #\o 0)) cmd-open-below)
      (keymap-bind! km (list (make-key-event 'char #\O 0)) cmd-open-above)

      ;; ---- Editing ----
      (keymap-bind! km (list (make-key-event 'char #\x 0)) cmd-delete-char)
      (keymap-bind! km (list (make-key-event 'char #\X 0)) cmd-delete-backward-char)
      (keymap-bind! km (list (make-key-event 'char #\D 0)) cmd-kill-line)
      (keymap-bind! km (list (make-key-event 'char #\C 0))
        (lambda (es) (cmd-kill-line es) (cmd-enter-insert-mode es)))
      (keymap-bind! km (list (make-key-event 'char #\s 0)) cmd-substitute-char)
      (keymap-bind! km (list (make-key-event 'char #\S 0)) cmd-substitute-line)
      (keymap-bind! km (list (make-key-event 'char #\p 0)) cmd-paste-after)
      (keymap-bind! km (list (make-key-event 'char #\P 0)) cmd-paste-before)
      (keymap-bind! km (list (make-key-event 'special 'backspace 0)) cmd-backward-char)
      (keymap-bind! km (list (make-key-event 'special 'delete 0)) cmd-delete-char)

      ;; ---- d prefix (delete operator) ----
      (keymap-bind! km (list (make-key-event 'char #\d 0) (make-key-event 'char #\d 0))
        cmd-kill-whole-line)
      (keymap-bind! km (list (make-key-event 'char #\d 0) (make-key-event 'char #\w 0))
        cmd-kill-word)
      (keymap-bind! km (list (make-key-event 'char #\d 0) (make-key-event 'char #\e 0))
        cmd-kill-word)
      (keymap-bind! km (list (make-key-event 'char #\d 0) (make-key-event 'char #\$ 0))
        cmd-kill-line)

      ;; ---- c prefix (change operator = delete + insert mode) ----
      (keymap-bind! km (list (make-key-event 'char #\c 0) (make-key-event 'char #\c 0))
        (lambda (es) (cmd-kill-whole-line es) (cmd-enter-insert-mode es)))
      (keymap-bind! km (list (make-key-event 'char #\c 0) (make-key-event 'char #\w 0))
        (lambda (es) (cmd-kill-word es) (cmd-enter-insert-mode es)))
      (keymap-bind! km (list (make-key-event 'char #\c 0) (make-key-event 'char #\e 0))
        (lambda (es) (cmd-kill-word es) (cmd-enter-insert-mode es)))
      (keymap-bind! km (list (make-key-event 'char #\c 0) (make-key-event 'char #\$ 0))
        (lambda (es) (cmd-kill-line es) (cmd-enter-insert-mode es)))

      ;; ---- Structural (lispy-style single keys) ----
      (keymap-bind! km (list (make-key-event 'char #\> 0)) cmd-forward-slurp)
      (keymap-bind! km (list (make-key-event 'char #\< 0)) cmd-forward-barf)

      ;; Enter always submits in normal mode
      (keymap-bind! km (list (make-key-event 'special 'return 0)) cmd-submit)

      km))

  ;; Module-level keymap instances.
  (define editor-insert-keymap (make-insert-keymap))
  (define editor-normal-keymap (make-normal-keymap))
  ;; Default keymap (for backward compat) points to insert keymap.
  (define editor-default-keymap editor-insert-keymap)

  ;; Module-level kill ring instance.
  (define editor-kill-ring (make-kill-ring))
  (define editor-history (open-history))

  ;; ======================================================================
  ;; Reverse incremental search state (module-level, one editor at a time)
  ;; ======================================================================

  (define search-active? #f)
  (define search-query "")
  (define search-match-idx -1)
  (define search-saved-buffer "")

  (define (reset-search-state!)
    (set! search-active? #f)
    (set! search-query "")
    (set! search-match-idx -1)
    (set! search-saved-buffer ""))

  ;; ======================================================================
  ;; Prefix-filtered history state
  ;; ======================================================================

  (define history-prefix #f)  ; #f or string prefix for filtered Up/Down

  ;; ======================================================================
  ;; Tab completion state (module-level, one editor at a time)
  ;; ======================================================================

  (define completion-candidates '())   ; list of completion strings
  (define completion-index -1)         ; selected candidate index (-1 = none)
  (define completion-start 0)          ; cursor position where prefix starts

  (define (dismiss-completion!)
    (unless (null? completion-candidates)
      (set! completion-candidates '())
      (set! completion-index -1)
      (set! completion-start 0)))

  ;; ======================================================================
  ;; Completion helpers
  ;; ======================================================================

  ;; word-at-cursor: extract partial symbol/token before cursor.
  ;; Returns (values prefix start-pos).
  ;; Word boundaries: whitespace, parens, brackets, quotes, semicolons, backtick, comma.
  (define (word-at-cursor gb)
    (let* ([pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)])
      (let loop ([i (- pos 1)])
        (cond
          [(< i 0)
           (values (substring text 0 pos) 0)]
          [(let ([ch (string-ref text i)])
             (or (char-whitespace? ch)
                 (memv ch '(#\( #\) #\[ #\] #\" #\; #\' #\` #\,))))
           (values (substring text (+ i 1) pos) (+ i 1))]
          [else (loop (- i 1))]))))

  ;; path-at-cursor: extract path fragment from inside a string.
  ;; Scans backward from cursor to opening quote.
  ;; Returns path string or #f.
  (define (path-at-cursor gb)
    (let* ([pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)])
      (let loop ([i (- pos 1)])
        (cond
          [(< i 0) #f]
          [(char=? (string-ref text i) #\")
           (substring text (+ i 1) pos)]
          [else (loop (- i 1))]))))

  ;; symbol-completions: filter environment-symbols by prefix match.
  ;; Returns sorted list of matching symbol-name strings.
  (define (symbol-completions prefix)
    (let* ([syms (environment-symbols (interaction-environment))]
           [prefix-len (string-length prefix)]
           [matches (filter
                      (lambda (s)
                        (let ([name (symbol->string s)])
                          (and (>= (string-length name) prefix-len)
                               (string=? prefix (substring name 0 prefix-len)))))
                      syms)]
           [names (map symbol->string matches)])
      (list-sort string<? names)))

  ;; filename-completions: list directory entries matching path prefix.
  ;; Handles ~ expansion to HOME.
  (define (filename-completions prefix)
    (let* ([expanded (if (and (> (string-length prefix) 0)
                              (char=? (string-ref prefix 0) #\~))
                         (let ([home (or (getenv "HOME") "")])
                           (string-append home (substring prefix 1 (string-length prefix))))
                         prefix)]
           [len (string-length expanded)])
      ;; Split into directory and basename parts
      (let* ([last-slash (let loop ([i (- len 1)])
                           (cond
                             [(< i 0) #f]
                             [(char=? (string-ref expanded i) #\/) i]
                             [else (loop (- i 1))]))]
             [dir (if last-slash
                      (if (= last-slash 0) "/" (substring expanded 0 (+ last-slash 1)))
                      ".")]
             [base-prefix (if last-slash
                              (substring expanded (+ last-slash 1) len)
                              expanded)]
             [base-prefix-len (string-length base-prefix)])
        (guard (e [#t '()])  ; non-existent dir -> empty list
          (let* ([entries (directory-list dir)]
                 [matches (filter
                            (lambda (entry)
                              (and (>= (string-length entry) base-prefix-len)
                                   (string=? base-prefix (substring entry 0 base-prefix-len))
                                   ;; Skip . and .. unless prefix starts with .
                                   (or (> base-prefix-len 0)
                                       (not (or (string=? entry ".") (string=? entry ".."))))))
                            entries)]
                 [full-paths (map (lambda (entry)
                                    (if last-slash
                                        (string-append
                                          (if (= last-slash 0) "/" (substring expanded 0 (+ last-slash 1)))
                                          entry)
                                        entry))
                                  matches)])
            (list-sort string<? full-paths))))))

  ;; shell-completions: filter PATH executable names by prefix match.
  ;; Returns sorted list of matching executable name strings.
  (define (shell-completions prefix)
    (if (string=? prefix "")
        '()
        (let* ([prefix-len (string-length prefix)]
               [ht (path-cache)]
               [keys (vector->list (hashtable-keys ht))]
               [matches (filter
                          (lambda (name)
                            (and (>= (string-length name) prefix-len)
                                 (string=? (substring name 0 prefix-len) prefix)))
                          keys)])
          (list-sort string<? matches))))

  ;; Detect whether the buffer looks like shell mode (no Scheme prefix characters).
  (define (shell-mode-buffer? text)
    (let ([len (string-length text)])
      (let loop ([i 0])
        (cond
          [(>= i len) #t]
          [(memv (string-ref text i) scheme-prefix-chars) #f]
          [else (loop (+ i 1))]))))

  ;; Detect whether cursor is on the first token (command position).
  ;; Returns #t if there is no non-whitespace character before the start of the current word.
  (define (first-token-position? text word-start)
    (let loop ([i (- word-start 1)])
      (cond
        [(< i 0) #t]
        [(char-whitespace? (string-ref text i)) (loop (- i 1))]
        [else #f])))

  ;; longest-common-prefix: find shared prefix across a list of strings.
  (define (longest-common-prefix strs)
    (cond
      [(null? strs) ""]
      [(null? (cdr strs)) (car strs)]
      [else
       (let* ([first (car strs)]
              [flen (string-length first)])
         (let loop ([i 0])
           (cond
             [(>= i flen) first]
             [else
              (let ([ch (string-ref first i)])
                (if (for-all
                      (lambda (s)
                        (and (> (string-length s) i)
                             (char=? (string-ref s i) ch)))
                      (cdr strs))
                    (loop (+ i 1))
                    (substring first 0 i)))])))]))

  ;; Helper: find opening-quote position scanning backward from pos.
  (define (find-opening-quote text pos)
    (let loop ([i (- pos 1)])
      (cond
        [(< i 0) 0]
        [(char=? (string-ref text i) #\") (+ i 1)]
        [else (loop (- i 1))])))

  ;; Helper: apply completions to editor state.
  ;; replace-start is cursor position where the prefix starts.
  ;; candidates is a non-empty list of match strings.
  (define (apply-completions! gb text pos replace-start candidates)
    (cond
      [(null? (cdr candidates))
       ;; Single match: insert directly
       (let* ([match (car candidates)]
              [new-text (string-append
                          (substring text 0 replace-start)
                          match
                          (substring text pos (string-length text)))]
              [new-pos (+ replace-start (string-length match))])
         (editor-replace-text! gb new-text new-pos))]
      [else
       ;; Multiple matches: insert longest common prefix, populate menu
       (let* ([lcp (longest-common-prefix candidates)]
              [new-text (string-append
                          (substring text 0 replace-start)
                          lcp
                          (substring text pos (string-length text)))]
              [new-pos (+ replace-start (string-length lcp))])
         (set! completion-candidates candidates)
         (set! completion-index -1)
         (set! completion-start replace-start)
         (editor-replace-text! gb new-text new-pos))]))

  ;; cmd-complete: Tab completion command.
  (define (cmd-complete es)
    (let ([gb (editor-state-gb es)])
      (cond
        ;; Menu already showing: cycle to next candidate
        [(not (null? completion-candidates))
         (let* ([n (length completion-candidates)]
                [new-idx (modulo (+ completion-index 1) n)]
                [candidate (list-ref completion-candidates new-idx)]
                [text (gap-buffer->string gb)]
                [pos (gap-buffer-cursor-pos gb)]
                [new-text (string-append
                            (substring text 0 completion-start)
                            candidate
                            (substring text pos (string-length text)))]
                [new-pos (+ completion-start (string-length candidate))])
           (set! completion-index new-idx)
           (editor-replace-text! gb new-text new-pos))]

        ;; Fresh Tab press: determine context and compute candidates
        [else
         (let* ([text (gap-buffer->string gb)]
                [pos (gap-buffer-cursor-pos gb)]
                [len (string-length text)]
                [state (lexer-state-at text pos)])
           (cond
             ;; In string: filename completion
             [(eq? state 'in-string)
              (let ([path (path-at-cursor gb)])
                (when path
                  (let ([candidates (filename-completions path)]
                        [quote-pos (find-opening-quote text pos)])
                    (unless (null? candidates)
                      (apply-completions! gb text pos quote-pos candidates)))))]
             ;; Shell mode: no Scheme prefix chars in buffer
             [(and (> len 0) (shell-mode-buffer? text))
              (let-values ([(prefix start) (word-at-cursor gb)])
                (when (> (string-length prefix) 0)
                  (let ([candidates
                         (if (first-token-position? text start)
                             ;; First token: PATH executables merged with filenames
                             (let* ([shell-cands (shell-completions prefix)]
                                    [file-cands (filename-completions prefix)]
                                    ;; Merge and deduplicate via hashtable
                                    [ht (make-hashtable string-hash string=?)])
                               (for-each (lambda (s) (hashtable-set! ht s #t)) shell-cands)
                               (for-each (lambda (s) (hashtable-set! ht s #t)) file-cands)
                               (list-sort string<? (vector->list (hashtable-keys ht))))
                             ;; Subsequent tokens: filenames only
                             (filename-completions prefix))])
                    (unless (null? candidates)
                      (apply-completions! gb text pos start candidates)))))]
             ;; Normal Scheme context: symbol completion
             [else
              (let-values ([(prefix start) (word-at-cursor gb)])
                (when (> (string-length prefix) 0)
                  (let ([candidates (symbol-completions prefix)])
                    (unless (null? candidates)
                      (apply-completions! gb text pos start candidates)))))]))])))


  ;; cmd-complete-prev: Shift-Tab / Up during completion: cycle backward
  (define (cmd-complete-prev es)
    (let ([gb (editor-state-gb es)])
      (when (not (null? completion-candidates))
        (let* ([n (length completion-candidates)]
               [new-idx (modulo (- (if (= completion-index -1) 0 completion-index) 1) n)]
               [candidate (list-ref completion-candidates new-idx)]
               [text (gap-buffer->string gb)]
               [len (string-length text)]
               [pos (gap-buffer-cursor-pos gb)]
               [new-text (string-append
                           (substring text 0 completion-start)
                           candidate
                           (substring text pos len))]
               [new-pos (+ completion-start (string-length candidate))])
          (set! completion-index new-idx)
          (editor-replace-text! gb new-text new-pos)))))

  ;; cmd-complete-accept: Return during completion: accept and dismiss
  (define (cmd-complete-accept es)
    (dismiss-completion!))

  ;; Track number of completion menu lines currently displayed
  (define completion-menu-lines 0)

  ;; Render edit line + optional completion menu.
  ;; Returns new prev-lines value for the next render call.
  (define (render-with-menu port prompt gb prev-lines)
    ;; render-line with extra lines accounting for previous menu
    (let ([lines (render-line port prompt gb (+ prev-lines completion-menu-lines))])
      (if (not (null? completion-candidates))
          ;; Save cursor, move to end of content, render menu, restore cursor
          (begin
            (display "\x1b;7" port)  ; save cursor position
            ;; Move cursor to end of buffer content (past all lines)
            (let* ([text (string-append (gap-buffer-before-string gb)
                                        (gap-buffer-after-string gb))]
                   [before (gap-buffer-before-string gb)]
                   [total-lines (let ([len (string-length text)])
                                  (let lp ([i 0] [n 0])
                                    (if (>= i len) n
                                        (lp (+ i 1) (if (char=? (string-ref text i) #\newline)
                                                        (+ n 1) n)))))]
                   [cursor-row (let ([blen (string-length before)])
                                 (let lp ([i 0] [n 0])
                                   (if (>= i blen) n
                                       (lp (+ i 1) (if (char=? (string-ref before i) #\newline)
                                                       (+ n 1) n)))))]
                   [lines-after (- total-lines cursor-row)])
              ;; Move down to end of content
              (when (> lines-after 0)
                (display "\x1b;[" port)
                (display lines-after port)
                (display "B" port))
              ;; Move to beginning of next line
              (display "\r" port)
              ;; Render menu
              (let ([menu-lines (render-completion-menu port
                                  completion-candidates
                                  completion-index)])
                ;; Restore cursor to edit position
                (display "\x1b;8" port)
                (flush-output-port port)
                (set! completion-menu-lines menu-lines)
                lines)))
          (begin
            (set! completion-menu-lines 0)
            lines))))

  ;; ======================================================================
  ;; Bracketed paste handling
  ;; ======================================================================

  ;; Handle bracketed paste: read key events until paste-end, inserting
  ;; each character directly into the gap buffer (no auto-pairing, no keymap).
  (define (handle-bracketed-paste es in-port)
    (let ([gb (editor-state-gb es)])
      (let loop ()
        (let ([evt (read-key-event in-port)])
          (cond
            [(eof-object? evt) (void)]  ; unexpected EOF during paste
            ;; paste-end: stop
            [(and (eq? (key-event-type evt) 'special)
                  (eq? (key-event-value evt) 'paste-end))
             (void)]
            ;; Regular character: insert literally
            [(and (eq? (key-event-type evt) 'char)
                  (char? (key-event-value evt)))
             (gap-buffer-insert! gb (key-event-value evt))
             (loop)]
            ;; Control char that maps to printable (e.g. return -> newline)
            [(and (eq? (key-event-type evt) 'special)
                  (eq? (key-event-value evt) 'return))
             (gap-buffer-insert! gb #\newline)
             (loop)]
            [(and (eq? (key-event-type evt) 'special)
                  (eq? (key-event-value evt) 'newline))
             (gap-buffer-insert! gb #\newline)
             (loop)]
            [(and (eq? (key-event-type evt) 'special)
                  (eq? (key-event-value evt) 'tab))
             (gap-buffer-insert! gb #\tab)
             (loop)]
            ;; Anything else during paste: skip
            [else (loop)])))))

  ;; ======================================================================
  ;; read-expression: the main entry point
  ;; ======================================================================

  ;; read-expression: read a line of input with emacs-style editing.
  ;; prompt: prompt string to display
  ;; in-port: input port to read key events from (default: current-input-port)
  ;; out-port: output port for rendering (default: console-output-port)
  ;; Returns: string (user input) or eof-object (C-d on empty buffer)
  (define read-expression
    (case-lambda
      [(prompt) (read-expression prompt (current-input-port) (console-output-port))]
      [(prompt in-port) (read-expression prompt in-port (console-output-port))]
      [(prompt in-port out-port)
       ;; Split prompt into prefix (all lines up to last newline) and last line.
       ;; Only the last line is re-rendered on each keypress; the prefix is displayed once.
       (let* ([last-nl (let loop ([i (- (string-length prompt) 1)])
                         (cond
                           [(< i 0) #f]
                           [(char=? (string-ref prompt i) #\newline) i]
                           [else (loop (- i 1))]))]
              [prompt-prefix (if last-nl (substring prompt 0 (+ last-nl 1)) "")]
              [prompt (if last-nl
                          (substring prompt (+ last-nl 1) (string-length prompt))
                          prompt)]
              [gb (make-gap-buffer)]
              [es (make-editor-state gb editor-kill-ring prompt out-port 0 #f #f 'insert)]
              [finish! (lambda (result)
                         (reset-search-state!)
                         (set! history-prefix #f)
                         (dismiss-completion!)
                         (display "\n" out-port)
                         (reset-cursor out-port)
                         (flush-output-port out-port)
                         result)])
         ;; Display multi-line prefix once (e.g. starship context lines)
         (when (> (string-length prompt-prefix) 0)
           (display prompt-prefix out-port)
           (flush-output-port out-port))
         ;; Set initial cursor shape + colour (bar/blue for insert mode)
         (set-cursor-bar out-port)
         (flush-output-port out-port)
         ;; Render initial edit line (last line of prompt + empty buffer)
         (let ([initial-lines (render-line out-port prompt gb 0)])
         ;; Command loop — prev-lines tracks screen lines for correct cursor-up
         (let loop ([prev-lines initial-lines])
           (let* ([mode (editor-state-mode es)]
                  [km (if (eq? mode 'normal) editor-normal-keymap editor-insert-keymap)]
                  [evt (read-key-event in-port)])
             (cond
               [(eof-object? evt)
                (finish! (if (= (gap-buffer-length gb) 0)
                             (eof-object)
                             (gap-buffer->string gb)))]
               [else
                ;; effective-prompt: search mode overrides the normal prompt
                (let* ([ep (if search-active?
                               (string-append "(reverse-i-search)'" search-query "': ")
                               prompt)]
                       [compl-active? (not (null? completion-candidates))])
                  (cond
                    ;; ------- Completion menu intercepts (highest priority) -------
                    [(and compl-active?
                          (eq? (key-event-type evt) 'special)
                          (eq? (key-event-value evt) 'backtab))
                     (cmd-complete-prev es)
                     (loop (render-with-menu out-port ep gb prev-lines))]
                    [(and compl-active?
                          (eq? (key-event-type evt) 'special)
                          (eq? (key-event-value evt) 'return))
                     (dismiss-completion!)
                     (set! completion-menu-lines 0)
                     (loop (render-with-menu out-port ep gb prev-lines))]
                    [(and compl-active?
                          (or (and (eq? (key-event-type evt) 'special)
                                   (eq? (key-event-value evt) 'escape))
                              (and (eq? (key-event-type evt) 'ctrl)
                                   (eqv? (key-event-value evt) #\g))))
                     (dismiss-completion!)
                     (set! completion-menu-lines 0)
                     (loop (render-with-menu out-port ep gb prev-lines))]

                    ;; ------- Bracketed paste -------
                    [(and (eq? (key-event-type evt) 'special)
                          (eq? (key-event-value evt) 'paste-start))
                     (when compl-active? (dismiss-completion!) (set! completion-menu-lines 0))
                     (when search-active? (exit-search-mode! es #f))
                     (handle-bracketed-paste es in-port)
                     (if (editor-state-done? es)
                         (finish! (editor-state-result es))
                         (loop (render-with-menu out-port ep gb prev-lines)))]

                    ;; ------- Search mode -------
                    [search-active?
                     (cond
                       [(and (eq? (key-event-type evt) 'ctrl)
                             (eqv? (key-event-value evt) #\r))
                        (cmd-reverse-search es)
                        (loop (render-with-menu out-port ep gb prev-lines))]
                       [(or (and (eq? (key-event-type evt) 'ctrl)
                                 (eqv? (key-event-value evt) #\g))
                            (and (eq? (key-event-type evt) 'special)
                                 (eq? (key-event-value evt) 'escape)))
                        (exit-search-mode! es #t)
                        (loop (render-with-menu out-port prompt gb prev-lines))]
                       [(and (eq? (key-event-type evt) 'char)
                             (char? (key-event-value evt))
                             (>= (char->integer (key-event-value evt)) 32))
                        (search-self-insert! es (key-event-value evt))
                        (loop (render-with-menu out-port ep gb prev-lines))]
                       [(and (eq? (key-event-type evt) 'special)
                             (eq? (key-event-value evt) 'backspace))
                        (when (> (string-length search-query) 0)
                          (set! search-query
                            (substring search-query 0 (- (string-length search-query) 1)))
                          (let ([entries (history-entries editor-history)])
                            (let ([idx (history-search-backward editor-history search-query
                                         (- (vector-length entries) 1))])
                              (when idx
                                (set! search-match-idx idx)
                                (gap-buffer-set-from-string! gb
                                  (vector-ref entries idx))))))
                        (loop (render-with-menu out-port ep gb prev-lines))]
                       [else
                        (exit-search-mode! es #f)
                        (let ([binding (keymap-lookup km evt)])
                          (cond
                            [(procedure? binding)
                             (binding es)
                             (if (editor-state-done? es)
                                 (finish! (editor-state-result es))
                                 (loop (render-with-menu out-port prompt gb prev-lines)))]
                            [else
                             (loop (render-with-menu out-port prompt gb prev-lines))]))])]

                    ;; ------- Normal dispatch -------
                    [else
                     ;; Dismiss completion on non-Tab keys
                     (when (and compl-active?
                                (not (and (eq? (key-event-type evt) 'special)
                                          (eq? (key-event-value evt) 'tab))))
                       (dismiss-completion!)
                       (set! completion-menu-lines 0))
                     (let ([binding (keymap-lookup km evt)])
                       (cond
                         [(procedure? binding)
                          (binding es)
                          (if (editor-state-done? es)
                              (finish! (editor-state-result es))
                              (loop (render-with-menu out-port prompt gb prev-lines)))]
                         [(keymap? binding)
                          (let ([next-evt (read-key-event in-port)])
                            (if (eof-object? next-evt)
                                (loop (render-with-menu out-port prompt gb prev-lines))
                                (let ([sub-binding (keymap-lookup binding next-evt)])
                                  (when (procedure? sub-binding)
                                    (sub-binding es))
                                  (if (editor-state-done? es)
                                      (finish! (editor-state-result es))
                                      (loop (render-with-menu out-port prompt gb prev-lines))))))]
                         [(and (eq? mode 'insert)
                               (eq? (key-event-type evt) 'char)
                               (let ([ch (key-event-value evt)])
                                 (and (char? ch)
                                      (>= (char->integer ch) 32))))
                          (cmd-self-insert es (key-event-value evt))
                          (if (editor-state-done? es)
                              (finish! (editor-state-result es))
                              (loop (render-with-menu out-port prompt gb prev-lines)))]
                         [else
                          (loop prev-lines)]))])
                ) ; close let*
              ]   ; close [else of eof/else cond
              )                          ; close eof/else cond
            )                            ; close let*
          )                              ; close let loop
          )                              ; close let ([initial-lines])
        )                                ; close let* ([last-nl])
      ]))                                ; close case-lambda + define

  ;; Bind Tab to completion (must follow cmd-complete definition, placed at end
  ;; of library so all definitions precede expressions as R6RS requires).
  (keymap-bind! editor-insert-keymap
    (list (make-key-event 'special 'tab 0)) cmd-complete)

) ; end library
