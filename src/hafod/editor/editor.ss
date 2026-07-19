;;; (hafod editor editor) -- Line editor core: raw mode, emacs bindings, read-expression
;;; Combines gap-buffer, kill-ring, sexp-tracker, input-decode, keymap, and render
;;; into a working line editor with emacs keybindings.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor editor)
  (export read-expression with-raw-mode editor-default-keymap
          editor-insert-keymap editor-normal-keymap
          ;; Keymap constructors + the insert->normal transition command
          ;; (white-box: exported so the default-on suite can build FRESH keymaps
          ;; with no configuration and assert their bindings directly -- including
          ;; that the insert keymap maps Escape to cmd-enter-normal-mode, so the
          ;; fully-featured normal/visual mode is reachable out of the box with no
          ;; opt-in.  Mirrors the eager module instances above; deliberately NOT
          ;; re-exported by the (hafod) umbrella, like editor-insert-keymap.)
          make-insert-keymap make-normal-keymap cmd-enter-normal-mode
          ;; Exported for testing
          make-editor-state editor-state-gb editor-state-done? editor-state-result
          editor-state-mode editor-state-mode-set!
          editor-state-mark editor-state-mark-set!
          ;; Derived selection span + mode indicator (exported for testing)
          editor-selection-range editor-mode-indicator
          ;; Text mode-indicator toggle: OFF by default, opt-in from a user's
          ;; init (white-box editor export; deliberately not re-exported by the
          ;; (hafod) umbrella, like editor-mode-indicator itself)
          show-mode-indicator?
          ;; Completion helpers (exported for testing)
          word-at-cursor symbol-completions filename-completions
          longest-common-prefix path-at-cursor
          ;; Completion menu anchor decision (exported for testing)
          menu-anchor-place
          ;; Auto-suggestion ghost seam (exported for testing)
          history-ghost-suffix
          ;; Shell-mode completion
          shell-completions
          ;; Keymap layers
          bind-base-keys! bind-paredit-keys! unbind-paredit-keys!
          ;; Paredit toggle
          toggle-paredit! paredit-enabled? enable-paredit! disable-paredit!
          ;; Structural s-expression selection (white-box: tests + reset hooks)
          cmd-select-sexp cmd-expand-region cmd-contract-region
          reset-sexp-stack!
          ;; S-expression transpose (white-box: tests); cmd-undo exported so the
          ;; transpose suite can prove a no-op pushes no undo snapshot
          cmd-transpose-sexp cmd-transpose-sexp-backward cmd-undo
          ;; Paste commands (white-box: exported so the register suite can drive
          ;; p / P directly and assert the selected-register insertion)
          cmd-paste-after cmd-paste-before
          ;; Dot-repeat: the committed last change's recorded keystroke bytes
          ;; (white-box: exported so the repeat suite can assert the recording is
          ;; the keystrokes of the change, not a descriptor)
          editor-dot-last-change
          ;; Dot-repeat: PTY-free drive harness that runs the real recording
          ;; boundary + dispatch over a keystroke script (white-box, for the suite
          ;; -- the decoder's ESC-timing makes a pure read-expression script unable
          ;; to enter normal mode mid-stream)
          editor-drive-keys!
          ;; Configurable structural keybindings (white-box): each key is held in
          ;; a parameter that bind-sexp-keys! reads, so an init can rebind by
          ;; setting the parameter and re-invoking bind-sexp-keys!
          sexp-expand-key sexp-contract-key sexp-transpose-key
          sexp-drag-fwd-key sexp-drag-back-key bind-sexp-keys!
          ;; History access (for history expansion and mode tagging)
          editor-history-entries editor-history-set-last-mode!
          editor-history-entry-mode
          ;; Finder injection (set by umbrella to break circular dependency)
          editor-finder-proc
          ;; Feature toggles
          fuzzy-finder? tab-completions?
          ;; History recall search mode: 'substring (default) matches the typed
          ;; needle anywhere in a past line, 'prefix keeps the strict
          ;; head-anchored behaviour.  A user-facing parameter published through
          ;; the umbrella later; exported here so the recall suite drives it.
          history-search-mode
          ;; Command-head abbreviations: the pure cores, table accessors and the
          ;; Space command are white-box exports the suite drives directly;
          ;; abbr-expand? is user-facing and re-exported by the umbrella later.
          expand-abbr-head expand-first-abbr
          abbr-set! abbr-remove! abbr-ref abbr-expand?
          cmd-expand-abbr-or-space
          ;; Help
          show-keybindings run-tutorial)
  (import (chezscheme)
          (hafod editor gap-buffer)
          (hafod editor kill-ring)
          (hafod editor sexp-tracker)
          (hafod editor input-decode)
          (hafod editor keymap)
          (hafod editor render)  ;; render-line, render-line/suggestion, etc.
          (hafod editor history)
          (hafod editor vi)
          (hafod editor help)
          (hafod fuzzy)
          (hafod tty)
          (only (hafod terminal-caps) ansi-ok? colour-ok?)
          (only (hafod shell classifier) path-cache scheme-prefix-chars)
          (only (hafod shell completers)
                lookup-completer user-completer command-flag-completer)
          (only (hafod collect) run/strings*)
          (only (hafod process) exec-path))

  ;; ======================================================================
  ;; Terminal size query
  ;; ======================================================================

  ;; Query the terminal column count, falling back to 80 off a terminal.  Reads
  ;; the SIGWINCH-refreshed cache in the shared tty leaf, so a per-render call
  ;; costs an O(1) cell read rather than a live TIOCGWINSZ ioctl every frame.
  (define (editor-query-terminal-cols)
    (let-values ([(rows cols) (cached-terminal-size)]) cols))

  ;; Query the terminal row count, falling back to 24 off a terminal.  Reads the
  ;; same cached (values rows cols) as editor-query-terminal-cols but keeps the
  ;; rows it discards -- the drop-up decision needs the terminal height.
  (define (editor-query-terminal-rows)
    (let-values ([(rows cols) (cached-terminal-size)]) rows))

  ;; ======================================================================
  ;; Raw mode
  ;; ======================================================================

  ;; with-raw-mode: enter raw mode on fd, run thunk, restore on exit.
  ;; A thin wrapper over the terminal owner's with-raw-mode*, which owns the
  ;; termios save/set/restore and the suspend handling for the extent of the raw
  ;; session (and keeps signals enabled, so Ctrl-C / Ctrl-Z still arrive as
  ;; signals while editing).  This wrapper adds only the bracketed-paste
  ;; enable/disable pair, gated on the console's capability so the enter and its
  ;; matching leave stay balanced and no escape leaks to a non-capable target.
  (define (with-raw-mode fd thunk)
    (with-raw-mode* fd
      (lambda ()
        (dynamic-wind
          (lambda ()
            ;; Enable bracketed paste mode (only when the console is a capable terminal)
            (when (ansi-ok? (console-output-port))
              (display "\x1b;[?2004h" (console-output-port)))
            (flush-output-port (console-output-port)))
          thunk
          (lambda ()
            ;; Disable bracketed paste mode -- gated on the same predicate as the
            ;; enable above so the enter/leave pair stays balanced.
            (when (ansi-ok? (console-output-port))
              (display "\x1b;[?2004l" (console-output-port)))
            (flush-output-port (console-output-port)))))))

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
      (mutable mode)           ;; 'insert or 'normal
      (mutable mark)))         ;; emacs mark: integer index when a region is set, #f otherwise

  ;; ======================================================================
  ;; Selection span + mode indicator (fed to the selection-aware renderer)
  ;; ======================================================================

  ;; Resolve the ONE span to highlight into a (start . end) index pair over the
  ;; buffer text (the gap-buffer-cursor-pos coordinate space), or #f when nothing
  ;; is selected.  The vi visual selection takes priority, but only outside insert
  ;; mode: while typing (insert) a still-active visual is ignored -- mirroring the
  ;; same insert gate on editor-mode-indicator -- so a visual left set on the
  ;; previous line cannot paint a phantom highlight as the user types.  When a
  ;; visual mode is active in a non-insert buffer the span is the vi visual range
  ;; (inclusive characterwise / linewise, from vi.ss).  Otherwise, when an emacs
  ;; mark is set, the span is the half-open mark..point region -- min..max with
  ;; NO +1, since the region excludes point, unlike the inclusive vi range --
  ;; clamped into [0, buffer-length].  With neither active there is no selection.
  (define (editor-selection-range es)
    (cond
      [(and (not (eq? (editor-state-mode es) 'insert)) (vi-visual-mode))
       (vi-visual-range (editor-state-gb es))]
      [else
       (let ([m (editor-state-mark es)])
         (and (integer? m)
              (let* ([gb (editor-state-gb es)]
                     [len (string-length (gap-buffer->string gb))]
                     [p (gap-buffer-cursor-pos gb)]
                     [lo (max 0 (min (min m p) len))]
                     [hi (max lo (min (max m p) len))])
                (cons lo hi))))]))

  ;; Optional text mode indicator, OFF by default.  The active mode is already
  ;; visible through the cursor shape and colour -- steady block / yellow in a
  ;; non-insert mode, steady bar / blue in insert -- so the textual
  ;; "-- NORMAL --" row is redundant and switched off out of the box.  A user's
  ;; init may switch it on with (show-mode-indicator? #t); it then shows on its
  ;; own row below the edit line whenever the mode is not insert.  Coerced to a
  ;; strict boolean, mirroring the fuzzy-finder? / tab-completions? toggles.
  (define show-mode-indicator?
    (make-parameter #f (lambda (v) (and v #t))))

  ;; The compact mode indicator to show, or #f when nothing should be drawn:
  ;; #f whenever the text indicator is switched off (the default) or the buffer
  ;; is in insert mode (nothing is shown while typing).  Otherwise defer to
  ;; vi-mode-indicator -- which already yields "-- VISUAL --" / "-- VISUAL LINE
  ;; --" / the operator strings -- and supply the plain-normal "-- NORMAL --"
  ;; fallback it returns #f for, so a non-insert mode always shows something.
  (define (editor-mode-indicator es)
    (if (or (not (show-mode-indicator?))
            (eq? (editor-state-mode es) 'insert))
        #f
        (or (vi-mode-indicator) "-- NORMAL --")))

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

  ;; Delete forward without EOF — for normal-mode x/Delete where empty buffer is a no-op.
  (define (cmd-delete-char-no-eof es)
    (let ([len (gap-buffer-length (editor-state-gb es))])
      (unless (= len 0)
        (cmd-delete-char es))))

  ;; Delete forward (C-d / Delete key) — structural: splices non-empty delimiters.
  (define (cmd-delete-char es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [len (gap-buffer-length gb)]
           [_ (when (and (> len 0) (< pos len)) (editor-snapshot! gb))])
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
        (editor-snapshot! gb)
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
           [_ (editor-snapshot! gb)]
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

  ;; ======================================================================
  ;; Emacs mark and region
  ;; ======================================================================

  ;; A region is active when the mark holds an integer index; #f means unset.
  (define (region-active? es)
    (and (editor-state-mark es) #t))

  ;; C-Space: set the mark at the current cursor position.  No snapshot and no
  ;; buffer mutation -- the mark is a bookmark, not an edit.
  ;;
  ;; Known limitation: the mark is stored as a fixed buffer index and is not
  ;; adjusted when text is inserted or deleted before it.  Editing between
  ;; set-mark and a copy or kill therefore drifts the region -- mark..point no
  ;; longer spans the text that was marked (the defensive clamp keeps it in
  ;; range but cannot recover the intended span).  The common flow -- set the
  ;; mark, move point WITHOUT editing, then copy or kill -- is unaffected.
  (define (cmd-set-mark es)
    (editor-state-mark-set! es (gap-buffer-cursor-pos (editor-state-gb es))))

  ;; M-w: copy the region mark..point into the kill-ring WITHOUT mutating the
  ;; buffer, then deactivate the region.  The span is clamped into [0,len] with
  ;; s <= e (the vi-yank-range! idiom) so a mark left past a since-shrunk buffer
  ;; cannot substring out of range; an empty span pushes nothing.
  (define (cmd-copy-region es)
    (when (region-active? es)
      (let* ([gb (editor-state-gb es)]
             [text (gap-buffer->string gb)]
             [len (string-length text)]
             [m (editor-state-mark es)]
             [p (gap-buffer-cursor-pos gb)]
             [s (max 0 (min (min m p) len))]
             [e (max s (min (max m p) len))])
        (when (< s e)
          (kill-ring-push! (editor-state-kr es) (substring text s e)))
        ;; Copying to the kill-ring breaks the yank-pop chain, exactly as a kill
        ;; does: a following M-y must start a fresh replace, not delete against a
        ;; length left over from an earlier C-y.
        (editor-state-last-yank-len-set! es 0)
        (editor-state-mark-set! es #f))))

  ;; C-w with a region active: kill mark..point into the kill-ring, snapshotting
  ;; first so the edit is undoable, then deactivate the region.  With NO region
  ;; active, delegate to backward-kill-word so the bound key keeps its historical
  ;; behaviour.  Same clamped span as cmd-copy-region.
  (define (cmd-kill-region es)
    (if (region-active? es)
        (let* ([gb (editor-state-gb es)]
               [text (gap-buffer->string gb)]
               [len (string-length text)]
               [m (editor-state-mark es)]
               [p (gap-buffer-cursor-pos gb)]
               [s (max 0 (min (min m p) len))]
               [e (max s (min (max m p) len))])
          (when (< s e)
            ;; Snapshot only when the region is non-empty, so an empty region
            ;; (mark == point) leaves no no-op entry on the undo stack.
            (editor-snapshot! gb)
            (kill-ring-push! (editor-state-kr es) (substring text s e))
            (editor-replace-text! gb
              (string-append (substring text 0 s) (substring text e len))
              s))
          (editor-state-last-yank-len-set! es 0)
          (editor-state-mark-set! es #f))
        (cmd-backward-kill-word es)))

  (define (cmd-kill-word es)
    (let* ([gb (editor-state-gb es)]
           [_ (editor-snapshot! gb)]
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
           [_ (editor-snapshot! gb)]
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
           [_ (editor-snapshot! gb)]
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
      (when (ansi-ok? port)
        (display "\x1b;[2J\x1b;[H" port))
      (flush-output-port port)))

  ;; Yank commands
  (define (cmd-yank es)
    (let* ([gb (editor-state-gb es)]
           [_ (editor-snapshot! gb)]
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
           [_ (editor-snapshot! gb)]
           [pos (gap-buffer-cursor-pos gb)]
           [len (gap-buffer-length gb)])
      (when (and (>= pos 2) (<= pos len))
        ;; If cursor is at end of buffer, transpose the two chars before it
        ;; If cursor is in middle, transpose char before cursor with char at cursor
        (let* ([swap-pos (if (= pos len) (- pos 2) (- pos 1))]
               [text (gap-buffer->string gb)]
               [ch1 (string-ref text swap-pos)]
               [ch2 (string-ref text (+ swap-pos 1))]
               [delta (- swap-pos pos)])
          ;; Move to swap-pos, delete both chars, insert swapped
          (gap-buffer-move-cursor! gb delta)
          (gap-buffer-delete-forward! gb)
          (gap-buffer-delete-forward! gb)
          (gap-buffer-insert! gb ch2)
          (gap-buffer-insert! gb ch1)))))

  ;; ======================================================================
  ;; Evil-mode: modal editing (normal / insert)
  ;; ======================================================================

  ;; Cursor shape + colour via ANSI (shape) and OSC 12 (colour)
  ;; Doom Emacs palette: insert=#51afef (blue), normal=#ECBE7B (yellow)
  ;; Shape is cursor control (gated on ansi-ok?); the colour tint is an OSC-12
  ;; sequence (gated on colour-ok?), so a capable terminal with colour disabled
  ;; keeps the vi-mode shape but drops the tint.  set/reset gate on matching
  ;; predicates so a suppressed set has a harmless suppressed reset.
  (define (set-cursor-block port)
    (when (ansi-ok? port)
      (display "\x1b;[2 q" port))            ; steady block
    (when (colour-ok? port)
      (display "\x1b;]12;#ECBE7B\x7;" port)))  ; yellow cursor colour
  (define (set-cursor-bar port)
    (when (ansi-ok? port)
      (display "\x1b;[6 q" port))            ; steady bar
    (when (colour-ok? port)
      (display "\x1b;]12;#51afef\x7;" port)))  ; blue cursor colour
  (define (reset-cursor port)
    (when (colour-ok? port)
      (display "\x1b;]112\x7;" port))           ; reset cursor colour to terminal default
    (when (ansi-ok? port)
      (display "\x1b;[0 q" port)))              ; reset cursor shape to terminal default

  ;; Enter normal mode (Escape from insert)
  (define (cmd-enter-normal-mode es)
    (editor-state-mode-set! es 'normal)
    ;; Vim convention: cursor moves left 1 when leaving insert
    (let ([gb (editor-state-gb es)])
      (when (> (gap-buffer-cursor-pos gb) 0)
        (gap-buffer-move-cursor! gb -1)))
    (set-cursor-block (editor-state-out-port es)))

  ;; Enter insert mode at cursor (i).  Dismiss any active vi visual first: every
  ;; insert-entry key (i a I A o O s S C, and the vi c operator) funnels through
  ;; here, so clearing the visual at this one chokepoint stops a selection open in
  ;; normal mode from lingering as a highlight once typing begins.
  (define (cmd-enter-insert-mode es)
    (vi-clear-visual!)
    (reset-sexp-stack!)
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
             ;; Balanced and in normal state: submit only if cursor is
             ;; at or past the last non-whitespace character (i.e. on/after
             ;; the final closing paren), otherwise insert newline.
             [(and (= depth 0) (eq? state 'normal))
              (let ([cursor-pos (gap-buffer-cursor-pos gb)]
                    [last-non-ws (let lp ([k (- len 1)])
                                   (cond [(< k 0) 0]
                                         [(char-whitespace? (string-ref text k))
                                          (lp (- k 1))]
                                         [else k]))])
                (if (>= cursor-pos last-non-ws)
                    (cmd-submit es)
                    (cmd-insert-newline es)))]
             ;; Unbalanced or in string/comment: insert newline
             [else
              (cmd-insert-newline es)]))])))

  ;; Always submit for eval (Enter in normal mode)
  (define (cmd-submit es)
    (let ([gb (editor-state-gb es)])
      ;; Fish "expand on execute": expand a pending command-head abbreviation
      ;; into the visible buffer before the line is taken, so both the submitted
      ;; line and its history entry carry the expansion.  Gated on abbr-expand?
      ;; so the toggle's "nothing expands when off" contract holds on Enter just
      ;; as it does on Space; with the toggle off the buffer is submitted
      ;; verbatim.  A Scheme buffer is left untouched by the shell-context gate
      ;; inside expand-first-abbr.  Folded into one undo step via editor-snapshot!
      ;; for symmetry with the Space path.
      (when (abbr-expand?)
        (let-values ([(nt nc) (expand-first-abbr (gap-buffer->string gb))])
          (when nt
            (editor-snapshot! gb)
            (editor-replace-text! gb nt nc))))
      (let ([text (gap-buffer->string gb)])
        (history-add! editor-history text)
        (history-reset-nav! editor-history)
        (editor-state-done?-set! es #t)
        (editor-state-result-set! es text))))

  ;; Helper: find the line number (0-based) of a position in a string
  (define (cursor-line text pos)
    (let loop ([i 0] [line 0])
      (cond
        [(>= i pos) line]
        [(char=? (string-ref text i) #\newline) (loop (+ i 1) (+ line 1))]
        [else (loop (+ i 1) line)])))

  ;; Helper: count total lines in text
  (define (text-line-count text)
    (let loop ([i 0] [n 0])
      (cond
        [(>= i (string-length text)) n]
        [(char=? (string-ref text i) #\newline) (loop (+ i 1) (+ n 1))]
        [else (loop (+ i 1) n)])))

  ;; Helper: move cursor to equivalent column on previous line
  (define (cmd-move-up es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)]
           [pos (gap-buffer-cursor-pos gb)]
           ;; Find start of current line
           [line-start (let scan ([i (- pos 1)])
                         (cond
                           [(< i 0) 0]
                           [(char=? (string-ref text i) #\newline) (+ i 1)]
                           [else (scan (- i 1))]))]
           [col (- pos line-start)]
           ;; Find start of previous line
           [prev-end (- line-start 1)]  ; position of newline ending prev line
           [prev-start (let scan ([i (- prev-end 1)])
                         (cond
                           [(< i 0) 0]
                           [(char=? (string-ref text i) #\newline) (+ i 1)]
                           [else (scan (- i 1))]))]
           [prev-len (- prev-end prev-start)]
           [target (+ prev-start (min col prev-len))])
      (gap-buffer-move-cursor! gb (- target pos))))

  ;; Helper: move cursor to equivalent column on next line
  (define (cmd-move-down es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)]
           [pos (gap-buffer-cursor-pos gb)]
           [len (string-length text)]
           ;; Find start of current line
           [line-start (let scan ([i (- pos 1)])
                         (cond
                           [(< i 0) 0]
                           [(char=? (string-ref text i) #\newline) (+ i 1)]
                           [else (scan (- i 1))]))]
           [col (- pos line-start)]
           ;; Find start of next line
           [next-start (let scan ([i pos])
                         (cond
                           [(>= i len) #f]
                           [(char=? (string-ref text i) #\newline) (+ i 1)]
                           [else (scan (+ i 1))]))])
      (when next-start
        (let* ([next-end (let scan ([i next-start])
                           (cond
                             [(>= i len) len]
                             [(char=? (string-ref text i) #\newline) i]
                             [else (scan (+ i 1))]))]
               [next-len (- next-end next-start)]
               [target (+ next-start (min col next-len))])
          (gap-buffer-move-cursor! gb (- target pos))))))

  ;; History: navigate to previous (older) entry with optional prefix filtering.
  ;; In multi-line buffers, moves cursor up within the text instead of
  ;; going to history, unless already on the first line.
  (define (cmd-history-prev es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)]
           [pos (gap-buffer-cursor-pos gb)]
           [line (cursor-line text pos)])
      (if (> line 0)
          ;; Not on first line: move up within buffer
          (cmd-move-up es)
          ;; On first line: history navigation
          (begin
            (when (= (history-cursor editor-history) -1)
              (history-save-input! editor-history text)
              (if (and (> (string-length text) 0) (> pos 0))
                  (set! history-prefix (substring text 0 pos))
                  (set! history-prefix #f)))
            (if history-prefix
                (let* ([cur (history-cursor editor-history)]
                       [start (if (= cur -1) (- (history-count editor-history) 1) (- cur 1))]
                       ;; Branch the backward scan on the search mode: the default
                       ;; matches the needle anywhere in a line, 'prefix keeps the
                       ;; head-anchored search.  cmd-history-next's forward loop
                       ;; branches on the same mode through the same predicate, so
                       ;; Up and Down agree on which entries match.
                       [idx (if (eq? (history-search-mode) 'prefix)
                                (history-prefix-search-backward editor-history history-prefix start)
                                (history-substring-search-backward editor-history history-prefix start))])
                  (when idx
                    (history-cursor-set! editor-history idx)
                    (gap-buffer-set-from-string! gb (history-ref editor-history idx))))
                (let ([entry (history-prev editor-history)])
                  (when entry
                    (gap-buffer-set-from-string! gb entry))))))))

  ;; History: navigate to next (newer) entry with optional prefix filtering.
  ;; In multi-line buffers, moves cursor down within the text instead of
  ;; going to history, unless already on the last line.
  (define (cmd-history-next es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)]
           [pos (gap-buffer-cursor-pos gb)]
           [line (cursor-line text pos)]
           [last-line (text-line-count text)]
           [cur (history-cursor editor-history)])
      (if (< line last-line)
          ;; Not on last line: move down within buffer
          (cmd-move-down es)
          ;; On last line: history navigation
          (if (and history-prefix (not (= cur -1)))
              (let ([len (history-count editor-history)])
                (let loop ([i (+ cur 1)])
                  (cond
                    [(>= i len)
                     (history-cursor-set! editor-history -1)
                     (set! history-prefix #f)
                     (gap-buffer-set-from-string! gb (history-saved-input editor-history))]
                    ;; Same mode dispatch as the backward scan in
                    ;; cmd-history-prev, through the shared smart-substring-match?
                    ;; predicate, so the forward cycle matches exactly the set the
                    ;; backward scan does (Up and Down never diverge).
                    [(if (eq? (history-search-mode) 'prefix)
                         (string-prefix? history-prefix (history-ref editor-history i))
                         (smart-substring-match? history-prefix (history-ref editor-history i)))
                     (history-cursor-set! editor-history i)
                     (gap-buffer-set-from-string! gb (history-ref editor-history i))]
                    [else (loop (+ i 1))])))
              (let ([entry (history-next editor-history)])
                (when entry
                  (gap-buffer-set-from-string! gb entry)))))))

  ;; ======================================================================
  ;; Fuzzy finder editor commands (history, file, directory pickers)
  ;; ======================================================================

  ;; Deduplicate history entries, returning most-recent-first list.
  ;; Entries vector is oldest-first (index 0 = oldest, len-1 = newest).
  ;; Iterate newest-to-oldest, skip duplicates.  cons builds the list
  ;; so that index 0 (= oldest unique) ends up at the head — then reverse
  ;; gives newest-first.
  ;; Deduplicate history entries (most recent wins).
  ;; Returns (values item-list mode-hashtable) where mode-hashtable maps
  ;; entry-string -> mode-symbol for use by the finder's colouring logic.
  (define (deduplicate-history entries)
    (let* ([len (vector-length entries)]
           [seen (make-hashtable string-hash string=?)]
           [mode-ht (make-hashtable string-hash string=?)])
      (let loop ([i (fx- len 1)] [acc '()])
        (if (fx< i 0)
            (values (reverse acc) mode-ht)
            (let ([entry (vector-ref entries i)])
              (if (or (not (string? entry))
                      (fx= (string-length entry) 0)
                      (hashtable-ref seen entry #f))
                  (loop (fx- i 1) acc)
                  (begin
                    (hashtable-set! seen entry #t)
                    (hashtable-set! mode-ht entry
                      (editor-history-entry-mode i))
                    (loop (fx- i 1) (cons entry acc)))))))))

  ;; Recursive file walk with depth limit, skipping hidden entries.
  (define (walk-files dir max-depth)
    (if (fx<= max-depth 0)
        '()
        (guard (e [#t '()])
          (let loop ([entries (directory-list dir)] [acc '()])
            (if (null? entries)
                acc
                (let ([name (car entries)])
                  (if (char=? (string-ref name 0) #\.)
                      (loop (cdr entries) acc)
                      (let ([path (string-append dir "/" name)])
                        (if (file-directory? path)
                            (loop (cdr entries)
                                  (append (walk-files path (fx- max-depth 1)) acc))
                            (loop (cdr entries) (cons path acc)))))))))))

  ;; Collect files: prefer git ls-files, fall back to walk.
  (define (collect-files)
    (let ([files (guard (e [#t '()])
                   (run/strings* (lambda () (exec-path "git" "ls-files"))))])
      (if (null? files)
          (walk-files "." 8)
          files)))

  ;; Collect directories: recursive walk from "." skipping hidden entries.
  (define (collect-directories)
    (let walk ([dir "."] [depth 8])
      (if (fx<= depth 0)
          '()
          (guard (e [#t '()])
            (let loop ([entries (directory-list dir)] [acc '()])
              (if (null? entries)
                  acc
                  (let ([name (car entries)])
                    (if (char=? (string-ref name 0) #\.)
                        (loop (cdr entries) acc)
                        (let ([path (if (string=? dir ".")
                                        name
                                        (string-append dir "/" name))])
                          (if (file-directory? path)
                              (loop (cdr entries)
                                    (cons path (append (walk path (fx- depth 1)) acc)))
                              (loop (cdr entries) acc)))))))))))

  ;; Quote a path for shell use.
  (define (quote-path s)
    (string-append "\"" s "\""))

  ;; Fuzzy history search: Ctrl-R
  (define (cmd-fuzzy-history es)
    (let ([finder (and (fuzzy-finder?) (editor-finder-proc))])
      (when finder
        (let* ([gb (editor-state-gb es)])
          (let-values ([(items mode-map) (deduplicate-history (history-entries editor-history))])
            (let ([result (finder items "> " #t mode-map #t)])
              (when result
                (gap-buffer-set-from-string! gb result))))))))

  ;; File picker: Ctrl-T
  (define (cmd-file-picker es)
    (let ([finder (and (fuzzy-finder?) (editor-finder-proc))])
      (when finder
        (let* ([gb (editor-state-gb es)]
               [files (collect-files)]
               [result (finder files "> ")])
          (when result
            ;; Insert a space before if cursor is adjacent to non-whitespace
            (let ([pos (gap-buffer-cursor-pos gb)])
              (when (and (> pos 0)
                         (not (char-whitespace?
                                (gap-buffer-char-at gb (- pos 1)))))
                (gap-buffer-insert! gb #\space)))
            (gap-buffer-insert-string! gb result))))))

  ;; Directory picker: Alt-C
  (define (cmd-dir-picker es)
    (let ([finder (and (fuzzy-finder?) (editor-finder-proc))])
      (when finder
        (let* ([gb (editor-state-gb es)]
               [dirs (collect-directories)]
               [result (finder dirs "> ")])
          (when result
            (gap-buffer-set-from-string! gb (string-append "cd " (quote-path result)))
            (editor-state-done?-set! es #t)
            (editor-state-result-set! es (gap-buffer->string (editor-state-gb es))))))))

  ;; Paste after cursor (p). Consult the selected vi register first: a named
  ;; register inserts its snapshot under a fresh undo step; the unnamed default
  ;; (vi-take-paste-register! returns #f) falls back to the kill-ring exactly as
  ;; before, so emacs kills and a dd-then-p still paste. The cursor advance keeps
  ;; the after-cursor placement for both paths.
  (define (cmd-paste-after es)
    (dot-taint-undo/redo/paste!)
    (let* ([gb (editor-state-gb es)]
           [reg-text (vi-take-paste-register!)])
      (when (< (gap-buffer-cursor-pos gb) (gap-buffer-length gb))
        (gap-buffer-move-cursor! gb 1))
      (if (string? reg-text)
          (begin
            (editor-snapshot! gb)
            (gap-buffer-insert-string! gb reg-text))
          (cmd-yank es))))

  ;; Paste before cursor (P). As cmd-paste-after but without the cursor advance,
  ;; so a named register (or the kill-ring fallback for the unnamed default)
  ;; lands before the cursor.
  (define (cmd-paste-before es)
    (dot-taint-undo/redo/paste!)
    (let* ([gb (editor-state-gb es)]
           [reg-text (vi-take-paste-register!)])
      (if (string? reg-text)
          (begin
            (editor-snapshot! gb)
            (gap-buffer-insert-string! gb reg-text))
          (cmd-yank es))))

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
           [_ (editor-snapshot-for-insert! gb ch)]
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
           [_ (editor-snapshot! gb)]
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
  ;; Structural s-expression selection (select / expand / contract)
  ;; ======================================================================
  ;;
  ;; A single command selects the innermost meaningful unit at point; expand and
  ;; contract then walk the nesting with a true retrace stack.  All three feed the
  ;; ONE visible selection through the emacs mark + cursor -- apply-span! sets the
  ;; mark at one end and moves the cursor to the other, so editor-selection-range
  ;; paints the span in both insert (emacs) and normal modes, with no second
  ;; highlighter.

  ;; The expansion stack: half-open (start . end) conses, newest first, mirroring
  ;; undo-stack.  It is a module variable, NOT an editor-state field -- a new
  ;; field would change make-editor-state's arity.  reset-sexp-stack! clears it
  ;; from the same chokepoints that clear the selection (insert-entry and submit).
  (define *sexp-expansion-stack* '())
  (define (reset-sexp-stack!) (set! *sexp-expansion-stack* '()))

  ;; Bound the string literal point sits inside: forward-scan to the closing
  ;; quote honouring backslash-escapes, then derive the opening quote by feeding
  ;; the position past the close quote to backward-sexp-start's close-quote
  ;; branch.  Returns (values open close), where close is the index of the closing
  ;; quote, or (values #f #f).  Kept internal to editor.ss (an independent copy so
  ;; this module stays decoupled from vi.ss).
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

  ;; The innermost enclosing form as a half-open span, or (values #f #f).
  (define (form-bounds text pos)
    (let-values ([(open close) (find-enclosing-parens text pos)])
      (if (and open close) (values open (fx+ close 1)) (values #f #f))))

  ;; The innermost meaningful unit at point as a half-open (values start end), or
  ;; (values #f #f).  Dispatch on the lexer state, then on the character at point:
  ;; a string is the whole literal, an opener/closer selects its list, an atom is
  ;; bounded end-first (so a list's first atom is still bounded past its opener),
  ;; and whitespace falls to the enclosing form.  Every primitive's #f collapses
  ;; to (values #f #f) so a malformed or edge position is a clean no-op.
  (define (sexp-at-point text pos)
    (let ([state (lexer-state-at text pos)]
          [len (string-length text)])
      (cond
        [(eq? state 'in-string)
         (let-values ([(open close) (string-bounds-at text pos)])
           (if (and open close) (values open (fx+ close 1)) (values #f #f)))]
        [(memq state '(in-line-comment in-block-comment))
         (form-bounds text pos)]
        [else
         (let ([ch (and (fx< pos len) (string-ref text pos))])
           (cond
             [(and ch (memv ch '(#\( #\[)))
              (let ([close (find-matching-close text pos)])
                (if close (values pos (fx+ close 1)) (values #f #f)))]
             [(and ch (memv ch '(#\) #\])))
              (let ([open (find-matching-paren text pos)])
                (if open (values open (fx+ pos 1)) (values #f #f)))]
             [(and ch (not (char-whitespace? ch)))
              (let* ([end (forward-sexp-end text pos)]
                     [start (and end (backward-sexp-start text end))])
                (if (and start end) (values start end) (values #f #f)))]
             [else (form-bounds text pos)]))])))

  ;; Show the half-open span [start,end) as the ONE visible selection.  Clear any
  ;; active vi visual first (so the mark path is not masked in normal mode), set
  ;; the emacs mark at start, and move the cursor to end.  No snapshot -- a
  ;; selection is not an edit.  editor-selection-range's mark branch then resolves
  ;; (start . end) in both insert and normal mode.
  (define (apply-span! es start end)
    (vi-clear-visual!)
    (editor-state-mark-set! es start)
    (let ([gb (editor-state-gb es)])
      (gap-buffer-move-cursor! gb (- end (gap-buffer-cursor-pos gb)))))

  ;; Select the innermost s-expression at point and seed the expansion stack.  A
  ;; no-op (the selection is left untouched) when there is no meaningful unit --
  ;; an empty buffer, or the whitespace between top-level forms.
  (define (cmd-select-sexp es)
    (let* ([gb (editor-state-gb es)]
           [pos (gap-buffer-cursor-pos gb)]
           [text (gap-buffer->string gb)])
      (reset-sexp-stack!)
      (let-values ([(start end) (sexp-at-point text pos)])
        (when (and start end (fx< start end))
          (apply-span! es start end)
          (set! *sexp-expansion-stack*
                (cons (cons start end) *sexp-expansion-stack*))))))

  ;; The stack is fresh only while the live selection still equals the span at its
  ;; top.  Any cursor move or edit changes editor-selection-range, so the compare
  ;; fails and the next expand transparently re-seeds from the new point -- the
  ;; locked reset condition for a moved cursor or an edited buffer.
  (define (stack-fresh? es)
    (and (pair? *sexp-expansion-stack*)
         (let ([cur (editor-selection-range es)])
           (and cur (equal? cur (car *sexp-expansion-stack*))))))

  ;; Progressive select-then-expand.  On a stale or empty stack (a fresh start)
  ;; this selects the innermost unit at point -- the first press selects, it does
  ;; not also grow.  On a fresh stack it grows the selection to the next enclosing
  ;; form, taken from the START index of the current top span (never the live
  ;; cursor), and no-ops at the outermost form (the empty [0,pos) window there
  ;; yields no enclosing pair).
  (define (cmd-expand-region es)
    (if (not (stack-fresh? es))
        (cmd-select-sexp es)
        (let ([text (gap-buffer->string (editor-state-gb es))])
          (let-values ([(open close)
                        (find-enclosing-parens text (car (car *sexp-expansion-stack*)))])
            (when (and open close)
              (let ([start open] [end (fx+ close 1)])
                (set! *sexp-expansion-stack*
                      (cons (cons start end) *sexp-expansion-stack*))
                (apply-span! es start end)))))))

  ;; True retrace inward: pop the stack head and re-apply exactly the previously
  ;; pushed span, mirroring cmd-undo popping undo-stack.  Only when the stack is
  ;; fresh (still a live structural selection) and a previous span remains --
  ;; never contract past the seed.
  (define (cmd-contract-region es)
    (when (and (stack-fresh? es) (pair? (cdr *sexp-expansion-stack*)))
      (set! *sexp-expansion-stack* (cdr *sexp-expansion-stack*))
      (let ([span (car *sexp-expansion-stack*)])
        (apply-span! es (car span) (cdr span)))))

  ;; Transpose the s-expression AT point with its next sibling (emacs
  ;; transpose-sexps semantics).  Anchor A on the sexp at point bound-by-end:
  ;; end-a bounds A by its end, then backward-sexp-start walks back from that end
  ;; to A's start -- so a cursor sitting on a non-first child anchors on THAT
  ;; child, never on its left sibling.  The next sibling B is found by running
  ;; forward-sexp-end again from A's end.  A safe no-op (no snapshot, no change)
  ;; when there is no next sibling: forward-sexp-end returns #f at a closer or the
  ;; buffer edge, so a last / single child yields end-b #f and the guard fails.
  ;; The swap is a single length-preserving whole-string rebuild applied through
  ;; editor-replace-text! (never char-by-char), so paredit's per-key auto-close
  ;; path cannot fire, the inter-form gap is carried across verbatim, and point
  ;; lands immediately after the moved form.  editor-snapshot! is taken INSIDE the
  ;; guard (guard-then-snapshot, the cmd-kill-region shape) so a no-op leaves the
  ;; undo history untouched.
  (define (cmd-transpose-sexp es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)]
           [pos (gap-buffer-cursor-pos gb)]
           [end-a (forward-sexp-end text pos)]
           [start-a (and end-a (backward-sexp-start text end-a))]
           [end-b (and end-a (forward-sexp-end text end-a))]
           [start-b (and end-b (backward-sexp-start text end-b))])
      (when (and start-a end-a end-b start-b (fx<= end-a start-b))
        (editor-snapshot! gb)
        (let ([new-text (string-append
                          (substring text 0 start-a)
                          (substring text start-b end-b)      ; B moves before A
                          (substring text end-a start-b)      ; inter-form gap
                          (substring text start-a end-a)      ; A moves after B
                          (substring text end-b (string-length text)))])
          (editor-replace-text! gb new-text end-b)))))

  ;; The symmetric mirror of cmd-transpose-sexp: swap the s-expression AT point
  ;; (anchored the SAME bound-by-end way) with the PREVIOUS sibling.  The previous
  ;; sibling P is found by stepping backward-sexp-start from A's start, then
  ;; bounding it with forward-sexp-end.  A safe no-op (no snapshot, no change) when
  ;; there is no previous sibling: backward-sexp-start returns #f at an opener, so
  ;; a first child / top-level first form yields start-p #f and the guard fails.
  ;; Same guard-then-snapshot discipline and length-preserving whole-string
  ;; rebuild; point lands after the moved A in its new, earlier position.
  (define (cmd-transpose-sexp-backward es)
    (let* ([gb (editor-state-gb es)]
           [text (gap-buffer->string gb)]
           [pos (gap-buffer-cursor-pos gb)]
           [end-a (forward-sexp-end text pos)]
           [start-a (and end-a (backward-sexp-start text end-a))]
           [start-p (and start-a (backward-sexp-start text start-a))]
           [end-p (and start-p (forward-sexp-end text start-p))])
      (when (and start-a end-a start-p end-p (fx<= end-p start-a))
        (editor-snapshot! gb)
        (let ([new-text (string-append
                          (substring text 0 start-p)
                          (substring text start-a end-a)      ; A moves before P
                          (substring text end-p start-a)      ; inter-form gap
                          (substring text start-p end-p)      ; P moves after A
                          (substring text end-a (string-length text)))])
          (editor-replace-text! gb new-text (fx+ start-p (fx- end-a start-a)))))))

  ;; ======================================================================
  ;; Undo/Redo state
  ;; ======================================================================

  (define undo-stack '())
  (define redo-stack '())
  (define undo-max 100)
  (define undo-insert-count 0)

  (define (reset-undo-state!)
    (set! undo-stack '())
    (set! redo-stack '())
    (set! undo-insert-count 0))

  (define (undo-push-snapshot! text cursor-pos)
    (set! undo-stack (cons (cons text cursor-pos)
                           (if (>= (length undo-stack) undo-max)
                               (list-head undo-stack (- undo-max 1))
                               undo-stack)))
    (set! redo-stack '())
    (set! undo-insert-count 0))

  (define (editor-snapshot-for-insert! gb ch)
    (cond
      [(or (char-whitespace? ch) (>= undo-insert-count 20))
       (undo-push-snapshot! (gap-buffer->string gb) (gap-buffer-cursor-pos gb))
       (set! undo-insert-count 1)]
      [(= undo-insert-count 0)
       (undo-push-snapshot! (gap-buffer->string gb) (gap-buffer-cursor-pos gb))
       (set! undo-insert-count 1)]
      [else (set! undo-insert-count (+ undo-insert-count 1))]))

  (define (editor-snapshot! gb)
    (undo-push-snapshot! (gap-buffer->string gb) (gap-buffer-cursor-pos gb)))

  (define (cmd-undo es)
    (dot-taint-undo/redo/paste!)
    (let ([gb (editor-state-gb es)])
      (when (pair? undo-stack)
        (let ([current (cons (gap-buffer->string gb) (gap-buffer-cursor-pos gb))]
              [prev (car undo-stack)])
          (set! redo-stack (cons current redo-stack))
          (set! undo-stack (cdr undo-stack))
          (set! undo-insert-count 0)
          (gap-buffer-set-from-string! gb (car prev))
          (let ([target-pos (cdr prev)]
                [len (string-length (car prev))])
            (gap-buffer-move-cursor! gb (- (min target-pos len) (gap-buffer-cursor-pos gb))))))))

  (define (cmd-redo es)
    (dot-taint-undo/redo/paste!)
    (let ([gb (editor-state-gb es)])
      (when (pair? redo-stack)
        (let ([current (cons (gap-buffer->string gb) (gap-buffer-cursor-pos gb))]
              [next (car redo-stack)])
          (set! undo-stack (cons current undo-stack))
          (set! redo-stack (cdr redo-stack))
          (set! undo-insert-count 0)
          (gap-buffer-set-from-string! gb (car next))
          (let ([target-pos (cdr next)]
                [len (string-length (car next))])
            (gap-buffer-move-cursor! gb (- (min target-pos len) (gap-buffer-cursor-pos gb))))))))

  ;; ======================================================================
  ;; Dot-repeat state (the last change's keystrokes + the in-flight recording)
  ;; ======================================================================

  ;; The committed last change: the recorded keystroke bytes of the most recent
  ;; buffer-modifying change (or #f before any change).  Modelled on the
  ;; undo-stack module state above.  This PERSISTS across prompts -- like vim's
  ;; redo register -- so . repeats it on a later line; only reset-undo-state!'s
  ;; sibling reset-dot-recording! (below) clears the *in-flight* recording.
  (define *dot-last-change* #f)
  ;; #t while a change is being recorded at the main-loop boundary.
  (define *dot-recording?* #f)
  ;; The buffer text snapshotted when the current change began; the change is
  ;; committed only when the buffer differs from this at the idle boundary.
  (define *dot-change-start-text* "")
  ;; The initiating key-event of the in-flight change ('unset until the first key
  ;; of the change is read).  Consulted by dot-excluded-event? at commit time.
  (define *dot-initiating-evt* 'unset)
  ;; #t when the in-flight change has invoked a replay (a . or N.).  Such a change
  ;; must never be committed as the last change: dot-excluded-event? catches a
  ;; bare . by its initiating key, but N. begins with a digit, so without this
  ;; flag N. would commit itself ("3.") and a following . would replay a . -- an
  ;; unbounded recursion.  Set by the replay driver, reset at each change-start.
  (define *dot-change-replayed?* #f)
  ;; #t when the in-flight change actually PERFORMED an undo, redo or paste.
  ;; dot-excluded-event? catches a BARE u / p / P / C-r / C-_ / M-/ by the change's
  ;; initiating key, but a leading count makes the initiating key a digit (2u, 3p,
  ;; 2P, 2C-r), slipping past that check -- so the counted undo / redo / paste would
  ;; be committed as the last change and a later . would replay it.  This flag
  ;; classifies the change by what it DID rather than its first key: it is raised
  ;; inside cmd-undo / cmd-redo / cmd-paste-after / cmd-paste-before whenever a
  ;; recording is live, and consulted in the commit guard beside dot-excluded-event?
  ;; (kept as belt-and-braces for the bare forms).  Reset at each change-start,
  ;; mirroring *dot-change-replayed?*.
  (define *dot-change-undo/redo/paste?* #f)

  ;; Raise the undo/redo/paste taint for the in-flight change, but only while a
  ;; change is being recorded -- outside a recording there is nothing to taint (and
  ;; the flag is cleared afresh at the next change-start regardless).
  (define (dot-taint-undo/redo/paste!)
    (when *dot-recording?*
      (set! *dot-change-undo/redo/paste?* #t)))

  ;; Reset the in-flight recording (called once per prompt, beside the vi session
  ;; reset).  The committed last change is deliberately LEFT intact so a change
  ;; made on a previous line stays repeatable on this one.
  (define (reset-dot-recording!)
    (set! *dot-recording?* #f)
    (set! *dot-initiating-evt* 'unset)
    (set! *dot-change-replayed?* #f)
    (set! *dot-change-undo/redo/paste?* #f)
    (current-key-recording #f))

  ;; The recorded bytes of the last change (white-box getter for the test).
  (define (editor-dot-last-change) *dot-last-change*)

  ;; #t when the initiating key of a change must NOT overwrite the last change:
  ;; . itself (else it would self-reference), the vi u undo and p / P paste, the
  ;; emacs undo C-_ and redo M-/, the reverse-history finder C-r, and the
  ;; terminal bracketed-paste start.  In normal mode vi-process-key consumes
  ;; neither C-_ nor M-/ (its #\/ arm is a 'char search, not a 'meta event), so
  ;; both fall through to the normal keymap and MUTATE the buffer via cmd-undo /
  ;; cmd-redo; unless excluded here the boundary would commit an undo/redo as the
  ;; last change and a later . would replay it.  A non-key-event (the 'unset
  ;; sentinel) is never excluded -- the buffer-changed guard already suppresses a
  ;; commit that has no real initiating key.
  (define (dot-excluded-event? evt)
    (and (key-event? evt)
         (let ([type (key-event-type evt)]
               [val (key-event-value evt)])
           (cond
             [(eq? type 'char) (memv val '(#\. #\u #\p #\P))]
             [(eq? type 'ctrl) (or (eqv? val #\_) (eqv? val #\r))]
             [(eq? type 'meta) (eqv? val #\/)]
             [(eq? type 'special) (eq? val 'paste-start)]
             [else #f]))))

  ;; ======================================================================
  ;; Auto-suggestion state (fish-style ghost text)
  ;; ======================================================================

  (define suggestion-text "")  ; the ghost suffix to display after cursor

  ;; history-ghost-suffix: compute the dim ghost suffix to show after the cursor,
  ;; or "" when none.  Keys the search off `before` (the typed before-cursor
  ;; prefix, ignoring paredit's auto-inserted trailing closers) and only offers a
  ;; suggestion when `after` is empty or all closing delimiters/quotes -- i.e. the
  ;; cursor sits at the end of the typed region.
  ;;
  ;; The suffix is drawn from a cascade of sources, most relevant first:
  ;;   1. history PREFIX -- the tail of the most recent past line that BEGINS with
  ;;      the typed text.  This is the original behaviour, unchanged: whenever a
  ;;      line heads with `before` its result is exactly what it was before, and
  ;;      history-prefix-search-backward is untouched.
  ;;   2. history SUBSTRING -- only on a prefix miss, the tail after the typed text
  ;;      found ANYWHERE in a past line, so a mid-line recall still offers where a
  ;;      prefix search would not.
  ;;   3. PATH-cache COMPLETION -- only on a substring miss too, a single
  ;;      unambiguous completion of a bare command head.
  ;; A real past command outranks a synthesised completion, hence the order.  The
  ;; two fallbacks are computed ONLY inside the prefix-miss branch, so a keystroke
  ;; that matches a past prefix pays exactly what it did before -- no substring
  ;; scan and no fuzzy filter on the common path.
  ;;
  ;; Bounds are safe in every arm: arm 1 cuts at the typed-prefix length and
  ;; `before` is a proven string-prefix? of the entry; arm 2 cuts past a matched
  ;; occurrence that lies within the entry; arm 3 cuts at the typed length of a
  ;; candidate proven to have `before` as a literal prefix.
  (define (history-ghost-suffix before after history)
    (if (or (= (string-length before) 0)
            (not (only-closing-delimiters? after)))
        ""
        ;; This runs from update-suggestion! on every render, so it reads the
        ;; history through the O(1) count/ref accessors -- never the right-sized
        ;; history-entries view, which would copy the whole store per keystroke.
        (let* ([n (history-count history)]
               [idx (history-prefix-search-backward history before (- n 1))]
               [prefix-suffix
                (if idx
                    (let ([entry (history-ref history idx)])
                      (substring entry (string-length before) (string-length entry)))
                    "")])
          (if (> (string-length prefix-suffix) 0)
              prefix-suffix                              ; arm 1 -- common path
              ;; Prefix miss: try a mid-line recall, then a lone completion.  Both
              ;; live here so they never run when the prefix source answered.
              (let ([sub-suffix (history-substring-suffix history before (- n 1))])
                (if (> (string-length sub-suffix) 0)
                    sub-suffix                           ; arm 2 -- substring recall
                    (completion-ghost-suffix before))))))) ; arm 3 -- lone completion

  ;; The ghost's last resort: a single unambiguous PATH-cache completion of a BARE
  ;; command head.  Reached only when neither a history prefix nor a history
  ;; substring offered anything, so a keystroke matching a past command never pays
  ;; for the fuzzy pass.  Confined to a bare head -- no whitespace in `before` --
  ;; because argument-position completion would have to read the filesystem, which
  ;; the render hot path must not do; the source is shell-completions, a pure
  ;; fuzzy pass over the PATH-cache keys with no directory read and no subprocess.
  ;; A suffix is offered ONLY when exactly one candidate comes back AND it really
  ;; does begin with the typed text, so the tail is a genuine continuation of it;
  ;; an ambiguous (more than one) or empty result, or a merely-fuzzy candidate
  ;; that does not head with `before`, contributes nothing.
  (define (completion-ghost-suffix before)
    (if (bare-head? before)
        (let ([cands (shell-completions before)])
          (if (and (pair? cands) (null? (cdr cands)))
              (let ([name (car (car cands))])
                (if (string-prefix? before name)
                    (substring name (string-length before) (string-length name))
                    ""))
              ""))
        ""))

  ;; True when `before` is a single bare token -- it holds no whitespace -- so a
  ;; PATH-cache completion of it names a command head rather than an argument.
  (define (bare-head? before)
    (let ([len (string-length before)])
      (let loop ([i 0])
        (cond
          [(>= i len) #t]
          [(char-whitespace? (string-ref before i)) #f]
          [else (loop (+ i 1))]))))

  ;; Update suggestion: search history for a prefix match of the typed text (the
  ;; before-cursor string), ignoring paredit's auto-inserted trailing closers, and
  ;; show when the cursor is at the end of the typed region.  Delegates to the pure
  ;; history-ghost-suffix seam.
  ;; Note: editor-history reads through ensure-editor-history!, so the first
  ;; render's ghost lookup is what opens the history database in interactive use.
  (define (update-suggestion! gb)
    (set! suggestion-text
      (history-ghost-suffix (gap-buffer-before-string gb)
                            (gap-buffer-after-string gb)
                            editor-history)))

  ;; Accept suggestion: land the buffer exactly on the matched history entry,
  ;; replacing paredit's auto-inserted trailing closers rather than inserting
  ;; before them (which would double the closers, e.g. "(+ 1 2))").
  ;;
  ;; The ghost only ever shows when the after-cursor text is empty or entirely
  ;; closing delimiters -- history-ghost-suffix gates on only-closing-delimiters?
  ;; -- and suggestion-text is that entry's tail, which itself ends with those
  ;; same closers.  So delete forward to end-of-buffer first (that only removes
  ;; closers: the gate guarantees nothing else can sit after the cursor while a
  ;; ghost is showing), then lay down the tail.  The result is exactly
  ;; `before + suggestion-text` == the entry, balanced, with no doubled closer.
  ;; The cursor ends at end-of-buffer, just past the inserted text.
  (define (cmd-accept-suggestion es)
    (let ([gb (editor-state-gb es)])
      (when (> (string-length suggestion-text) 0)
        (let ([text suggestion-text])
          (set! suggestion-text "")
          ;; Drop the auto-inserted closers (the only thing that can follow the
          ;; cursor while a ghost shows) so the tail is not stacked on top of them.
          (let drop ()
            (when (< (gap-buffer-cursor-pos gb) (gap-buffer-length gb))
              (gap-buffer-delete-forward! gb)
              (drop)))
          (do ([i 0 (+ i 1)])
              ((= i (string-length text)))
            (gap-buffer-insert! gb (string-ref text i)))))))

  ;; ghost-acceptable?: may a showing ghost be laid down right now?  Only when a
  ;; non-empty suggestion-text is matched by a LIVE after-cursor string that is
  ;; still empty or all closing delimiters -- the exact gate history-ghost-suffix
  ;; applied when it computed the ghost.  Re-reading the buffer here, rather than
  ;; trusting the module variable alone, refuses a ghost left stale by an
  ;; intervening edit: update-suggestion! is suppressed while a completion menu is
  ;; up, so a Tab that rewrites the line leaves suggestion-text describing text
  ;; the cursor no longer sits on.  An empty after-cursor string stays acceptable,
  ;; so a literal end-of-buffer accept keeps working.
  (define (ghost-acceptable? gb)
    (and (> (string-length suggestion-text) 0)
         (only-closing-delimiters? (gap-buffer-after-string gb))))

  ;; Suggestion-aware movement: accept the ghost when one is genuinely acceptable
  ;; (ghost-acceptable?), else move normally.  The buffer re-check means an accept
  ;; only ever lands the entry at real end-of-buffer or before paredit's
  ;; auto-inserted closers, never on top of a stale suggestion.  With no
  ;; acceptable ghost, Right still moves the cursor forward one character.
  (define (cmd-forward-char-or-accept es)
    (if (ghost-acceptable? (editor-state-gb es))
        (cmd-accept-suggestion es)
        (cmd-forward-char es)))

  (define (cmd-end-of-line-or-accept es)
    (if (ghost-acceptable? (editor-state-gb es))
        (cmd-accept-suggestion es)
        (cmd-end-of-line es)))

  ;; Ctrl-Right: accept an acceptable ghost like Right/End, else fall through to
  ;; the ordinary Ctrl-Right word motion.  Keeping word movement as the no-ghost
  ;; path makes the explicit chord a superset of its usual motion and stays
  ;; symmetric with Ctrl-Left, so nothing regresses when no suggestion is on
  ;; screen.
  (define (cmd-forward-word-or-accept es)
    (if (ghost-acceptable? (editor-state-gb es))
        (cmd-accept-suggestion es)
        (cmd-forward-word es)))

  ;; Ctrl-Z: drop to the shell. VSUSP is disabled in raw mode, so Ctrl-Z reaches
  ;; the editor as byte 0x1A rather than a SIGTSTP the blocking read would defer;
  ;; the terminal owner publishes the suspend dance as current-suspend-hook for
  ;; the raw extent. Calling it restores cooked mode, stops the process, and
  ;; re-enters raw on resume -- the command loop then repaints the line. A no-op
  ;; when no hook is set (outside a raw session), which cannot happen on a real
  ;; keystroke. The editor stays out of the signal machinery: the owner owns it.
  (define (cmd-suspend es)
    (let ([suspend (current-suspend-hook)])
      (when suspend (suspend))))

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

    ;; Arrow keys and Home/End.  Right/End accept the history ghost whenever one
    ;; is showing -- including when the cursor sits before paredit's auto-inserted
    ;; closers -- else move normally.  Ctrl-Right is deliberately NOT bound in this
    ;; base layer: bind-paredit-keys! runs after it and claims Ctrl-Right for
    ;; forward-slurp in both keymaps, so a base bind would be immediately
    ;; overwritten (dead).  The explicit Ctrl-Right accept is installed in the
    ;; insert keymap (make-insert-keymap), where history ghosts arise while typing;
    ;; normal-mode Ctrl-Right stays paredit's forward-slurp.
    (keymap-bind! km (list (make-key-event 'special 'left 0)) cmd-backward-char)
    (keymap-bind! km (list (make-key-event 'special 'right 0)) cmd-forward-char-or-accept)
    (keymap-bind! km (list (make-key-event 'special 'home 0)) cmd-beginning-of-line)
    (keymap-bind! km (list (make-key-event 'special 'end 0)) cmd-end-of-line-or-accept)
    (keymap-bind! km (list (make-key-event 'special 'left MOD_ALT)) cmd-backward-word)
    (keymap-bind! km (list (make-key-event 'special 'right MOD_ALT)) cmd-forward-word)

    ;; Kill (Emacs)
    (keymap-bind! km (list (make-key-event 'ctrl #\k 0)) cmd-kill-line)
    ;; C-w kills the region when a mark is set, else backward-kill-word.
    ;; cmd-kill-region already owns that fallback, so bind it directly rather
    ;; than re-testing the region here.
    (keymap-bind! km (list (make-key-event 'ctrl #\w 0)) cmd-kill-region)
    (keymap-bind! km (list (make-key-event 'meta #\d 0)) cmd-kill-word)
    (keymap-bind! km (list (make-key-event 'special 'backspace MOD_ALT)) cmd-backward-kill-word)

    ;; Yank
    (keymap-bind! km (list (make-key-event 'ctrl #\y 0)) cmd-yank)
    (keymap-bind! km (list (make-key-event 'meta #\y 0)) cmd-yank-pop)

    ;; Emacs mark / region: C-Space sets the mark (the NUL key event -- < 32, so
    ;; cmd-self-insert never claims it), M-w copies mark..point to the kill-ring.
    (keymap-bind! km (list (make-key-event 'char (integer->char 0) 0)) cmd-set-mark)
    (keymap-bind! km (list (make-key-event 'meta #\w 0)) cmd-copy-region)

    ;; History navigation
    (keymap-bind! km (list (make-key-event 'special 'up 0)) cmd-history-prev)
    (keymap-bind! km (list (make-key-event 'special 'down 0)) cmd-history-next)
    (keymap-bind! km (list (make-key-event 'ctrl #\p 0)) cmd-history-prev)
    (keymap-bind! km (list (make-key-event 'ctrl #\n 0)) cmd-history-next)

    ;; Fuzzy finder commands
    (keymap-bind! km (list (make-key-event 'ctrl #\r 0)) cmd-fuzzy-history)
    (keymap-bind! km (list (make-key-event 'ctrl #\t 0)) cmd-file-picker)
    (keymap-bind! km (list (make-key-event 'meta #\c 0)) cmd-dir-picker)

    ;; Screen
    (keymap-bind! km (list (make-key-event 'ctrl #\l 0)) cmd-clear-screen)

    ;; Undo/Redo
    (keymap-bind! km (list (make-key-event 'ctrl #\_ 0)) cmd-undo)
    (keymap-bind! km (list (make-key-event 'meta #\/ 0)) cmd-redo)

    ;; Job control (Ctrl-Z suspend; see cmd-suspend)
    (keymap-bind! km (list (make-key-event 'ctrl #\z 0)) cmd-suspend))

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
  ;; Backward-compatible wrapper calling the base, paredit, and structural
  ;; s-expression layers.  Both make-insert-keymap and make-normal-keymap call
  ;; this, so the configurable s-expression keys are installed into BOTH the
  ;; emacs/insert and vi/normal keymaps at construction, default-on.
  (define (bind-common-keys! km)
    (bind-base-keys! km)
    (bind-paredit-keys! km)
    (bind-sexp-keys! km))

  ;; ======================================================================
  ;; Configurable structural s-expression keybindings
  ;; ======================================================================

  ;; The structural select / expand / contract / transpose / drag commands are
  ;; reachable out of the box through Lispy / lispyville (Doom-Emacs) default
  ;; keys, but every key is held in an exported parameter so a user's init can
  ;; rebind it.  Each default is the exact key-event the terminal decoder yields:
  ;;   C-M-SPC   = (meta #\nul 0)     -- ESC+NUL: progressive select, then expand
  ;;   M-SPC     = (meta #\space 0)   -- ESC+space: contract
  ;;   C-M-t     = (ctrl #\t MOD_ALT) -- ESC+C-t: transpose forward (mirrors C-M-f)
  ;;   M-j / M-k = (meta #\j 0) / (meta #\k 0) -- drag forward / backward
  ;; Every default is a 'meta or 'ctrl event, so vi never consumes it: the keys
  ;; fall through to the keymap in normal and visual modes and reach it directly
  ;; in insert mode.
  (define sexp-expand-key    (make-parameter (make-key-event 'meta (integer->char 0) 0)))
  (define sexp-contract-key  (make-parameter (make-key-event 'meta #\space 0)))
  (define sexp-transpose-key (make-parameter (make-key-event 'ctrl #\t MOD_ALT)))
  (define sexp-drag-fwd-key  (make-parameter (make-key-event 'meta #\j 0)))
  (define sexp-drag-back-key (make-parameter (make-key-event 'meta #\k 0)))

  ;; Bind the structural commands into KM, reading each key from its parameter at
  ;; call time (never a literal) so a re-invocation after an init override
  ;; installs the new key.  keymap-bind! overwrites any existing entry for a key,
  ;; so this is idempotent and re-invokable: an init can set a parameter (e.g.
  ;; (sexp-transpose-key (make-key-event 'ctrl #\y MOD_ALT))) then call
  ;; (bind-sexp-keys! editor-insert-keymap) and (bind-sexp-keys! editor-normal-keymap)
  ;; to rebind without touching source.  Drag-forward is transpose-forward, so it
  ;; shares cmd-transpose-sexp with the transpose key.  These binds are untouched
  ;; by disable-paredit! (which only unbinds the paredit set), so structural
  ;; selection stays on when paredit is toggled off.
  (define (bind-sexp-keys! km)
    (keymap-bind! km (list (sexp-expand-key))    cmd-expand-region)
    (keymap-bind! km (list (sexp-contract-key))  cmd-contract-region)
    (keymap-bind! km (list (sexp-transpose-key)) cmd-transpose-sexp)
    (keymap-bind! km (list (sexp-drag-fwd-key))  cmd-transpose-sexp)
    (keymap-bind! km (list (sexp-drag-back-key)) cmd-transpose-sexp-backward))

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

      ;; Ctrl+Left/Right: word movement in insert mode (overrides paredit slurp/barf).
      ;; Ctrl-Right additionally accepts a showing ghost suggestion before moving,
      ;; so the entry can be taken through paredit's auto-inserted closers.
      (keymap-bind! km (list (make-key-event 'special 'left MOD_CTRL)) cmd-backward-word)
      (keymap-bind! km (list (make-key-event 'special 'right MOD_CTRL)) cmd-forward-word-or-accept)

      km))

  ;; Build the normal-mode keymap.
  ;; Vi motions, operators, count prefixes, text objects, visual mode,
  ;; search, registers, marks are handled by vi.ss (called before keymap).
  ;; This keymap covers: insert-mode entry, simple editing, structural,
  ;; and keys that vi.ss falls through on.
  (define (make-normal-keymap)
    (let ([km (make-keymap)])
      (bind-common-keys! km)

      ;; ---- Sexp navigation (lispy-style single keys) ----
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

      ;; ---- Editing (not handled by vi.ss) ----
      (keymap-bind! km (list (make-key-event 'char #\x 0)) cmd-delete-char-no-eof)
      (keymap-bind! km (list (make-key-event 'char #\X 0)) cmd-delete-backward-char)
      (keymap-bind! km (list (make-key-event 'char #\D 0)) cmd-kill-line)
      (keymap-bind! km (list (make-key-event 'char #\C 0))
        (lambda (es) (cmd-kill-line es) (cmd-enter-insert-mode es)))
      (keymap-bind! km (list (make-key-event 'char #\s 0)) cmd-substitute-char)
      (keymap-bind! km (list (make-key-event 'char #\S 0)) cmd-substitute-line)
      (keymap-bind! km (list (make-key-event 'char #\p 0)) cmd-paste-after)
      (keymap-bind! km (list (make-key-event 'char #\P 0)) cmd-paste-before)
      (keymap-bind! km (list (make-key-event 'special 'backspace 0)) cmd-backward-char)
      (keymap-bind! km (list (make-key-event 'special 'delete 0)) cmd-delete-char-no-eof)
      (keymap-bind! km (list (make-key-event 'char #\Y 0))
        (lambda (es)
          (let* ([gb (editor-state-gb es)]
                 [text (gap-buffer->string gb)]
                 [pos (gap-buffer-cursor-pos gb)]
                 [rest (substring text pos (string-length text))])
            (kill-ring-push! editor-kill-ring rest))))

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

  ;; The command history handle is opened lazily on first interactive use.
  ;; This library is instantiated for EVERY invocation -- including a non-
  ;; interactive `-c`/`-s` that just evaluates an expression -- so opening the
  ;; history database at instantiation would dlopen libsqlite3 at startup for
  ;; a run that never reads history.  Instead %editor-history is a memo cell,
  ;; ensure-editor-history! opens the database once and caches it, and the
  ;; identifier macro `editor-history` routes every read below through that
  ;; accessor.  The handle therefore materialises on the first interactive
  ;; touch -- the first render's ghost lookup, an Up/Down press, or a submit --
  ;; and is reused thereafter; a non-interactive boot never opens it.
  (define %editor-history #f)
  (define (ensure-editor-history!)
    (or %editor-history
        (let ([h (open-history)])
          (set! %editor-history h)
          h)))
  (define-syntax editor-history
    (identifier-syntax (ensure-editor-history!)))

  ;; Return the history entries vector (for history expansion in interactive.ss).
  (define (editor-history-entries)
    (history-entries editor-history))

  ;; Tag the most recent history entry with its eval mode ('scheme or 'shell).
  (define (editor-history-set-last-mode! mode)
    (history-set-last-mode! editor-history mode))

  ;; Look up the mode for a history entry by index.
  (define (editor-history-entry-mode idx)
    (history-entry-mode editor-history idx))

  ;; Finder procedure injection: set by umbrella (hafod) after loading (hafod finder)
  ;; to break the circular dependency editor -> finder -> editor.
  ;; Value: (lambda (items prompt) ...) -> string or #f
  (define editor-finder-proc (make-parameter #f))

  ;; Toggle for fuzzy finder pickers (Ctrl-R history, Ctrl-T files, Alt-C dirs).
  ;; When #f, these keybindings do nothing.
  (define fuzzy-finder?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; Toggle for tab completion.
  ;; When #f, Tab inserts a literal tab character.
  (define tab-completions?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; ======================================================================
  ;; Command-head abbreviations (fish-style expand-as-typed)
  ;; ======================================================================

  ;; A command-head abbreviation expands the VISIBLE, editable buffer at the
  ;; word boundary: pressing Space (or Enter) after a lone command-head token
  ;; that names an abbreviation rewrites the token to its expansion in place, so
  ;; the user sees and can edit the result.  This is distinct from an alias,
  ;; which stays hidden until the line is run.  The table is a string -> string
  ;; map living beside the keymaps, populated from a user's init.
  (define abbr-table-ht (make-hashtable string-hash string=?))
  (define (abbr-set! name expansion) (hashtable-set! abbr-table-ht name expansion))
  (define (abbr-remove! name) (hashtable-delete! abbr-table-ht name))
  (define (abbr-ref name) (hashtable-ref abbr-table-ht name #f))

  ;; Toggle for command-head abbreviation expansion.  Default on (daily-driver
  ;; ergonomics); when off, Space inserts a literal space and nothing expands.
  (define abbr-expand?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; ======================================================================
  ;; History search state
  ;; ======================================================================

  ;; How Up/Down recall matches the typed needle against past lines.  The
  ;; default 'substring finds the needle anywhere in a line (fish-style recall);
  ;; 'prefix restores the strict head-anchored behaviour.  Validated: any other
  ;; value is rejected at the set site, so a stray value in a user's init fails
  ;; fast rather than silently disabling recall.  Both scan directions
  ;; (cmd-history-prev backward, cmd-history-next forward) branch on this through
  ;; the one shared predicate, so Up and Down can never disagree on a match.
  (define history-search-mode
    (make-parameter 'substring
      (lambda (v)
        (if (memq v '(substring prefix))
            v
            (error 'history-search-mode "expected 'substring or 'prefix" v)))))

  (define history-prefix #f)  ; #f or the typed needle for filtered Up/Down

  ;; ======================================================================
  ;; Tab completion state (module-level, one editor at a time)
  ;; ======================================================================

  (define completion-candidates '())   ; list of completion strings
  (define completion-positions '())    ; list of match-position lists (parallel to candidates)
  (define completion-descriptions '()) ; list of description strings or #f (parallel to candidates)
  (define completion-index -1)         ; selected candidate index (-1 = none)
  (define completion-start 0)          ; cursor position where prefix starts

  ;; Grid layout state (computed by render-completion-grid)
  (define completion-grid-cols 1)
  (define completion-grid-rows 0)
  (define completion-scroll-offset 0)

  (define (dismiss-completion!)
    (unless (null? completion-candidates)
      (set! completion-candidates '())
      (set! completion-positions '())
      (set! completion-descriptions '())
      (set! completion-index -1)
      (set! completion-start 0)
      (set! completion-grid-cols 1)
      (set! completion-grid-rows 0)
      (set! completion-scroll-offset 0)
      ;; A completion changed the buffer, so any history ghost computed before it
      ;; is now stale -- update-suggestion! is suppressed while the menu is up, so
      ;; suggestion-text still describes the pre-completion line.  Clear it here so
      ;; the accept path (Right/End) can never lay a stale suffix onto whatever the
      ;; cursor now sits on; the next render recomputes a fresh ghost.  Guarded
      ;; inside the unless: the plain-submit ghost-clear in finish! runs with no
      ;; active menu, where this branch is skipped anyway.
      (set! suggestion-text "")))

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

  ;; symbol-completions: fuzzy-match environment-symbols against prefix.
  ;; Returns list of (name . positions) pairs sorted by fuzzy score.
  (define (symbol-completions prefix)
    (let* ([syms (environment-symbols (interaction-environment))]
           [names (map symbol->string syms)])
      (fuzzy-filter/positions prefix names)))

  ;; filename-completions: list directory entries fuzzy-matching path basename.
  ;; Handles ~ expansion to HOME. Returns list of (fullpath . positions) pairs.
  (define (filename-completions prefix)
    (let* ([expanded (if (and (> (string-length prefix) 0)
                              (char=? (string-ref prefix 0) #\~))
                         (let ([home (or (getenv "HOME") "")])
                           (string-append home (substring prefix 1 (string-length prefix))))
                         prefix)]
           [len (string-length expanded)]
           ;; Split into directory and basename parts
           [last-slash (let loop ([i (- len 1)])
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
           [dir-prefix (if last-slash
                           (if (= last-slash 0) "/" (substring expanded 0 (+ last-slash 1)))
                           #f)]
           [dir-prefix-len (if dir-prefix (string-length dir-prefix) 0)])
      (guard (e [#t '()])  ; non-existent dir -> empty list
        (let* ([entries (directory-list dir)]
               [visible (filter
                          (lambda (entry)
                            ;; Skip . and .. unless base-prefix starts with .
                            (or (> (string-length base-prefix) 0)
                                (not (or (string=? entry ".") (string=? entry "..")))))
                          entries)]
               [sorted (fuzzy-filter/positions base-prefix visible)])
          (map (lambda (pair)
                 (let* ([entry (car pair)]
                        [positions (cdr pair)]
                        [full-path (string-append dir "/" entry)]
                        [is-dir? (guard (e [#t #f]) (file-directory? full-path))]
                        [display-entry (if is-dir?
                                           (string-append entry "/")
                                           entry)])
                   (if dir-prefix
                       (cons (string-append dir-prefix display-entry)
                             (map (lambda (p) (+ p dir-prefix-len)) positions))
                       (cons display-entry positions))))
               sorted)))))

  ;; shell-completions: fuzzy-match PATH executable names.
  ;; Returns list of (name . positions) pairs sorted by fuzzy score.
  (define (shell-completions prefix)
    (if (string=? prefix "")
        '()
        (let* ([ht (path-cache)]
               [keys (vector->list (hashtable-keys ht))])
          (fuzzy-filter/positions prefix keys))))

  ;; Detect whether the buffer looks like shell mode (no Scheme prefix characters).
  (define (shell-mode-buffer? text)
    (let ([len (string-length text)])
      (let loop ([i 0])
        (cond
          [(>= i len) #t]
          [(memv (string-ref text i) scheme-prefix-chars) #f]
          [else (loop (+ i 1))]))))

  ;; Extract the first whitespace-delimited token from buffer text.
  ;; Returns the first token as a string, or "" if empty.
  (define (first-token text)
    (let ([len (string-length text)])
      (let skip ([i 0])
        (cond
          [(>= i len) ""]
          [(char-whitespace? (string-ref text i)) (skip (+ i 1))]
          [else
           (let collect ([j i])
             (cond
               [(>= j len) (substring text i j)]
               [(char-whitespace? (string-ref text j)) (substring text i j)]
               [else (collect (+ j 1))]))]))))

  ;; Extract previous tokens (arguments) from buffer text, excluding the
  ;; current word being completed. Returns list of tokens between first token
  ;; and the word at word-start.
  (define (previous-args text word-start)
    (let ([len word-start])
      (let loop ([i 0] [args '()] [in-first? #t])
        (cond
          [(>= i len) (reverse args)]
          [(char-whitespace? (string-ref text i))
           (loop (+ i 1) args #f)]
          [else
           (let collect ([j i])
             (cond
               [(or (>= j len) (char-whitespace? (string-ref text j)))
                (if in-first?
                    (loop j args #f)  ; skip command name
                    (loop j (cons (substring text i j) args) #f))]
               [else (collect (+ j 1))]))]))))

  ;; Detect whether cursor is on the first token (command position).
  ;; Returns #t if there is no non-whitespace character before the start of the current word.
  (define (first-token-position? text word-start)
    (let loop ([i (- word-start 1)])
      (cond
        [(< i 0) #t]
        [(char-whitespace? (string-ref text i)) (loop (- i 1))]
        [else #f])))

  ;; Pure command-head abbreviation expansion (no gap buffer, no I/O) so the
  ;; whole decision is exhaustively unit-testable over a flat string.  The head
  ;; token is rewritten to its abbreviation only when EVERY gate holds:
  ;;   * the buffer is shell context -- the first non-whitespace character is not
  ;;     a Scheme-prefix char, so a Scheme expression is never touched;
  ;;   * the head sits in command position (nothing non-whitespace precedes it);
  ;;   * for the Space path the cursor sits at the END of that head token, so a
  ;;     mid-token or later-word Space never expands -- a #f cursor lifts this
  ;;     gate for the Enter/submit path, which expands the pending head even when
  ;;     arguments are already typed;
  ;;   * the head token names an abbreviation.
  ;; On success it returns (values new-text new-cursor) with the head span
  ;; replaced by its expansion and the cursor at the end of the inserted text; in
  ;; every other case it returns (values #f #f).  The command-head detectors
  ;; (shell-mode-buffer? / first-token / first-token-position?) are reused rather
  ;; than re-derived.
  (define (abbr-expand-head-token text cursor)
    (let* ([head (first-token text)]
           [hlen (string-length head)])
      (if (= hlen 0)
          (values #f #f)
          (let* ([ts (let skip ([i 0])
                       (if (and (< i (string-length text))
                                (char-whitespace? (string-ref text i)))
                           (skip (+ i 1))
                           i))]
                 [te (+ ts hlen)])
            (cond
              [(and (shell-mode-buffer? text)
                    (first-token-position? text ts)
                    (or (not cursor) (= cursor te))
                    (abbr-ref head))
               => (lambda (expansion)
                    (values (string-append (substring text 0 ts)
                                           expansion
                                           (substring text te (string-length text)))
                            (+ ts (string-length expansion))))]
              [else (values #f #f)])))))

  ;; Space path: expand only when the cursor is at the end of the head token.
  (define (expand-abbr-head text cursor)
    (abbr-expand-head-token text cursor))

  ;; Enter/submit path: expand the pending first-word head regardless of cursor
  ;; position (fish "expand on execute"), so the submitted line and its history
  ;; entry both carry the expansion.
  (define (expand-first-abbr text)
    (abbr-expand-head-token text #f))

  ;; Space in the INSERT keymap: expand a command-head abbreviation in the
  ;; visible buffer, then insert the space.  The space is inserted on EVERY path
  ;; (match or not) or Space would stop working.  On a match the expansion and
  ;; its trailing space are folded into ONE undo step -- editor-snapshot! opens
  ;; the step, editor-replace-text! rewrites the head, and the low-level
  ;; gap-buffer-insert! adds the space WITHOUT a further snapshot.  Routing the
  ;; space through cmd-self-insert would push a fresh whitespace snapshot and
  ;; split the edit into two undo steps (a single undo would then stop at the
  ;; expansion rather than restoring the original abbreviation), so the match
  ;; path deliberately avoids it.  With the toggle off, Space is a literal space.
  (define (cmd-expand-abbr-or-space es)
    (if (not (abbr-expand?))
        (cmd-self-insert es #\space)
        (let ([gb (editor-state-gb es)])
          (let-values ([(nt nc) (expand-abbr-head (gap-buffer->string gb)
                                                  (gap-buffer-cursor-pos gb))])
            (cond
              [nt
               (editor-snapshot! gb)
               (editor-replace-text! gb nt nc)
               ;; Add the separating space only when the expansion does not
               ;; already butt against following whitespace.  If the head was
               ;; expanded mid-line with arguments already present (`gco arg`,
               ;; cursor at the head-token end), the existing space is the
               ;; separator and inserting another would double it.  The undo
               ;; step opened above still folds the whole edit either way.
               (unless (and (< nc (string-length nt))
                            (char-whitespace? (string-ref nt nc)))
                 (gap-buffer-insert! gb #\space))]
              [else
               (cmd-self-insert es #\space)])))))

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

  ;; Helper: does the string contain the given character?  Used to tell a bare
  ;; ~name (a login to complete) from a ~name/rest (a path under a home).
  (define (string-has-char? str ch)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref str i) ch) #t]
          [else (loop (+ i 1))]))))

  ;; Helper: apply completions to editor state.
  ;; replace-start is cursor position where the prefix starts.
  ;; candidates is a list of (name . positions) pairs.
  ;; Optional descs is a parallel list of description strings or #f.
  (define apply-completions!
    (case-lambda
      [(gb text pos replace-start candidates)
       (apply-completions! gb text pos replace-start candidates #f)]
      [(gb text pos replace-start candidates descs)
       ;; Insertion is byte-exact.  Candidate names (and descriptions) are stored
       ;; and inserted with their ORIGINAL bytes, so completing a filename or path
       ;; that legally carries a control byte names the real file rather than a
       ;; lossily-stripped near-miss.  The control-strip that stops a crafted name
       ;; from repainting the screen lives at the SINGLE display choke-point --
       ;; render-completion-grid, where the menu is drawn -- so what is SHOWN is
       ;; sanitised while what is INSERTED (the single match, the longest common
       ;; prefix, and the completion-candidates that cycle-insert reads back) stays
       ;; exact.
       (let ([names (map car candidates)])
         (cond
           [(null? (cdr candidates))
            ;; Single match: insert directly
            (let* ([match (car names)]
                   [new-text (string-append
                               (substring text 0 replace-start)
                               match
                               (substring text pos (string-length text)))]
                   [new-pos (+ replace-start (string-length match))])
              (editor-replace-text! gb new-text new-pos))]
           [else
            ;; Multiple matches: insert longest common prefix, populate menu
            (let* ([lcp (longest-common-prefix names)]
                   [new-text (string-append
                               (substring text 0 replace-start)
                               lcp
                               (substring text pos (string-length text)))]
                   [new-pos (+ replace-start (string-length lcp))])
              (set! completion-candidates names)
              (set! completion-positions (map cdr candidates))
              (set! completion-descriptions (or descs (map (lambda (_) #f) candidates)))
              (set! completion-index -1)
              (set! completion-start replace-start)
              (editor-replace-text! gb new-text new-pos))]))]))

  ;; cmd-complete: Tab completion command.
  (define (cmd-complete es)
    (if (not (tab-completions?))
        (gap-buffer-insert! (editor-state-gb es) #\tab)
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
                  ;; A ~-token with no slash completes login names.  It is tried
                  ;; first, in both command and argument position, so it intercepts
                  ;; before filename-completions would prepend $HOME and mangle a
                  ;; bare ~name; a ~name/rest token (slash present) is a path under a
                  ;; home directory and is left to the filename fallback below.
                  (let ([tilde-results
                         (if (and (char=? (string-ref prefix 0) #\~)
                                  (not (string-has-char? prefix #\/)))
                             (guard (e [#t '()]) (user-completer prefix '()))
                             '())])
                    (cond
                      ;; ~user arm: login names -> home directories.
                      [(not (null? tilde-results))
                       (let ([pairs (map (lambda (e) (cons (car e) (cadr e))) tilde-results)]
                             [descs (map caddr tilde-results)])
                         (apply-completions! gb text pos start pairs descs))]
                      ;; First token: PATH executables merged with filenames
                      [(first-token-position? text start)
                       (let* ([shell-cands (shell-completions prefix)]
                              [file-cands (filename-completions prefix)]
                              [ht (make-hashtable string-hash string=?)]
                              [all (append shell-cands file-cands)]
                              [deduped (let lp ([rest all] [acc '()])
                                        (cond
                                          [(null? rest) (reverse acc)]
                                          [(hashtable-ref ht (caar rest) #f)
                                           (lp (cdr rest) acc)]
                                          [else
                                           (hashtable-set! ht (caar rest) #t)
                                           (lp (cdr rest) (cons (car rest) acc))]))])
                         (unless (null? deduped)
                           (apply-completions! gb text pos start deduped)))]
                      ;; Subsequent tokens: check for registered completer
                      [else
                       (let* ([cmd (first-token text)]
                              [completer (lookup-completer cmd)])
                         (if completer
                             ;; Registered completer returns (name positions desc) triples
                             (let* ([ctx (list (cons 'args (previous-args text start)))]
                                    [results (guard (e [#t '()])
                                               (completer prefix ctx))])
                               (if (null? results)
                                   ;; Fall back to filenames
                                   (let ([fc (filename-completions prefix)])
                                     (unless (null? fc)
                                       (apply-completions! gb text pos start fc)))
                                   ;; Extract pairs and descriptions from triples
                                   (let ([pairs (map (lambda (e) (cons (car e) (cadr e))) results)]
                                         [descs (map caddr results)])
                                     (apply-completions! gb text pos start pairs descs))))
                             ;; No registered completer: a dash-prefixed token offers
                             ;; the command's option flags, parsed from its --help/man
                             ;; output; anything else falls back to filenames.
                             (if (char=? (string-ref prefix 0) #\-)
                                 (let ([results (guard (e [#t '()])
                                                  (command-flag-completer cmd prefix))])
                                   (if (null? results)
                                       (let ([fc (filename-completions prefix)])
                                         (unless (null? fc)
                                           (apply-completions! gb text pos start fc)))
                                       (let ([pairs (map (lambda (e) (cons (car e) (cadr e))) results)]
                                             [descs (map caddr results)])
                                         (apply-completions! gb text pos start pairs descs))))
                                 (let ([fc (filename-completions prefix)])
                                   (unless (null? fc)
                                     (apply-completions! gb text pos start fc))))))]))))]
             ;; Normal Scheme context: symbol completion
             [else
              (let-values ([(prefix start) (word-at-cursor gb)])
                (when (> (string-length prefix) 0)
                  (let ([candidates (symbol-completions prefix)])
                    (unless (null? candidates)
                      (apply-completions! gb text pos start candidates)))))]))]))))


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

  ;; cmd-completion-navigate: arrow key navigation in the grid
  (define (cmd-completion-navigate es direction)
    (let* ([gb (editor-state-gb es)]
           [n (length completion-candidates)]
           [cols completion-grid-cols]
           [cur (if (< completion-index 0) 0 completion-index)]
           [new-idx
            (case direction
              [(right) (let ([next (+ cur 1)])
                         (if (>= next n) cur next))]
              [(left)  (let ([prev (- cur 1)])
                         (if (< prev 0) cur prev))]
              [(down)  (let ([next (+ cur cols)])
                         (if (>= next n) cur next))]
              [(up)    (let ([prev (- cur cols)])
                         (if (< prev 0) cur prev))]
              [else cur])]
           [candidate (list-ref completion-candidates new-idx)]
           [text (gap-buffer->string gb)]
           [pos (gap-buffer-cursor-pos gb)]
           [new-text (string-append
                       (substring text 0 completion-start)
                       candidate
                       (substring text pos (string-length text)))]
           [new-pos (+ completion-start (string-length candidate))])
      (set! completion-index new-idx)
      (editor-replace-text! gb new-text new-pos)))

  ;; Track the completion menu currently on screen: how many rows it drew, and
  ;; which way it was anchored ('down dropdown / 'up drop-up).  completion-menu-lines
  ;; was written-only before; it now drives the pre-repaint clear.  The place is
  ;; needed because a drop-up's rows sit ABOVE the prompt, out of reach of the
  ;; downward clear render-line does, so the menu clear must wipe them explicitly.
  (define completion-menu-lines 0)
  (define completion-menu-place 'down)

  ;; Predict render-completion-grid's column/row layout for the current candidate
  ;; list at term-cols WITHOUT drawing it, mirroring the grid's own geometry so the
  ;; drawn height is known before the draw -- needed to anchor the menu and to bound
  ;; its rows.  A stored candidate name is byte-exact and so may carry a stray
  ;; control byte (the grid strips those only at draw time); such a name is
  ;; measured a shade wider here than it renders, which can over-estimate the menu
  ;; height by a row -- cosmetic and bounds-safe, and never reached by the ordinary
  ;; escape-free name, for which this width matches the grid's exactly.  Descriptions
  ;; force a single column exactly as the grid does.
  (define (completion-grid-dimensions term-cols)
    (let* ([n (length completion-candidates)]
           [has-descs? (not (null? completion-descriptions))]
           [max-name-w (fold-left
                         (lambda (mx s) (max mx (ansi-display-width s)))
                         0 completion-candidates)]
           [cell-width (if has-descs?
                           term-cols
                           (+ 2 (min max-name-w (quotient term-cols 2)) 2))]
           [grid-cols (if has-descs?
                          1
                          (max 1 (quotient term-cols cell-width)))]
           [grid-rows (let ([q (quotient n grid-cols)]
                            [r (remainder n grid-cols)])
                        (if (> r 0) (+ q 1) q))])
      (values grid-cols grid-rows)))

  ;; The height render-completion-grid draws for grid-rows candidate rows under a
  ;; visible cap: the capped rows plus a pager row when the list overflows the cap
  ;; -- exactly the menu-lines the grid returns for the same inputs.
  (define (completion-menu-drawn-rows grid-rows max-visible)
    (+ (min grid-rows max-visible)
       (if (> grid-rows max-visible) 1 0)))

  ;; Choose the menu anchor purely from the room below the edit line and the menu's
  ;; height: a dropdown by default, a drop-up when the room below cannot hold the
  ;; menu.  Pure over its two inputs, so the choice is unit-testable off a terminal.
  (define (menu-anchor-place rows-below menu-height)
    (if (< rows-below menu-height) 'up 'down))

  ;; The display column of the edit cursor on its own visual row, matching
  ;; render-line's own final positioning (prompt width on the first row, plus the
  ;; width since the last newline, one-based) so the cursor sits at the edit point
  ;; while the menu is shown.  before is escape-free, so ansi-display-width is its
  ;; display width.
  (define (edit-cursor-column prompt-width before cursor-row)
    (let* ([len (string-length before)]
           [nl-start (let loop ([i (- len 1)])
                       (cond [(< i 0) 0]
                             [(char=? (string-ref before i) #\newline) (+ i 1)]
                             [else (loop (- i 1))]))])
      (+ (if (= cursor-row 0) prompt-width 0)
         (ansi-display-width (substring before nl-start len))
         1)))

  ;; Render edit line + optional completion menu + ghost suggestion.
  ;; Returns new prev-lines value for the next render call.  es is threaded in so
  ;; the redisplay can compute the selection span and the mode indicator.
  (define (render-with-menu port prompt gb prev-lines es)
    ;; Update suggestion on every render (cheap: just a prefix search)
    (when (null? completion-candidates)
      (update-suggestion! gb))
    (let* ([term-cols (editor-query-terminal-cols)]
           [ansi? (ansi-ok? port)]
           ;; Pre-clear a stale drop-up left by the previous render.  render-line
           ;; (below) moves up to the prompt line and clears to end of screen, which
           ;; wipes the edit line and every row BELOW it -- so a previous dropdown is
           ;; cleared for free, and must NOT be over-cleared here (climbing over a
           ;; below-line span would eat the edit line).  A previous drop-up, though,
           ;; drew its rows ABOVE the prompt, out of that downward clear's reach; wipe
           ;; them now, driven by the tracked span.  The cursor sits prev-lines rows
           ;; below the prompt: climb to the prompt, clear the tracked span above it,
           ;; and drop back to the prompt row, leaving render-line nothing above to do.
           [start-lines
            (if (and ansi? (eq? completion-menu-place 'up) (> completion-menu-lines 0))
                (begin
                  (when (> prev-lines 0)
                    (display "\x1b;[" port) (display prev-lines port) (display "A" port))
                  (overlay-clear! port completion-menu-lines ansi?)
                  (display "\x1b;[" port) (display completion-menu-lines port) (display "B" port)
                  0)
                prev-lines)]
           ;; The selection span and the mode indicator, computed once from es.
           ;; When either is active -- a vi visual / emacs selection to highlight,
           ;; or a non-insert mode to announce -- the redisplay routes through the
           ;; selection-aware renderer; the plain and ghost-suggestion paths stay
           ;; untouched for insert-mode typing.
           [sel-range (editor-selection-range es)]
           [indicator (editor-mode-indicator es)]
           ;; render-line clears from the prompt line down (\x1b[J) and redraws the
           ;; edit line, wiping any dropdown below it.
           [lines (cond
                    [(or sel-range indicator)
                     (render-line/selection port prompt gb
                       start-lines sel-range indicator term-cols)]
                    [(and (null? completion-candidates)
                          (> (string-length suggestion-text) 0))
                     (render-line/suggestion port prompt gb
                       start-lines suggestion-text term-cols)]
                    [else (render-line port prompt gb start-lines term-cols)])])
      (if (not (null? completion-candidates))
          ;; Draw the menu in place as a dropdown or drop-up through the overlay
          ;; helper -- relative motion only, no cursor save/restore.
          (let* ([text (string-append (gap-buffer-before-string gb)
                                      (gap-buffer-after-string gb))]
                 [before (gap-buffer-before-string gb)]
                 [prompt-width (ansi-display-width prompt)]
                 [total-lines (count-visual-lines prompt-width text term-cols)]
                 [cursor-row (cursor-visual-row prompt-width before term-cols)]
                 [lines-after (- total-lines cursor-row)]
                 [term-rows (editor-query-terminal-rows)]
                 ;; Rows on screen below the edit block, and the room a menu may take
                 ;; above or below it.  The editor does not track the block's absolute
                 ;; row, so treat it as top-anchored: an upper bound that keeps the
                 ;; menu inside the terminal.
                 [rows-below (max 0 (- (- term-rows 1) total-lines))]
                 [room (max 1 (- (- term-rows 1) total-lines))]
                 [entries (if (null? completion-descriptions)
                              (map cons completion-candidates completion-positions)
                              (map list completion-candidates completion-positions completion-descriptions))])
            (let-values ([(grid-cols grid-rows) (completion-grid-dimensions term-cols)])
              (let* ([cap 10]
                     [max-visible (min cap room)]
                     [place (menu-anchor-place
                              rows-below
                              (completion-menu-drawn-rows grid-rows cap))]
                     ;; The height the grid will actually draw under max-visible; the
                     ;; drop-up offset lifts the menu by exactly that so its foot sits
                     ;; just above the prompt.
                     [menu-height (completion-menu-drawn-rows grid-rows max-visible)]
                     [offset (if (eq? place 'up)
                                 (+ cursor-row menu-height 1)
                                 lines-after)]
                     [cursor-col (edit-cursor-column prompt-width before cursor-row)]
                     [menu-lines
                      (overlay-draw port place offset ansi?
                        (lambda ()
                          (let-values ([(ml gcols grows soff)
                                        (render-completion-grid port entries
                                          completion-index term-cols max-visible)])
                            (set! completion-grid-cols gcols)
                            (set! completion-grid-rows grows)
                            (set! completion-scroll-offset soff)
                            ml)))])
                ;; overlay-draw returns the cursor to the edit ROW; put it back on the
                ;; edit COLUMN too.  An absolute column move is scroll-safe, exactly as
                ;; render-line's own final column positioning is.
                (when ansi?
                  (display "\x1b;[" port) (display cursor-col port) (display "G" port))
                (flush-output-port port)
                (set! completion-menu-lines menu-lines)
                (set! completion-menu-place place)
                lines)))
          (begin
            (set! completion-menu-lines 0)
            (set! completion-menu-place 'down)
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
  ;; Dot-repeat change boundary (run at the top of each main-loop pass)
  ;; ======================================================================

  ;; The change-recording boundary.  It has two jobs, both gated on an idle
  ;; normal state (normal mode AND not vi-mid-command?): first COMMIT a completed
  ;; change -- when the buffer text actually differs from the change-start
  ;; snapshot and the initiating key is not excluded, the recorded bytes become
  ;; the last change -- then BEGIN a fresh recording.  Recording spans the whole
  ;; change uniformly because the decoder tee (current-key-recording) captures
  ;; every consumed byte, so an operator + motion, an insert session across the
  ;; normal->insert->normal flip, a multi-key surround whose operands are
  ;; inline-read, and sexp-transpose are all captured with no other call-site
  ;; changes.  Recording begins only from idle normal mode, so the insert-default
  ;; line composition before the first Escape is never a recorded change, and
  ;; never while a completion menu is open.
  (define (dot-repeat-boundary! es gb)
    (let ([mode (editor-state-mode es)])
      ;; Commit a completed change (never one that invoked a replay).
      (when (and *dot-recording?* (eq? mode 'normal) (not (vi-mid-command?)))
        (let ([bytes (get-output-string (current-key-recording))])
          (when (and (not *dot-change-replayed?*)
                     (not *dot-change-undo/redo/paste?*)
                     (not (string=? (gap-buffer->string gb) *dot-change-start-text*))
                     (not (dot-excluded-event? *dot-initiating-evt*)))
            (set! *dot-last-change* bytes)))
        (current-key-recording #f)
        (set! *dot-recording?* #f))
      ;; Begin a new change from idle normal mode.
      (when (and (not *dot-recording?*) (eq? mode 'normal) (not (vi-mid-command?))
                 (null? completion-candidates))
        (current-key-recording (open-output-string))
        (set! *dot-change-start-text* (gap-buffer->string gb))
        (set! *dot-initiating-evt* 'unset)
        (set! *dot-change-replayed?* #f)
        (set! *dot-change-undo/redo/paste?* #f)
        (set! *dot-recording?* #t))))

  ;; Record the initiating event of a fresh recording (the first key read after a
  ;; recording begins), consulted by dot-excluded-event? at commit time.  Called
  ;; right after each key is read, by both the main loop and the white-box drive
  ;; harness, so the two share one capture rule.
  (define (dot-capture-initiating! evt)
    (when (and *dot-recording?* (eq? *dot-initiating-evt* 'unset)
               (not (eof-object? evt)))
      (set! *dot-initiating-evt* evt)))

  ;; ======================================================================
  ;; Dot-repeat replay driver (wired below as vi-replay!-proc)
  ;; ======================================================================

  ;; Dispatch ONE key-event through the real command handlers, exactly as the
  ;; main loop's normal-dispatch branch does (minus completion / bracketed paste /
  ;; rendering): in normal mode the vi state machine gets first crack and, when it
  ;; does not claim the key, the normal keymap is consulted (a prefix keymap reads
  ;; its follow-up from `in`); in insert mode the insert keymap runs, falling back
  ;; to cmd-self-insert for a printable char.  `in` is the port inline reads pull
  ;; from -- a replayed surround / cs / ds / text object satisfies its inline
  ;; read-key-event calls from it, and a replayed insert re-runs cmd-self-insert so
  ;; paredit auto-pairing fires exactly once -- never a double-inserted closer.
  ;; Shared by the replay driver and the white-box drive harness so both exercise
  ;; the same dispatch the main loop uses.
  (define (dot-dispatch-event! es gb in out evt)
    (if (eq? (editor-state-mode es) 'normal)
        (unless (vi-process-key es evt in out gb editor-kill-ring)
          ;; A count typed before this keymap-dispatched command (3x, 5p, 4a)
          ;; was surfaced by vi-process-key; read it and repeat the bound
          ;; command that many times (default 1), stopping early on done. Keeps
          ;; this dispatch path consistent with the main loop's.
          (let ([binding (keymap-lookup editor-normal-keymap evt)]
                [reps (vi-take-keymap-count!)])
            (cond
              [(procedure? binding)
               (let rep ([i 0])
                 (when (and (< i reps) (not (editor-state-done? es)))
                   (binding es)
                   (rep (+ i 1))))]
              [(keymap? binding)
               (let ([next-evt (read-key-event in)])
                 (unless (eof-object? next-evt)
                   (let ([sub (keymap-lookup binding next-evt)])
                     (when (procedure? sub) (sub es)))))]
              [else (void)])))
        (let ([binding (keymap-lookup editor-insert-keymap evt)])
          (cond
            [(procedure? binding) (binding es)]
            [(and (eq? (key-event-type evt) 'char)
                  (char? (key-event-value evt))
                  (>= (char->integer (key-event-value evt)) 32))
             (cmd-self-insert es (key-event-value evt))]
            [else (void)]))))

  ;; Re-drive a recorded keystroke byte-string through the real dispatch, reading
  ;; key-events from a synthetic string port until EOF.
  (define (dot-drive-bytes! es gb out bytes)
    (let ([in (open-input-string bytes)])
      (let loop ()
        (let ([evt (read-key-event in)])
          (unless (eof-object? evt)
            (dot-dispatch-event! es gb in out evt)
            (loop))))))

  ;; The dot-repeat replay driver, wired below as vi-replay!-proc.  Re-feeds the
  ;; last change (max 1 count) times, with the recording tee parameterized OFF so
  ;; a replay is never itself recorded -- this, together with . being in the
  ;; exclusion set, is what bounds the recursion.  Honouring the count by LOOPING
  ;; (rather than substituting the change's leading digit) repeats an insert
  ;; change AND an operator change uniformly, because hafod's insert-entry
  ;; commands ignore a vi count.  A bare . loops once, re-feeding the change
  ;; verbatim (so an embedded count is reproduced).  A #f / empty last change is a
  ;; safe no-op.  The vi transient state is cleared before each iteration: the .
  ;; arm reads the dot's count and calls this hook BEFORE its own vi-reset-state!,
  ;; so without the reset a bare-. replay of dw would inherit the dot's count and
  ;; delete N words; clearing it lets the recorded bytes (which carry their own
  ;; count) drive the change alone.
  (define (dot-replay! es count)
    (set! *dot-change-replayed?* #t)
    (let ([bytes *dot-last-change*])
      (when (and (string? bytes) (> (string-length bytes) 0))
        (let ([gb (editor-state-gb es)]
              [out (editor-state-out-port es)]
              [n (max 1 count)])
          (parameterize ([current-key-recording #f])
            (let lp ([i 0])
              (when (< i n)
                (vi-reset-state!)
                (dot-drive-bytes! es gb out bytes)
                (lp (+ i 1)))))))))

  ;; PTY-free drive harness (white-box, for the dot-repeat suite).  It runs the
  ;; SAME recording boundary, initiating-event capture and dispatch the main loop
  ;; runs, so a test exercises the real record / commit / replay path without the
  ;; terminal ESC-timing the full decoder loop needs.  INPUTS is a list whose
  ;; elements are either:
  ;;   - a STRING, fed as a bounded port and decoded through read-key-event so the
  ;;     recording tee captures its bytes; a standalone Escape (\x1b) sits at
  ;;     end-of-port and so decodes to 'escape (mid-stream it would decode to Meta,
  ;;     matching a real terminal only when isolated), and a change's inline reads
  ;;     (surround / text object) pull their trailing bytes from the same port; or
  ;;   - a pre-built KEY-EVENT, dispatched directly, for a key the byte decoder
  ;;     cannot synthesise (e.g. C-_, which arrives as a raw control byte the
  ;;     decoder maps to a plain char, never a 'ctrl event).
  ;; The boundary runs before every key and once more after the last, so a change
  ;; completed by the final input is committed exactly as the main loop commits it.
  (define (editor-drive-keys! es inputs)
    (let ([gb (editor-state-gb es)]
          [out (editor-state-out-port es)])
      (for-each
        (lambda (input)
          (if (string? input)
              (let ([in (open-input-string input)])
                (let loop ()
                  (dot-repeat-boundary! es gb)
                  (let ([evt (read-key-event in)])
                    (unless (eof-object? evt)
                      (dot-capture-initiating! evt)
                      (dot-dispatch-event! es gb in out evt)
                      (loop)))))
              (begin
                (dot-repeat-boundary! es gb)
                (dot-capture-initiating! input)
                (dot-dispatch-event! es gb (open-input-string "") out input))))
        inputs)
      (dot-repeat-boundary! es gb)))

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
              [es (make-editor-state gb editor-kill-ring prompt out-port 0 #f #f 'insert #f)]
              [finish! (lambda (result)
                         (set! history-prefix #f)
                         (dismiss-completion!)
                         (reset-undo-state!)
                         ;; Erase whatever the last frame drew over and around the
                         ;; line, and echo the committed line whole.
                         ;;
                         ;; Three things can be on the screen that the transcript
                         ;; must not keep: a history ghost, a mode-indicator row,
                         ;; and a reverse-video selection.  ONE re-render clears
                         ;; all three: render-line climbs to the prompt row, clears
                         ;; the whole edit block from there (so every ghost row
                         ;; goes, however many the suggestion wrapped onto),
                         ;; redraws the prompt and the full buffer with no ghost,
                         ;; no indicator and no highlight, then leaves the cursor
                         ;; on the typing row -- which is exactly what a truthful
                         ;; echo is.  The indicator has to go because it sits on
                         ;; its own row BELOW the edit line and would otherwise
                         ;; collide with the result printed next (a submitted
                         ;; (+ 9 5 4) printing as "18 NORMAL --").
                         ;;
                         ;; The ghost cannot be erased by clearing from the cursor
                         ;; to the end of the screen, which is what this did
                         ;; before.  That relied on the ghost only ever showing at
                         ;; end-of-buffer, and it does not: the ghost shows
                         ;; whenever the text after the cursor is all closing
                         ;; delimiters (only-closing-delimiters?), which is
                         ;; precisely where paredit parks its auto-inserted
                         ;; closers -- and it is drawn AT the cursor, in front of
                         ;; them.  A clear from the cursor therefore reached past
                         ;; the ghost and took those real closers with it: a
                         ;; submitted (+ 1 2) echoed as "> (+ 1 2", the transcript
                         ;; disagreeing with what was evaluated.
                         ;;
                         ;; Gated on a terminal AND something actually drawn: a
                         ;; non-tty sink emitted nothing to clear and must not be
                         ;; sent a duplicated buffer, and a plain line with no
                         ;; ghost or overlay needs no repaint.  The ghost is read
                         ;; before it is dropped, and the selection before the mark
                         ;; is cleared below, so both are still seen here.
                         (let ([ghost? (> (string-length suggestion-text) 0)])
                           (set! suggestion-text "")
                           (when (and (ansi-ok? out-port)
                                      (or ghost?
                                          (editor-mode-indicator es)
                                          (editor-selection-range es)))
                             (let ([term-cols (editor-query-terminal-cols)])
                               (render-line out-port prompt gb
                                            (cursor-visual-row (ansi-display-width prompt)
                                                               (gap-buffer-before-string gb)
                                                               term-cols)
                                            term-cols))))
                         ;; Clear the emacs mark so a region set on this line never
                         ;; bleeds a highlight into the next prompt (vi visual is
                         ;; already cleared by the session reset).  Drop the
                         ;; expansion stack alongside it, so a structural selection
                         ;; cannot grow a span on the next line.
                         (editor-state-mark-set! es #f)
                         (reset-sexp-stack!)
                         ;; Move cursor past all content lines before newline, so the
                         ;; result prints below the full expression -- counting the
                         ;; wrapped rows via the shared geometry helpers, not just
                         ;; newlines, so a buffer that wraps is stepped past in full.
                         (let* ([text (gap-buffer->string gb)]
                                [term-cols (editor-query-terminal-cols)]
                                [prompt-width (ansi-display-width prompt)]
                                [total-lines (count-visual-lines prompt-width text term-cols)]
                                [cursor-row (cursor-visual-row prompt-width
                                                               (gap-buffer-before-string gb)
                                                               term-cols)]
                                [lines-below (- total-lines cursor-row)])
                           (when (and (ansi-ok? out-port) (> lines-below 0))
                             (display "\x1b;[" out-port)
                             (display lines-below out-port)
                             (display "B" out-port)))
                         (display "\n" out-port)
                         (reset-cursor out-port)
                         (flush-output-port out-port)
                         result)])
         ;; Clear any vi session state left over from the previous line: a visual
         ;; mode never dismissed before submit must not bleed a highlight into
         ;; this fresh prompt, which starts in insert mode.
         (vi-reset-session!)
         ;; Discard any half-recorded change from the previous line so this fresh
         ;; prompt starts clean; the committed last change is left intact so a
         ;; change made earlier stays repeatable with . on this line.
         (reset-dot-recording!)
         ;; Display multi-line prefix once (e.g. starship context lines)
         (when (> (string-length prompt-prefix) 0)
           (display prompt-prefix out-port)
           (flush-output-port out-port))
         ;; Set initial cursor shape + colour (bar/blue for insert mode)
         (set-cursor-bar out-port)
         (flush-output-port out-port)
         ;; Seed the terminal-size cache once for this prompt, so the first
         ;; render below -- and every keystroke render after it -- reads the
         ;; true width from the cache instead of a live ioctl per render.
         (refresh-terminal-size-cache!)
         ;; Render initial edit line (last line of prompt + empty buffer)
         (let ([initial-lines (render-line out-port prompt gb 0 (editor-query-terminal-cols))])
         ;; Command loop — prev-lines tracks screen lines for correct cursor-up
         (let loop ([prev-lines initial-lines])
           ;; Dot-repeat boundary: commit a completed change and begin the next
           ;; one before this key is read, so the tee is armed for the keystroke
           ;; about to be decoded.
           (dot-repeat-boundary! es gb)
           (let* ([mode (editor-state-mode es)]
                  [km (if (eq? mode 'normal) editor-normal-keymap editor-insert-keymap)]
                  [evt (read-key-event in-port)])
             ;; Capture the initiating event of a fresh recording (the tee has
             ;; already recorded evt's bytes inside read-key-event, so any leading
             ;; count digits are in the byte stream regardless).
             (dot-capture-initiating! evt)
             (cond
               [(eof-object? evt)
                (finish! (if (= (gap-buffer-length gb) 0)
                             (eof-object)
                             (gap-buffer->string gb)))]
               [else
                (let* ([ep prompt]
                       [compl-active? (not (null? completion-candidates))])
                  (cond
                    ;; ------- Completion menu intercepts (highest priority) -------
                    [(and compl-active?
                          (eq? (key-event-type evt) 'special)
                          (eq? (key-event-value evt) 'backtab))
                     (cmd-complete-prev es)
                     (loop (render-with-menu out-port ep gb prev-lines es))]
                    ;; Arrow keys during completion: grid navigation
                    [(and compl-active?
                          (eq? (key-event-type evt) 'special)
                          (memq (key-event-value evt) '(up down left right)))
                     (cmd-completion-navigate es (key-event-value evt))
                     (loop (render-with-menu out-port ep gb prev-lines es))]
                    [(and compl-active?
                          (eq? (key-event-type evt) 'special)
                          (eq? (key-event-value evt) 'return))
                     (dismiss-completion!)
                     (loop (render-with-menu out-port ep gb prev-lines es))]
                    [(and compl-active?
                          (or (and (eq? (key-event-type evt) 'special)
                                   (eq? (key-event-value evt) 'escape))
                              (and (eq? (key-event-type evt) 'ctrl)
                                   (eqv? (key-event-value evt) #\g))))
                     (dismiss-completion!)
                     (loop (render-with-menu out-port ep gb prev-lines es))]

                    ;; ------- Bracketed paste -------
                    [(and (eq? (key-event-type evt) 'special)
                          (eq? (key-event-value evt) 'paste-start))
                     (when compl-active? (dismiss-completion!))
                     (handle-bracketed-paste es in-port)
                     (if (editor-state-done? es)
                         (finish! (editor-state-result es))
                         (loop (render-with-menu out-port ep gb prev-lines es)))]

                    ;; ------- Normal dispatch -------
                    [else
                     ;; Dismiss completion on non-Tab keys
                     (when (and compl-active?
                                (not (and (eq? (key-event-type evt) 'special)
                                          (eq? (key-event-value evt) 'tab))))
                       (dismiss-completion!))
                     ;; Vi state machine gets first crack in normal mode
                     (if (and (eq? mode 'normal)
                              (vi-process-key es evt in-port out-port
                                              gb editor-kill-ring))
                         ;; Vi handled the key
                         (if (editor-state-done? es)
                             (finish! (editor-state-result es))
                             (loop (render-with-menu out-port prompt gb prev-lines es)))
                         ;; Fall through to keymap. A numeric count typed before
                         ;; a keymap-dispatched command (3x, 5p, 4a) was surfaced
                         ;; by vi-process-key; in normal mode read it and repeat
                         ;; the bound command that many times (default 1). Taken
                         ;; only in normal mode, so insert-mode typing is
                         ;; unaffected; the loop also stops early if a command
                         ;; signals done.
                         (let ([binding (keymap-lookup km evt)]
                               [reps (if (eq? mode 'normal) (vi-take-keymap-count!) 1)])
                           (cond
                             [(procedure? binding)
                              (let rep ([i 0])
                                (when (and (< i reps) (not (editor-state-done? es)))
                                  (binding es)
                                  (rep (+ i 1))))
                              (if (editor-state-done? es)
                                  (finish! (editor-state-result es))
                                  (loop (render-with-menu out-port prompt gb prev-lines es)))]
                             [(keymap? binding)
                              (let ([next-evt (read-key-event in-port)])
                                (if (eof-object? next-evt)
                                    (loop (render-with-menu out-port prompt gb prev-lines es))
                                    (let ([sub-binding (keymap-lookup binding next-evt)])
                                      (when (procedure? sub-binding)
                                        (sub-binding es))
                                      (if (editor-state-done? es)
                                          (finish! (editor-state-result es))
                                          (loop (render-with-menu out-port prompt gb prev-lines es))))))]
                             [(and (eq? mode 'insert)
                                   (eq? (key-event-type evt) 'char)
                                   (let ([ch (key-event-value evt)])
                                     (and (char? ch)
                                          (>= (char->integer ch) 32))))
                              (cmd-self-insert es (key-event-value evt))
                              (if (editor-state-done? es)
                                  (finish! (editor-state-result es))
                                  (loop (render-with-menu out-port prompt gb prev-lines es)))]
                             [else
                              (loop prev-lines)])))])
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

  ;; Bind Space to command-head abbreviation expansion in the INSERT keymap ONLY
  ;; (never the shared or normal keymap): a command-head abbreviation expands the
  ;; visible buffer, then the space is inserted; when no abbreviation matches, or
  ;; the toggle is off, Space inserts a literal space.  Placed here beside Tab so
  ;; the command definition precedes this expression.
  (keymap-bind! editor-insert-keymap
    (list (make-key-event 'char #\space 0)) cmd-expand-abbr-or-space)

  ;; Wire up vi.ss procedure hooks (must be after all cmd-* definitions)
  (vi-snapshot!-proc (lambda (es) (editor-snapshot! (editor-state-gb es))))
  (vi-undo!-proc (lambda (es) (cmd-undo es)))
  (vi-redo!-proc (lambda (es) (cmd-redo es)))
  (vi-enter-insert!-proc (lambda (es) (cmd-enter-insert-mode es)))
  (vi-enter-normal!-proc (lambda (es) (cmd-enter-normal-mode es)))
  (vi-history-prev!-proc (lambda (es) (cmd-history-prev es)))
  (vi-history-next!-proc (lambda (es) (cmd-history-next es)))
  (vi-submit!-proc (lambda (es) (cmd-submit es)))
  ;; Dot-repeat: the . arm delegates to this driver, which re-feeds the recorded
  ;; keystrokes of the last change through a synthetic port, count times.
  (vi-replay!-proc dot-replay!)

) ; end library
