;;; (hafod interactive) -- Interactive REPL loop for hafod
;;; Provides configurable read-eval-print loop with prompt and eval hooks.
;;; Handles SIGINT (Ctrl-C) to interrupt evaluation and SIGWINCH for terminal width.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod interactive)
  (export
    interactive-repl
    eval-script
    repl-prompt-hook
    repl-prompt-string
    repl-right-prompt-hook
    repl-pre-eval-hook
    repl-post-eval-hook
    last-status
    last-duration
    terminal-width
    query-terminal-width
    query-terminal-size
    repl-continuation-prompt
    ansi-visible-length
    background-job-count
    ;; Shell mode re-exports (for config access)
    rebuild-path-cache!
    classify-input
    ;; Feature toggles
    shell-mode?
    history-expansion?)

  (import (except (chezscheme) getenv)
          (only (hafod posix) status:exit-val SIGWINCH)
          (only (hafod internal posix-constants) TIOCGWINSZ)
          (only (hafod environment) getenv setenv)
          (only (hafod procobj) background-job-count)
          (only (hafod editor editor) read-expression with-raw-mode
                editor-history-entries editor-history-set-last-mode!)
          (only (hafod tty) tty?)
          (only (hafod editor render) tokenize display-colourised)
          (only (hafod shell classifier) classify-input rebuild-path-cache! path-cache)
          (only (hafod shell parser) parse-shell-command)
          (only (hafod shell builtins) run-builtin! builtin-names dir-stack)
          (only (hafod shell jobs) install-job-signals! update-jobs! drain-notifications!)
          (only (hafod shell history-expand) history-expand)
          (only (hafod fuzzy) fuzzy-filter))

  ;; === Hook parameters ===

  ;; Simple prompt customisation: set this string to change the default prompt.
  ;; For dynamic prompts, replace repl-prompt-hook instead.
  (define repl-prompt-string
    (make-parameter "> "
      (lambda (v)
        (unless (string? v)
          (error 'repl-prompt-string "expected a string" v))
        v)))

  ;; Prompt hook: called before each read. Takes no arguments.
  ;; Default displays (repl-prompt-string) and flushes output.
  (define repl-prompt-hook
    (make-parameter
      (lambda ()
        (display (repl-prompt-string) (console-output-port))
        (flush-output-port (console-output-port)))
      (lambda (v)
        (unless (procedure? v)
          (error 'repl-prompt-hook "expected a procedure" v))
        v)))

  ;; Right prompt hook: procedure that writes to current-output-port (captured by REPL).
  ;; Default: no-op (no right prompt).
  (define repl-right-prompt-hook
    (make-parameter
      (lambda () (void))
      (lambda (v)
        (unless (procedure? v)
          (error 'repl-right-prompt-hook "expected a procedure" v))
        v)))

  ;; Continuation prompt: static string displayed on continuation lines.
  ;; Default: ".. " (similar to Chez's built-in indentation).
  (define repl-continuation-prompt
    (make-parameter
      ".. "
      (lambda (v)
        (unless (string? v)
          (error 'repl-continuation-prompt "expected a string" v))
        v)))

  ;; Pre-eval hook: called before eval with the form. Takes one argument.
  ;; Default is a no-op.
  (define repl-pre-eval-hook
    (make-parameter
      (lambda (form) (void))
      (lambda (v)
        (unless (procedure? v)
          (error 'repl-pre-eval-hook "expected a procedure" v))
        v)))

  ;; Post-eval hook: called after eval with the form and result. Takes two arguments.
  ;; Default is a no-op.
  (define repl-post-eval-hook
    (make-parameter
      (lambda (form result) (void))
      (lambda (v)
        (unless (procedure? v)
          (error 'repl-post-eval-hook "expected a procedure" v))
        v)))

  ;; === Status and timing parameters ===

  ;; Last command exit status: 0 for success, 1 for exception,
  ;; decoded exit code for wait-status-shaped integers.
  (define last-status
    (make-parameter 0
      (lambda (v)
        (unless (and (integer? v) (exact? v))
          (error 'last-status "expected an exact integer" v))
        v)))

  ;; Last command duration in milliseconds.
  (define last-duration
    (make-parameter 0
      (lambda (v)
        (unless (and (integer? v) (exact? v) (>= v 0))
          (error 'last-duration "expected a non-negative exact integer" v))
        v)))

  ;; === Feature toggles ===

  ;; Toggle for shell-compatibility mode.
  ;; When #t (default), bare commands like "ls -la" are routed to the shell
  ;; executor.  Set to #f to treat all input as Scheme expressions.
  (define shell-mode?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; Toggle for history expansion (!!, !$, !n, !-n, !prefix).
  ;; When #t (default), history expansion is performed before evaluation.
  ;; Set to #f to disable (e.g. if ! in identifiers causes problems).
  (define history-expansion?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; === Terminal width ===

  ;; Terminal width parameter, updated on SIGWINCH.
  ;; Defaults to 80 columns.
  (define terminal-width
    (make-parameter 80
      (lambda (v)
        (unless (and (integer? v) (exact? v) (> v 0))
          (error 'terminal-width "expected positive exact integer" v))
        v)))

  ;; Query actual terminal width via ioctl(TIOCGWINSZ).
  ;; Returns the column count, or 80 as fallback if not a terminal.
  (define query-terminal-width
    (let ([c-ioctl (foreign-procedure "hafod_ioctl_ptr" (int unsigned-long void*) int)])
      (lambda ()
        (let ([buf (foreign-alloc 8)])  ;; struct winsize = 4 unsigned shorts = 8 bytes
          (let try-fd ([fds '(1 0 2)])  ;; try stdout, stdin, stderr
            (cond
              [(null? fds)
               (foreign-free buf)
               80]  ;; fallback
              [else
               (let ([rc (c-ioctl (car fds) TIOCGWINSZ buf)])
                 (if (zero? rc)
                     (let ([cols (foreign-ref 'unsigned-16 buf 2)])  ;; ws_col at offset 2
                       (foreign-free buf)
                       (if (> cols 0) cols 80))
                     (try-fd (cdr fds))))]))))))

  ;; Query actual terminal size (rows and columns) via ioctl(TIOCGWINSZ).
  ;; Returns (values rows cols) with fallback 24x80 if not a terminal.
  (define query-terminal-size
    (let ([c-ioctl (foreign-procedure "hafod_ioctl_ptr" (int unsigned-long void*) int)])
      (lambda ()
        (let ([buf (foreign-alloc 8)])  ;; struct winsize = 4 unsigned shorts = 8 bytes
          (let try-fd ([fds '(1 0 2)])  ;; try stdout, stdin, stderr
            (cond
              [(null? fds)
               (foreign-free buf)
               (values 24 80)]  ;; fallback
              [else
               (let ([rc (c-ioctl (car fds) TIOCGWINSZ buf)])
                 (if (zero? rc)
                     (let* ([rows (foreign-ref 'unsigned-16 buf 0)]  ;; ws_row at offset 0
                            [cols (foreign-ref 'unsigned-16 buf 2)]) ;; ws_col at offset 2
                       (foreign-free buf)
                       (values (if (> rows 0) rows 24)
                               (if (> cols 0) cols 80)))
                     (try-fd (cdr fds))))]))))))

  ;; === Colourised output ===

  ;; Pretty-print a value with syntax colouring via tokenize + display-colourised.
  ;; Uses terminal-width for pretty-line-length.
  (define (pretty-print-colourised value port)
    (let* ([sp (open-output-string)])
      (parameterize ([pretty-line-length (terminal-width)])
        (pretty-print value sp))
      (let* ([str (get-output-string sp)]
             ;; Remove trailing newline that pretty-print adds
             [str (if (and (> (string-length str) 0)
                           (char=? (string-ref str (- (string-length str) 1)) #\newline))
                      (substring str 0 (- (string-length str) 1))
                      str)]
             [tokens (tokenize str)])
        ;; Pass cursor-pos = -1 to skip enclosing-paren highlighting
        (display-colourised port str tokens -1)
        (newline port))))

  ;; === Helpers ===

  ;; Compute visible character count of a string, stripping ANSI escape sequences.
  ;; Handles CSI sequences (ESC [ ... final-byte) and non-CSI escapes (ESC + 1 char).
  (define (ansi-visible-length str)
    (let ([len (string-length str)]
          [esc (integer->char #x1b)])
      (let loop ([i 0] [visible 0])
        (cond
          [(>= i len) visible]
          [(and (char=? (string-ref str i) esc)
                (< (+ i 1) len)
                (char=? (string-ref str (+ i 1)) #\[))
           ;; CSI sequence: skip until final byte (0x40-0x7E range)
           (let skip ([j (+ i 2)])
             (cond
               [(>= j len) visible]  ;; malformed, stop
               [(let ([c (char->integer (string-ref str j))])
                  (and (>= c #x40) (<= c #x7E)))
                (loop (+ j 1) visible)]  ;; skip final byte too
               [else (skip (+ j 1))]))]
          [(char=? (string-ref str i) esc)
           ;; Non-CSI escape: ESC followed by one char (e.g., ESC 7, ESC 8)
           (if (< (+ i 1) len)
               (loop (+ i 2) visible)
               (loop (+ i 1) visible))]
          [else
           (loop (+ i 1) (+ visible 1))]))))

  ;; Display right prompt at the right terminal edge using ANSI cursor positioning.
  ;; Captures right prompt hook output, calculates column, emits ESC 7 / ESC[colG / ESC 8.
  ;; Skips if output is empty or terminal too narrow.
  (define (display-right-prompt port)
    (let ([str (let ([p (open-output-string)])
                 (parameterize ([current-output-port p])
                   ((repl-right-prompt-hook)))
                 (get-output-string p))])
      (when (> (string-length str) 0)
        (let* ([visible-len (ansi-visible-length str)]
               [col (+ (- (terminal-width) visible-len) 1)])
          (when (> col 0)
            (display "\x1b;7" port)
            (display (format "\x1b;[~aG" col) port)
            (display str port)
            (display "\x1b;8" port))))))

  ;; Create a continuation port with reset capability.
  ;; Wraps real-port; after the first line, displays repl-continuation-prompt before each
  ;; subsequent read! triggered by a newline.
  ;; Returns (values port reset-proc) -- call reset-proc before each read to clear state.
  ;; Key design: reads one character at a time from real-port to ensure the continuation
  ;; prompt fires between lines. get-string-n! would aggregate multiple underlying reads,
  ;; preventing the prompt from appearing.
  ;; Tracks whether a significant (non-whitespace) character has been seen, to avoid
  ;; showing a continuation prompt for trailing whitespace from the previous expression.
  (define (make-continuation-port real-port)
    (let ([in-expr? #f]       ;; have we seen non-whitespace? (expression started)
          [need-prompt? #f])  ;; was a newline seen? (next read should show prompt)
      (values
        (make-custom-textual-input-port
          "continuation-input-port"
          ;; read! : string * int * int -> int
          (lambda (buf start count)
            ;; If we're inside an expression and need a continuation prompt, display it
            (when (and need-prompt? in-expr?)
              (let ([cp (repl-continuation-prompt)])
                (when (and (string? cp) (> (string-length cp) 0))
                  (display cp (console-output-port))
                  (flush-output-port (console-output-port))))
              (set! need-prompt? #f))
            ;; Read one character at a time from real port to ensure per-line callbacks
            (let ([c (read-char real-port)])
              (cond
                [(eof-object? c) 0]
                [else
                 (string-set! buf start c)
                 (cond
                   [(char=? c #\newline)
                    (when in-expr?
                      (set! need-prompt? #t))]
                   [(not (char-whitespace? c))
                    (set! in-expr? #t)])
                 1])))
          #f  ;; get-position
          #f  ;; set-position!
          #f  ;; close -- do NOT close underlying port
          )
        ;; Reset procedure: call before each read to reset continuation state
        (lambda ()
          (set! in-expr? #f)
          (set! need-prompt? #f)))))

  ;; Compute elapsed milliseconds between two time-monotonic objects.
  (define (elapsed-milliseconds t0 t1)
    (let* ([s0 (time-second t0)]
           [ns0 (time-nanosecond t0)]
           [s1 (time-second t1)]
           [ns1 (time-nanosecond t1)]
           [ds (- s1 s0)]
           [dns (- ns1 ns0)])
      ;; Handle nanosecond underflow by borrowing from seconds
      (if (< dns 0)
          (+ (* (- ds 1) 1000) (quotient (+ dns 1000000000) 1000000))
          (+ (* ds 1000) (quotient dns 1000000)))))

  ;; Convert eval result to exit status code.
  ;; If result looks like a wait status (exact non-negative integer),
  ;; decode it using status:exit-val or status:term-sig.
  ;; Otherwise return 0 (success).
  ;; Only decode via status:exit-val (low 7 bits = 0, i.e. multiples of 128).
  ;; term-sig detection is skipped to avoid false positives on small integers
  ;; (e.g., 3 from (+ 1 2) would otherwise look like "killed by signal 3").
  ;; Process wait statuses from actual waitpid always have exit-val decodable form.
  (define (result->exit-status result)
    (if (and (integer? result) (exact? result) (>= result 0))
        (cond
          [(status:exit-val result) => (lambda (code) code)]
          [else 0])
        0))

  ;; === Command timing display ===

  ;; Format milliseconds as human-readable duration.
  (define (format-duration ms)
    (cond
      [(< ms 1000) #f]  ; don't display sub-second
      [(< ms 60000)
       (let* ([s (/ ms 1000.0)])
         (format #f "~,1fs" s))]
      [(< ms 3600000)
       (let* ([total-s (quotient ms 1000)]
              [m (quotient total-s 60)]
              [s (remainder total-s 60)])
         (format #f "~am ~as" m s))]
      [else
       (let* ([total-s (quotient ms 1000)]
              [h (quotient total-s 3600)]
              [m (quotient (remainder total-s 3600) 60)]
              [s (remainder total-s 60)])
         (format #f "~ah ~am ~as" h m s))]))

  ;; Display duration to stderr if > 2 seconds.
  (define (display-command-timing!)
    (let ([dur (last-duration)])
      (when (>= dur 2000)
        (let ([str (format-duration dur)])
          (when str
            (display "\x1b;[38;5;240m" (console-error-port))
            (display str (console-error-port))
            (display "\x1b;[39m" (console-error-port))
            (newline (console-error-port)))))))

  ;; === Command-not-found suggestions ===

  ;; Extract first whitespace-delimited token from a string.
  (define (first-token str)
    (let ([len (string-length str)])
      (let skip-ws ([i 0])
        (cond
          [(>= i len) ""]
          [(char-whitespace? (string-ref str i)) (skip-ws (+ i 1))]
          [else
           (let collect ([j i])
             (cond
               [(or (>= j len) (char-whitespace? (string-ref str j)))
                (substring str i j)]
               [else (collect (+ j 1))]))]))))

  ;; Check if a command exists in PATH cache; if not, suggest similar names.
  ;; Shows up to 3 fuzzy-matched suggestions from the PATH cache.
  (define (command-not-found-check line)
    (let* ([cmd (first-token line)]
           [ht (path-cache)]
           [keys (vector->list (hashtable-keys ht))])
      (when (and (> (string-length cmd) 0)
                 (not (hashtable-ref ht cmd #f)))
        (let ([suggestions (let ([filtered (fuzzy-filter cmd keys)])
                             (if (> (length filtered) 3)
                                 (list (car filtered) (cadr filtered) (caddr filtered))
                                 filtered))])
          (display "\x1b;[38;5;240m" (console-error-port))
          (display cmd (console-error-port))
          (display ": command not found" (console-error-port))
          (when (pair? suggestions)
            (display ". Did you mean: " (console-error-port))
            (let lp ([rest suggestions] [first? #t])
              (unless (null? rest)
                (unless first? (display ", " (console-error-port)))
                (display (car rest) (console-error-port))
                (lp (cdr rest) #f)))
            (display "?" (console-error-port)))
          (display "\x1b;[39m" (console-error-port))
          (newline (console-error-port))))))

  ;; === REPL state ===

  ;; Mutable variable tracking eval start time. #f when not in eval.
  ;; Used by keyboard-interrupt-handler to capture partial duration.
  (define current-t0 #f)

  ;; Tag for multi-expression paste (unique vector, not eq? to any user value)
  (define %multi-form-tag (vector 'hafod-multi-form))

  ;; === Script evaluation ===

  ;; Evaluate a string as a sequence of Scheme forms in the interaction environment.
  ;; Scheme equivalent of bash's eval "$(cmd)" pattern.
  ;; Reads all top-level forms from the string and evaluates each in order.
  (define (eval-script str)
    (let ([port (open-input-string str)])
      (let loop ()
        (let ([form (read port)])
          (unless (eof-object? form)
            (eval form (interaction-environment))
            (loop))))))

  ;; === REPL loop ===

  (define (interactive-repl)
    ;; SHLVL management: increment on entry, decrement on exit
    (let ([old-shlvl (let ([v (getenv "SHLVL")])
                       (if v (or (string->number v) 0) 0))])
      (setenv "SHLVL" (number->string (+ old-shlvl 1)))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          ;; Initialize terminal width
          (terminal-width (query-terminal-width))

          ;; Initialize PATH cache for shell-mode classification
          (rebuild-path-cache!)

          ;; Install job control signal handlers
          (install-job-signals!)

          ;; Register SIGWINCH handler to update terminal-width dynamically
          (register-signal-handler SIGWINCH
            (lambda (sig)
              (terminal-width (query-terminal-width))))

          ;; Main REPL loop with call/cc restart pattern
          ;; Determine at startup whether stdin is a terminal (editor vs bare read)
          (let ([use-editor? (tty? 0)])
          ;; Create continuation port once (persists across loop iterations to avoid losing buffered data)
          ;; Only needed for non-terminal mode
          (let-values ([(cport cport-reset!) (make-continuation-port (console-input-port))])
          (let ([restart-k #f])
            (let loop ()
              ;; Capture restart continuation at loop head
              (call/cc
                (lambda (k)
                  (set! restart-k k)))

              ;; Install reset-handler to restart loop on (reset)
              (reset-handler (lambda () (restart-k (void))))

              ;; Install keyboard-interrupt-handler for SIGINT (Ctrl-C)
              (keyboard-interrupt-handler
                (lambda ()
                  (newline (console-output-port))
                  (flush-output-port (console-output-port))
                  ;; Only set status/duration if we were in eval (current-t0 is set)
                  (when current-t0
                    (let ([t1 (current-time 'time-monotonic)])
                      (last-duration (elapsed-milliseconds current-t0 t1)))
                    (set! current-t0 #f)
                    (last-status 130))
                  (reset)))

              (let ([form
                     (if use-editor?
                         ;; Terminal mode: use line editor
                         (let* ([prompt-str
                                 (let ([sp (open-output-string)])
                                   (parameterize ([console-output-port sp]
                                                  [current-output-port sp])
                                     ((repl-prompt-hook)))
                                   (get-output-string sp))])
                           ;; Drain job notifications before prompt
                           (update-jobs!)
                           (for-each (lambda (msg)
                                       (display msg (console-error-port))
                                       (newline (console-error-port)))
                                     (drain-notifications!))
                           ;; Display right prompt before starting editor
                           (display-right-prompt (console-output-port))
                           (let ([line (with-raw-mode 0
                                         (lambda () (read-expression prompt-str)))])
                           (cond
                             [(eof-object? line) (eof-object)]
                             [else
                              ;; History expansion (!! !$ !n !-n !prefix)
                              (let* ([expanded (if (history-expansion?)
                                                   (history-expand line (editor-history-entries))
                                                   line)]
                                     [line (if (string=? expanded line)
                                               line
                                               (begin
                                                 ;; Print expanded command (bash convention)
                                                 (display expanded (console-error-port))
                                                 (newline (console-error-port))
                                                 expanded))])
                              (let ([class (if (shell-mode?)
                                               (classify-input line)
                                               'scheme)])
                                ;; Tag the history entry with its eval mode
                                (editor-history-set-last-mode!
                                  (if (memq class '(shell builtin)) 'shell 'scheme))
                                (case class
                                  [(builtin)
                                   ;; Execute builtin directly, skip eval
                                   (guard (exn
                                            [#t (display "\x1b;[31m" (console-error-port))
                                                (display-condition exn (console-error-port))
                                                (display "\x1b;[39m" (console-error-port))
                                                (newline (console-error-port))])
                                     (run-builtin! line))
                                   ;; Return (void) so the existing cond path skips display
                                   (void)]
                                  [(shell)
                                   ;; Parse to EPF datum, return for eval
                                   (parse-shell-command line)]
                                  [else
                                   ;; Original path (scheme)
                                   ;; Check for command-not-found when input doesn't
                                   ;; start with Scheme prefix chars
                                   (let* ([trimmed (let skip ([i 0])
                                                     (if (and (< i (string-length line))
                                                              (char-whitespace? (string-ref line i)))
                                                         (skip (+ i 1))
                                                         i))]
                                          [first-ch (and (< trimmed (string-length line))
                                                         (string-ref line trimmed))])
                                     (when (and first-ch
                                                (char-alphabetic? first-ch)
                                                (not (memv first-ch '(#\( #\' #\` #\# #\, #\[))))
                                       (command-not-found-check line)))
                                   ;; Read all s-expressions from the input.
                                   ;; Multiple expressions are tagged so the
                                   ;; eval loop can process each one individually,
                                   ;; printing results between them.
                                   (let* ([p (open-input-string line)]
                                          [first (read p)])
                                     (if (eof-object? first)
                                         first
                                         (let ([second (read p)])
                                           (if (eof-object? second)
                                               first
                                               (let gather ([acc (list second first)])
                                                 (let ([next (read p)])
                                                   (if (eof-object? next)
                                                       (cons %multi-form-tag (reverse acc))
                                                       (gather (cons next acc)))))))))])))])))
                         ;; Non-terminal mode: existing behaviour
                         (begin
                           ;; 1. Display right prompt (before left prompt)
                           (display-right-prompt (console-output-port))
                           ;; 2. Call left prompt hook
                           ((repl-prompt-hook))
                           ;; 3. Flush after both prompts
                           (flush-output-port (console-output-port))
                           ;; 4. Reset continuation port state and read
                           (cport-reset!)
                           (read cport)))])
                (cond
                  [(eof-object? form)
                   ;; EOF: exit cleanly
                   (newline (console-output-port))
                   (void)]
                  [(eq? form (void))
                   ;; Builtin already executed, loop without eval
                   (loop)]
                  [(and (pair? form) (eq? (car form) %multi-form-tag))
                   ;; Multiple pasted expressions: eval each, print each result
                   (let eval-each ([forms (cdr form)])
                     (if (null? forms)
                         (loop)
                         (let ([f (car forms)]
                               [more (cdr forms)])
                           (guard (exn
                                    [#t
                                     (display "\x1b;[31m" (console-error-port))
                                     (display-condition exn (console-error-port))
                                     (display "\x1b;[39m" (console-error-port))
                                     (newline (console-error-port))
                                     ;; Stop on error (don't eval remaining forms)
                                     (loop)])
                             (call-with-values
                               (lambda () (eval f (interaction-environment)))
                               (lambda results
                                 (cond
                                   [(null? results) (void)]
                                   [(and (null? (cdr results)) (eq? (car results) (void)))
                                    (void)]
                                   [(null? (cdr results))
                                    (pretty-print-colourised (car results) (console-output-port))]
                                   [else
                                    (for-each
                                      (lambda (v)
                                        (unless (eq? v (void))
                                          (pretty-print-colourised v (console-output-port))))
                                      results)])
                                 (eval-each more)))))))]
                  [else
                   ;; 3. Pre-eval hook
                   ((repl-pre-eval-hook) form)

                   ;; Mark: we are now in eval (for keyboard-interrupt-handler)
                   (set! current-t0 (current-time 'time-monotonic))

                   ;; During eval, use a non-escaping interrupt handler.
                   ;; The default handler calls (reset) which abandons the
                   ;; eval — breaking foreground child processes that are
                   ;; still running.  Instead, raise a condition so the
                   ;; guard below handles it normally.
                   (keyboard-interrupt-handler
                     (lambda ()
                       (newline (console-output-port))
                       (flush-output-port (console-output-port))
                       (when current-t0
                         (let ([t1 (current-time 'time-monotonic)])
                           (last-duration (elapsed-milliseconds current-t0 t1)))
                         (set! current-t0 #f)
                         (last-status 130))
                       (raise (make-message-condition "interrupted"))))

                   ;; 4. Eval with exception handling
                   (guard (exn
                            [#t
                             ;; Set timing and status (unless already set by interrupt handler)
                             (when current-t0
                               (let ([t1 (current-time 'time-monotonic)])
                                 (last-duration (elapsed-milliseconds current-t0 t1))
                                 (set! current-t0 #f)
                                 (last-status 1)))
                             (display "\x1b;[31m" (console-error-port))
                             (display-condition exn (console-error-port))
                             (display "\x1b;[39m" (console-error-port))
                             (newline (console-error-port))
                             ;; Post-eval hook (failure case)
                             ((repl-post-eval-hook) form (void))
                             (display-command-timing!)
                             (loop)])
                     (call-with-values
                       (lambda () (eval form (interaction-environment)))
                       (lambda results
                         ;; Capture end time and clear eval marker
                         (let ([t1 (current-time 'time-monotonic)]
                               [primary (if (null? results) (void) (car results))])
                           (last-duration (elapsed-milliseconds current-t0 t1))
                           (set! current-t0 #f)
                           (last-status (result->exit-status primary)))
                         (cond
                           ;; Zero values (e.g. (values)) or single void: nothing to print
                           [(null? results)
                            ((repl-post-eval-hook) form (void))]
                           [(and (null? (cdr results)) (eq? (car results) (void)))
                            ((repl-post-eval-hook) form (void))]
                           ;; Single value: pretty-print with syntax colouring
                           [(null? (cdr results))
                            (pretty-print-colourised (car results) (console-output-port))
                            ((repl-post-eval-hook) form (car results))]
                           ;; Multiple values: print each with syntax colouring
                           [else
                            (for-each
                              (lambda (v)
                                (unless (eq? v (void))
                                  (pretty-print-colourised v (console-output-port))))
                              results)
                            ((repl-post-eval-hook) form (car results))])
                         (display-command-timing!)
                         (loop))))])))))))
        (lambda ()
          ;; Cleanup: decrement SHLVL, never below 0
          (let ([cur (let ([v (getenv "SHLVL")])
                       (if v (or (string->number v) 0) 1))])
            (setenv "SHLVL" (number->string (max 0 (- cur 1)))))))))

  ) ; end library
