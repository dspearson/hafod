;;; (hafod interactive) -- Interactive REPL loop for hafod
;;; Provides configurable read-eval-print loop with prompt and eval hooks.
;;; Handles SIGINT (Ctrl-C) to interrupt evaluation and SIGWINCH for terminal width.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod interactive)
  (export
    interactive-repl
    eval-script
    repl-prompt-hook
    repl-right-prompt-hook
    repl-pre-eval-hook
    repl-post-eval-hook
    last-status
    last-duration
    terminal-width
    query-terminal-width
    repl-continuation-prompt
    ansi-visible-length
    background-job-count)

  (import (except (chezscheme) getenv)
          (only (hafod posix) status:exit-val SIGWINCH)
          (only (hafod internal posix-constants) TIOCGWINSZ)
          (only (hafod environment) getenv setenv)
          (only (hafod procobj) background-job-count)
          (only (hafod editor editor) read-expression with-raw-mode)
          (only (hafod tty) tty?))

  ;; === Hook parameters ===

  ;; Prompt hook: called before each read. Takes no arguments.
  ;; Default displays "> " and flushes output.
  (define repl-prompt-hook
    (make-parameter
      (lambda ()
        (display "> " (console-output-port))
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
    (let ([s0 (time-second t0)]
          [ns0 (time-nanosecond t0)]
          [s1 (time-second t1)]
          [ns1 (time-nanosecond t1)])
      (let ([ds (- s1 s0)]
            [dns (- ns1 ns0)])
        ;; Handle nanosecond underflow by borrowing from seconds
        (if (< dns 0)
            (+ (* (- ds 1) 1000) (quotient (+ dns 1000000000) 1000000))
            (+ (* ds 1000) (quotient dns 1000000))))))

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

  ;; === REPL state ===

  ;; Mutable variable tracking eval start time. #f when not in eval.
  ;; Used by keyboard-interrupt-handler to capture partial duration.
  (define current-t0 #f)

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
                           ;; Display right prompt before starting editor
                           (display-right-prompt (console-output-port))
                           (let ([line (with-raw-mode 0
                                         (lambda () (read-expression prompt-str)))])
                             (if (eof-object? line)
                                 (eof-object)
                                 (read (open-input-string line)))))
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
                  [else
                   ;; 3. Pre-eval hook
                   ((repl-pre-eval-hook) form)

                   ;; Mark: we are now in eval (for keyboard-interrupt-handler)
                   (set! current-t0 (current-time 'time-monotonic))

                   ;; 4. Eval with exception handling
                   (guard (exn
                            [#t
                             ;; Set timing and status before post-eval hook
                             (let ([t1 (current-time 'time-monotonic)])
                               (last-duration (elapsed-milliseconds current-t0 t1))
                               (set! current-t0 #f)
                               (last-status 1))
                             (display-condition exn (console-error-port))
                             (newline (console-error-port))
                             ;; Post-eval hook (failure case)
                             ((repl-post-eval-hook) form (void))
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
                           ;; Single value: pretty-print it
                           [(null? (cdr results))
                            (pretty-print (car results) (console-output-port))
                            ((repl-post-eval-hook) form (car results))]
                           ;; Multiple values: print each
                           [else
                            (for-each
                              (lambda (v)
                                (unless (eq? v (void))
                                  (pretty-print v (console-output-port))))
                              results)
                            ((repl-post-eval-hook) form (car results))])
                         (loop))))])))))))
        (lambda ()
          ;; Cleanup: decrement SHLVL, never below 0
          (let ([cur (let ([v (getenv "SHLVL")])
                       (if v (or (string->number v) 0) 1))])
            (setenv "SHLVL" (number->string (max 0 (- cur 1)))))))))

  ) ; end library
