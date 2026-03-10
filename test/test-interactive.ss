(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod interactive)
        (only (hafod posix) posix-kill posix-getpid SIGINT SIGWINCH)
        (only (hafod internal posix-constants) TIOCGWINSZ)
        (only (hafod environment) getenv setenv)
        (only (hafod editor editor) read-expression)
        (only (hafod editor render) tokenize display-colourised))

(test-begin "interactive")

;; Helper: string-contains (not built into Chez)
(define (string-contains haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (and (>= hlen nlen)
         (let loop ([i 0])
           (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) #t]
             [else (loop (+ i 1))])))))

;; Helper: create a slow input port that delivers input one character at a time.
;; This simulates terminal-like input where read must make multiple read! calls.
(define (make-slow-input-port str)
  (let ([pos 0]
        [len (string-length str)])
    (make-custom-textual-input-port
      "slow-input-port"
      (lambda (buf start count)
        (if (>= pos len)
            0
            (begin
              (string-set! buf start (string-ref str pos))
              (set! pos (+ pos 1))
              1)))
      #f #f #f)))

;; Helper: run REPL with given input string, return captured output
(define (test-repl-with-input input-string)
  (let ([inp (open-input-string input-string)]
        [out (open-output-string)])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   ;; Reset hooks to defaults to isolate tests
                   [repl-prompt-hook (lambda () (void))]
                   [repl-pre-eval-hook (lambda (form) (void))]
                   [repl-post-eval-hook (lambda (form result) (void))])
      (interactive-repl))
    (get-output-string out)))

;; REPL-01a: Feeding "(+ 1 2)" produces output containing "3"
(test-assert "REPL-01a: eval (+ 1 2) produces 3"
  (let ([output (test-repl-with-input "(+ 1 2)\n")])
    (string-contains output "3")))

;; REPL-01b: Void result produces no pretty-printed output
(test-assert "REPL-01b: void result produces no value output"
  (let ([output (test-repl-with-input "(values)\n")])
    ;; Output should contain only the EOF newline (no prompt since we suppressed it, no value since void)
    (string=? output "\n")))

;; REPL-01c: Prompt hook is called before each read
(test-assert "REPL-01c: prompt hook called before each read"
  (let ([count 0]
        [inp (open-input-string "(+ 1 2)\n(+ 3 4)\n")]
        [out (open-output-string)])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   [repl-prompt-hook (lambda () (set! count (+ count 1)))]
                   [repl-pre-eval-hook (lambda (form) (void))]
                   [repl-post-eval-hook (lambda (form result) (void))])
      (interactive-repl))
    ;; Should be called 3 times: before first read, before second read, before EOF read
    (= count 3)))

;; REPL-01d: Pre-eval hook receives the form before eval
(test-assert "REPL-01d: pre-eval hook receives form"
  (let ([captured-form #f]
        [inp (open-input-string "(+ 1 2)\n")]
        [out (open-output-string)])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   [repl-prompt-hook (lambda () (void))]
                   [repl-pre-eval-hook (lambda (form) (set! captured-form form))]
                   [repl-post-eval-hook (lambda (form result) (void))])
      (interactive-repl))
    (equal? captured-form '(+ 1 2))))

;; REPL-01e: Post-eval hook receives form and result after eval
(test-assert "REPL-01e: post-eval hook receives form and result"
  (let ([captured-form #f]
        [captured-result #f]
        [inp (open-input-string "(+ 1 2)\n")]
        [out (open-output-string)])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   [repl-prompt-hook (lambda () (void))]
                   [repl-pre-eval-hook (lambda (form) (void))]
                   [repl-post-eval-hook (lambda (form result)
                                          (set! captured-form form)
                                          (set! captured-result result))])
      (interactive-repl))
    (and (equal? captured-form '(+ 1 2))
         (equal? captured-result 3))))

;; REPL-01f: Empty input (EOF immediately) exits cleanly without error
(test-assert "REPL-01f: EOF exits cleanly"
  (let ([output (test-repl-with-input "")])
    ;; Should exit without error; output may contain a newline at most
    (or (string=? output "") (string=? output "\n"))))

;; Definitions persist across REPL iterations
(test-assert "definitions persist: define x then reference x"
  (let ([output (test-repl-with-input "(define x 42)\nx\n")])
    (string-contains output "42")))

;; Exceptions are caught without killing the REPL
(test-assert "exceptions caught: error does not kill REPL"
  (let ([output (test-repl-with-input "(error 'test \"boom\")\n(+ 10 20)\n")])
    ;; Output should contain the error message AND the result 30
    (and (string-contains output "boom")
         (string-contains output "30"))))

;; === Helper: run REPL and capture last-status and last-duration ===
(define (test-repl-capture input-string)
  ;; Returns (status . duration) captured from the LAST eval
  (let ([captured-status #f]
        [captured-duration #f]
        [inp (open-input-string input-string)]
        [out (open-output-string)])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   [repl-prompt-hook (lambda () (void))]
                   [repl-pre-eval-hook (lambda (form) (void))]
                   [repl-post-eval-hook
                    (lambda (form result)
                      (set! captured-status (last-status))
                      (set! captured-duration (last-duration)))])
      (interactive-repl))
    (cons captured-status captured-duration)))

;; === REPL-02: Exit status capture ===

;; REPL-02a: Normal eval sets last-status to 0
(test-assert "REPL-02a: normal eval sets last-status to 0"
  (let ([result (test-repl-capture "(+ 1 2)\n")])
    (eqv? (car result) 0)))

;; REPL-02b: Exception sets last-status to 1
(test-assert "REPL-02b: exception sets last-status to 1"
  (let ([result (test-repl-capture "(error 'test \"boom\")\n")])
    (eqv? (car result) 1)))

;; REPL-02c: Wait-status integer (* 42 256) sets last-status to 42
(test-assert "REPL-02c: wait-status integer sets last-status via status:exit-val"
  (let ([result (test-repl-capture "(* 42 256)\n")])
    (eqv? (car result) 42)))

;; REPL-02d: Another wait-status-shaped integer with different exit code
(test-assert "REPL-02d: wait-status exit code 1 from (* 1 256)"
  ;; (* 1 256) = 256, status:exit-val returns 1
  (let ([result (test-repl-capture "(* 1 256)\n")])
    (eqv? (car result) 1)))

;; === REPL-03: Command duration measurement ===

;; REPL-03a: After eval, last-duration is a non-negative exact integer
(test-assert "REPL-03a: last-duration is non-negative exact integer"
  (let ([result (test-repl-capture "(+ 1 2)\n")])
    (and (exact? (cdr result))
         (integer? (cdr result))
         (>= (cdr result) 0))))

;; REPL-03b: After a slow expression, last-duration is >= 0
(test-assert "REPL-03b: last-duration >= 0 for slow expression"
  (let ([result (test-repl-capture "(let loop ([i 0]) (when (< i 1000000) (loop (+ i 1))))\n")])
    (and (exact? (cdr result))
         (integer? (cdr result))
         (>= (cdr result) 0))))

;; REPL-03c: After exception, last-duration is still set (non-negative)
(test-assert "REPL-03c: last-duration set even after exception"
  (let ([result (test-repl-capture "(error 'test \"boom\")\n")])
    (and (exact? (cdr result))
         (integer? (cdr result))
         (>= (cdr result) 0))))

;; === REPL-10d: TIOCGWINSZ constant check ===

(test-assert "REPL-10d: TIOCGWINSZ is a positive integer"
  (and (integer? TIOCGWINSZ) (> TIOCGWINSZ 0)))

;; === REPL-10a: terminal-width parameter exists and defaults to 80 ===

(test-assert "REPL-10a: terminal-width is a positive exact integer"
  (let ([w (terminal-width)])
    (and (integer? w) (exact? w) (> w 0))))

;; === REPL-10b: query-terminal-width returns positive exact integer ===

(test-assert "REPL-10b: query-terminal-width returns positive exact integer (80 fallback)"
  (let ([w (query-terminal-width)])
    (and (integer? w) (exact? w) (> w 0))))

;; === REPL-10c: SIGWINCH handler updates terminal-width ===

(test-assert "REPL-10c: SIGWINCH handler is registered and invoked"
  (let ([inp (open-input-string "(+ 1 2)\n")]
        [out (open-output-string)])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   [repl-prompt-hook (lambda () (void))]
                   [repl-pre-eval-hook (lambda (form) (void))]
                   [repl-post-eval-hook (lambda (form result) (void))])
      (interactive-repl))
    ;; After REPL has run (which registers SIGWINCH handler), send SIGWINCH
    (posix-kill (posix-getpid) SIGWINCH)
    ;; terminal-width should still be a positive integer (80 in test since not a real terminal)
    (let ([w (terminal-width)])
      (and (integer? w) (exact? w) (> w 0)))))

;; === REPL-04a: SIGINT during eval interrupts, REPL continues ===

(test-assert "REPL-04a: SIGINT during eval interrupts and REPL continues"
  (let ([output (test-repl-with-input
                  "(begin (posix-kill (posix-getpid) SIGINT) (let loop () (loop)))\n(+ 1 2)\n")])
    ;; The SIGINT should interrupt the infinite loop, and the next expression should succeed
    (string-contains output "3")))

;; === REPL-04b: last-status is 130 after SIGINT ===

(test-assert "REPL-04b: last-status is 130 after SIGINT interruption"
  (let ([output (test-repl-with-input
                  "(begin (posix-kill (posix-getpid) SIGINT) (let loop () (loop)))\n(last-status)\n")])
    (string-contains output "130")))

;; === REPL-05b: Prompt hook output appears in captured console output ===

(test-assert "REPL-05b: prompt hook output visible in captured output"
  (let ([inp (open-input-string "(+ 1 2)\n")]
        [out (open-output-string)])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   [repl-prompt-hook (lambda ()
                                      (display "myprompt> " (console-output-port))
                                      (flush-output-port (console-output-port)))]
                   [repl-right-prompt-hook (lambda () (void))]
                   [repl-pre-eval-hook (lambda (form) (void))]
                   [repl-post-eval-hook (lambda (form result) (void))])
      (interactive-repl))
    (let ([output (get-output-string out)])
      (string-contains output "myprompt> "))))

;; === REPL-06a: repl-right-prompt-hook parameter validation ===

(test-assert "REPL-06a: repl-right-prompt-hook exists and validates procedure"
  (and (procedure? (repl-right-prompt-hook))
       (guard (exn [#t #t])
         (repl-right-prompt-hook "not-a-procedure")
         #f)))

;; === REPL-06b: ansi-visible-length strips ANSI codes ===

(test-assert "REPL-06b: ansi-visible-length plain text"
  (= (ansi-visible-length "hello") 5))

(test-assert "REPL-06b: ansi-visible-length CSI color codes"
  (= (ansi-visible-length "\x1b;[31mred\x1b;[0m") 3))

(test-assert "REPL-06b: ansi-visible-length empty string"
  (= (ansi-visible-length "") 0))

(test-assert "REPL-06b: ansi-visible-length non-CSI escapes (ESC 7 / ESC 8)"
  (= (ansi-visible-length "\x1b;7\x1b;8") 0))

;; === REPL-06c: Right prompt output contains ANSI save/restore cursor ===

(test-assert "REPL-06c: right prompt contains ANSI save/restore and text"
  (let ([inp (open-input-string "(+ 1 2)\n")]
        [out (open-output-string)])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   [terminal-width 80]
                   [repl-prompt-hook (lambda () (void))]
                   [repl-right-prompt-hook (lambda () (display "rprompt"))]
                   [repl-continuation-prompt ""]
                   [repl-pre-eval-hook (lambda (form) (void))]
                   [repl-post-eval-hook (lambda (form result) (void))])
      (interactive-repl))
    (let ([output (get-output-string out)])
      (and (string-contains output "\x1b;7")
           (string-contains output "rprompt")
           (string-contains output "\x1b;8")))))

;; === REPL-06d: Right prompt suppressed on narrow terminal ===

(test-assert "REPL-06d: right prompt suppressed when terminal too narrow"
  ;; First expression sets terminal-width to 5, second expression triggers a prompt
  ;; cycle where the right prompt should be suppressed (rprompt=7 chars > width=5).
  ;; We check that the output after the width change does not contain save cursor.
  (let ([inp (open-input-string "(terminal-width 5)\n(+ 1 2)\n")]
        [out (open-output-string)]
        [saw-narrow-rprompt #f])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   [repl-prompt-hook (lambda () (void))]
                   [repl-right-prompt-hook (lambda () (display "rprompt"))]
                   [repl-continuation-prompt ""]
                   [repl-pre-eval-hook (lambda (form) (void))]
                   [repl-post-eval-hook (lambda (form result) (void))])
      (interactive-repl))
    ;; The first prompt cycle (before terminal-width 5) will have the right prompt.
    ;; After setting width to 5, the second prompt cycle should suppress it.
    ;; Count occurrences of ESC 7 -- should be exactly 1 (from the first prompt cycle only).
    (let ([output (get-output-string out)])
      (let count-esc7 ([i 0] [n 0])
        (cond
          [(> (+ i 1) (string-length output)) (= n 1)]
          [(and (char=? (string-ref output i) (integer->char #x1b))
                (< (+ i 1) (string-length output))
                (char=? (string-ref output (+ i 1)) #\7))
           (count-esc7 (+ i 2) (+ n 1))]
          [else (count-esc7 (+ i 1) n)])))))

;; === REPL-07a: repl-continuation-prompt parameter validation ===

(test-assert "REPL-07a: repl-continuation-prompt exists and defaults to '.. '"
  (and (string=? (repl-continuation-prompt) ".. ")
       (guard (exn [#t #t])
         (repl-continuation-prompt 42)
         #f)))

;; === REPL-07b: Continuation prompt appears for multi-line input ===

(test-assert "REPL-07b: continuation prompt appears for multi-line input"
  (let ([inp (make-slow-input-port "(define x\n  42)\n")]
        [out (open-output-string)])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   [repl-prompt-hook (lambda () (void))]
                   [repl-right-prompt-hook (lambda () (void))]
                   [repl-continuation-prompt "... "]
                   [repl-pre-eval-hook (lambda (form) (void))]
                   [repl-post-eval-hook (lambda (form result) (void))])
      (interactive-repl))
    (let ([output (get-output-string out)])
      (string-contains output "... "))))

;; === REPL-07c: Continuation prompt does NOT appear on single-line input ===

(test-assert "REPL-07c: no continuation prompt on single-line input"
  (let ([inp (make-slow-input-port "(+ 1 2)\n")]
        [out (open-output-string)])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   [repl-prompt-hook (lambda () (void))]
                   [repl-right-prompt-hook (lambda () (void))]
                   [repl-continuation-prompt ".. "]
                   [repl-pre-eval-hook (lambda (form) (void))]
                   [repl-post-eval-hook (lambda (form result) (void))])
      (interactive-repl))
    (let ([output (get-output-string out)])
      (not (string-contains output ".. ")))))

;; === REPL-08: SHLVL environment variable tracking ===

;; REPL-08a: SHLVL is incremented on REPL entry and decremented on exit
(test-assert "REPL-08a: SHLVL incremented during REPL, restored on exit"
  (begin
    (setenv "SHLVL" "5")
    (let ([inp (open-input-string "(getenv \"SHLVL\")\n")]
          [out (open-output-string)])
      (parameterize ([console-input-port inp]
                     [console-output-port out]
                     [console-error-port out]
                     [repl-prompt-hook (lambda () (void))]
                     [repl-right-prompt-hook (lambda () (void))]
                     [repl-pre-eval-hook (lambda (form) (void))]
                     [repl-post-eval-hook (lambda (form result) (void))])
        (interactive-repl))
      (let ([output (get-output-string out)]
            [after (getenv "SHLVL")])
        (and (string-contains output "\"6\"")
             (string=? after "5"))))))

;; REPL-08c: Missing SHLVL defaults to 0, becomes "1" during REPL
(test-assert "REPL-08c: missing SHLVL defaults to 0, becomes 1"
  (begin
    (setenv "SHLVL" #f)  ;; unset
    (let ([inp (open-input-string "(getenv \"SHLVL\")\n")]
          [out (open-output-string)])
      (parameterize ([console-input-port inp]
                     [console-output-port out]
                     [console-error-port out]
                     [repl-prompt-hook (lambda () (void))]
                     [repl-right-prompt-hook (lambda () (void))]
                     [repl-pre-eval-hook (lambda (form) (void))]
                     [repl-post-eval-hook (lambda (form result) (void))])
        (interactive-repl))
      (string-contains (get-output-string out) "\"1\""))))

;; === REPL-09: Background job count ===

;; REPL-09a: background-job-count returns 0 when no jobs exist
(test-assert "REPL-09a: background-job-count returns 0 with no jobs"
  (eqv? (background-job-count) 0))

;; REPL-09b: background-job-count returns an integer
(test-assert "REPL-09b: background-job-count returns an integer"
  (integer? (background-job-count)))

;; REPL-09c: background-job-count accessible from interactive REPL
(test-assert "REPL-09c: background-job-count accessible in REPL"
  (let ([inp (open-input-string "(background-job-count)\n")]
        [out (open-output-string)])
    (parameterize ([console-input-port inp]
                   [console-output-port out]
                   [console-error-port out]
                   [repl-prompt-hook (lambda () (void))]
                   [repl-right-prompt-hook (lambda () (void))]
                   [repl-pre-eval-hook (lambda (form) (void))]
                   [repl-post-eval-hook (lambda (form result) (void))])
      (interactive-repl))
    (string-contains (get-output-string out) "0")))

;; === INTG-01: read-expression importable and usable ===

(test-assert "INTG-01a: read-expression importable from (hafod editor editor)"
  (procedure? (eval 'read-expression (environment '(hafod editor editor)))))

;; Test the read-expression -> open-input-string -> read pipeline
(test-assert "INTG-01b: read from editor string produces correct S-expression"
  (let ([result (read (open-input-string "(+ 1 2)"))])
    (equal? result '(+ 1 2))))

;; Test read-expression with string port input returns expected string
(test-assert "INTG-01c: read-expression with string port returns input string"
  (let* ([input (open-input-string "(+ 1 2)\n")]
         [out (open-output-string)]
         [result (read-expression "> " input out)])
    (and (string? result)
         (equal? (read (open-input-string result)) '(+ 1 2)))))

;; Test read-expression EOF on empty input
(test-assert "INTG-01d: read-expression returns eof on empty input"
  (let* ([input (open-input-string "")]
         [out (open-output-string)]
         [result (read-expression "> " input out)])
    (eof-object? result)))

;; === REPL-OUT-01: tokenize and display-colourised exported from render.ss ===

(test-assert "REPL-OUT-01a: tokenize is a procedure (exported from render.ss)"
  (procedure? tokenize))

(test-assert "REPL-OUT-01b: display-colourised is a procedure (exported from render.ss)"
  (procedure? display-colourised))

(test-assert "REPL-OUT-01c: tokenize produces tokens for known input"
  (let ([tokens (tokenize "(1 \"hello\" #t foo)")])
    (and (list? tokens)
         (> (length tokens) 0)
         ;; Each token is (type start end depth)
         (for-all (lambda (tok) (and (list? tok) (= (length tok) 4))) tokens))))

(test-assert "REPL-OUT-01d: display-colourised produces ANSI-coloured output"
  (let* ([text "(1 \"hello\" #t foo)"]
         [tokens (tokenize text)]
         [port (open-output-string)])
    (display-colourised port text tokens -1)
    (let ([output (get-output-string port)])
      ;; Output should contain ANSI escape sequences (ESC[)
      (string-contains output "\x1b;["))))

;; === REPL-OUT-02: pretty-print-colourised in REPL output ===

(test-assert "REPL-OUT-02a: REPL output contains ANSI escapes for coloured output"
  (let ([output (test-repl-with-input "(list 1 2 3)\n")])
    ;; Output should contain ANSI colour codes from syntax highlighting
    (string-contains output "\x1b;[")))

(test-assert "REPL-OUT-02b: REPL output for multi-value contains ANSI escapes"
  (let ([output (test-repl-with-input "(values 1 2 3)\n")])
    ;; Each value should be colourised
    (string-contains output "\x1b;[")))

(test-assert "REPL-OUT-02c: REPL void suppression still works with colourised output"
  (let ([output (test-repl-with-input "(values)\n")])
    ;; Void should still produce no output (just the EOF newline)
    (string=? output "\n")))

;; === REPL-OUT-03: Error display in red ===

(test-assert "REPL-OUT-03a: error output contains red ANSI escape"
  (let ([output (test-repl-with-input "(error 'test \"boom\")\n(+ 1 2)\n")])
    ;; Error should be wrapped in red (ESC[31m)
    (and (string-contains output "\x1b;[31m")
         (string-contains output "boom"))))

;; === REPL-OUT-04: repl-prompt-string parameter ===

(test-assert "REPL-OUT-04a: repl-prompt-string is a parameter defaulting to \"> \""
  (string=? (repl-prompt-string) "> "))

(test-assert "REPL-OUT-04b: repl-prompt-string rejects non-strings"
  (guard (exn [#t #t])
    (repl-prompt-string 42)
    #f))

(test-assert "REPL-OUT-04c: repl-prompt-string accepts strings"
  (begin
    (let ([old (repl-prompt-string)])
      (repl-prompt-string "my> ")
      (let ([val (repl-prompt-string)])
        (repl-prompt-string old)
        (string=? val "my> ")))))

(test-end)
