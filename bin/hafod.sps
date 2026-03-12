#!chezscheme
;;; hafod -- Script launcher and REPL for hafod (scsh on Chez Scheme)
;;;
;;; scsh-compatible command-line interface:
;;;   hafod -s script.ss [args ...]   Run a script file
;;;   hafod -c '(expr)'               Evaluate an expression
;;;   hafod -e entry -s script.ss     Run script, then call (entry args)
;;;   hafod --                        Start interactive REPL
;;;   hafod                           Start interactive REPL
;;;   hafod --help                    Show usage
;;;
;;; Shebang usage:
;;;   #!/usr/bin/env hafod
;;;   !#
;;;   (display "Hello\n")
;;;
;;;   #!/usr/bin/env hafod
;;;   -e main -s
;;;   !#
;;;   (define (main args) (display "Hello\n"))
;;;
;;; Meta-arg style (hardcoded path, scsh-compatible):
;;;   #!/usr/local/bin/hafod \
;;;   -e main -s
;;;   !#
;;;   (define (main args) (display "Hello\n"))
;;;
;;; Copyright (c) 1995 Olin Shivers. R6RS adaptation (c) 2026 Dominic Pearson.

;; Import (hafod) at compile time so compile-whole-program can merge all
;; library code into bin/hafod.so, eliminating per-file library loading.
;; The 57 symbols that conflict between (chezscheme) and (hafod) are excluded
;; from (chezscheme); (hafod) provides its own versions of those.
(import
  (except (chezscheme)
    ;; 57 symbols that conflict with (hafod) -- exclude from (chezscheme)
    bitwise-and bitwise-ior bitwise-not bitwise-xor
    call-with-input-file call-with-output-file call-with-string-output-port
    char->integer char-alphabetic? char-lower-case? char-numeric? char-ready?
    char-upper-case? char-whitespace? command-line command-line-arguments
    current-error-port date? delete-directory delete-file display error exit
    file-directory? file-exists? file-options file-regular? format getenv
    input-port? integer->char list->string make-date newline number->string
    open-input-file open-output-file output-port? read-char record-reader
    rename-file string->list string->number string-append string-copy
    string-downcase string-upcase substring thread-join thread? time
    truncate-file vector-append with-input-from-file with-output-to-file
    write write-char)
  ;; Import Chez's command-line/command-line-arguments under prefixed names
  ;; so we can set them as parameters (hafod's versions are read-only).
  (only (rename (chezscheme)
                (command-line chez:command-line)
                (command-line-arguments chez:command-line-arguments))
    chez:command-line chez:command-line-arguments)
  (hafod))

;; port->string is now provided by (hafod) via (hafod port-collect).

;; Import (hafod) into the interaction-environment so user scripts, -c exprs,
;; and the REPL can use hafod symbols.  This is the main startup cost (~22ms).
;; Guarded: only imports once, subsequent calls are no-ops.
(define %hafod-imported? #f)
(define (ensure-hafod-interaction-environment!)
  (unless %hafod-imported?
    (eval '(import (hafod)) (interaction-environment))
    (set! %hafod-imported? #t)))

;; Wire fuzzy finder into editor.  The umbrella library body expression
;; (editor-finder-proc run-finder) is not reliably executed by Chez when
;; the library is loaded via compile-whole-program, so we do it here at
;; program startup where side effects are guaranteed to run.
(editor-finder-proc run-finder)

;; Set both the Chez command-line parameters AND the hafod command-line state.
;; chez:command-line / chez:command-line-arguments are the Chez parameters
;; (renamed to avoid conflict); set-command-line-args! sets hafod's internal state.
(define (set-command-line! args)
  (chez:command-line args)
  (chez:command-line-arguments (if (pair? args) (cdr args) '()))
  (set-command-line-args! args))

;; ======================================================================
;; Meta-argument processing
;; Ported from scsh/scheme/meta-arg.scm
;; ======================================================================

;; Read a backslash escape sequence from port (after the \ has been consumed).
(define (read-backslash-sequence port)
  (let ([c1 (read-char port)])
    (cond
      [(eof-object? c1)
       (error 'meta-arg "Premature EOF within backslash-sequence")]
      [(char=? c1 #\n) #\newline]
      [(char=? c1 #\r) #\return]
      [(char=? c1 #\t) #\tab]
      [(char=? c1 #\b) #\backspace]
      [(char=? c1 #\a) #\alarm]
      [(char=? c1 #\f) (integer->char 12)]  ;; form-feed
      [(char=? c1 #\v) (integer->char 11)]  ;; vertical-tab
      ;; Simple knockdowns: \, space, tab, newline
      [(memv c1 '(#\\ #\space #\tab #\newline)) c1]
      ;; Octal: exactly 3 digits
      [(and (char>=? c1 #\0) (char<=? c1 #\7))
       (let ([d1 (- (char->integer c1) (char->integer #\0))]
             [c2 (read-char port)]
             [c3 (read-char port)])
         (unless (and (char? c2) (char>=? c2 #\0) (char<=? c2 #\7))
           (error 'meta-arg "Non-octal digit in \\nnn escape" c2))
         (unless (and (char? c3) (char>=? c3 #\0) (char<=? c3 #\7))
           (error 'meta-arg "Non-octal digit in \\nnn escape" c3))
         (let ([d2 (- (char->integer c2) (char->integer #\0))]
               [d3 (- (char->integer c3) (char->integer #\0))])
           (integer->char (+ d3 (* 8 (+ d2 (* 8 d1)))))))]
      [else
       (error 'meta-arg "Illegal \\ escape sequence" c1)])))

;; Read one secondary arg from port (up to space/newline/eof).
(define (read-secondary-arg port)
  (let loop ([chars '()])
    (let ([c (peek-char port)])
      (cond
        [(or (eof-object? c) (char=? c #\newline) (char=? c #\space))
         (list->string (reverse chars))]
        [(char=? c #\tab)
         (error 'meta-arg "Illegal tab character in meta-arg argument line")]
        [(char=? c #\\)
         (read-char port)  ;; consume the backslash
         (loop (cons (read-backslash-sequence port) chars))]
        [else
         (loop (cons (read-char port) chars))]))))

;; Read a line of secondary args from port.
(define (read-secondary-args port)
  (let loop ([args '()])
    (let* ([arg (read-secondary-arg port)]
           [c (read-char port)])
      (if (or (eof-object? c) (char=? c #\newline))
          (reverse (cons arg args))
          ;; c was a space — continue reading
          (loop (cons arg args))))))

;; Read secondary args from line 2 of file.
(define (read-file-secondary-args fname)
  (call-with-input-file fname
    (lambda (port)
      ;; Skip line 1
      (let skip ()
        (let ([c (read-char port)])
          (unless (or (eof-object? c) (char=? c #\newline))
            (skip))))
      ;; Read args from line 2
      (read-secondary-args port))))

;; Expand meta-args: if args starts with ("\" <filename> ...), expand.
(define (meta-arg-process-arglist args)
  (let loop ([args args])
    (if (and (pair? args) (string=? (car args) "\\"))
        (loop (append (read-file-secondary-args (cadr args))
                      (cdr args)))
        args)))

;; ======================================================================
;; Source preprocessor: | -> pipe, |+ -> pipe+
;; Transforms lone | and |+ symbols before the Chez reader sees them.
;; This is needed because Chez Scheme's reader treats | as a symbol
;; escape character, making (| (ls) (grep "foo")) unparseable.
;; ======================================================================

;; Returns #t if c is a delimiter that ends a symbol.
(define (delimiter? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (memv c '(#\( #\) #\[ #\] #\; #\" #\#))))

;; Preprocess source text: replace lone | with pipe, |+ with pipe+.
;; Preserves | inside strings, comments, character literals, and || (empty symbol).
(define (preprocess-pipe-symbols src)
  (let ([len (string-length src)])
    (let loop ([i 0] [out '()])
      (if (>= i len)
          (list->string (reverse out))
          (let ([c (string-ref src i)])
            (cond
              ;; String literal -- skip to closing "
              [(char=? c #\")
               (let sloop ([j (+ i 1)] [acc (cons c out)])
                 (cond
                   [(>= j len) (loop j acc)]
                   [(char=? (string-ref src j) #\\)
                    (if (< (+ j 1) len)
                        (sloop (+ j 2) (cons (string-ref src (+ j 1))
                                             (cons #\\ acc)))
                        (sloop (+ j 1) (cons #\\ acc)))]
                   [(char=? (string-ref src j) #\")
                    (loop (+ j 1) (cons #\" acc))]
                   [else
                    (sloop (+ j 1) (cons (string-ref src j) acc))]))]
              ;; Line comment -- skip to newline
              [(char=? c #\;)
               (let cloop ([j i] [acc out])
                 (if (or (>= j len) (char=? (string-ref src j) #\newline))
                     (loop j acc)
                     (cloop (+ j 1) (cons (string-ref src j) acc))))]
              ;; Block comment #| ... |#
              [(and (char=? c #\#)
                    (< (+ i 1) len)
                    (char=? (string-ref src (+ i 1)) #\|))
               (let bloop ([j (+ i 2)] [depth 1] [acc (cons #\| (cons #\# out))])
                 (cond
                   [(>= j len) (loop j acc)]
                   [(and (char=? (string-ref src j) #\|)
                         (< (+ j 1) len)
                         (char=? (string-ref src (+ j 1)) #\#))
                    (if (= depth 1)
                        (loop (+ j 2) (cons #\# (cons #\| acc)))
                        (bloop (+ j 2) (- depth 1) (cons #\# (cons #\| acc))))]
                   [(and (char=? (string-ref src j) #\#)
                         (< (+ j 1) len)
                         (char=? (string-ref src (+ j 1)) #\|))
                    (bloop (+ j 2) (+ depth 1) (cons #\| (cons #\# acc)))]
                   [else
                    (bloop (+ j 1) depth (cons (string-ref src j) acc))]))]
              ;; Character literal #\|
              [(and (char=? c #\#)
                    (< (+ i 2) len)
                    (char=? (string-ref src (+ i 1)) #\\)
                    (char=? (string-ref src (+ i 2)) #\|))
               (loop (+ i 3) (cons #\| (cons #\\ (cons #\# out))))]
              ;; Symbol escape |...| -- pass through unchanged
              [(char=? c #\|)
               (let ([next (if (< (+ i 1) len) (string-ref src (+ i 1)) #\space)])
                 (cond
                   ;; || -- empty symbol (scsh's OR combinator) -- pass through
                   [(char=? next #\|)
                    (loop (+ i 2) (cons #\| (cons #\| out)))]
                   ;; |+ followed by delimiter -> pipe+
                   [(and (char=? next #\+)
                         (let ([after (if (< (+ i 2) len) (string-ref src (+ i 2)) #\space)])
                           (delimiter? after)))
                    (loop (+ i 2)
                          (append (reverse (string->list "pipe+")) out))]
                   ;; | followed by delimiter -> pipe
                   [(delimiter? next)
                    (loop (+ i 1)
                          (append (reverse (string->list "pipe")) out))]
                   ;; |symbol| escape sequence -- pass through to closing |
                   [else
                    (let eloop ([j (+ i 1)] [acc (cons c out)])
                      (cond
                        [(>= j len) (loop j acc)]
                        [(char=? (string-ref src j) #\|)
                         (loop (+ j 1) (cons #\| acc))]
                        [else
                         (eloop (+ j 1) (cons (string-ref src j) acc))]))]))]
              ;; Everything else -- pass through
              [else
               (loop (+ i 1) (cons c out))]))))))

;; ======================================================================
;; !# header stripping
;; ======================================================================

;; Strip the script header: everything from start up to and including
;; a line containing just "!#" (with optional trailing whitespace).
;; Returns the remaining script content as a string.
(define (strip-script-header fname)
  (call-with-input-file fname
    (lambda (port)
      ;; Read lines until we find one starting with !#
      (let skip ()
        (let ([line (get-line port)])
          (cond
            [(eof-object? line)
             ;; No !# found -- return empty string (degenerate case)
             ""]
            [(and (>= (string-length line) 2)
                  (char=? (string-ref line 0) #\!)
                  (char=? (string-ref line 1) #\#))
             ;; Found !# -- rest of port is the script body
             (port->string port)]
            [else (skip)]))))))

;; ======================================================================
;; Header argument extraction
;; ======================================================================

;; Extract meta-arguments from a script's #!...!# header.
;; Reads lines between the shebang line and the !# terminator, parsing
;; each line using the scsh secondary-arg reader (with backslash escapes).
;; Returns a flat list of argument strings, or '() if no header.
(define (extract-script-header-args fname)
  (call-with-input-file fname
    (lambda (port)
      ;; N.B. let* is essential: Chez evaluates let init-exprs right-to-left,
      ;; so (let ([c1 (read-char p)] [c2 (read-char p)]) ...) reads c2 first.
      (let* ([c1 (read-char port)]
             [c2 (read-char port)])
        (if (and (eqv? c1 #\#) (eqv? c2 #\!))
            (begin
              ;; Skip rest of line 1 (the shebang line)
              (let skip ()
                (let ([c (read-char port)])
                  (unless (or (eof-object? c) (char=? c #\newline))
                    (skip))))
              ;; Read arg lines until !# or EOF
              (let loop ([all-args '()])
                (let ([line (get-line port)])
                  (cond
                    [(eof-object? line) (reverse all-args)]
                    [(and (>= (string-length line) 2)
                          (char=? (string-ref line 0) #\!)
                          (char=? (string-ref line 1) #\#))
                     (reverse all-args)]
                    [else
                     (let* ([args (read-secondary-args
                                   (open-input-string line))]
                            [non-empty (filter
                                         (lambda (s) (> (string-length s) 0))
                                         args)])
                       (loop (append (reverse non-empty) all-args)))]))))
            '())))))

;; Parse header flags: extract -e and -l from a header arg list.
;; -s is accepted and ignored (implicit for bare filename invocation).
;; Returns (values entry-symbol-or-#f preload-list).
(define (parse-header-flags args)
  (let loop ([args args] [entry #f] [preloads '()])
    (if (null? args)
        (values entry (reverse preloads))
        (let ([arg (car args)] [rest (cdr args)])
          (cond
            [(string=? arg "-s") (loop rest entry preloads)]
            [(string=? arg "-e")
             (if (null? rest)
                 (values entry (reverse preloads))
                 (loop (cdr rest) (string->symbol (car rest)) preloads))]
            [(string=? arg "-l")
             (if (null? rest)
                 (values entry (reverse preloads))
                 (loop (cdr rest) entry (cons (car rest) preloads)))]
            [else (loop rest entry preloads)])))))

;; Load a script file, stripping !# header if present.
;; Auto-imports (hafod) into the interaction environment before eval.
;; Preprocesses | -> pipe for scsh compatibility.
(define (load-script-file fname)
  (unless (file-exists? fname)
    (display (string-append "hafod: " fname ": No such file\n") (current-error-port))
    (exit 1))
  ;; Ensure (hafod) is available in the interaction environment
  (ensure-hafod-interaction-environment!)
  (let ([has-header?
          (call-with-input-file fname
            (lambda (port)
              (let ([c1 (read-char port)])
                (and (eqv? c1 #\#)
                     (let ([c2 (read-char port)])
                       (eqv? c2 #\!))))))])
    (let ([body (if has-header?
                    (strip-script-header fname)
                    (call-with-input-file fname port->string))])
      (let ([preprocessed (preprocess-pipe-symbols body)])
        (eval (cons 'begin
                    (let ([p (open-input-string preprocessed)])
                      (let loop ([forms '()])
                        (let ([form (read p)])
                          (if (eof-object? form)
                              (reverse forms)
                              (loop (cons form forms)))))))
              (interaction-environment))))))

;; ======================================================================
;; Usage
;; ======================================================================

(define (show-usage)
  (for-each display
    '("Usage: hafod [switches] [terminator] [args ...]\n"
      "\n"
      "Switches:\n"
      "  -e PROC    After loading script, call (PROC args) as entry point\n"
      "  -l FILE    Load FILE before executing main action (repeatable)\n"
      "  --login    Start as a login shell\n"
      "  --no-config  Skip loading config files\n"
      "  --norc     Alias for --no-config (backward compat)\n"
      "\n"
      "Terminators (at most one):\n"
      "  -s FILE    Load and run FILE as a hafod/scsh script\n"
      "  -c EXPR    Evaluate EXPR and exit\n"
      "  --         Start interactive REPL (remaining args go to command-line)\n"
      "\n"
      "  --help     Show this help message\n"
      "  --version  Show version\n"
      "\n"
      "With no arguments, starts an interactive REPL with (hafod) imported.\n"
      "\n"
      "Startup files (interactive mode only):\n"
      "  ~/.config/hafod/init.ss  Loaded on interactive startup\n"
      "                           (or $XDG_CONFIG_HOME/hafod/init.ss)\n"
      "\n"
      "Shebang (meta-arg) usage:\n"
      "  #!/path/to/hafod \\\n"
      "  -e main -s\n"
      "  !#\n"
      "  (define (main args) ...)\n")))

(define (show-version)
  (display "hafod 1.1 (scsh on Chez Scheme)\n"))

;; ======================================================================
;; Argument parsing (scsh-compatible)
;; ======================================================================

(define (load-preload-files preloads)
  (for-each load-script-file (reverse preloads)))

(define (parse-and-execute raw-args)
  (let ([args (meta-arg-process-arglist raw-args)])
    (let loop ([args args]
               [entry #f]       ;; -e <entry-point>
               [preloads '()]   ;; -l files (in reverse order)
               [login? #f]      ;; --login flag
               [no-config? #f]) ;; --no-config flag
      (if (null? args)
          ;; No terminator -- interactive REPL
          (begin
            (set-command-line! '("hafod"))
            (load-preload-files preloads)
            (ensure-hafod-interaction-environment!)
            (unless no-config?
              (load-config-file (hafod-init-file)))
            (interactive-repl))

          (let ([arg (car args)]
                [rest (cdr args)])
            (cond
              ;; -l FILE -- preload a file (non-terminating, accumulates)
              [(string=? arg "-l")
               (when (null? rest)
                 (display "hafod: -l requires a filename\n" (current-error-port))
                 (exit 1))
               (loop (cdr rest) entry (cons (car rest) preloads) login? no-config?)]

              ;; -e ENTRY -- set entry point
              [(string=? arg "-e")
               (when (null? rest)
                 (display "hafod: -e requires a procedure name\n" (current-error-port))
                 (exit 1))
               (loop (cdr rest) (string->symbol (car rest)) preloads login? no-config?)]

              ;; --login -- mark as login shell
              [(string=? arg "--login")
               (loop rest entry preloads #t no-config?)]

              ;; --no-config / --norc -- skip config file loading
              [(or (string=? arg "--no-config") (string=? arg "--norc"))
               (loop rest entry preloads login? #t)]

              ;; -s FILE -- script mode (terminating)
              [(string=? arg "-s")
               (when (null? rest)
                 (display "hafod: -s requires a filename\n" (current-error-port))
                 (exit 1))
               (let ([script (car rest)]
                     [script-args (cdr rest)])
                 (set-command-line! (cons script script-args))
                 (load-preload-files preloads)
                 (load-script-file script)
                 (when entry
                   (let ([proc (eval entry (interaction-environment))])
                     (proc script-args))))]

              ;; -c EXPR -- expression mode (terminating)
              [(string=? arg "-c")
               (when entry
                 (display "hafod: -c cannot be combined with -e\n" (current-error-port))
                 (exit 1))
               (when (null? rest)
                 (display "hafod: -c requires an expression\n" (current-error-port))
                 (exit 1))
               (set-command-line! (cons "hafod" (cdr rest)))
               (load-preload-files preloads)
               (ensure-hafod-interaction-environment!)
               (let ([expr (read (open-input-string (car rest)))])
                 (eval expr (interaction-environment)))]

              ;; -- explicit REPL (terminating)
              [(string=? arg "--")
               (set-command-line! (cons "hafod" rest))
               (load-preload-files preloads)
               (ensure-hafod-interaction-environment!)
               (unless no-config?
                 (load-config-file (hafod-init-file)))
               (interactive-repl)]

              ;; --help
              [(string=? arg "--help")
               (show-usage)]

              ;; --version
              [(string=? arg "--version")
               (show-version)]

              ;; Unknown switch
              [(and (> (string-length arg) 0)
                    (char=? (string-ref arg 0) #\-))
               (display (string-append "hafod: unknown switch: " arg "\n")
                        (current-error-port))
               (show-usage)
               (exit 1)]

              ;; Bare filename -- treat as script (like -s).
              ;; If the script has a #!...!# header, parse inner lines
              ;; for -e/-l/-s flags (extends scsh for #!/usr/bin/env usage).
              [else
               (set-command-line! (cons arg rest))
               (load-preload-files preloads)
               (let-values ([(hdr-entry hdr-preloads)
                             (parse-header-flags
                               (extract-script-header-args arg))])
                 (for-each load-script-file hdr-preloads)
                 (load-script-file arg)
                 (let ([effective-entry (or entry hdr-entry)])
                   (when effective-entry
                     (let ([proc (eval effective-entry
                                      (interaction-environment))])
                       (proc rest)))))]))))))

;; ======================================================================
;; Main
;; ======================================================================

;; Detect login shell invocation: argv[0] starts with "-" (e.g., "-hafod").
;; This is the standard Unix convention used by login(1) and getty(8).
(let ([argv0 (car (chez:command-line))]
      [args (command-line-arguments)])
  (if (and (> (string-length argv0) 0)
           (char=? (string-ref argv0 0) #\-))
      (parse-and-execute (cons "--login" args))
      (parse-and-execute args)))
