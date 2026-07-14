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
    ;; 57 original symbols that conflict with (hafod)
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
    write write-char
    ;; SRFI-1 — (hafod) now provides these with SRFI-1 semantics
    cons* make-list list-copy iota last-pair
    filter find partition remove remove!
    append! reverse! break assoc
    fold-right
    ;; SRFI-13 — (hafod) now provides these with SRFI-13 semantics
    string-for-each string-copy! string-fill! string-hash)
  ;; Import Chez's command-line/command-line-arguments under prefixed names
  ;; so we can set them as parameters (hafod's versions are read-only).
  (only (rename (chezscheme)
                (command-line chez:command-line)
                (command-line-arguments chez:command-line-arguments))
    chez:command-line chez:command-line-arguments)
  ;; Recorded build Chez version, for the best-effort assert below.
  (hafod internal chez-version)
  ;; reassert-cooked-tty! backs the top-level cooked-mode guard around the Main
  ;; dispatch below.  It is a direct (only ...) import rather than an umbrella
  ;; export, so it introduces no binding that conflicts with (hafod).
  (only (hafod tty) reassert-cooked-tty!)
  ;; reset-signal-to-default! lets SIGPIPE terminate hafod quietly on a broken
  ;; downstream pipe.  A direct (only ...) import (not umbrella-exported), so the
  ;; umbrella surface is unchanged; SIGPIPE itself comes from (hafod).
  (only (hafod signal) reset-signal-to-default!)
  ;; colour-override backs the --color[=WHEN] flag below: setting it to
  ;; 'always/'never forces or suppresses SGR colour for any target, while the
  ;; default #f leaves the automatic NO_COLOR/CLICOLOR_FORCE/tty precedence in
  ;; charge.  A direct (only ...) import -- it is launcher plumbing, deliberately
  ;; NOT re-exported from the (hafod) umbrella, so the umbrella surface is
  ;; unchanged and it introduces no binding that conflicts with (hafod).
  (only (hafod terminal-caps) colour-override)
  (hafod))

;; Best-effort version guard for the source path.  This is the first body
;; expression, so it runs before any of the program's own side effects.  It
;; fires only when a full (compiler-bearing) Chez of the wrong version compiles
;; bin/hafod.sps directly: the fasl path never reaches here (the loader rejects
;; a mismatched bin/hafod.so first) and the petite source path never reaches
;; here either ((import (hafod)) is compiled before this runs).  The bin/hafod
;; wrapper covers those two; this catches the remaining full-but-wrong scheme.
;;
;; (scheme-version-number) returns THREE values, so bind it with let-values and
;; compare the rebuilt list against the recorded list literal -- never apply
;; equal? to the raw multiple values.
(let-values ([(maj min sub) (scheme-version-number)])
  (unless (equal? (list maj min sub) build-chez-version-number)
    ;; Render a triple as "maj.min.sub" so the message reads like a version,
    ;; not a run-together digit string.
    (let ([dotted (lambda (parts)
                    (let loop ([parts parts] [first #t])
                      (unless (null? parts)
                        (unless first (display "." (current-error-port)))
                        (display (car parts) (current-error-port))
                        (loop (cdr parts) #f))))])
      (display "hafod: Chez Scheme " (current-error-port))
      (dotted build-chez-version-number)
      (display " required, but running " (current-error-port))
      (dotted (list maj min sub))
      (display ".\n" (current-error-port))
      (display "hafod: run 'nix develop' (or set HAFOD_SCHEME), then 'make compile-wpo' to rebuild.\n"
               (current-error-port)))
    ;; Exit 1 to match the wrapper's version/compiler guards and the other
    ;; (exit 1) error handlers below: a wrong-version Chez always fails with the
    ;; same code, whichever path (wrapper pre-flight or this source-path assert)
    ;; trips first.
    (exit 1)))

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

;; Evaluate a source string as hafod/scsh code.  Applies the | -> pipe
;; preprocessing pass, reads EVERY form from the string into a list, then
;; evaluates them together as one (begin ...) in the interaction environment.
;; Reading all forms BEFORE evaluating any is load-bearing: a read/parse error
;; aborts before a single form runs, so a malformed source never partially
;; evaluates and then crashes.  Shared by load-script-file and the -c branch so
;; both turn a source string into evaluated forms by exactly the same route.
(define (eval-source-string source)
  (let ([preprocessed (preprocess-pipe-symbols source)])
    (eval (cons 'begin
                (let ([p (open-input-string preprocessed)])
                  (let loop ([forms '()])
                    (let ([form (read p)])
                      (if (eof-object? form)
                          (reverse forms)
                          (loop (cons form forms)))))))
          (interaction-environment))))

;; Report a missing script file the friendly way: name the file on stderr and
;; exit non-zero.  Shared by load-script-file's guard and the bare-filename
;; branch so there is exactly one "No such file" message and one exit status.
(define (report-missing-file fname)
  (display (string-append "hafod: " fname ": No such file\n") (current-error-port))
  (exit 1))

;; Load a script file, stripping !# header if present.
;; Auto-imports (hafod) into the interaction environment before eval.
;; Preprocesses | -> pipe for scsh compatibility.
(define (load-script-file fname)
  (unless (file-exists? fname)
    (report-missing-file fname))
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
      (eval-source-string body))))

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
      "  --batch    Force the non-editor REPL path (also: HAFOD_BATCH=1)\n"
      "  --color[=WHEN]  Control colour output (auto, always, never; bare = always)\n"
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
  ;; hafod-version-string comes from (hafod) -> (hafod system), which derives it
  ;; from the build-time generated (hafod internal version). Single source of truth.
  (display hafod-version-string)
  (display " (scsh on Chez Scheme)\n"))

;; ======================================================================
;; Argument parsing (scsh-compatible)
;; ======================================================================

(define (load-preload-files preloads)
  (for-each load-script-file (reverse preloads)))

;; --color[=WHEN] helpers.  WHEN is one of the GNU-ls set auto/always/never.
;; The external flag and its WHEN tokens stay American (--color); every
;; identifier and comment here is British (colour).
;;   colour-when?       -- is this string a recognised WHEN token?
;;   colour-eq-value    -- the value after a leading "--color=" prefix, else #f.
;;   apply-colour-when! -- map a validated WHEN onto the colour-override
;;                         parameter: auto -> #f (leave the automatic precedence
;;                         in charge), always -> 'always, never -> 'never.  It is
;;                         a plain parameter set, so the last --color wins.
(define (colour-when? s)
  (or (string=? s "auto") (string=? s "always") (string=? s "never")))

(define (colour-eq-value arg)
  (let ([p "--color="])
    (and (>= (string-length arg) (string-length p))
         (string=? (substring arg 0 (string-length p)) p)
         (substring arg (string-length p) (string-length arg)))))

(define (apply-colour-when! when-str)
  (cond
    [(string=? when-str "always") (colour-override 'always)]
    [(string=? when-str "never")  (colour-override 'never)]
    [else                         (colour-override #f)]))   ; "auto"

;; Leave a run that has done its work the way an explicit (exit) leaves it.
;;
;; Every terminator below RETURNS when its work is done.  interactive-repl returns
;; at EOF -- Ctrl-D at the prompt, or a redirected stdin reaching its end -- and
;; that return is the ordinary way out of a shell; a script returns once its last
;; form is evaluated; a -c expression returns once it is evaluated.  Left to itself
;; each of those returns lands back in parse-and-execute, returns from there, and
;; falls off the end of this program: a path that runs neither the exit hooks nor
;; Chez's exit-handler.
;;
;; So every hook the run registered would simply not run.  add-exit-hook! is public
;; API, which makes those a user's hooks as much as hafod's own -- the one their
;; script registered to close a database, drop a lock file or post a report on the
;; way out, and which hafod then quietly never called.  hafod's history is the case
;; that showed it up: the hook it registers when it opens is the close of the SQLite
;; connection, so the database was released when a run typed (exit), and abandoned
;; when a user pressed Ctrl-D or a script reached its end -- leaving the write-ahead
;; log and its shared-memory file stranded beside it, uncheckpointed, holding what
;; had just been written.
;;
;; So exit explicitly: hafod's exit (not Chez's -- (hafod) shadows it) runs the exit
;; hooks, flushes the ports and _exits, which is exactly the route an explicit
;; (exit) already took.  The status is 0, the status each of these paths exited with
;; when it fell off the end, so nothing observable changes but the hooks running.
;;
;; It cannot run them twice.  An explicit (exit N) -- typed at the REPL, written in
;; a script, written in a -c expression -- runs the hooks and _exits from there: the
;; process is gone, this is never reached, and the status stays that (exit N)'s
;; rather than becoming this one's.  Returning is the only other way control leaves
;; a terminator.  Composing Chez's exit-handler instead would not do: it does not
;; fire on the fall-off-the-end path either, and composed as well as this it would
;; run every hook a second time.
;;
;; A raise is not a return, so an uncaught error still unwinds past this to the
;; top-level guard and Chez's default handler, exiting with the status and the
;; diagnostic it always did.
;;
;; interactive-repl and load-script-file are themselves left returning normally,
;; which is what a caller embedding either in a program needs them to do; the
;; launcher owns when the PROCESS ends, and this is the launcher.
(define (exit-normally)
  (exit 0))

(define (repl-and-exit)
  (interactive-repl)
  (exit-normally))

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
            (repl-and-exit))

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

              ;; --batch -- force the non-editor REPL path regardless of whether
              ;; stdin is a tty (non-terminating, like --login)
              [(string=? arg "--batch")
               (batch-mode? #t)
               (loop rest entry preloads login? no-config?)]

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
                     (proc script-args)))
                 ;; The script has run: leave through the exit hooks rather than off
                 ;; the end of the program.  See exit-normally.
                 (exit-normally))]

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
               (eval-source-string (car rest))
               ;; The expression has been evaluated: leave through the exit hooks
               ;; rather than off the end of the program.  See exit-normally.
               (exit-normally)]

              ;; -- explicit REPL (terminating)
              [(string=? arg "--")
               (set-command-line! (cons "hafod" rest))
               (load-preload-files preloads)
               (ensure-hafod-interaction-environment!)
               (unless no-config?
                 (load-config-file (hafod-init-file)))
               (repl-and-exit)]

              ;; --help
              [(string=? arg "--help")
               (show-usage)]

              ;; --version
              [(string=? arg "--version")
               (show-version)]

              ;; --color / --color WHEN -- control SGR colour (auto/always/never;
              ;; bare = always).  Non-terminating: it sets colour-override by a
              ;; plain call (like --batch's batch-mode?), so the verdict persists
              ;; through to whichever terminator (-c/-s/-e/-l/--/the REPL) follows.
              ;; The space form consumes the next arg ONLY when it is a recognised
              ;; WHEN; otherwise this is a bare --color and rest is left intact, so
              ;; a following script/positional/terminator is never swallowed.
              [(string=? arg "--color")
               (if (and (pair? rest) (colour-when? (car rest)))
                   (begin (apply-colour-when! (car rest))
                          (loop (cdr rest) entry preloads login? no-config?))
                   (begin (colour-override 'always)
                          (loop rest entry preloads login? no-config?)))]

              ;; --color=WHEN -- validated; an unrecognised WHEN is rejected with a
              ;; clear message and (exit 1), matching every other launcher bad-arg
              ;; path.  Placed after the plain --color clause (so --color=never
              ;; reaches here) and before the unknown-switch guard (so --color... is
              ;; never mistaken for an unknown switch).
              [(colour-eq-value arg)
               => (lambda (when-str)
                    (if (colour-when? when-str)
                        (begin (apply-colour-when! when-str)
                               (loop rest entry preloads login? no-config?))
                        (begin
                          (display (string-append
                                     "hafod: invalid --color value: " when-str
                                     " (expected auto, always, or never)\n")
                                   (current-error-port))
                          (exit 1))))]

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
               ;; Guard existence BEFORE extract-script-header-args opens the
               ;; file to parse a #!...!# header, so a missing bare filename is
               ;; reported with the same friendly message as -s instead of a raw
               ;; Chez condition.
               (unless (file-exists? arg) (report-missing-file arg))
               (let-values ([(hdr-entry hdr-preloads)
                             (parse-header-flags
                               (extract-script-header-args arg))])
                 (for-each load-script-file hdr-preloads)
                 (load-script-file arg)
                 (let ([effective-entry (or entry hdr-entry)])
                   (when effective-entry
                     (let ([proc (eval effective-entry
                                      (interaction-environment))])
                       (proc rest))))
                 ;; A shebang boots through here, so this is the branch most scripts
                 ;; a user writes actually take -- and it fell off the same end as -s.
                 ;; See exit-normally.
                 (exit-normally))]))))))

;; ======================================================================
;; Main
;; ======================================================================

;; Detect login shell invocation: argv[0] starts with "-" (e.g., "-hafod").
;; This is the standard Unix convention used by login(1) and getty(8).
;;
;; Wrap the whole dispatch in a top-level cooked-mode boundary.  An unhandled
;; exception raised outside the REPL's own dynamic-wind -- during start-up, or
;; while the line editor is mid-read -- would otherwise reach Chez's default
;; handler, which prints the diagnostic and exits WITHOUT running the exit
;; restore, stranding the outer shell with no echo.  Re-assert cooked for fd 0
;; first, then re-raise the original condition unchanged so the diagnostic and
;; exit status are exactly as before.  The re-assert has its own guard so a
;; restore failure can never mask the original error, and it is a no-op off a
;; terminal (a piped or redirected run), so this is inert unless stdin is a
;; real tty.  The success path and the (exit)-based -c/--help/--version paths
;; are untouched -- a clean return never enters the handler.
;; Let a broken downstream pipe terminate hafod the way a normal Unix filter
;; ends -- quietly.  Chez sets SIGPIPE to SIG_IGN so a write to a closed pipe
;; returns EPIPE and surfaces as an i/o exception (from the REPL, an eval's own
;; output, or a flush); restoring the default disposition makes the kernel end
;; the process at the write syscall instead, with no diagnostic.  Fix (a) keeps
;; the line editor off whenever stdout is not a terminal, so a broken pipe can
;; only occur when stdin/stdout is redirected -- i.e. never in raw mode -- so
;; this quiet termination never strands a raw terminal.
(reset-signal-to-default! SIGPIPE)

(guard (con [#t
             (guard (e [#t (void)])
               (reassert-cooked-tty! 0))
             (raise con)])
  (let ([argv0 (car (chez:command-line))]
        [args (command-line-arguments)])
    (if (and (> (string-length argv0) 0)
             (char=? (string-ref argv0 0) #\-))
        (parse-and-execute (cons "--login" args))
        (parse-and-execute args))))
