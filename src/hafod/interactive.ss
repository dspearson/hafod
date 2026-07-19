;;; (hafod interactive) -- Interactive REPL loop for hafod
;;; Provides configurable read-eval-print loop with prompt and eval hooks.
;;; Handles SIGINT (Ctrl-C) to interrupt evaluation and SIGWINCH for terminal width.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod interactive)
  (export
    interactive-repl
    ;; interactive-enhancements? -- master gate for the REPL-entry default-on
    ;; affordances (visit recording, the default alias set).  init.ss sets it #f
    ;; for a bare interactive surface; re-exported through the (hafod) umbrella.
    interactive-enhancements?
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
    ;; The single resize step and its handler installer.  Leaf-only exports --
    ;; deliberately NOT re-exported from the (hafod) umbrella; a test reaches
    ;; them via (only (hafod interactive) ...) so it can drive the REPL's OWN
    ;; resize path rather than a copy of it.
    refresh-terminal-width!
    install-resize-handler!
    repl-continuation-prompt
    ansi-visible-length
    background-job-count
    ;; prompt-path-segment -- pure home-relative + fish-truncated path renderer.
    ;; User-facing: the (hafod) umbrella re-exports it so init.ss config can
    ;; compose a working-directory prompt segment.
    prompt-path-segment
    ;; prompt-git-segment -- single-spawn, coloured, sanitised, fail-quiet git
    ;; state renderer; prompt-colour-ok? -- the overridable colour verdict thunk
    ;; (default (colour-ok? 1)) that every coloured prompt segment gates on.
    ;; Both user-facing (a later plan re-exports them from the (hafod) umbrella).
    prompt-git-segment
    prompt-colour-ok?
    ;; prompt-exit-segment -- the red "✘N" exit-code badge for the right prompt,
    ;; shown only after a non-zero last command.  User-facing: a later plan
    ;; re-exports it from the (hafod) umbrella.
    prompt-exit-segment
    ;; prompt-timing-segment -- a format-duration readout for the right prompt,
    ;; shown only when the last command ran at least prompt-timing-threshold (a
    ;; parameter defaulting to 2000 ms).  Both user-facing: a later plan re-exports
    ;; them from the (hafod) umbrella.
    prompt-timing-segment
    prompt-timing-threshold
    ;; enable-informative-prompt! -- the one-call preset that composes the four
    ;; prompt segments into the existing left/right hooks, configured by a
    ;; quoted-symbol option plist with sensible defaults and still overridable
    ;; segment by segment.  User-facing: a later plan re-exports it from the
    ;; (hafod) umbrella and the config surface, beside set-prompt!.
    enable-informative-prompt!
    ;; sanitize-control -- pure C0/ESC/DEL/CR/LF stripper for prompt-derived
    ;; text.  Leaf-only export (white-box test surface): a test reaches it via
    ;; (only (hafod interactive) ...) to prove the prompt's control-byte defence
    ;; directly.  Deliberately NOT re-exported from the (hafod) umbrella, the
    ;; same convention as refresh-terminal-width! above.
    sanitize-control
    ;; parse-git-porcelain-v2 -- pure `git status --porcelain=v2 --branch` line
    ;; parser.  Leaf-only export (white-box test surface): a test reaches it via
    ;; (only (hafod interactive) ...) to prove the field extraction directly, the
    ;; same convention as sanitize-control above.  NOT re-exported from the
    ;; (hafod) umbrella.
    parse-git-porcelain-v2
    ;; Shell mode re-exports (for config access)
    rebuild-path-cache!
    classify-input
    ;; Feature toggles
    shell-mode?
    history-expansion?
    batch-mode?
    use-editor?*
    ;; auto-cd? -- when #t (default), a bare shell-mode line naming an existing
    ;; directory changes into it.  User-facing: a later plan re-exports it from
    ;; the (hafod) umbrella and the config surface so init.ss can switch it off.
    auto-cd?
    ;; auto-cd-target / auto-cd-decision / run-auto-cd! -- the leaf pieces of the
    ;; auto-cd decision: the once-per-submit directory probe, the mode+toggle
    ;; composite, and the literal-path change.  Leaf-only exports (white-box test
    ;; surface): a test reaches them via (only (hafod interactive) ...) to drive
    ;; the REPL's own auto-cd decision rather than a copy of it, the same
    ;; convention as sanitize-control above.  NOT re-exported from the umbrella.
    auto-cd-target
    auto-cd-decision
    run-auto-cd!)

  (import (except (chezscheme) getenv)
          (only (hafod posix) status:exit-val SIGWINCH posix-waitpid)
          (only (hafod signal) set-signal-handler!)
          (only (hafod environment) getenv setenv)
          (only (hafod procobj) background-job-count)
          (only (hafod editor editor) read-expression with-raw-mode
                editor-history-entries editor-history-set-last-mode!)
          (only (hafod tty) tty? terminal-size refresh-terminal-size-cache!
                cached-terminal-size
                reassert-cooked-tty! install-terminal-guard!)
          (only (hafod terminal-caps) ansi-ok? colour-ok?)
          (only (hafod editor render) tokenize display-colourised)
          ;; Pure prompt-path helpers: logical "."/".."/"//" cleanup, $HOME and
          ;; the current directory (the latter two feed the impure prompt caller
          ;; and are held in scope here), and the wcwidth display-width oracle so
          ;; the path budget counts rendered columns rather than characters.
          (only (hafod fname) simplify-file-name)
          (only (hafod user-group) home-directory)
          (only (hafod process-state) cwd)
          (only (hafod editor input-decode) string-display-width)
          (only (hafod shell classifier) classify-input rebuild-path-cache! path-cache
                command-not-found-suppress? command-not-found-suggestions alias-expand-line)
          (only (hafod shell parser) parse-shell-command parse-command-words)
          (only (hafod shell builtins) run-builtin! builtin-names dir-stack builtin-cd-path)
          ;; visit-recording? gates the directory visit store; interactive-repl
          ;; turns it on so an interactive session records visits, while a
          ;; -c/-s/batch run leaves it off and never opens the database.
          (only (hafod shell visit-db) visit-recording?)
          (only (hafod shell default-aliases) default-aliases? install-default-aliases!)
          (only (hafod shell jobs) install-job-signals! update-jobs! drain-notifications!)
          (only (hafod shell history-expand) history-expand))

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

  ;; Toggle for auto-cd.
  ;; When #t (default), a bare shell-mode line that names an existing directory
  ;; -- and resolves to no command, builtin, alias, or bound identifier -- changes
  ;; into that directory instead of being evaluated.  Set to #f to switch it off.
  (define auto-cd?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; auto-cd-target -- the sole filesystem probe on the auto-cd path, run once per
  ;; submitted line (never on the per-keystroke classify path).  Returns the
  ;; expanded directory word when LINE is a single bare shell token naming an
  ;; existing directory that resolves to nothing else, or #f otherwise.  The
  ;; single-word gate -- parse-command-words yielding exactly one word -- rejects
  ;; any line carrying a shell operator or redirect, and command-not-found-suppress?
  ;; declines the moment the token is a builtin, keyword, literal, alias, PATH
  ;; command, or a bound Scheme identifier, so a real command or a bound variable
  ;; always wins over a same-named directory.
  (define (auto-cd-target line)
    (let ([words (parse-command-words line)])
      (and (pair? words)
           (null? (cdr words))
           (let ([word (car words)])
             (and (not (command-not-found-suppress? word))
                  (file-directory? word)
                  word)))))

  ;; auto-cd-decision -- compose the mode gate, the toggle, and the target so the
  ;; dispatch and the tests share one predicate.  Returns the directory to enter,
  ;; or #f when shell mode is off, auto-cd is off, or the line is not a bare
  ;; existing-directory token.
  (define (auto-cd-decision line)
    (and (shell-mode?) (auto-cd?) (auto-cd-target line)))

  ;; run-auto-cd! -- enter DIR through the injection-safe literal-cd seam.  DIR
  ;; reaches chdir verbatim (never a rebuilt "cd " command line), so a directory
  ;; whose name holds shell metacharacters is entered with no side effect.
  ;; Return (void) so the REPL loops without evaluating anything.  The history
  ;; entry is tagged 'shell once by the dispatch (which computes the auto-cd
  ;; decision ahead of the case), so this no longer re-tags it.
  (define (run-auto-cd! dir)
    (builtin-cd-path dir)
    (void))

  ;; Toggle for history expansion (!!, !$, !n, !-n, !prefix).
  ;; When #t (default), history expansion is performed before evaluation.
  ;; Set to #f to disable (e.g. if ! in identifiers causes problems).
  (define history-expansion?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; Force the non-editor (bare read) input path regardless of whether stdin is
  ;; a terminal.  Set by the launcher's --batch flag and also honoured via the
  ;; HAFOD_BATCH environment variable, so the deterministic read path can be a
  ;; deliberate choice rather than a side effect of redirecting stdin.
  (define batch-mode?
    (make-parameter #f (lambda (v) (and v #t))))

  ;; Choose the line editor only when BOTH ends are a real terminal and batch
  ;; mode has not been forced.  The full-screen editor reads keys in raw mode
  ;; from fd 0 and renders with cursor control to fd 1, so it needs a terminal
  ;; at each end; when stdout is a pipe or file (`hafod | cat`, `hafod > log`)
  ;; the bare line-read path is used instead, giving clean output with no
  ;; per-keystroke re-render.  Pure in its four inputs -- the two live tty
  ;; states, the batch-mode? value, and a HAFOD_BATCH boolean -- so the
  ;; precedence (both terminals AND NOT batch AND NOT env) is unit-testable
  ;; without a pseudo-terminal.
  (define (use-editor?* tty-at-fd0? tty-at-fd1? forced-batch? env-batch?)
    (and tty-at-fd0? tty-at-fd1? (not forced-batch?) (not env-batch?)))

  ;; === Terminal width ===

  ;; Terminal width parameter, updated on SIGWINCH.
  ;; Defaults to 80 columns.
  (define terminal-width
    (make-parameter 80
      (lambda (v)
        (unless (and (integer? v) (exact? v) (> v 0))
          (error 'terminal-width "expected positive exact integer" v))
        v)))

  ;; Query the terminal column count, falling back to 80 off a terminal.
  ;; A LIVE query, on every call: this is public API, and what it owes its caller
  ;; is the width of the terminal NOW, not the width of the terminal once.
  ;;
  ;; Deliberately NOT answered from the width cache below.  A script may call this
  ;; far from any REPL -- `hafod -s`, `hafod -c` and a shebang script all load
  ;; their source and exit without ever entering interactive-repl -- and in such a
  ;; process nothing installs the SIGWINCH handler and nothing takes an editor
  ;; entry, which are the only things that ever refresh that cache.  A cached
  ;; answer would therefore have no refresher at all on that path: the caller
  ;; would be frozen at the first width it ever saw, for the life of the process,
  ;; while the user resized the window around it.
  ;;
  ;; The cache is for the per-render hot path, which refreshes it on every resize
  ;; and can therefore trust it.  A caller who cannot make that promise asks the
  ;; kernel: one ioctl is the price of an answer that is still true.
  (define (query-terminal-width)
    (let-values ([(rows cols) (terminal-size)]) cols))

  ;; Query the terminal size as (values rows cols), falling back to 24x80
  ;; off a terminal. Delegates to the shared tty helper.
  (define (query-terminal-size)
    (terminal-size))

  ;; The single resize step, shared by REPL entry and the SIGWINCH handler.
  ;; ONE live query fills the cache, and the width parameter is then republished
  ;; from the cache THAT LINE HAS JUST FILLED -- so a resize asks the kernel once
  ;; and both consumers, the cache and the parameter, are fed from that single
  ;; answer instead of each paying for a query of its own.
  ;;
  ;; Republishing through query-terminal-width would ask a SECOND time, about a
  ;; window this procedure has just measured -- and the two answers need not even
  ;; agree, since a resize can land between them.  The answer is already in hand;
  ;; read it back.
  ;;
  ;; The order is load-bearing.  Swap these two lines and the width is read
  ;; before the refresh runs, so the parameter is republished with the PREVIOUS
  ;; size -- and stays wrong on every render until the next resize.  It would
  ;; fail silently, which is why the two lines are adjacent and why a test plants
  ;; a distinct pre-resize width and cache to catch exactly that inversion.
  (define (refresh-terminal-width!)
    (refresh-terminal-size-cache!)
    (let-values ([(rows cols) (cached-terminal-size)])
      (terminal-width cols)))

  ;; Install the SIGWINCH handler that reruns the resize step, returning the
  ;; disposition that was live before it (set-signal-handler! already yields it).
  ;; Recording it through the disposition registry keeps this handler recoverable
  ;; as the prior one, so a scoped swap (such as the fuzzy finder's own resize
  ;; handling) can restore it on exit instead of clobbering it.
  (define (install-resize-handler!)
    (set-signal-handler! SIGWINCH (lambda (sig) (refresh-terminal-width!))))

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
        ;; Pass cursor-pos = -1 to skip enclosing-paren highlighting.
        ;; Colour only when the target port is a live terminal (colour-ok?):
        ;; a piped or captured stream (string port) yields plain, escape-free text.
        (display-colourised port str tokens -1 (colour-ok? port))
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

  ;; Strip terminal-hostile bytes from a prompt-derived string.  A directory or
  ;; repository name may legally carry ESC, other C0 controls, CR, LF or DEL, and
  ;; those bytes -- emitted verbatim -- would let a hostile name repaint the
  ;; line, move the cursor or otherwise spoof the prompt.  Every path- and
  ;; repository-derived string therefore passes through here before display: the
  ;; result keeps every character of code >= space except DEL (#x7f) and the C1
  ;; control range (#x80-#x9f), and drops ESC (#x1b) together with all other C0
  ;; controls below the space code (so CR, LF and TAB are removed too).  The C1
  ;; range is dropped as well because a UTF-8 terminal can read those code points
  ;; as an eight-bit CSI (#x9b) or OSC (#x9d) introducer -- a second escape route
  ;; a repository or directory name must not reach.  A single left-to-right walk
  ;; mirrors ansi-visible-length above; no regex.
  (define (sanitize-control str)
    (let ([len (string-length str)]
          [out (open-output-string)])
      (let loop ([i 0])
        (if (>= i len)
            (get-output-string out)
            (let* ([ch (string-ref str i)]
                   [c (char->integer ch)])
              (when (and (>= c #x20)
                         (not (= c #x7f))
                         (not (and (>= c #x80) (<= c #x9f))))
                (write-char ch out))
              (loop (+ i 1)))))))

  ;; === Prompt path segment (pure) ===

  ;; Split a string on "/", preserving empty segments so a leading "/" yields a
  ;; leading empty segment (and thus a preserved leading slash on rejoin).
  (define (split-on-slash str)
    (let ([len (string-length str)])
      (let loop ([i 0] [start 0] [acc '()])
        (cond
          [(>= i len) (reverse (cons (substring str start len) acc))]
          [(char=? (string-ref str i) #\/)
           (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]
          [else (loop (+ i 1) start acc)]))))

  ;; Rejoin path segments with "/".
  (define (join-on-slash segs)
    (if (null? segs)
        ""
        (let loop ([rest (cdr segs)] [acc (car segs)])
          (if (null? rest)
              acc
              (loop (cdr rest) (string-append acc "/" (car rest)))))))

  ;; Abbreviate one leading path segment to its first character, with two
  ;; exceptions: a hidden segment (leading ".") keeps its dot and the following
  ;; character (".config" -> ".c"), and "~" passes through unchanged.  An empty
  ;; segment (the leading-slash placeholder) is returned untouched.  The "first
  ;; character" is a whole Scheme char, so a wide/multibyte name abbreviates to
  ;; one glyph, not one byte.
  (define (abbreviate-segment seg)
    (cond
      [(string=? seg "~") "~"]
      [(= (string-length seg) 0) seg]
      [(char=? (string-ref seg 0) #\.)
       (if (>= (string-length seg) 2) (substring seg 0 2) seg)]
      [else (substring seg 0 1)]))

  ;; Fish-style truncation of an already home-relative, simplified path: keep the
  ;; last non-empty segment in full and abbreviate every earlier non-empty
  ;; segment.  A path of only slashes (no non-empty segment) is returned as given.
  (define (fish-truncate-path str)
    (let* ([segs (split-on-slash str)]
           [last-idx (let loop ([i (- (length segs) 1)])
                       (cond
                         [(< i 0) #f]
                         [(> (string-length (list-ref segs i)) 0) i]
                         [else (loop (- i 1))]))])
      (if (not last-idx)
          str
          (join-on-slash
            (let loop ([rest segs] [i 0] [acc '()])
              (cond
                [(null? rest) (reverse acc)]
                [(= i last-idx) (loop (cdr rest) (+ i 1) (cons (car rest) acc))]
                [else (loop (cdr rest) (+ i 1)
                            (cons (abbreviate-segment (car rest)) acc))]))))))

  ;; Render an absolute path home-relative and, past a display-width budget,
  ;; fish-truncated -- purely, touching neither the filesystem nor git.  The
  ;; caller passes the path string, the home string and the budget; nothing here
  ;; reads $PWD, resolves a symlink or spawns a process, so the whole renderer is
  ;; a table-driven unit under test.  Steps, in order:
  ;;   1. Strip control bytes from the input (a directory name may carry
  ;;      ESC/CR/LF/C0/DEL -- the boundary defence).
  ;;   2. Home-relative: an exact match of home renders "~"; a path under home
  ;;      (home followed by "/") has that prefix replaced with "~".  The boundary
  ;;      is exact -- /home/user2 does NOT collapse to ~2 when home is
  ;;      /home/user, because the character after the home prefix must be "/".
  ;;      simplify-file-name then folds logical "."/".."/"//" without ever
  ;;      resolving a symlink.
  ;;   3. Within budget -- measured by DISPLAY width, so a wide CJK/emoji name
  ;;      counts its rendered columns, not its character count -- the path is
  ;;      returned untruncated.  The budget triggers truncation; it is not a hard
  ;;      cap.
  ;;   4. Otherwise fish-truncate.  Root "/" always renders "/".
  (define (prompt-path-segment path home budget)
    (let* ([clean (sanitize-control path)]
           [home-len (string-length home)]
           [rel (cond
                  ;; An empty home (HOME unset or "") has no prefix to collapse --
                  ;; without this guard every absolute path would match the "" prefix
                  ;; and render as "~/...".
                  [(= home-len 0) clean]
                  [(string=? clean home) "~"]
                  [(and (> (string-length clean) home-len)
                        (string=? (substring clean 0 home-len) home)
                        (char=? (string-ref clean home-len) #\/))
                   (string-append "~" (substring clean home-len (string-length clean)))]
                  [else clean])]
           [simplified (simplify-file-name rel)])
      (cond
        [(string=? simplified "/") "/"]
        [(<= (string-display-width simplified) budget) simplified]
        [else (fish-truncate-path simplified)])))

  ;; === Prompt git segment ===

  ;; Return the remainder of LINE after PREFIX when LINE begins with PREFIX, else
  ;; #f -- how the value is plucked from a porcelain-v2 "# branch.<field> <value>"
  ;; header line.
  (define (git-header-value line prefix)
    (let ([plen (string-length prefix)]
          [llen (string-length line)])
      (and (>= llen plen)
           (string=? (substring line 0 plen) prefix)
           (substring line plen llen))))

  ;; Does LINE begin with the kind byte C followed by a space?  Porcelain-v2 tags
  ;; each entry with a leading kind byte -- "1"/"2" for a changed entry, "u" for
  ;; an unmerged one, "?" for an untracked path -- then a space.
  (define (git-line-kind? line c)
    (and (>= (string-length line) 2)
         (char=? (string-ref line 0) c)
         (char=? (string-ref line 1) #\space)))

  ;; Magnitude of a signed ahead/behind token: "+1" and "-1" both yield 1.  Git
  ;; always prints the sign, so the sign carries no information the +/- position
  ;; does not; only the count matters.
  (define (ab-magnitude tok)
    (let ([n (string->number tok)])
      (if (and n (integer? n)) (abs n) 0)))

  ;; Parse the "+N -M" body of a "# branch.ab" line into two non-negative counts.
  ;; A malformed body (no separating space) degrades to 0/0 rather than raising.
  (define (parse-ahead-behind body)
    (let* ([len (string-length body)]
           [sp (let loop ([i 0])
                 (cond
                   [(>= i len) #f]
                   [(char=? (string-ref body i) #\space) i]
                   [else (loop (+ i 1))]))])
      (if (not sp)
          (values 0 0)
          (values (ab-magnitude (substring body 0 sp))
                  (ab-magnitude (substring body (+ sp 1) len))))))

  ;; Pure parser over the lines of `git status --porcelain=v2 --branch`.  Returns
  ;; six values and performs no I/O or rendering -- it only reads the line list:
  ;;   head    text after "# branch.head " (may be the literal "(detached)"); #f
  ;;           when no such line is present, which an empty list -- not a
  ;;           repository -- yields.
  ;;   oid     text after "# branch.oid " (may be the literal "(initial)" on an
  ;;           unborn branch); #f when absent.
  ;;   ahead   the +N magnitude of "# branch.ab +N -M"; 0 when the line is absent.
  ;;   behind  the -M magnitude; 0 when absent.  A branch with no upstream carries
  ;;           no branch.ab line at all, so both default to 0 rather than erroring.
  ;;   staged? any "1"/"2" entry whose staged (X) column is not ".", or any "u"
  ;;           unmerged entry.
  ;;   dirty?  any "1"/"2" entry whose worktree (Y) column is not ".", any "u"
  ;;           unmerged entry, or any "?" untracked entry.  Untracked-as-dirty is
  ;;           a deliberate policy: an untracked-only tree is not clean -- it
  ;;           renders yellow with a "*" marker -- which keeps the marker set to
  ;;           just "*"/"+" with no third glyph.
  (define (parse-git-porcelain-v2 lines)
    (let loop ([ls lines] [head #f] [oid #f] [ahead 0] [behind 0]
               [staged? #f] [dirty? #f])
      (if (null? ls)
          (values head oid ahead behind staged? dirty?)
          (let ([line (car ls)] [rest (cdr ls)])
            (cond
              [(git-header-value line "# branch.head ")
               => (lambda (v) (loop rest v oid ahead behind staged? dirty?))]
              [(git-header-value line "# branch.oid ")
               => (lambda (v) (loop rest head v ahead behind staged? dirty?))]
              [(git-header-value line "# branch.ab ")
               => (lambda (v)
                    (let-values ([(a b) (parse-ahead-behind v)])
                      (loop rest head oid a b staged? dirty?)))]
              [(or (git-line-kind? line #\1) (git-line-kind? line #\2))
               (if (>= (string-length line) 4)
                   (loop rest head oid ahead behind
                         (or staged? (not (char=? (string-ref line 2) #\.)))
                         (or dirty?  (not (char=? (string-ref line 3) #\.))))
                   (loop rest head oid ahead behind staged? dirty?))]
              [(git-line-kind? line #\u)
               (loop rest head oid ahead behind #t #t)]
              [(git-line-kind? line #\?)
               (loop rest head oid ahead behind staged? #t)]
              [else
               (loop rest head oid ahead behind staged? dirty?)])))))

  ;; Single-spawn collector for the git segment: run `git status --porcelain=v2
  ;; --branch` and return its stdout lines, closing both child pipes and
  ;; swallowing any error.  Kept private and copied from the completion helper's
  ;; one-shot pattern rather than shared, so the prompt owns its own git probe.
  ;; Three properties are load-bearing.  The command is a fixed literal -- no
  ;; repository- or user-derived token is concatenated into it, so there is
  ;; nothing to inject through.  The stderr pipe is closed unread, so git's
  ;; "fatal: not a git repository" outside a working tree is discarded rather than
  ;; scrolling above the prompt.  And the child is reaped before returning, on
  ;; every path, so it does not linger as a zombie until the next collection: the
  ;; segment runs on every prompt draw, and an unreaped child here would let the
  ;; process table fill across a long session.  The blocking wait returns at once
  ;; because stdout has already reached EOF, so git has finished; a missing child
  ;; (already reaped by a collector guardian) surfaces as an error and is ignored.
  ;; A spawn failure (git absent) is caught and read as no lines, i.e. an empty
  ;; segment.
  (define (git-status-porcelain-v2)
    (guard (e [#t '()])
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports
                      "git status --porcelain=v2 --branch"
                      (buffer-mode block)
                      (make-transcoder (utf-8-codec)))])
        (close-port to-stdin)
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (let loop ([acc '()])
              (let ([line (get-line from-stdout)])
                (if (eof-object? line)
                    (reverse acc)
                    (loop (cons line acc))))))
          (lambda ()
            (close-port from-stdout)
            (close-port from-stderr)
            (guard (e [#t (void)])
              (let-values ([(w s) (posix-waitpid pid 0)]) (void))))))))

  ;; The colour verdict for coloured prompt segments, held as an overridable
  ;; thunk.  It MUST gate on the real terminal, never on the current output port:
  ;; while the left-prompt hook runs, both the console and current output ports
  ;; are rebound to a string capture port (the editor reads the captured prompt
  ;; back), so a port-based verdict would read "not a terminal" and every segment
  ;; would be plain in real use.  The default asks whether fd 1 -- the descriptor
  ;; the editor renders to -- may carry colour.  A user (or a test) may rebind it
  ;; to force the decision; a test uses this as the deterministic, PTY-free seam
  ;; for the fd-1 gate.  The validator requires a procedure, mirroring the prompt
  ;; hooks above.
  (define prompt-colour-ok?
    (make-parameter
      (lambda () (colour-ok? 1))
      (lambda (v)
        (unless (procedure? v)
          (error 'prompt-colour-ok? "expected a procedure" v))
        v)))

  ;; Build the git-state segment from one `git status --porcelain=v2 --branch`
  ;; spawn: the branch name (or "@" + a 7-char short oid when detached), "*" when
  ;; the working tree is dirty and "+" when it has staged changes, then "⇡N"/"⇣M"
  ;; ahead/behind counts.  Empty outside a repository or when git is absent -- a
  ;; quiet, escape-free "".  The branch head is stripped of control bytes before
  ;; display, since a repository name is attacker-controlled, and the detached oid
  ;; is sliced to seven hex characters.  Colour is 256-colour SGR -- green
  ;; (index 2) when clean, yellow (index 3) when dirty or staged -- emitted only
  ;; when the injectable colour verdict is true; a false verdict yields the same
  ;; glyphs as plain text.  The up/down arrows are written as hex escapes so the
  ;; source is unambiguous about the exact code points (U+21E1, U+21E3), both
  ;; display-width 1.
  (define (prompt-git-segment)
    (let ([lines (git-status-porcelain-v2)])
      (if (null? lines)
          ""
          (let-values ([(head oid ahead behind staged? dirty?)
                        (parse-git-porcelain-v2 lines)])
            (if (not head)
                ""
                (let* ([name (if (string=? head "(detached)")
                                 (string-append
                                   "@" (if (string? oid)
                                           (substring oid 0 (min 7 (string-length oid)))
                                           ""))
                                 (sanitize-control head))]
                       [markers (string-append (if dirty? "*" "") (if staged? "+" ""))]
                       [ab (string-append
                             (if (> ahead 0)
                                 (string-append "\x21e1;" (number->string ahead)) "")
                             (if (> behind 0)
                                 (string-append "\x21e3;" (number->string behind)) ""))]
                       [body (string-append
                               name
                               (if (string=? markers "") "" (string-append " " markers))
                               (if (string=? ab "") "" (string-append " " ab)))]
                       [clean? (and (not dirty?) (not staged?))])
                  (if ((prompt-colour-ok?))
                      (string-append (if clean? "\x1b;[38;5;2m" "\x1b;[38;5;3m")
                                     body
                                     "\x1b;[39m")
                      body)))))))

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
            ;; Save the cursor, jump to the column, then restore -- only when the
            ;; target is a live terminal. Otherwise emit the right-prompt text
            ;; plain so a piped or captured stream carries no cursor escapes.
            (let ([ansi? (ansi-ok? port)])
              (when ansi?
                (display "\x1b;7" port)
                (display (format "\x1b;[~aG" col) port))
              (display str port)
              (when ansi?
                (display "\x1b;8" port))))))))

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
            ;; Grey the duration only when stderr is a live terminal; the plain
            ;; duration text always survives to a piped or captured stream.
            (let ([colour? (colour-ok? (console-error-port))])
              (when colour? (display "\x1b;[38;5;240m" (console-error-port)))
              (display str (console-error-port))
              (when colour? (display "\x1b;[39m" (console-error-port)))
              (newline (console-error-port))))))))

  ;; === Prompt exit and timing segments (right prompt) ===

  ;; Build the exit-code segment for the right prompt: the empty string after a
  ;; successful last command -- success is silent -- and "✘N", N being the
  ;; decimal last status, after a failure.  The badge is red 256-colour SGR
  ;; (index 1) when the injectable colour verdict is true and the same glyphs
  ;; plain when it is false, exactly the discipline the git segment follows:
  ;; colour gates on prompt-colour-ok? (fd 1 by default), never on the string
  ;; capture port the prompt is drawn into.  The cross U+2718 is written as a hex
  ;; escape so the source is unambiguous about the code point; it is
  ;; display-width 1.
  (define (prompt-exit-segment)
    (let ([s (last-status)])
      (if (zero? s)
          ""
          (let ([body (string-append "\x2718;" (number->string s))])
            (if ((prompt-colour-ok?))
                (string-append "\x1b;[38;5;1m" body "\x1b;[39m")
                body)))))

  ;; The command-timing display threshold in milliseconds, an overridable
  ;; parameter so a user may raise or lower the point at which the right prompt
  ;; begins to show a duration.  Default 2000 ms, matching display-command-timing!'s
  ;; own fixed gate.  Validated as a non-negative exact integer, mirroring
  ;; last-duration above.
  (define prompt-timing-threshold
    (make-parameter 2000
      (lambda (v)
        (unless (and (integer? v) (exact? v) (>= v 0))
          (error 'prompt-timing-threshold "expected a non-negative exact integer" v))
        v)))

  ;; Build the command-timing segment for the right prompt.  It is the empty
  ;; string unless the last command ran at least the threshold in milliseconds AND
  ;; format-duration yields a string; otherwise it is that formatted duration.  The
  ;; two gates are independent: the threshold is the configurable point at which
  ;; timing begins to show, while format-duration returns #f below one second
  ;; regardless -- so a threshold under 1000 still shows nothing until the duration
  ;; crosses a second.  With no argument the live prompt-timing-threshold is read;
  ;; a threshold may also be passed explicitly.  Coloured grey (256-colour
  ;; index 240, matching display-command-timing!) when the injectable colour
  ;; verdict is true and plain when it is false.
  (define prompt-timing-segment
    (case-lambda
      [() (prompt-timing-segment (prompt-timing-threshold))]
      [(threshold)
       (let ([dur (last-duration)])
         (if (>= dur threshold)
             (let ([str (format-duration dur)])
               (if str
                   (if ((prompt-colour-ok?))
                       (string-append "\x1b;[38;5;240m" str "\x1b;[39m")
                       str)
                   ""))
             ""))]))

  ;; === Informative prompt preset ===

  ;; Join the non-empty strings of PARTS with a single space, dropping the empty
  ;; ones, so a hidden right-prompt segment leaves no stray separator.  Only two
  ;; parts arise here (the exit badge and the timing readout), but the fold is
  ;; written for any count.
  (define (join-nonempty-space parts)
    (let loop ([ps parts] [acc ""])
      (cond
        [(null? ps) acc]
        [(string=? (car ps) "") (loop (cdr ps) acc)]
        [(string=? acc "") (loop (cdr ps) (car ps))]
        [else (loop (cdr ps) (string-append acc " " (car ps)))])))

  ;; The current directory as the shell sees it: the logical $PWD when it is set
  ;; and non-empty, otherwise the real working directory from (cwd).  Reading $PWD
  ;; keeps a path through a symbolic link exactly as the user reached it -- the
  ;; logical PWD semantics every shell follows -- rather than resolving the link;
  ;; the (cwd) fallback covers a process started with no PWD in its environment.
  ;; This impure read is kept OUT of the pure prompt-path-segment, which is handed
  ;; the directory string and so stays filesystem-free and table-testable.
  (define (prompt-logical-cwd)
    (let ([p (getenv "PWD")])
      (if (and p (> (string-length p) 0)) p (cwd))))

  ;; Install the informative prompt in a single call: compose the path and git
  ;; segments into the left prompt and the exit-code and command-timing segments
  ;; into the right prompt, by SETTING the two existing hook parameters.  It adds
  ;; no dispatch mechanism and no theme language of its own -- it only wires the
  ;; segment procedures already defined above into repl-prompt-hook and
  ;; repl-right-prompt-hook.
  ;;
  ;; OPTS is a plist of QUOTED SYMBOLS (key value key value ...), not reader
  ;; keywords.  Chez has no keyword objects, so a written "#:git" would read as a
  ;; fresh uninterned symbol that could never match a key and would be silently
  ;; ignored; the keys are therefore plain symbols -- (enable-informative-prompt!
  ;; 'git #f 'path-width 60).  Each key has a sensible default, so a bare
  ;; (enable-informative-prompt!) installs a working prompt.  Recognised keys:
  ;;   git                 boolean, default #t  -- include the git-state segment
  ;;   path-width          integer, default 40  -- the path truncation budget
  ;;   timing-threshold-ms integer, default 2000 -- the timing display threshold
  ;;   exit-code           boolean, default #t  -- include the exit-code badge
  ;;   timing              boolean, default #t  -- include the command-timing readout
  ;; An odd-length option list or an unknown key raises a clear error rather than
  ;; silently mis-configuring the prompt.
  ;;
  ;; The preset stays overridable segment by segment precisely because it composes
  ;; the existing parameters rather than freezing a rendering: rebinding
  ;; repl-prompt-hook or repl-right-prompt-hook wholesale after this call replaces
  ;; the whole line on the next draw, and the timing threshold is published through
  ;; prompt-timing-threshold, so raising or lowering it later still takes effect.
  ;; Nothing here captures a hook or a segment by value.
  (define (enable-informative-prompt! . opts)
    (let loop ([o opts] [git? #t] [path-width 40] [timing-ms 2000]
               [exit? #t] [timing? #t])
      (cond
        [(null? o)
         ;; Publish the timing threshold so the right hook -- and any later bare
         ;; (prompt-timing-segment) -- reads it, and so an invalid value is
         ;; rejected here rather than at the first prompt draw.
         (prompt-timing-threshold timing-ms)
         (repl-prompt-hook
           (lambda ()
             (let ([path (prompt-path-segment
                           (prompt-logical-cwd) (home-directory) path-width)]
                   [git (if git? (prompt-git-segment) "")])
               (display path (current-output-port))
               (unless (string=? git "")
                 (display " " (current-output-port))
                 (display git (current-output-port)))
               ;; A trailing space separates the prompt from the typed input.
               (display " " (current-output-port)))))
         (repl-right-prompt-hook
           (lambda ()
             (display
               (join-nonempty-space
                 (list (if exit? (prompt-exit-segment) "")
                       (if timing? (prompt-timing-segment) "")))
               (current-output-port))))]
        [(null? (cdr o))
         (error 'enable-informative-prompt! "odd option list" opts)]
        [else
         (let ([key (car o)] [val (cadr o)] [rest (cddr o)])
           (case key
             [(git)                 (loop rest val path-width timing-ms exit? timing?)]
             [(path-width)
              ;; Validate here, at configuration time, rather than letting a bad
              ;; width raise inside the prompt hook at draw time -- that raise would
              ;; land outside the REPL's eval guard and disrupt the read loop.
              (unless (and (integer? val) (exact? val) (positive? val))
                (error 'enable-informative-prompt!
                       "path-width must be a positive exact integer" val))
              (loop rest git? val timing-ms exit? timing?)]
             [(timing-threshold-ms) (loop rest git? path-width val exit? timing?)]
             [(exit-code)           (loop rest git? path-width timing-ms val timing?)]
             [(timing)              (loop rest git? path-width timing-ms exit? val)]
             [else (error 'enable-informative-prompt! "unknown option" key)]))])))

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

  ;; Report a genuinely-unknown command, with up to three fuzzy suggestions.
  ;; The classifier owns the decision: a bound/legal Scheme identifier (car,
  ;; list, a user define), a keyword, a literal, or a known command is
  ;; suppressed, and the suggestion list is computed lazily and already capped,
  ;; so no full-PATH scan runs on a suppressed line.
  (define (command-not-found-check line)
    (let ([cmd (first-token line)])
      (unless (command-not-found-suppress? cmd)
        (let* ([suggestions (command-not-found-suggestions cmd)]
               ;; Grey the diagnostic only when stderr is a live terminal; the
               ;; message text always survives to a piped or captured stream.
               [colour? (colour-ok? (console-error-port))])
          (when colour? (display "\x1b;[38;5;240m" (console-error-port)))
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
          (when colour? (display "\x1b;[39m" (console-error-port)))
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

  ;; Master switch for the REPL-entry default-on affordances.  init.ss is loaded
  ;; BEFORE the REPL runs, so a bare (visit-recording? #f) there would be undone
  ;; by the enablement below; this gate lets init.ss (or plain-shell!) turn the
  ;; whole default-on set off up front.  #t installs them, #f leaves a bare REPL;
  ;; the individual toggles (shell-highlight?, auto-cd?, ...) still apply on top.
  (define interactive-enhancements?
    (make-parameter #t (lambda (v) (and v #t))))

  (define (interactive-repl)
    ;; The REPL-entry default-on affordances, all behind the master gate so a
    ;; plain-shell! / (interactive-enhancements? #f) in init.ss (loaded first)
    ;; suppresses them: recording directory visits (an interactive-session
    ;; affordance, off at program top-level so a -c/-s/batch run never opens the
    ;; visit database) and installing the default alias set (each alias skipped
    ;; when the user already defined it in init.ss).
    (when (interactive-enhancements?)
      (visit-recording? #t)
      (when (default-aliases?)
        (install-default-aliases!)))
    ;; SHLVL management: increment on entry, decrement on exit
    (let ([old-shlvl (let ([v (getenv "SHLVL")])
                       (if v (or (string->number v) 0) 0))])
      (setenv "SHLVL" (number->string (+ old-shlvl 1)))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          ;; Seed the width cache and the width parameter at entry from ONE live
          ;; query, so the editor's first render reads the true width rather than
          ;; the stale 24x80 default.
          (refresh-terminal-width!)

          ;; Initialize PATH cache for shell-mode classification
          (rebuild-path-cache!)

          ;; Install job control signal handlers
          (install-job-signals!)

          ;; Arm the last-resort terminal guard once, so a cooked terminal is
          ;; re-asserted on the exit and fatal-signal paths the editor's own
          ;; dynamic-wind cannot see.
          (install-terminal-guard!)

          ;; Register the SIGWINCH handler that reruns the resize step, so both
          ;; the width parameter and the leaf width cache track the new size --
          ;; from a single kernel query, not one apiece.
          (install-resize-handler!)

          ;; Main REPL loop with call/cc restart pattern
          ;; Determine at startup whether stdin is a terminal (editor vs bare read).
          ;; --batch (via batch-mode?) and HAFOD_BATCH force the bare-read path even
          ;; on a terminal.  HAFOD_BATCH enables batch when set to a non-falsy
          ;; value (1, yes, true, ...); it is ignored -- leaving the choice to the
          ;; tty -- when unset or set to a falsy value: the empty string, 0, false
          ;; or no, matched case-insensitively.  So HAFOD_BATCH=0 turns batch off
          ;; rather than silently enabling it.
          (let ([use-editor? (use-editor?* (tty? 0)
                                           (tty? 1)
                                           (batch-mode?)
                                           (let ([v (getenv "HAFOD_BATCH")])
                                             (and v
                                                  (not (member (string-downcase v)
                                                               '("" "0" "false" "no")))
                                                  #t)))])
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
                              (let* ([class (if (shell-mode?)
                                                (classify-input line)
                                                'scheme)]
                                     ;; The line actually executed.  An alias head
                                     ;; is expanded to its target here, at exec
                                     ;; time, so the builtin, shell and scheme arms
                                     ;; all run the expansion -- while history and
                                     ;; the echoed line keep the raw alias name
                                     ;; (aliases stay invisible, unlike
                                     ;; history-expand, which prints its rewrite
                                     ;; above).  The scheme arm reads exec-line too:
                                     ;; an alias whose expansion is a Scheme form, a
                                     ;; literal, or a bare bound identifier DOES
                                     ;; classify as scheme, so it lands there and
                                     ;; must run the expansion, not the raw name.
                                     ;; The auto-cd rescue keeps the raw line, as an
                                     ;; aliased head is command-not-found suppressed
                                     ;; and so never auto-cds.
                                     [exec-line (if (shell-mode?)
                                                    (alias-expand-line line)
                                                    line)]
                                     ;; The auto-cd decision, computed once: a bare
                                     ;; existing-directory token in shell mode
                                     ;; (which classifies as scheme).  Reused to tag
                                     ;; history AND to dispatch below, so the tag is
                                     ;; written a single time here -- run-auto-cd!
                                     ;; no longer re-tags it.
                                     [cd-dir (and (eq? class 'scheme)
                                                  (auto-cd-decision line))])
                                ;; Tag the history entry with its eval mode: a
                                ;; builtin, a shell command, or an auto-cd all act
                                ;; as shell; only a genuine Scheme evaluation is
                                ;; tagged 'scheme.
                                (editor-history-set-last-mode!
                                  (if (or (memq class '(shell builtin)) cd-dir)
                                      'shell
                                      'scheme))
                                (case class
                                  [(builtin)
                                   ;; Execute builtin directly, skip eval
                                   (guard (exn
                                            [#t (let ([colour? (colour-ok? (console-error-port))])
                                                  (when colour? (display "\x1b;[31m" (console-error-port)))
                                                  (display-condition exn (console-error-port))
                                                  (when colour? (display "\x1b;[39m" (console-error-port)))
                                                  (newline (console-error-port)))])
                                     (run-builtin! exec-line))
                                   ;; Return (void) so the existing cond path skips display
                                   (void)]
                                  [(shell)
                                   ;; Parse to EPF datum, return for eval
                                   (parse-shell-command exec-line)]
                                  [else
                                   ;; Original path (scheme), with a leading
                                   ;; auto-cd rescue: a bare shell-mode token that
                                   ;; names an existing directory (resolving to no
                                   ;; command, builtin, alias, or bound identifier)
                                   ;; changes into it and skips eval, returning
                                   ;; (void) so the loop continues just as the
                                   ;; builtin arm does.  A real command/builtin/alias
                                   ;; never reaches this arm (it classifies as
                                   ;; 'shell or 'builtin), so "a real command wins"
                                   ;; needs no extra code here.
                                   (cond
                                     [cd-dir
                                      => (lambda (dir) (run-auto-cd! dir) (void))]
                                     [else
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
                                   ;; Multiple expressions are tagged so the eval
                                   ;; loop can process each one individually,
                                   ;; printing results between them.  read-forms is
                                   ;; applied to exec-line when an alias expanded to
                                   ;; a Scheme form (so the EXPANSION runs, not the
                                   ;; bare alias name), and to the raw line
                                   ;; otherwise -- the two are identical for all
                                   ;; genuine, non-alias Scheme input, so that path
                                   ;; is byte-for-byte unchanged.  A malformed alias
                                   ;; expansion is caught rather than aborting the
                                   ;; REPL, and an empty one is a no-op instead of
                                   ;; an EOF (which would quit the loop).
                                   (let ([read-forms
                                          (lambda (src)
                                            (let* ([p (open-input-string src)]
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
                                                                (gather (cons next acc))))))))))])
                                     (if (string=? exec-line line)
                                         (read-forms line)
                                         (let ([forms
                                                (guard (exn
                                                         [#t (let ([colour? (colour-ok? (console-error-port))])
                                                               (when colour? (display "\x1b;[31m" (console-error-port)))
                                                               (display-condition exn (console-error-port))
                                                               (when colour? (display "\x1b;[39m" (console-error-port)))
                                                               (newline (console-error-port)))
                                                             (void)])
                                                  (read-forms exec-line))])
                                           (if (eof-object? forms) (void) forms))))])])))])))
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
                                     (let ([colour? (colour-ok? (console-error-port))])
                                       (when colour? (display "\x1b;[31m" (console-error-port)))
                                       (display-condition exn (console-error-port))
                                       (when colour? (display "\x1b;[39m" (console-error-port)))
                                       (newline (console-error-port)))
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
                             (let ([colour? (colour-ok? (console-error-port))])
                               (when colour? (display "\x1b;[31m" (console-error-port)))
                               (display-condition exn (console-error-port))
                               (when colour? (display "\x1b;[39m" (console-error-port)))
                               (newline (console-error-port)))
                             ;; Post-eval hook (failure case)
                             ((repl-post-eval-hook) form (void))
                             (display-command-timing!)
                             (loop)])
                     (call-with-values
                       (lambda ()
                         ;; Re-assert the known-good cooked terminal after each
                         ;; evaluation, so an interactive child (ssh, vim, ...)
                         ;; that left the terminal dirty does not strand the
                         ;; prompt in raw mode.  The single owner holds the one
                         ;; cooked baseline; re-asserting it is idempotent and
                         ;; internally guarded.
                         (dynamic-wind
                           void
                           (lambda () (eval form (interaction-environment)))
                           (lambda ()
                             (when (tty? 0) (reassert-cooked-tty! 0)))))
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
