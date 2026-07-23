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
    ;; prompt-customised? -- the pristine-prompt guard: #t once the user has taken
    ;; over the prompt (set-prompt!, a direct repl-prompt-hook / repl-prompt-string
    ;; / repl-right-prompt-hook rebind, or composing the prompt-segments list via
    ;; register-prompt-segment! / clear-prompt-segments! / the preset), so the
    ;; REPL-entry default-on skips a user's own prompt rather than clobbering it.  A
    ;; leaf-only white-box surface (a test reaches it to drive the guard);
    ;; deliberately NOT a (hafod) umbrella member.
    prompt-customised?
    ;; informative-prompt? -- the opt-out parameter (default #t) for the default-on
    ;; informative prompt; user-facing, re-exported through the (hafod) umbrella.
    ;; should-install-default-prompt? -- the REPL-entry default-on decision minus the
    ;; real-terminal gate (interactive-enhancements? AND informative-prompt? AND not
    ;; customised AND HAFOD_PROMPT not falsy).  A leaf-only white-box surface, NOT a
    ;; (hafod) umbrella member.
    informative-prompt?
    should-install-default-prompt?
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
    ;; display-right-prompt -- position the right-prompt hook output flush at the
    ;; right terminal edge (cursor save / column jump / restore), reserving the
    ;; TRUE wcwidth cell width so a width-2 glyph lands at the edge without
    ;; overflowing.  A leaf-only white-box surface (a test drives it to prove the
    ;; cell alignment); deliberately NOT a (hafod) umbrella member.
    display-right-prompt
    background-job-count
    ;; prompt-path-segment -- pure home-relative + fish-truncated path renderer.
    ;; User-facing: the (hafod) umbrella re-exports it so init.ss config can
    ;; compose a working-directory prompt segment.
    prompt-path-segment
    ;; prompt-git-segment -- single-spawn, coloured, sanitised, fail-quiet git
    ;; state renderer; prompt-colour-ok? -- the overridable colour verdict thunk
    ;; (default (colour-ok? 1)) that every coloured prompt segment gates on.
    ;; Both are user-facing and re-exported from the (hafod) umbrella.
    prompt-git-segment
    prompt-colour-ok?
    ;; with-spawn-timeout -- the bounded, synchronous child-output collector: it
    ;; spawns a program with its stdout piped back, polls that descriptor against
    ;; a deadline, and on a timeout kills (SIGTERM->SIGKILL) and reaps the child
    ;; exactly once.  The 3-arg form discards the child's stderr and spawns with an
    ;; empty environment (the shipped git-probe disposition); the 5-arg form adds a
    ;; stderr disposition ('merge folds the child's stderr into the captured pipe)
    ;; and an environment list handed to the child.  with-spawn-timeout/status is
    ;; the same collector returning the child's exit status as a third value, which
    ;; the version probe needs to tell a banner from a failing shim's
    ;; version-shaped diagnostic; its 6-arg form adds the CALLER'S OWN ceiling on
    ;; how many bytes the child may push into the accumulator (#f, and so every
    ;; other form, means no ceiling and the deadline as the only bound).  Past that
    ;; ceiling the child is killed and reaped and the captured prefix is returned
    ;; as a NON-timeout with an unknown exit status: a byte cap is not a wall-clock
    ;; overrun, and a caller that can use the prefix -- the git segment reads its
    ;; branch header off the front -- must not be told its answer failed.
    ;; prompt-spawn-timeout-ms -- the deadline parameter (default 150 ms,
    ;; opt-tunable).  Both collectors are leaf-only white-box surfaces;
    ;; prompt-spawn-timeout-ms is an umbrella member.
    with-spawn-timeout
    with-spawn-timeout/status
    prompt-spawn-timeout-ms
    ;; spawn-grace-probe-count -- a leaf test seam counting the SIGTERM->SIGKILL
    ;; grace-loop reap probes on a timeout, so a test can prove the loop sleeps
    ;; between probes rather than busy-spinning.  Leaf-only, NOT a (hafod) umbrella
    ;; member (the same convention as git-probe-count).
    spawn-grace-probe-count
    ;; cached-git-segment -- the per-cwd, HEAD+index-mtime-keyed git segment: it
    ;; serves an unchanged repository from the cache instead of re-spawning git.
    ;; resolve-git-dir / git-marker-stamp -- the git-dir walk (handling the
    ;; ".git is a FILE" worktree case) and the two-mtime stamp it keys on.
    ;; git-probe-count -- a leaf test seam counting real recomputes.  All four are
    ;; leaf-only white-box surfaces; the preset composes cached-git-segment.
    cached-git-segment
    resolve-git-dir
    git-marker-stamp
    git-probe-count
    ;; prompt-exit-segment -- the red "✘N" exit-code badge for the right prompt,
    ;; shown only after a non-zero last command.  User-facing: the (hafod)
    ;; umbrella re-exports it.
    prompt-exit-segment
    ;; prompt-timing-segment -- a format-duration readout for the right prompt,
    ;; shown only when the last command ran at least prompt-timing-threshold (a
    ;; parameter defaulting to 2000 ms).  Both are user-facing and re-exported
    ;; from the (hafod) umbrella.
    prompt-timing-segment
    prompt-timing-threshold
    ;; enable-informative-prompt! -- the one-call preset that composes the four
    ;; prompt segments into the existing left/right hooks, configured by a
    ;; quoted-symbol option plist with sensible defaults and still overridable
    ;; segment by segment.  User-facing: the (hafod) umbrella and the config
    ;; surface re-export it, beside set-prompt!.
    enable-informative-prompt!
    ;; === Prompt segment model ===
    ;; prompt-segments -- the ordered (placement . thunk) segment list parameter;
    ;; register-prompt-segment! appends (registration order is draw order) and
    ;; clear-prompt-segments! empties it.  make-current-prompt-ctx captures the
    ;; live draw state; render-prompt-segments / render-right-prompt-segments are
    ;; the placement-aware renderers the preset installs into the hooks.
    ;; The prompt-ctx field accessors expose the per-draw state to a segment thunk.
    ;; prompt-char-segment -- the exit-coloured input glyph (❯ / ascii '>'), the
    ;; first line segment.  register / clear / the parameter are (hafod) umbrella
    ;; members; the renderers, ctx builder, accessors and prompt-char-segment
    ;; stay leaf.
    prompt-segments
    register-prompt-segment!
    clear-prompt-segments!
    make-current-prompt-ctx
    render-prompt-segments
    render-right-prompt-segments
    prompt-char-segment
    prompt-ctx-exit
    prompt-ctx-duration
    prompt-ctx-cwd
    prompt-ctx-colour-depth
    prompt-ctx-glyph-tier
    prompt-ctx-width
    ;; === Per-language version tools ===
    ;; The registry surface an init.ss composes with, all four re-exported from the
    ;; (hafod) umbrella: register-prompt-tool! adds a language version segment
    ;; (marker -> command -> version parse -> glyph), unregister-prompt-tool! drops
    ;; one by name, clear-prompt-tools! empties the registry, and prompt-tools is
    ;; the ordered list itself.  prompt-tool-name goes with them, since without at
    ;; least one accessor the exported registry is a list of unreadable records.
    ;; The rest of the record (make-prompt-tool / prompt-tool? / the other seven
    ;; accessors) stays a LEAF white-box surface -- a test reaches it via
    ;; (only (hafod interactive) ...) -- deliberately NOT an umbrella member.
    register-prompt-tool!
    unregister-prompt-tool!
    prompt-tools
    clear-prompt-tools!
    make-prompt-tool
    prompt-tool?
    prompt-tool-name
    prompt-tool-markers
    prompt-tool-command
    prompt-tool-args
    prompt-tool-parse
    prompt-tool-emoji
    prompt-tool-ascii
    prompt-tool-nerd
    ;; parse-version/common + the per-tool parse procedures -- the shared
    ;; version-token extractor and its ten thin per-language wrappers.  Leaf-only
    ;; exports (white-box test surface): a test reaches them via
    ;; (only (hafod interactive) ...) to prove each parse rule against canned
    ;; --version strings with no spawn.  Deliberately NOT umbrella members, the
    ;; same convention as sanitize-control below.
    parse-version/common
    parse-python
    parse-node
    parse-rust
    parse-go
    parse-bun
    parse-ruby
    parse-java
    parse-deno
    parse-php
    parse-zig
    ;; nerd-glyphs? -- the Nerd-Font opt-in parameter.  A documented user
    ;; configuration knob ("a user opts in from their init with (nerd-glyphs? #t)",
    ;; config being Scheme), so it is re-exported from the (hafod) umbrella: an
    ;; init.ss that imports (hafod) must be able to reach the opt-in it is told to
    ;; use.  tool-glyph / render-one-tool -- the tier-aware glyph picker and the
    ;; one-tool "via <glyph> vX" renderer; leaf-only (white-box test surface), a
    ;; test reaches them via (only (hafod interactive) ...) to prove the glyph
    ;; choice, the layout, the colour gating and the control-byte sanitisation with
    ;; no spawn.  Those two stay leaf beside the segment model.
    nerd-glyphs?
    tool-glyph
    render-one-tool
    ;; detect-tools-in / tool-on-path? -- the current-directory marker detector
    ;; (one directory-list against the union of the registered markers, NO
    ;; walk-up) and the O(1) path-cache PATH gate that declines an uninstalled
    ;; tool before any spawn.  Leaf-only white-box surfaces reached via
    ;; (only (hafod interactive) ...); deliberately NOT umbrella members, the same
    ;; convention as cached-git-segment above.
    detect-tools-in
    tool-on-path?
    ;; tool-command-candidates / resolve-tool-command / tool-probe-argv -- the
    ;; SINGLE definition of a valid command shape (a binary name, or a non-empty
    ;; list of candidate names tried in order), the first-candidate-on-PATH
    ;; resolver the gate, the probe and the cache stamp all consult, and the argv
    ;; builder that substitutes the resolved winner as argv[0] for a candidate list
    ;; ONLY -- a single-name command keeps its argv verbatim.  Leaf-only white-box
    ;; surfaces reached via (only (hafod interactive) ...); deliberately NOT
    ;; umbrella members, the same convention as the two seams above.
    tool-command-candidates
    resolve-tool-command
    tool-probe-argv
    ;; version-marker-stamp / version-probe-count / cached-version-segment /
    ;; probe-tool-version / render-version-group -- the per-cwd version cache
    ;; (mirroring cached-git-segment, keyed on the toolchain environment, the
    ;; detected tool names and the detected markers' mtimes), the REAL-spawn
    ;; counter seam, and the bounded, stderr-merging, env-passing single probe.
    ;; version-cache-ttl-ms -- the age at which a cached group is re-probed
    ;; anyway, so a test can force expiry instead of waiting.
    ;; version-detect-count -- the REAL-directory-scan counter seam, the oracle
    ;; for "a repeat draw in an unchanged directory does no filesystem work".
    ;; version-cache-cap -- how many directories the table holds before an insert
    ;; of a new key clears it, a parameter so a test can drive the overflow rule
    ;; with three directories rather than sixty-five.
    ;; version-group-segment -- the `left` segment thunk the preset registers
    ;; after git.  Leaf-only white-box surfaces (the cache table stays private);
    ;; NOT umbrella members, the same convention as the git cache above.
    ;; prompt-versions? -- the version group's own opt-out (default #t), so a user
    ;; who does not want a cd into an untrusted repository to run its toolchains
    ;; can switch off the group alone rather than the whole informative prompt.
    ;; User-facing: the (hafod) umbrella re-exports it.  version-group-enabled? --
    ;; the parameter AND the falsy-HAFOD_PROMPT_VERSIONS check together, a
    ;; leaf-only seam a test drives directly.
    version-marker-stamp
    version-probe-count
    version-detect-count
    version-cache-ttl-ms
    version-cache-cap
    cached-version-segment
    probe-tool-version
    render-version-group
    prompt-versions?
    version-group-enabled?
    version-group-segment
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
    ;; directory changes into it.  User-facing: the (hafod) umbrella and the
    ;; config surface re-export it so init.ss can switch it off.
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
          (only (hafod posix) status:exit-val SIGWINCH posix-waitpid
                ;; Bounded child collector: spawn with stdout piped back, drive
                ;; the read fd non-blocking against a poll(2) deadline, and kill
                ;; (SIGTERM->SIGKILL) + reap on timeout.  stat-info-mtime feeds
                ;; the per-cwd git segment cache key.
                posix-spawnp/pipe posix-fcntl posix-read posix-close posix-kill
                fd-wait-readable O_WRONLY O_NONBLOCK F_GETFL F_SETFL
                SIGTERM SIGKILL wait/poll posix-stat stat-info-mtime)
          (only (hafod signal) set-signal-handler!)
          ;; cached-env-strings is the project's single spawn-environment path --
          ;; the dirty-flag-invalidated "KEY=VALUE" list (hafod process) already
          ;; hands to posix_spawnp.  The version probe hands the child the same
          ;; list, so a spawned tool (or a version-manager shim) resolves against
          ;; the caller's environment, without a second implementation of how
          ;; hafod composes a child environment.
          (only (hafod environment) getenv setenv cached-env-strings)
          (only (hafod procobj) background-job-count)
          (only (hafod editor editor) read-expression with-raw-mode
                editor-history-entries editor-history-set-last-mode!)
          (only (hafod tty) tty? terminal-size refresh-terminal-size-cache!
                cached-terminal-size
                reassert-cooked-tty! install-terminal-guard!)
          ;; colour-depth / glyph-tier: the capability tiers a prompt segment
          ;; consults to pick colours and glyphs (the per-draw context caches
          ;; them so a thunk reads a field rather than re-deriving the verdict).
          (only (hafod terminal-caps) ansi-ok? colour-ok? colour-depth glyph-tier)
          (only (hafod editor render) tokenize display-colourised)
          ;; Pure prompt-path helpers: logical "."/".."/"//" cleanup, $HOME and
          ;; the current directory (the latter two feed the impure prompt caller
          ;; and are held in scope here), and the wcwidth display-width oracle so
          ;; the path budget counts rendered columns rather than characters.
          (only (hafod fname) simplify-file-name)
          (only (hafod user-group) home-directory)
          (only (hafod process-state) cwd)
          (only (hafod editor input-decode) string-display-width char-display-width)
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

  ;; === Pristine-prompt guard ===
  ;;
  ;; prompt-customised? records whether the user has taken over the prompt -- by
  ;; calling set-prompt!, by rebinding repl-prompt-hook / repl-right-prompt-hook /
  ;; repl-prompt-string directly, or by composing the prompt-segments list (through
  ;; register-prompt-segment! / clear-prompt-segments! / the enable-informative-
  ;; prompt! preset).  The default-on flip at REPL entry consults it so a user's own
  ;; prompt (set in init.ss, which loads before the REPL) is never clobbered.
  ;;
  ;; The mechanism dodges a Chez trap: a parameter's converter runs on its DEFAULT
  ;; value at construction, so a converter that unconditionally marked the prompt
  ;; customised would trip the instant repl-prompt-string and the two hooks are built,
  ;; before any user action -- and default-on would then never fire on a pristine
  ;; start.  The guard is therefore ARMED (%prompt-guard-armed? set #t) only at the
  ;; very END of the library body, once every parameter and the preset are defined;
  ;; the converters trip the flag only while it is armed.  The preset and the
  ;; default-on flip install their own hooks with the guard DISARMED
  ;; (%with-guard-disarmed), so composing the default prompt does not itself look like
  ;; a user customisation.
  (define prompt-customised?
    (make-parameter #f (lambda (v) (and v #t))))

  ;; Armed at the end of the library body (see the set! there); #f throughout
  ;; construction so the hook converters running on their own defaults never
  ;; self-mark the prompt customised (the Chez converter-on-default trap).
  (define %prompt-guard-armed? #f)

  ;; Run THUNK with the guard disarmed, restoring the prior armed state afterwards.
  ;; The preset and the default-on flip install their hooks through this, so their
  ;; own sets do not trip prompt-customised?.
  (define (%with-guard-disarmed thunk)
    (let ([saved %prompt-guard-armed?])
      (dynamic-wind
        (lambda () (set! %prompt-guard-armed? #f))
        thunk
        (lambda () (set! %prompt-guard-armed? saved)))))

  ;; Simple prompt customisation: set this string to change the default prompt.
  ;; For dynamic prompts, replace repl-prompt-hook instead.
  (define repl-prompt-string
    (make-parameter "> "
      (lambda (v)
        (unless (string? v)
          (error 'repl-prompt-string "expected a string" v))
        (when %prompt-guard-armed? (prompt-customised? #t))
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
        (when %prompt-guard-armed? (prompt-customised? #t))
        v)))

  ;; Right prompt hook: procedure that writes to current-output-port (captured by REPL).
  ;; Default: no-op (no right prompt).
  (define repl-right-prompt-hook
    (make-parameter
      (lambda () (void))
      (lambda (v)
        (unless (procedure? v)
          (error 'repl-right-prompt-hook "expected a procedure" v))
        (when %prompt-guard-armed? (prompt-customised? #t))
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

  ;; Opt-out for the default informative prompt.
  ;; When #t (the default), a pristine interactive session -- one where the user has
  ;; not set their own prompt -- installs the informative prompt at REPL entry.  Set
  ;; it #f in init.ss (or set HAFOD_PROMPT to a falsy value: the empty string, 0,
  ;; false, or no) to keep the plain "> " prompt without having to define one.  A
  ;; user who sets their own prompt needs neither: the pristine guard already skips
  ;; default-on for them.
  (define informative-prompt?
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

  ;; Compute the visible DISPLAY WIDTH of a string in terminal cells, stripping
  ;; ANSI escape sequences.  Handles CSI sequences (ESC [ ... final-byte) and
  ;; non-CSI escapes (ESC + 1 char).  A plain character contributes its wcwidth
  ;; cell count, not a flat one, so a wide/emoji/CJK glyph counts two -- this makes
  ;; the result identical to (hafod editor render)'s ansi-display-width and lets
  ;; display-right-prompt reserve the true rendered width (a width-2 right prompt
  ;; no longer overflows or wraps the line).
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
           (loop (+ i 1) (+ visible (char-display-width (string-ref str i))))]))))

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

  ;; === Bounded child collector ===

  ;; The deadline, in milliseconds, that with-spawn-timeout gives a child probe
  ;; before it is bounded and killed.  150 ms is enough for a warm `git status`
  ;; yet short enough that a cold or very large repository shows an empty git
  ;; segment on the first draw rather than stalling the prompt -- the deliberate
  ;; bounded-synchronous tradeoff (hafod's green threads run on one OS thread, so
  ;; true async is out).  Opt-tunable; validated as a positive exact integer,
  ;; mirroring prompt-timing-threshold above.
  (define prompt-spawn-timeout-ms
    (make-parameter 150
      (lambda (v)
        (unless (and (integer? v) (exact? v) (positive? v))
          (error 'prompt-spawn-timeout-ms "expected a positive exact integer" v))
        v)))

  ;; How long, in milliseconds, a SIGTERM'd child is given to exit before the
  ;; escalation to SIGKILL.  Short: the child is a read-only `git status` (or a
  ;; test's sleep), losing nothing when forced.
  (define %spawn-term-grace-ms 20)

  ;; The ceiling THE VERSION PROBE asks the collector for -- not a global one.
  ;; The read loop appends 4 KB chunks until EOF or the deadline, so without a
  ;; ceiling a program that writes fast for the whole deadline can put tens of
  ;; megabytes into the bytevector port -- which the finish then materialises
  ;; through utf8->string at four bytes per character, a further fourfold
  ;; expansion, all inside a prompt draw.  The version group widened the callee
  ;; set from `git status` to arbitrary user-installed interpreters and wrapper
  ;; shims, several of which will happily dump a log, so THAT caller wants a
  ;; bound: 64 KB is orders of magnitude more than any --version banner and still
  ;; a trivial allocation.
  ;;
  ;; It is passed per call rather than applied inside the collector because the
  ;; collector's other caller is the git segment, whose payload is a whole
  ;; `git status --porcelain=v2` -- some 150 bytes per changed or untracked entry,
  ;; so a few hundred of them clear 64 KB in an ordinary dirty tree.  A ceiling
  ;; charged to that caller truncates a legitimate answer, and the branch header
  ;; the renderer needs is at the very front of it.  The git probe therefore asks
  ;; for no ceiling at all and keeps the deadline as its only bound, exactly as it
  ;; always had.
  (define %spawn-max-bytes 65536)

  ;; The pause between grace-loop reap probes on the timeout path.  Without it the
  ;; SIGTERM->SIGKILL wait busy-spins waitpid(WNOHANG) as fast as the CPU allows for
  ;; up to the grace window -- burning a core and, on the single OS thread, yielding
  ;; to no green thread.  A couple of milliseconds keeps the escalation prompt (a
  ;; cooperative child honouring SIGTERM is still reaped within a probe or two) while
  ;; removing the spin.  A precomputed time-duration so the loop allocates none.
  (define %spawn-grace-poll-sleep (make-time 'time-duration 2000000 0))  ; 2 ms

  ;; A leaf test seam: incremented once per grace-loop reap probe on a timeout, so a
  ;; test can prove the loop sleeps between probes (a bounded count) rather than
  ;; busy-spinning (thousands).  Default 0; NOT a (hafod) umbrella member.
  (define spawn-grace-probe-count
    (make-parameter 0
      (lambda (v)
        (unless (and (integer? v) (exact? v) (>= v 0))
          (error 'spawn-grace-probe-count "expected a non-negative exact integer" v))
        v)))

  ;; A single, guarded, BLOCKING reap of PID, yielding the raw wait status this
  ;; call collected, or #f when there was none to collect.  Guarded because an
  ;; earlier reap probe may already have taken this child (ECHILD) -- so the reap
  ;; runs at most once effectively and never raises.  Used only after SIGKILL,
  ;; which is uninterceptable, so the wait returns promptly.  The status is
  ;; returned rather than dropped because the version probe needs it: a toolchain
  ;; shim that fails prints a version-shaped diagnostic and exits non-zero, and
  ;; only the exit status tells the two apart.
  (define (reap-child-blocking pid)
    (guard (e [#t #f])
      (let-values ([(w s) (posix-waitpid pid 0)])
        (and (integer? w) (not (= w 0)) s))))

  ;; A WNOHANG reap probe.  Yields the child's raw wait status when THIS call
  ;; collected it, the symbol 'gone when it had already been reaped (ECHILD,
  ;; raised then caught -- the status is no longer available), and #f while it is
  ;; still running (0).  Reaping here is the point: it lets a grace loop collect a
  ;; SIGTERM'd child without a second blocking wait, and -- crucially -- lets the
  ;; escalation skip SIGKILL once the child is gone, so no signal is ever sent to
  ;; a reaped pid (closing the pid-reuse window).
  (define (reap-child-poll pid)
    (guard (e [#t 'gone])
      (let-values ([(w s) (posix-waitpid pid wait/poll)])
        (and (integer? w) (not (= w 0)) s))))

  ;; #t when the child is no longer a live, unreaped child of ours.
  (define (child-reaped? pid) (and (reap-child-poll pid) #t))

  ;; Reap a child that has finished WRITING, bounded.  EOF on the pipe means every
  ;; write end is closed; it does NOT mean the child has exited.  A wrapper that
  ;; redirects its own stdout and stderr away and then works on
  ;; (`exec >/dev/null 2>&1; ...`), or a launcher that daemonises, yields EOF at
  ;; once and keeps running -- and a plain blocking waitpid there hangs the prompt
  ;; draw for as long as the child lives.  The deadline machinery bounds the READ;
  ;; this bounds the reap, giving the clean path the same treatment the timeout
  ;; path already had: poll for the same brief grace, then kill and reap.  Yields
  ;; the wait status when it was collected, else #f.
  (define (reap-child-once pid)
    (let ([t0 (real-time)])
      (let poll ([r (reap-child-poll pid)])
        (cond
          [r r]                                    ; collected, or already gone
          [(>= (- (real-time) t0) %spawn-term-grace-ms)
           (kill-and-reap-child pid)
           #f]
          [else
           ;; Sleep between probes for the same reason the timeout grace loop does:
           ;; a tight waitpid(WNOHANG) spin burns a core and, on hafod's single OS
           ;; thread, yields to no green thread.
           (sleep %spawn-grace-poll-sleep)
           (poll (reap-child-poll pid))]))))

  ;; Kill and reap a timed-out child exactly once: SIGTERM, a brief wall-clock
  ;; grace polling for its exit, then SIGKILL + a single blocking reap only if it
  ;; is still alive.  Because the escalation is skipped the moment child-reaped?
  ;; is true, a SIGKILL is never sent to a pid that has already been reaped and
  ;; possibly reused.  Every signal and wait is guarded, so the internal
  ;; child-reaped? / reap-child-once double-reap cannot raise out of the prompt
  ;; draw (reaping is synchronous -- there is no async SIGCHLD handler to race).
  (define (kill-and-reap-child pid)
    (guard (e [#t (void)]) (posix-kill pid SIGTERM))
    (let ([g0 (real-time)])
      (let grace ()
        (cond
          [(child-reaped? pid) (void)]        ; exited on SIGTERM (or already reaped)
          [(>= (- (real-time) g0) %spawn-term-grace-ms)
           (guard (e [#t (void)]) (posix-kill pid SIGKILL))
           (reap-child-blocking pid)]
          [else
           ;; Count the probe and pause briefly before re-probing, so the wait is a
           ;; bounded sleep-poll rather than a tight waitpid spin that burns a core.
           (spawn-grace-probe-count (+ (spawn-grace-probe-count) 1))
           (sleep %spawn-grace-poll-sleep)
           (grace)]))))

  ;; Close the read fd once (guarded, on every exit path), settle the child, and
  ;; return the accumulated bytes decoded ONCE as UTF-8 -- never a partial
  ;; multibyte chunk (get-acc yields the whole buffer).
  ;;
  ;; DISPOSITION says WHY the read loop stopped, and there are three answers, not
  ;; two:
  ;;   eof       the child closed its output and finished -- reap it, nothing was
  ;;             cut short, and its exit value is the third returned value.
  ;;   deadline  the wall clock ran out (or the setup faulted) -- kill, reap, and
  ;;             report timed-out.
  ;;   ceiling   the caller's byte ceiling tripped -- the child is still running,
  ;;             so it is killed and reaped exactly as a deadline overrun is, but
  ;;             it is NOT reported as a timeout.  Conflating the two told the git
  ;;             segment that a large-but-perfectly-good `git status` had failed,
  ;;             and it dropped a complete answer it was already holding.  A
  ;;             ceiling stop is instead visible as an UNKNOWN exit status (the
  ;;             child was killed, so there is none), which is what the version
  ;;             probe already rejects on.
  ;;
  ;; Returns (values output-string timed-out? exit-status), where exit-status is
  ;; the child's exit value on a normal exit and #f when it is unknown: a killed
  ;; child (timed out or capped), one that died on a signal, or one already reaped
  ;; by a grace-loop probe.  A caller that only wants the output ignores it; the
  ;; version probe reads it to reject a shim's non-zero-exit diagnostic, which is
  ;; otherwise indistinguishable from a banner.
  (define (finish-spawn pid rfd get-acc disposition)
    (guard (e [#t (void)]) (posix-close rfd))
    (let* ([finished? (eq? disposition 'eof)]
           [status (if finished?
                       (reap-child-once pid)
                       (begin (kill-and-reap-child pid) #f))])
      (values (utf8->string (get-acc))
              (eq? disposition 'deadline)
              (and (integer? status) (status:exit-val status)))))

  ;; with-spawn-timeout/status: a bounded, synchronous child-output collector.
  ;; Spawn PROGRAM (searched on PATH) with ARGV and stdout piped back, drive the
  ;; read fd non-blocking against DEADLINE-MS with fd-wait-readable: on 'ready read
  ;; a chunk (a zero-length read is EOF -> the child finished), on 'timeout or an
  ;; expired deadline finish timed-out, on 'interrupted (EINTR from SIGWINCH/SIGCHLD)
  ;; recompute the remaining deadline and re-poll.  A spurious EAGAIN from the read
  ;; re-polls.  Bytes accumulate in a bytevector and are decoded once at the finish.
  ;; The whole spawn is guarded so an absent program returns a quiet empty result,
  ;; matching the fail-quiet git path.  Returns
  ;; (values output-string timed-out? exit-status), the third value being the
  ;; child's exit value on a normal exit and #f when it is unknown (killed, dead on
  ;; a signal, or never spawned).
  ;;
  ;; STDERR-MODE 'merge dups fd 1 -- already the pipe write end by the time caller
  ;; file actions run (posix-spawnp/pipe appends them after its own dup2 of the pipe
  ;; onto fd 1) -- onto fd 2, so a child that prints its banner to stderr (such as
  ;; `java -version`) is captured in the same pipe; any other mode discards stderr
  ;; to /dev/null as before.  ENV-LIST -- a "KEY=VALUE" list or #f -- is delivered
  ;; to the child: a real list lets a spawned tool (or a version-manager shim)
  ;; resolve against the caller's environment, while #f keeps the shipped empty
  ;; environment.
  ;;
  ;; MAX-BYTES is the caller's OWN ceiling on what the child may push into the
  ;; accumulator, or #f for no ceiling; the 5-argument form is #f, so every
  ;; existing caller keeps the deadline as its only bound.  The ceiling belongs to
  ;; the caller because the callers want opposite things: a --version banner is a
  ;; couple of lines and anything past 64 KB of it is a log, while a
  ;; `git status --porcelain=v2` legitimately runs to hundreds of kilobytes in a
  ;; dirty tree.  Past the ceiling the child is killed and reaped and the result
  ;; is reported as a NON-timeout with an unknown exit status (see finish-spawn).
  (define with-spawn-timeout/status
   (case-lambda
    [(program argv deadline-ms stderr-mode env-list)
     (with-spawn-timeout/status program argv deadline-ms stderr-mode env-list #f)]
    [(program argv deadline-ms stderr-mode env-list max-bytes)
     (let ([stderr-action
            (case stderr-mode
              [(merge) (list 'dup2 1 2)]                          ; 2>&1 into the pipe
              [else    (list 'open 2 "/dev/null" O_WRONLY 0)])])  ; discard (shipped)
       (guard (e [#t (values "" #f #f)])
         (let-values ([(pid rfd)
                       (posix-spawnp/pipe program argv
                         (list stderr-action) env-list)])
           ;; The child and its read fd now exist.  Open the accumulator first so
           ;; get-acc is in scope for the teardown, then run the non-blocking setup
           ;; and the read loop under an inner guard: if anything between here and
           ;; the loop raises -- in practice only the posix-fcntl F_GETFL/F_SETFL
           ;; pair, since the read is guarded inline -- finish-spawn still closes the
           ;; read fd (exactly once) and kills/reaps the child, so the descriptor is
           ;; never leaked for the process lifetime.  A spawn failure re-raises past
           ;; this to the outer guard, and posix-spawnp/pipe has already closed both
           ;; pipe ends there.
           (let-values ([(acc get-acc) (open-bytevector-output-port)])
             (guard (e [#t (finish-spawn pid rfd get-acc 'deadline)])
               (let ([fl (posix-fcntl rfd F_GETFL)])
                 (posix-fcntl rfd F_SETFL (bitwise-ior fl O_NONBLOCK)))
               (let ([t0 (current-time 'time-monotonic)])
                 (let loop ([total 0])
                   (let ([remaining (- deadline-ms
                                       (elapsed-milliseconds
                                         t0 (current-time 'time-monotonic)))])
                     (if (<= remaining 0)
                         (finish-spawn pid rfd get-acc 'deadline)
                         (case (fd-wait-readable rfd remaining)
                           [(timeout)     (finish-spawn pid rfd get-acc 'deadline)]
                           [(interrupted) (loop total)]
                           [(ready)
                            (let ([chunk (guard (e [#t #f]) (posix-read rfd 4096))])
                              (cond
                                [(not chunk) (loop total)]     ; spurious EAGAIN -> re-poll
                                [(zero? (bytevector-length chunk))
                                 (finish-spawn pid rfd get-acc 'eof)]  ; EOF -> child done
                                [else
                                 (put-bytevector acc chunk)
                                 (let ([n (+ total (bytevector-length chunk))])
                                   ;; Past the ceiling THIS CALLER asked for, stop
                                   ;; reading: kill and reap the child (it is still
                                   ;; writing) and hand back what was captured.  Not
                                   ;; a timeout -- the wall clock had not run out --
                                   ;; so a caller holding a usable prefix keeps it.
                                   (if (and max-bytes (>= n max-bytes))
                                       (finish-spawn pid rfd get-acc 'ceiling)
                                       (loop n)))]))]))))))))))]))

  ;; with-spawn-timeout: the two-value view of the collector above, keeping the
  ;; shipped ("what did the child print, and was it cut short?") contract for every
  ;; caller that has no use for an exit status.  The 3-arg form is the shipped
  ;; disposition: it discards the child's stderr to /dev/null (posix-spawnp/pipe
  ;; only dup2's stdout, so an unredirected "fatal: not a git repository" would
  ;; scroll above the prompt) and hands the child an empty environment.  That is
  ;; what the git segment probe and every existing caller rely on, kept
  ;; byte-for-byte by delegating with the discard mode and no environment.  The
  ;; 5-arg form passes the stderr disposition and environment through.  Neither
  ;; form takes a byte ceiling: a two-value caller cannot tell a capped read from
  ;; a timed-out one, so it is handed neither -- the deadline is its only bound,
  ;; as it always was.  Returns (values output-string timed-out?).
  (define with-spawn-timeout
    (case-lambda
      [(program argv deadline-ms)
       (with-spawn-timeout program argv deadline-ms 'discard #f)]
      [(program argv deadline-ms stderr-mode env-list)
       (let-values ([(out timed-out? exit-status)
                     (with-spawn-timeout/status program argv deadline-ms
                                                stderr-mode env-list)])
         (values out timed-out?))]))

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

  ;; Render the git-state segment from a list of `git status --porcelain=v2
  ;; --branch` LINES: the branch name (or "@" + a 7-char short oid when detached),
  ;; "*" when the working tree is dirty and "+" when it has staged changes, then
  ;; "⇡N"/"⇣M" ahead/behind counts.  Empty for no lines or a headless parse -- a
  ;; quiet, escape-free "".  The branch head is stripped of control bytes before
  ;; display, since a repository name is attacker-controlled, and the detached oid
  ;; is sliced to seven hex characters.  Colour is 256-colour SGR -- green
  ;; (index 2) when clean, yellow (index 3) when dirty or staged -- emitted only
  ;; when the injectable colour verdict is true; a false verdict yields the same
  ;; glyphs as plain text.  The up/down arrows are written as hex escapes so the
  ;; source is unambiguous about the exact code points (U+21E1, U+21E3), both
  ;; display-width 1.  This is the shared render body: prompt-git-segment applies
  ;; it to the blocking collector's lines and the bounded cache path applies it to
  ;; the timed collector's split output, so both emit a byte-identical segment.
  (define (render-git-lines lines)
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
                    body))))))

  ;; Build the git-state segment from one blocking `git status --porcelain=v2
  ;; --branch` spawn -- the shared render body over the existing collector.  Its
  ;; observable output is unchanged; the bounded, cached path (cached-git-segment)
  ;; is a separate entry that reuses render-git-lines.
  (define (prompt-git-segment)
    (render-git-lines (git-status-porcelain-v2)))

  ;; === Per-cwd git segment cache ===

  ;; A cwd-string -> (stamp . rendered-string) table.  The stamp is the (HEAD
  ;; mtime, index mtime) pair, so an unchanged repository is served from here
  ;; instead of re-spawning `git status` on every prompt draw.  A modest cap keeps
  ;; a long session (few repositories) bounded; on overflow the whole table is
  ;; cleared rather than evicting one entry -- a session visits few cwds, so the
  ;; simplest bound suffices.  Per-command segments (exit, timing) are NEVER
  ;; cached -- only this per-cwd git state is.  Accepted tradeoff: a bare tracked-
  ;; file edit (echo x >> f) does not touch HEAD or index, so the segment shows
  ;; clean until the next `git add`/commit bumps a marker mtime; it self-heals and
  ;; the key is deliberately NOT widened to a worktree scan (which would defeat the
  ;; cache).
  (define git-segment-cache (make-hashtable string-hash string=?))
  (define %git-cache-cap 64)

  ;; A leaf test seam: increments once per REAL recompute (a cache miss), so a
  ;; test can prove a hit does not re-probe and an mtime bump does.
  (define git-probe-count
    (make-parameter 0
      (lambda (v)
        (unless (and (integer? v) (exact? v) (>= v 0))
          (error 'git-probe-count "expected a non-negative exact integer" v))
        v)))

  ;; The mtime (epoch seconds) of PATH, or #f when it is absent or unreadable --
  ;; posix-stat raises on ENOENT, guarded here so a missing marker is a stable #f
  ;; rather than a raise on the prompt path.
  (define (safe-mtime path)
    (guard (e [#t #f]) (stat-info-mtime (posix-stat path))))

  ;; The lexical parent of an absolute directory path, or #f at the root (so the
  ;; git-dir walk terminates).  A trailing slash is ignored; "/" and "" have no
  ;; parent.  Purely lexical -- no symlink resolution, matching git's own upward
  ;; search over the logical path.
  (define (parent-of d)
    (let* ([len (let strip ([n (string-length d)])
                  (if (and (> n 1) (char=? (string-ref d (- n 1)) #\/))
                      (strip (- n 1))
                      n))])
      (and (> len 1)
           (let loop ([i (- len 1)])
             (cond
               [(< i 0) #f]
               [(char=? (string-ref d i) #\/)
                (if (= i 0) "/" (substring d 0 i))]
               [else (loop (- i 1))])))))

  ;; Trim trailing whitespace (space and below, so CR and LF too) from a string.
  (define (trim-trailing-ws s)
    (let loop ([n (string-length s)])
      (if (and (> n 0) (char<=? (string-ref s (- n 1)) #\space))
          (loop (- n 1))
          (substring s 0 n))))

  ;; Read a ".git is a FILE" pointer: a worktree/submodule .git holds a single
  ;; "gitdir: <path>" line.  Returns <path> (relative pointers resolved against
  ;; DIR, the directory holding the .git file), or #f when the file is unreadable
  ;; or malformed.
  (define (read-gitdir-pointer path dir)
    (guard (e [#t #f])
      (let ([line (call-with-input-file path get-line)])
        (and (string? line)
             (let ([prefix "gitdir: "])
               (and (>= (string-length line) (string-length prefix))
                    (string=? (substring line 0 (string-length prefix)) prefix)
                    (let ([p (trim-trailing-ws
                               (substring line (string-length prefix)
                                          (string-length line)))])
                      (cond
                        [(= (string-length p) 0) #f]
                        [(char=? (string-ref p 0) #\/) p]        ; absolute
                        [else (string-append dir "/" p)]))))))))  ; relative to DIR

  ;; Resolve the git directory governing CWD, walking up the logical path.  At
  ;; each level "<d>/.git" is either a directory (a normal repository -> that
  ;; path), a regular file (a worktree/submodule pointer -> follow "gitdir:"), or
  ;; absent (ascend).  Returns the git-dir path or #f at the root.  The predicates
  ;; are guarded (a permission error mid-walk is treated as "not here", ascend).
  (define (resolve-git-dir cwd)
    (let walk ([d cwd])
      (and d (> (string-length d) 0)
           (let ([dot (string-append d "/.git")])
             (cond
               [(guard (e [#t #f]) (file-directory? dot)) dot]
               [(guard (e [#t #f]) (file-regular? dot))
                (let ([p (read-gitdir-pointer dot d)])
                  (or p (walk (parent-of d))))]
               [else (walk (parent-of d))])))))

  ;; The cache stamp for CWD: the two mtimes of <git-dir>/HEAD and <git-dir>/index
  ;; as a two-element list, or #f when CWD is in no repository.  Reading only
  ;; HEAD + index (the LOCKED key) means a commit/checkout/`git add` -- which bumps
  ;; one of them -- invalidates, while an unchanged repo is a stable stamp.  A
  ;; worktree's real git dir is resolved first, so a linked worktree stamps its own
  ;; per-worktree HEAD/index rather than thrashing on a missing <cwd>/.git/HEAD.
  (define (git-marker-stamp cwd)
    (let ([g (resolve-git-dir cwd)])
      (and g (list (safe-mtime (string-append g "/HEAD"))
                   (safe-mtime (string-append g "/index"))))))

  ;; Split STR on newline into a line list, matching the blocking collector's
  ;; get-line semantics: the empty string yields '(), and a single trailing
  ;; newline does NOT add a spurious empty final line.
  (define (string->lines str)
    (let ([len (string-length str)])
      (let loop ([i 0] [start 0] [acc '()])
        (cond
          [(>= i len)
           (reverse (if (> len start) (cons (substring str start len) acc) acc))]
          [(char=? (string-ref str i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]
          [else (loop (+ i 1) start acc)]))))

  ;; Recompute the git segment through the bounded collector: run `git status
  ;; --porcelain=v2 --branch` under with-spawn-timeout at the current deadline.
  ;; On a timeout return "" (the sentinel cached against the current stamp, so a
  ;; cold repo does not re-spawn every prompt); otherwise render the captured
  ;; output's lines through the shared render body.  The argv is a fixed literal,
  ;; so there is no injection surface, and git absence is the fail-quiet "".
  (define (render-git-segment/bounded)
    (let-values ([(out timed-out?)
                  (with-spawn-timeout "git"
                    '("git" "status" "--porcelain=v2" "--branch")
                    (prompt-spawn-timeout-ms))])
      (if timed-out?
          ""
          (render-git-lines (string->lines out)))))

  ;; The cached git segment for the current logical cwd.  Compute the marker
  ;; stamp (a couple of stats); on a cache hit whose stamp is unchanged, return
  ;; the stored rendered string WITHOUT re-spawning; otherwise count a real
  ;; recompute, run the bounded collector, store (stamp . rendered) under the cwd,
  ;; and return it.  The stored value covers the empty/timed-out sentinel too, so
  ;; a repeat draw at the same stamp is always a hit.  On overflow the table is
  ;; cleared before the insert.
  (define (cached-git-segment)
    (let* ([cwd   (prompt-logical-cwd)]
           [stamp (git-marker-stamp cwd)]
           [hit   (hashtable-ref git-segment-cache cwd #f)])
      (if (and hit (equal? (car hit) stamp))
          (cdr hit)
          (begin
            (git-probe-count (+ (git-probe-count) 1))
            (let ([rendered (render-git-segment/bounded)])
              (when (>= (hashtable-size git-segment-cache) %git-cache-cap)
                (hashtable-clear! git-segment-cache))
              (hashtable-set! git-segment-cache cwd (cons stamp rendered))
              rendered)))))

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

  ;; === Environment switches ===

  ;; #t unless NAME is set to a falsy spelling -- the empty string, 0, false or
  ;; no, matched case-insensitively.  Unset counts as ON, so a default-on switch
  ;; needs no environment variable at all and HAFOD_FOO=0 turns it off rather than
  ;; silently on.
  ;;
  ;; This is the SINGLE definition of "a falsy HAFOD_* switch".  The set of
  ;; accepted spellings had been written out once per gate, and every user-facing
  ;; kill switch that grows a copy is one a later addition to that set ("off",
  ;; "disabled", "n") will miss -- leaving a documented way to turn one thing off
  ;; that works for its neighbour and not for it.  Every gate below calls this.
  (define (env-switch-on? name)
    (let ([v (getenv name)])
      (or (not v)
          (not (member (string-downcase v) '("" "0" "false" "no"))))))

  ;; #t only when NAME is set AND not to a falsy spelling: the inverted reading,
  ;; for a default-OFF switch that an environment variable turns on.  Same
  ;; spellings, same single definition.
  (define (env-switch-explicitly-on? name)
    (and (getenv name) (env-switch-on? name) #t))

  ;; === Prompt segment model ===
  ;;
  ;; A segment is a bare pair (placement . thunk) -- no record boilerplate.  The
  ;; placement is one of the symbols left / right / line; the thunk takes the
  ;; per-draw context and returns a rendered string or #f (skip).  The ordered
  ;; list lives in the prompt-segments parameter: register appends (so registration
  ;; order is draw order) and clear empties it.  Configuration is Scheme -- a
  ;; parameter and two procedures -- not a theme DSL.

  ;; A placement is one of the three layout slots.
  (define (valid-placement? p) (memq p '(left right line)))

  ;; A segment is a (placement . thunk) pair.
  (define (valid-segment? s)
    (and (pair? s) (valid-placement? (car s)) (procedure? (cdr s))))

  ;; The ordered segment list.  The validator holds the whole value to the
  ;; (placement . thunk) shape, so a malformed set is rejected at the parameter
  ;; boundary rather than corrupting a later draw -- mirroring the hook validators.
  (define prompt-segments
    (make-parameter '()
      (lambda (v)
        (unless (and (list? v) (for-all valid-segment? v))
          (error 'prompt-segments
                 "expected a list of (placement . thunk) pairs" v))
        ;; A segment-list mutation while the guard is armed is a user composing the
        ;; prompt in init.ss -- through register-prompt-segment! / clear-prompt-
        ;; segments! or a direct set -- so mark the prompt customised exactly as the
        ;; hook converters do.  The REPL-entry default-on then skips rather than
        ;; wiping the user's segments (its leading clear-prompt-segments! would
        ;; otherwise discard them).  The default-on / preset install sets segments
        ;; with the guard DISARMED at its call site, so its own composition does not
        ;; self-mark.
        (when %prompt-guard-armed? (prompt-customised? #t))
        v)))

  ;; Append a segment, so registration order is draw order.  Placement and thunk
  ;; are validated here too, for a clear error at the call site.
  (define (register-prompt-segment! placement thunk)
    (unless (valid-placement? placement)
      (error 'register-prompt-segment!
             "placement must be left, right, or line" placement))
    (unless (procedure? thunk)
      (error 'register-prompt-segment! "thunk must be a procedure" thunk))
    (prompt-segments (append (prompt-segments) (list (cons placement thunk)))))

  ;; Empty the segment list.
  (define (clear-prompt-segments!) (prompt-segments '()))

  ;; The per-draw context, built once per prompt draw and handed to every segment
  ;; thunk.  A small nongenerative record (house style) -- the "no boilerplate"
  ;; rule is about the SEGMENTS, which stay bare pairs; the shared draw state is a
  ;; named record so a thunk reads a field rather than re-probing the live state.
  (define-record-type prompt-ctx
    (nongenerative)
    (fields exit duration cwd colour-depth glyph-tier width))

  ;; Capture the live draw state into a fresh context: the last command's exit and
  ;; duration, the logical cwd, the fd-1 colour depth and the glyph tier (so a
  ;; segment picks colours/glyphs by tier), and the width budget -- the whole
  ;; terminal at the head of a row, narrowed per segment by segment-outputs below.
  (define (make-current-prompt-ctx)
    (make-prompt-ctx (last-status) (last-duration) (prompt-logical-cwd)
                     (colour-depth 1) (glyph-tier) (terminal-width)))

  ;; The same context with a different width budget, or CTX itself when the budget
  ;; has not moved (the common case: no earlier segment on this row).
  (define (prompt-ctx-at-width ctx width)
    (if (eqv? width (prompt-ctx-width ctx))
        ctx
        (make-prompt-ctx (prompt-ctx-exit ctx) (prompt-ctx-duration ctx)
                         (prompt-ctx-cwd ctx) (prompt-ctx-colour-depth ctx)
                         (prompt-ctx-glyph-tier ctx) width)))

  ;; The non-#f, non-empty rendered strings of the segments at PLACEMENT, in
  ;; registration order.  A thunk returning #f or "" (or a non-string) is dropped,
  ;; so a hidden segment leaves no stray separator; a thunk that RAISES is dropped
  ;; the same way, so a broken user segment degrades to a skip rather than crashing
  ;; the prompt draw.
  ;;
  ;; The LEFT segments space-join onto ONE row, so each is handed the columns still
  ;; FREE on that row rather than the whole terminal: the width in the context is a
  ;; budget, and a segment that budgets against the whole line while sharing it
  ;; with others is not budgeted at all.  The group of language versions was the
  ;; case in point -- half of eighty columns, drawn after a forty-column path and a
  ;; whole branch name, still wrapped the row onto a second one and broke the right
  ;; prompt's single-row column positioning.  Each rendered segment costs its
  ;; visible columns (SGR-free, wide glyphs counted as two) plus the joining space.
  ;; The line placement gives every segment its own row and the right placement is
  ;; positioned from the far edge, so neither narrows.
  (define (segment-outputs placement ctx)
    (let ([narrow? (eq? placement 'left)])
      (let loop ([segs (prompt-segments)] [free (prompt-ctx-width ctx)] [acc '()])
        (cond
          [(null? segs) (reverse acc)]
          [(eq? (caar segs) placement)
           ;; The thunk is a public extension point (register-prompt-segment!), so it
           ;; may be arbitrary user code -- isolate each call in a guard: a raise is
           ;; treated as a skip (#f), never propagating out of the draw to the REPL,
           ;; where it would escape the eval guards and livelock the prompt redraw.
           (let ([out (guard (e [#t #f])
                        ((cdar segs) (if narrow? (prompt-ctx-at-width ctx free) ctx)))])
             (if (and (string? out) (not (string=? out "")))
                 (loop (cdr segs)
                       (if narrow?
                           (max 0 (- free (ansi-visible-length out) 1))
                           free)
                       (cons out acc))
                 (loop (cdr segs) free acc)))]
          [else (loop (cdr segs) free acc)]))))

  ;; Render the left and line segments to PORT for one CTX: the left segments
  ;; space-join into the info line (written first), then each line segment breaks
  ;; onto its own row (a newline then its output).  The break before the FIRST line
  ;; segment is suppressed when there is no left/info content, so a line-only prompt
  ;; opens on the current row rather than a spurious blank line.  With a single line
  ;; segment -- the input glyph -- above a non-empty info line this is the two-line
  ;; "info...\n<glyph> " shape; the editor's read-expression already splits a
  ;; multi-line prompt on the last newline, so no cursor-maths change is needed here.
  (define (render-prompt-segments port ctx)
    (let ([left (join-nonempty-space (segment-outputs 'left ctx))]
          [lines (segment-outputs 'line ctx)])
      (display left port)
      (let loop ([ls lines] [first? #t])
        (unless (null? ls)
          ;; Emit the leading newline only when there is content above this segment:
          ;; the info line for the first segment (skip the newline when it is empty),
          ;; or a previous line segment for the rest.
          (when (or (not first?) (not (string=? left "")))
            (newline port))
          (display (car ls) port)
          (loop (cdr ls) #f)))))

  ;; Render the right segments to PORT for one CTX, space-joined -- the text
  ;; display-right-prompt positions at the right edge.
  (define (render-right-prompt-segments port ctx)
    (display (join-nonempty-space (segment-outputs 'right ctx)) port))

  ;; The exit-coloured input glyph -- a `line` placement segment, so it renders on
  ;; its own row below the info segments (the two-line default; read-expression
  ;; already splits a multi-line prompt on the last newline, so no editor change is
  ;; needed).  The glyph is the heavy right angle ❯ (U+276F, written as the hex
  ;; escape matching the \x2718;/\x21e1; convention above) under the emoji tier and
  ;; '>' under the ascii tier -- a Nerd Font is never assumed.  It is coloured by
  ;; the LAST command's exit: the green 256-colour SGR (index 2) after a success,
  ;; the red (index 1) after a failure, gated on the injectable colour verdict
  ;; exactly as the git and exit segments are -- a false (mono) verdict emits the
  ;; glyph plain, with no SGR.  A trailing space always follows so the cursor sits
  ;; after "❯ ".  Symbol and colours stay overridable because the segment is just a
  ;; thunk the user may replace in the list.
  (define (prompt-char-segment ctx)
    (let ([glyph (if (eq? (prompt-ctx-glyph-tier ctx) 'ascii) ">" "\x276f;")]
          [ok?   (zero? (prompt-ctx-exit ctx))])
      (if ((prompt-colour-ok?))
          (string-append (if ok? "\x1b;[38;5;2m" "\x1b;[38;5;1m")
                         glyph
                         "\x1b;[39m ")
          (string-append glyph " "))))

  ;; === Per-language version segments ===
  ;;
  ;; A version segment reports a project's toolchain version -- "via <glyph> vX"
  ;; -- for a language detected in the current directory.  The subsystem is data
  ;; first: a tool is described by a prompt-tool record and the ordered set of
  ;; tools lives in the prompt-tools registry.  What follows, in order, is the
  ;; descriptor and its registry, the version-string parser and the built-in tool
  ;; set, the glyph choice and one-tool renderer, the current-directory marker
  ;; detection and the PATH gate, and finally the per-cwd cache, the bounded
  ;; probe and the group segment the preset registers.

  ;; A tool descriptor.  markers is a (files . exts) pair: a list of marker
  ;; filenames plus an optional list of marker extensions -- the starship
  ;; detect_files / detect_extensions split -- both consulted in the current
  ;; directory with no walk-up.  command and args name the version probe (a fixed
  ;; literal argv, never a shell string); parse maps that probe's output to a
  ;; version string or #f.  emoji is the default glyph and ascii the short
  ;; fallback on a weak glyph terminal; nerd (optional, #f) is a Nerd-Font glyph
  ;; consulted only under an explicit opt-in.  A nongenerative record, the house
  ;; style shared with prompt-ctx above.
  ;;
  ;; command is EITHER a binary name OR a non-empty list of candidate binary names
  ;; tried in order: the first candidate that resolves on PATH gates the tool and is
  ;; substituted as argv[0] of the fixed literal argv, the descriptor's remaining
  ;; arguments being kept.  A single name is passed through untouched, argv[0]
  ;; included, so a descriptor that deliberately invokes a multi-call binary under
  ;; an applet name keeps doing so.  The candidate form is what lets ONE descriptor
  ;; cover a language whose interpreter is spelled differently from one
  ;; distribution to the next; it needs no extra field, because the notion lives
  ;; entirely in the shape this one accepts.
  (define-record-type prompt-tool
    (nongenerative)
    (fields name markers command args parse emoji ascii nerd))

  ;; --- Version-string parsing (canned-string tested, no live tool) ---
  ;;
  ;; Pull the version token -- a dotted-decimal run [0-9]+(.[0-9]+)+ -- out of
  ;; arbitrary --version output, or #f when there is none.  A single
  ;; left-to-right scan, no regex engine: the same hand-scan house style as
  ;; sanitize-control / ansi-visible-length above.
  ;;
  ;; The scan is LINE ANCHORED and requires a MINOR component: the first LINE
  ;; carrying a run of at least two dot-separated components wins, and a bare
  ;; integer is stepped over rather than taken as a version.  Both halves are
  ;; load-bearing, because the probe merges the child's stderr into its stdout
  ;; and a piped child's stderr is unbuffered while its stdout is block
  ;; buffered -- so whatever a toolchain prints BEFORE its banner normally
  ;; reaches the pipe first.  A rule of "the first digit run anywhere" reports
  ;; the JVM's own "Picked up JAVA_TOOL_OPTIONS: -Xmx2048m" as v2048 and a PHP
  ;; startup warning's "on line 0" as v0; both are bare integers, so both are
  ;; stepped over here and the banner on the following line wins.  (A shim
  ;; ERROR that does quote a whole version -- pyenv's "version `3.12.1' is not
  ;; installed" -- is not a parsing problem: the child exits non-zero and
  ;; probe-tool-version discards its output before it ever reaches this scan.)
  ;;
  ;; Within the winning line the scan walks to the first digit -- stepping over
  ;; a label word ("Python "), a quote (openjdk's "21.0.2"), a leading `v`
  ;; (node's v20.11.1) or a command prefix (go's go1.22.1), each just a
  ;; non-digit character -- and reads the maximal run of digits and interior
  ;; dots from there.  A mis-shaped or empty banner yields #f, so a tool
  ;; contributes no segment rather than crashing the prompt.
  (define (parse-version/common out)
    (let ([len (string-length out)])
      (define (digit? c) (and (char>=? c #\0) (char<=? c #\9)))
      (define (eol? c) (or (char=? c #\newline) (char=? c #\return)))
      ;; The end of the dotted run beginning at the digit at START: consume the
      ;; leading digits, then each `.<digit>` group, stopping before a trailing
      ;; dot (a dot not followed by a digit), so "1." yields "1" and "1.77.0)"
      ;; yields "1.77.0".  A newline is neither, so a run never crosses a line.
      (define (run-end start)
        (let loop ([i start])
          (cond
            [(and (< i len) (digit? (string-ref out i))) (loop (+ i 1))]
            [(and (< (+ i 1) len)
                  (char=? (string-ref out i) #\.)
                  (digit? (string-ref out (+ i 1))))
             (loop (+ i 1))]
            [else i])))
      ;; #t when the run [START,END) carries an interior dot -- i.e. it has a
      ;; minor component.  A bare integer (a heap size, a line number, a year)
      ;; is not a version.
      (define (has-minor? start end)
        (let loop ([i start])
          (cond
            [(>= i end) #f]
            [(char=? (string-ref out i) #\.) #t]
            [else (loop (+ i 1))])))
      ;; The index one past the end of the line beginning at START.
      (define (line-end start)
        (let loop ([i start])
          (if (or (>= i len) (eol? (string-ref out i))) i (loop (+ i 1)))))
      ;; The first dotted-with-minor run inside [START,STOP), or #f.
      (define (scan-line i stop)
        (cond
          [(>= i stop) #f]
          [(digit? (string-ref out i))
           (let ([end (run-end i)])
             (if (has-minor? i end) (substring out i end) (scan-line end stop)))]
          [else (scan-line (+ i 1) stop)]))
      (let scan-lines ([i 0])
        (and (< i len)
             (let ([stop (line-end i)])
               (or (scan-line i stop)
                   (scan-lines (+ stop 1))))))))

  ;; The per-tool parse procedures.  Every built-in banner -- a labelled line
  ;; ("Python 3.13.14", "PHP 8.3.3 ..."), a bare number ("1.0.30", "0.11.0"), a
  ;; leading-v line ("v20.11.1"), a second-token line ("rustc 1.77.0 ...",
  ;; "ruby 3.3.0 ..."), the quoted java line ('openjdk version "21.0.2"') and the
  ;; go-prefixed token ("go1.22.1") -- reduces to "the first dotted-decimal run",
  ;; so each delegates to the shared helper.  They stay separate named
  ;; procedures so a tool whose format later diverges can gain a bespoke rule
  ;; without touching the others or the descriptors that name them.
  (define (parse-python out) (parse-version/common out))
  (define (parse-node out)   (parse-version/common out))
  (define (parse-rust out)   (parse-version/common out))
  (define (parse-go out)     (parse-version/common out))
  (define (parse-bun out)    (parse-version/common out))
  (define (parse-ruby out)   (parse-version/common out))
  (define (parse-java out)   (parse-version/common out))
  (define (parse-deno out)   (parse-version/common out))
  (define (parse-php out)    (parse-version/common out))
  (define (parse-zig out)    (parse-version/common out))

  ;; The built-in tool set seeds the registry's DEFAULT value: the shipped ten
  ;; languages in a fixed registration order (= draw order) -- python, node,
  ;; rust, go, bun, ruby, java, deno, php, zig.  Seeding them as the parameter
  ;; default, rather than registering them at load time, avoids a load-time side
  ;; effect and honours the R6RS definitions-before-expressions rule; a user's
  ;; register-prompt-tool! then appends after these.  Each descriptor carries its
  ;; marker (files . exts) pair (consulted in the current directory only, no
  ;; walk-up), the fixed literal version argv (never a shell string), its parse
  ;; rule, an emoji glyph (the default, a single-scalar hex escape matching the
  ;; \x276f; convention) and a short ascii mnemonic (the fallback on a weak glyph
  ;; terminal).  go and java take a bare subcommand / -version rather than
  ;; --version; java prints its banner to stderr, which the version probe captures
  ;; by merging the child's streams.
  ;;
  ;; The last field is the optional Nerd-Font glyph, consulted only under the
  ;; nerd-glyphs? opt-in.  The seven languages with a long-established devicon in
  ;; the Nerd Font private use area carry theirs; bun, deno and zig have no settled
  ;; code point, so they stay #f and fall back to their emoji under the opt-in
  ;; rather than being given a guess that would render as a blank box.  Shipping
  ;; the seven is what makes the opt-in mean something out of the box: with every
  ;; descriptor at #f, (nerd-glyphs? #t) changed nothing at all.
  ;;
  ;; python is the one descriptor whose command is a CANDIDATE LIST rather than a
  ;; single name.  Most Linux distributions and macOS follow PEP 394 and ship only
  ;; the version-suffixed interpreter, so the unsuffixed name alone left the
  ;; commonest language segment of the ten invisible on the commonest layout --
  ;; every pyproject.toml directory detected the tool and the PATH gate then
  ;; correctly declined it, unless an environment happened to be activated.  The
  ;; UNSUFFIXED name is tried FIRST because an activated virtualenv or conda
  ;; environment always provides it and that is the interpreter the user is
  ;; actually developing against; the suffixed name is the fallback for a bare
  ;; system install.  Its argv is unchanged, because the resolved winner is
  ;; substituted as argv[0].
  (define %builtin-prompt-tools
    (list
      (make-prompt-tool "python"
        '(("pyproject.toml" "setup.py" "requirements.txt" "Pipfile"
           ".python-version" "tox.ini" "setup.cfg") . (".py"))
        '("python" "python3") '("python" "--version")
        parse-python "\x1f40d;" "py" "\xe73c;")
      (make-prompt-tool "node"
        '(("package.json" ".nvmrc" ".node-version") . (".js" ".mjs" ".cjs" ".ts"))
        "node" '("node" "--version") parse-node "\x2b22;" "node" "\xe718;")
      (make-prompt-tool "rust"
        '(("Cargo.toml" "Cargo.lock") . (".rs"))
        "rustc" '("rustc" "--version") parse-rust "\x1f980;" "rs" "\xe7a8;")
      (make-prompt-tool "go"
        '(("go.mod" "go.sum") . (".go"))
        "go" '("go" "version") parse-go "\x1f439;" "go" "\xe627;")
      (make-prompt-tool "bun"
        '(("bun.lockb" "bunfig.toml") . ())
        "bun" '("bun" "--version") parse-bun "\x1f95f;" "bun" #f)
      (make-prompt-tool "ruby"
        '(("Gemfile" "Gemfile.lock" ".ruby-version" "Rakefile" ".rubocop.yml") . (".rb"))
        "ruby" '("ruby" "--version") parse-ruby "\x1f48e;" "rb" "\xe739;")
      (make-prompt-tool "java"
        '(("pom.xml" "build.gradle" "build.gradle.kts" ".java-version" "deps.edn")
          . (".java" ".gradle"))
        "java" '("java" "-version") parse-java "\x2615;" "java" "\xe738;")
      (make-prompt-tool "deno"
        '(("deno.json" "deno.jsonc" "deno.lock" "mod.ts" "deps.ts") . ())
        "deno" '("deno" "--version") parse-deno "\x1f995;" "deno" #f)
      (make-prompt-tool "php"
        '(("composer.json" "composer.lock" ".php-version") . (".php"))
        "php" '("php" "--version") parse-php "\x1f418;" "php" "\xe73d;")
      (make-prompt-tool "zig"
        '(("build.zig" "build.zig.zon") . (".zig"))
        "zig" '("zig" "version") parse-zig "\x26a1;" "zig" #f)))

  ;; The ordered tool registry: the built-ins first (the default above), user
  ;; tools appended after, so registration order is draw order.  A parameter,
  ;; mirroring prompt-segments, so an init.ss can inspect or reset it.  The
  ;; validator holds the whole value to a list of prompt-tool records, rejecting a
  ;; malformed set at the parameter boundary.  Unlike prompt-segments it does NOT
  ;; mark the prompt customised -- this is the tool registry, not the user's
  ;; segment composition.
  (define prompt-tools
    (make-parameter %builtin-prompt-tools
      (lambda (v)
        (unless (and (list? v) (for-all prompt-tool? v))
          (error 'prompt-tools "expected a list of prompt-tool records" v))
        v)))

  ;; Register a tool, appended so registration order stays draw order.  Positional
  ;; only: Chez has no keyword objects and hafod grows no config DSL, so a tool is
  ;; added by handing its fields as Scheme values (scsh-not-POSIX).  The seven-
  ;; argument form supplies no Nerd-Font glyph (nerd = #f); the eight-argument form
  ;; carries one.
  ;;
  ;; EVERY field is checked here, not just parse, because every one of them is
  ;; dereferenced structurally further down and a bad shape there raises inside a
  ;; prompt draw, where the segment machinery's blanket guard swallows it: the
  ;; whole version group -- including all ten working built-ins -- would simply
  ;; vanish with no diagnostic.  The commonest slip is handing markers a bare list
  ;; of filenames instead of a (filenames . extensions) pair, which the detector
  ;; then reads as a string where a list belongs.  A raise at the point of the
  ;; mistake, naming the field, is worth far more than a silently missing prompt
  ;; segment an hour later.
  ;;
  ;; The command field is checked the same way but by a different means: a
  ;; candidate list is dereferenced structurally both by the PATH gate and by the
  ;; argv builder, and the natural slip is handing a bare symbol or an empty list.
  ;; The check asks the SAME normaliser the gate does -- "does this yield at least
  ;; one candidate?" -- rather than restating the shape rule, so "the boundary
  ;; accepted it" and "the gate can resolve it" cannot come apart.
  (define register-prompt-tool!
    (case-lambda
      [(name markers command args parse emoji ascii)
       (register-prompt-tool! name markers command args parse emoji ascii #f)]
      [(name markers command args parse emoji ascii nerd)
       (unless (procedure? parse)
         (error 'register-prompt-tool! "parse must be a procedure" parse))
       (unless (and (pair? markers)
                    (list? (car markers)) (for-all string? (car markers))
                    (list? (cdr markers)) (for-all string? (cdr markers)))
         (error 'register-prompt-tool!
                "markers must be a (filenames . extensions) pair of string lists"
                markers))
       (unless (and (list? args) (pair? args) (for-all string? args))
         (error 'register-prompt-tool!
                "args must be a non-empty list of strings" args))
       (unless (pair? (tool-command-candidates command))
         (error 'register-prompt-tool!
                "command must be a binary name or a non-empty list of candidate binary names"
                command))
       (unless (and (string? name)
                    (string? emoji) (string? ascii)
                    (or (not nerd) (string? nerd)))
         (error 'register-prompt-tool!
                "name, emoji and ascii must be strings and nerd a string or #f"
                name))
       (invalidate-version-cache!)
       (prompt-tools
         (append (prompt-tools)
                 (list (make-prompt-tool name markers command args
                                         parse emoji ascii nerd))))]))

  ;; Empty the tool registry (mirrors clear-prompt-segments!).
  (define (clear-prompt-tools!)
    (invalidate-version-cache!)
    (prompt-tools '()))

  ;; Drop the tool called NAME, keeping the rest in registry order; a no-op when
  ;; no tool carries that name, since removing what is not there is already the
  ;; state the caller asked for.  The named counterpart of register-prompt-tool!,
  ;; and what makes the registry a usable public surface: "drop the java segment,
  ;; keep the other nine" is the obvious edit, and doing it through the bare
  ;; parameter would mean exporting the record accessors so a caller could filter
  ;; the list by hand.
  (define (unregister-prompt-tool! name)
    (unless (string? name)
      (error 'unregister-prompt-tool! "expected a tool name string" name))
    (invalidate-version-cache!)
    (prompt-tools
      (filter (lambda (t) (not (string=? (prompt-tool-name t) name)))
              (prompt-tools))))

  ;; --- Glyph choice, the Nerd-Font opt-in and the one-tool renderer ---
  ;;
  ;; A Nerd Font is NEVER assumed.  nerd-glyphs? is the explicit opt-in: a tool's
  ;; richer nerd glyph is shown ONLY when this is on AND the descriptor carries
  ;; one.  The default is #f, so an out-of-the-box draw shows the emoji glyph (or
  ;; the ascii mnemonic on a weak glyph terminal), never a nerd glyph.  It is a
  ;; plain parameter -- a user opts in from their init with (nerd-glyphs? #t),
  ;; config being Scheme (scsh-not-POSIX), and no env var flips the default, so
  ;; the "never assumed" default holds unconditionally.
  (define nerd-glyphs? (make-parameter #f))

  ;; The glyph for a tool at the current glyph tier.  The ASCII tier is checked
  ;; FIRST, ahead of the Nerd-Font opt-in.  That tier is not a preference: it is
  ;; returned for TERM=linux, TERM=dumb, HAFOD_ASCII or an explicit override --
  ;; terminals known not to render non-ASCII at all.  A nerd glyph is a
  ;; private-use-area code point, so honouring the opt-in there would emit
  ;; mojibake on exactly the terminals the fallback exists to protect.  Below the
  ;; ascii tier the opt-in wins when the descriptor supplies a nerd glyph, else
  ;; the emoji default.  So emoji is the out-of-the-box glyph, ascii the automatic
  ;; fallback on a poor glyph terminal (the glyph-tier verdict
  ;; make-current-prompt-ctx caches), and nerd strictly opt-in.
  (define (tool-glyph t glyph-tier)
    (cond
      [(eq? glyph-tier 'ascii)                   (prompt-tool-ascii t)]
      [(and (nerd-glyphs?) (prompt-tool-nerd t)) (prompt-tool-nerd t)]
      [else                                      (prompt-tool-emoji t)]))

  ;; Render one detected tool as a starship-style segment -- "via <glyph> vX" --
  ;; choosing the glyph by the tier and colouring safely.  The version string is
  ;; process-controlled and the glyph descriptor is repo-controlled, so BOTH pass
  ;; through sanitize-control before assembly: a hostile marker, or a shim
  ;; printing ESC/CR/LF, cannot move the cursor or spoof the prompt.  Colour is a
  ;; single soft 256-colour SGR emitted ONLY when the colour depth is non-mono
  ;; AND the injectable fd-1 verdict is true -- the same discipline as the git and
  ;; exit segments: it gates on prompt-colour-ok? (fd 1 by default), never on the
  ;; string capture port the prompt is drawn into, so a mono terminal (or a
  ;; capture sink) gets the plain body with no SGR.  The layout is identical
  ;; across tiers -- only the glyph swaps -- so the fallback is invisible bar the
  ;; glyph.  GLYPH-TIER and COLOUR-DEPTH are the verdicts the per-draw context
  ;; caches, handed in rather than re-derived.
  (define (render-one-tool t glyph-tier colour-depth version)
    (let ([body (string-append "via "
                               (sanitize-control (tool-glyph t glyph-tier))
                               " v"
                               (sanitize-control version))])
      (if (and (not (eq? colour-depth 'mono)) ((prompt-colour-ok?)))
          (string-append "\x1b;[38;5;66m" body "\x1b;[39m")
          body)))

  ;; --- Marker detection (current directory only) and the PATH gate ---
  ;;
  ;; Detection is one guarded directory-list of the cwd matched against the union
  ;; of every registered tool's markers -- the starship detect_files /
  ;; detect_extensions split -- in the CURRENT directory only, with NO walk-up.
  ;; (resolve-git-dir above ASCENDS because a git dir governs a whole subtree; a
  ;; language marker does not, so detection must not copy that ascent.)  A detected
  ;; tool is then declined by the PATH gate unless its command is installed --
  ;; BEFORE any probe is spawned -- so an unrelated or toolless directory spawns
  ;; nothing.

  ;; The lowercased extension of NAME, including the leading dot (".py"), or "" for
  ;; a name with no extension.  A leading-dot dotfile (".nvmrc", ".python-version")
  ;; has no extension -- it is a marker FILENAME, matched by name -- so the scan
  ;; stops before index 1 and yields "".
  (define (file-ext name)
    (let ([n (string-length name)])
      (let loop ([i (- n 1)])
        (cond
          [(< i 1) ""]
          [(char=? (string-ref name i) #\.) (string-downcase (substring name i n))]
          [else (loop (- i 1))]))))

  ;; One guarded directory-list of CWD as a string set (filename -> #t) for O(1)
  ;; membership, EMPTY on an absent or unreadable directory (directory-list raises
  ;; on ENOENT/EACCES) so detection never raises out of a prompt draw.  "." and
  ;; ".." are not listed.  Read once; the extension set is derived from the same
  ;; entries, so detection is a single directory read.
  (define (dir-entry-set cwd)
    (let ([ht (make-hashtable string-hash string=?)])
      (guard (e [#t (void)])
        (for-each (lambda (name) (hashtable-set! ht name #t))
                  (directory-list cwd)))
      ht))

  ;; The lowercased extensions present among ENTRIES (a filename set), as a string
  ;; set, so a tool's extension markers match in O(1).  Derived from the SAME
  ;; entries the directory-list produced -- no second directory read.
  (define (ext-set-of entries)
    (let ([ht (make-hashtable string-hash string=?)])
      (vector-for-each
        (lambda (name)
          (let ([ext (file-ext name)])
            (unless (string=? ext "") (hashtable-set! ht ext #t))))
        (hashtable-keys entries))
      ht))

  ;; The registered tools whose markers are present in CWD, in registry (= draw)
  ;; order.  A tool matches when ANY of its marker filenames is an entry of CWD OR
  ;; ANY of its marker extensions is present among those entries -- reading the
  ;; (files . exts) pair through the record accessor.  One directory-list then
  ;; O(entries + tools) matching: no per-tool stat, no walk-up, no spawn.  filter
  ;; over (prompt-tools) preserves registration order.
  ;;
  ;; The match is guarded PER TOOL.  register-prompt-tool! validates the marker
  ;; shape, but the registry is a plain parameter a user (or a parameterize) may
  ;; set directly, so a malformed descriptor can still reach here -- and the match
  ;; dereferences the pair structurally.  Unguarded, that raise escapes the whole
  ;; segment and the prompt loses every working tool along with the broken one; a
  ;; per-tool guard costs the broken descriptor its own segment and nothing else.
  (define (detect-tools-in cwd)
    (let* ([entries (dir-entry-set cwd)]
           [exts    (ext-set-of entries)])
      (filter
        (lambda (t)
          (guard (e [#t #f])
            (let ([markers (prompt-tool-markers t)])
              ;; Both halves are checked for shape BEFORE the match is attempted,
              ;; because `exists` short-circuits: a marker list whose FIRST entry
              ;; is a valid, present filename was detected whatever followed it,
              ;; and the half-shaped remainder then reached the marker STAMP --
              ;; which walks every entry and appends each to the cwd -- outside
              ;; this guard entirely.  Refusing to detect a descriptor that is not
              ;; wholly well-formed is what makes "its own segment and nothing
              ;; else" true rather than merely intended.
              (and (for-all string? (car markers))
                   (for-all string? (cdr markers))
                   (or (exists (lambda (f) (hashtable-contains? entries f))
                               (car markers))
                       (exists (lambda (x) (hashtable-contains? exts (string-downcase x)))
                               (cdr markers)))))))
        (prompt-tools))))

  ;; The candidate binary names a COMMAND VALUE offers, in the order they are to be
  ;; tried: a string yields the one-element list carrying it, a non-empty list of
  ;; strings yields itself, and every other shape yields the empty list.
  ;;
  ;; It takes the bare VALUE rather than a descriptor, for two reasons.  Being
  ;; TOTAL -- answering "no candidates" rather than raising -- is what keeps a
  ;; malformed descriptor (one set straight into the registry parameter instead of
  ;; through the validating entry point) costing its own segment and nothing else,
  ;; matching the per-tool guards the detector and the group already use.  And
  ;; taking the value makes this the SINGLE definition of a valid command shape in
  ;; the subsystem: register-prompt-tool! enforces the boundary by asking whether
  ;; this yields at least one candidate, rather than restating the rule somewhere
  ;; the two could drift apart.  The empty list is the case that makes the point --
  ;; it satisfies list?, so it slips past a rule spelled merely "a string or a
  ;; list", yet it offers nothing to resolve and must be rejected.
  (define (tool-command-candidates c)
    (cond
      [(string? c) (list c)]
      [(and (list? c) (pair? c) (for-all string? c)) c]
      [else '()]))

  ;; The first of tool T's candidate binaries present in the classifier's
  ;; path-cache, or #f when none is.  One hashtable read per candidate, short-
  ;; circuiting on the first hit: no directory scan of $PATH, no stat and no
  ;; rebuild-path-cache! call, so a descriptor offering alternates costs at most as
  ;; many O(1) reads as it lists names.  The shipped staleness rule is unchanged --
  ;; the winner is drawn from the cache the REPL built, so a tool installed
  ;; mid-session stays invisible until that cache is rebuilt.
  (define (resolve-tool-command t)
    (let ([cache (path-cache)])
      (let loop ([cs (tool-command-candidates (prompt-tool-command t))])
        (cond
          [(null? cs) #f]
          [(hashtable-ref cache (car cs) #f) (car cs)]
          [else (loop (cdr cs))]))))

  ;; The argv to exec for tool T once WINNER has resolved.  The substitution is
  ;; CONDITIONAL on the command's shape, and the asymmetry is the whole point.
  ;;
  ;; A SINGLE-NAME command's argv is passed through VERBATIM, argv[0] included,
  ;; because such a descriptor may set argv[0] to something other than the program
  ;; deliberately: that is the multi-call-binary idiom, where a busybox-style
  ;; command is invoked under the applet name it should dispatch on -- the same
  ;; thing a shell's exec-with-a-chosen-name does.  Rewriting argv[0]
  ;; unconditionally would clobber that shipped capability without a word.
  ;;
  ;; A CANDIDATE LIST has no such convention to protect: the descriptor cannot know
  ;; which of its names will win, so substituting the winner is the only
  ;; unambiguous reading.  So argv[0] moves for a list command and ONLY for a list
  ;; command.  The remaining arguments are the descriptor's own fixed literals
  ;; either way, so a tool taking a bare subcommand (`go version`) or a single-dash
  ;; flag (`java -version`) keeps it.
  (define (tool-probe-argv t winner)
    (let ([args (prompt-tool-args t)])
      (cond
        [(string? (prompt-tool-command t)) args]
        [(pair? args) (cons winner (cdr args))]
        [else (list winner)])))

  ;; The PATH gate: #t iff ANY of the tool's candidate binaries is a key in the
  ;; classifier's path-cache (a command-name -> #t table rebuilt at REPL entry) --
  ;; an O(1) read per candidate, NO spawn and NO stat, and for a single-name
  ;; command byte for byte the shipped answer.  A marker-present-but-uninstalled
  ;; tool fails here and is never probed, so an unrelated or toolless directory
  ;; spawns nothing.  The cache is basename-presence rebuilt at REPL entry, so a
  ;; tool installed mid-session is invisible until the next rebuild -- an accepted
  ;; staleness, deliberately not a per-probe PATH scan.
  (define (tool-on-path? t)
    (and (resolve-tool-command t) #t))

  ;; --- The per-cwd version cache, the bounded probe and the group ---
  ;;
  ;; A cwd-string -> #(stamp stamped-at rendered-string) table mirroring
  ;; git-segment-cache: an unchanged directory is served from here rather than
  ;; re-probing every tool on every draw.  A stamp change invalidates; the EMPTY
  ;; group (an unrelated dir, or every tool uninstalled/failed) is cached too, so
  ;; a toolless directory never re-probes.  Capped like the git cache.  The key is
  ;; deliberately NOT widened to a spawn-derived value -- that would defeat the
  ;; cache.
  (define version-segment-cache (make-hashtable string-hash string=?))

  ;; How many directories the table holds before an insert of a NEW key clears it.
  ;; A parameter rather than a constant so a test can drive the overflow rule with
  ;; three directories instead of sixty-five; leaf-only, NOT a (hafod) umbrella
  ;; member.
  (define version-cache-cap
    (make-parameter 64
      (lambda (v)
        (unless (and (integer? v) (exact? v) (positive? v))
          (error 'version-cache-cap "expected a positive exact integer" v))
        v)))

  ;; Drop every cached group.  Called from the registry write path
  ;; (register-prompt-tool! / unregister-prompt-tool! / clear-prompt-tools!)
  ;; because those change what a directory RESOLVES to, and the stamp cannot
  ;; always see it: the stamp folds in the detected tool NAMES, which catches most
  ;; of it, but a cached entry's detected set is itself reused while the directory
  ;; is unchanged, so registering a tool at the REPL and expecting it to show up in
  ;; the directory you are standing in needs an explicit clear.  Defined here
  ;; beside the table so the registry does not reach into the cache's internals.
  (define (invalidate-version-cache!)
    (hashtable-clear! version-segment-cache))

  ;; The age at which a cached group is re-probed even with an unchanged stamp.
  ;; The stamp catches every change hafod can SEE; this catches the ones it
  ;; cannot -- a toolchain upgraded in place under an unchanged $PATH by
  ;; `apt upgrade python3` or `rustup update`, where neither a marker file nor an
  ;; environment variable moves.  A backstop, not the primary mechanism: 30 s is
  ;; long enough that a busy session still serves nearly every draw from the
  ;; cache, short enough that an upgrade shows up without a new shell.  A
  ;; parameter so a test can force expiry rather than waiting; leaf-only, NOT a
  ;; (hafod) umbrella member.
  (define version-cache-ttl-ms
    (make-parameter 30000
      (lambda (v)
        (unless (and (integer? v) (exact? v) (>= v 0))
          (error 'version-cache-ttl-ms "expected a non-negative exact integer" v))
        v)))

  ;; The environment variables a toolchain switch moves.  Every one is a plain
  ;; getenv -- no spawn, no stat -- and between them they cover how the version
  ;; managers hafod's own probe comment anticipates actually work: $PATH is what
  ;; nvm/fnm/asdf/mise rewrite, and the rest are the per-shell overrides
  ;; (pyenv shell, rbenv shell, rustup's toolchain override, an activated
  ;; virtualenv or conda environment) that change the answer without touching
  ;; $PATH at all.
  (define %version-env-keys
    '("PATH" "VIRTUAL_ENV" "CONDA_DEFAULT_ENV" "PYENV_VERSION" "RBENV_VERSION"
      "NODENV_VERSION" "NODE_VERSION" "ASDF_DIR" "RUSTUP_TOOLCHAIN" "JAVA_HOME"))

  ;; The environment half of the cache stamp: each watched key paired with its
  ;; current value (an absent variable stamps as "", so setting or clearing one
  ;; both count as a change).
  (define (version-env-stamp)
    (map (lambda (k) (cons k (or (getenv k) ""))) %version-env-keys))

  ;; A leaf test seam counting REAL directory scans from the cached path -- one
  ;; per cached-version-segment call that has to re-run detect-tools-in.  It
  ;; counts SCANS, not spawns (the distinction from version-probe-count): the
  ;; cache saves the spawns, and this is the oracle for the separate question of
  ;; whether it also saves the filesystem work.  Default 0; NOT a (hafod) umbrella
  ;; member.
  (define version-detect-count
    (make-parameter 0
      (lambda (v)
        (unless (and (integer? v) (exact? v) (>= v 0))
          (error 'version-detect-count "expected a non-negative exact integer" v))
        v)))

  ;; The longest string the probe will accept as a version.  Comfortably above any
  ;; real banner (the longest shipped tool prints eleven characters) and far below
  ;; anything that could disturb the info line.
  (define %version-max-length 32)

  ;; A leaf test seam counting REAL spawns -- one per probe-tool-version call that
  ;; reaches with-spawn-timeout.  It counts SPAWNS, not segment recomputes (the
  ;; distinction from git-probe-count, which counts recomputes): a single
  ;; cached-version-segment miss may probe several detected on-PATH tools, so this
  ;; seam lets a test pin "an unrelated dir spawns nothing" (0), "each detected
  ;; on-PATH tool at most once per cwd" (flat on a repeat draw) and "the PATH gate
  ;; declines before any spawn" (0).  NOT a (hafod) umbrella member.
  (define version-probe-count
    (make-parameter 0
      (lambda (v)
        (unless (and (integer? v) (exact? v) (>= v 0))
          (error 'version-probe-count "expected a non-negative exact integer" v))
        v)))

  ;; The cache stamp for CWD given its DETECTED tools -- three parts, all
  ;; recomputed with no spawn:
  ;;
  ;;   the environment      the watched keys above, because the one event this
  ;;                        segment exists to report is a toolchain SWITCH, and
  ;;                        a switch is an environment change, not a file change.
  ;;                        `nvm use 20`, `pyenv shell 3.12`, `rustup default
  ;;                        nightly`, a direnv reload: none of them touches a
  ;;                        marker, so a marker-only stamp would show the
  ;;                        pre-switch version for the rest of the session --
  ;;                        precisely the case the probe passes the caller's
  ;;                        environment through in order to get right.
  ;;   the detected names   -- each paired with the BINARY IT RESOLVED TO -- so a
  ;;                        registry change re-renders.  A tool detected purely by
  ;;                        EXTENSION contributes no marker file below, so without
  ;;                        the name a tool registered while sitting in a matching
  ;;                        directory would never appear there.  The resolved
  ;;                        binary belongs in the key because the rendered string
  ;;                        reports the version of one SPECIFIC binary: the
  ;;                        environment half above already catches a $PATH move,
  ;;                        but the path-cache is also rebuilt by an explicit
  ;;                        rebuild-path-cache! with no environment change at all,
  ;;                        and without this a directory cached as empty while the
  ;;                        tool was absent would keep serving empty for the whole
  ;;                        cache lifetime after it became visible.  The cost is
  ;;                        one short-circuiting hashtable read per DETECTED tool
  ;;                        per draw -- no spawn and no stat -- and the detected
  ;;                        set is itself bounded by the registry.
  ;;   the marker mtimes    a name-sorted list of (marker-filename . mtime) over
  ;;                        the marker FILES actually present (via safe-mtime,
  ;;                        which yields #f -- and so is skipped -- for an absent
  ;;                        marker), sorted so the stamp is stable regardless of
  ;;                        directory order.
  ;;
  ;; A bare content edit that touches no marker and no watched variable is left
  ;; to the cache's own age limit rather than to a full directory scan.
  (define (version-marker-stamp cwd detected)
    (let ([acc '()])
      (for-each
        (lambda (t)
          ;; Guarded per tool, exactly as the detector guards its match.  This
          ;; walks the WHOLE marker list and appends each name to the cwd, so a
          ;; single malformed entry raises -- and the raise would leave here, leave
          ;; the cache and leave the segment thunk, to be swallowed by the draw's
          ;; blanket guard, taking every working tool registered beside the broken
          ;; one down with it.  A broken descriptor stamps nothing instead.
          (guard (e [#t (void)])
            (for-each
              (lambda (fname)
                (let ([mt (safe-mtime (string-append cwd "/" fname))])
                  (when mt (set! acc (cons (cons fname mt) acc)))))
              (car (prompt-tool-markers t)))))
        detected)
      (list (version-env-stamp)
            (map (lambda (t) (cons (prompt-tool-name t) (resolve-tool-command t)))
                 detected)
            (list-sort (lambda (a b) (string<? (car a) (car b))) acc))))

  ;; Probe one tool's version once, bounded.  RESOLVE the tool's command against
  ;; the path-cache first -- a descriptor none of whose candidates is installed
  ;; returns #f here having neither counted nor spawned -- then count the spawn and
  ;; run the tool's
  ;; FIXED LITERAL argv (never a shell string, so the killed pid is the reaped pid
  ;; and a marker value is no injection vector) through the bounded collector at
  ;; DEADLINE-MS with the stderr MERGE disposition -- so a stderr-only banner such
  ;; as `java -version` is captured -- and the inherited environment, so a version-
  ;; manager shim resolves as it would for the user.  The environment comes from
  ;; cached-env-strings, the project's single spawn-env path: it is dirty-flag
  ;; invalidated, so an unchanged-environment group rebuilds the "KEY=VALUE" list
  ;; once rather than once per probe, and there is one implementation of how hafod
  ;; composes a child environment rather than two that can drift.  Then run the
  ;; tool's own parse over the captured output, GUARDED so a user parse that
  ;; raises is fail-quiet.
  ;; Returns a non-empty version string, or #f on a timeout, a spawn failure, a
  ;; non-zero exit, or a version-less parse.
  ;;
  ;; The exit status is the second half of the "is this a banner?" question, and
  ;; the merged stderr is why it has to be asked.  A version-manager shim that
  ;; cannot satisfy the directory's request prints a version-shaped DIAGNOSTIC and
  ;; exits non-zero -- pyenv's "version `3.12.1' is not installed", rustup's
  ;; "error: toolchain '1.75.0' is not installed".  Textually those are
  ;; indistinguishable from a banner, so the prompt would confidently report a
  ;; toolchain that is not installed and is not in use.  Requiring a clean exit
  ;; discards them: a tool that failed reports no version at all.
  ;;
  ;; The resolve is deliberately repeated here rather than threaded down from the
  ;; group's own gate: it keeps this procedure's shipped two-argument signature and
  ;; costs one more short-circuiting hashtable read, and it makes the "counts REAL
  ;; spawns" contract of version-probe-count exact -- a descriptor that resolves to
  ;; nothing can no longer charge a spawn it never made.
  (define (probe-tool-version t deadline-ms)
    (let ([winner (resolve-tool-command t)])
      (if (not winner)
          #f
          (begin
            (version-probe-count (+ (version-probe-count) 1))
            (let-values ([(out timed-out? exit-status)
                          (with-spawn-timeout/status winner
                                                     (tool-probe-argv t winner)
                                                     deadline-ms
                                                     'merge
                                                     (cached-env-strings)
                                                     %spawn-max-bytes)])
              (if (or timed-out? (not (eqv? exit-status 0)))
                  #f
                  (let ([v (guard (e [#t #f]) ((prompt-tool-parse t) out))])
                    ;; The version is interpolated verbatim into the prompt (control
                    ;; bytes are stripped, digits and dots are not), so its LENGTH is
                    ;; bounded here as well.  No real version runs to 32 characters;
                    ;; anything that long is a digit run out of a log, and an
                    ;; arbitrarily long "version" on the info line is exactly what
                    ;; the width budget cannot recover from.
                    (and (string? v)
                         (positive? (string-length v))
                         (<= (string-length v) %version-max-length)
                         v))))))))

  ;; The whole group's wall-clock budget, in milliseconds -- the bound the USER
  ;; experiences, as distinct from prompt-spawn-timeout-ms, which bounds one
  ;; probe.  The two are not the same number and must not be confused: the group
  ;; runs its probes SERIALLY inside the prompt draw, so a per-probe bound alone
  ;; multiplies by the number of detected tools, and a polyglot directory with
  ;; several slow version-manager shims would freeze the REPL for well over a
  ;; second on every cache miss (and marker files such as Cargo.lock,
  ;; package.json and go.sum are rewritten by ordinary builds, so misses are
  ;; routine).  300 ms is the whole group's share of a draw; each probe is
  ;; charged what remains of it, and once it is spent the rest of the tools are
  ;; skipped WITHOUT spawning.  The floor at prompt-spawn-timeout-ms keeps a user
  ;; who deliberately raises the per-probe deadline above 300 ms from starving
  ;; even the first probe.
  (define %version-group-budget-ms 300)

  ;; The most tools one draw will probe.  The budget already bounds the time; this
  ;; bounds the fan-out, so a polyglot monorepo whose directory happens to carry
  ;; eight markers cannot spawn eight children per miss.  Four segments is also
  ;; about as much as an info line can carry before the width budget trims it.
  (define %version-group-max-tools 4)

  ;; The first N elements of LST (all of them when it is shorter).
  (define (take-at-most lst n)
    (if (or (null? lst) (<= n 0))
        '()
        (cons (car lst) (take-at-most (cdr lst) (- n 1)))))

  ;; Render the version group for the DETECTED tools at the given glyph TIER,
  ;; colour DEPTH and column budget WIDTH: the detected tools are first passed
  ;; through the $PATH gate, the survivors capped at the fan-out limit, and each of
  ;; those probed once and rendered through the one-tool renderer.  The non-empty
  ;; parts join with a single space, so a version-less tool leaves no stray
  ;; separator.
  ;;
  ;; WIDTH is the columns THIS GROUP may occupy, not the width of the terminal.
  ;; Charging it a fixed share of the whole line -- half of it -- budgeted the
  ;; group in isolation while the info line it sits in is path + git + group, and
  ;; neither of the first two is truncated against what is left: a forty-column
  ;; path, a branch name and a forty-column group is some ninety-five columns on
  ;; an eighty-column terminal, which wraps onto a second row and breaks the right
  ;; prompt's single-row cursor positioning -- exactly what budgeting the group was
  ;; for.  segment-outputs now hands each left segment the columns still free on
  ;; the row, so the group is budgeted against the line rather than against the
  ;; terminal.  The 3-argument form falls back to the whole terminal width, which
  ;; is the right answer for a bare call with nothing beside it.
  ;;
  ;; The gate runs BEFORE the cap, not inside the loop after it.  It is an O(1)
  ;; hashtable read with no spawn and no stat, so a tool it declines costs
  ;; nothing -- but capping the DETECTED list ahead of it spent a probe slot on
  ;; that free decline, and an installed tool sitting behind four uninstalled ones
  ;; was never reached at all.  A polyglot repository root carrying
  ;; pyproject.toml, package.json, Cargo.toml, go.mod and pom.xml on a machine
  ;; without rust or go is the ordinary case, not a contrived one.
  ;;
  ;; The group holds ONE wall-clock budget for the whole draw.  Each probe is
  ;; charged what is left of it (never more than a single probe's own deadline),
  ;; and a tool reached with the budget already spent is skipped without spawning
  ;; at all -- so the cost of the group is bounded by the budget plus one kill
  ;; grace, not by the number of tools times the per-probe deadline.
  ;;
  ;; It also holds a COLUMN budget, measured with ansi-visible-length so an SGR
  ;; costs nothing and a wide glyph costs its true two cells.  Tools are added
  ;; while they fit; the first one that does not ends the group, so draw order
  ;; still means what it says.
  ;;
  ;; Each tool is rendered inside its OWN guard, for the same reason the detector
  ;; guards its match: the descriptor fields are user data that the PATH gate and
  ;; the renderer dereference structurally, and an unguarded raise here would take
  ;; the whole group down with the one broken tool.
  (define render-version-group
    (case-lambda
      [(detected glyph-tier colour-depth)
       (render-version-group detected glyph-tier colour-depth (terminal-width))]
      [(detected glyph-tier colour-depth width)
       (let ([t0     (current-time 'time-monotonic)]
             [budget (max %version-group-budget-ms (prompt-spawn-timeout-ms))]
             ;; A budget of zero is a real answer -- the row is full, the group
             ;; renders nothing -- so only a width that is not a count at all
             ;; falls back to the terminal.
             [cols   (if (and (integer? width) (exact? width) (>= width 0))
                         width
                         (terminal-width))])
         (let loop ([ts (take-at-most
                          ;; Gate, THEN cap.  The gate is guarded per tool for the
                          ;; same reason the render below is: the command field is
                          ;; user data the gate dereferences structurally.
                          (filter (lambda (t) (guard (e [#t #f]) (tool-on-path? t)))
                                  detected)
                          %version-group-max-tools)]
                    [used 0]
                    [acc '()])
           (if (null? ts)
               (join-nonempty-space (reverse acc))
               (let ([out (guard (e [#t ""])
                            (let ([left (- budget
                                           (elapsed-milliseconds
                                             t0 (current-time 'time-monotonic)))])
                              (if (<= left 0)
                                  ""     ; budget spent -> skip, no spawn
                                  (let ([v (probe-tool-version
                                             (car ts)
                                             (min (prompt-spawn-timeout-ms) left))])
                                    (if v
                                        (render-one-tool (car ts) glyph-tier
                                                         colour-depth v)
                                        "")))))])
                 (if (string=? out "")
                     (loop (cdr ts) used acc)
                     ;; The joining space counts towards the budget too.
                     (let ([w (+ (ansi-visible-length out) (if (null? acc) 0 1))])
                       (if (> (+ used w) cols)
                           (join-nonempty-space (reverse acc))
                           (loop (cdr ts) (+ used w) (cons out acc)))))))))]))

  ;; How long a recorded detection may be reused without re-listing the directory,
  ;; in milliseconds.  It is deliberately NOT the probe cache's TTL: the two answer
  ;; different questions, and tying them together is what made a fresh marker
  ;; invisible for half a minute.  safe-mtime yields whole EPOCH SECONDS, so a
  ;; marker created in the same wall-clock second as the entry's own stat leaves
  ;; the directory mtime equal -- and equal it stays, for ever, because nothing
  ;; else touches the directory.  Holding the reuse window strictly below that
  ;; one-second granularity means the next draw a second later re-lists and sees
  ;; the marker, so `cargo init` / `npm init` / `touch Cargo.toml` in the directory
  ;; one is standing in shows up, and a marker DELETION stops being probed, within
  ;; a second rather than at the TTL's leisure.
  (define %version-detect-reuse-ms 1000)

  ;; The cached version group for CWD at the given glyph TIER and colour DEPTH.
  ;; A cache entry is
  ;;
  ;;     #(dir-mtime registry detected stamp stamped-at rendered detected-at)
  ;;
  ;; and it is consulted in two steps, because the two halves of the work have
  ;; different invalidation signals.
  ;;
  ;; Detection is a full directory-list of the cwd plus a hashtable of every
  ;; filename in it plus an extension table over the same entries -- work whose
  ;; cost is unbounded in the SIZE of the directory, and a prompt is drawn after
  ;; every command.  Running it on every draw meant that merely sitting in a
  ;; downloads directory, a node_modules, a Maildir or a stalled network mount
  ;; paid for the whole listing each time, cache hit or miss.  But the detected
  ;; set is a pure function of the directory's entries and the registry, so a
  ;; recording made at the same directory mtime, under the same registry, and
  ;; within %version-detect-reuse-ms of the scan that produced it is reused and no
  ;; scan happens at all.
  ;;
  ;; That last clause is measured from when the SCAN ran (slot 6), not from when
  ;; the entry was last stored: a cache hit returns without re-storing, so the
  ;; store time never refreshes on the hit path and drifts forward on the miss
  ;; path, and either way it says nothing about how old the detection is.
  ;;
  ;; The marker mtimes are then stamped as before -- a per-marker stat, which is
  ;; bounded by the number of detected tools rather than by the directory -- and
  ;; a hit whose stamp is unchanged and which has not aged out returns the stored
  ;; group WITHOUT probing, so a repeat draw spawns nothing.  Otherwise the group
  ;; is rendered, the table cleared on overflow, and the entry stored (the empty
  ;; group cached too).  So an unrelated dir is a cached "" that never probes, and
  ;; a project re-probes when a marker moves, when the toolchain environment
  ;; moves, or when the entry ages out.
  (define cached-version-segment
    (case-lambda
      [(cwd glyph-tier colour-depth)
       (cached-version-segment cwd glyph-tier colour-depth (terminal-width))]
      [(cwd glyph-tier colour-depth width)
       (let* ([hit    (hashtable-ref version-segment-cache cwd #f)]
              [fresh? (and hit
                           (< (- (real-time) (vector-ref hit 4))
                              (version-cache-ttl-ms)))]
              [dir-mt (safe-mtime cwd)]
              [tools  (prompt-tools)]
              [reuse-detection?
               (and hit dir-mt
                    (< (- (real-time) (vector-ref hit 6)) %version-detect-reuse-ms)
                    (equal? (vector-ref hit 0) dir-mt)
                    (eq? (vector-ref hit 1) tools))]
              [detected
               (if reuse-detection?
                   (vector-ref hit 2)                ; unchanged directory -> no scan
                   (begin
                     (version-detect-count (+ (version-detect-count) 1))
                     (detect-tools-in cwd)))]
              ;; Carried forward untouched when the recording is reused, so the
              ;; window is measured from the SCAN and cannot be walked forward by
              ;; a run of marker-stamp misses in an otherwise unchanged directory.
              [detected-at (if reuse-detection? (vector-ref hit 6) (real-time))]
              ;; The width is part of the stamp because the group is trimmed to
              ;; it: a cached string rendered for a wider budget would overflow a
              ;; narrower one.  Since the budget is now what the row has LEFT, it
              ;; also moves when the segments before the group change length -- a
              ;; branch name or a dirty marker -- so those re-probe as well.  That
              ;; is the honest cost of trimming against the line rather than
              ;; against the terminal, and it is bounded by the same budget, cap
              ;; and PATH gate as any other miss.
              [stamp  (list width (version-marker-stamp cwd detected))]
              ;; A probe hit: a fresh entry whose stamp is unchanged, so the
              ;; stored group is returned and nothing is spawned.
              [hit?   (and fresh? (equal? (vector-ref hit 3) stamp))]
              [rendered (if hit?
                            (vector-ref hit 5)
                            (render-version-group detected glyph-tier colour-depth
                                                  width))])
         ;; Only an insert of a NEW key can grow the table, so only that can
         ;; overflow it.  Testing the size before every insert meant that once a
         ;; session had visited the cap's worth of directories, every marker touch
         ;; in any ALREADY-cached one wiped all of them -- to store a key that was
         ;; there already.  The git cache does the same, but the cost is not the
         ;; same: refilling that one is a spawn per directory, and refilling this
         ;; one is a whole tool group per directory.
         (unless (hashtable-contains? version-segment-cache cwd)
           (when (>= (hashtable-size version-segment-cache) (version-cache-cap))
             (hashtable-clear! version-segment-cache)))
         ;; The entry is rewritten on a probe HIT as well as on a miss, because a
         ;; hit may still have re-listed the directory -- the two halves have
         ;; separate windows -- and that recording has to be kept.  Returning
         ;; early without it left the scan time fixed at the first draw, so once
         ;; the reuse window had passed every later draw re-listed the directory:
         ;; the unbounded per-draw listing back again, merely deferred by a second.
         ;; stamped-at is carried over unchanged on a hit, so the TTL keeps ageing
         ;; the entry from the render that produced it rather than sliding forward
         ;; with every draw and never expiring at all.
         (hashtable-set! version-segment-cache cwd
                         (vector dir-mt tools detected stamp
                                 (if hit? (vector-ref hit 4) (real-time))
                                 rendered detected-at))
         rendered)]))

  ;; The version group's own opt-out, default #t (the group is shown).
  ;;
  ;; It exists because the group is on by default and merely CHANGING INTO a
  ;; directory is enough to run every detected toolchain's version command there.
  ;; The argv is a fixed literal and the PATH gate means only already-installed
  ;; tools run, so there is no injection vector -- but the marker FILENAMES are
  ;; repository-controlled, and several of them (.python-version, .ruby-version,
  ;; .node-version/.nvmrc, .java-version, .php-version) are exactly the files a
  ;; version-manager shim reads to decide WHICH toolchain to run.  Cloning an
  ;; untrusted repository and cd-ing into it therefore makes the user's own shim
  ;; resolve a repository-chosen version, and with asdf or mise an unknown version
  ;; can provoke an install or a network fetch.  That is the same posture starship
  ;; takes and it is a defensible default, but a user who does not want it needs
  ;; somewhere narrower to say so than "turn the whole prompt off".
  (define prompt-versions? (make-parameter #t (lambda (v) (and v #t))))

  ;; #t unless the group has been turned off, either from init.ss with
  ;; (prompt-versions? #f) -- config being Scheme -- or from the environment with
  ;; a falsy HAFOD_PROMPT_VERSIONS.  The env var mirrors HAFOD_PROMPT because it
  ;; asks the same question of the same shared predicate, not because two copies
  ;; of the spelling list happen to agree: unset or non-falsy leaves the group on
  ;; (so the default needs no env at all), while a falsy value turns it off,
  ;; leaving the rest of the informative prompt untouched.
  (define (version-group-enabled?)
    (and (prompt-versions?)
         (env-switch-on? "HAFOD_PROMPT_VERSIONS")
         #t))

  ;; The version-group segment thunk -- the single `left` segment the preset
  ;; registers after git.  It reads the logical cwd, the glyph tier, the colour
  ;; depth and the WIDTH budget off the per-draw context and returns the cached
  ;; group ("via <glyph> vX ..." for the detected, installed toolchains, or ""
  ;; when the directory has none).  A pure read of the ctx, so the preset composes
  ;; it exactly as it does the path and git segments.  The width is what the ctx
  ;; carries it for: the path segment budgets against its own limit, this is the
  ;; other segment that can grow without bound, and segment-outputs has already
  ;; narrowed the ctx to the columns the earlier left segments left free.
  ;;
  ;; The opt-out is consulted FIRST, ahead of the cache and therefore ahead of the
  ;; directory scan and every spawn: switching the group off must stop the work,
  ;; not merely hide its result.
  (define (version-group-segment ctx)
    (if (not (version-group-enabled?))
        ""
        (cached-version-segment (prompt-ctx-cwd ctx)
                                (prompt-ctx-glyph-tier ctx)
                                (prompt-ctx-colour-depth ctx)
                                (prompt-ctx-width ctx))))

  ;; Install the informative prompt in a single call, re-expressed as a
  ;; COMPOSITION over the segment model rather than a pair of frozen lambdas: it
  ;; rebuilds the ordered segment list -- the path (left), the cached git state
  ;; (left), the per-language version group (left), the exit-code badge (right),
  ;; the command-timing readout (right) and the exit-coloured input glyph (the
  ;; `line` break) -- then SETS the two hook parameters to the default renderers.
  ;; So the visible layout is the two-line "info...\n<glyph> " default with the
  ;; exit-coloured `❯`, and both the git state and the version group come through
  ;; their cached, spawn-timeout-bounded paths (no per-prompt re-spawn of an
  ;; unchanged repository or toolchain, no block on a cold `git status`, and no
  ;; spawn at all in a directory with no toolchain marker).  It adds no
  ;; dispatch mechanism and no theme language of its own -- it only registers the
  ;; segment thunks already defined above and installs render-prompt-segments /
  ;; render-right-prompt-segments into repl-prompt-hook / repl-right-prompt-hook.
  ;;
  ;; OPTS is a plist of QUOTED SYMBOLS (key value key value ...), not reader
  ;; keywords.  Chez has no keyword objects, so a written "#:git" would read as a
  ;; fresh uninterned symbol that could never match a key and would be silently
  ;; ignored; the keys are therefore plain symbols -- (enable-informative-prompt!
  ;; 'git #f 'path-width 60).  Each key has a sensible default, so a bare
  ;; (enable-informative-prompt!) installs a working prompt.  Recognised keys:
  ;;   git                 boolean, default #t  -- include the git-state segment
  ;;   version             boolean, default #t  -- include the per-language version group
  ;;   path-width          integer, default 40  -- the path truncation budget
  ;;   timing-threshold-ms integer, default 2000 -- the timing display threshold
  ;;   exit-code           boolean, default #t  -- include the exit-code badge
  ;;   timing              boolean, default #t  -- include the command-timing readout
  ;; An odd-length option list or an unknown key raises a clear error rather than
  ;; silently mis-configuring the prompt.
  ;;
  ;; The preset stays overridable segment by segment precisely because it composes
  ;; the existing parameters rather than freezing a rendering: each segment is a
  ;; thunk the user may replace in the prompt-segments list, rebinding
  ;; repl-prompt-hook or repl-right-prompt-hook wholesale after this call replaces
  ;; the whole line on the next draw, and the timing threshold is published through
  ;; prompt-timing-threshold, so raising or lowering it later still takes effect.
  ;; Nothing here captures a hook or a segment by value.
  (define (enable-informative-prompt! . opts)
    (let loop ([o opts] [git? #t] [path-width 40] [timing-ms 2000]
               [exit? #t] [timing? #t] [version? #t])
      (cond
        [(null? o)
         ;; Publish the timing threshold so the timing segment thunk -- and any
         ;; later bare (prompt-timing-segment) -- reads it, and so an invalid value
         ;; is rejected here rather than at the first prompt draw.
         (prompt-timing-threshold timing-ms)
         ;; Rebuild the ordered segment list from scratch, then install the default
         ;; renderers.  Each option gates its segment: 'git #f drops the git
         ;; segment, 'version #f the version group, 'exit-code #f the exit badge,
         ;; 'timing #f the timing readout, 'path-width sets the path budget.  The
         ;; git segment is the CACHED, spawn-timeout-bounded path
         ;; (cached-git-segment), never the blocking collector, so a cold or
         ;; unchanged repository never spikes the prompt.
         (clear-prompt-segments!)
         (register-prompt-segment! 'left
           (lambda (ctx)
             (prompt-path-segment (prompt-ctx-cwd ctx) (home-directory) path-width)))
         (when git?
           (register-prompt-segment! 'left (lambda (ctx) (cached-git-segment))))
         ;; The per-language version group -- ONE left segment registered directly
         ;; after git, so the info line reads path, git, then "via <glyph> vX" for
         ;; each detected, installed toolchain.  Registering it HERE (rather than
         ;; at library top level) is what keeps the default prompt out of the
         ;; pristine guard's way: the REPL-entry default-on calls this preset with
         ;; the guard disarmed, so composing the version group does not itself mark
         ;; the prompt customised and a user's own set-prompt! still wins.  Like the
         ;; git segment it is the cached, spawn-timeout-bounded path
         ;; (version-group-segment -> cached-version-segment), and a directory with
         ;; no toolchain marker never spawns at all.
         (when version?
           (register-prompt-segment! 'left version-group-segment))
         (when exit?
           (register-prompt-segment! 'right (lambda (ctx) (prompt-exit-segment))))
         (when timing?
           (register-prompt-segment! 'right (lambda (ctx) (prompt-timing-segment))))
         ;; The exit-coloured input glyph on its own row -- the `line` placement
         ;; drives the two-line "info...\n<glyph> " layout (read-expression already
         ;; splits a multi-line prompt on the last newline, so no editor change).
         (register-prompt-segment! 'line prompt-char-segment)
         ;; Install the default renderers with the pristine guard DISARMED, so the
         ;; preset composing the default prompt does not itself mark the prompt
         ;; customised -- a user who calls this AND has default-on fire is harmless.
         (%with-guard-disarmed
           (lambda ()
             (repl-prompt-hook
               (lambda ()
                 (render-prompt-segments (current-output-port) (make-current-prompt-ctx))))
             (repl-right-prompt-hook
               (lambda ()
                 (render-right-prompt-segments
                   (current-output-port) (make-current-prompt-ctx))))))]
        [(null? (cdr o))
         (error 'enable-informative-prompt! "odd option list" opts)]
        [else
         (let ([key (car o)] [val (cadr o)] [rest (cddr o)])
           (case key
             [(git)                 (loop rest val path-width timing-ms exit? timing? version?)]
             [(path-width)
              ;; Validate here, at configuration time, rather than letting a bad
              ;; width raise inside the prompt hook at draw time -- that raise would
              ;; land outside the REPL's eval guard and disrupt the read loop.
              (unless (and (integer? val) (exact? val) (positive? val))
                (error 'enable-informative-prompt!
                       "path-width must be a positive exact integer" val))
              (loop rest git? val timing-ms exit? timing? version?)]
             [(timing-threshold-ms) (loop rest git? path-width val exit? timing? version?)]
             [(exit-code)           (loop rest git? path-width timing-ms val timing? version?)]
             [(timing)              (loop rest git? path-width timing-ms exit? val version?)]
             [(version)             (loop rest git? path-width timing-ms exit? timing? val)]
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

  ;; The REPL-entry default-on decision, minus the real-terminal gate (the REPL adds
  ;; use-editor? at the install site, so a -c/-s/pipe/non-tty run -- where use-editor?
  ;; is #f -- installs nothing).  The informative prompt is installed by default only
  ;; when the master gate is on, the opt-out parameter is #t, the user has not already
  ;; customised the prompt, and HAFOD_PROMPT is unset or non-falsy.  HAFOD_PROMPT
  ;; mirrors HAFOD_BATCH but INVERTED: an unset or non-falsy value enables the default
  ;; (so the out-of-the-box prompt needs no env at all), while a falsy value -- the
  ;; empty string, 0, false, or no, matched case-insensitively -- disables it, so
  ;; HAFOD_PROMPT=0 turns the default prompt off rather than silently on.  Both
  ;; readings come from the one shared spelling list; see env-switch-on?.
  (define (should-install-default-prompt?)
    (and (interactive-enhancements?)
         (informative-prompt?)
         (not (prompt-customised?))
         (env-switch-on? "HAFOD_PROMPT")
         #t))

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
                                           (env-switch-explicitly-on? "HAFOD_BATCH"))])
          ;; Default-on: with a pristine prompt on a real terminal, install the
          ;; informative prompt here -- AFTER init.ss (loaded before the REPL), so a
          ;; user's set-prompt! / hook rebind there has already tripped the pristine
          ;; guard and wins.  Gating on use-editor? at THIS site (not the earlier
          ;; master-gated block) is what keeps the informative prompt out of
          ;; -c/-s/pipe/non-tty runs, whose use-editor? is #f.  The install goes
          ;; through the guard-disarm so it does not itself mark the prompt customised
          ;; (idempotent with a user's own enable-informative-prompt!).
          (when (and use-editor? (should-install-default-prompt?))
            (%with-guard-disarmed (lambda () (enable-informative-prompt!))))
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

  ;; Arm the pristine-prompt guard now that every prompt parameter and the preset
  ;; are constructed.  The hook converters ran on their own construction defaults
  ;; above while the guard was #f, so a pristine start stays customised-#f; from
  ;; here a genuine user set-prompt! / hook rebind trips prompt-customised?.
  (set! %prompt-guard-armed? #t)

  ) ; end library
