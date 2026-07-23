#!chezscheme
;;; test-prompt-preset.ss -- The informative-prompt preset and its right-prompt
;;; segments.  The first half covers the exit-code badge (a red cross plus the
;;; code, shown only after a non-zero last command) and the command-timing readout
;;; (a formatted duration shown only above a configurable threshold), each driven
;;; deterministically by parameterising the underlying state.  The second half
;;; proves the one-call enable-informative-prompt! preset end-to-end through the
;;; PTY-free (test vterm) oracle: inside a clean repo the left line shows the path
;;; and a green branch, a non-zero exit renders a red cross badge on the right,
;;; outside a repository the left line carries no branch, and a hook rebound after
;;; the preset still wins.  The last section proves the per-language version group
;;; is shown by default and removed by the `version` option.  render->screen forces
;;; the terminal-capability verdict on, so the segments' gated 256-colour reaches
;;; the cell grid.
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
;; The preset render proofs shell out to build temp git repositories (create-
;; directory and the run EPF), so the full chezscheme surface is imported --
;; except getenv, taken from (hafod environment) below for its presence-aware
;; semantics -- mirroring the sibling git-prompt suite.
(import (except (chezscheme) getenv)
        (test runner)
        (test vterm)
        (only (hafod interactive)
              prompt-exit-segment prompt-colour-ok? last-status
              prompt-timing-segment prompt-timing-threshold last-duration
              enable-informative-prompt! repl-prompt-hook repl-right-prompt-hook
              prompt-path-segment display-right-prompt terminal-width
              prompt-tools make-prompt-tool parse-version/common
              prompt-versions? version-probe-count)
        (only (hafod environment) getenv setenv)
        (only (hafod shell classifier) path-cache)
        (only (hafod fileinfo) create-directory)
        (only (hafod syntax) run run/strings)
        (only (hafod process-state) with-cwd* pid)
        (only (hafod editor input-decode) char-display-width)
        (only (hafod terminal-caps) assume-terminal-caps glyph-tier))

(test-begin "prompt-preset")

;; Pin CLICOLOR_FORCE unset, mirroring the sibling git-prompt suite: an ambient
;; CLICOLOR_FORCE=1 would force colour on the non-tty test sink and perturb the
;; default-verdict content assertions below.
(setenv "CLICOLOR_FORCE" #f)

;; Substring search (the runner carries no string-contains).
(define (string-contains? haystack needle)
  (let ([hlen (string-length haystack)] [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

;; True when STR carries no ESC (#x1b) byte -- the invariant a plain, uncoloured
;; segment must satisfy.
(define (no-esc? str)
  (let loop ([i 0])
    (cond
      [(>= i (string-length str)) #t]
      [(= (char->integer (string-ref str i)) #x1b) #f]
      [else (loop (+ i 1))])))

;; === prompt-exit-segment -- a red "✘N" badge shown only on a non-zero exit ===

;; Success is silent: a zero last status renders the empty string.
(parameterize ([last-status 0])
  (test-equal "exit: success renders nothing" "" (prompt-exit-segment)))

;; A non-zero status renders the cross glyph followed by the decimal code.
(parameterize ([last-status 1])
  (test-assert "exit: a failure shows the cross and code"
    (string-contains? (prompt-exit-segment) "\x2718;1")))

;; The SIGINT status (130) renders its full multi-digit code, not a truncation.
(parameterize ([last-status 130])
  (test-assert "exit: SIGINT shows the full 130"
    (string-contains? (prompt-exit-segment) "\x2718;130")))

;; Colour gate: a true verdict wraps the badge in the red 256-colour SGR run
;; (index 1); the decision follows prompt-colour-ok?, never the output port, so
;; a port-gated implementation would emit plain text here and fail this assert.
(parameterize ([last-status 1] [prompt-colour-ok? (lambda () #t)])
  (test-assert "exit: red 256-colour SGR under a true verdict"
    (string-contains? (prompt-exit-segment) "38;5;1m")))

;; A false verdict yields the same glyphs entirely plain -- no escape byte at all.
(parameterize ([last-status 1] [prompt-colour-ok? (lambda () #f)])
  (test-assert "exit: plain (no ESC) under a false verdict"
    (no-esc? (prompt-exit-segment))))

;; === prompt-timing-segment + prompt-timing-threshold -- a duration shown only
;; above a configurable threshold ===

;; Below one second: under the default threshold AND below format-duration's
;; sub-second floor (it returns #f) -- empty on both counts.
(parameterize ([last-duration 500])
  (test-equal "timing: a sub-second command renders nothing"
    "" (prompt-timing-segment)))

;; 900 ms against an explicit 2000 ms threshold: under the threshold -> empty.
(parameterize ([last-duration 900])
  (test-equal "timing: under an explicit threshold renders nothing"
    "" (prompt-timing-segment 2000)))

;; 1500 ms under the DEFAULT 2000 ms threshold: format-duration would yield a
;; string, but the threshold gate still suppresses it -> empty.  This isolates the
;; threshold gate from the sub-second floor.
(parameterize ([last-duration 1500])
  (test-equal "timing: above a second yet under the default threshold is empty"
    "" (prompt-timing-segment)))

;; 2500 ms at the default threshold: a formatted duration (containing "s").
(parameterize ([last-duration 2500])
  (test-assert "timing: at or above the default threshold shows a duration"
    (string-contains? (prompt-timing-segment) "s")))

;; A lowered threshold surfaces a shorter (but still >= 1s) duration that the
;; default 2000 ms gate would have hidden.
(parameterize ([prompt-timing-threshold 1000] [last-duration 1500])
  (test-assert "timing: a lowered threshold surfaces a shorter duration"
    (string-contains? (prompt-timing-segment) "s")))

;; Colour gate: grey 256-colour (index 240) under a true verdict, plain under a
;; false one -- the same port-independent discipline as the exit badge.
(parameterize ([last-duration 2500] [prompt-colour-ok? (lambda () #t)])
  (test-assert "timing: grey 256-colour SGR under a true verdict"
    (string-contains? (prompt-timing-segment) "38;5;240m")))
(parameterize ([last-duration 2500] [prompt-colour-ok? (lambda () #f)])
  (test-assert "timing: plain (no ESC) under a false verdict"
    (no-esc? (prompt-timing-segment))))

;; The threshold parameter validates its input: a non-integer argument raises
;; through the make-parameter converter (mirroring last-duration).
(test-error "timing: a non-integer threshold is rejected"
  (prompt-timing-threshold "later"))

;; === enable-informative-prompt! -- the one-call preset, proven through the
;; (test vterm) oracle in known states ===
;;
;; enable-informative-prompt! SETS repl-prompt-hook and repl-right-prompt-hook.
;; render->screen captures a render thunk's bytes into a cell grid with the
;; terminal-capability verdict forced on, so a segment's gated colour reaches the
;; cells.  The oracle records only 256/truecolour foreground, so colour is
;; asserted by index: a clean git branch is green (index 2), the exit badge red
;; (index 1) -- exactly the 256-colour SGR the segments emit.

;; Option parsing is fail-fast: a malformed plist is rejected rather than silently
;; mis-configuring the prompt.  Both raise BEFORE any hook is set (the loop rejects
;; the option list before reaching the install arm), so neither disturbs the hooks.
(test-error "preset: an odd-length option list raises"
  (enable-informative-prompt! 'git))
(test-error "preset: an unknown option key raises"
  (enable-informative-prompt! 'nonsense #t))
;; path-width is validated at configuration time -- not left to raise inside the
;; prompt hook at draw time, where the error would escape the REPL's eval guard.
(test-error "preset: a non-integer path-width raises"
  (enable-informative-prompt! 'path-width 'wide))
(test-error "preset: a non-positive path-width raises"
  (enable-informative-prompt! 'path-width 0))

;; The 0-based index of NEEDLE in HAYSTACK, or #f (the runner has no such helper).
(define (string-index haystack needle)
  (let ([hlen (string-length haystack)] [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) i]
        [else (loop (+ i 1))]))))

;; The cell at the first row that contains NEEDLE, at NEEDLE's start column, or
;; #f.  The row-text index equals the grid column here because every glyph on a
;; prompt line under test is ASCII width-1, so no wide glyph shifts the mapping;
;; scanning every row keeps the lookup correct even if a long path wrapped.
(define (find-text-cell vt needle)
  (let ([rows (vterm-rows vt)])
    (let lp ([r 0])
      (and (< r rows)
           (let ([idx (string-index (vterm-row-text vt r) needle)])
             (if idx (vterm-cell vt r idx) (lp (+ r 1))))))))

;; The first cell (row-major) whose glyph is CH, or #f.
(define (find-cell-by-glyph vt ch)
  (let ([rows (vterm-rows vt)] [cols (vterm-cols vt)])
    (let row-lp ([r 0])
      (and (< r rows)
           (or (let col-lp ([c 0])
                 (and (< c cols)
                      (let ([cell (vterm-cell vt r c)])
                        (if (and cell (char=? (cell-glyph cell) ch))
                            cell
                            (col-lp (+ c 1))))))
               (row-lp (+ r 1)))))))

;; Render the LIVE left hook into a fresh COLS-wide screen.  Both output ports are
;; the capture port, exactly as the REPL rebinds them while drawing the prompt, so
;; the segments' fd-gated colour still lands because render->screen forces the
;; capability verdict on rather than reading these ports.
(define (render-left cols)
  (render->screen cols
    (lambda (p)
      (parameterize ([current-output-port p] [console-output-port p])
        ((repl-prompt-hook))))))

;; Render the LIVE right hook into a fresh COLS-wide screen.
(define (render-right cols)
  (render->screen cols
    (lambda (p)
      (parameterize ([current-output-port p])
        ((repl-right-prompt-hook))))))

;; --- git-independent proofs (the right hook and the override reach no repository,
;; so these run on every leg) ---

;; A non-zero last status renders the red cross-plus-code badge on the right; the
;; badge cell carries the red 256-index (1), colour having reached the grid.
(enable-informative-prompt!)
(parameterize ([last-status 1] [last-duration 0])
  (let* ([scr (render-right 80)]
         [joined (apply string-append (vterm-lines scr))]
         [cross (find-cell-by-glyph scr #\x2718)])
    (test-assert "preset: a failure renders the cross and code on the right"
      (string-index joined "\x2718;1"))
    (test-assert "preset: the exit badge is present in the grid"
      (and cross #t))
    (test-equal "preset: the exit badge cell is red (256 index 1)"
      1 (cell-fg cross))))

;; A successful last command is silent on the right -- no cross badge at all.
(parameterize ([last-status 0] [last-duration 0])
  (let* ([scr (render-right 80)]
         [joined (apply string-append (vterm-lines scr))])
    (test-assert "preset: success shows no exit badge"
      (not (string-index joined "\x2718;")))))

;; Override wins: a hook rebound AFTER the preset changes the render.  The preset
;; only SETS the hook parameters, so a later rebinding must take effect; a preset
;; that froze the render by value would ignore these and fail.
(enable-informative-prompt!)
(parameterize ([repl-prompt-hook (lambda () (display "custom>" (current-output-port)))])
  (let ([scr (render-left 80)])
    (test-equal "preset: a left hook rebound after the preset wins"
      "custom>" (vterm-row-text scr 0))))
(parameterize ([repl-right-prompt-hook
                 (lambda () (display "RIGHT-OVERRIDE" (current-output-port)))])
  (let* ([scr (render-right 80)]
         [joined (apply string-append (vterm-lines scr))])
    (test-assert "preset: a right hook rebound after the preset wins"
      (string-index joined "RIGHT-OVERRIDE"))))

;; --- two-line layout: the input glyph on its own row, exit-coloured ---
;;
;; The preset composes a `line` segment (the input glyph) after the left info
;; segments, so the left render is "<info>\n<glyph> ": the path (and any branch)
;; on grid row 0, the input glyph on the row below.  The glyph is the heavy angle
;; ❯ under the emoji tier and '>' under the ascii tier, so the cell at (row 1,
;; column 0) is asserted -- a position that holds whichever glyph this terminal
;; renders.  render->screen (inside render-left) forces the capability verdict on,
;; so the glyph's gated colour reaches the cell: green (256 index 2) after a
;; success, red (index 1) after a failure.  A single-line preset would put the
;; glyph on row 0 and leave no row 1, failing the row-count and the (1,0) checks.
(enable-informative-prompt!)

;; The glyph this tier renders -- ❯ (U+276F) under emoji, '>' under ascii -- read
;; from the live verdict so the exact cell can be asserted on either terminal.
(define input-glyph (if (eq? (glyph-tier) 'ascii) #\> #\x276f))

(parameterize ([last-status 0] [last-duration 0])
  (let* ([scr  (render-left 80)]
         [cell (vterm-cell scr 1 0)])
    (test-assert "preset: a second row exists for the input glyph"
      (> (vterm-rows scr) 1))
    (test-assert "preset: the input glyph sits on the row below the info line"
      (and cell (char=? (cell-glyph cell) input-glyph)))
    (test-equal "preset: the input glyph is green (256 index 2) after a success"
      2 (and cell (cell-fg cell)))))

(parameterize ([last-status 1] [last-duration 0])
  (let ([cell (vterm-cell (render-left 80) 1 0)])
    (test-equal "preset: the input glyph is red (256 index 1) after a failure"
      1 (and cell (cell-fg cell)))))

;; --- right-prompt cell alignment (wcwidth): a width-2 glyph lands flush at the
;; right edge, not one column past it ---
;;
;; display-right-prompt reserves (ansi-visible-length str) cells and jumps the
;; cursor to (terminal-width - visible-len + 1).  For a width-2 CJK glyph that is
;; two cells, so on an 80-column screen the glyph starts at column 78 (0-based)
;; and its SECOND cell is the rightmost (79) -- flush at the edge.  A codepoint-
;; counting visible length would reserve one cell, place the glyph at column 79,
;; and the vterm's wcwidth-aware wrap would push it onto the next row at column 0.
;; Both assertions below therefore fail on that pre-fix behaviour -- non-vacuous.

;; The (row . col) of the first cell carrying glyph CH, or #f -- the suite's
;; find-cell-by-glyph reports the cell; this reports its position so the column
;; arithmetic can be checked.
(define (glyph-cell-rc vt ch)
  (let ([rows (vterm-rows vt)] [cols (vterm-cols vt)])
    (let row-lp ([r 0])
      (and (< r rows)
           (or (let col-lp ([c 0])
                 (and (< c cols)
                      (let ([cell (vterm-cell vt r c)])
                        (if (and cell (char=? (cell-glyph cell) ch))
                            (cons r c)
                            (col-lp (+ c 1))))))
               (row-lp (+ r 1)))))))

(let ([wide #\x4e16])   ; 世 -- a CJK ideograph, two display cells wide
  (parameterize ([repl-right-prompt-hook
                   (lambda () (display (string wide) (current-output-port)))]
                 [terminal-width 80])
    (let* ([scr (render->screen 80 (lambda (p) (display-right-prompt p)))]
           [rc  (glyph-cell-rc scr wide)])
      (test-assert "preset: the wide right-prompt glyph reaches the grid"
        (and rc #t))
      ;; Its last occupied cell (start column + width - 1) is column 79, the right
      ;; edge of an 80-wide screen: flush, not overflowing past it.
      (test-equal "preset: a width-2 right prompt lands flush at the right edge"
        (- 80 1) (+ (cdr rc) (char-display-width wide) -1))
      ;; And it stays on the drawn row -- the pre-fix over-reservation wraps it to
      ;; the next row (column 0) instead.
      (test-equal "preset: the wide right prompt does not overflow onto a new row"
        0 (car rc)))))

;; --- git-dependent proofs (temp-repo renders; self-skip when git is absent) ---

;; A unique temp path under $TMPDIR (or /tmp), keyed by pid and a random suffix so
;; concurrent runs never collide.
(define (temp-dir-name tag)
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/hafod-prompt-preset-" tag "-"
                 (number->string (pid)) "-"
                 (number->string (random 1000000))))

;; Run THUNK inside a throwaway git repo on master prepared by SETUP.  A compact
;; duplicate of the git-prompt suite's harness (test-helper duplication across
;; suites is acceptable and avoids a shared-harness edit).  $PWD is set to the repo
;; directory for the render's path segment and both it and the working directory
;; are restored on exit; teardown removes the tree.
(define (with-temp-git-repo setup thunk)
  (let ([dir (temp-dir-name "repo")]
        [old-pwd (getenv "PWD")])
    (create-directory dir)
    (dynamic-wind
      void
      (lambda ()
        (with-cwd* dir
          (lambda ()
            (setenv "PWD" dir)
            (run (git "init" "-q" "-b" "master"))
            (run (git "config" "user.email" "t@t"))
            (run (git "config" "user.name" "t"))
            (run (git "config" "commit.gpgsign" "false"))
            (setup)
            (thunk))))
      (lambda ()
        (setenv "PWD" old-pwd)
        (run (rm "-rf" ,dir))))))

;; Run THUNK (passed the directory) inside a plain temp dir that is NOT a git
;; repository, with $PWD set to it and restored on exit.
(define (with-plain-temp-dir thunk)
  (let ([dir (temp-dir-name "plain")]
        [old-pwd (getenv "PWD")])
    (create-directory dir)
    (dynamic-wind
      void
      (lambda ()
        (with-cwd* dir
          (lambda ()
            (setenv "PWD" dir)
            (thunk dir))))
      (lambda ()
        (setenv "PWD" old-pwd)
        (run (rm "-rf" ,dir))))))

;; Commit a single tracked file so the repo is clean on master.
(define (setup-clean)
  (run (sh "-c" "echo hi > a.txt"))
  (run (git "add" "a.txt"))
  (run (git "commit" "-q" "-m" "base")))

;; Probe for a usable git binary; the temp-repo renders self-skip when it is
;; absent so a git-less leg still passes on the git-independent proofs above.
(define git-present?
  (guard (e [#t #f])
    (pair? (run/strings (git "--version")))))

(when git-present?
  ;; Inside a clean repo: the left line shows the path and the branch, and the
  ;; branch cell is the green 256-index (2).  "master" appears only as the branch
  ;; (the temp path carries no such text), so its cell is the git segment's.
  (with-temp-git-repo setup-clean
    (lambda ()
      (enable-informative-prompt!)
      (let* ([scr (render-left 80)]
             [branch (find-text-cell scr "master")])
        (test-assert "preset: inside a clean repo the branch text is shown"
          (and branch #t))
        (test-equal "preset: inside a clean repo the branch cell is green (256 index 2)"
          2 (cell-fg branch)))))

  ;; Outside any repository (a plain temp dir): the left line shows the path text
  ;; but carries no branch -- the git segment is empty when the cwd is not a repo.
  (with-plain-temp-dir
    (lambda (dir)
      (enable-informative-prompt!)
      (let ([scr (render-left 80)])
        (test-assert "preset: outside a repo the path text is shown"
          (and (find-text-cell scr "hafod-prompt-preset") #t))
        (test-assert "preset: outside a repo no branch text is shown"
          (not (find-text-cell scr "master")))))))

;; === The per-language version group: shown by default, removed by the option ===
;;
;; The preset registers the version group as ONE left segment, so a directory
;; carrying a toolchain marker shows "via <glyph> vX" on the info line out of the
;; box, and (enable-informative-prompt! 'version #f) leaves it out.  The proofs
;; drive a FAKE toolchain -- a real `echo` wrapped in a prompt-tool descriptor,
;; its marker planted in the rendered directory and its PATH gate seeded -- so no
;; real toolchain is required and the rendered token is a fixed, unmistakable
;; string.  Both of the fake's glyphs are ASCII width-1, so nothing the group adds
;; to the info line is wide.

;; echo resolved to an absolute path where present, else the bare (PATH-resolved)
;; name -- the idiom the sibling version suite uses for its fake tools.
(define echo-bin
  (cond [(file-exists? "/bin/echo") "/bin/echo"]
        [(file-exists? "/usr/bin/echo") "/usr/bin/echo"]
        [else "echo"]))

;; The fake toolchain: "vfix.marker" in the rendered directory detects it, echo is
;; its command, and the echoed banner carries the version the shared parser
;; extracts -- so the rendered segment is "via T v7.7.7" (or "via t v7.7.7" on the
;; ascii tier), carrying a token that appears nowhere else on the line.
(define fake-version-tool
  (make-prompt-tool "vfix" '(("vfix.marker") . ()) echo-bin
                    (list echo-bin "Fake 7.7.7") parse-version/common "T" "t" #f))

;; The PATH gate reads the classifier path-cache (decoupled from the OS PATH the
;; spawn itself resolves against), so seed it for echo.
(hashtable-set! (path-cache) echo-bin #t)

;; Write CONTENT to PATH (every path below is fresh under a unique temp dir).
(define (write-file path content)
  (call-with-output-file path (lambda (p) (put-string p content))))

;; The info line (grid row 0) and the whole screen text of a left render: the
;; version group belongs on the info line, so presence is asserted on row 0 and
;; absence across every row.
(define (info-line scr) (vterm-row-text scr 0))
(define (screen-text scr) (apply string-append (vterm-lines scr)))

;; The new key is RECOGNISED -- neither rejected as an unknown option nor
;; swallowed by the odd-length check.  A preset without the option raises here, so
;; the guard yields #f and the row goes red: non-vacuous.
(test-assert "version: the version key is a recognised option"
  (guard (e [#t #f]) (enable-informative-prompt! 'version #t) #t))

;; Presence and gating in a plain (non-repo) temp dir carrying the fake marker.  A
;; preset that never registered the group fails the first row; one that ignored
;; the option fails the second -- both non-vacuous.  The path text is asserted
;; alongside so a leg that lost the whole left line cannot pass silently.
(with-plain-temp-dir
  (lambda (dir)
    (write-file (string-append dir "/vfix.marker") "x")
    (parameterize ([prompt-tools (list fake-version-tool)])
      (enable-informative-prompt!)
      (let ([scr (render-left 160)])
        (test-assert "version: the default preset shows the version group"
          (string-index (info-line scr) "v7.7.7"))
        (test-assert "version: the path text is shown beside it"
          (string-index (info-line scr) "hafod-prompt-preset")))
      (enable-informative-prompt! 'version #f)
      (let ([scr (render-left 160)])
        (test-assert "version: 'version #f omits the version group entirely"
          (not (string-index (screen-text scr) "v7.7.7")))
        (test-assert "version: 'version #f leaves the rest of the left line intact"
          (string-index (info-line scr) "hafod-prompt-preset")))
      (enable-informative-prompt!)
      (let ([scr (render-left 160)])
        (test-assert "version: a following default call restores the group"
          (string-index (info-line scr) "v7.7.7"))))))

;; === The version group's own opt-out, without giving up the prompt ===
;;
;; Changing into a directory runs every detected toolchain's version command
;; there, and several marker filenames are the very files a version-manager shim
;; reads to decide which toolchain to run -- so a cd into an untrusted repository
;; resolves a repository-chosen version through the user's own shim.  Before this
;; the only environment-level escape was HAFOD_PROMPT=0, which removes the whole
;; informative prompt; 'version #f needed an explicit preset call in init.ss.
;;
;; Both narrow opt-outs are proved end to end through the default preset, with
;; the path text asserted alongside so a leg that lost the entire left line
;; cannot pass by accident.  A preset with no group opt-out shows v7.7.7 in every
;; one of these renders: non-vacuous.  The opt-out is read ahead of the cache, so
;; the probe count proves it stopped the work rather than hiding its result.
(with-plain-temp-dir
  (lambda (dir)
    (write-file (string-append dir "/vfix.marker") "x")
    (parameterize ([prompt-tools (list fake-version-tool)])
      (enable-informative-prompt!)
      (setenv "HAFOD_PROMPT_VERSIONS" "0")
      (version-probe-count 0)
      (let ([scr (render-left 160)])
        (test-assert "optout: HAFOD_PROMPT_VERSIONS=0 removes the version group"
          (not (string-index (screen-text scr) "v7.7.7")))
        (test-assert "optout: and leaves the rest of the informative prompt intact"
          (string-index (info-line scr) "hafod-prompt-preset"))
        (test-equal "optout: the disabled group spawns nothing at all"
          0 (version-probe-count)))
      (setenv "HAFOD_PROMPT_VERSIONS" #f)
      (let ([scr (render-left 160)])
        (test-assert "optout: clearing the variable brings the group back"
          (string-index (info-line scr) "v7.7.7")))
      (parameterize ([prompt-versions? #f])
        (let ([scr (render-left 160)])
          (test-assert "optout: (prompt-versions? #f) removes the group too"
            (not (string-index (screen-text scr) "v7.7.7")))
          (test-assert "optout: and still leaves the path segment"
            (string-index (info-line scr) "hafod-prompt-preset")))))))

;; Placement: inside a repository that ALSO carries the fake marker, the info line
;; reads path, then the branch, then the version group -- the group registered
;; AFTER git.  The column order on row 0 is the proof: a preset that registered
;; the group before git would put the version token left of "master" and fail the
;; ordering row.  The input glyph still sits on the row below, so the group joined
;; the info line rather than displacing the two-line layout.  Self-skips when git
;; is absent -- the gating proofs above need no repository.
(when git-present?
  (with-temp-git-repo
    (lambda ()
      (setup-clean)
      (write-file "vfix.marker" "x"))
    (lambda ()
      (parameterize ([prompt-tools (list fake-version-tool)])
        (enable-informative-prompt!)
        (let* ([scr     (render-left 160)]
               [info    (info-line scr)]
               [ipath   (string-index info "hafod-prompt-preset")]
               [ibranch (string-index info "master")]
               [iver    (string-index info "v7.7.7")])
          (test-assert "version: the info line carries the path, the branch and the version"
            (and ipath ibranch iver))
          (test-assert "version: the version group follows the path segment"
            (and ipath iver (< ipath iver)))
          (test-assert "version: the version group follows the git segment"
            (and ibranch iver (< ibranch iver)))
          (test-assert "version: the input glyph still sits on the row below the info line"
            (let ([cell (vterm-cell scr 1 0)])
              (and cell (char=? (cell-glyph cell) input-glyph)))))
        ;; The two options are independent: 'git #f drops the branch and the
        ;; version group closes up directly behind the path.
        (enable-informative-prompt! 'git #f)
        (let* ([scr  (render-left 160)]
               [info (info-line scr)])
          (test-assert "version: 'git #f drops the branch but keeps the version group"
            (and (not (string-index info "master"))
                 (string-index info "v7.7.7")))
          (test-assert "version: with git off the version group still follows the path"
            (< (string-index info "hafod-prompt-preset")
               (string-index info "v7.7.7"))))))))

(test-end)
