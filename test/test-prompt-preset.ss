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
;;; the preset still wins.  render->screen forces the terminal-capability verdict
;;; on, so the segments' gated 256-colour reaches the cell grid.
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
              prompt-path-segment)
        (only (hafod environment) getenv setenv)
        (only (hafod fileinfo) create-directory)
        (only (hafod syntax) run run/strings)
        (only (hafod process-state) with-cwd* pid)
        (only (hafod terminal-caps) assume-terminal-caps))

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

(test-end)
