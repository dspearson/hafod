#!chezscheme
;;; test-prompt-git.ss -- Git-state prompt segment: pure porcelain-v2 parse table,
;;; temp-git-repo integration states, and the fd-1 colour-gate seam.
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
;; The integration block shells out (create-directory, the run EPF), so the full
;; chezscheme surface is imported -- except getenv, taken from (hafod environment)
;; below for its presence-aware semantics.
(import (except (chezscheme) getenv)
        (test runner)
        (only (hafod interactive)
              parse-git-porcelain-v2 prompt-git-segment prompt-colour-ok?
              sanitize-control
              ;; The CACHED, spawn-timeout-bounded segment -- the one the default
              ;; prompt actually draws -- plus the collector behind it and its
              ;; deadline knob, for the large-working-tree proof at the end.
              cached-git-segment with-spawn-timeout prompt-spawn-timeout-ms)
        (only (hafod environment) getenv setenv)
        (only (hafod fileinfo) create-directory)
        (only (hafod syntax) run run/strings)
        (only (hafod process-state) with-cwd* pid)
        (only (hafod posix) posix-waitpid)
        (only (hafod terminal-caps) assume-terminal-caps))

(test-begin "prompt-git")

;; Pin CLICOLOR_FORCE unset, mirroring the sibling prompt suite: an ambient
;; CLICOLOR_FORCE=1 could force colour on a non-tty sink and perturb the
;; capability-gated colour assertions added alongside the integration states.
(setenv "CLICOLOR_FORCE" #f)

;; === parse-git-porcelain-v2 -- pure parser over `git status --porcelain=v2
;; --branch` lines (six values: head oid ahead behind staged? dirty?) ===
;;
;; Each case is a synthetic line list lifted from the verified git 2.53.0 output;
;; the parser does no I/O, so these run with no git binary present.  A single
;; 40-char hex oid is reused wherever the exact oid value is asserted.

(define sha "0123456789abcdef0123456789abcdef01234567")

;; Collect the six returned values into a list for a single equal? comparison.
(define (p6 lines)
  (call-with-values (lambda () (parse-git-porcelain-v2 lines)) list))

;; Clean committed tree: head + oid headers only, no entry lines.
(test-equal "parse: clean committed"
  (list "master" sha 0 0 #f #f)
  (p6 (list (string-append "# branch.oid " sha)
            "# branch.head master")))

;; Unstaged modification (`1 .M ...`): X column ".", Y column "M" -> dirty only.
(test-equal "parse: unstaged dirty"
  (list "master" sha 0 0 #f #t)
  (p6 (list (string-append "# branch.oid " sha)
            "# branch.head master"
            "1 .M N... 100644 100644 100644 aaaa bbbb a.txt")))

;; Staged modification (`1 M. ...`): X "M", Y "." -> staged only.
(test-equal "parse: staged"
  (list "master" sha 0 0 #t #f)
  (p6 (list (string-append "# branch.oid " sha)
            "# branch.head master"
            "1 M. N... 100644 100644 100644 aaaa bbbb a.txt")))

;; Staged and further modified (`1 MM ...`): both columns set -> staged AND dirty.
(test-equal "parse: staged and dirty"
  (list "master" sha 0 0 #t #t)
  (p6 (list (string-append "# branch.oid " sha)
            "# branch.head master"
            "1 MM N... 100644 100644 100644 aaaa bbbb a.txt")))

;; Untracked only (`? path`): counts as dirty (the locked untracked-as-dirty
;; policy), staged stays false.
(test-equal "parse: untracked only is dirty"
  (list "master" sha 0 0 #f #t)
  (p6 (list (string-append "# branch.oid " sha)
            "# branch.head master"
            "? untracked.txt")))

;; Unmerged (`u UU ...`): a conflict counts as both staged and dirty.
(test-equal "parse: unmerged is staged and dirty"
  (list "master" sha 0 0 #t #t)
  (p6 (list (string-append "# branch.oid " sha)
            "# branch.head master"
            "u UU N... 100644 100644 100644 100644 aa bb cc dd conflict.txt")))

;; Ahead/behind: `# branch.ab +1 -2` -> ahead 1, behind 2 (magnitudes, sign
;; dropped); the upstream header line is ignored.
(test-equal "parse: ahead and behind"
  (list "master" sha 1 2 #f #f)
  (p6 (list (string-append "# branch.oid " sha)
            "# branch.head master"
            "# branch.upstream origin/master"
            "# branch.ab +1 -2")))

;; No upstream: without a `# branch.ab` line ahead/behind default to 0, even with
;; local changes present (a fresh branch has no upstream and no ab line).
(test-equal "parse: no upstream defaults ahead/behind to zero"
  (list "feature" sha 0 0 #f #t)
  (p6 (list (string-append "# branch.oid " sha)
            "# branch.head feature"
            "1 .M N... 100644 100644 100644 aaaa bbbb a.txt")))

;; Detached HEAD: head is the literal "(detached)", oid a real sha.
(test-equal "parse: detached head"
  (list "(detached)" sha 0 0 #f #f)
  (p6 (list (string-append "# branch.oid " sha)
            "# branch.head (detached)")))

;; Unborn (initial) repository: oid is the literal "(initial)", head a real name.
(test-equal "parse: unborn initial"
  (list "master" "(initial)" 0 0 #f #f)
  (p6 (list "# branch.oid (initial)"
            "# branch.head master")))

;; Unborn with a staged file (`1 A. ...` before the first commit): head kept,
;; oid still "(initial)", staged flagged.
(test-equal "parse: unborn with staged file"
  (list "master" "(initial)" 0 0 #t #f)
  (p6 (list "# branch.oid (initial)"
            "# branch.head master"
            "1 A. N... 000000 100644 100644 0000 abcd a.txt")))

;; Empty line list (git produced no stdout -> not a repository): head is #f.
(test-equal "parse: empty list is not a repo"
  (list #f #f 0 0 #f #f)
  (p6 '()))

;; === prompt-git-segment -- single-spawn coloured, sanitised, fail-quiet ===

;; Substring search (the runner has no string-contains).
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

;; --- Deterministic, git-free seams (run unconditionally) ---

;; The default colour verdict routes through the capability gate on fd 1: forced
;; off it is false, forced on it is true.  This anchors the seam to colour-ok?,
;; independently of any repository.
(parameterize ([assume-terminal-caps 'off])
  (test-assert "verdict: default false off a terminal" (not ((prompt-colour-ok?)))))
(parameterize ([assume-terminal-caps 'on])
  (test-assert "verdict: default true on a terminal" ((prompt-colour-ok?))))

;; Branch-head sanitisation: an ESC embedded in a "# branch.head" line is parsed
;; through, then stripped on the compose path before display.
(test-assert "sanitise: ESC stripped directly"
  (no-esc? (sanitize-control
             (string-append "a" (string (integer->char #x1b)) "b"))))
(test-assert "sanitise: ESC in branch.head stripped before display"
  (let-values ([(head oid ahead behind staged? dirty?)
                (parse-git-porcelain-v2
                  (list (string-append "# branch.oid " sha)
                        (string-append "# branch.head ma"
                                       (string (integer->char #x1b)) "ster")))])
    (and (no-esc? (sanitize-control head))
         (string=? (sanitize-control head) "master"))))

;; --- Temp-git-repo integration (git-presence gated) ---

;; A unique temp path under $TMPDIR (or /tmp), keyed by pid and a random suffix
;; so concurrent runs never collide.
(define (temp-dir-name tag)
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/hafod-prompt-git-" tag "-"
                 (number->string (pid)) "-"
                 (number->string (random 1000000))))

;; Run THUNK inside a throwaway git repo prepared by SETUP.  A fresh temp dir is
;; created, git init seeded with a deterministic identity and branch, SETUP run
;; to drive the repo into the wanted state, then THUNK.  Teardown removes the
;; tree recursively.  All setup uses the argv-exec EPF.
(define (with-temp-git-repo setup thunk)
  (let ([dir (temp-dir-name "repo")])
    (create-directory dir)
    (dynamic-wind
      void
      (lambda ()
        (with-cwd* dir
          (lambda ()
            (run (git "init" "-q" "-b" "master"))
            (run (git "config" "user.email" "t@t"))
            (run (git "config" "user.name" "t"))
            (run (git "config" "commit.gpgsign" "false"))
            (setup)
            (thunk))))
      (lambda () (run (rm "-rf" ,dir))))))

;; Commit a single tracked file so the repo is clean on master.
(define (setup-clean)
  (run (sh "-c" "echo hi > a.txt"))
  (run (git "add" "a.txt"))
  (run (git "commit" "-q" "-m" "base")))

;; Probe for a usable git binary; the integration block self-skips when absent so
;; a git-less CI leg still passes (the parse table and the seams above do not).
(define git-present?
  (guard (e [#t #f])
    (pair? (run/strings (git "--version")))))

(when git-present?
  ;; Clean committed -> branch name, no markers; plain text carries no ESC.
  (with-temp-git-repo setup-clean
    (lambda ()
      (parameterize ([assume-terminal-caps 'off])
        (test-assert "segment: clean shows branch and no markers"
          (let ([s (prompt-git-segment)])
            (and (string-contains? s "master")
                 (not (string-contains? s "*"))
                 (not (string-contains? s "+")))))
        (test-assert "segment: clean carries no ESC under a false verdict"
          (no-esc? (prompt-git-segment))))
      ;; fd-1 colour gate: the segment colours iff prompt-colour-ok? says so,
      ;; independently of the output port.  A port-gated implementation would
      ;; emit plain text here (the sink is not a tty) and fail the first assert.
      (parameterize ([prompt-colour-ok? (lambda () #t)])
        (test-assert "segment: 256-colour SGR when the verdict is true"
          (string-contains? (prompt-git-segment) "38;5;")))
      (parameterize ([prompt-colour-ok? (lambda () #f)])
        (test-assert "segment: plain (no ESC) when the verdict is false"
          (no-esc? (prompt-git-segment))))))

  ;; Reap regression (Linux-gated; self-skips where /proc is absent): the segment
  ;; reaps its git child on every draw, so a burst of draws leaves no unreaped
  ;; child behind.  When the child was left unwaited it lingered until the next
  ;; collection, so a low-allocation burst of prompt draws could fill the process
  ;; table on the hottest interactive path.  The count is taken with a
  ;; non-blocking waitpid rather than by walking /proc for defunct entries on
  ;; purpose: a /proc walk allocates heavily and would trigger the very collection
  ;; whose guardian reaps these children anyway, masking a genuine leak and
  ;; leaving the proof toothless.  A waitpid poll allocates almost nothing, so a
  ;; tree without the synchronous reap still shows its lingering children here and
  ;; fails.  A first drain clears the reapable children the run-based scaffolding
  ;; leaves (a shell keeps waited-for processes around); after the draws, a
  ;; correctly-reaping segment leaves nothing to drain.
  (when (file-exists? "/proc")
    (with-temp-git-repo setup-clean
      (lambda ()
        ;; Reap and count every already-finished child of this process.  A
        ;; non-blocking waitpid (the 1 is WNOHANG) returns a positive pid for each
        ;; finished child in turn, 0 while a child is still running, and raises
        ;; when no children remain at all -- swallowed here as "none left".
        (define (drain-finished-children)
          (let loop ([n 0])
            (let ([r (guard (e [#t 'none])
                       (let-values ([(w s) (posix-waitpid -1 1)]) w))])
              (cond
                [(eq? r 'none) n]
                [(and (integer? r) (> r 0)) (loop (+ n 1))]
                [else n]))))
        (drain-finished-children)   ; clear any child left by earlier scaffolding
        (do ([i 0 (+ i 1)]) ((= i 40)) (prompt-git-segment))
        (test-assert "segment: repeated draws leave no unreaped git child"
          (= (drain-finished-children) 0)))))

  ;; Unstaged modification -> "*".
  (with-temp-git-repo
    (lambda () (setup-clean) (run (sh "-c" "echo more >> a.txt")))
    (lambda ()
      (parameterize ([assume-terminal-caps 'off])
        (test-assert "segment: dirty shows *"
          (string-contains? (prompt-git-segment) "*")))))

  ;; Staged modification -> "+", no "*".
  (with-temp-git-repo
    (lambda () (setup-clean) (run (sh "-c" "echo more >> a.txt"))
               (run (git "add" "a.txt")))
    (lambda ()
      (parameterize ([assume-terminal-caps 'off])
        (test-assert "segment: staged shows + and not *"
          (let ([s (prompt-git-segment)])
            (and (string-contains? s "+") (not (string-contains? s "*"))))))))

  ;; Staged then further modified -> "*+".
  (with-temp-git-repo
    (lambda () (setup-clean) (run (sh "-c" "echo more >> a.txt"))
               (run (git "add" "a.txt")) (run (sh "-c" "echo evenmore >> a.txt")))
    (lambda ()
      (parameterize ([assume-terminal-caps 'off])
        (test-assert "segment: staged+dirty shows *+"
          (string-contains? (prompt-git-segment) "*+")))))

  ;; Ahead of a (local) upstream by one commit -> the ahead glyph.
  (with-temp-git-repo
    (lambda ()
      (run (git "commit" "-q" "--allow-empty" "-m" "base"))
      (run (git "branch" "upstream"))
      (run (git "branch" "--set-upstream-to=upstream" "master"))
      (run (git "commit" "-q" "--allow-empty" "-m" "extra")))
    (lambda ()
      (parameterize ([assume-terminal-caps 'off])
        (test-assert "segment: ahead shows the up glyph"
          (string-contains? (prompt-git-segment) "\x21e1;")))))

  ;; Detached HEAD -> "@" + 7-char short sha (8 chars total).
  (with-temp-git-repo
    (lambda () (setup-clean) (run (git "checkout" "-q" "--detach")))
    (lambda ()
      (parameterize ([assume-terminal-caps 'off])
        (let ([s (prompt-git-segment)])
          (test-assert "segment: detached begins with @"
            (and (> (string-length s) 0) (char=? (string-ref s 0) #\@)))
          (test-equal "segment: detached is @ plus a 7-char sha"
            8 (string-length s))))))

  ;; Outside any repository -> the empty string (fail-quiet, no stderr leak).
  (let ([dir (temp-dir-name "plain")])
    (create-directory dir)
    (dynamic-wind
      void
      (lambda ()
        (with-cwd* dir
          (lambda ()
            (parameterize ([assume-terminal-caps 'off])
              (test-equal "segment: outside a repo is empty"
                "" (prompt-git-segment))))))
      (lambda () (run (rm "-rf" ,dir))))))

;; === A large working tree still renders its branch (the cached, bounded path) ===
;;
;; The default prompt draws cached-git-segment, which renders `git status
;; --porcelain=v2 --branch` through the bounded collector, maps a cut-short read
;; to "", and then CACHES that "" against the HEAD/index stamp -- so the segment
;; stays gone until the stamp moves.  A byte ceiling applied inside the collector
;; and reported as a timeout therefore deleted the segment outright on any tree
;; with a few hundred changed or untracked entries: porcelain v2 costs roughly
;; 150 bytes apiece, so an ordinary bulk reformat, a vendored drop or an
;; unignored build directory clears 64 KB.  The branch header the renderer needs
;; sits at the very front of the capture, so nothing about the answer was
;; actually missing.
;;
;; The fixture plants 340 untracked files with 200-character names -- some 70 KB
;; of porcelain, asserted here so the proof cannot quietly go vacuous if the
;; padding or the format changes.  The deadline is raised well clear of the
;; default 150 ms so the wall clock cannot be what cuts the read short: the only
;; thing on trial is the byte count.
(when git-present?
  (with-temp-git-repo
    (lambda ()
      (setup-clean)
      ;; One shell loop and one long name prefix: 340 x ~205 bytes of `? <path>`.
      (let ([pad (make-string 200 #\u)])
        (run (sh "-c" ,(string-append "i=0; while [ $i -lt 340 ]; do : > \""
                                      pad "$i\"; i=$((i+1)); done")))))
    (lambda ()
      (let ([old-pwd (getenv "PWD")])
        (dynamic-wind
          ;; cached-git-segment keys on the LOGICAL cwd; with PWD unset that is
          ;; the real one, which with-temp-git-repo has already made the fixture.
          (lambda () (setenv "PWD" #f))
          (lambda ()
            (parameterize ([assume-terminal-caps 'off]
                           [prompt-spawn-timeout-ms 10000])
              (let-values ([(out timed-out?)
                            (with-spawn-timeout
                              "git"
                              '("git" "status" "--porcelain=v2" "--branch")
                              10000)])
                (test-assert "fixture: the porcelain payload really exceeds 64 KB"
                  (> (string-length out) 65536))
                (test-assert "collector: a large porcelain read is not a timeout"
                  (not timed-out?)))
              (test-assert "segment: a large working tree still shows its branch"
                (string-contains? (cached-git-segment) "master"))))
          (lambda () (setenv "PWD" old-pwd)))))))

(test-end)
