(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod shell completers)
        (hafod fuzzy)
        ;; The git-object block drives a throwaway repository through the EPF
        ;; runner, mirroring the prompt-git integration harness.
        (only (hafod syntax) run run/strings)
        (only (hafod process-state) with-cwd* pid)
        (only (hafod fileinfo) create-directory))

(test-begin "Shell Completers")

;; === Registry ===

(test-assert "git completer registered"
  (procedure? (lookup-completer "git")))

(test-assert "ssh completer registered"
  (procedure? (lookup-completer "ssh")))

(test-assert "scp completer registered"
  (procedure? (lookup-completer "scp")))

(test-assert "kill completer registered"
  (procedure? (lookup-completer "kill")))

(test-assert "make completer registered"
  (procedure? (lookup-completer "make")))

(test-assert "unknown command returns #f"
  (not (lookup-completer "nonexistent-cmd-xyz")))

;; === Custom registration ===

(register-completer! "test-cmd" (lambda (prefix ctx) '()))

(test-assert "custom completer registered"
  (procedure? (lookup-completer "test-cmd")))

(test-assert "completer-names includes built-ins"
  (let ([names (completer-names)])
    (and (member "git" names)
         (member "ssh" names)
         (member "make" names))))

;; === Git completer: subcommands ===

(let ([results (git-completer "comm" '((args)))])
  (test-assert "git completer returns results for 'comm'"
    (not (null? results)))
  (test-assert "git completer finds 'commit'"
    (assoc "commit" results))
  ;; Check result format: (name positions desc)
  (test-assert "git result has 3 elements"
    (= (length (car results)) 3))
  (test-assert "git result has positions list"
    (list? (cadr (car results))))
  (test-assert "git result has description"
    (string? (caddr (assoc "commit" results)))))

;; Empty prefix returns all subcommands
(let ([results (git-completer "" '((args)))])
  (test-assert "git empty prefix returns subcommands"
    (> (length results) 10)))

;; === Make completer ===

;; Test with the project's own Makefile
(let ([results (make-completer "comp" '((args)))])
  (test-assert "make completer finds targets"
    ;; Our Makefile should have compile-wpo target
    (or (null? results)  ; no Makefile in test dir
        (pair? results))))

;; === SSH completer ===

;; Just verify it doesn't crash (may return empty if no ssh config)
(let ([results (ssh-completer "test" '((args)))])
  (test-assert "ssh completer returns list"
    (list? results)))

;; === Kill completer ===

;; Should find at least some processes
(let ([results (kill-completer "1" '((args)))])
  (test-assert "kill completer returns list"
    (list? results)))

;; === Shared scanners for the git-object and ~user assertions ===

;; True when NEEDLE appears anywhere in HAYSTACK (the runner has no
;; string-contains).
(define (string-contains? haystack needle)
  (let ([hlen (string-length haystack)] [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

;; True when STR carries no ESC (#x1b) byte -- the invariant a candidate or
;; description drawn from a repository or the user database must satisfy, since a
;; stray escape sequence emitted to the menu could repaint the display.
(define (no-esc? str)
  (let loop ([i 0])
    (cond
      [(>= i (string-length str)) #t]
      [(= (char->integer (string-ref str i)) #x1b) #f]
      [else (loop (+ i 1))])))

;; === Git objects under a throwaway repository (git-presence gated) ===

;; A unique temp path under $TMPDIR (or /tmp), keyed by pid and a random suffix
;; so concurrent runs never collide.
(define (temp-dir-name tag)
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/hafod-completers-" tag "-"
                 (number->string (pid)) "-"
                 (number->string (random 1000000))))

;; Run THUNK inside a throwaway git repo prepared by SETUP: a fresh temp dir is
;; git-init'd with a deterministic identity and branch, SETUP drives it into the
;; wanted state, then THUNK runs with the repo as cwd.  Teardown removes the tree.
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

;; Probe for a usable git binary; the git-object block self-skips when absent so
;; a git-less CI leg still passes on the unit assertions above.
(define git-present?
  (guard (e [#t #f])
    (pair? (run/strings (git "--version")))))

(when git-present?
  ;; A repo carrying one tag, one remote and one stash: each git-object arm must
  ;; surface its object after the matching subcommand, as a strip-cleaned triple
  ;; whose description is the literal type tag.
  (with-temp-git-repo
    (lambda ()
      (setup-clean)
      (run (git "tag" "v0.0"))
      (run (git "remote" "add" "origin" "."))
      (run (sh "-c" "echo stashme >> a.txt"))
      (run (git "stash" "push" "-q")))
    (lambda ()
      (let ([tags (git-completer "" '((args "tag")))]
            [remotes (git-completer "" '((args "push")))]
            [stashes (git-completer "" '((args "stash")))])
        ;; Tag after `git tag`/`git show`.
        (test-assert "git tag completion includes the created tag"
          (assoc "v0.0" tags))
        (test-assert "git tag triple has a string description"
          (let ([t (assoc "v0.0" tags)]) (and t (string? (caddr t)))))
        ;; Remote after `git push`/`pull`/`fetch`/`remote`.
        (test-assert "git push completion includes the remote name"
          (assoc "origin" remotes))
        (test-assert "git remote triple has a string description"
          (let ([r (assoc "origin" remotes)]) (and r (string? (caddr r)))))
        ;; Stash after `git stash`.
        (test-assert "git stash completion includes a stash@{ entry"
          (exists (lambda (r) (string-contains? (car r) "stash@{")) stashes))
        (test-assert "git stash triple has a string description"
          (and (pair? stashes) (string? (caddr (car stashes)))))))))

;; === ~user login -> home completer (unconditional) ===

(let ([results (user-completer "~" '())])
  (test-assert "user completer returns a non-empty list"
    (pair? results))
  (test-assert "every user candidate is a ~-prefixed 3-element triple"
    (for-all (lambda (r)
               (and (list? r) (= (length r) 3)
                    (string? (car r))
                    (> (string-length (car r)) 0)
                    (char=? (string-ref (car r) 0) #\~)))
             results))
  (test-assert "every user candidate is a home-directory description string"
    (for-all (lambda (r) (string? (caddr r))) results))
  ;; Control-byte guard: no returned candidate or description carries an ESC.
  (test-assert "every user candidate and description is ESC-free"
    (for-all (lambda (r) (and (no-esc? (car r)) (no-esc? (caddr r)))) results)))

;; A login-derived prefix narrows to that login.  The current login is used when
;; it is enumerable (it is, on a real system); otherwise the first enumerated
;; entry is taken, so the narrowing assertion always runs and is never vacuous.
(let* ([all (user-completer "~" '())]
       [login-env (or (getenv "LOGNAME") (getenv "USER"))]
       [want-env (and login-env (> (string-length login-env) 0)
                      (string-append "~" login-env))]
       [sample (cond
                 [(and want-env
                       (exists (lambda (r) (string=? (car r) want-env)) all))
                  want-env]
                 [(pair? all) (caar all)]
                 [else #f])])
  (when sample
    (let* ([body (substring sample 1 (string-length sample))]
           [n (min 2 (string-length body))]
           [pfx (substring sample 0 (+ 1 n))]
           [narrowed (user-completer pfx '())])
      (test-assert "a login prefix narrows to include that login"
        (exists (lambda (r) (string=? (car r) sample)) narrowed)))))

;; === Option flags parsed from a command's --help output ===

;; A hand-built fixture standing in for a real command's --help text.  It is
;; deliberately messy, mirroring what actual tools emit: the first flag is
;; wrapped in a genuine OSC-8 hyperlink and SGR-bold run (as GNU coreutils print
;; even down a pipe) with its description on the FOLLOWING indented line; then an
;; =ARG placeholder with a same-line description after two spaces; a long-first
;; pair whose short flag is the non-alphanumeric -@, description on the next
;; line; a comma-joined pair carrying a [no-] negation with a same-line
;; description; a bracketed usage-synopsis fragment and a prose section header
;; that must BOTH yield no flags; and a bare short flag with no description at
;; all.  The escape bytes live only here, in the test data.
(define help-fixture
  (list
    "  \x1b;]8;;https://example.invalid/ls\x1b;\\\x1b;[1m-a, --all\x1b;[0m\x1b;]8;;\x1b;\\"
    "      list everything"
    "  --block-size=SIZE   scale sizes by SIZE"
    "  --all, -@"
    "      do everything"
    "  -q, --[no-]quiet   suppress output"
    "  [-a | --interactive ...]"
    "Commit message options"
    "  -c"))

(define help-parsed (parse-help-output help-fixture))

;; OSC-8 + SGR are stripped, the short and long token share the next-line desc.
(test-assert "help parse: OSC-8/SGR short flag takes the next-line description"
  (member '("-a" . "list everything") help-parsed))
(test-assert "help parse: OSC-8/SGR long flag shares the next-line description"
  (member '("--all" . "list everything") help-parsed))
;; =ARG is tolerated and dropped; the same-line description is kept.
(test-assert "help parse: --long=ARG keeps the plain flag with its same-line desc"
  (member '("--block-size" . "scale sizes by SIZE") help-parsed))
;; Long-first ordering: the long flag and the non-alphanumeric short flag both
;; parse, sharing the next-line description.
(test-assert "help parse: long-first pair yields the long flag"
  (member '("--all" . "do everything") help-parsed))
(test-assert "help parse: long-first pair yields the non-alphanumeric short flag"
  (member '("-@" . "do everything") help-parsed))
;; [no-] is tolerated and normalised away.
(test-assert "help parse: a [no-] negation normalises to the plain long flag"
  (member '("--quiet" . "suppress output") help-parsed))
(test-assert "help parse: the comma-joined short flag keeps its same-line desc"
  (member '("-q" . "suppress output") help-parsed))
;; A bare short flag with no description still completes (empty description).
(test-assert "help parse: a short flag with no description completes with an empty desc"
  (member '("-c" . "") help-parsed))
;; A usage synopsis (leading bracket) contributes no flags -- its --interactive
;; token, unique to that line, must be absent.
(test-assert "help parse: a usage-synopsis fragment yields no options"
  (not (assoc "--interactive" help-parsed)))
;; A prose section header contributes no flags -- 'options' must not be read as
;; a flag token.
(test-assert "help parse: a prose section header yields no options"
  (not (assoc "options" help-parsed)))
;; Exactly the eight expected flag pairs, nothing spurious from the synopsis,
;; the header or the two description lines.
(test-assert "help parse: exactly the eight expected flag pairs"
  (= (length help-parsed) 8))

;; === Option-flag spawn gate, per-command cache and completer ===

;; safe-command-name? is the gate that must pass before any --help/man child is
;; spawned from a user-influenced token: a bare basename or an explicit path
;; built only from the portable command character set is accepted, and anything
;; carrying whitespace or a shell metacharacter is rejected.
(test-assert "safe name: plain basenames and an explicit path pass"
  (and (safe-command-name? "ls")
       (safe-command-name? "git")
       (safe-command-name? "a.out")
       (safe-command-name? "foo-bar_baz")
       (safe-command-name? "/usr/bin/ls")))
(test-assert "safe name: metacharacters, whitespace and the empty string fail"
  (and (not (safe-command-name? "ls; rm"))
       (not (safe-command-name? "a|b"))
       (not (safe-command-name? "$(x)"))
       (not (safe-command-name? "`x`"))
       (not (safe-command-name? "a&b"))
       (not (safe-command-name? "a>b"))
       (not (safe-command-name? ""))
       (not (safe-command-name? "a b"))))

;; A counting stand-in for the real --help/man producer proves the cache and the
;; gate without a live tool.  Each assertion uses a fresh command name so an
;; earlier cache entry cannot mask a later one.

;; Cache hit: the producer runs once across two completions of one command.
(let ([calls 0])
  (parameterize ([command-flag-source
                  (lambda (cmd)
                    (set! calls (+ calls 1))
                    '(("--foo" . "a foo") ("-f" . "a foo")))])
    (command-flags "demo-cache-hit")
    (command-flags "demo-cache-hit")
    (test-assert "cache: the source runs once across two completions"
      (= calls 1))))

;; Empty cached: a flagless command is not re-spawned on the second completion.
(let ([calls 0])
  (parameterize ([command-flag-source
                  (lambda (cmd) (set! calls (+ calls 1)) '())])
    (command-flags "demo-cache-empty")
    (command-flags "demo-cache-empty")
    (test-assert "cache: an empty result is cached, so a flagless command is not re-run"
      (= calls 1))))

;; Slash-bearing name re-probed: a relative or absolute path names a different
;; executable in every working directory, so it must NOT be served from a
;; session cache keyed on the typed string.  The producer runs on BOTH
;; completions of "./demo-slash", where a bare basename runs once (asserted
;; above).
(let ([calls 0])
  (parameterize ([command-flag-source
                  (lambda (cmd) (set! calls (+ calls 1)) '(("--foo" . "a foo")))])
    (command-flags "./demo-slash")
    (command-flags "./demo-slash")
    (test-assert "cache: a slash-bearing name is re-probed, never cached"
      (= calls 2))))

;; Poisoned-cache guard: priming a slash-bearing token under one producer must
;; not let a stale answer leak into a later lookup of the same token.  The
;; second lookup re-probes and sees the fresh flags -- projA's ./build never
;; answers for projB's ./build.
(parameterize ([command-flag-source (lambda (cmd) '(("--old" . "stale")))])
  (command-flags "./demo-poison"))
(parameterize ([command-flag-source (lambda (cmd) '(("--new" . "fresh")))])
  (let ([res (command-flags "./demo-poison")])
    (test-assert "cache: a slash-bearing name is not served a stale poisoned entry"
      (and (assoc "--new" res) (not (assoc "--old" res))))))

;; Gate before producer: an unsafe name yields nothing and never reaches the
;; source, so no child is ever spawned from a metacharacter-bearing token.
(let ([calls 0])
  (parameterize ([command-flag-source
                  (lambda (cmd) (set! calls (+ calls 1)) '(("--x" . "")))])
    (let ([res (command-flags "a; rm")])
      (test-assert "gate: an unsafe command name yields no candidates"
        (null? res))
      (test-assert "gate: an unsafe command name never reaches the source"
        (= calls 0)))))

;; command-flag-completer fuzzes the flag names and returns strip-cleaned
;; (name positions description) triples, sourced through the same gated cache.
(parameterize ([command-flag-source
                (lambda (cmd) '(("--all" . "list all")
                                ("--almost" . "nearly")
                                ("-a" . "list all")))])
  (let ([triples (command-flag-completer "demo-completer" "--al")])
    (test-assert "completer: returns (name positions desc) triples"
      (and (pair? triples)
           (for-all (lambda (tr)
                      (and (list? tr) (= (length tr) 3)
                           (string? (car tr)) (list? (cadr tr))
                           (string? (caddr tr))))
                    triples)))
    (test-assert "completer: the --al prefix includes --all with its description"
      (let ([hit (exists (lambda (tr) (and (string=? (car tr) "--all") tr))
                         triples)])
        (and hit (string=? (caddr hit) "list all"))))))

(test-end)
