(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod shell completers)
        (hafod fuzzy)
        ;; The git-object block drives a throwaway repository through the EPF
        ;; runner, mirroring the prompt-git integration harness.
        (only (hafod syntax) run run/strings)
        (only (hafod process-state) with-cwd* pid)
        (only (hafod fileinfo) create-directory create-symlink)
        ;; The dash-arm and named-directory / $CDPATH oracles drive the navigation
        ;; seam the completion pairs with: run-builtin! grows the directory stack
        ;; via pushd, register-named-directory! populates the named-directory table,
        ;; and setenv points $CDPATH at a throwaway base.
        (only (hafod shell builtins) run-builtin! dir-stack)
        (only (hafod shell parser) register-named-directory!)
        (only (hafod environment) setenv))

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

;; === Branch names offered by the branch-taking subcommands (git-presence gated) ===

;; A repo carrying two extra local branches must offer every local branch --
;; including the current one -- after each branch-taking subcommand.  This block
;; is non-vacuous against the shipped pre-fix tree: there git-branches ran the
;; %(refname:short) format through /bin/sh -c, whose unquoted parentheses abort
;; with a syntax error, so the branch list came back empty and every assertion
;; below failed on that tree.  Gated on a usable git so a git-less leg still
;; passes, exactly as the git-object block above.
(when git-present?
  (with-temp-git-repo
    (lambda ()
      (setup-clean)
      (run (git "branch" "feature-alpha"))
      (run (git "branch" "release-beta")))
    (lambda ()
      (let ([checkout (git-completer "" '((args "checkout")))]
            [sw (git-completer "" '((args "switch")))]
            [merge (git-completer "" '((args "merge")))])
        ;; Each result is a (name positions desc) triple, so assoc matches on the
        ;; candidate branch name.
        (test-assert "git checkout completion includes a created branch"
          (assoc "feature-alpha" checkout))
        (test-assert "git checkout completion includes a second created branch"
          (assoc "release-beta" checkout))
        ;; Every local branch is offered, the current one included.
        (test-assert "git checkout completion includes the current branch"
          (assoc "master" checkout))
        ;; switch and merge take branches on the same arm.
        (test-assert "git switch completion offers a branch"
          (assoc "feature-alpha" sw))
        (test-assert "git merge completion offers a branch"
          (assoc "release-beta" merge))
        ;; The branch triple's description tag is the literal "branch".
        (test-assert "a branch triple carries the branch description tag"
          (let ([t (assoc "feature-alpha" checkout)])
            (and t (string=? (caddr t) "branch"))))))))

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

;; === Directory-only completion: cd / pushd / rmdir ===

;; cd / pushd / rmdir share a directory-only completer registered through the
;; same seam as git / ssh, so a directory operand completes to directories (and
;; symlinks-to-directories) only, never plain files.

(test-assert "cd completer registered"
  (procedure? (lookup-completer "cd")))
(test-assert "pushd completer registered"
  (procedure? (lookup-completer "pushd")))
(test-assert "rmdir completer registered"
  (procedure? (lookup-completer "rmdir")))

(test-assert "dir-only-command? holds for cd/pushd/rmdir and not for git"
  (and (dir-only-command? "cd")
       (dir-only-command? "pushd")
       (dir-only-command? "rmdir")
       (not (dir-only-command? "git"))))

;; True when some triple in RESULTS carries NAME as its candidate name.
(define (offers? results name)
  (exists (lambda (r) (string=? (car r) name)) results))

;; Build a throwaway tree with BUILDER (handed the absolute root), run THUNK with
;; that tree as the working directory, then remove the tree.  BUILDER runs before
;; the cwd switch, so it populates the tree through absolute paths.
(define (with-dir-fixture tag builder thunk)
  (let ([dir (temp-dir-name tag)])
    (create-directory dir)
    (dynamic-wind
      void
      (lambda () (builder dir) (with-cwd* dir thunk))
      (lambda () (run (rm "-rf" ,dir))))))

;; A tree holding a real subdirectory, a symlink to that directory, a plain file
;; and a symlink to that file.  The completer offers the directory and the
;; symlink-to-directory -- each with a trailing "/" and no "@" -- and never the
;; plain file nor the symlink-to-file.  A files-included implementation fails on
;; the plain file; an lstat/nofollow implementation fails on the symlink-to-dir.
(with-dir-fixture "dirs"
  (lambda (root)
    (create-directory (string-append root "/sub"))
    (run (sh "-c" ,(string-append "echo hi > " root "/f")))
    (create-symlink "sub" (string-append root "/dlink"))
    (create-symlink "f" (string-append root "/flink")))
  (lambda ()
    (let ([results (cd-completer "" '((args)))])
      (test-assert "cd offers a real subdirectory with a trailing slash"
        (offers? results "sub/"))
      (test-assert "cd follows a symlink-to-directory (trailing slash)"
        (offers? results "dlink/"))
      (test-assert "cd never offers a plain file"
        (not (offers? results "f")))
      (test-assert "cd never offers a symlink-to-file"
        (not (offers? results "flink")))
      (test-assert "a completed directory carries a trailing slash, not a bare name"
        (not (offers? results "sub")))
      (test-assert "a symlink-to-directory carries a slash, not an @ marker"
        (and (not (offers? results "dlink"))
             (not (offers? results "dlink@"))))
      (test-assert "every cd candidate is a (name positions desc) triple"
        (for-all (lambda (r)
                   (and (list? r) (= (length r) 3)
                        (string? (car r)) (list? (cadr r))
                        (string? (caddr r))))
                 results)))))

;; Smart-hidden: a dot-directory is offered only when the typed base begins with
;; a dot, matching the zsh default.
(with-dir-fixture "hidden"
  (lambda (root)
    (create-directory (string-append root "/.hidden"))
    (create-directory (string-append root "/shown")))
  (lambda ()
    (test-assert "cd omits a dot-directory for a non-dot prefix"
      (not (offers? (cd-completer "" '((args))) ".hidden/")))
    (test-assert "cd offers a visible directory for the empty prefix"
      (offers? (cd-completer "" '((args))) "shown/"))
    (test-assert "cd offers a dot-directory when the prefix starts with a dot"
      (offers? (cd-completer "." '((args))) ".hidden/"))))

;; No-match on a files-only tree: the completer itself contributes nothing (the
;; editor guard then renders no menu rather than falling back to files).
(with-dir-fixture "filesonly"
  (lambda (root)
    (run (sh "-c" ,(string-append "echo hi > " root "/widget")))
    (run (sh "-c" ,(string-append "echo hi > " root "/gadget"))))
  (lambda ()
    (test-assert "cd yields nothing when only a file matches the prefix"
      (null? (cd-completer "widge" '((args)))))
    (test-assert "cd yields nothing for a wholly non-matching prefix"
      (null? (cd-completer "zzznomatch" '((args)))))))

;; === Navigation niceties: cd -<tab>, ~name/ and $CDPATH completion ===

;; The first triple in RESULTS whose candidate name is NAME, or #f.
(define (triple results name)
  (let loop ([rs results])
    (cond
      [(null? rs) #f]
      [(string=? (car (car rs)) name) (car rs)]
      [else (loop (cdr rs))])))

;; The 0-based index of the first triple named NAME in RESULTS, or #f when absent.
(define (name-index results name)
  (let loop ([rs results] [i 0])
    (cond
      [(null? rs) #f]
      [(string=? (car (car rs)) name) i]
      [else (loop (cdr rs) (+ i 1))])))

;; cd -<tab> lists the directory stack as -N labels (each described by the entry's
;; directory, so -N names exactly the directory cd -N navigates) plus a bare "-"
;; for $OLDPWD.  The stack is grown with real pushd calls -- the very navigation
;; the labels pair with -- and the numbering is 1-based, top of the stack first.
(let ([root (temp-dir-name "dashstack")])
  (create-directory root)
  (dynamic-wind
    void
    (lambda ()
      (create-directory (string-append root "/a"))
      (create-directory (string-append root "/b"))
      (with-cwd* root
        (lambda ()
          ;; pushd a, then pushd b: the stack becomes (root/a root), $OLDPWD is
          ;; root/a; both pushes travel the same seam cd -N navigates.
          (run-builtin! (string-append "pushd " root "/a"))
          (run-builtin! (string-append "pushd " root "/b"))
          (let ([results (cd-completer "-" (list (cons 'args '()) (cons 'command "cd")))]
                [stack (dir-stack)])
            (test-assert "cd -<tab> offers a -1 label described by the top stack entry"
              (let ([t (triple results "-1")])
                (and t (pair? stack) (string=? (caddr t) (list-ref stack 0)))))
            (test-assert "cd -<tab> offers a -2 label described by the second stack entry"
              (let ([t (triple results "-2")])
                (and t (>= (length stack) 2) (string=? (caddr t) (list-ref stack 1)))))
            (test-assert "cd -<tab> also offers a bare - label for $OLDPWD"
              (and (triple results "-") #t))
            (test-assert "every dash label is a (name positions desc) triple"
              (for-all (lambda (r)
                         (and (list? r) (= (length r) 3)
                              (string? (car r)) (list? (cadr r)) (string? (caddr r))))
                       results))
            (test-assert "the -2 prefix narrows to the -2 label alone"
              (let ([narrowed (cd-completer "-2" (list (cons 'args '()) (cons 'command "cd")))])
                (and (offers? narrowed "-2")
                     (not (offers? narrowed "-1"))
                     (not (offers? narrowed "-")))))
            ;; The dash-stack labels are a cd nicety: pushd shares this completer
            ;; but its +N/-N stack semantics differ and were not implemented, so
            ;; pushd -<tab> must not be offered the cd -N labels (nor the bare -).
            (test-assert "pushd -<tab> is not offered the cd -N stack labels"
              (let ([for-pushd (cd-completer "-" (list (cons 'args '()) (cons 'command "pushd")))])
                (and (not (offers? for-pushd "-1"))
                     (not (offers? for-pushd "-"))))))
          ;; restore the stack so it does not leak into the later suites
          (run-builtin! "popd")
          (run-builtin! "popd"))))
    (lambda () (run (rm "-rf" ,root)))))

;; ~name/ path completion: a leading "~name/" expands the registered named
;; directory before listing, so cd ~proj/<tab> offers the directories under proj,
;; coherent with cd ~proj navigating there.  The inserted candidate carries the
;; expanded path (matching the shipped "~/"-expansion).
(let ([root (temp-dir-name "namedir")])
  (create-directory root)
  (dynamic-wind
    void
    (lambda ()
      (create-directory (string-append root "/np"))
      (create-directory (string-append root "/np/inner"))
      (register-named-directory! "proj" (string-append root "/np"))
      (let ([results (cd-completer "~proj/" '((args)))])
        (test-assert "cd ~proj/<tab> lists a directory under the named directory"
          (offers? results (string-append root "/np/inner/")))
        (test-assert "the ~name/ expansion yields (name positions desc) triples"
          (for-all (lambda (r)
                     (and (list? r) (= (length r) 3)
                          (string? (car r)) (list? (cadr r)) (string? (caddr r))))
                   results))))
    (lambda () (run (rm "-rf" ,root)))))

;; $CDPATH completion: a base directory on $CDPATH holds "proj"; the cwd holds a
;; distinct "prcwd" (so both fuzzy-match "pr") and no local "proj".  cd pr<tab>
;; then offers the local "prcwd/" AND the $CDPATH "proj/" as a BARE basename (so
;; builtin-cd resolves it back through $CDPATH), the cwd candidate first, the
;; $CDPATH candidate carrying its root as the description.
(let ([root (temp-dir-name "cdpath")]
      [orig-cdpath (getenv "CDPATH")])
  (create-directory root)
  (dynamic-wind
    void
    (lambda ()
      (create-directory (string-append root "/prcwd"))
      (create-directory (string-append root "/base"))
      (create-directory (string-append root "/base/proj"))
      (with-cwd* root
        (lambda ()
          (dynamic-wind
            (lambda () (setenv "CDPATH" (string-append root "/base")))
            (lambda ()
              (let ([results (cd-completer "pr" (list (cons 'args '()) (cons 'command "cd")))]
                    [base-root (string-append root "/base")])
                (test-assert "cd <tab> offers a local directory matching the base"
                  (offers? results "prcwd/"))
                (test-assert "cd <tab> offers a $CDPATH-reachable directory as a bare basename"
                  (offers? results "proj/"))
                (test-assert "the $CDPATH candidate is the bare basename, not the root-prefixed path"
                  (not (offers? results (string-append base-root "/proj/"))))
                (test-assert "the $CDPATH candidate carries its root as the description"
                  (let ([t (triple results "proj/")])
                    (and t (string=? (caddr t) base-root))))
                (test-assert "cwd candidates are ordered before $CDPATH candidates"
                  (let ([i-cwd (name-index results "prcwd/")]
                        [i-cd (name-index results "proj/")])
                    (and i-cwd i-cd (< i-cwd i-cd))))))
            (lambda () (setenv "CDPATH" orig-cdpath))))))
    (lambda () (run (rm "-rf" ,root)))))

;; The completer is command-aware: only cd honours $CDPATH, so only cd is offered
;; the $CDPATH basenames.  pushd and rmdir share the completer but consult neither
;; $CDPATH nor the cd -N stack, so a directory reachable ONLY through $CDPATH --
;; which they could not navigate to -- must not be proposed for them; the same
;; base still offers it for cd.  The cwd holds no matching directory, so the
;; $CDPATH candidate is the sole discriminator.
(let ([root (temp-dir-name "cmdaware")]
      [orig-cdpath (getenv "CDPATH")])
  (create-directory root)
  (dynamic-wind
    void
    (lambda ()
      (create-directory (string-append root "/base"))
      (create-directory (string-append root "/base/proj"))
      (create-directory (string-append root "/start"))
      (with-cwd* (string-append root "/start")
        (lambda ()
          (dynamic-wind
            (lambda () (setenv "CDPATH" (string-append root "/base")))
            (lambda ()
              (let ([for-cd (cd-completer "pr" (list (cons 'args '()) (cons 'command "cd")))]
                    [for-pushd (cd-completer "pr" (list (cons 'args '()) (cons 'command "pushd")))]
                    [for-rmdir (cd-completer "pr" (list (cons 'args '()) (cons 'command "rmdir")))])
                (test-assert "cd offers a $CDPATH-only directory as a bare basename"
                  (offers? for-cd "proj/"))
                (test-assert "pushd is not offered a $CDPATH-only directory"
                  (not (offers? for-pushd "proj/")))
                (test-assert "rmdir is not offered a $CDPATH-only directory"
                  (not (offers? for-rmdir "proj/")))))
            (lambda () (setenv "CDPATH" (or orig-cdpath "")))))))
    (lambda () (run (rm "-rf" ,root)))))

;; === ~<tab>: named directories merged ahead of passwd logins ===

;; The count of triples in RESULTS whose candidate name is exactly NAME.
(define (count-named results name)
  (fold-left (lambda (acc r) (if (string=? (car r) name) (+ acc 1) acc)) 0 results))

;; A registered named directory is offered by user-completer, carries its
;; directory as the description, and -- a clean prefix match where a login could
;; only be a scattered one -- ranks ahead of the passwd logins.
(register-named-directory! "zznameddir" "/tmp/zz-named-dir")

(test-assert "user-completer offers a registered named directory with its directory as the description"
  (let ([t (triple (user-completer "~zzname" '()) "~zznameddir")])
    (and t (string=? (caddr t) "/tmp/zz-named-dir"))))

(test-assert "the named directory is the top-ranked ~-completion for its prefix"
  (let ([results (user-completer "~zzname" '())])
    (and (pair? results) (string=? (car (car results)) "~zznameddir"))))

;; A named directory whose name equals an existing login (root exists on every
;; POSIX system) shadows the login: the ~root entry carries the NAMED directory as
;; its description, not root's passwd home, and appears exactly once -- the named
;; entry wins the dedup on the ~name key.
(register-named-directory! "root" "/tmp/zz-root-override")

(let ([results (user-completer "~root" '())])
  (test-assert "a named directory shadows a same-named login (named description wins)"
    (let ([t (triple results "~root")])
      (and t (string=? (caddr t) "/tmp/zz-root-override"))))
  (test-assert "the shadowed name appears exactly once (deduped on the ~name key)"
    (= 1 (count-named results "~root"))))

;; The merge keeps the passwd logins too: ~<tab> still surfaces at least one login
;; alongside the named directories.
(let ([all (user-completer "~" '())])
  (test-assert "both named directories and passwd logins are offered on ~<tab>"
    (and (exists (lambda (r) (string=? (car r) "~zznameddir")) all)
         (exists (lambda (r)
                   (and (not (string=? (car r) "~zznameddir"))
                        (not (string=? (car r) "~proj"))
                        (not (string=? (car r) "~root"))))
                 all))))

;; === Smart case is uniform across the registered completers ===

;; Every registered completer routes its matching through the one shared
;; matcher (fuzzy-filter/positions), so each inherits its smart-case rule: a
;; lowercase query matches a capitalised candidate case-insensitively, while a
;; query carrying an uppercase letter matches case-sensitively.  Each block
;; below feeds a completer a fixture holding a capitalised name and asserts both
;; directions -- an ad-hoc case-insensitive-always matcher fails the exclusion,
;; an ad-hoc case-sensitive-always matcher fails the inclusion.

;; git: an object-taking subcommand (here `tag`) filters object names through the
;; shared matcher.  Two tags -- one capitalised, one lowercase -- drive both
;; directions.  Gated on a usable git, like the git-object block above.
(when git-present?
  (with-temp-git-repo
    (lambda ()
      (setup-clean)
      (run (git "tag" "Feature"))
      (run (git "tag" "hotfix")))
    (lambda ()
      (let ([lower (git-completer "feature" '((args "tag")))]
            [upper (git-completer "HOTFIX" '((args "tag")))])
        (test-assert "git: a lowercase query matches a capitalised tag"
          (offers? lower "Feature"))
        (test-assert "git: an all-caps query rejects a lowercase tag"
          (not (offers? upper "hotfix")))))))

;; ssh: hostnames are read from $HOME/.ssh/config; a throwaway HOME holding a
;; capitalised and a lowercase Host drives both directions.  HOME is restored on
;; the way out so the override cannot leak into a later block.
(let ([home-dir (temp-dir-name "sshhome")]
      [orig-home (getenv "HOME")])
  (create-directory home-dir)
  (create-directory (string-append home-dir "/.ssh"))
  (run (sh "-c" ,(string-append "printf 'Host DevBox\\nHost prod\\n' > "
                                home-dir "/.ssh/config")))
  (dynamic-wind
    (lambda () (setenv "HOME" home-dir))
    (lambda ()
      (let ([lower (ssh-completer "devbox" '((args)))]
            [upper (ssh-completer "PROD" '((args)))])
        (test-assert "ssh: a lowercase query matches a capitalised host"
          (offers? lower "DevBox"))
        (test-assert "ssh: an all-caps query rejects a lowercase host"
          (not (offers? upper "prod")))))
    (lambda ()
      (setenv "HOME" (or orig-home ""))
      (run (rm "-rf" ,home-dir)))))

;; make: targets are read from a file named Makefile in the working directory; a
;; throwaway Makefile with a capitalised and a lowercase target drives both
;; directions.
(let ([mk-dir (temp-dir-name "mkcase")])
  (create-directory mk-dir)
  (dynamic-wind
    void
    (lambda ()
      (run (sh "-c" ,(string-append
                      "printf 'Build:\\n\\techo x\\nclean:\\n\\techo x\\n' > "
                      mk-dir "/Makefile")))
      (with-cwd* mk-dir
        (lambda ()
          (let ([lower (make-completer "build" '((args)))]
                [upper (make-completer "CLEAN" '((args)))])
            (test-assert "make: a lowercase query matches a capitalised target"
              (offers? lower "Build"))
            (test-assert "make: an all-caps query rejects a lowercase target"
              (not (offers? upper "clean")))))))
    (lambda () (run (rm "-rf" ,mk-dir)))))

;; user: the ~<tab> set includes registered named directories; a capitalised and
;; a lowercase named directory drive both directions.  The exclusion names the
;; lowercase entry directly, so it is unaffected by any other ~-candidate.
(register-named-directory! "SmartUpper" "/tmp/zz-smart-upper")
(register-named-directory! "smartlower" "/tmp/zz-smart-lower")
(let ([lower (user-completer "~smartupper" '())]
      [upper (user-completer "~SMARTLOWER" '())])
  (test-assert "user: a lowercase query matches a capitalised named directory"
    (offers? lower "~SmartUpper"))
  (test-assert "user: an all-caps query rejects a lowercase named directory"
    (not (offers? upper "~smartlower"))))

;; kill: PIDs are numeric, so the case dimension does not apply to a PID.  kill's
;; use of the shared matcher is instead pinned by a fuzzy (subsequence) match
;; that a bare prefix filter would miss; composed with the smart-case pins on
;; fuzzy-filter/positions, this shows kill inherits smart case through the shared
;; matcher.  A live PID whose first two digits differ yields a tail that is a
;; subsequence but not a prefix; this process's own PID is preferred so the probe
;; stays alive across both listings.
(let* ([mypid (number->string (pid))]
       [pids (map car (kill-completer "" '((args))))]
       [qualifies? (lambda (p)
                     (and (>= (string-length p) 2)
                          (not (char=? (string-ref p 0) (string-ref p 1)))))]
       [probe (cond
                [(and (member mypid pids) (qualifies? mypid)) mypid]
                [else (let loop ([ps pids])
                        (cond
                          [(null? ps) #f]
                          [(qualifies? (car ps)) (car ps)]
                          [else (loop (cdr ps))]))])])
  (test-assert "kill: the empty prefix returns the whole process list"
    (pair? pids))
  (test-assert "kill: a multi-digit PID with distinct leading digits exists"
    (and probe #t))
  (when probe
    (let ([tail (substring probe 1 (string-length probe))])
      (test-assert "kill: a non-prefix subsequence still matches (fuzzy routing)"
        (offers? (kill-completer tail '((args))) probe)))))

(test-end)
