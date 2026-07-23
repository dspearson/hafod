;;; (hafod shell completers) -- Programmable command-specific completions
;;; Provides a registry mapping command names to completer procedures,
;;; plus built-in completers for git, ssh, kill, and make.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod shell completers)
  (export register-completer! lookup-completer completer-names
          completers-registered? strip-terminal-escapes
          git-completer
          ssh-completer kill-completer make-completer user-completer
          cd-completer dir-only-command?
          parse-help-output safe-command-name? command-flag-source
          command-flags command-flag-completer)
  (import (except (chezscheme) getenv)
          ;; HOME (for a leading "~/" in the directory completer) is read through
          ;; hafod's cache-aware getenv, so it agrees with the Scheme-side
          ;; environment the shell mutates rather than Chez's raw view.
          (only (hafod environment) getenv)
          (only (hafod posix) posix-waitpid posix-getpwent-all
                passwd-info-name passwd-info-dir)
          (only (hafod shell classifier) path-cache)
          ;; The cd completer's dash arm sources its -N labels from the directory
          ;; stack; it also shares the colon splitter and the $CDPATH enumerator
          ;; with the cd builtin, so the "empty element = cwd" rule lives in one
          ;; place.  Its ~name/ arm expands a named directory before listing.  All
          ;; one-way edges -- neither builtins nor parser imports this library --
          ;; so there is no cycle.
          (only (hafod shell builtins) dir-stack split-on-colon cdpath-entries)
          (only (hafod shell parser) named-directory-ref named-directories-list)
          ;; The git branch listing is collected as a verbatim argument vector:
          ;; exec-path execs the tool directly and run/strings* captures its stdout
          ;; lines while reaping the child synchronously.  A joined /bin/sh -c
          ;; command string cannot be used there because the %(refname:short)
          ;; format carries parentheses a shell would read as subshell
          ;; metacharacters.  Both edges run strictly downward -- neither collect
          ;; nor process imports this library -- so there is no cycle.
          (only (hafod collect) run/strings*)
          (only (hafod process) exec-path)
          (hafod fuzzy))

  ;; Registry: command-name → completer procedure
  ;; A completer is (lambda (prefix context) -> list of (name . description))
  ;; where context is an alist with keys like 'args (previous args on line).
  (define *completers* (make-hashtable string-hash string=?))

  ;; The built-in completers register on the first lookup, not at library
  ;; instantiation, so a non-interactive invocation never pays for them.  The
  ;; flag doubles as a probe; set before registering, it keeps
  ;; ensure-builtin-completers! idempotent and non-re-entrant.  Single-threaded
  ;; REPL, so a bare flag guard is sufficient.
  (define %completers-registered? #f)

  (define (register-completer! cmd proc)
    (hashtable-set! *completers* cmd proc))

  (define (lookup-completer cmd)
    (ensure-builtin-completers!)
    (hashtable-ref *completers* cmd #f))

  (define (completer-names)
    (ensure-builtin-completers!)
    (vector->list (hashtable-keys *completers*)))

  (define (completers-registered?) %completers-registered?)

  ;; ======================================================================
  ;; Helper: run a command and collect output lines
  ;; ======================================================================

  ;; Run a command and collect its stdout lines, closing both child pipes and
  ;; reaping the child on every exit path.  A completion spawns a child per query
  ;; (git objects, an option probe, a manual page), so an unreaped child here
  ;; would linger as a zombie until the next collection and, across a long
  ;; session of completions, steadily fill the process table.  The line-reading
  ;; loop is therefore wrapped in a dynamic-wind whose cleanup thunk closes
  ;; stdout then stderr and then blocking-waits on the child: the wait returns at
  ;; once because stdout has already reached end-of-file, so the child has
  ;; already finished, and a child a garbage collector guardian has already
  ;; reaped surfaces as an error that is swallowed.  Placing the port closes and
  ;; the wait in the cleanup thunk runs them on the normal, end-of-file and error
  ;; paths alike, so a read that throws still closes the ports and reaps.  A spawn
  ;; failure -- the tool is absent -- is caught by the outer guard and read as an
  ;; empty result.
  (define (command-output cmd . args)
    (guard (e [#t '()])
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports
                      (apply string-append cmd
                             (map (lambda (a) (string-append " " a)) args))
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

  ;; ======================================================================
  ;; Helper: strip terminal escape sequences and stray control bytes
  ;; ======================================================================

  ;; Reduce any string to its visible text, removing whole terminal escape
  ;; sequences and stray control bytes.  A candidate or description drawn from a
  ;; subprocess, the passwd database or a filename may legally carry an ESC-led
  ;; escape sequence (a CSI colour run, an OSC-8 hyperlink) or a bare control
  ;; byte; emitted verbatim to the menu those bytes could repaint the line, move
  ;; the cursor or otherwise spoof the display, and they also confuse the option
  ;; parser that reads a tool's help text.  A single left-to-right walk consumes
  ;; each whole sequence and drops every stray control byte, so only the visible
  ;; glyphs remain.  On ESC a following "[" opens a CSI consumed up to and
  ;; including its final byte in the #x40..#x7e range; a following "]" opens an
  ;; OSC consumed up to and including its BEL (#x07) or two-byte ST (ESC "\")
  ;; terminator; any other byte is a two-byte ESC whose single following byte is
  ;; dropped.  A sequence truncated at end-of-string stops safely.  Every other
  ;; byte is emitted only when its code is at least space and is neither DEL
  ;; (#x7f) nor in the C1 range (#x80..#x9f), so carriage return, line feed, tab,
  ;; backspace and the other C0 controls are removed too; the C1 range is dropped
  ;; because a UTF-8 terminal can read those code points as an eight-bit CSI or
  ;; OSC introducer -- a second escape route the text must not reach.  Merely
  ;; dropping the ESC while leaving the sequence body would still let the body's
  ;; bytes through, so the whole sequence is consumed.
  (define (strip-terminal-escapes str)
    (let ([len (string-length str)]
          [out (open-output-string)])
      ;; From k (just after "["), consume a CSI up to and including the first
      ;; final byte in #x40..#x7e; a CSI with no final byte stops at the end.
      (define (skip-csi k)
        (let scan ([j k])
          (cond
            [(>= j len) len]
            [(let ([c (char->integer (string-ref str j))])
               (and (>= c #x40) (<= c #x7e)))
             (+ j 1)]
            [else (scan (+ j 1))])))
      ;; From k (just after "]"), consume an OSC up to and including a BEL or a
      ;; two-byte ST (ESC "\"); an unterminated OSC stops at the end.
      (define (skip-osc k)
        (let scan ([j k])
          (cond
            [(>= j len) len]
            [(char=? (string-ref str j) #\x07) (+ j 1)]
            [(and (char=? (string-ref str j) #\x1b)
                  (< (+ j 1) len)
                  (char=? (string-ref str (+ j 1)) #\\))
             (+ j 2)]
            [else (scan (+ j 1))])))
      (let loop ([i 0])
        (if (>= i len)
            (get-output-string out)
            (let ([ch (string-ref str i)])
              (if (char=? ch #\x1b)
                  (if (>= (+ i 1) len)
                      (get-output-string out) ; a bare trailing ESC: stop safely
                      (let ([next (string-ref str (+ i 1))])
                        (cond
                          [(char=? next #\[) (loop (skip-csi (+ i 2)))]
                          [(char=? next #\]) (loop (skip-osc (+ i 2)))]
                          [else (loop (+ i 2))]))) ; a two-byte ESC: drop both
                  (let ([c (char->integer ch)])
                    (when (and (>= c #x20)
                               (not (= c #x7f))
                               (not (and (>= c #x80) (<= c #x9f))))
                      (write-char ch out))
                    (loop (+ i 1)))))))))

  ;; ======================================================================
  ;; Helper: parse option flags out of a command's --help / man output
  ;; ======================================================================

  ;; A command's own --help (or, failing that, its manual page) is the source of
  ;; its option flags for completion.  Real help text is far messier than a
  ;; two-space-separated table: coreutils wrap each flag in an OSC-8 hyperlink and
  ;; an SGR-bold run even when the output is a pipe, so every line is escape-
  ;; stripped before it is read; a description sits on the same line after a wide
  ;; gap or spills onto the next indented line; a flag may carry an "=ARG" or a
  ;; separate placeholder, an embedded negation, or come long-first with a
  ;; non-alphanumeric short partner.  The parser below is pure -- it takes the
  ;; already-collected lines and returns an association of flag name to
  ;; description -- so it is exercised over a fixed fixture with no subprocess.
  ;; Anything it cannot make sense of contributes nothing, since offering a wrong
  ;; flag is worse than offering none.

  ;; Index of the first non-space character in STR, or #f when STR is empty or
  ;; entirely spaces.  Only the plain space marks indentation; every other
  ;; control byte has already been removed by the escape strip.
  (define (first-non-space str)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref str i) #\space) (loop (+ i 1))]
          [else i]))))

  ;; STR with any trailing whitespace removed.
  (define (right-trim str)
    (let loop ([i (string-length str)])
      (if (and (> i 0) (char-whitespace? (string-ref str (- i 1))))
          (loop (- i 1))
          (substring str 0 i))))

  ;; The prefix of STR up to (not including) the first CH, or STR whole when CH
  ;; is absent.  Used to shed an argument placeholder glued to a flag with "="
  ;; or "<".
  (define (before-char str ch)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(>= i len) str]
          [(char=? (string-ref str i) ch) (substring str 0 i)]
          [else (loop (+ i 1))]))))

  ;; STR with the first embedded negation marker removed, so a negatable long
  ;; flag reduces to its plain form.  The marker is built character-wise rather
  ;; than written as a literal.
  (define (drop-negation-marker str)
    (let* ([len (string-length str)]
           [marker (string #\[ #\n #\o #\- #\])]
           [mlen (string-length marker)])
      (let loop ([i 0])
        (cond
          [(> (+ i mlen) len) str]
          [(string=? (substring str i (+ i mlen)) marker)
           (string-append (substring str 0 i) (substring str (+ i mlen) len))]
          [else (loop (+ i 1))]))))

  ;; Reduce a raw option token to the flag it names, or #f when it is not one.  A
  ;; trailing "=ARG" or "<ARG" placeholder is shed and an embedded negation is
  ;; removed; what remains is a flag when it is a long "--word" (two dashes then
  ;; at least one further character) or a short "-x" (one dash then exactly one
  ;; further character, which may be non-alphanumeric).
  (define (token->flag tok)
    (let* ([t (before-char tok #\=)]
           [t (before-char t #\<)]
           [t (drop-negation-marker t)]
           [n (string-length t)])
      (cond
        [(and (>= n 3)
              (char=? (string-ref t 0) #\-)
              (char=? (string-ref t 1) #\-))
         t]
        [(and (= n 2)
              (char=? (string-ref t 0) #\-)
              (not (char=? (string-ref t 1) #\-)))
         t]
        [else #f])))

  ;; The index just past the token beginning at I: a run of non-comma,
  ;; non-whitespace characters.
  (define (token-end line i len)
    (let loop ([j i])
      (cond
        [(>= j len) len]
        [(char=? (string-ref line j) #\,) j]
        [(char-whitespace? (string-ref line j)) j]
        [else (loop (+ j 1))])))

  ;; Advance past a single space at I, if there is one.
  (define (skip-one-space line i len)
    (if (and (< i len) (char=? (string-ref line i) #\space)) (+ i 1) i))

  ;; The index of the first non-space character at or after I.
  (define (spaces-end line i len)
    (let loop ([j i])
      (if (and (< j len) (char=? (string-ref line j) #\space)) (loop (+ j 1)) j)))

  ;; True when TOK looks like an argument placeholder rather than a flag: an
  ;; angle-bracketed name, or an upper-case word (no lower-case letters, at least
  ;; one upper-case).  Such a token following a flag by a single space is the
  ;; flag's argument, not part of the description.
  (define (placeholder? tok)
    (and (> (string-length tok) 0)
         (or (char=? (string-ref tok 0) #\<)
             (and (let no-lower ([i 0])
                    (cond
                      [(>= i (string-length tok)) #t]
                      [(let ([c (string-ref tok i)])
                         (and (char-alphabetic? c) (not (char-upper-case? c)))) #f]
                      [else (no-lower (+ i 1))]))
                  (let any-upper ([i 0])
                    (cond
                      [(>= i (string-length tok)) #f]
                      [(char-upper-case? (string-ref tok i)) #t]
                      [else (any-upper (+ i 1))]))))))

  ;; Close an option run: the flag names in written order paired with the
  ;; same-line description, or #f when the run named no flag at all.
  (define (finish-run flags desc)
    (if (null? flags) #f (cons (reverse flags) desc)))

  ;; Read the leading option run of an already-stripped LINE.  Returns a pair of
  ;; the flag names and the same-line description (or #f for "no same-line
  ;; description, consider the next line"), or #f when the line is not an option
  ;; line -- a section header, a description line, or a bracketed usage synopsis,
  ;; each of which fails the "first non-space character is a dash" test.  Flags
  ;; within the run are comma-separated; the run ends at the first token that is
  ;; not a flag, or at a two-space gap that opens the description.
  (define (parse-option-line line)
    (let ([len (string-length line)]
          [start (first-non-space line)])
      (and start
           (char=? (string-ref line start) #\-)
           (let loop ([i start] [flags '()])
             (if (or (>= i len) (not (char=? (string-ref line i) #\-)))
                 (finish-run flags #f)
                 (let* ([tend (token-end line i len)]
                        [flag (token->flag (substring line i tend))]
                        [flags2 (if flag (cons flag flags) flags)])
                   (cond
                     ;; The line ends at the option run: no same-line description.
                     [(>= tend len) (finish-run flags2 #f)]
                     ;; A comma joins the next flag of the run.
                     [(char=? (string-ref line tend) #\,)
                      (loop (skip-one-space line (+ tend 1) len) flags2)]
                     ;; Whitespace after the token.
                     [else
                      (let* ([desc-start (spaces-end line tend len)]
                             [gap (- desc-start tend)])
                        (cond
                          ;; Only trailing spaces: consider the next line instead.
                          [(>= desc-start len) (finish-run flags2 #f)]
                          ;; Two or more spaces open the same-line description.
                          [(>= gap 2)
                           (finish-run flags2
                                       (right-trim (substring line desc-start len)))]
                          ;; A single space may precede an argument placeholder,
                          ;; which is shed before re-checking for a description.
                          [else
                           (let ([pend (token-end line desc-start len)])
                             (if (placeholder? (substring line desc-start pend))
                                 (let ([d2 (spaces-end line pend len)])
                                   (if (>= d2 len)
                                       (finish-run flags2 #f)
                                       (finish-run
                                         flags2
                                         (right-trim (substring line d2 len)))))
                                 (finish-run flags2 #f)))]))])))))))

  ;; The description carried by the line after option line I: the next line's
  ;; text when it is indented deeper than the option and does not itself start a
  ;; flag; otherwise #f.
  (define (next-line-description vec n i opt-start)
    (and (< (+ i 1) n)
         (let* ([nl (vector-ref vec (+ i 1))]
                [s (first-non-space nl)])
           (and s
                opt-start
                (> s opt-start)
                (not (char=? (string-ref nl s) #\-))
                (right-trim (substring nl s (string-length nl)))))))

  ;; Parse a command's help output (a list of raw lines) into an association of
  ;; flag name to description.  Each raw line is escape-stripped first; option
  ;; lines yield one entry per flag, short and long sharing the line's
  ;; description (same-line where present, else the next indented line, else
  ;; empty); every other line contributes nothing.
  (define (parse-help-output raw-lines)
    (let* ([vec (list->vector (map strip-terminal-escapes raw-lines))]
           [n (vector-length vec)])
      ;; Accumulate each line's flag pairs onto the front of the result and
      ;; reverse once at the end, rather than appending the growing accumulator
      ;; per line -- the append made assembly quadratic in the accumulated flag
      ;; count, and a resolvable tool's --help is read whole and unbounded. The
      ;; fold-left over a line's flags prepends them in written order, so the
      ;; single final reverse restores document order exactly.
      (let loop ([i 0] [acc '()])
        (if (>= i n)
            (reverse acc)
            (let* ([line (vector-ref vec i)]
                   [parsed (parse-option-line line)])
              (if parsed
                  (let* ([flags (car parsed)]
                         [desc (or (cdr parsed)
                                   (next-line-description vec n i (first-non-space line))
                                   "")])
                    (loop (+ i 1)
                          (fold-left (lambda (a f) (cons (cons f desc) a)) acc flags)))
                  (loop (+ i 1) acc)))))))

  ;; ======================================================================
  ;; Option-flag completer: gate, per-command cache and producer
  ;; ======================================================================

  ;; True when CMD is safe to place into the --help/man spawn string: a non-empty
  ;; name built only from letters, digits and the punctuation that legitimately
  ;; appears in a command name or an explicit path -- full stop, underscore,
  ;; hyphen and slash.  Every shell metacharacter, and whitespace, is thereby
  ;; excluded, so a token such as a semicolon-joined pair or a command
  ;; substitution can never reach the shell.  This is the gate that must pass
  ;; before any child is spawned from a user-influenced token.
  (define (safe-command-name? cmd)
    (and (string? cmd)
         (> (string-length cmd) 0)
         (let loop ([i 0])
           (or (>= i (string-length cmd))
               (let ([c (string-ref cmd i)])
                 (and (or (char<=? #\a c #\z)
                          (char<=? #\A c #\Z)
                          (char<=? #\0 c #\9)
                          (char=? c #\.)
                          (char=? c #\_)
                          (char=? c #\-)
                          (char=? c #\/))
                      (loop (+ i 1))))))))

  ;; True when CMD carries a slash: a relative or absolute path (./build,
  ;; ../tool, bin/x, /usr/bin/ls) rather than a bare basename resolved on PATH.
  (define (has-slash? cmd)
    (let ([len (string-length cmd)])
      (let loop ([i 0])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref cmd i) #\/) #t]
          [else (loop (+ i 1))]))))

  ;; True when CMD names something this process could actually run: an explicit
  ;; path (one containing a slash) that exists on disc, or a bare basename
  ;; present in the shell's PATH cache.  A name that resolves nowhere is never
  ;; spawned, so an absent command produces no child and no candidates.
  (define (command-resolvable? cmd)
    (if (has-slash? cmd)
        (guard (e [#t #f]) (file-exists? cmd))
        (and (hashtable-ref (path-cache) cmd #f) #t)))

  ;; The producer that turns a command name into its (flag . desc) association by
  ;; consulting the command itself.  It is held in a parameter so a deterministic
  ;; stand-in can be substituted to prove the cache and the gate without spawning
  ;; a real tool.  The default resolves the command first (no spawn otherwise),
  ;; then reads its --help with stderr folded in, since some tools print their
  ;; help there; should that yield no flags, it falls back to the manual page
  ;; with the overstrike stripped.  Any spawn failure collapses, through the
  ;; reaping command-output, to no flags.
  (define command-flag-source
    (make-parameter
      (lambda (cmd)
        (if (not (command-resolvable? cmd))
            '()
            (let ([from-help (parse-help-output
                               (command-output cmd "--help" "2>&1"))])
              (if (null? from-help)
                  (parse-help-output
                    (command-output "man" cmd "2>/dev/null" "|" "col" "-b"))
                  from-help))))))

  ;; Per-command flag cache, keyed by a bare basename, living for the session.
  ;; A dedicated sentinel distinguishes a name never looked up from one whose
  ;; lookup genuinely produced no flags, so a command without --help is consulted
  ;; once and thereafter answered from the cache -- never re-spawned on each
  ;; subsequent completion.
  (define *help-cache* (make-hashtable string-hash string=?))
  (define %help-miss (list 'help-miss))

  ;; The option flags for CMD as a (flag . desc) association.  The safety gate
  ;; comes first: an unsafe name returns nothing without touching the cache or
  ;; the producer, so no child is ever spawned from a metacharacter-bearing
  ;; token.  A slash-bearing name (./build, ../tool, bin/x) is a path relative to
  ;; the current directory: the same token names a different executable in every
  ;; directory, so it must NOT be served from a session cache keyed on the typed
  ;; string -- projA's ./build would answer for projB's.  Such names bypass the
  ;; cache and are re-probed each time; only bare basenames, PATH-resolved and
  ;; stable within a session, are served from the cache or produced once and
  ;; cached -- the empty result included.
  (define (command-flags cmd)
    (cond
      [(not (safe-command-name? cmd)) '()]
      [(has-slash? cmd) ((command-flag-source) cmd)]
      [else
       (let ([cached (hashtable-ref *help-cache* cmd %help-miss)])
         (if (eq? cached %help-miss)
             (let ([flags ((command-flag-source) cmd)])
               (hashtable-set! *help-cache* cmd flags)
               flags)
             cached))]))

  ;; Complete a dash-prefixed token to CMD's option flags.  The flag names are
  ;; fuzzy-matched against the typed prefix and returned as (name positions
  ;; description) triples, each name and description cleaned of terminal escapes
  ;; before it reaches the menu.  Dispatched directly on the token shape rather
  ;; than through the completer registry.
  (define (command-flag-completer cmd prefix)
    (let* ([flags (command-flags cmd)]
           [names (map car flags)])
      (map (lambda (m)
             (let ([desc (cond [(assoc (car m) flags) => cdr] [else ""])])
               (list (strip-terminal-escapes (car m))
                     (cdr m)
                     (strip-terminal-escapes desc))))
           (fuzzy-filter/positions prefix names))))

  ;; ======================================================================
  ;; Git completer
  ;; ======================================================================

  (define (git-subcommands)
    '("add" "bisect" "branch" "checkout" "cherry-pick" "clone" "commit"
      "diff" "fetch" "grep" "init" "log" "merge" "mv" "pull" "push"
      "rebase" "remote" "reset" "restore" "revert" "rm" "show" "stash"
      "status" "switch" "tag" "worktree"))

  (define (git-subcommand-descriptions)
    '(("add" . "stage files")
      ("bisect" . "binary search for bugs")
      ("branch" . "list/create/delete branches")
      ("checkout" . "switch branches or restore files")
      ("cherry-pick" . "apply commit changes")
      ("clone" . "clone a repository")
      ("commit" . "record changes")
      ("diff" . "show changes")
      ("fetch" . "download remote refs")
      ("grep" . "search tracked files")
      ("init" . "create repository")
      ("log" . "show commit history")
      ("merge" . "join branches")
      ("mv" . "move/rename files")
      ("pull" . "fetch and merge")
      ("push" . "update remote refs")
      ("rebase" . "reapply commits")
      ("remote" . "manage remotes")
      ("reset" . "reset HEAD")
      ("restore" . "restore working tree files")
      ("revert" . "revert commits")
      ("rm" . "remove files")
      ("show" . "show objects")
      ("stash" . "stash changes")
      ("status" . "show working tree status")
      ("switch" . "switch branches")
      ("tag" . "manage tags")
      ("worktree" . "manage worktrees")))

  ;; Local branch names for git completion.  The names are collected as a verbatim
  ;; argument vector -- exec-path execs git directly and run/strings* captures its
  ;; stdout lines, reaping the child synchronously -- rather than through a joined
  ;; /bin/sh -c command string, because the %(refname:short) format carries
  ;; parentheses a shell reads as subshell metacharacters and aborts on, so a
  ;; shell-joined run yields no branches at all.  The whole body is guarded so a
  ;; spawn or pipe failure yields an empty list rather than throwing.  On the clean
  ;; --format output the leading whitespace/asterisk strip is a harmless no-op (git
  ;; prints bare short names with no marker), kept as a defensive guard.
  (define (git-branches)
    (guard (e [#t '()])
      (let ([lines (run/strings*
                     (lambda ()
                       (exec-path "git" "branch" "--list"
                                  "--format=%(refname:short)")))])
        (map (lambda (line)
               ;; strip leading whitespace/asterisk
               (let loop ([i 0])
                 (if (and (< i (string-length line))
                          (or (char-whitespace? (string-ref line i))
                              (char=? (string-ref line i) #\*)))
                     (loop (+ i 1))
                     (substring line i (string-length line)))))
             lines))))

  (define (git-modified-files)
    (let ([lines (command-output "git" "status" "--porcelain")])
      (filter (lambda (s) (> (string-length s) 0))
              (map (lambda (line)
                     (if (> (string-length line) 3)
                         ;; skip 2-char status + space
                         (let ([f (substring line 3 (string-length line))])
                           ;; strip trailing whitespace
                           (let loop ([i (- (string-length f) 1)])
                             (if (and (>= i 0) (char-whitespace? (string-ref f i)))
                                 (loop (- i 1))
                                 (substring f 0 (+ i 1)))))
                         ""))
                   lines))))

  ;; Configured remote names (`git remote`), one per line.  Empty outside a
  ;; repository or when no remote is set; blank lines are dropped as
  ;; git-modified-files does.  Routed through the reaped command-output like the
  ;; other git queries, so a burst of completions leaves no lingering child.
  (define (git-remotes)
    (filter (lambda (s) (> (string-length s) 0))
            (command-output "git" "remote")))

  ;; Tag names (`git tag --list`), one per line; empty when the repository holds
  ;; no tags.
  (define (git-tags)
    (filter (lambda (s) (> (string-length s) 0))
            (command-output "git" "tag" "--list")))

  ;; Stash entries as `stash@{N}` selectors (`git stash list --format=%gd`), one
  ;; per line; empty when the stash is empty.
  (define (git-stashes)
    (filter (lambda (s) (> (string-length s) 0))
            (command-output "git" "stash" "list" "--format=%gd")))

  (define (git-completer prefix context)
    (let ([args (cdr (assq 'args context))])
      (cond
        ;; No subcommand yet: complete subcommands
        [(null? args)
         (let* ([descs (git-subcommand-descriptions)]
                [matches (fuzzy-filter/positions prefix (git-subcommands))])
           (map (lambda (m)
                  (let ([name (car m)]
                        [positions (cdr m)])
                    (let ([desc (assoc name descs)])
                      (list name positions (if desc (cdr desc) #f)))))
                matches))]
        ;; After subcommand: context-specific
        [else
         (let ([sub (car args)])
           (cond
             ;; Branch-taking subcommands
             [(member sub '("checkout" "switch" "merge" "rebase" "branch" "diff" "log"))
              (let ([branches (git-branches)])
                (map (lambda (m) (list (car m) (cdr m) "branch"))
                     (fuzzy-filter/positions prefix branches)))]
             ;; File-taking subcommands
             [(member sub '("add" "restore" "rm" "mv" "diff" "reset"))
              (let ([files (git-modified-files)])
                (map (lambda (m) (list (car m) (cdr m) "modified"))
                     (fuzzy-filter/positions prefix files)))]
             ;; Remote-taking subcommands: complete configured remote names.  The
             ;; candidate is stripped of terminal escapes before it is returned,
             ;; so a remote whose name carries an escape sequence in a crafted
             ;; repository cannot repaint the menu (belt-and-braces alongside the
             ;; central strip on the render path).
             [(member sub '("push" "pull" "fetch" "remote"))
              (let ([remotes (git-remotes)])
                (map (lambda (m)
                       (list (strip-terminal-escapes (car m)) (cdr m) "remote"))
                     (fuzzy-filter/positions prefix remotes)))]
             ;; Tag-naming subcommands: `tag` names an existing tag to inspect or
             ;; delete and `show` takes a tag.  checkout/switch stay on branches
             ;; above by design, so tags are offered only for these two.
             [(member sub '("tag" "show"))
              (let ([tags (git-tags)])
                (map (lambda (m)
                       (list (strip-terminal-escapes (car m)) (cdr m) "tag"))
                     (fuzzy-filter/positions prefix tags)))]
             ;; Stash subcommand: complete the `stash@{N}` selectors.
             [(member sub '("stash"))
              (let ([stashes (git-stashes)])
                (map (lambda (m)
                       (list (strip-terminal-escapes (car m)) (cdr m) "stash"))
                     (fuzzy-filter/positions prefix stashes)))]
             [else '()]))])))

  ;; ======================================================================
  ;; SSH completer — hostnames from known_hosts and config
  ;; ======================================================================

  (define (ssh-hosts)
    (let ([hosts '()]
          [home (or (getenv "HOME") "")])
      ;; Parse known_hosts
      (guard (e [#t #f])
        (let ([p (open-input-file (string-append home "/.ssh/known_hosts"))])
          (let loop ()
            (let ([line (get-line p)])
              (unless (eof-object? line)
                (when (and (> (string-length line) 0)
                           (not (char=? (string-ref line 0) #\#)))
                  ;; First field before space is hostname (possibly hashed)
                  (let ([end (let scan ([i 0])
                               (cond
                                 [(>= i (string-length line)) i]
                                 [(or (char=? (string-ref line i) #\space)
                                      (char=? (string-ref line i) #\,)) i]
                                 [else (scan (+ i 1))]))])
                    (let ([host (substring line 0 end)])
                      ;; Skip hashed entries (start with |)
                      (unless (and (> (string-length host) 0)
                                   (char=? (string-ref host 0) #\|))
                        (set! hosts (cons host hosts))))))
                (loop))))
          (close-port p)))
      ;; Parse ssh config Host entries
      (guard (e [#t #f])
        (let ([p (open-input-file (string-append home "/.ssh/config"))])
          (let loop ()
            (let ([line (get-line p)])
              (unless (eof-object? line)
                ;; Match lines starting with "Host " (case-insensitive)
                (let ([trimmed (let skip ([i 0])
                                 (if (and (< i (string-length line))
                                          (char-whitespace? (string-ref line i)))
                                     (skip (+ i 1))
                                     (substring line i (string-length line))))])
                  (when (and (>= (string-length trimmed) 5)
                             (let ([pfx (substring trimmed 0 5)])
                               (or (string=? pfx "Host ")
                                   (string=? pfx "host "))))
                    ;; Extract hostname (skip "Host ")
                    (let ([host (let skip ([i 5])
                                  (if (and (< i (string-length trimmed))
                                           (char-whitespace? (string-ref trimmed i)))
                                      (skip (+ i 1))
                                      (let end ([j i])
                                        (if (or (>= j (string-length trimmed))
                                                (char-whitespace? (string-ref trimmed j)))
                                            (substring trimmed i j)
                                            (end (+ j 1))))))])
                      ;; Skip wildcards
                      (unless (let loop ([i 0])
                                (cond
                                  [(>= i (string-length host)) #f]
                                  [(char=? (string-ref host i) #\*) #t]
                                  [(char=? (string-ref host i) #\?) #t]
                                  [else (loop (+ i 1))]))
                        (set! hosts (cons host hosts))))))
                (loop))))
          (close-port p)))
      ;; Deduplicate
      (let ([ht (make-hashtable string-hash string=?)])
        (filter (lambda (h)
                  (if (hashtable-ref ht h #f)
                      #f
                      (begin (hashtable-set! ht h #t) #t)))
                (reverse hosts)))))

  (define (ssh-completer prefix context)
    (let ([hosts (ssh-hosts)])
      (map (lambda (m) (list (car m) (cdr m) "host"))
           (fuzzy-filter/positions prefix hosts))))

  ;; ======================================================================
  ;; Kill completer — PIDs from process table
  ;; ======================================================================

  (define (kill-completer prefix context)
    (let* ([lines (command-output "ps" "-eo" "pid=" "-eo" "comm=")]
           [entries
            (filter (lambda (e) (> (string-length (car e)) 0))
              (map (lambda (line)
                     (let* ([trimmed (let skip ([i 0])
                                       (if (and (< i (string-length line))
                                                (char-whitespace? (string-ref line i)))
                                           (skip (+ i 1))
                                           (substring line i (string-length line))))]
                            [space (let scan ([i 0])
                                     (cond
                                       [(>= i (string-length trimmed)) #f]
                                       [(char-whitespace? (string-ref trimmed i)) i]
                                       [else (scan (+ i 1))]))])
                       (if space
                           (cons (substring trimmed 0 space)
                                 (let skip ([i (+ space 1)])
                                   (if (and (< i (string-length trimmed))
                                            (char-whitespace? (string-ref trimmed i)))
                                       (skip (+ i 1))
                                       (substring trimmed i (string-length trimmed)))))
                           (cons trimmed ""))))
                   lines))]
           [pids (map car entries)]
           [desc-ht (let ([ht (make-hashtable string-hash string=?)])
                      (for-each (lambda (e) (hashtable-set! ht (car e) (cdr e))) entries)
                      ht)])
      (map (lambda (m)
             (list (car m) (cdr m)
                   (hashtable-ref desc-ht (car m) #f)))
           (fuzzy-filter/positions prefix pids))))

  ;; ======================================================================
  ;; Make completer — targets from Makefile
  ;; ======================================================================

  (define (make-targets)
    (guard (e [#t '()])
      (let ([p (open-input-file "Makefile")])
        (let loop ([acc '()])
          (let ([line (get-line p)])
            (if (eof-object? line)
                (begin (close-port p) (reverse acc))
                (if (and (> (string-length line) 0)
                         (not (char=? (string-ref line 0) #\tab))
                         (not (char=? (string-ref line 0) #\#))
                         (not (char=? (string-ref line 0) #\.)))
                    ;; Find colon
                    (let ([colon (let scan ([i 0])
                                   (cond
                                     [(>= i (string-length line)) #f]
                                     [(char=? (string-ref line i) #\:) i]
                                     [else (scan (+ i 1))]))])
                      (if (and colon (> colon 0)
                               ;; Not a variable assignment (no = before :)
                               (not (let scan ([i 0])
                                      (cond
                                        [(>= i colon) #f]
                                        [(char=? (string-ref line i) #\=) #t]
                                        [else (scan (+ i 1))]))))
                          (let ([target (substring line 0 colon)])
                            ;; Skip pattern rules (contain %)
                            (if (let scan ([i 0])
                                  (cond
                                    [(>= i (string-length target)) #f]
                                    [(char=? (string-ref target i) #\%) #t]
                                    [else (scan (+ i 1))]))
                                (loop acc)
                                (loop (cons target acc))))
                          (loop acc)))
                    (loop acc))))))))

  (define (make-completer prefix context)
    (let ([targets (make-targets)])
      (map (lambda (m) (list (car m) (cdr m) "target"))
           (fuzzy-filter/positions prefix targets))))

  ;; ======================================================================
  ;; Tilde-user completer — login names to home directories
  ;; ======================================================================

  ;; Enumerate login names paired with their home directories, each keyed by the
  ;; login with a leading `~` so the inserted candidate `~alice` is later resolved
  ;; by the tilde expander.  The passwd-database walk is tried first: it is
  ;; NSS-correct, so it sees LDAP, SSSD and NIS logins a bare file read would
  ;; miss.  Should that walk come back empty — a stripped container, or a database
  ;; fault swallowed below the enumeration — /etc/passwd is read directly (field 0
  ;; the login, field 5 the home) so completion still offers the local logins; the
  ;; open is guarded, so an unreadable or absent file yields an empty list rather
  ;; than an error.  The result is de-duplicated on the `~login` key, keeping the
  ;; first home seen, exactly as ssh-hosts de-duplicates its host names.
  (define (user-entries)
    (define (from-database)
      (map (lambda (pw)
             (cons (string-append "~" (passwd-info-name pw))
                   (passwd-info-dir pw)))
           (posix-getpwent-all)))
    (define (from-etc-passwd)
      (guard (e [#t '()])
        (let ([p (open-input-file "/etc/passwd")])
          (dynamic-wind
            (lambda () (void))
            (lambda ()
              (let loop ([acc '()])
                (let ([line (get-line p)])
                  (if (eof-object? line)
                      (reverse acc)
                      (if (or (= (string-length line) 0)
                              (char=? (string-ref line 0) #\#))
                          (loop acc)
                          (let ([fields (split-on-colon line)])
                            (if (>= (length fields) 6)
                                (loop (cons (cons (string-append "~" (list-ref fields 0))
                                                  (list-ref fields 5))
                                            acc))
                                (loop acc))))))))
            (lambda () (close-port p))))))
    (let ([entries (let ([db (from-database)])
                     (if (null? db) (from-etc-passwd) db))]
          [ht (make-hashtable string-hash string=?)])
      (filter (lambda (e)
                (if (hashtable-ref ht (car e) #f)
                    #f
                    (begin (hashtable-set! ht (car e) #t) #t)))
              entries)))

  ;; The `~<tab>` candidate set: every registered named directory (keyed `~name`
  ;; with its directory as the description) placed AHEAD of the passwd logins, so a
  ;; named `~proj` shadows a login `proj`; de-duplicated on the `~name` key keeping
  ;; the first (named) entry, exactly as user-entries de-duplicates the logins.
  (define (tilde-entries)
    (let ([entries (append
                     (map (lambda (p)
                            (cons (string-append "~" (car p)) (cdr p)))
                          (named-directories-list))
                     (user-entries))]
          [ht (make-hashtable string-hash string=?)])
      (filter (lambda (e)
                (if (hashtable-ref ht (car e) #f)
                    #f
                    (begin (hashtable-set! ht (car e) #t) #t)))
              entries)))

  ;; Complete a `~`-prefixed token to a named directory or a login's home.  The
  ;; prefix carries the leading `~` (so `~al` ranks `~alice`), and the returned
  ;; candidate keeps that `~` so the editor inserts `~alice`, later expanded by the
  ;; tilde expander; the directory rides along as the menu description.  A
  ;; registered named directory is offered ahead of a same-named login (the
  ;; tilde-entries merge).  Both the candidate and the description pass through
  ;; strip-terminal-escapes: a login, a named directory or a home path must not
  ;; carry an escape sequence into the rendered menu.  Dispatched by the editor on
  ;; the token's `~` shape rather than by a command name, so it is deliberately not
  ;; in the completer registry.
  (define (user-completer prefix context)
    (let* ([entries (tilde-entries)]
           [names (map car entries)])
      (map (lambda (m)
             ;; A named directory shadows a same-named login: resolve its home
             ;; from the named-directory table FIRST -- exactly as the tilde
             ;; expander does -- so the named entry wins the description even
             ;; where the merged passwd enumeration and the named entry disagree
             ;; (the merge order is not authoritative across every libc).
             (let* ([name (car m)]
                    [named (and (fx> (string-length name) 1)
                                (char=? (string-ref name 0) #\~)
                                (named-directory-ref
                                  (substring name 1 (string-length name))))]
                    [home (or named
                              (cond [(assoc name entries) => cdr]
                                    [else ""]))])
               (list (strip-terminal-escapes name)
                     (cdr m)
                     (strip-terminal-escapes home))))
           (fuzzy-filter/positions prefix names))))

  ;; ======================================================================
  ;; Directory-only completion -- cd / pushd / rmdir
  ;; ======================================================================

  ;; The commands whose operand is always a directory.  Their completion offers
  ;; directories only, and the editor consults this predicate to suppress its
  ;; filename fallback for them: when the directory completer yields nothing a
  ;; no-match cd shows an empty menu rather than leaking plain files.
  (define (dir-only-command? cmd)
    (and (member cmd '("cd" "pushd" "rmdir")) #t))

  ;; Resolve a "~name" (NAME without the leading "~") to a directory: a registered
  ;; named directory wins -- matching the tilde expander's hash -d precedence, so
  ;; completing a name agrees with where cd navigates -- otherwise the login's
  ;; passwd home, and #f when the name is neither, so an unknown "~name/" completes
  ;; to nothing.
  (define (named-or-user-home name)
    (or (named-directory-ref name)
        (let ([key (string-append "~" name)])
          (cond
            [(assoc key (user-entries)) => cdr]
            [else #f]))))

  ;; Expand a leading tilde form to a concrete prefix before the path is split on
  ;; its last "/", so the directory listing sees a real path:
  ;;   "~/pr"        -> "$HOME/pr"                (home, read through hafod's getenv)
  ;;   "~name/rest"  -> "<named-or-home>/rest"    (a registered named directory,
  ;;                                               else the login's passwd home)
  ;; A bare "~" or a "~name" WITHOUT a slash is intercepted by the editor's tilde
  ;; arm (served by user-completer) and never reaches here, so only the with-slash
  ;; forms expand; an unknown "~name/" (neither a named directory nor a login) is
  ;; left as typed and simply lists nothing.
  (define (expand-leading-tilde prefix)
    (if (and (> (string-length prefix) 0)
             (char=? (string-ref prefix 0) #\~))
        (let* ([len (string-length prefix)]
               [slash (let loop ([i 1])
                        (cond
                          [(>= i len) #f]
                          [(char=? (string-ref prefix i) #\/) i]
                          [else (loop (+ i 1))]))])
          (if (not slash)
              prefix
              (let ([name (substring prefix 1 slash)]
                    [rest (substring prefix slash len)])
                (if (string=? name "")
                    (string-append (or (getenv "HOME") "") rest)
                    (let ([home (named-or-user-home name)])
                      (if home (string-append home rest) prefix))))))
        prefix))

  ;; The directory-stack completion for a leading "-": a bare "-" whose
  ;; description is $OLDPWD (the cd - target, useful after a plain cd with no
  ;; pushd), then "-1", "-2", ... one per directory-stack entry with that entry's
  ;; directory as the description, so the label "-i" names exactly the directory
  ;; cd -i enters (the stack is 1-based, top first).  The labels are fuzzy-ranked
  ;; against the typed prefix so "-2" narrows to the one label, and every label
  ;; and description is escape-stripped before it reaches the menu.
  (define (dir-stack-labels prefix)
    (let* ([old (getenv "OLDPWD")]
           [entries
            (append
              (if old (list (cons "-" old)) '())
              (let loop ([ds (dir-stack)] [i 1] [acc '()])
                (if (null? ds)
                    (reverse acc)
                    (loop (cdr ds) (+ i 1)
                          (cons (cons (string-append "-" (number->string i)) (car ds))
                                acc)))))]
           [names (map car entries)])
      (map (lambda (m)
             (let ([desc (cond [(assoc (car m) entries) => cdr] [else ""])])
               (list (strip-terminal-escapes (car m))
                     (cdr m)
                     (strip-terminal-escapes desc))))
           (fuzzy-filter/positions prefix names))))

  ;; List the directories directly under DIR whose name fuzzy-matches BASE, each a
  ;; (name positions description) triple.  Only entries that are directories --
  ;; followed through a symlink by the one-arg file-directory?, never the nofollow
  ;; form that would drop a symlink-to-a-directory -- are kept, each given a
  ;; trailing "/" (and no "@"); smart-hidden offers a dot-entry only when BASE
  ;; itself begins with a dot (the zsh default).  When DIR-PREFIX is a string it is
  ;; prepended to each candidate name (and the match positions shifted), so a typed
  ;; "a/b" completes to "a/bcd/"; when it is #f the bare basename is inserted (a
  ;; cwd or a $CDPATH candidate).  DESC is the menu description every candidate
  ;; carries -- the empty string for a cwd listing, the $CDPATH root for a $CDPATH
  ;; entry.  Fuzzy-filtering first and statting only the survivors bounds the
  ;; directory probes to the matches, an unreadable DIR yields no candidates rather
  ;; than an error, and every name and description is escape-stripped.
  (define (list-dirs-in dir base dir-prefix desc)
    (let* ([dir-prefix-len (if dir-prefix (string-length dir-prefix) 0)]
           [want-hidden? (and (> (string-length base) 0)
                              (char=? (string-ref base 0) #\.))]
           [entries (guard (e [#t '()]) (directory-list dir))]
           [visible (filter
                      (lambda (entry)
                        (or want-hidden?
                            (= (string-length entry) 0)
                            (not (char=? (string-ref entry 0) #\.))))
                      entries)]
           [matched (fuzzy-filter/positions base visible)])
      (fold-right
        (lambda (pair acc)
          (let* ([entry (car pair)]
                 [positions (cdr pair)]
                 [full (string-append dir "/" entry)]
                 [is-dir? (guard (e [#t #f]) (file-directory? full))])
            (if is-dir?
                (let* ([shown (string-append entry "/")]
                       [name (if dir-prefix (string-append dir-prefix shown) shown)]
                       [shifted (if dir-prefix
                                    (map (lambda (p) (+ p dir-prefix-len)) positions)
                                    positions)])
                  (cons (list (strip-terminal-escapes name)
                              shifted
                              (strip-terminal-escapes desc))
                        acc))
                acc)))
        '()
        matched)))

  ;; The non-empty $CDPATH entries, or '() when $CDPATH is unset or empty.  The
  ;; raw entries (and the "empty element = cwd" rule) come from the cd builtin's
  ;; shared cdpath-entries; the current directory is already listed directly, so
  ;; the empty elements that denote it are dropped here.
  (define (cdpath-roots)
    (filter (lambda (e) (> (string-length e) 0)) (cdpath-entries)))

  ;; For each non-empty $CDPATH entry, the subdirectories matching BASE offered as
  ;; BARE basenames (dir-prefix #f) with the entry as the description, so the
  ;; inserted basename is exactly what builtin-cd resolves back through $CDPATH
  ;; rather than an absolute path; the entry rides only as the description.
  (define (cdpath-dirs base)
    (fold-right
      (lambda (root acc)
        (append (list-dirs-in root base #f root) acc))
      '()
      (cdpath-roots)))

  ;; Merge the cwd candidates ahead of the $CDPATH candidates, dropping any
  ;; basename already offered so a directory reachable both locally and via
  ;; $CDPATH appears once, as the local candidate (cwd first).
  (define (dedup-candidates cwd cdpath)
    (let ([seen (make-hashtable string-hash string=?)])
      (filter (lambda (tr)
                (if (hashtable-ref seen (car tr) #f)
                    #f
                    (begin (hashtable-set! seen (car tr) #t) #t)))
              (append cwd cdpath))))

  ;; Complete a cd / pushd / rmdir operand.  The completer is COMMAND-AWARE: the
  ;; $CDPATH union and the cd -N / bare-"-" dash-stack labels are cd-only, because
  ;; only builtin-cd consults $CDPATH and only cd -N walks the directory stack --
  ;; pushd and rmdir honour neither, so offering them such a target would propose a
  ;; candidate the command cannot reach.  The invoking command rides in the context
  ;; as 'command (threaded by the editor); a caller that omits it gets the
  ;; conservative dirs-only behaviour.
  ;;
  ;; A leading "-" (cd only) lists the directory stack as -N labels plus a bare "-"
  ;; for $OLDPWD.  Otherwise the typed PREFIX is a path fragment: a leading "~/" or
  ;; "~name/" is expanded first, then the fragment is split on its last "/" into a
  ;; directory to list and a base to match, and the directories under it are offered
  ;; (trailing "/", dirs-only, symlink-followed, smart-hidden, escapes stripped).
  ;; For cd, when the base is a bare relative name (no "/", not "."/"..", not "~")
  ;; the cwd listing is unioned with the directories reachable via $CDPATH -- as
  ;; bare basenames, cwd candidates first, deduped by basename -- so every
  ;; completable cd target is one builtin-cd can also navigate to.  Filesystem
  ;; access is guarded, so an unreadable or absent directory yields no candidates.
  (define (cd-completer prefix context)
    (guard (e [#t '()])
      (let ([cd? (equal? (cond [(assq 'command context) => cdr] [else #f]) "cd")])
        (if (and cd?
                 (> (string-length prefix) 0)
                 (char=? (string-ref prefix 0) #\-))
            (dir-stack-labels prefix)
            (let* ([expanded (expand-leading-tilde prefix)]
                   [len (string-length expanded)]
                   [last-slash (let loop ([i (- len 1)])
                                 (cond
                                   [(< i 0) #f]
                                   [(char=? (string-ref expanded i) #\/) i]
                                   [else (loop (- i 1))]))]
                   [dir (if last-slash
                            (if (= last-slash 0) "/" (substring expanded 0 (+ last-slash 1)))
                            ".")]
                   [base (if last-slash
                             (substring expanded (+ last-slash 1) len)
                             expanded)]
                   [dir-prefix (if last-slash
                                   (if (= last-slash 0) "/" (substring expanded 0 (+ last-slash 1)))
                                   #f)]
                   [cwd-candidates (list-dirs-in dir base dir-prefix "")]
                   [use-cdpath? (and cd?
                                     (not last-slash)
                                     (> (string-length base) 0)
                                     (not (char=? (string-ref base 0) #\~))
                                     (not (string=? base "."))
                                     (not (string=? base "..")))])
              (if use-cdpath?
                  (dedup-candidates cwd-candidates (cdpath-dirs base))
                  cwd-candidates))))))

  ;; ======================================================================
  ;; Register built-in completers (on first lookup, not at instantiation)
  ;; ======================================================================

  ;; Set the flag first so a lookup that re-enters during registration sees the
  ;; built-ins as already in progress and the second call is a no-op.
  (define (ensure-builtin-completers!)
    (unless %completers-registered?
      (set! %completers-registered? #t)
      (register-completer! "git" git-completer)
      (register-completer! "ssh" ssh-completer)
      (register-completer! "scp" ssh-completer)
      (register-completer! "kill" kill-completer)
      (register-completer! "killall" kill-completer)
      (register-completer! "make" make-completer)
      (register-completer! "gmake" make-completer)
      (register-completer! "cd" cd-completer)
      (register-completer! "pushd" cd-completer)
      (register-completer! "rmdir" cd-completer)))

) ; end library
