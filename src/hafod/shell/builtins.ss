;;; (hafod shell builtins) -- Shell builtin commands
;;; cd, pushd, popd, export -- must execute in-process.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod shell builtins)
  (export builtin? run-builtin! builtin-names dir-stack builtin-cd-path
          nav-finder-proc split-on-colon cdpath-entries)
  (import (except (chezscheme) getenv)
          (only (hafod process-state) chdir cwd process-cwd)
          (only (hafod environment) getenv setenv)
          (only (hafod fname)
                split-file-name file-name-absolute? file-name-as-directory)
          (only (hafod user-group) home-directory)
          (only (hafod process) init-exec-path-list)
          (only (hafod shell parser) parse-command-words)
          (only (hafod shell classifier)
                rebuild-path-cache! alias-set! alias-remove! alias-ref alias-names)
          (only (hafod shell jobs) list-jobs job-fg! job-bg-resume!)
          (only (hafod shell visit-db)
                ensure-visit-db! visit-record! visit-query-best
                visit-candidates visit-now))

  ;; Directory stack for pushd/popd
  (define dir-stack-list '())

  (define (dir-stack) dir-stack-list)

  ;; alias/unalias and the z/zi jump commands join the builtin set here; the
  ;; classifier keeps a deliberate duplicate of these names in its own
  ;; builtin-names-set, the two held in step to avoid a circular import between
  ;; the classifier and this module.
  (define builtin-name-list
    '("cd" "pushd" "popd" "export" "jobs" "fg" "bg" "alias" "unalias" "z" "zi"))

  (define (builtin-names) builtin-name-list)

  (define (builtin? name)
    (and (member name builtin-name-list) #t))

  ;; --- logical path normalisation ---
  ;; Collapse an ABSOLUTE path lexically: drop "." and empty ("//") segments and
  ;; pop each ".." against the segment to its left, never past the root.  This is
  ;; a purely LEXICAL rewrite -- a "symlink/.." resolves to the symlink's lexical
  ;; parent, matching zsh/bash without -P -- so it never calls realpath and never
  ;; touches the filesystem.  simplify-file-name (in (hafod fname)) deliberately
  ;; keeps "..", so the pop is done here.  The result is rebuilt from OUT, which
  ;; holds the kept segments deepest-first, so each fold step wraps the growing
  ;; tail with its parent and the original order is restored.  A no-op on an
  ;; already-clean path, which keeps every existing cd/pushd/auto-cd/z result
  ;; byte-for-byte unchanged.
  (define (normalise-logical abs)
    (let loop ([segs (split-file-name abs)] [out '()])
      (cond
        [(null? segs)
         (if (null? out)
             "/"
             (fold-left (lambda (acc s) (string-append "/" s acc)) "" out))]
        [(or (string=? (car segs) "") (string=? (car segs) "."))
         (loop (cdr segs) out)]
        [(string=? (car segs) "..")
         (loop (cdr segs) (if (null? out) out (cdr out)))]
        [else (loop (cdr segs) (cons (car segs) out))])))

  ;; --- cd ---
  ;; The single directory-change seam.  Every route into a new directory -- cd,
  ;; pushd, popd, auto-cd and the z jump -- passes through here, so the visit is
  ;; recorded in exactly one place and a change is never counted twice.  TARGET is
  ;; entered verbatim: the caller has already resolved any shortcut (empty ->
  ;; home, "-" -> $OLDPWD, a $CDPATH or ~name target), so this seam interprets
  ;; nothing further, which is what lets builtin-cd-path reach chdir literally.
  ;;
  ;; The target is first resolved to an ABSOLUTE path and lexically collapsed
  ;; (normalise-logical), and it is that clean absolute path that reaches chdir --
  ;; process-state's chdir stores an absolute argument verbatim, so %cwd (and
  ;; hence (cwd), $PWD and the prompt) carries no literal "..".  Doing the collapse
  ;; here means a "cd ..", a pushd or an auto-cd all land a clean logical path.
  ;;
  ;; Returns #t when the directory actually changed and #f when the chdir failed,
  ;; so a caller (pushd/popd) can gate its own bookkeeping on the move having
  ;; happened.  A failed chdir keeps the cd: diagnostic and yields #f.
  ;;
  ;; On success the RESOLVED absolute working directory -- (cwd), not the raw
  ;; TARGET, so a "..", a "-" or a relative name is recorded as where we actually
  ;; landed -- is handed to the visit database exactly once, best-effort: the
  ;; record sits in its own guard so a database fault can never disturb the chdir,
  ;; and the handle opens only while visit recording is switched on (it is off for
  ;; a non-interactive run, which then never touches the database at all).
  (define (cd-to! target)
    (let* ([old (cwd)]
           [abs (if (file-name-absolute? target)
                    target
                    (string-append (file-name-as-directory old) target))]
           ;; An empty operand is NOT the filesystem root.  file-name-absolute?
           ;; reports "" as absolute and normalise-logical would collapse it to
           ;; "/", so an un-guarded `cd ""` -- and `cd "$UNSET"`, whose empty
           ;; expansion reaches here the same way -- would silently jump to /.
           ;; Keep an empty target verbatim so chdir raises ENOENT and the cd:
           ;; diagnostic fires without moving, as it did before the logical
           ;; collapse was introduced.
           [resolved (if (string=? target "")
                         target
                         (normalise-logical abs))])
      (guard (e [#t (display
                      (format "cd: ~a: ~a\n"
                              target
                              (if (condition? e)
                                  (condition-message e)
                                  e))
                      (console-error-port))
                    #f])
        ;; Enter the collapsed absolute path so %cwd carries no literal "..";
        ;; should that path prove unenterable -- a broken link somewhere in the
        ;; lexical chain -- fall back to the raw target before surfacing the error.
        ;; The fallback enters a relative target, so %cwd would otherwise carry
        ;; whatever literal ".." it held; re-anchor to the physical directory
        ;; actually reached so (cwd)/$PWD stay clean.  Only the fallback re-anchors
        ;; -- the primary path keeps its logical, symlink-preserving result.
        (guard (inner [#t (chdir target) (chdir (process-cwd))])
          (chdir resolved))
        (setenv "OLDPWD" old)
        (setenv "PWD" (cwd))
        (guard (e [#t #f])
          (let ([vdb (ensure-visit-db!)])
            (when vdb (visit-record! vdb (cwd) ((visit-now))))))
        #t)))

  ;; --- cd - / cd -N / $CDPATH resolution ---

  ;; Echo the directory just entered, as POSIX cd does after resolving a
  ;; non-literal operand (cd -, cd -N, or a jump taken through a non-cwd $CDPATH
  ;; entry) -- so the user sees where an operand that was not typed verbatim
  ;; landed.  Ordinary output, so it goes to the current output port, not stderr.
  (define (cd-echo dir)
    (display dir)
    (newline))

  ;; A `cd -N` operand: a leading '-' followed by one or more digits and nothing
  ;; else, yielding N.  Bare "-" (no digits) is NOT a -N form -- it is the $OLDPWD
  ;; shortcut, handled separately -- so it returns #f here.
  (define (cd-stack-index arg)
    (and (> (string-length arg) 1)
         (char=? (string-ref arg 0) #\-)
         (let loop ([i 1])
           (cond
             [(>= i (string-length arg))
              (string->number (substring arg 1 (string-length arg)))]
             [(char<=? #\0 (string-ref arg i) #\9) (loop (+ i 1))]
             [else #f]))))

  ;; Split S on ':' into its elements, keeping empty elements.  Shared with the
  ;; completer (which imports it) for both the $CDPATH split -- where an empty
  ;; element denotes the current directory, per POSIX -- and its colon-separated
  ;; /etc/passwd parse, so the two never drift.
  (define (split-on-colon s)
    (let ([len (string-length s)])
      (let loop ([start 0] [acc '()])
        (if (> start len)
            (reverse acc)
            (let ([colon (let scan ([i start])
                           (cond
                             [(>= i len) #f]
                             [(char=? (string-ref s i) #\:) i]
                             [else (scan (+ i 1))]))])
              (if colon
                  (loop (+ colon 1) (cons (substring s start colon) acc))
                  (loop (+ len 1) (cons (substring s start len) acc))))))))

  ;; The $CDPATH entries, or '() when it is unset or empty -- so an absent-CDPATH
  ;; run takes no CDPATH branch at all and stays byte-for-byte as before.
  (define (cdpath-entries)
    (let ([cp (getenv "CDPATH")])
      (if (or (not cp) (string=? cp ""))
          '()
          (split-on-colon cp))))

  ;; $CDPATH is consulted only for a RELATIVE operand whose first path component
  ;; is neither "." nor ".." (POSIX): an absolute path, or one beginning "./" or
  ;; "../" (or bare "."/".."), bypasses the search and is entered as given.  A
  ;; ~name operand has already expanded to an absolute path upstream, so it too
  ;; bypasses this naturally.
  (define (cdpath-eligible? operand)
    (and (> (string-length operand) 0)
         (not (char=? (string-ref operand 0) #\/))
         (let ([fc (let scan ([i 0])
                     (cond
                       [(>= i (string-length operand)) operand]
                       [(char=? (string-ref operand i) #\/)
                        (substring operand 0 i)]
                       [else (scan (+ i 1))]))])
           (not (or (string=? fc ".") (string=? fc ".."))))))

  ;; Resolve a CDPATH-eligible OPERAND: try the current directory first, then each
  ;; non-empty $CDPATH entry in turn, taking the first whose join is an existing
  ;; directory.  Returns two values -- the concrete path to hand cd-to! and whether
  ;; the hit came from a NON-cwd entry (so builtin-cd knows to echo the resolved
  ;; directory, as POSIX requires for a CDPATH-sourced jump).  Falls back to the
  ;; bare operand (today's behaviour) when CDPATH is unset or nothing matches.
  (define (resolve-via-cdpath operand)
    (let ([entries (cdpath-entries)])
      (cond
        [(null? entries) (values operand #f)]            ; no CDPATH -> unchanged
        [(file-directory? operand) (values operand #f)]  ; cwd first, no echo
        [else
         (let loop ([es entries])
           (cond
             [(null? es) (values operand #f)]            ; nothing matched
             [(string=? (car es) "") (loop (cdr es))]    ; empty = cwd, already tried
             [else
              (let ([candidate (string-append
                                 (file-name-as-directory (car es)) operand)])
                (if (file-directory? candidate)
                    (values candidate #t)                ; non-cwd hit -> echo
                    (loop (cdr es))))]))])))

  ;; The cd builtin.  Beyond a plain path it handles: no operand -> home; "-" ->
  ;; $OLDPWD (echoed); "-N" -> the Nth directory-stack entry (echoed); and a bare
  ;; relative operand searched along $CDPATH (echoed on a non-cwd hit).  Every move
  ;; still funnels through cd-to!, so the resolved path reaches chdir verbatim
  ;; (injection-safe) and picks up the logical .. collapse uniformly.
  (define (builtin-cd args)
    (cond
      [(null? args) (cd-to! (home-directory))]
      [(string=? (car args) "-")
       (let ([dst (getenv "OLDPWD")])
         (if dst
             (when (cd-to! dst) (cd-echo (cwd)))
             (display "cd: OLDPWD not set\n" (console-error-port))))]
      [(cd-stack-index (car args)) =>
       (lambda (n)
         (let ([stack (dir-stack)])
           (if (and (>= n 1) (<= n (length stack)))
               (when (cd-to! (list-ref stack (- n 1))) (cd-echo (cwd)))
               (display
                 (format "cd: -~a: no such entry in the directory stack\n" n)
                 (console-error-port)))))]
      [(cdpath-eligible? (car args))
       (let-values ([(target echo?) (resolve-via-cdpath (car args))])
         (when (and (cd-to! target) echo?)
           (cd-echo (cwd))))]
      [else (cd-to! (car args))]))

  ;; --- cd to a single literal path ---
  ;; Change directory to exactly PATH, with NO argument interpretation.  Unlike
  ;; builtin-cd, the empty -> home and "-" -> $OLDPWD shortcuts are deliberately
  ;; absent: a directory literally named "-" is entered rather than jumping to
  ;; $OLDPWD.  PATH reaches chdir verbatim -- no command string is built around
  ;; it and there is no re-tokenising -- so a directory whose name holds shell
  ;; metacharacters (or is a bare "-") is never re-parsed.  This is the
  ;; injection-safe seam auto-cd enters a bare directory token through.
  (define (builtin-cd-path path)
    (cd-to! path))

  ;; --- z / zi : frecency directory jump ---
  ;; The interactive picker the bare `z`/`zi` open over the frecency-ordered
  ;; candidates, injected exactly like the editor's finder seam so this module
  ;; needs no dependency on the terminal/finder stack and a test can drive the
  ;; picker with a stub.  The launcher fills it with the real finder; until then,
  ;; and throughout a non-interactive run, it is #f and every picker path no-ops.
  ;; Its contract mirrors run-finder: (proc items prompt) -> the chosen original
  ;; string, or #f when the user cancels.
  (define nav-finder-proc (make-parameter #f))

  ;; Open the picker over the visited directories, frecency-descending, and jump
  ;; to the choice.  Every jump -- picker or substring -- lands through
  ;; builtin-cd-path, the same literal chdir cd uses, so a chosen directory whose
  ;; name carries shell metacharacters is entered verbatim and never rebuilt into
  ;; a command line.  A quiet no-op when there is no finder, no live database or
  ;; no candidate, and when the user cancels.
  (define (run-z-picker!)
    (let ([f (nav-finder-proc)]
          [vdb (ensure-visit-db!)])
      (when (and f vdb)
        (let ([cands (visit-candidates vdb ((visit-now)))])
          (when (pair? cands)
            (let ([chosen (f cands "> ")])
              (when chosen
                (builtin-cd-path chosen))))))))

  ;; z: with a substring operand, jump to the highest-frecency visited directory
  ;; whose path contains it AND still exists; with no operand, open the picker.
  ;; The substring reaches the visit database only as a Scheme-side filter over a
  ;; static query -- never concatenated into SQL -- and the jump is always
  ;; builtin-cd-path, so the stored path is entered literally.  file-directory?
  ;; is handed to the query so a top-ranked directory that has since been deleted
  ;; is skipped for the next-best match still on disk, rather than jumping onto a
  ;; dead path and erroring; existence is the filesystem layer's to know, so the
  ;; store stays filesystem-free and the check lives here.  A no-op when nothing
  ;; matches, no match still exists, or the database is unavailable.
  (define (builtin-z args)
    (if (null? args)
        (run-z-picker!)
        (let ([best (visit-query-best (ensure-visit-db!) (car args)
                                      ((visit-now)) file-directory?)])
          (when best
            (builtin-cd-path best)))))

  ;; zi: always open the interactive picker, whatever the operands -- the
  ;; "interactive" companion to z.
  (define (builtin-zi args)
    (run-z-picker!))

  ;; --- pushd ---
  ;; Push the current directory, then change into the operand THROUGH cd-to!, so
  ;; the chdir, the OLDPWD/PWD update and the single visit record all happen in
  ;; one place rather than being duplicated here.  The stack grows only when the
  ;; move succeeded, so a failed pushd leaves the stack untouched and records
  ;; nothing; its failure is reported by cd-to! under the cd: prefix (the success
  ;; path is unchanged, which is all the builtins suite asserts).
  (define (builtin-pushd args)
    (if (null? args)
        (display "pushd: no directory specified\n" (console-error-port))
        (let ([old (cwd)])
          (when (cd-to! (car args))
            (set! dir-stack-list (cons old dir-stack-list))))))

  ;; --- popd ---
  ;; Pop the top of the directory stack and change into it, again through cd-to!
  ;; so the move records exactly like any other directory change.  The entry is
  ;; dropped only when the move succeeded; a failed popd keeps it (and reports
  ;; under the cd: prefix).
  (define (builtin-popd args)
    (if (null? dir-stack-list)
        (display "popd: directory stack empty\n" (console-error-port))
        (let ([target (car dir-stack-list)])
          (when (cd-to! target)
            (set! dir-stack-list (cdr dir-stack-list))))))

  ;; --- export ---
  ;; Apply every VAR=value pair, not just the first: split each argument on its
  ;; first `=` and set the variable. A bare NAME with no `=` marks an
  ;; already-set variable exported by re-setting its current value (a not-yet-set
  ;; bare name is left alone), so `export A=1 B=2 C` sets A and B and marks C.
  ;;
  ;; When an assignment changes PATH, re-read $PATH into the exec-path search
  ;; list and repopulate the classifier's command cache once, so a command that
  ;; has just become reachable classifies as a shell command within the session.
  ;; The rebuild fires only for a PATH assignment, not on every export.
  (define (builtin-export args)
    (let ([path-changed? #f])
      (for-each
        (lambda (arg)
          (let ([eqpos (let loop ([i 0])
                         (cond
                           [(>= i (string-length arg)) #f]
                           [(char=? (string-ref arg i) #\=) i]
                           [else (loop (+ i 1))]))])
            (if eqpos
                (let ([var (substring arg 0 eqpos)])
                  (setenv var (substring arg (+ eqpos 1) (string-length arg)))
                  (when (string=? var "PATH")
                    (set! path-changed? #t)))
                (let ([cur (getenv arg)])
                  (when cur (setenv arg cur))))))
        args)
      (when path-changed?
        (init-exec-path-list)
        (rebuild-path-cache!))))

  ;; --- alias / unalias ---
  ;; Index of the first `=` in S, or #f when there is none.
  (define (first-equals-index s)
    (let ([len (string-length s)])
      (let loop ([i 0])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref s i) #\=) i]
          [else (loop (+ i 1))]))))

  ;; Join operand strings with a single space.  The space form of an alias
  ;; definition takes every operand after the name as the expansion, so both
  ;; `alias ll 'ls -la'` and `alias ll ls -la` yield `ls -la` rather than
  ;; silently dropping the trailing words.
  (define (join-with-space strs)
    (if (null? strs)
        ""
        (fold-left (lambda (acc s) (string-append acc " " s))
                   (car strs)
                   (cdr strs))))

  ;; Write one alias, unless the name is empty (a malformed definition) or names
  ;; a builtin.  A builtin is never shadowable by an alias -- the classifier's
  ;; expander already declines to expand a builtin head, and refusing to store it
  ;; here keeps the table honest -- so such an attempt is dropped quietly.
  (define (define-one-alias! name expansion orig)
    (cond
      [(string=? name "")
       (display (format "alias: ~a: invalid alias definition\n" orig)
                (console-error-port))]
      [(builtin? name) #f]
      [else (alias-set! name expansion)]))

  ;; The alias builtin.  With no operands it lists every alias as name=expansion,
  ;; one per line, sorted for a stable display (zsh-like).  With operands it
  ;; defines an alias from either form the shared tokeniser can hand us:
  ;;   alias ll='ls -la'  -> one operand "ll=ls -la"; split on the first `=`.
  ;;   alias ll 'ls -la'  -> operands ("ll" "ls -la"); the name then the
  ;;                          expansion (the remaining operands, space-joined).
  ;; A lone bare name with neither `=` nor an expansion operand prints just that
  ;; alias, rather than defining it with an empty expansion.  No positional
  ;; substitution is performed: an alias maps a head to an expansion string and
  ;; the classifier appends the user's remaining words verbatim.
  (define (builtin-alias args)
    (cond
      [(null? args)
       (for-each
         (lambda (name)
           (display name)
           (display "=")
           (display (or (alias-ref name) ""))
           (newline))
         (list-sort string<? (alias-names)))]
      [else
       (let* ([head (car args)]
              [eqpos (first-equals-index head)])
         (cond
           [eqpos
            (define-one-alias! (substring head 0 eqpos)
                               (substring head (+ eqpos 1) (string-length head))
                               head)]
           [(pair? (cdr args))
            (define-one-alias! head (join-with-space (cdr args)) head)]
           [else
            (let ([value (alias-ref head)])
              (if value
                  (begin (display head) (display "=") (display value) (newline))
                  (display (format "alias: ~a: not found\n" head)
                           (console-error-port))))]))]))

  ;; The unalias builtin.  Remove each named alias; an unknown name is ignored
  ;; (idempotent), matching the tolerant style of the other builtins.
  (define (builtin-unalias args)
    (for-each (lambda (name) (alias-remove! name)) args))

  ;; --- Dispatcher ---
  ;; Split and expand the whole line once through the shared parser tokeniser,
  ;; so builtin arguments undergo the same $/~/quote/escape/glob expansion as an
  ;; external command's. Because the tokeniser skips a leading empty word,
  ;; leading whitespace before the command name is tolerated ("   cd /tmp").
  (define (run-builtin! str)
    (let* ([words (parse-command-words str)]
           [cmd (if (null? words) "" (car words))]
           [args (if (null? words) '() (cdr words))])
      (cond
          [(string=? cmd "cd") (builtin-cd args)]
          [(string=? cmd "pushd") (builtin-pushd args)]
          [(string=? cmd "popd") (builtin-popd args)]
          [(string=? cmd "export") (builtin-export args)]
          [(string=? cmd "alias") (builtin-alias args)]
          [(string=? cmd "unalias") (builtin-unalias args)]
          [(string=? cmd "z") (builtin-z args)]
          [(string=? cmd "zi") (builtin-zi args)]
          [(string=? cmd "jobs") (list-jobs)]
          [(string=? cmd "fg") (job-fg! (if (null? args) "" (car args)))]
          [(string=? cmd "bg") (job-bg-resume! (if (null? args) "" (car args)))]
          [else (display (format "~a: not a builtin\n" cmd) (console-error-port))])))
)
