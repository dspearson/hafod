;;; (hafod shell builtins) -- Shell builtin commands
;;; cd, pushd, popd, export -- must execute in-process.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod shell builtins)
  (export builtin? run-builtin! builtin-names dir-stack builtin-cd-path
          nav-finder-proc)
  (import (except (chezscheme) getenv)
          (only (hafod process-state) chdir cwd)
          (only (hafod environment) getenv setenv)
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

  ;; --- cd ---
  ;; The single directory-change seam.  Every route into a new directory -- cd,
  ;; pushd, popd, auto-cd and the z jump -- passes through here, so the visit is
  ;; recorded in exactly one place and a change is never counted twice.  TARGET is
  ;; entered verbatim: the caller has already resolved any shortcut (empty ->
  ;; home, "-" -> $OLDPWD), so this seam interprets nothing further, which is what
  ;; lets builtin-cd-path reach chdir literally.
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
    (let ([old (cwd)])
      (guard (e [#t (display
                      (format "cd: ~a: ~a\n"
                              target
                              (if (condition? e)
                                  (condition-message e)
                                  e))
                      (console-error-port))
                    #f])
        (chdir target)
        (setenv "OLDPWD" old)
        (setenv "PWD" (cwd))
        (guard (e [#t #f])
          (let ([vdb (ensure-visit-db!)])
            (when vdb (visit-record! vdb (cwd) ((visit-now))))))
        #t)))

  (define (builtin-cd args)
    (let ([target (cond
                    [(null? args) (home-directory)]
                    [(string=? (car args) "-")
                     (or (getenv "OLDPWD")
                         (begin
                           (display "cd: OLDPWD not set\n" (console-error-port))
                           #f))]
                    [else (car args)])])
      (when target
        (cd-to! target))))

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
