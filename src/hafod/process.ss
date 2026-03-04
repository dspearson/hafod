;;; (hafod process) -- Process operations for hafod
;;; Provides fork, exec, exit, pipeline, and sleep operations.
;;; Ported from scsh/scheme/process.scm — simplified for single-threaded v1.
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026 Dominic Pearson.

(library (hafod process)
  (export
    ;; Fork
    fork %fork

    ;; Exec
    exec exec-path exec/env exec-path/env
    exec-path-search exec-path-list %exec
    halts? init-exec-path-list

    ;; Exit
    exit %exit call-terminally

    ;; Pipelines
    fork/pipe %fork/pipe fork/pipe+ %fork/pipe+
    pipe* tail-pipe tail-pipe+

    ;; Sleep
    process-sleep process-sleep-until

    ;; Utilities
    preserve-ports split-colon-list suspend

    ;; Fast spawn (posix_spawn)
    spawn-program spawn-program/pipe

    ;; Fast pipelines (posix_spawn)
    spawn-pipeline spawn-pipeline/port)

  (import (hafod internal base)
          (hafod posix) (hafod compat) (hafod procobj)
          (hafod fd-ports) (hafod process-state) (hafod environment)
          (hafod fname) (hafod exit-hooks))

  ;; ======================================================================
  ;; Utilities
  ;; ======================================================================

  ;; Split a colon-separated string into a list of strings.
  ;; "" -> (), "a:b:c" -> ("a" "b" "c"), "single" -> ("single")
  (define (split-colon-list str)
    (let ([len (string-length str)])
      (if (zero? len) '()
          (let loop ([i 0])
            (let scan ([j i])
              (cond
                [(= j len) (list (substring str i len))]
                [(char=? (string-ref str j) #\:)
                 (cons (substring str i j) (loop (+ j 1)))]
                [else (scan (+ j 1))]))))))

  ;; Find the index of a character in a string, or #f.
  (define (string-index-of str ch)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(= i len) #f]
          [(char=? (string-ref str i) ch) i]
          [else (loop (+ i 1))]))))

  ;; Check if a file is executable. Returns #t or #f.
  ;; posix-access returns 0 on success, -1 on failure (no exception raised).
  (define (file-executable? path)
    (zero? (posix-access path X_OK)))

  ;; ======================================================================
  ;; PATH search
  ;; ======================================================================

  ;; Parameter holding the search path as a list of directory strings.
  ;; Initialized from $PATH at library load time.
  (define exec-path-list
    (make-parameter
      (cond [(posix-getenv "PATH") => split-colon-list]
            [else '()])))

  ;; halts? -- trivial predicate, always returns #f in hafod.
  ;; scsh uses this to mark process forms that halt the process.
  (define (halts?) #f)

  ;; init-exec-path-list: re-read $PATH into the exec-path-list parameter.
  (define (init-exec-path-list)
    (exec-path-list
      (cond [(posix-getenv "PATH") => split-colon-list]
            [else '()])))

  ;; Search PATH directories for an executable program.
  ;; If prog contains a slash, check it directly (no path search).
  ;; Returns full path or #f.
  (define (exec-path-search prog path-list)
    (cond
      [(string-index-of prog #\/)
       ;; Contains slash — use directly, check if executable
       (and (file-executable? prog) prog)]
      [else
       (let loop ([dirs path-list])
         (if (null? dirs) #f
             (let ([candidate (string-append (car dirs) "/" prog)])
               (if (file-executable? candidate)
                   candidate
                   (loop (cdr dirs))))))]))

  ;; ======================================================================
  ;; Exec
  ;; ======================================================================

  ;; All resources that must be aligned before fork/exec.
  (define *all-resources*
    (list environ-resource cwd-resource umask-resource euid-resource egid-resource))

  ;; Low-level exec: stringify args, choose execvp vs execve based on env.
  (define (%exec prog argv env)
    (let ([argv (map stringify argv)])
      (if env
          ;; env is an alist — convert to "KEY=VALUE" strings for execve
          (let ([env-strings (map (lambda (pair)
                                    (string-append (car pair) "=" (cdr pair)))
                                  env)])
            (posix-execve prog argv env-strings))
          ;; No env — use execvp (PATH search + inherited env)
          (posix-exec prog argv))))

  ;; exec/env: flush, align resources, exec with optional environment alist.
  (define (exec/env prog env . arglist)
    (flush-all-ports)
    (with-resources-aligned *all-resources*
      (lambda () (%exec (stringify prog) (cons (stringify prog) arglist) env))))

  ;; exec: exec without custom environment.
  (define (exec prog . arglist)
    (apply exec/env prog #f arglist))

  ;; exec-path/env: flush, align, search PATH, exec with optional environment.
  (define (exec-path/env prog env . arglist)
    (flush-all-ports)
    (with-resources-aligned *all-resources*
      (lambda ()
        (let ([prog (stringify prog)])
          (cond
            [(string-index-of prog #\/)
             ;; Contains slash — no PATH search
             (%exec prog (cons prog (map stringify arglist)) env)]
            [else
             ;; Search PATH
             (let ([found (exec-path-search prog (exec-path-list))])
               (if found
                   (%exec found (cons prog (map stringify arglist)) env)
                   (error 'exec-path/env "No executable found" prog)))])))))

  ;; exec-path: exec with PATH search, no custom environment.
  (define (exec-path prog . arglist)
    (apply exec-path/env prog #f arglist))

  ;; ======================================================================
  ;; Exit
  ;; ======================================================================

  ;; %exit: immediate _exit, no hooks, no flushing.
  (define (%exit . maybe-status)
    (posix-_exit (:optional maybe-status 0)))

  ;; exit: run exit hooks, flush ports, then _exit.
  ;; Uses call-exit-hooks-and-run from (hafod exit-hooks).
  (define (exit . maybe-status)
    (let ([status (:optional maybe-status 0)])
      (call-exit-hooks-and-run
        (lambda ()
          (flush-all-ports)
          (posix-_exit status)))))

  ;; call-terminally: run thunk, flush ports, then %exit 0. Never returns.
  ;; Used in child processes to ensure they don't return into parent's continuation.
  (define (call-terminally thunk)
    (thunk)
    (flush-all-ports)
    (%exit 0))

  ;; ======================================================================
  ;; Preserve ports
  ;; ======================================================================

  ;; Capture current stdin/stdout/stderr and return a thunk that restores them
  ;; around the given thunk. Used in fork to ensure child runs with correct ports.
  (define (preserve-ports thunk)
    (let ([cin (current-input-port)]
          [cout (current-output-port)]
          [cerr (current-error-port)])
      (lambda ()
        (with-current-input-port* cin
          (lambda ()
            (with-current-output-port* cout
              (lambda ()
                (with-current-error-port* cerr thunk))))))))

  ;; ======================================================================
  ;; Fork
  ;; ======================================================================

  ;; Internal fork implementation.
  ;; clear-interactive?: if #t, flush all ports and align all resources.
  ;;                     if #f, only align environ (for internal pipeline use).
  ;; maybe-thunk: if provided, child runs thunk then exits.
  (define (really-fork clear-interactive? maybe-thunk)
    (when clear-interactive?
      (flush-all-ports))
    (drain-port-guardian!)
    (with-resources-aligned
      (if clear-interactive? *all-resources* (list environ-resource))
      (lambda ()
        (let ([child-pid (posix-fork)])
          (if (zero? child-pid)
              ;; Child
              (begin
                (when maybe-thunk
                  (call-terminally (preserve-ports maybe-thunk)))
                ;; If no thunk, child returns #f
                #f)
              ;; Parent
              (new-child-proc child-pid))))))

  ;; fork: fork with port flushing and full resource alignment.
  ;; (fork) => proc in parent, #f in child
  ;; (fork thunk) => proc in parent, child runs thunk and exits
  (define (fork . rest)
    (let-optionals* rest ([maybe-thunk #f])
      (really-fork #t maybe-thunk)))

  ;; %fork: fork without flushing or clearing interactive state.
  ;; Used internally for pipeline construction.
  (define (%fork . rest)
    (let-optionals* rest ([maybe-thunk #f])
      (really-fork #f maybe-thunk)))

  ;; ======================================================================
  ;; Fork/pipe
  ;; ======================================================================

  ;; Common code for fork/pipe and %fork/pipe.
  ;; Creates a pipe, forks. Parent: close write end, move read to fd 0 (stdin).
  ;; Child: close read end, move write to fd 1 (stdout).
  ;; If thunk provided, child runs it via call-terminally.
  (define (really-fork/pipe forker rest)
    (let-optionals* rest ([maybe-thunk #f])
      (receive (r w) (pipe)
        (let ([p (forker)])
          (cond
            [p  ;; Parent (forker returned proc object)
             (close w)
             (move->fdes r 0)
             p]
            [else  ;; Child (forker returned #f)
             (close r)
             (move->fdes w 1)
             (when maybe-thunk
               (call-terminally maybe-thunk))
             #f])))))

  (define (fork/pipe . rest)
    (really-fork/pipe fork rest))

  (define (%fork/pipe . rest)
    (really-fork/pipe %fork rest))

  ;; ======================================================================
  ;; Fork/pipe+ (generalized connections)
  ;; ======================================================================

  ;; Fork/pipe with explicit connection list.
  ;; Each connection is (from-child to-parent):
  ;;   from-child = fd number in child to write to
  ;;   to-parent  = fd number in parent to read from
  ;; Example: ((1 0)) = connect child's stdout to parent's stdin
  ;; Example: ((1 0) (2 0)) = connect child's stdout AND stderr to parent's stdin
  (define (really-fork/pipe+ forker conns rest)
    (let-optionals* rest ([maybe-thunk #f])
      ;; Create one pipe per connection
      (let ([pipes (map (lambda (conn)
                          (receive (r w) (pipe) (cons r w)))
                        conns)])
        (let ([p (forker)])
          (cond
            [p  ;; Parent
             (for-each (lambda (conn r/w)
                         (let ([to-parent (cadr conn)]
                               [r (car r/w)]
                               [w (cdr r/w)])
                           (close w)
                           (move->fdes r to-parent)))
                       conns pipes)
             p]
            [else  ;; Child
             (for-each (lambda (conn r/w)
                         (let ([from-child (car conn)]
                               [r (car r/w)]
                               [w (cdr r/w)])
                           (close r)
                           (move->fdes w from-child)))
                       conns pipes)
             (when maybe-thunk
               (call-terminally maybe-thunk))
             #f])))))

  (define (fork/pipe+ conns . rest)
    (really-fork/pipe+ fork conns rest))

  (define (%fork/pipe+ conns . rest)
    (really-fork/pipe+ %fork conns rest))

  ;; ======================================================================
  ;; Pipeline construction
  ;; ======================================================================

  ;; pipe*: lay a pipeline, one process per thunk. Last thunk runs in
  ;; current process via call-terminally. Never returns.
  (define (pipe* . thunks)
    (if (null? thunks)
        (error 'pipe* "No thunks passed to pipe*")
        (let loop ([thunks thunks])
          (let ([thunk (car thunks)]
                [rest (cdr thunks)])
            (if (pair? rest)
                (begin (fork/pipe thunk)
                       (loop rest))
                (call-terminally thunk))))))

  ;; tail-pipe: fork a with pipe output, run b in current process.
  (define (tail-pipe a b)
    (fork/pipe a)
    (call-terminally b))

  ;; tail-pipe+: same with explicit connections.
  (define (tail-pipe+ conns a b)
    (fork/pipe+ conns a)
    (call-terminally b))

  ;; ======================================================================
  ;; Sleep
  ;; ======================================================================

  ;; Sleep for the given number of seconds.
  (define (process-sleep secs)
    (posix-sleep (if (exact? secs)
                     secs
                     (exact (floor secs)))))

  ;; Sleep until the specified absolute time (seconds since epoch).
  (define (process-sleep-until when-time)
    (let loop ()
      (let* ([now (time-second (current-time))]
             [remaining (- when-time now)])
        (when (> remaining 0)
          (posix-sleep (exact (ceiling remaining)))
          (loop)))))

  ;; ======================================================================
  ;; Miscellaneous
  ;; ======================================================================

  ;; Suspend the current process (send SIGSTOP to self).
  (define (suspend)
    (posix-kill (posix-getpid) SIGSTOP))

  ;; ======================================================================
  ;; Fast spawn via posix_spawn (avoids fork overhead)
  ;; ======================================================================

  ;; Build "KEY=VALUE" string list from Scheme environment for posix_spawnp.
  (define (spawn-env-strings)
    (alist->env-list (env->alist)))

  ;; Temporarily align CWD and umask to Scheme parameters for spawn,
  ;; then restore. Environment is passed explicitly via envp, not aligned.
  (define (with-cwd+umask-aligned thunk)
    (let ([os-cwd (posix-getcwd)]
          [os-umask (posix-umask 0)])
      (posix-umask os-umask)  ;; restore from the read
      (let ([target-cwd (cwd)]
            [target-umask (umask)])
        (dynamic-wind
          (lambda ()
            (unless (string=? os-cwd target-cwd)
              (posix-chdir target-cwd))
            (unless (= os-umask target-umask)
              (posix-umask target-umask)))
          thunk
          (lambda ()
            (unless (string=? os-cwd target-cwd)
              (posix-chdir os-cwd))
            (unless (= os-umask target-umask)
              (posix-umask os-umask)))))))

  ;; spawn-program: run a program without fork(). Returns a proc object.
  ;; prog: program name (PATH-searched), args: list of string arguments.
  ;; Optional file-actions: list of (dup2 old new), (close fd), (open fd path flags mode).
  ;; Passes the Scheme environment explicitly via posix_spawnp envp argument.
  ;; Temporarily aligns CWD/umask to Scheme parameters (saves/restores parent state).
  (define spawn-program
    (case-lambda
      [(prog args)
       (spawn-program prog args '())]
      [(prog args file-actions)
       (flush-all-ports)
       (let ([env-strs (spawn-env-strings)])
         (with-cwd+umask-aligned
           (lambda ()
             (let ([pid (posix-spawnp (stringify prog)
                          (map stringify (cons prog args))
                          (if (null? file-actions) #f file-actions)
                          env-strs)])
               (new-child-proc pid)))))]))

  ;; spawn-program/pipe: spawn with stdout piped to parent. Returns proc object.
  ;; Parent's stdin (fd 0) is replaced with the read end of the pipe.
  (define (spawn-program/pipe prog args . maybe-extra-actions)
    (flush-all-ports)
    (let ([env-strs (spawn-env-strings)])
      (with-cwd+umask-aligned
        (lambda ()
          (let-values ([(pid rfd)
                        (posix-spawnp/pipe (stringify prog)
                          (map stringify (cons prog args))
                          (if (null? maybe-extra-actions) '() (car maybe-extra-actions))
                          env-strs)])
            (move->fdes (fdes->inport rfd) 0)
            (new-child-proc pid))))))

  ;; ======================================================================
  ;; Fast pipelines via posix_spawn (avoids multiple forks)
  ;; ======================================================================

  ;; Helper: create N pipes, returning list of (rfd . wfd) pairs.
  (define (make-pipes n)
    (let loop ([i 0] [acc '()])
      (if (= i n) (reverse acc)
          (loop (+ i 1) (cons (posix-pipe) acc)))))

  ;; Helper: build file-actions list for a pipeline stage.
  ;; stage: 0-based index, n: total stages, pipes: list of (rfd . wfd).
  ;; extra-wfd: if not #f, dup2 this fd to stdout for last stage (output pipe).
  (define (pipeline-file-actions stage n pipes all-fds extra-wfd)
    (append
      ;; stdin from previous pipe (if not first stage)
      (if (> stage 0)
          (list (list 'dup2 (car (list-ref pipes (- stage 1))) 0))
          '())
      ;; stdout to next pipe (if not last stage)
      ;; or to extra-wfd (if last stage and collecting output)
      (cond
        [(< stage (- n 1))
         (list (list 'dup2 (cdr (list-ref pipes stage)) 1))]
        [extra-wfd
         (list (list 'dup2 extra-wfd 1))]
        [else '()])
      ;; Close all pipe fds (child doesn't need them after dup2)
      (map (lambda (fd) (list 'close fd)) all-fds)))

  ;; spawn-pipeline: spawn all stages of a pipeline using posix_spawn.
  ;; cmds: list of (prog arg ...) lists, already evaluated.
  ;; Returns list of proc objects (one per stage), last element is last stage.
  (define (close-pipe-fds pipes)
    (for-each (lambda (p)
                (posix-close (car p))
                (posix-close (cdr p)))
              pipes))

  (define (spawn-pipeline cmds)
    (flush-all-ports)
    (let* ([n (length cmds)]
           [env-strs (spawn-env-strings)])
      (with-cwd+umask-aligned
        (lambda ()
          (let* ([pipes (make-pipes (- n 1))]
                 [all-fds (append (map car pipes) (map cdr pipes))])
            (guard (e [#t (close-pipe-fds pipes) (raise e)])
              (let ([procs
                     (let loop ([cmds cmds] [stage 0] [acc '()])
                       (if (null? cmds) (reverse acc)
                           (let* ([cmd (car cmds)]
                                  [prog (stringify (car cmd))]
                                  [args (map stringify (cdr cmd))]
                                  [fa (pipeline-file-actions stage n pipes all-fds #f)]
                                  [pid (posix-spawnp prog (cons prog args) fa env-strs)])
                             (loop (cdr cmds) (+ stage 1)
                                   (cons (new-child-proc pid) acc)))))])
                ;; Close all pipe fds in parent
                (close-pipe-fds pipes)
                procs)))))))

  ;; spawn-pipeline/port: spawn pipeline with last stage stdout piped back.
  ;; Returns (values input-port proc-list).
  (define (spawn-pipeline/port cmds)
    (flush-all-ports)
    (let* ([n (length cmds)]
           [env-strs (spawn-env-strings)])
      (with-cwd+umask-aligned
        (lambda ()
          (let* ([pipes (make-pipes (- n 1))]
                 [out-pipe (posix-pipe)]
                 [out-rfd (car out-pipe)]
                 [out-wfd (cdr out-pipe)]
                 [all-fds (append (map car pipes) (map cdr pipes)
                                  (list out-rfd out-wfd))])
            (guard (e [#t
                       (close-pipe-fds pipes)
                       (posix-close out-rfd)
                       (posix-close out-wfd)
                       (raise e)])
              (let ([procs
                     (let loop ([cmds cmds] [stage 0] [acc '()])
                       (if (null? cmds) (reverse acc)
                           (let* ([cmd (car cmds)]
                                  [prog (stringify (car cmd))]
                                  [args (map stringify (cdr cmd))]
                                  [last? (null? (cdr cmds))]
                                  [fa (pipeline-file-actions stage n pipes all-fds
                                        (and last? out-wfd))]
                                  [pid (posix-spawnp prog (cons prog args) fa env-strs)])
                             (loop (cdr cmds) (+ stage 1)
                                   (cons (new-child-proc pid) acc)))))])
                ;; Close all pipe fds in parent
                (close-pipe-fds pipes)
                (posix-close out-wfd)
                ;; Return read port and procs
                (let ([port (fdes->inport out-rfd)])
                  (release-port-handle port)
                  (values port procs)))))))))

) ;; end library
