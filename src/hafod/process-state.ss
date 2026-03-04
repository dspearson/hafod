;;; (hafod process-state) -- Process state management for hafod
;;; Provides cwd, umask, pid, process-group, uid/gid, resource records,
;;; and with-resources-aligned for resource alignment before fork/exec.
;;; Ported from scsh/scheme/process-state.scm and scsh/scheme/resource.scm
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod process-state)
  (export
    ;; CWD
    cwd chdir with-cwd with-cwd* process-cwd process-chdir
    ;; Umask
    umask set-umask with-umask with-umask*
    ;; PID
    pid parent-pid
    ;; Process group
    process-group set-process-group become-session-leader
    ;; UID/GID
    user-uid user-effective-uid user-gid user-effective-gid
    user-supplementary-gids
    set-uid set-gid set-user-effective-uid set-user-effective-gid
    with-user-effective-uid with-user-effective-uid*
    with-user-effective-gid with-user-effective-gid*
    ;; Login name, process times
    user-login-name process-times cpu-ticks/sec
    ;; Resources
    resource make-resource resource? resource-name resource-align!
    with-resources-aligned
    cwd-resource umask-resource euid-resource egid-resource)

  (import (hafod internal base)
          (hafod posix) (hafod compat) (hafod fname)
          (hafod user-group) (hafod environment))

  ;; ======================================================================
  ;; Resource record type
  ;; ======================================================================

  ;; Resource descriptor for alignment before fork/exec.
  ;; v1 simplified: no lock (single-threaded Chez Scheme).
  (define-record-type resource
    (fields name align!))

  ;; ======================================================================
  ;; define-dynamic-scoper macro
  ;; ======================================================================

  ;; (define-dynamic-scoper with-X getter setter) expands to:
  ;;   (define (with-X* new-val thunk) ..dynamic-wind save/restore..)
  ;;   (define-simple-syntax (with-X val body ...) (with-X* val (lambda () body ...)))
  (define-syntax define-dynamic-scoper
    (lambda (stx)
      (syntax-case stx ()
        [(_ name getter setter)
         (with-syntax ([name* (datum->syntax #'name
                                (string->symbol
                                  (string-append
                                    (symbol->string (syntax->datum #'name))
                                    "*")))]
                       [dots (syntax (... ...))])
           #'(begin
               (define (name* new-val thunk)
                 (let ([saved (getter)])
                   (dynamic-wind
                     (lambda () (setter new-val))
                     thunk
                     (lambda () (setter saved)))))
               (define-simple-syntax (name val body dots)
                 (name* val (lambda () body dots)))))])))

  ;; ======================================================================
  ;; CWD -- Current Working Directory
  ;; ======================================================================

  ;; CWD parameter: holds the Scheme-side working directory.
  ;; Initialized at library load from the OS cwd.
  (define %cwd (make-parameter (posix-getcwd)))

  ;; Return the Scheme-side current working directory.
  (define (cwd) (%cwd))

  ;; Return the raw OS working directory (bypassing Scheme-side cache).
  (define (process-cwd) (posix-getcwd))

  ;; Low-level chdir: changes the OS cwd directly.
  ;; Defaults to home directory if no argument.
  (define (process-chdir . maybe-dir)
    (let ([dir (:optional maybe-dir (home-dir))])
      (posix-chdir (ensure-file-name-is-nondirectory dir))))

  ;; High-level chdir: changes both OS and Scheme-side cwd.
  ;; Resolves relative paths against the Scheme-side cwd.
  ;; Defaults to home directory if no argument.
  (define (chdir . maybe-dir)
    (let ([dir (:optional maybe-dir (home-dir))])
      (let ([abs (ensure-file-name-is-nondirectory
                   (if (file-name-absolute? dir)
                       dir
                       (string-append (file-name-as-directory (cwd)) dir)))])
        (posix-chdir abs)
        (%cwd abs))))

  ;; Dynamically scope the working directory.
  (define-dynamic-scoper with-cwd cwd chdir)

  ;; Align the OS cwd to match the Scheme-side value.
  ;; Called before fork/exec via with-resources-aligned.
  (define (align-cwd!)
    (let ([scheme-cwd (cwd)])
      (unless (string=? scheme-cwd (posix-getcwd))
        (posix-chdir scheme-cwd))))

  (define cwd-resource (make-resource 'cwd align-cwd!))

  ;; ======================================================================
  ;; Umask -- File creation mask
  ;; ======================================================================

  ;; Read the current process umask.
  ;; umask(2) sets and returns old mask, so we call it twice to read.
  (define (read-umask)
    (let ([m (posix-umask 0)])
      (posix-umask m)
      m))

  ;; Umask parameter: holds the Scheme-side umask.
  (define %umask (make-parameter (read-umask)))

  ;; Return the Scheme-side umask.
  (define (umask) (%umask))

  ;; Set both the OS umask and Scheme-side umask.
  (define (set-umask new-mask)
    (posix-umask new-mask)
    (%umask new-mask))

  ;; Dynamically scope the file creation mask.
  (define-dynamic-scoper with-umask umask set-umask)

  ;; Align the OS umask to match the Scheme-side value.
  (define (align-umask!)
    (let ([scheme-umask (umask)])
      (unless (= scheme-umask (read-umask))
        (posix-umask scheme-umask))))

  (define umask-resource (make-resource 'umask align-umask!))

  ;; ======================================================================
  ;; PID
  ;; ======================================================================

  (define (pid) (posix-getpid))
  (define (parent-pid) (posix-getppid))

  ;; ======================================================================
  ;; Process groups and session
  ;; ======================================================================

  (define (process-group) (posix-getpgrp))

  ;; set-process-group: one or two arguments.
  ;; (set-process-group pgrp) -- set current process to pgrp
  ;; (set-process-group pid pgrp) -- set pid to pgrp
  (define (set-process-group arg1 . maybe-arg2)
    (receive (the-pid pgrp)
             (if (null? maybe-arg2)
                 (values (posix-getpid) arg1)
                 (values arg1 (car maybe-arg2)))
      (posix-setpgid the-pid pgrp)))

  (define (become-session-leader) (posix-setsid))

  ;; ======================================================================
  ;; UID/GID
  ;; ======================================================================

  (define (user-uid) (posix-getuid))
  (define (user-effective-uid) (posix-geteuid))
  (define (user-gid) (posix-getgid))
  (define (user-effective-gid) (posix-getegid))
  (define (user-supplementary-gids) (posix-getgroups))

  (define (set-uid uid) (posix-setuid uid))
  (define (set-gid gid) (posix-setgid gid))
  (define (set-user-effective-uid uid) (posix-seteuid uid))
  (define (set-user-effective-gid gid) (posix-setegid gid))

  ;; Dynamically scope the effective UID (requires appropriate privileges).
  (define-dynamic-scoper with-user-effective-uid user-effective-uid set-user-effective-uid)

  ;; Dynamically scope the effective GID (requires appropriate privileges).
  (define-dynamic-scoper with-user-effective-gid user-effective-gid set-user-effective-gid)

  ;; ======================================================================
  ;; Login name
  ;; ======================================================================

  ;; user-login-name: returns the current user's login name.
  ;; Tries posix-getlogin first; falls back to looking up uid via passwd db.
  (define (user-login-name)
    (or (posix-getlogin)
        (passwd-info-name (user-info (user-uid)))))

  ;; ======================================================================
  ;; Process times and CPU ticks
  ;; ======================================================================

  ;; process-times: returns (values utime stime cutime cstime) in clock ticks.
  ;; Wraps posix-times which returns the same 4 values.
  (define (process-times)
    (posix-times))

  ;; cpu-ticks/sec: returns clock ticks per second via sysconf(_SC_CLK_TCK).
  ;; _SC_CLK_TCK = 2 on Linux.
  (define (cpu-ticks/sec)
    (posix-sysconf 2))

  ;; Align effective UID/GID to Scheme-side values.
  (define (align-euid!)
    (let ([scheme-euid (user-effective-uid)])
      (unless (= scheme-euid (posix-geteuid))
        (posix-seteuid scheme-euid))))

  (define (align-egid!)
    (let ([scheme-egid (user-effective-gid)])
      (unless (= scheme-egid (posix-getegid))
        (posix-setegid scheme-egid))))

  (define euid-resource (make-resource 'euid align-euid!))
  (define egid-resource (make-resource 'egid align-egid!))

  ;; ======================================================================
  ;; Resource alignment
  ;; ======================================================================

  ;; with-resources-aligned: align all resources, then run thunk.
  ;; v1 simplified: no locking, just call align! for each resource.
  ;; Handles both resource records and cons pairs (for environ-resource
  ;; from Plan 01 which uses (name . align-thunk) convention).
  (define (with-resources-aligned resources thunk)
    (for-each (lambda (r)
                (let ([align! (if (resource? r)
                                  (resource-align! r)
                                  ;; Handle environ-resource which is a cons pair
                                  (cdr r))])
                  (align!)))
              resources)
    (thunk))

  ) ; end library
