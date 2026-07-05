;;; (hafod signal) -- Signal sending for hafod
;;; Provides signal-process, signal-process-group, a prior-handler disposition
;;; registry (set/get/scoped-restore), a fatal re-raise primitive, and signal
;;; constant re-exports.
;;; Ported from scsh/scheme/signal.scm
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod signal)
  (export
    signal-process signal-process-group pause
    signal  ;; scsh-compatible (signal name) syntax macro
    ;; Prior-handler disposition registry
    set-signal-handler! current-signal-handler with-signal-handler
    ;; Reset-to-default + re-raise, so a caught fatal dies with the right status
    die-by-signal
    ;; Restore a signal's default disposition (e.g. let SIGPIPE terminate quietly)
    reset-signal-to-default!
    ;; Re-export signal constants from (hafod posix)
    SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGBUS SIGFPE SIGKILL
    SIGUSR1 SIGSEGV SIGUSR2 SIGPIPE SIGALRM SIGTERM SIGCHLD SIGCONT
    SIGSTOP SIGTSTP SIGTTIN SIGTTOU
    SIGURG SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGIO SIGPWR SIGSYS)

  (import (chezscheme) (hafod posix) (hafod procobj))

  ;; signal-process: send a signal to a process.
  ;; Accepts either an integer pid or a proc object.
  (define (signal-process proc/pid signal)
    (let ([pid (cond
                 [(integer? proc/pid) proc/pid]
                 [(proc? proc/pid) (proc:pid proc/pid)]
                 [else (error 'signal-process
                              "argument must be an integer pid or proc object"
                              proc/pid)])])
      ;; A -1 signal marks a constant the platform does not define
      ;; (e.g. SIGPWR on macOS/BSD). Reject it with a clear message rather
      ;; than passing an invalid number to kill(2). Guard on exactly -1:
      ;; signal 0 is the legitimate kill(pid, 0) existence check.
      (when (eqv? signal -1)
        (error 'signal-process
               "signal not available on this platform" signal))
      (posix-kill pid signal)))

  ;; signal-process-group: send a signal to a process group.
  ;; Negates the pid to target the process group.
  ;; Accepts either an integer pid/pgid or a proc object.
  (define (signal-process-group pgrp/pid signal)
    (let ([pgid (cond
                  [(integer? pgrp/pid) pgrp/pid]
                  [(proc? pgrp/pid) (proc:pid pgrp/pid)]
                  [else (error 'signal-process-group
                               "argument must be an integer pid/pgid or proc object"
                               pgrp/pid)])])
      ;; Reject the -1 platform-absent sentinel before kill(2); the pgid is
      ;; deliberately negated below, but the signal argument must be valid.
      ;; Guard on exactly -1 so kill(pid, 0) existence checks keep working.
      (when (eqv? signal -1)
        (error 'signal-process-group
               "signal not available on this platform" signal))
      (posix-kill (- pgid) signal)))

  ;; pause: suspend until a signal is delivered. (POSIX pause(2))
  (define (pause) (posix-pause))

  ;; ---- Prior-handler disposition registry -----------------------------------
  ;; The low-level install primitive (register-signal-handler) returns no useful
  ;; value, so once a new handler is installed the disposition that was live
  ;; before it is lost -- there is no way to ask for it back and restore it. We
  ;; therefore keep our own table of the most recently installed handler for each
  ;; signal. set-signal-handler! returns that recorded prior, which is precisely
  ;; the value the low-level primitive cannot supply, so a scoped swap can put
  ;; back exactly what was there before instead of clobbering it with a no-op.
  ;; Signal numbers are small integers, hence an eqv hashtable.
  (define *handlers* (make-eqv-hashtable))

  ;; set-signal-handler!: install PROC (a one-argument procedure) for SIG, record
  ;; it in the registry, make it live, and return the prior recorded handler for
  ;; SIG (or #f when none was recorded).
  (define (set-signal-handler! sig proc)
    (let ([prior (hashtable-ref *handlers* sig #f)])
      (hashtable-set! *handlers* sig proc)
      (register-signal-handler sig proc)
      prior))

  ;; current-signal-handler: the handler most recently installed for SIG through
  ;; this registry, or #f if none. Lets callers (and tests) read a disposition
  ;; back without delivering a signal.
  (define (current-signal-handler sig)
    (hashtable-ref *handlers* sig #f))

  ;; with-signal-handler: install PROC for SIG for the dynamic extent of THUNK,
  ;; then restore the prior disposition on exit. When there was a genuine prior
  ;; handler it is put back exactly; only when there was truly none do we fall
  ;; back to a benign one-argument no-op. This save-and-restore-prior behaviour
  ;; is what stops a scoped swap from leaving a signal permanently clobbered.
  (define (with-signal-handler sig proc thunk)
    (let ([prior (set-signal-handler! sig proc)])
      (dynamic-wind
        void
        thunk
        (lambda ()
          (if prior
              (set-signal-handler! sig prior)
              (set-signal-handler! sig (lambda (s) (void))))))))

  ;; ---- Fatal re-raise -------------------------------------------------------
  ;; die-by-signal resets SIG to its default disposition and then re-raises it,
  ;; so the process terminates with that signal's own default action and the
  ;; correct wait status rather than re-entering a Scheme handler and looping.
  ;;
  ;; signal(2) is an already-linked libc symbol (no third-party dependency); we
  ;; bind it minimally -- the signal number as an int, the disposition as a
  ;; pointer-width value -- and use SIG_DFL, the null disposition pointer (0).
  ;;
  ;; The caller is responsible for restoring the terminal to cooked mode BEFORE
  ;; calling this, because control never returns. It must never be invoked
  ;; against the running test process -- it would terminate it.
  (define c-signal (foreign-procedure "signal" (int uptr) uptr))
  (define SIG_DFL 0)
  (define (die-by-signal sig)
    (c-signal sig SIG_DFL)               ; restore the default disposition
    (posix-kill (posix-getpid) sig))     ; re-raise; delivery now terminates

  ;; Restore SIG's default disposition without raising it. Chez installs SIG_IGN
  ;; for some signals (notably SIGPIPE, so a broken-pipe write surfaces as an i/o
  ;; exception rather than a signal); resetting to SIG_DFL lets the kernel take
  ;; the default action -- for SIGPIPE, quiet termination at the write syscall.
  (define (reset-signal-to-default! sig)
    (c-signal sig SIG_DFL)
    (void))

  ;; (signal name) -- scsh-compatible syntax macro for signal constants.
  ;; (signal hup) => SIGHUP, (signal int) => SIGINT, etc.
  (define-syntax signal
    (lambda (stx)
      (syntax-case stx ()
        [(_ name)
         (identifier? #'name)
         (let ([sym (syntax->datum #'name)])
           (case sym
             [(hup)     #'SIGHUP]
             [(int)     #'SIGINT]
             [(quit)    #'SIGQUIT]
             [(ill)     #'SIGILL]
             [(trap)    #'SIGTRAP]
             [(abrt iot) #'SIGABRT]
             [(bus)     #'SIGBUS]
             [(fpe)     #'SIGFPE]
             [(kill)    #'SIGKILL]
             [(usr1)    #'SIGUSR1]
             [(segv)    #'SIGSEGV]
             [(usr2)    #'SIGUSR2]
             [(pipe)    #'SIGPIPE]
             [(alrm alarm) #'SIGALRM]
             [(term)    #'SIGTERM]
             [(chld)    #'SIGCHLD]
             [(cont)    #'SIGCONT]
             [(stop)    #'SIGSTOP]
             [(tstp)    #'SIGTSTP]
             [(ttin)    #'SIGTTIN]
             [(ttou)    #'SIGTTOU]
             [(urg)     #'SIGURG]
             [(xcpu)    #'SIGXCPU]
             [(xfsz)    #'SIGXFSZ]
             [(vtalrm)  #'SIGVTALRM]
             [(prof)    #'SIGPROF]
             [(winch)   #'SIGWINCH]
             [(io poll) #'SIGIO]
             [(pwr)     #'SIGPWR]
             [(sys)     #'SIGSYS]
             [else (syntax-violation 'signal "unknown signal name" stx #'name)]))])))

  ) ; end library
