;;; (hafod signal) -- Signal sending for hafod
;;; Provides signal-process, signal-process-group, and signal constant re-exports.
;;; Ported from scsh/scheme/signal.scm
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod signal)
  (export
    signal-process signal-process-group pause
    signal  ;; scsh-compatible (signal name) syntax macro
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
      (posix-kill (- pgid) signal)))

  ;; pause: suspend until a signal is delivered. (POSIX pause(2))
  (define (pause) (posix-pause))

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
