;;; (hafod procobj) -- Process objects and wait/reap for hafod
;;; Provides proc record type, pid->proc table, wait, wait-any, reap-zombies.
;;; Ported from scsh/scheme/procobj.scm — heavily simplified for single-threaded v1.
;;; No placeholders, locks, weak tables, reaped-proc queue, or autoreap policy.
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod procobj)
  (export
    ;; Process record
    proc? proc:pid proc:finished? proc:status proc:zombie? proc:stopped?

    ;; Process table
    new-child-proc pid->proc maybe-pid->proc ->proc pid/proc?

    ;; Wait
    wait wait-any

    ;; Reap
    reap-zombies

    ;; Internal (for process.ss)
    obituary mark-proc-waited!

    ;; Job control (for shell/jobs.ss)
    mark-proc-continued!

    ;; Background job count
    background-job-count

    ;; Re-export wait flags and status decoders for convenience
    wait/poll wait/stopped-children
    status:exit-val status:term-sig status:stop-sig)

  (import (hafod internal base)
          (hafod posix) (hafod compat))

  ;; ======================================================================
  ;; Process record type
  ;; ======================================================================

  ;; proc: a GC'd abstraction for Unix process ids.
  ;; - pid: integer process id
  ;; - finished?: #f while running, #t when terminated
  ;; - status: the wait(2) status integer, set when reaped (#f until then)
  ;; - zombie?: #t when reaped but not yet waited on by user code
  ;; - stopped?: #t while the child is stopped-but-alive (SIGSTOP/SIGTSTP),
  ;;   #f otherwise. Independent of finished? — a stopped child is still alive.
  (define-record-type proc
    (fields pid (mutable finished?) (mutable status) (mutable zombie?)
            (mutable stopped?))
    (protocol (lambda (new) (lambda (pid) (new pid #f #f #t #f)))))

  ;; scsh-compatible accessor names
  (define proc:pid proc-pid)
  (define proc:finished? proc-finished?)
  (define proc:status proc-status)
  (define proc:zombie? proc-zombie?)
  (define proc:stopped? proc-stopped?)

  ;; ======================================================================
  ;; Process table: pid -> proc mapping
  ;; ======================================================================

  (define *process-table* (make-eqv-hashtable))

  (define (process-table-ref pid)
    (hashtable-ref *process-table* pid #f))

  (define (process-table-set! pid proc)
    (hashtable-set! *process-table* pid proc))

  ;; ======================================================================
  ;; Process creation / lookup
  ;; ======================================================================

  ;; Create a new proc for a child pid and register it.
  (define (new-child-proc pid)
    (let ([p (make-proc pid)])
      (process-table-set! pid p)
      p))

  ;; Look up proc by pid. Returns proc or #f without error.
  (define (maybe-pid->proc pid)
    (process-table-ref pid))

  ;; Look up proc by pid with configurable probe behavior.
  ;; probe? = #f (default): error if not found
  ;; probe? = #t: return #f if not found
  ;; probe? = 'create: auto-create and register if not found
  (define (pid->proc pid . maybe-probe?)
    (let ([probe? (:optional maybe-probe? #f)])
      (or (maybe-pid->proc pid)
          (case probe?
            [(#f) (error 'pid->proc "Pid has no corresponding process object" pid)]
            [(create) (new-child-proc pid)]
            [else #f]))))

  ;; Coerce proc or pid integer to proc.
  (define (->proc proc/pid)
    (cond
      [(proc? proc/pid) proc/pid]
      [(and (integer? proc/pid) (>= proc/pid 0))
       (pid->proc proc/pid 'create)]
      [else (error '->proc "Illegal argument" proc/pid)]))

  ;; Predicate: is x a pid (non-negative integer) or a proc?
  (define (pid/proc? x)
    (or (proc? x) (and (integer? x) (>= x 0))))

  ;; ======================================================================
  ;; Obituary / mark-waited
  ;; ======================================================================

  ;; Mark a proc as finished and cache its wait status.
  ;; A finished proc is never stopped, so clear the stopped flag as well.
  (define (obituary p status)
    (proc-status-set! p status)
    (proc-stopped?-set! p #f)
    (proc-finished?-set! p #t))

  ;; Record a stopped-but-alive child: cache the wait status and flag it stopped,
  ;; without marking it finished. Internal — only reap-zombies records a stop.
  (define (mark-proc-stopped! p status)
    (proc-status-set! p status)
    (proc-stopped?-set! p #t))

  ;; Clear the stopped flag when the child is continued (SIGCONT). The kernel's
  ;; WUNTRACED stop is one-shot and a continue is not reported, so the flag would
  ;; otherwise go stale and a resumed child would be falsely reported stopped.
  (define (mark-proc-continued! p)
    (proc-stopped?-set! p #f))

  ;; Mark that user code has consumed this proc's status.
  (define (mark-proc-waited! p)
    (proc-zombie?-set! p #f))

  ;; ======================================================================
  ;; Wait
  ;; ======================================================================

  ;; (wait proc/pid [flags]) => status or #f
  ;; FLAGS (default 0) is bitwise-ior of wait/poll, wait/stopped-children.
  ;; Returns the wait(2) status integer, or #f if wait/poll and not ready.
  ;; If the process was already waited on, returns cached status.
  (define (wait pid/proc . maybe-flags)
    (let* ([flags (:optional maybe-flags 0)]
           [p (->proc pid/proc)])
      (if (proc:finished? p)
          ;; Already finished — return cached status (multi-wait support)
          (proc:status p)
          ;; Not yet finished — call waitpid
          (receive (wpid status)
            (posix-waitpid (proc:pid p) flags)
            (cond
              [(zero? wpid) #f]  ;; wait/poll and process not ready
              [(status:stop-sig status)
               ;; Stopped, not dead (WUNTRACED). Record the one-shot stop so a
               ;; foregrounded job can re-foreground it; leave finished? #f — a
               ;; stopped child is still alive. Mirrors the reap-zombies stop path.
               (mark-proc-stopped! p status)
               status]
              [else
               (obituary p status)
               (mark-proc-waited! p)
               status])))))

  ;; (wait-any [flags]) => (values proc status)
  ;; Wait for any child process.
  ;; Returns:
  ;;   (values proc status) — a child terminated
  ;;   (values #f #f)       — wait/poll and none ready
  ;;   (values #f #t)       — no children exist (ECHILD)
  (define (wait-any . maybe-flags)
    (let ([flags (:optional maybe-flags 0)])
      (guard (e [(and (posix-error? e) (= (posix-errno e) 10))  ;; ECHILD
                 (values #f #t)])
        (receive (wpid status)
          (posix-waitpid -1 flags)
          (if (zero? wpid)
              (values #f #f)  ;; poll, none ready
              (let ([p (pid->proc wpid 'create)])
                (obituary p status)
                (mark-proc-waited! p)
                (values p status)))))))

  ;; ======================================================================
  ;; Reap zombies
  ;; ======================================================================

  ;; (reap-zombies) => bool
  ;; Move any zombies from the kernel process table into hafod.
  ;; Return #t if no more outstanding children; #f if some still live.
  (define (reap-zombies)
    (let loop ()
      (guard (e [(and (posix-error? e) (= (posix-errno e) 10))  ;; ECHILD
                 #t])  ;; No children at all
        (receive (wpid status)
          (posix-waitpid -1 (bitwise-ior wait/poll wait/stopped-children))
          (cond
            [(zero? wpid) #f]  ;; Some children still live
            [(status:stop-sig status)
             ;; Stopped, not dead. Record the one-shot WUNTRACED stop on the proc
             ;; so job control can report it; do not reap (the child is alive).
             (mark-proc-stopped! (pid->proc wpid 'create) status)
             #f]
            [else
             (let ([p (pid->proc wpid 'create)])
               (obituary p status)
               (proc-zombie?-set! p #t))
             (loop)])))))

  ;; ======================================================================
  ;; Background job count
  ;; ======================================================================

  ;; (background-job-count) => integer
  ;; Count the number of unfinished (still running) processes in the process table.
  ;; Calls reap-zombies first to update the table.
  (define (background-job-count)
    (reap-zombies)
    (let ([count 0])
      (let-values ([(keys vals) (hashtable-entries *process-table*)])
        (vector-for-each
          (lambda (p)
            (unless (proc:finished? p)
              (set! count (+ count 1))))
          vals))
      count))

  ;; ======================================================================
  ;; Custom printer (expression — must be after all definitions)
  ;; ======================================================================

  (record-writer (record-type-descriptor proc)
    (lambda (r p wr)
      (display "#<proc " p)
      (display (proc-pid r) p)
      (when (proc-finished? r)
        (display " finished" p))
      (display ">" p)))

) ;; end library
