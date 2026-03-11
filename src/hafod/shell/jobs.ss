;;; (hafod shell jobs) -- Job table for background process tracking
;;; Manages a table of background jobs with status tracking,
;;; SIGCHLD-driven reaping, and notification at prompt time.

(library (hafod shell jobs)
  (export job-bg! job-fg! job-bg-resume!
          list-jobs update-jobs! drain-notifications!
          job-table-empty?
          install-job-signals!)

  (import (chezscheme)
          (only (hafod posix) posix-setpgid posix-kill posix-tcsetpgrp posix-tcgetpgrp
                posix-getpgrp SIGCONT SIGTSTP SIGTTIN SIGTTOU)
          (only (hafod procobj) proc? proc:pid proc:finished? proc:status
                wait reap-zombies wait/poll wait/stopped-children)
          (only (hafod posix) status:exit-val status:stop-sig status:term-sig)
          (only (hafod signal) signal-process-group)
          (only (hafod process-state) set-process-group))

  ;; ======================================================================
  ;; Job record
  ;; ======================================================================

  (define-record-type job
    (fields id              ; integer job number (1-based)
            command         ; string — display command
            proc            ; proc object from fork
            pgid            ; process group id
            (mutable status)) ; 'running | 'stopped | 'done
    (protocol
      (lambda (new)
        (lambda (id command proc pgid)
          (new id command proc pgid 'running)))))

  ;; ======================================================================
  ;; Job table state
  ;; ======================================================================

  (define *jobs* '())           ; list of job records
  (define *next-id* 1)          ; next job number
  (define *notifications* '())  ; list of notification strings for prompt display

  ;; ======================================================================
  ;; Job table operations
  ;; ======================================================================

  (define (job-table-empty?)
    (null? *jobs*))

  ;; Find job by id. Returns job or #f.
  (define (find-job id)
    (let loop ([jobs *jobs*])
      (cond
        [(null? jobs) #f]
        [(= (job-id (car jobs)) id) (car jobs)]
        [else (loop (cdr jobs))])))

  ;; Find most recent job (highest id). Returns job or #f.
  (define (most-recent-job)
    (if (null? *jobs*) #f
        (let loop ([jobs (cdr *jobs*)] [best (car *jobs*)])
          (cond
            [(null? jobs) best]
            [(> (job-id (car jobs)) (job-id best))
             (loop (cdr jobs) (car jobs))]
            [else (loop (cdr jobs) best)]))))

  ;; Remove completed jobs from table.
  (define (prune-done-jobs!)
    (set! *jobs* (filter (lambda (j) (not (eq? (job-status j) 'done))) *jobs*)))

  ;; Update all jobs by checking process status.
  ;; Adds notifications for state changes.
  (define (update-jobs!)
    (reap-zombies)
    (for-each
      (lambda (j)
        (when (and (not (eq? (job-status j) 'done))
                   (proc:finished? (job-proc j)))
          (let ([st (proc:status (job-proc j))])
            (cond
              [(and st (status:stop-sig st))
               (unless (eq? (job-status j) 'stopped)
                 (job-status-set! j 'stopped)
                 (set! *notifications*
                   (cons (format #f "[~a]  Stopped  ~a" (job-id j) (job-command j))
                         *notifications*)))]
              [else
               (job-status-set! j 'done)
               (let ([code (if (and st (status:exit-val st))
                               (status:exit-val st)
                               (if (and st (status:term-sig st))
                                   (format #f "signal ~a" (status:term-sig st))
                                   "?"))])
                 (set! *notifications*
                   (cons (format #f "[~a]  Done (~a)  ~a" (job-id j) code (job-command j))
                         *notifications*)))]))))
      *jobs*)
    (prune-done-jobs!))

  ;; Drain pending notifications. Returns list of strings (may be empty).
  (define (drain-notifications!)
    (let ([notes (reverse *notifications*)])
      (set! *notifications* '())
      notes))

  ;; ======================================================================
  ;; Job creation (background)
  ;; ======================================================================

  ;; Register a background job. Called with the command string and proc object.
  ;; Prints [N] PID and returns the proc.
  (define (job-bg! cmd-str proc)
    (when (proc? proc)
      (let* ([id *next-id*]
             [pid (proc:pid proc)]
             [j (make-job id cmd-str proc pid)])
        (set! *next-id* (+ *next-id* 1))
        (set! *jobs* (cons j *jobs*))
        ;; Try to set child's process group (may fail if child already exec'd)
        (guard (e [#t (void)])
          (posix-setpgid pid pid))
        (display (format #f "[~a] ~a\n" id pid) (console-error-port))))
    proc)

  ;; ======================================================================
  ;; fg / bg operations
  ;; ======================================================================

  ;; Parse job spec: "%1" -> 1, "%" or "" -> most recent job id, "%cmd" -> search
  (define (parse-job-spec str)
    (cond
      [(or (string=? str "") (string=? str "%") (string=? str "%%"))
       (let ([j (most-recent-job)])
         (and j (job-id j)))]
      [(and (> (string-length str) 0) (char=? (string-ref str 0) #\%))
       (let ([rest (substring str 1 (string-length str))])
         (or (string->number rest)
             ;; Search by command prefix
             (let loop ([jobs *jobs*])
               (cond
                 [(null? jobs) #f]
                 [(let ([cmd (job-command (car jobs))])
                    (and (>= (string-length cmd) (string-length rest))
                         (string=? rest (substring cmd 0 (string-length rest)))))
                  (job-id (car jobs))]
                 [else (loop (cdr jobs))]))))]
      [(string->number str) => (lambda (n) n)]
      [else #f]))

  ;; Bring job to foreground: send SIGCONT, wait for completion or stop.
  ;; Returns wait status.
  (define (job-fg! spec-str)
    (update-jobs!)
    (let ([id (parse-job-spec spec-str)])
      (if (not id)
          (begin
            (display "fg: no such job\n" (console-error-port))
            1)
          (let ([j (find-job id)])
            (if (not j)
                (begin
                  (display (format #f "fg: ~a: no such job\n" id) (console-error-port))
                  1)
                (begin
                  ;; Display command being foregrounded
                  (display (job-command j) (console-error-port))
                  (newline (console-error-port))
                  ;; Give terminal to job's process group
                  (guard (e [#t (void)])
                    (posix-tcsetpgrp 0 (job-pgid j)))
                  ;; Send SIGCONT if stopped
                  (when (eq? (job-status j) 'stopped)
                    (job-status-set! j 'running)
                    (guard (e [#t (void)])
                      (signal-process-group (job-pgid j) SIGCONT)))
                  ;; Wait with WUNTRACED
                  (let ([status (wait (job-proc j) (fxior wait/poll wait/stopped-children))])
                    ;; If not finished yet, do a blocking wait
                    (let ([status (if status status
                                      (wait (job-proc j) wait/stopped-children))])
                      ;; Reclaim terminal
                      (guard (e [#t (void)])
                        (posix-tcsetpgrp 0 (posix-getpgrp)))
                      ;; Update job status
                      (cond
                        [(and status (status:stop-sig status))
                         (job-status-set! j 'stopped)
                         (display (format #f "\n[~a]  Stopped  ~a\n"
                                          (job-id j) (job-command j))
                                  (console-error-port))
                         ;; Return 148 (128 + SIGTSTP=20) like bash
                         148]
                        [else
                         (job-status-set! j 'done)
                         (prune-done-jobs!)
                         (or (and status (status:exit-val status)) 0)])))))))))

  ;; Resume job in background: send SIGCONT.
  (define (job-bg-resume! spec-str)
    (update-jobs!)
    (let ([id (parse-job-spec spec-str)])
      (if (not id)
          (begin
            (display "bg: no such job\n" (console-error-port))
            1)
          (let ([j (find-job id)])
            (if (not j)
                (begin
                  (display (format #f "bg: ~a: no such job\n" id) (console-error-port))
                  1)
                (begin
                  (job-status-set! j 'running)
                  (guard (e [#t (void)])
                    (signal-process-group (job-pgid j) SIGCONT))
                  (display (format #f "[~a]  ~a &\n" (job-id j) (job-command j))
                           (console-error-port))
                  0))))))

  ;; List all jobs to stderr.
  (define (list-jobs)
    (update-jobs!)
    (for-each
      (lambda (j)
        (let ([status-str (case (job-status j)
                            [(running) "Running"]
                            [(stopped) "Stopped"]
                            [(done) "Done"]
                            [else "?"])])
          (display (format #f "[~a]  ~a  ~a\n"
                           (job-id j) status-str (job-command j))
                   (console-error-port))))
      (sort (lambda (a b) (< (job-id a) (job-id b))) *jobs*)))

  ;; ======================================================================
  ;; Signal setup
  ;; ======================================================================

  ;; Install signal handlers for job control.
  ;; Shell ignores SIGTSTP, SIGTTIN, SIGTTOU.
  ;; SIGCHLD triggers zombie reaping.
  (define (install-job-signals!)
    ;; Ignore job-control signals in the shell itself
    (register-signal-handler SIGTSTP (lambda (sig) (void)))
    (register-signal-handler SIGTTIN (lambda (sig) (void)))
    (register-signal-handler SIGTTOU (lambda (sig) (void))))

) ; end library
