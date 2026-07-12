;;; test-jobs.ss -- Tests for (hafod shell jobs)
;;; Stopped-job detection, fg resume without hanging, and the no-false-restop
;;; regression. Real fork + self-signal (SIGSTOP/SIGCONT), PTY-free. Every case
;;; is bounded by a watchdog child that SIGKILLs the suite after N seconds, so a
;;; resume regression fails deterministically instead of hanging the run.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod shell jobs) (hafod procobj) (hafod posix)
        (hafod signal) (chezscheme))

(test-begin "jobs")

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

;; Fork a sibling that SIGKILLs this process after SECONDS, run THUNK, then
;; cancel the sibling on the happy path and return the thunk's value. A blocking
;; wait that never returns (an un-continued stopped child) dies here rather than
;; hanging the suite out to the 60 s per-suite backstop.
;;
;; victim is captured in an OUTER let, fully bound before the inner (posix-fork):
;; Chez evaluates let inits right-to-left, so a single flat let would evaluate the
;; fork before (posix-getpid) and the watchdog child would capture its own pid and
;; SIGKILL itself, never the test process. The nested lets fix the order.
(define (with-watchdog seconds thunk)
  (let ([victim (posix-getpid)])
    (let ([wpid (posix-fork)])
      (if (zero? wpid)
          (begin (posix-sleep seconds) (posix-kill victim SIGKILL) (posix-_exit 0))
          (let ([result (thunk)])
            (posix-kill wpid SIGKILL)
            (posix-waitpid wpid 0)
            result)))))

;; Call (step) then test (ready?) repeatedly until ready? holds or MS milliseconds
;; of wall-clock elapse; return #t once observed ready, #f on timeout. Bounding by
;; real time rather than a fixed iteration count is what makes the stopped-child
;; cases deterministic: a freshly forked child needs a scheduler slice to run its
;; self-SIGSTOP, but a tight 200-iteration spin of a bare waitpid can burn every
;; iteration in microseconds — before the stop is even deliverable — and give up
;; before it is ever observable. real-time (Chez, milliseconds) bounds the wait by
;; the wall clock; each step's waitpid syscall is itself a scheduler yield point.
(define (poll-until ms step ready?)
  (let ([deadline (+ (real-time) ms)])
    (let loop ()
      (cond [(ready?) #t]
            [(>= (real-time) deadline) #f]
            [else (step) (loop)]))))

;; Is NEEDLE a substring of HAYSTACK? (mirrors the test-signal.ss helper.)
(define (substring? needle haystack)
  (let ([nl (string-length needle)] [hl (string-length haystack)])
    (let loop ([i 0])
      (cond [(> (+ i nl) hl) #f]
            [(string=? needle (substring haystack i (+ i nl))) #t]
            [else (loop (+ i 1))]))))

;; Drain the pending prompt notifications (a list of strings) and report whether
;; any contains SUBSTR. Draining resets the pending set.
(define (notes-contain? substr)
  (let loop ([ns (drain-notifications!)])
    (cond [(null? ns) #f]
          [(substring? substr (car ns)) #t]
          [else (loop (cdr ns))])))

;; =============================================================================
;; Stopped-job detection + fg resume without hanging
;; =============================================================================

;; A background child self-stops. The parent bounded-polls update-jobs! until it
;; reports the stop, then foregrounds it. fg must SIGCONT before the blocking
;; wait so the child runs to completion and fg RETURNS an integer status. Before
;; the fix the stop is never detected, fg reaches the blocking wait on a still
;; stopped child, and the case hangs until the watchdog SIGKILLs the suite.
(test-assert "a self-stopped background job is detected and fg resumes it without hanging"
  (with-watchdog 10
    (lambda ()
      (let ([child (posix-fork)])
        (if (zero? child)
            (begin
              (posix-kill (posix-getpid) SIGSTOP)   ; self-stop; resume on SIGCONT
              (posix-_exit 0))
            (begin
              (drain-notifications!)
              (job-bg! "self-stopper" (new-child-proc child))
              (poll-until 5000 update-jobs! (lambda () (notes-contain? "Stopped")))
              ;; fg must return an integer status; a hang trips the watchdog.
              (integer? (job-fg! ""))))))))

;; =============================================================================
;; No false re-stop after a bg resume (Pitfall 2)
;; =============================================================================

;; Continuing a stopped job clears its one-shot stop flag. The child stays alive
;; after SIGCONT so the flag, if left stale, would be re-read by the next
;; update-jobs! and the running job falsely re-reported "Stopped".
(test-assert "a bg-resumed job is not falsely re-reported as stopped"
  (with-watchdog 10
    (lambda ()
      (let ([child (posix-fork)])
        (if (zero? child)
            (begin
              (posix-kill (posix-getpid) SIGSTOP)   ; self-stop
              (posix-sleep 30)                       ; stay alive after SIGCONT
              (posix-_exit 0))
            (begin
              (drain-notifications!)
              (job-bg! "sleeper" (new-child-proc child))
              (poll-until 5000 update-jobs! (lambda () (notes-contain? "Stopped")))
              (job-bg-resume! "")                    ; SIGCONT + clear the flag
              (drain-notifications!)                 ; discard the first stop report
              (update-jobs!)
              (update-jobs!)
              (let ([saw-stop (notes-contain? "Stopped")])
                (posix-kill child SIGKILL)           ; cleanup — child was sleeping
                (posix-waitpid child 0)
                (not saw-stop))))))))

;; =============================================================================
;; Proc-level reinforcement: the reaped stop is recorded, not discarded
;; =============================================================================

;; reap-zombies must record the WUNTRACED stop on the proc — stopped? true,
;; finished? still false (a stopped child is alive), status carrying the stop
;; signal — rather than throwing the one-shot notification away.
(test-assert "reap-zombies records a stop on the proc without marking it finished"
  (with-watchdog 10
    (lambda ()
      (let ([child (posix-fork)])
        (if (zero? child)
            (begin
              (posix-kill (posix-getpid) SIGSTOP)
              (posix-_exit 0))
            (let ([p (new-child-proc child)])
              (poll-until 5000 reap-zombies (lambda () (proc:stopped? p)))
              (let ([ok (and (proc:stopped? p)
                             (not (proc:finished? p))
                             (= SIGSTOP (status:stop-sig (proc:status p))))])
                (posix-kill child SIGKILL)
                (posix-waitpid child 0)
                ok)))))))

;; =============================================================================
;; A job re-stopped while foregrounded stays alive, not finished
;; =============================================================================

;; fg resumes a stopped job with SIGCONT and then blocks in wait. If the job
;; stops a SECOND time (Ctrl-Z during fg), that WUNTRACED stop must be recorded
;; as a stop — the proc stays alive: stopped? true, finished? still false — so a
;; later fg can re-foreground it. Recording it as a termination instead caches the
;; stop status behind finished?=#t, and the next fg then takes wait's finished
;; fast-path and returns at once without ever foregrounding the resumed child.
;; The child self-stops, is foregrounded, then self-stops again; we assert fg
;; still returns a status and leaves the proc recorded stopped-but-alive.
(test-assert "a job re-stopped while foregrounded is recorded stopped-alive, not finished"
  (with-watchdog 10
    (lambda ()
      (let ([child (posix-fork)])
        (if (zero? child)
            (begin
              (posix-kill (posix-getpid) SIGSTOP)   ; first stop
              (posix-kill (posix-getpid) SIGSTOP)   ; stop AGAIN after fg's SIGCONT
              (posix-_exit 0))                       ; exit only after a second resume
            (let ([p (new-child-proc child)])
              (drain-notifications!)
              (job-bg! "re-stopper" p)
              ;; Bounded-poll until the first stop is detected.
              (let poll ([n 0] [seen #f])
                (cond [seen #t]
                      [(> n 200) #f]
                      [else (update-jobs!)
                            (poll (+ n 1) (notes-contain? "Stopped"))]))
              ;; fg resumes it; the child self-stops again and fg's blocking wait
              ;; returns that second stop. fg reports Stopped and returns a status.
              (let ([rc (job-fg! "")])
                (let ([ok (and (integer? rc)
                               (proc:stopped? p)          ; recorded stopped-alive
                               (not (proc:finished? p)))]) ; not finished-while-alive
                  ;; Cleanup: the child is stopped-but-alive.
                  (posix-kill child SIGKILL)
                  (posix-waitpid child 0)
                  ok))))))))

(test-end)
