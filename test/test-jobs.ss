;;; test-jobs.ss -- Tests for (hafod shell jobs)
;;; Stopped-job detection, fg resume without hanging, and the no-false-restop
;;; regression. Real fork + self-signal (SIGSTOP/SIGCONT), PTY-free. Every case
;;; is bounded by a watchdog child that SIGKILLs the suite after N seconds, so a
;;; resume regression fails deterministically instead of hanging the run.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (test poll) (hafod shell jobs) (hafod procobj) (hafod posix)
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
              ;; The poll's verdict is part of the assertion, not a preamble to it.
              ;; Discarded, a timed-out poll is indistinguishable from an observed
              ;; stop: control reaches fg either way, and the case is then decided
              ;; by whatever the never-observed child happened to be doing.
              (if (poll-until 5000 update-jobs!
                              (lambda () (notes-contain? "Stopped")))
                  ;; fg must return an integer status; a hang trips the watchdog.
                  (integer? (job-fg! ""))
                  ;; Timed out. fg does not run, so nothing reaps the child: kill it
                  ;; here (SIGKILL is delivered to a stopped process) rather than
                  ;; leave a stopped orphan behind the failing case.
                  (begin
                    (posix-kill child SIGKILL)
                    (posix-waitpid child 0)
                    #f))))))))

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
              ;; The poll's verdict is part of the assertion. A timed-out poll means
              ;; the first stop was never observed, and "not falsely re-reported as
              ;; stopped" is then vacuously true of a job that was never stopped in
              ;; the first place — a pass with its whole subject missing.
              (let ([saw-stop (poll-until 5000 update-jobs!
                                          (lambda () (notes-contain? "Stopped")))])
                (job-bg-resume! "")                  ; SIGCONT + clear the flag
                (drain-notifications!)               ; discard the first stop report
                (update-jobs!)
                (update-jobs!)
                (let ([saw-restop (notes-contain? "Stopped")])
                  (posix-kill child SIGKILL)         ; cleanup — child was sleeping
                  (posix-waitpid child 0)
                  (and saw-stop (not saw-restop))))))))))

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
              ;; The poll's verdict is part of the assertion: it is the difference
              ;; between "reap-zombies recorded the stop" and "reap-zombies was
              ;; never given a stop to record".
              (let ([saw-stop (poll-until 5000 reap-zombies
                                          (lambda () (proc:stopped? p)))])
                (let ([ok (and saw-stop
                               (proc:stopped? p)
                               (not (proc:finished? p))
                               (= SIGSTOP (status:stop-sig (proc:status p))))])
                  (posix-kill child SIGKILL)
                  (posix-waitpid child 0)
                  ok))))))))

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
              ;; Bounded-poll until the first stop is detected — by the wall clock,
              ;; not by an iteration count. A counted bound is not merely untidy
              ;; here, it is silently fatal to this case: 201 spins of update-jobs!
              ;; complete in under a millisecond, long before the freshly forked
              ;; child has had a slice in which to run its self-SIGSTOP, so the poll
              ;; gives up having observed nothing and fg then SIGCONTs a child that
              ;; has not stopped. That SIGCONT is a no-op; the child goes on to its
              ;; FIRST self-stop, fg's blocking wait returns that, and every
              ;; assertion below is satisfied without the re-stop this case exists to
              ;; exercise ever having happened.
              ;;
              ;; Which is why the poll's verdict is threaded into ok rather than
              ;; discarded: it is what makes that failure visible. A timed-out poll
              ;; fails the case here instead of handing it to fg to pass by accident.
              (let ([saw-stop (poll-until 5000 update-jobs!
                                          (lambda () (notes-contain? "Stopped")))])
                ;; fg resumes it; the child self-stops again and fg's blocking wait
                ;; returns that second stop. fg reports Stopped and returns a status.
                (let ([rc (job-fg! "")])
                  (let ([ok (and saw-stop                    ; the FIRST stop was seen
                                 (integer? rc)
                                 (proc:stopped? p)           ; recorded stopped-alive
                                 (not (proc:finished? p)))]) ; not finished-while-alive
                    ;; Cleanup: the child is stopped-but-alive.
                    (posix-kill child SIGKILL)
                    (posix-waitpid child 0)
                    ok)))))))))

(test-end)
