;;; test-poll-vacuity.ss -- Proof that a count-bounded wait is not a timeout.
;;; A wait on an asynchronous event that is bounded by a COUNT of attempts has the
;;; shape of a timeout and none of the substance. This suite demonstrates that,
;;; rather than arguing it: a forked child reaches its observable state only after
;;; a real wall-clock delay; the counted bound fails to observe it and gives up in
;;; microseconds, while poll-until, handed the identical step and ready? thunks,
;;; observes it. Both halves are the proof — a wait that never succeeds and a wait
;;; that always succeeds are each useless, and only the pair rules both out.
;;; Real fork + a marker file, PTY-free.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (test poll) (hafod posix) (hafod temp-file) (chezscheme))

(test-begin "poll-vacuity")

;; -----------------------------------------------------------------------------
;; The two bounds, side by side
;; -----------------------------------------------------------------------------

;; The anti-pattern this work eliminates, defined here and nowhere else: its only
;; purpose is to be the subject of the comparison below. Identical to poll-until
;; in every respect except the bound — where poll-until stops at a deadline on the
;; wall clock, this stops after a fixed number of attempts. The substitution looks
;; innocuous and is not: the attempts are spent as fast as the machine can spend
;; them, so the bound expires in microseconds regardless of the wall-clock time the
;; awaited event actually needs.
(define (count-bounded n step ready?)
  (let loop ([i 0])
    (cond [(ready?) #t]
          [(>= i n) #f]
          [else (step) (loop (+ i 1))])))

;; =============================================================================
;; The proof
;; =============================================================================

;; An observable that cannot be true at t=0 and becomes true only after a real
;; wall-clock delay: a marker file, created by a forked child one second after the
;; fork. create-temp-file yields an exclusively-created, unpredictable path that
;; honours $TMPDIR — never a guessable path a local attacker could pre-create or
;; point at a symlink — and we unlink it at once, so the path names a file that
;; does not yet exist.
(let ([marker (create-temp-file)])
  (posix-unlink marker)

  ;; The child inherits this process's buffered output. Flush before forking, and
  ;; let the child leave via posix-_exit, which does not flush; plain exit would
  ;; write the inherited buffer a second time and double the run's reported counts.
  (flush-output-port (current-output-port))

  (let ([child (posix-fork)])
    (if (zero? child)
        (begin
          (posix-sleep 1)                                        ; the wall-clock delay
          (guard (e [#t #f]) (close-port (open-output-file marker)))
          (posix-_exit 0))

        ;; Both bounds are handed the SAME step and the SAME ready?. Only the bound
        ;; differs — given different thunks the comparison would discriminate nothing.
        (let* ([step   (lambda () (posix-getpid))]   ; a cheap real syscall, standing in
                                                     ; for the bare waitpid the original
                                                     ; count-bounded spin turned on
               [ready? (lambda () (file-exists? marker))]
               [t0      (real-time)]
               [counted (count-bounded 200 step ready?)]
               [counted-ms (- (real-time) t0)])

          ;; The vacuous pass, exhibited. The child is still sleeping; the marker
          ;; cannot exist yet; the counted bound reports that it waited and saw
          ;; nothing — having in truth given the child no time in which to be seen.
          (test-assert "a count-bounded spin cannot observe a child that has not yet run"
            (not counted))

          ;; ...and here is why: it did not wait at all. Its 200 attempts are gone in
          ;; well under the 1000 ms the child needs, so no bound expressed as a count
          ;; of attempts can be read as a bound on time.
          (test-assert "the count-bounded spin exhausts its attempts in microseconds, not in wall-clock time"
            (< counted-ms 200))

          ;; The wall-clock bound, same thunks, same child: it waits, and it sees.
          (test-assert "the wall-clock bound observes the same child through the same thunks"
            (poll-until 5000 step ready?))

          ;; Positive control. Without it, both assertions above would be satisfied by
          ;; a count-bounded that simply never returned #t — a vacuous proof of
          ;; vacuity, which is the very failure this suite exists to rule out. Handed
          ;; the identical thunks now that poll-until has waited the marker into
          ;; existence, the counted bound does observe it. It is not blind; it is
          ;; early.
          (test-assert "the count-bounded spin does observe the marker once it exists"
            (count-bounded 200 step ready?))

          (posix-waitpid child 0)
          (guard (e [#t #f]) (posix-unlink marker))))))

(test-end)
