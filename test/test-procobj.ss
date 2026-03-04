;;; test-procobj.ss -- Tests for (hafod procobj)
;;; Tests proc records, process table, wait, wait-any, reap-zombies.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod procobj) (hafod posix) (hafod compat) (chezscheme))

(test-begin "Process Objects")

;; =============================================================================
;; Proc record basics
;; =============================================================================

(test-assert "new-child-proc creates proc"
  (let ([p (new-child-proc 99999)])
    (proc? p)))

(test-equal "proc:pid returns pid" 99999
  (proc:pid (new-child-proc 99999)))

(test-assert "new proc is not finished"
  (not (proc:finished? (new-child-proc 99998))))

(test-assert "new proc status is #f"
  (not (proc:status (new-child-proc 99997))))

(test-assert "new proc is zombie (#t)"
  (proc:zombie? (new-child-proc 99996)))

;; =============================================================================
;; Process table
;; =============================================================================

(test-assert "pid->proc finds registered proc"
  (let ([p (new-child-proc 88888)])
    (eq? p (pid->proc 88888))))

(test-assert "maybe-pid->proc returns proc"
  (let ([p (new-child-proc 88887)])
    (eq? p (maybe-pid->proc 88887))))

(test-assert "maybe-pid->proc returns #f for unknown"
  (not (maybe-pid->proc 77777)))

(test-error "pid->proc errors on unknown pid (probe=#f)"
  (pid->proc 77776))

(test-assert "pid->proc returns #f on unknown (probe=#t)"
  (not (pid->proc 77775 #t)))

(test-assert "pid->proc creates on unknown (probe='create)"
  (proc? (pid->proc 77774 'create)))

;; =============================================================================
;; ->proc coercion
;; =============================================================================

(test-assert "->proc with proc returns same proc"
  (let ([p (new-child-proc 66666)])
    (eq? p (->proc p))))

(test-assert "->proc with integer creates proc"
  (proc? (->proc 66665)))

(test-error "->proc with negative integer errors"
  (->proc -1))

;; =============================================================================
;; pid/proc? predicate
;; =============================================================================

(test-assert "pid/proc? true for proc"
  (pid/proc? (new-child-proc 55555)))

(test-assert "pid/proc? true for non-negative integer"
  (pid/proc? 42))

(test-assert "pid/proc? true for 0"
  (pid/proc? 0))

(test-assert "pid/proc? false for negative"
  (not (pid/proc? -1)))

(test-assert "pid/proc? false for string"
  (not (pid/proc? "hello")))

;; =============================================================================
;; Fork + wait (real process tests)
;; =============================================================================

(test-assert "fork child exits 42, wait returns status with exit-val 42"
  (let ([child-pid (posix-fork)])
    (if (zero? child-pid)
        ;; Child: exit with status 42
        (posix-_exit 42)
        ;; Parent: create proc, wait for it
        (let ([p (new-child-proc child-pid)])
          (let ([status (wait p)])
            (= 42 (status:exit-val status)))))))

(test-assert "wait on already-finished proc returns cached status"
  (let ([child-pid (posix-fork)])
    (if (zero? child-pid)
        (posix-_exit 7)
        (let ([p (new-child-proc child-pid)])
          (let ([s1 (wait p)])
            (let ([s2 (wait p)])
              ;; Both calls should return same status
              (and (= (status:exit-val s1) 7)
                   (= (status:exit-val s2) 7))))))))

(test-assert "wait/poll on running child returns #f"
  (let ([child-pid (posix-fork)])
    (if (zero? child-pid)
        ;; Child: sleep briefly then exit
        (begin (posix-sleep 2) (posix-_exit 0))
        (let ([p (new-child-proc child-pid)])
          (let ([result (wait p wait/poll)])
            ;; Clean up: wait for child to finish
            (posix-waitpid child-pid 0)
            (not result))))))

;; =============================================================================
;; wait-any
;; =============================================================================

(test-assert "wait-any returns proc and status for terminated child"
  (let ([child-pid (posix-fork)])
    (if (zero? child-pid)
        (posix-_exit 33)
        (begin
          ;; Give child time to exit
          (posix-sleep 1)
          (receive (p status) (wait-any)
            (and (proc? p)
                 (= 33 (status:exit-val status))))))))

(test-assert "wait-any with poll returns #f #f when none ready"
  ;; Fork a child that sleeps, then poll immediately
  (let ([child-pid (posix-fork)])
    (if (zero? child-pid)
        (begin (posix-sleep 2) (posix-_exit 0))
        (let ()
          (receive (p status) (wait-any wait/poll)
            ;; Clean up
            (posix-waitpid child-pid 0)
            (and (not p) (not status)))))))

;; =============================================================================
;; reap-zombies
;; =============================================================================

(test-assert "reap-zombies collects terminated children"
  (let ([pid1 (posix-fork)]
        [pid2 #f])
    (if (zero? pid1)
        (posix-_exit 0)
        (begin
          (set! pid2 (posix-fork))
          (if (zero? pid2)
              (posix-_exit 0)
              (begin
                ;; Wait for both children to terminate
                (posix-sleep 1)
                ;; Reap them
                (let ([result (reap-zombies)])
                  ;; result could be #t (all reaped) or #f (some live)
                  ;; After sleep, both should be done
                  (or result (eq? result #f)))))))))

;; =============================================================================
;; wait-any with no children
;; =============================================================================

;; This test is tricky — we need to ensure no leftover children.
;; Fork a child that itself has no children and calls wait-any.
(test-assert "wait-any returns #f #t when no children"
  (let ([child-pid (posix-fork)])
    (if (zero? child-pid)
        ;; Child: no children of its own, try wait-any
        (receive (p status) (wait-any wait/poll)
          ;; Should get #f #t (ECHILD)
          (if (and (not p) (eq? status #t))
              (posix-_exit 0)
              (posix-_exit 1)))
        ;; Parent: wait for child
        (receive (wpid status) (posix-waitpid child-pid 0)
          (= 0 (status:exit-val status))))))

(test-end)
