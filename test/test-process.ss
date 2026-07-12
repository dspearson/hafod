;;; test-process.ss -- Tests for (hafod process)
;;; Tests fork, exec, exit, sleep, pipelines.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod process) (hafod procobj) (hafod posix) (hafod fd-ports)
        (hafod compat) (chezscheme))

;; True if `needle` occurs anywhere in `haystack`.
(define (substring? needle haystack)
  (let ([nl (string-length needle)] [hl (string-length haystack)])
    (let loop ([i 0])
      (cond [(> (+ i nl) hl) #f]
            [(string=? needle (substring haystack i (+ i nl))) #t]
            [else (loop (+ i 1))]))))

;; Fail-fast watchdog: fork a sibling that SIGKILLs this process after `seconds`,
;; so a regression that hangs a blocking wait dies deterministically instead of
;; stalling the suite. Cancels the watchdog on the happy path and returns the
;; thunk's value.
;;
;; victim is captured in an OUTER let, fully bound before the inner (posix-fork):
;; Chez evaluates let inits right-to-left, so a single flat let would run the fork
;; before (posix-getpid) and the watchdog child would capture its own pid and
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

(test-begin "Process Operations")

;; =============================================================================
;; split-colon-list
;; =============================================================================

(test-equal "split-colon-list: a:b:c" '("a" "b" "c")
  (split-colon-list "a:b:c"))

(test-equal "split-colon-list: empty" '()
  (split-colon-list ""))

(test-equal "split-colon-list: single" '("single")
  (split-colon-list "single"))

(test-equal "split-colon-list: trailing colon" '("a" "b" "")
  (split-colon-list "a:b:"))

(test-equal "split-colon-list: leading colon" '("" "b")
  (split-colon-list ":b"))

;; =============================================================================
;; exec-path-list
;; =============================================================================

(test-assert "exec-path-list is a non-empty list"
  (and (list? (exec-path-list))
       (not (null? (exec-path-list)))))

;; =============================================================================
;; exec-path-search
;; =============================================================================

(test-assert "exec-path-search finds 'true'"
  (let ([result (exec-path-search "true" (exec-path-list))])
    (and (string? result)
         (> (string-length result) 0))))

(test-assert "exec-path-search returns #f for nonexistent"
  (not (exec-path-search "nonexistent-program-xyz-12345" (exec-path-list))))

(test-assert "exec-path-search with absolute path checks executability"
  (let ([result (exec-path-search "/bin/sh" '())])
    (string? result)))

;; =============================================================================
;; fork basics
;; =============================================================================

(test-assert "fork with thunk: parent gets proc"
  (let ([p (fork (lambda () (posix-_exit 0)))])
    (and (proc? p)
         (begin (wait p) #t))))

(test-assert "fork without thunk: parent gets proc, child gets #f"
  (let ([result (fork)])
    (if (proc? result)
        ;; Parent
        (begin (wait result) #t)
        ;; Child (result is #f)
        (posix-_exit 0))))

;; =============================================================================
;; fork + exec
;; =============================================================================

(test-assert "fork + exec-path 'true' exits 0"
  (let ([p (fork (lambda () (exec-path "true")))])
    (= 0 (status:exit-val (wait p)))))

(test-assert "fork + exec-path 'false' exits 1"
  (let ([p (fork (lambda () (exec-path "false")))])
    (= 1 (status:exit-val (wait p)))))

;; =============================================================================
;; call-terminally / %exit
;; =============================================================================

(test-assert "call-terminally runs thunk and exits"
  (let ([p (fork (lambda () (call-terminally (lambda () #f))))])
    (= 0 (status:exit-val (wait p)))))

(test-assert "%exit terminates with given status"
  (let ([child-pid (posix-fork)])
    (if (zero? child-pid)
        (%exit 42)
        (receive (wpid status) (posix-waitpid child-pid 0)
          (= 42 (status:exit-val status))))))

(test-assert "exit flushes and terminates"
  (let ([child-pid (posix-fork)])
    (if (zero? child-pid)
        (exit 7)
        (receive (wpid status) (posix-waitpid child-pid 0)
          (= 7 (status:exit-val status))))))

;; =============================================================================
;; process-sleep
;; =============================================================================

(test-assert "process-sleep sleeps approximately 1 second"
  (let ([start (time-second (current-time))])
    (process-sleep 1)
    (let ([elapsed (- (time-second (current-time)) start)])
      (>= elapsed 1))))

;; =============================================================================
;; preserve-ports
;; =============================================================================

(test-assert "preserve-ports captures and restores ports"
  (let* ([cin (current-input-port)]
         [cout (current-output-port)]
         [cerr (current-error-port)]
         [thunk (preserve-ports (lambda ()
                  ;; Inside preserved, ports should match captured
                  (and (eq? (current-input-port) cin)
                       (eq? (current-output-port) cout)
                       (eq? (current-error-port) cerr))))])
    (thunk)))

;; =============================================================================
;; fork/pipe
;; =============================================================================

(test-assert "fork/pipe: parent reads child output"
  ;; Save stdin, run fork/pipe, read, restore
  (let ([saved-stdin (dup->inport 0)])
    (let ([child (fork/pipe (lambda ()
                              (display "hello-pipe")
                              (newline)))])
      (let ([line (get-line (current-input-port))])
        (wait child)
        (move->fdes saved-stdin 0)
        (string=? "hello-pipe" line)))))

(test-assert "fork/pipe with exec: echo through pipe"
  (let ([saved-stdin (dup->inport 0)])
    (let ([child (fork/pipe (lambda () (exec-path "echo" "pipe-test")))])
      (let ([line (get-line (current-input-port))])
        (wait child)
        (move->fdes saved-stdin 0)
        (string=? "pipe-test" line)))))

;; =============================================================================
;; fork/pipe+
;; =============================================================================

(test-assert "fork/pipe+ with ((1 0)) equivalent to fork/pipe"
  (let ([saved-stdin (dup->inport 0)])
    (let ([child (fork/pipe+ '((1 0)) (lambda ()
                                         (display "pipe-plus")
                                         (newline)))])
      (let ([line (get-line (current-input-port))])
        (wait child)
        (move->fdes saved-stdin 0)
        (string=? "pipe-plus" line)))))

;; =============================================================================
;; Pipeline tests (run in forked children since pipe*/tail-pipe never return)
;; =============================================================================

(test-assert "pipe* two-stage pipeline"
  ;; Fork a child that sets up a pipeline and captures output
  (let ([saved-stdin (dup->inport 0)])
    (let ([child (fork/pipe
                   (lambda ()
                     ;; This child runs a pipeline: echo | cat
                     ;; pipe* never returns (last thunk is call-terminally)
                     (pipe*
                       (lambda () (exec-path "echo" "pipe-star-test"))
                       (lambda () (exec-path "cat")))))])
      (let ([line (get-line (current-input-port))])
        (wait child)
        (move->fdes saved-stdin 0)
        (string=? "pipe-star-test" line)))))

(test-assert "tail-pipe: fork a, run b in current process (via child)"
  ;; We need to test tail-pipe inside a forked child since it never returns
  (let ([saved-stdin (dup->inport 0)])
    (let ([child (fork/pipe
                   (lambda ()
                     (tail-pipe
                       (lambda () (exec-path "echo" "tail-pipe-test"))
                       (lambda () (exec-path "cat")))))])
      (let ([line (get-line (current-input-port))])
        (wait child)
        (move->fdes saved-stdin 0)
        (string=? "tail-pipe-test" line)))))

;; =============================================================================
;; forked-child exception containment
;; =============================================================================

;; An uncaught exception raised inside a forked-child thunk must terminate the
;; child cleanly with a non-zero exit (status 1), reporting a single line to the
;; child's own stderr — it must never unwind back into the parent's continuation.
;; We point the child's error port at a pipe (so its report is captured rather
;; than reaching the console) and confirm both the report and the clean exit
;; (exit-val 1, no terminating signal). Watchdog-guarded so a regression that
;; unwinds and hangs fails fast.
(test-assert "a raising forked-child thunk exits with status 1 and reports on its own stderr"
  (with-watchdog 10
    (lambda ()
      (receive (r w) (pipe)
        (let ([p (with-current-error-port* w
                   (lambda () (fork (lambda () (error 'boom "child explode")))))])
          (close w)
          (let ([line (get-line r)])
            (let ([st (wait p)])
              (close r)
              (and (string? line)
                   (substring? "uncaught exception in child process" line)
                   (= 1 (status:exit-val st))
                   (not (status:term-sig st))))))))))

;; =============================================================================
;; partial pipeline-spawn failure reaps already-launched stages
;; =============================================================================

;; When a pipeline fails to launch its Nth stage, the stages already launched
;; (1..N-1) must be signalled and reaped, not left behind as zombie/orphan
;; children. We build a 3-stage pipeline whose first two stages are a real,
;; fast-exiting program at an absolute path and whose final stage names a
;; non-existent absolute path, so posix-spawnp raises for the final stage after
;; the first two have launched.
;;
;; The reap teardown runs under a watchdog so a regressed (e.g. non-lethal
;; signal) blocking wait fails the suite deterministically rather than stalling
;; it. The pre-existing-child drain and the leftover-child probe use
;; (posix-waitpid -1 0), which would block on the watchdog's own live child, so
;; they sit OUTSIDE the watchdog window — with-watchdog reaps its child before it
;; returns, so the probe then observes only this pipeline's stages.
(test-assert "a partial pipeline-spawn failure leaves no reapable child"
  (let ([spawn-failed #f])
    ;; Clear any child an earlier case left reapable so the probe sees only ours.
    (let drain ()
      (when (guard (_ [#t #f]) (posix-waitpid -1 0) #t)
        (drain)))
    ;; Launch the pipeline; the final stage cannot be spawned, so posix-spawnp
    ;; raises after stages 1-2 have launched. Swallow the expected error and
    ;; record that it fired, so the test proves it exercised the failure path.
    (with-watchdog 8
      (lambda ()
        (guard (_ [#t (set! spawn-failed #t)])
          (spawn-pipeline (list (list "/bin/echo")
                                (list "/bin/echo")
                                (list "/nonexistent/hafod-spawn-fail-xyz"))))))
    (and
      ;; The failure path actually fired (not a silently-successful pipeline).
      spawn-failed
      ;; No launched stage survives as a reapable child: post-fix the teardown
      ;; reaped them, so waitpid raises ECHILD (no children -> #t); pre-fix a
      ;; leaked stage is reapable, so the blocking wait returns a pid (-> #f).
      (guard (_ [#t #t])
        (posix-waitpid -1 0)
        #f))))

(test-end)
