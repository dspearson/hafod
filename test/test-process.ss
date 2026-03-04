;;; test-process.ss -- Tests for (hafod process)
;;; Tests fork, exec, exit, sleep, pipelines.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod process) (hafod procobj) (hafod posix) (hafod fd-ports)
        (hafod compat) (chezscheme))

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

(test-end)
