;;; 12-process-demo.ss -- Fork, wait, signals, background processes
;;;
;;; Demonstrates: fork, wait, proc?, proc:pid, status:exit-val,
;;;               &, run, signal-process, process-sleep,
;;;               pid, parent-pid, with-cwd, exit
;;;
;;; Source: scsh manual process section; "A Scheme Shell" paper.

(import (hafod))

(define (section title)
  (newline)
  (display (string-append "=== " title " ===\n")))

;;; --- Basic fork and wait ---
(section "Fork and Wait")

(let ([child (fork (lambda ()
                     (display (string-append "  Child PID: "
                                             (number->string (pid)) "\n"))
                     (display (string-append "  Child's parent: "
                                             (number->string (parent-pid)) "\n"))
                     (exit 42)))])
  (display (string-append "Parent PID: " (number->string (pid)) "\n"))
  (display (string-append "Child PID: " (number->string (proc:pid child)) "\n"))
  (let ([status (wait child)])
    (display (string-append "Child exit status: "
                            (number->string (status:exit-val status)) "\n"))))

;;; --- Background processes ---
(section "Background Processes (&)")

;; Launch a background process
(let ([bg-proc (& (sleep 0))])
  (display (string-append "Background process PID: "
                          (number->string (proc:pid bg-proc)) "\n"))
  (display "Waiting for background process...\n")
  (let ([status (wait bg-proc)])
    (display (string-append "Background exit status: "
                            (number->string (status:exit-val status)) "\n"))))

;;; --- Multiple children ---
(section "Multiple Children")

(let ([procs (map (lambda (n)
                    (fork (lambda ()
                            (display (string-append "  Worker " (number->string n)
                                                    " (PID " (number->string (pid))
                                                    ")\n"))
                            (exit n))))
                  '(1 2 3))])
  (display (string-append "Launched " (number->string (length procs)) " workers\n"))
  ;; Wait for all children
  (for-each
    (lambda (p)
      (let ([status (wait p)])
        (display (string-append "  PID " (number->string (proc:pid p))
                                " exited with " (number->string (status:exit-val status))
                                "\n"))))
    procs))

;;; --- fork/pipe (pipe between parent and child) ---
(section "Fork/Pipe")

;; fork/pipe: child's stdout is connected to parent's stdin
(let ([child (fork/pipe (lambda ()
                          (display "line-one\n")
                          (display "line-two\n")
                          (display "line-three\n")))])
  ;; Parent reads from child's stdout
  (let ([lines (port->string-list (current-input-port))])
    (display (string-append "Read " (number->string (length lines))
                            " lines from child:\n"))
    (for-each (lambda (l) (display (string-append "  " l "\n"))) lines))
  (wait child))

;;; --- Process status inspection ---
(section "Process Status")

;; Run a successful command
(let ([status (run (true))])
  (display (string-append "true exit: " (number->string status) "\n")))

;; Run a failing command
(let ([status (run (false))])
  (display (string-append "false exit: " (number->string status) "\n")))

;; Signal detection — signal-process accepts proc objects directly
(let ([child (fork (lambda ()
                     (process-sleep 60)))])
  (signal-process child SIGTERM)
  (let ([status (wait child)])
    (let ([sig (status:term-sig status)])
      (display (string-append "Killed child terminated by signal: "
                              (if sig (number->string sig) "none")
                              "\n")))))

;;; --- with-cwd in subprocesses ---
(section "CWD in Subprocesses")

(let ([result (run/string
                (begin
                  (with-cwd "/tmp"
                    (display (cwd))
                    (newline))))])
  (display (string-append "Child CWD was: " result)))

(display (string-append "Parent CWD is: " (cwd) "\n"))

;;; --- Conditional execution ---
(section "Conditional Execution")

;; && : run until failure
(display "&& (all succeed): ")
(display (if (&& (true) (true) (true)) "passed" "failed"))
(newline)

(display "&& (middle fails): ")
(display (if (&& (true) (false) (true)) "passed" "failed"))
(newline)

;; || : run until success
(display "|| (all fail): ")
(display (if (:or: (false) (false) (false)) "one passed" "all failed"))
(newline)

(display "|| (last succeeds): ")
(display (if (:or: (false) (false) (true)) "one passed" "all failed"))
(newline)

(display "\nDone.\n")
