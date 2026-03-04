;;; 03-pipeline-demo.ss -- Process notation, pipelines, and redirections
;;;
;;; Demonstrates: run, run/string, run/strings, run/port, run/sexp,
;;;               pipe (pipeline), redirections (<, >, >>. <<),
;;;               &, ||, &&, run/collecting, run/file, unquote (,var)
;;;
;;; These are the core scsh features -- running Unix commands as
;;; s-expressions. Source: scsh manual, "A Scheme Shell" paper.

(import (hafod))

(define (section title)
  (newline)
  (display (string-append "=== " title " ===\n")))

(define (trim-whitespace s)
  (let loop ([i 0])
    (if (and (< i (string-length s))
             (char-whitespace? (string-ref s i)))
        (loop (+ i 1))
        (let rloop ([j (- (string-length s) 1)])
          (if (and (> j i) (char-whitespace? (string-ref s j)))
              (rloop (- j 1))
              (if (> j i) (substring s i (+ j 1)) ""))))))

;;; --- Basic command execution ---
(section "Basic Commands")

;; run/string captures stdout as a string
(let ([hostname (run/string (uname -n))])
  (display (string-append "Hostname: " (trim-whitespace hostname) "\n")))

;; run/strings captures stdout as a list of lines
(let ([users (run/strings (who))])
  (display (string-append "Logged-in sessions: "
                          (number->string (length users)) "\n")))

;;; --- Pipelines ---
(section "Pipelines")

;; Count files in /tmp using a pipeline
(let ([count (run/string (pipe (ls /tmp) (wc -l)))])
  (display (string-append "Files in /tmp: " (trim-whitespace count) "\n")))

;; Multi-stage pipeline: find unique shells
(let ([shells (run/strings (pipe (cat /etc/passwd)
                                (cut -d: -f7)
                                (sort)
                                (uniq)))])
  (display "Unique shells:\n")
  (for-each (lambda (s) (display (string-append "  " s "\n"))) shells))

;;; --- Here-strings (<<) ---
(section "Here-Strings")

;; Feed data to a command via here-string redirection
(let ([sorted (run/string (sort) (<< "banana\napple\ncherry\n"))])
  (display (string-append "Sorted fruit:\n" sorted)))

;; wc on here-string input
(let ([wc-out (run/string (wc -w) (<< "one two three four five"))])
  (display (string-append "Word count: " (trim-whitespace wc-out) "\n")))

;;; --- Output redirections ---
(section "Output Redirections")

;; Write to a temp file using Scheme I/O, then process with commands
(let ([tmpfile (create-temp-file)])
  (let ([port (open-output-file tmpfile)])
    (display "Hello from hafod!\n" port)
    (display "Second line\n" port)
    (close port))
  ;; Read it back via cat (using unquote for variable reference)
  (let ([contents (run/string (cat ,tmpfile))])
    (display (string-append "File contents:\n" contents)))
  (delete-file tmpfile))

;;; --- Scheme code in subprocesses ---
(section "Scheme in Subprocesses")

;; Run Scheme code in a forked child, capture output
(let ([result (run/string (begin
                            (display "Computed: ")
                            (display (* 6 7))
                            (newline)))])
  (display result))

;; Pipeline with Scheme producer
(let ([result (run/string
                (pipe (begin
                        (for-each (lambda (n)
                                    (display n) (newline))
                                  '(42 17 99 3 55 8)))
                      (sort -n)))])
  (display (string-append "Sorted numbers:\n" result)))

;;; --- Conditional execution ---
(section "Conditional Execution (|| and &&)")

;; && runs commands until one fails
(let ([ok (&& (true) (echo "first succeeded") (echo "second succeeded"))])
  (display (string-append "&& result: " (if ok "all succeeded" "one failed") "\n")))

;; || runs commands until one succeeds
(let ([ok (:or: (false) (false) (echo "third attempt succeeded"))])
  (display (string-append "|| result: " (if ok "one succeeded" "all failed") "\n")))

;;; --- run/collecting (multiple fd capture) ---
(section "run/collecting")

;; Capture both stdout and stderr from a command
(let-values ([(status out err) (run/collecting (1 2)
                                 (begin
                                   (display "stdout data\n")
                                   (display "stderr data\n" (current-error-port))))])
  (display (string-append "Exit status: " (number->string status) "\n"))
  (display (string-append "Stdout: " (port->string out)))
  (display (string-append "Stderr: " (port->string err)))
  (close out)
  (close err))

;;; --- run/file ---
(section "run/file")

(let ([fname (run/file (echo "This was written to a temp file"))])
  (display (string-append "Temp file: " fname "\n"))
  (display (string-append "Contents: " (run/string (cat ,fname))))
  (delete-file fname))

;;; --- run/string* (procedural form) ---
(section "Procedural Forms (run/string*)")

;; Using unquote for dynamic arguments
(let ([file "/etc/passwd"])
  (let ([line-count (length (run/strings (cat ,file)))])
    (display (string-append file " has "
                            (number->string line-count) " lines\n"))))

;; run/sexp reads one s-expression from child's stdout
(let ([val (run/sexp (begin (write '(hello world 42))))])
  (display (string-append "S-expression: " (format "~s" val) "\n")))

(display "\nDone.\n")
