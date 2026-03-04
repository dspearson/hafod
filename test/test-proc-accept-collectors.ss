#!chezscheme
;;; test-proc-accept-collectors.ss -- Acceptance tests: collection forms, here-strings, nested EPFs
;;; Requirements: PROC-04, PROC-05
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (hafod syntax) (hafod collect) (hafod port-collect) (hafod rdelim)
        (hafod process) (hafod procobj) (hafod posix) (hafod fd-ports) (hafod compat)
        (except (chezscheme) exit vector-append open-input-file open-output-file getenv)
        (test runner))

(test-begin "Collectors & Here-strings Acceptance")

;; =============================================================================
;; Section 1: PROC-04 -- Collection forms end-to-end
;; =============================================================================

;; run/string with realistic multi-line command
(test-equal "run/string with multi-line command"
  "line1\nline2\nline3\n"
  (run/string (sh "-c" "echo line1; echo line2; echo line3")))

;; run/string with pipeline
(test-equal "run/string with pipeline (sort)"
  "a\nb\nc\n"
  (run/string (pipe (printf "c\na\nb\n") (sort))))

;; run/strings with realistic command
(test-equal "run/strings multi-line"
  '("alpha" "beta" "gamma")
  (run/strings (sh "-c" "echo alpha; echo beta; echo gamma")))

;; run/port with read loop
(test-equal "run/port with manual read loop"
  '("1" "2" "3")
  (let ([p (run/port (sh "-c" "for i in 1 2 3; do echo $i; done"))])
    (let loop ([lines '()])
      (let ([line (read-line p)])
        (if (eof-object? line)
            (begin (close p) (reverse lines))
            (loop (cons line lines)))))))

;; run/sexp reads structured data
(test-equal "run/sexp reads structured sexp"
  '(define x 42)
  (run/sexp (echo "(define x 42)")))

;; run/sexps reads multiple expressions
(test-equal "run/sexps reads multiple expressions"
  '((+ 1 2) (* 3 4) (list 5 6))
  (run/sexps (echo "(+ 1 2) (* 3 4) (list 5 6)")))

;; run/port+proc returns usable port and proc
(test-equal "run/port+proc: port content and exit status"
  '("hello\nworld\n" . 0)
  (receive (port proc) (run/port+proc (sh "-c" "echo hello; echo world"))
    (let* ([content (port->string port)]
           [_c (close port)]
           [s (wait proc)])
      (cons content (status:exit-val s)))))

;; run/collecting collects stdout and stderr separately
(test-equal "run/collecting stdout+stderr separate"
  '("out-data\n" "err-data\n" 0)
  (receive (status p1 p2)
    (run/collecting (1 2) (sh "-c" "echo out-data; echo err-data >&2"))
    (let* ([out (port->string p1)]
           [err (port->string p2)]
           [_c1 (close p1)]
           [_c2 (close p2)])
      (list out err (status:exit-val status)))))

;; run/collecting with single fd
(test-equal "run/collecting single fd"
  "single-fd\n"
  (receive (status p1) (run/collecting (1) (echo "single-fd"))
    (let ([content (port->string p1)])
      (close p1)
      content)))

;; run/file creates readable temp file
(let ([fname (run/file (sh "-c" "echo file-line1; echo file-line2"))])
  (let* ([p (open-input-file fname)]
         [content (get-string-all p)]
         [_c (close p)])
    (posix-unlink fname)
    (test-equal "run/file creates file with correct content"
      "file-line1\nfile-line2\n" content)))

;; run/string* procedural form with pipeline thunk
(test-equal "run/string* procedural with shell pipeline"
  "hello\n"
  (run/string* (lambda () (exec-path "sh" "-c" "echo hello | cat"))))

;; run/collecting* procedural form collects multi-fd
(test-equal "run/collecting* procedural stdout+stderr"
  '("proc-out\n" "proc-err\n")
  (receive (status p1 p2)
    (run/collecting* '(1 2)
      (lambda () (exec-path "sh" "-c" "echo proc-out; echo proc-err >&2")))
    (let* ([out (port->string p1)]
           [err (port->string p2)]
           [_c1 (close p1)]
           [_c2 (close p2)])
      (list out err))))

;; =============================================================================
;; Section 2: PROC-05 -- Here-string redirection and nested process forms
;; =============================================================================

;; here-string as stdin
(test-equal "here-string as stdin"
  "hello from here-string"
  (run/string (cat) (<< "hello from here-string")))

;; here-string with variable content
(let ([content "dynamic heredoc content"])
  (test-equal "here-string with variable"
    content
    (run/string (cat) (<< ,content))))

;; here-string with multi-line content
(test-equal "here-string with multi-line content"
  "line1\nline2\nline3"
  (run/string (cat) (<< "line1\nline2\nline3")))

;; here-string piped through grep
(test-equal "here-string piped through grep"
  "target-line\n"
  (run/string (pipe (cat) (grep "target")) (<< "noise\ntarget-line\nmore noise")))

;; here-string with number
(test-equal "here-string with number"
  "99999"
  (run/string (cat) (<< 99999)))

;; nested EPF: redirection inside run
(let ([tmpfile "/tmp/hafod-accept-proc05-nested"])
  (run (echo "nested-out") (> ,tmpfile))
  (let* ([p (open-input-file tmpfile)]
         [content (get-string-all p)]
         [_c (close p)])
    (posix-unlink tmpfile)
    (test-equal "nested EPF with output redirection" "nested-out\n" content)))

;; nested begin with computation and output
(test-equal "nested begin with computation"
  "30"
  (run/string (begin (let ([x 10] [y 20]) (display (+ x y))))))

;; here-string combined with output redirection
(let ([tmpfile "/tmp/hafod-accept-proc05-heredoc-file"])
  (run (cat) (<< "heredoc-to-file") (> ,tmpfile))
  (let* ([p (open-input-file tmpfile)]
         [content (get-string-all p)]
         [_c (close p)])
    (posix-unlink tmpfile)
    (test-equal "here-string combined with output redirection"
      "heredoc-to-file" content)))

;; nested process form: begin inside pipeline
(test-equal "begin inside pipeline"
  "scheme\n"
  (run/string (pipe (begin (display "scheme") (newline)) (cat) (cat))))

;; here-string with sexp content read back
(test-equal "here-string with sexp content read back"
  '(+ 1 2 3)
  (run/sexp (cat) (<< "(+ 1 2 3)")))

(test-end)
