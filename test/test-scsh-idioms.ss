#!chezscheme
;;; test-scsh-idioms.ss -- Tests for scsh shell idioms and EPF patterns
;;; Requirements: SCSH-03
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (hafod syntax) (hafod process) (hafod procobj)
        (hafod posix) (hafod fd-ports) (hafod compat)
        (hafod collect) (hafod port-collect) (hafod rdelim)
        (hafod re) (hafod environment) (hafod process-state)
        (hafod fileinfo) (hafod temp-file)
        (except (chezscheme) exit vector-append open-input-file open-output-file getenv record-reader)
        (test runner))

(test-begin "scsh Shell Idioms")

;; Helper: substring search
(define (string-contains haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

;; Helper: trim trailing newline
(define (trim-newline s)
  (let ((len (string-length s)))
    (if (and (> len 0) (char=? (string-ref s (- len 1)) #\newline))
        (substring s 0 (- len 1))
        s)))

(define test-base (string-append "/tmp/hafod-scsh-idioms-" (number->string (pid))))

;; =============================================================================
;; Section 1: SCSH-03 -- Here-string idioms
;; =============================================================================

;; Here-string as stdin
(test-equal "here-string: basic" "hello world"
  (run/string (cat) (<< "hello world")))

;; Here-string with multiline content
(test-equal "here-string: multiline" "line1\nline2\n"
  (run/string (cat) (<< "line1\nline2\n")))

;; Here-string in pipeline
(test-equal "here-string: in pipeline with sort" "a\nb\nc\n"
  (run/string (pipe (cat) (sort)) (<< "b\na\nc\n")))

;; Here-string with unquote (dynamic value)
(let ((x "dynamic"))
  (test-equal "here-string: unquote" "dynamic"
    (run/string (cat) (<< ,x))))

;; =============================================================================
;; Section 2: SCSH-03 -- Process substitution patterns (run/port as input source)
;; =============================================================================

;; Read process output as port
(test-equal "run/port as input: port->string" "data\n"
  (let ((p (run/port (echo "data"))))
    (let ((result (port->string p)))
      (close p)
      result)))

;; run/port+proc: get both port and process object
(test-equal "run/port+proc: read port then wait" "inner\n"
  (receive (port proc)
    (run/port+proc (echo "inner"))
    (let ((result (get-string-all port)))
      (close port)
      (wait proc)
      result)))

;; Nested run/string inside begin
(test-equal "nested run/string in begin" "hello\n"
  (run/string (begin (display (run/string (echo "hello"))))))

;; =============================================================================
;; Section 3: SCSH-03 -- EPF form patterns
;; =============================================================================

;; Multiple redirections: stdout to one file, stderr to another
(let ([outfile (string-append test-base "-epf-out")]
      [errfile (string-append test-base "-epf-err")])
  (run (sh "-c" "echo stdout-data; echo stderr-data >&2")
       (> ,outfile) (> 2 ,errfile))
  (let* ([p1 (open-input-file outfile)]
         [out (get-string-all p1)]
         [_c1 (close p1)]
         [p2 (open-input-file errfile)]
         [err (get-string-all p2)]
         [_c2 (close p2)])
    (posix-unlink outfile)
    (posix-unlink errfile)
    (test-equal "EPF: stdout to file" "stdout-data\n" out)
    (test-equal "EPF: stderr to file" "stderr-data\n" err)))

;; Fd close in redirection
(test-equal "EPF: close stderr doesn't crash" 0
  (status:exit-val (run (begin (display "out")) (- 2))))

;; Fd dup: stderr to stdout
(let ([result (run/string (sh "-c" "echo out; echo err >&2") (= 2 1))])
  (test-assert "EPF: dup stderr to stdout captures both"
    (and (string-contains result "out")
         (string-contains result "err"))))

;; Append mode: write twice with >>
(let ([tmpfile (string-append test-base "-epf-append")])
  (run (echo "first") (> ,tmpfile))
  (run (echo "second") (>> ,tmpfile))
  (let* ([p (open-input-file tmpfile)]
         [content (get-string-all p)]
         [_c (close p)])
    (posix-unlink tmpfile)
    (test-equal "EPF: append mode" "first\nsecond\n" content)))

;; Input from file + output to file simultaneously
(let ([infile (string-append test-base "-epf-in")]
      [outfile (string-append test-base "-epf-out2")])
  (let ([p (open-output-file infile)])
    (display "round-trip" p)
    (close p))
  (run (cat) (< ,infile) (> ,outfile))
  (let* ([p (open-input-file outfile)]
         [content (get-string-all p)]
         [_c (close p)])
    (posix-unlink infile)
    (posix-unlink outfile)
    (test-equal "EPF: input + output redirect" "round-trip" content)))

;; =============================================================================
;; Section 4: SCSH-03 -- Conditional process sequencing idioms
;; =============================================================================

;; || short-circuit: first success stops
(test-assert "||: first success stops" (|| (true) (false)))

;; || all fail
(test-assert "||: all fail" (not (|| (false) (false))))

;; && short-circuit: first failure stops
(test-assert "&&: first failure stops" (not (&& (false) (true))))

;; && all succeed
(test-assert "&&: all succeed" (&& (true) (true)))

;; Conditional with real commands
(test-assert "&&: test -f /etc/passwd && true"
  (&& (test "-f" "/etc/passwd") (true)))

;; Sequential conditional: && then ||
(test-assert "sequential: && fails then || succeeds"
  (or (not (&& (false) (true)))
      (|| (true) (false))))

;; =============================================================================
;; Section 5: SCSH-03 -- begin process form patterns
;; =============================================================================

;; begin with Scheme I/O
(test-equal "begin: for-each display" "abc"
  (run/string (begin (for-each display '("a" "b" "c")))))

;; begin sees redirections: write to file via begin + redirect
(let ([tmpfile (string-append test-base "-begin-redir")])
  (run (begin (display "begin-output")) (> ,tmpfile))
  (let* ([p (open-input-file tmpfile)]
         [content (get-string-all p)]
         [_c (close p)])
    (posix-unlink tmpfile)
    (test-equal "begin: sees output redirect" "begin-output" content)))

;; begin in pipeline
(test-equal "begin: in pipeline with sort" "hello\nworld\n"
  (run/string (pipe (begin (display "world\nhello\n")) (sort))))

;; begin with env access
(test-assert "begin: env access"
  (let ((result (run/string (begin (display (get-environment-variable "HOME"))))))
    (and (string? result) (> (string-length result) 0))))

;; begin computing and printing result
(test-equal "begin: Scheme computation" "42"
  (run/string (begin (display (* 6 7)))))

;; begin with multiple statements
(test-equal "begin: multiple display" "hello world"
  (run/string (begin (display "hello") (display " ") (display "world"))))

(test-end)
