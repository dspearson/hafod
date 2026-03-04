#!chezscheme
;;; test-scsh-doc-examples.ss -- Tests extracted from scsh documentation code examples
;;; Requirements: SCSH-01
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (hafod syntax) (hafod process) (hafod procobj)
        (hafod posix) (hafod fd-ports) (hafod compat)
        (hafod collect) (hafod port-collect) (hafod rdelim)
        (hafod process-state)
        (except (chezscheme) exit vector-append open-input-file open-output-file getenv)
        (test runner))

(test-begin "scsh Documentation Code Examples")

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

;; =============================================================================
;; Section 1: SCSH-01 -- Process forms (from process-notation.scribble)
;; =============================================================================

;; run/string with cat reading a file
(let ([tmpfile (string-append "/tmp/hafod-scsh-doc-" (number->string (pid)) "-cat")])
  (let ([p (open-output-file tmpfile)])
    (display "hello from file" p)
    (close p))
  (let ([result (run/string (cat ,tmpfile))])
    (posix-unlink tmpfile)
    (test-equal "run/string (cat file)" "hello from file" result)))

;; Pipeline: echo | sort | uniq
(test-equal "pipeline: echo | sort | uniq"
  "apple\nbanana\ncherry\n"
  (run/string (pipe (printf "banana\napple\ncherry\napple\n") (sort) (uniq))))

;; Pipeline: data through sort for ordering
(test-equal "pipeline: printf | sort"
  "a\nb\nc\n"
  (run/string (pipe (printf "b\na\nc\n") (sort))))

;; Background process: fork, wait, verify zero exit
(let* ([p (& (sleep "0"))]
       [s (wait p)])
  (test-equal "& (sleep 0) exits 0" 0 (status:exit-val s)))

;; begin process form
(test-equal "run (begin (display ...)) captures output"
  "hello\n"
  (run/string (begin (display "hello") (newline))))

;; run returns exit status
(test-equal "run (true) returns 0" 0
  (status:exit-val (run (true))))

(test-assert "run (false) returns non-zero"
  (not (zero? (status:exit-val (run (false))))))

;; =============================================================================
;; Section 2: SCSH-01 -- I/O redirections (from process-notation.scribble)
;; =============================================================================

;; Output redirect (> file)
(let ([tmpfile (string-append "/tmp/hafod-scsh-doc-" (number->string (pid)) "-out")])
  (run (echo "test output") (> ,tmpfile))
  (let* ([p (open-input-file tmpfile)]
         [content (get-string-all p)]
         [_c (close p)])
    (posix-unlink tmpfile)
    (test-equal "output redirect (> file)" "test output\n" content)))

;; Input redirect (< file)
(let ([tmpfile (string-append "/tmp/hafod-scsh-doc-" (number->string (pid)) "-in")])
  (let ([p (open-output-file tmpfile)])
    (display "input content" p)
    (close p))
  (let ([result (run/string (cat) (< ,tmpfile))])
    (posix-unlink tmpfile)
    (test-equal "input redirect (< file)" "input content" result)))

;; Append redirect (>> file)
(let ([tmpfile (string-append "/tmp/hafod-scsh-doc-" (number->string (pid)) "-append")])
  (run (echo "first") (> ,tmpfile))
  (run (echo "second") (>> ,tmpfile))
  (let* ([p (open-input-file tmpfile)]
         [content (get-string-all p)]
         [_c (close p)])
    (posix-unlink tmpfile)
    (test-equal "append redirect (>> file)" "first\nsecond\n" content)))

;; Here-string redirect (<< object)
(test-equal "here-string redirect (<< string)"
  "hello world"
  (run/string (cat) (<< "hello world")))

;; Stderr redirect (> 2 file)
(let ([errfile (string-append "/tmp/hafod-scsh-doc-" (number->string (pid)) "-err")])
  (run (sh "-c" "echo error-msg >&2") (> 2 ,errfile))
  (let* ([p (open-input-file errfile)]
         [content (get-string-all p)]
         [_c (close p)])
    (posix-unlink errfile)
    (test-equal "stderr redirect (> 2 file)" "error-msg\n" content)))

;; Fd dup (= 2 1) merges stderr into stdout
(let ([result (run/string (sh "-c" "echo normal; echo error >&2") (= 2 1))])
  (test-assert "fd dup (= 2 1) captures both"
    (and (string-contains result "normal")
         (string-contains result "error"))))

;; Close redirection (- fd)
(test-equal "close redirection (- 2) doesn't crash" 0
  (status:exit-val (run (true) (- 2))))

;; =============================================================================
;; Section 3: SCSH-01 -- Collection forms (from process-notation.scribble)
;; =============================================================================

;; run/string returns stdout as string
(test-equal "run/string (echo hello)" "hello\n"
  (run/string (echo "hello")))

;; run/strings returns list of lines
(test-equal "run/strings (printf lines)" '("a" "b" "c")
  (run/strings (printf "a\nb\nc\n")))

;; run/sexp returns a single sexp
(test-equal "run/sexp returns sexp" '(1 2 3)
  (run/sexp (begin (write '(1 2 3)))))

;; run/sexps returns list of sexps
(test-equal "run/sexps returns list" '(1 2 3)
  (run/sexps (begin (write 1) (display " ") (write 2) (display " ") (write 3))))

;; run/port returns a readable port
(let* ([p (run/port (echo "data"))]
       [content (get-string-all p)])
  (close p)
  (test-equal "run/port returns readable port" "data\n" content))

;; run/collecting captures stdout and stderr separately
(receive (status stdout-port stderr-port)
  (run/collecting (1 2) (sh "-c" "echo out; echo err >&2"))
  (let ([out (get-string-all stdout-port)]
        [err (get-string-all stderr-port)])
    (close stdout-port)
    (close stderr-port)
    (test-equal "run/collecting stdout" "out\n" out)
    (test-equal "run/collecting stderr" "err\n" err)))

;; run/port+proc returns both port and process
(receive (port proc)
  (run/port+proc (echo "port+proc"))
  (let ([content (get-string-all port)])
    (close port)
    (wait proc)
    (test-equal "run/port+proc port content" "port+proc\n" content)))

;; =============================================================================
;; Section 4: SCSH-01 -- Conditional process sequencing
;; =============================================================================

;; || returns #t when any succeeds
(test-assert "|| (false) (true) returns truthy"
  (|| (false) (true)))

;; || returns #f when all fail
(test-assert "|| (false) (false) returns falsy"
  (not (|| (false) (false))))

;; && returns #t when all succeed
(test-assert "&& (true) (true) returns truthy"
  (&& (true) (true)))

;; && returns #f when any fails
(test-assert "&& (true) (false) returns falsy"
  (not (&& (true) (false))))

;; && short-circuit: stops at first failure
(test-assert "&& (false) (true) returns falsy (short-circuit)"
  (not (&& (false) (true))))

;; || short-circuit: stops at first success
(test-assert "|| (true) (false) returns truthy (short-circuit)"
  (|| (true) (false)))

;; =============================================================================
;; Section 5: SCSH-01 -- Port utilities
;; =============================================================================

;; port->string reads all from port
(test-equal "port->string from string-port"
  "hello world"
  (port->string (open-input-string "hello world")))

;; port->string-list reads lines
(test-equal "port->string-list"
  '("line1" "line2" "line3")
  (port->string-list (open-input-string "line1\nline2\nline3\n")))

;; port->sexp-list reads s-expressions
(test-equal "port->sexp-list"
  '(1 (2 3) "four")
  (port->sexp-list (open-input-string "1 (2 3) \"four\"")))

;; port->list with custom reader
(test-equal "port->list with read-line"
  '("a" "b" "c")
  (port->list read-line (open-input-string "a\nb\nc\n")))

;; port-fold accumulates
(test-equal "port-fold counting lines"
  3
  (port-fold (open-input-string "a\nb\nc\n")
             read-line
             (lambda (line count) (+ count 1))
             0))

;; reduce-port builds list
(test-equal "reduce-port to list"
  '("c" "b" "a")
  (reduce-port (open-input-string "a\nb\nc\n")
               read-line
               cons
               '()))

(test-end)
