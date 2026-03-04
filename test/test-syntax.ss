#!chezscheme
;;; test-syntax.ss -- Tests for (hafod syntax) EPF Process Notation
;;; Tests all EPF macros, redirections, pipelines, and process forms.
;;; scsh EPF order: (exec-epf process-form redirection ...)
;;; So: (run (echo "hello") (> "file")) -- process form FIRST, redirections AFTER.
;;;
;;; Note: bare | cannot be used as a symbol in Chez Scheme source code
;;; (it starts a symbol escape sequence). Use the `pipe` alias instead.

(library-directories '(("src" . "src") ("." . ".")))
(import (hafod syntax) (hafod process) (hafod procobj)
        (hafod posix) (hafod fd-ports) (hafod compat)
        (except (chezscheme) exit vector-append open-input-file open-output-file getenv)
        (test runner))

(test-begin "EPF Process Notation")

;; =============================================================================
;; Runtime helpers
;; =============================================================================

;; --- open-string-source ---

(test-assert "open-string-source returns input port"
  (let ([p (open-string-source "hello")])
    (let ([result (input-port? p)])
      (close p)
      result)))

(test-equal "open-string-source: string content"
  "hello world"
  (let ([p (open-string-source "hello world")])
    (let ([line (get-line p)])
      (close p)
      line)))

(test-equal "open-string-source: number"
  "42"
  (let ([p (open-string-source 42)])
    (let ([line (get-line p)])
      (close p)
      line)))

(test-equal "open-string-source: symbol"
  "foo"
  (let ([p (open-string-source 'foo)])
    (let ([line (get-line p)])
      (close p)
      line)))

;; --- stdports->stdio ---

(test-assert "stdports->stdio does not error"
  (let ([status (wait (fork (lambda ()
                              (stdports->stdio)
                              (%exit 0))))])
    (zero? (status:exit-val status))))

;; --- with-stdio-ports* ---

(test-assert "with-stdio-ports* does not error"
  (let ([status (wait (fork (lambda ()
                              (with-stdio-ports* (lambda () #t))
                              (%exit 0))))])
    (zero? (status:exit-val status))))

;; =============================================================================
;; Basic process forms: run, &, exec-epf
;; =============================================================================

(test-equal "run true exits 0" 0
  (status:exit-val (run (true))))

(test-assert "run false exits non-zero"
  (not (zero? (run (false)))))

(test-assert "& returns proc object"
  (let ([p (& (true))])
    (wait p)
    (proc? p)))

(test-equal "run echo exits 0" 0
  (status:exit-val (run (echo "hello"))))

(test-equal "run with multiple args" 0
  (status:exit-val (run (echo "hello" "world"))))

;; exec-epf in forked child
(test-equal "exec-epf in fork runs and exits" 0
  (let ([p (fork (lambda () (exec-epf (true))))])
    (status:exit-val (wait p))))

;; =============================================================================
;; Output redirection: >
;; Process form comes FIRST, redirections AFTER.
;; =============================================================================

(let ([tmpfile "/tmp/hafod-test-redir-out"])
  (run (echo "redirected output") (> ,tmpfile))
  (let ([p (open-input-file tmpfile)])
    (let ([line (get-line p)])
      (close p)
      (posix-unlink tmpfile)
      (test-equal "> redirects stdout to file" "redirected output" line))))

;; > with explicit fd
(let ([tmpfile "/tmp/hafod-test-redir-fd"])
  (run (echo "fd1-output") (> 1 ,tmpfile))
  (let ([p (open-input-file tmpfile)])
    (let ([line (get-line p)])
      (close p)
      (posix-unlink tmpfile)
      (test-equal "> with explicit fd 1" "fd1-output" line))))

;; =============================================================================
;; Input redirection: <
;; =============================================================================

(let ([infile "/tmp/hafod-test-redir-in"]
      [outfile "/tmp/hafod-test-redir-in-out"])
  ;; Create input file
  (let ([p (open-output-file infile)])
    (display "input content" p)
    (newline p)
    (close p))
  ;; Read via < and capture with >. Process form first, then redirections.
  (run (cat) (< ,infile) (> ,outfile))
  (let ([p (open-input-file outfile)])
    (let ([line (get-line p)])
      (close p)
      (posix-unlink infile)
      (posix-unlink outfile)
      (test-equal "< redirects stdin from file" "input content" line))))

;; =============================================================================
;; Append redirection: >>
;; =============================================================================

(let ([tmpfile "/tmp/hafod-test-append"])
  (run (echo "line1") (> ,tmpfile))
  (run (echo "line2") (>> ,tmpfile))
  (let ([p (open-input-file tmpfile)])
    (let* ([l1 (get-line p)]
           [l2 (get-line p)])
      (close p)
      (posix-unlink tmpfile)
      (test-equal ">> appends line1" "line1" l1)
      (test-equal ">> appends line2" "line2" l2))))

;; =============================================================================
;; Here-string redirection: <<
;; =============================================================================

(let ([outfile "/tmp/hafod-test-heredoc-out"])
  (run (cat) (<< "hello heredoc") (> ,outfile))
  (let ([p (open-input-file outfile)])
    (let ([content (get-line p)])
      (close p)
      (posix-unlink outfile)
      (test-equal "<< feeds here-string to stdin" "hello heredoc" content))))

;; << with non-string object
(let ([outfile "/tmp/hafod-test-heredoc-num"])
  (run (cat) (<< 12345) (> ,outfile))
  (let ([p (open-input-file outfile)])
    (let ([content (get-line p)])
      (close p)
      (posix-unlink outfile)
      (test-equal "<< with number" "12345" content))))

;; =============================================================================
;; Dup redirection: =
;; =============================================================================

;; (= 2 1) makes fd 2 point where fd 1 points -- stderr goes to stdout
(let ([outfile "/tmp/hafod-test-dup"])
  (run (begin (display "to-stderr" (current-error-port))) (> ,outfile) (= 2 1))
  (let ([p (open-input-file outfile)])
    (let ([content (get-line p)])
      (close p)
      (posix-unlink outfile)
      (test-equal "= dups fd" "to-stderr" content))))

;; =============================================================================
;; Close redirection: -
;; =============================================================================

(test-equal "- closes fd and process still runs" 0
  (status:exit-val (run (true) (- 2))))

;; =============================================================================
;; Pipelines: pipe
;; =============================================================================

;; Note: use `pipe` keyword since bare | is a symbol escape in Chez Scheme.

(let ([outfile "/tmp/hafod-test-pipe"])
  (run (pipe (echo "piped") (cat)) (> ,outfile))
  (let ([p (open-input-file outfile)])
    (let ([content (get-line p)])
      (close p)
      (posix-unlink outfile)
      (test-equal "pipe pipeline works" "piped" content))))

(test-equal "multi-stage pipeline exits 0" 0
  (status:exit-val (run (pipe (echo "a") (cat) (cat)))))

;; =============================================================================
;; Scheme code process form: begin
;; =============================================================================

(let ([outfile "/tmp/hafod-test-begin"])
  (run (begin (display "from-scheme") (newline)) (> ,outfile))
  (let ([p (open-input-file outfile)])
    (let ([content (get-line p)])
      (close p)
      (posix-unlink outfile)
      (test-equal "begin process form runs Scheme code" "from-scheme" content))))

;; begin with multiple expressions
(let ([outfile "/tmp/hafod-test-begin2"])
  (run (begin (display "hello ") (display "world") (newline)) (> ,outfile))
  (let ([p (open-input-file outfile)])
    (let ([content (get-line p)])
      (close p)
      (posix-unlink outfile)
      (test-equal "begin with multiple expressions" "hello world" content))))

;; =============================================================================
;; Sequencing operators: || && :or:
;; =============================================================================

(test-assert "|| short-circuit OR: second succeeds"
  (|| (false) (true)))

(test-assert "|| all fail returns #f"
  (not (|| (false) (false))))

(test-assert "&& all succeed returns #t"
  (&& (true) (true)))

(test-assert "&& second fails returns #f"
  (not (&& (true) (false))))

(test-assert "&& first fails short-circuits"
  (not (&& (false) (true))))

(test-assert ":or: alias works"
  (:or: (false) (true)))

(test-assert ":or: all fail returns #f"
  (not (:or: (false) (false))))

;; =============================================================================
;; stdports redirection keyword
;; =============================================================================

(test-equal "stdports keyword in redirection" 0
  (status:exit-val (run (true) stdports)))

;; =============================================================================
;; Combined redirections
;; =============================================================================

;; Input from file, output to different file
(let ([infile "/tmp/hafod-test-combo-in"]
      [outfile "/tmp/hafod-test-combo-out"])
  (let ([p (open-output-file infile)])
    (display "combo-test" p)
    (newline p)
    (close p))
  (run (cat) (< ,infile) (> ,outfile))
  (let ([p (open-input-file outfile)])
    (let ([content (get-line p)])
      (close p)
      (posix-unlink infile)
      (posix-unlink outfile)
      (test-equal "combined < and > redirections" "combo-test" content))))

;; =============================================================================
;; Summary
;; =============================================================================

(test-end)
