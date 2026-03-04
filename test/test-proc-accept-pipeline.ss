#!chezscheme
;;; test-proc-accept-pipeline.ss -- Acceptance tests: pipelines, redirections, background processes
;;; Requirements: PROC-01, PROC-02, PROC-03
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (hafod syntax) (hafod process) (hafod procobj)
        (hafod posix) (hafod fd-ports) (hafod compat)
        (except (chezscheme) exit vector-append open-input-file open-output-file getenv)
        (test runner))

(test-begin "Process & Pipeline Acceptance")

;; Helper: substring search
(define (string-contains haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

;; =============================================================================
;; Section 1: PROC-01 -- Multi-stage pipelines (3+ stages)
;; =============================================================================

;; 3-stage pipeline: data flows through echo | sort | uniq
(test-equal "3-stage pipeline: echo | sort | uniq"
  "apple\nbanana\ncherry\n"
  (run/string (pipe (printf "banana\napple\ncherry\napple\n") (sort) (uniq))))

;; 4-stage pipeline: printf | sort | head | cat
(test-equal "4-stage pipeline: printf | sort | head | cat"
  "a\nb\nm\n"
  (run/string (pipe (printf "z\na\nm\nb\nx\n") (sort) (head "-n" "3") (cat))))

;; 3-stage pipeline exit status is last stage's exit
(test-equal "3-stage pipeline: last stage true => exit 0"
  0
  (status:exit-val (run (pipe (echo "data") (cat) (true)))))

(test-assert "3-stage pipeline: last stage false => non-zero exit"
  (not (zero? (status:exit-val (run (pipe (echo "data") (cat) (false)))))))

;; Pipeline with begin (Scheme code) stage
(test-equal "pipeline with begin stage"
  "scheme-pipeline\n"
  (run/string (pipe (begin (display "scheme-pipeline") (newline)) (cat) (cat))))

;; Pipeline preserves multi-line data
(test-equal "pipeline preserves multi-line data"
  "line1\nline2\nline3\n"
  (run/string (pipe (printf "line1\nline2\nline3\n") (cat) (cat))))

;; =============================================================================
;; Section 2: PROC-02 -- EPF redirections (stdout/stderr to separate files)
;; =============================================================================

;; stdout and stderr to separate files
(let ([stdout-file "/tmp/hafod-accept-proc02-stdout"]
      [stderr-file "/tmp/hafod-accept-proc02-stderr"])
  (run (sh "-c" "echo stdout-content; echo stderr-content >&2")
       (> ,stdout-file) (> 2 ,stderr-file))
  (let* ([p1 (open-input-file stdout-file)]
         [out (get-string-all p1)]
         [_c1 (close p1)]
         [p2 (open-input-file stderr-file)]
         [err (get-string-all p2)]
         [_c2 (close p2)])
    (posix-unlink stdout-file)
    (posix-unlink stderr-file)
    (test-equal "stdout to separate file" "stdout-content\n" out)
    (test-equal "stderr to separate file" "stderr-content\n" err)))

;; stderr redirect does not capture stdout
(let ([errfile "/tmp/hafod-accept-proc02-errcap"])
  (let ([stdout-str (run/string (sh "-c" "echo out-only; echo err-only >&2")
                                (> 2 ,errfile))])
    (let* ([p (open-input-file errfile)]
           [err-content (get-string-all p)]
           [_c (close p)])
      (posix-unlink errfile)
      (test-equal "stderr redirect: stdout captured as string" "out-only\n" stdout-str)
      (test-equal "stderr redirect: stderr in file" "err-only\n" err-content))))

;; input redirection with output redirection
(let ([infile "/tmp/hafod-accept-proc02-in"]
      [outfile "/tmp/hafod-accept-proc02-out"])
  (let ([p (open-output-file infile)])
    (display "round-trip content\n" p)
    (close p))
  (run (cat) (< ,infile) (> ,outfile))
  (let* ([p (open-input-file outfile)]
         [content (get-string-all p)]
         [_c (close p)])
    (posix-unlink infile)
    (posix-unlink outfile)
    (test-equal "input+output redirection round-trip" "round-trip content\n" content)))

;; append redirection accumulates content
(let ([tmpfile "/tmp/hafod-accept-proc02-append"])
  (run (echo "line1") (> ,tmpfile))
  (run (echo "line2") (>> ,tmpfile))
  (run (echo "line3") (>> ,tmpfile))
  (run (echo "line4") (>> ,tmpfile))
  (let* ([p (open-input-file tmpfile)]
         [content (get-string-all p)]
         [_c (close p)])
    (posix-unlink tmpfile)
    (test-equal "append redirection accumulates 4 lines"
      "line1\nline2\nline3\nline4\n" content)))

;; dup redirection: stderr to stdout
(let ([result (run/string (sh "-c" "echo normal; echo error >&2") (= 2 1))])
  (test-assert "dup redirection: stderr merged to stdout contains normal"
    (and (string? result)
         (> (string-length result) 0)
         ;; Both should be present in captured stdout
         (string-contains result "normal")
         (string-contains result "error"))))

;; close redirection: process runs with fd closed
(test-equal "close redirection: closing stderr doesn't crash" 0
  (status:exit-val (run (true) (- 2))))

;; =============================================================================
;; Section 3: PROC-03 -- Background processes
;; =============================================================================

;; & spawns background process returning proc
(let ([p (& (sleep "0"))])
  (test-assert "& returns proc object" (proc? p))
  (wait p))

;; & process exit status collected via wait - success
(let* ([p (& (true))]
       [s (wait p)])
  (test-equal "& true: exit status 0" 0 (status:exit-val s)))

;; & process exit status collected via wait - failure
(let* ([p (& (false))]
       [s (wait p)])
  (test-assert "& false: exit status non-zero"
    (not (zero? (status:exit-val s)))))

;; multiple background processes waited independently
(let ([p1 (& (true))]
      [p2 (& (false))]
      [p3 (& (sh "-c" "exit 42"))])
  (let ([s1 (wait p1)]
        [s2 (wait p2)]
        [s3 (wait p3)])
    (test-equal "multi-bg: first exits 0" 0 (status:exit-val s1))
    (test-assert "multi-bg: second exits non-zero"
      (not (zero? (status:exit-val s2))))
    (test-equal "multi-bg: third exits 42" 42 (status:exit-val s3))))

;; background process with output redirection
(let ([tmpfile "/tmp/hafod-accept-proc03-bgout"])
  (let ([p (& (echo "bg-output") (> ,tmpfile))])
    (wait p)
    (let* ([fp (open-input-file tmpfile)]
           [content (get-string-all fp)]
           [_c (close fp)])
      (posix-unlink tmpfile)
      (test-equal "background process with output redirection" "bg-output\n" content))))

;; background process runs concurrently (non-blocking launch)
(let ([p (& (sleep "1"))])
  (test-assert "background process: poll returns #f immediately"
    (not (wait p wait/poll)))
  (let ([s (wait p)])
    (test-equal "background process: final wait returns 0" 0 (status:exit-val s))))

(test-end)
