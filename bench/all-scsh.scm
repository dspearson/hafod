#!/usr/local/bin/scsh \
-s
!#
;;; All benchmarks in a single process — no startup overhead per test.
;;; Output: BENCH <name> <milliseconds>

(define tps (ticks/sec))

(define (get-time-ms)
  (call-with-values time+ticks
    (lambda (s t)
      (exact->inexact (+ (* s 1000) (/ (* t 1000) tps))))))

(define (bench name n thunk)
  (let ((t0 (get-time-ms)))
    (let loop ((i 0))
      (if (< i n)
          (begin (thunk) (loop (+ i 1)))))
    (let ((t1 (get-time-ms)))
      (display "BENCH ")
      (display name)
      (display " ")
      (display (- t1 t0))
      (newline))))

;; 01-fork-exec
(bench "fork-exec" 500
  (lambda ()
    (wait (fork (lambda () (exit 0))))))

;; 02-pipeline
(bench "pipeline" 200
  (lambda ()
    (run (| (echo "hello world") (cat)))))

;; 03-string-io
(bench "string-io" 200
  (lambda ()
    (run/string (echo "benchmark string output"))))

;; 04-regex
(let ((re (rx "fo+" any)))
  (bench "regex" 10000
    (lambda ()
      (regexp-search re "this is a foo bar baz test string"))))

;; 05-file-ops
(bench "file-ops" 500
  (lambda ()
    (let ((f (create-temp-file)))
      (file-info f)
      (delete-file f))))

;; 06-computation
(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(bench "computation" 1
  (lambda () (fib 35)))

;; 07-env-ops
(bench "env-ops" 50000
  (lambda ()
    (setenv "BENCH_VAR" "value")
    (getenv "BENCH_VAR")))

;; 08-glob
(bench "glob" 100
  (lambda ()
    (glob "/usr/bin/*")))

;; 14-readline
(bench "readline" 500
  (lambda ()
    (run/string (echo "line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10"))))

;; 15-field-split
(let ((split (infix-splitter (rx ":"))))
  (bench "field-split" 5000
    (lambda ()
      (split "one:two:three:four:five:six:seven:eight:nine:ten"))))

;; 16-awk (simplified)
(bench "awk" 200
  (lambda ()
    (let ((lines (run/strings (echo "alpha 1\nbeta 2\ngamma 3\ndelta 4\nepsilon 5"))))
      (length lines))))

;; 17-regex-subst
(let ((re (rx "foo")))
  (bench "regex-subst" 5000
    (lambda ()
      (regexp-substitute/global #f re "foo bar foo baz foo"
        'pre "quux" 'post))))

;; 18-redir
(bench "redir" 500
  (lambda ()
    (run (begin (display "redir test") (newline)) (> /dev/null))))

;; 19-temp-file
(bench "temp-file" 2000
  (lambda ()
    (let ((f (create-temp-file)))
      (delete-file f))))

;; 20-with-cwd
(bench "with-cwd" 50000
  (lambda ()
    (with-cwd "/tmp" (cwd))))
