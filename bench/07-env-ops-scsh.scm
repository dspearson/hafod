#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 07: Environment variable operations
(define N 5000)
(let loop ((i 0))
  (if (< i N)
      (begin
        (setenv "BENCH_VAR" (number->string i))
        (getenv "BENCH_VAR")
        (loop (+ i 1)))))
