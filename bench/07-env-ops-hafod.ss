#!/usr/bin/env hafod
-s
!#
;;; Benchmark 07: Environment variable operations
(import (hafod))
(define N 50000)
(let loop ([i 0])
  (when (< i N)
    (setenv "BENCH_VAR" (number->string i))
    (getenv "BENCH_VAR")
    (loop (+ i 1))))
