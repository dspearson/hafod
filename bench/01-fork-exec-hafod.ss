#!/usr/bin/env hafod
-s
!#
;;; Benchmark 01: Fork/exec/wait throughput
(import (hafod))
(define N 500)
(let loop ([i 0])
  (when (< i N)
    (run (true))
    (loop (+ i 1))))
