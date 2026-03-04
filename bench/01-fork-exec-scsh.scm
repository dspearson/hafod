#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 01: Fork/exec/wait throughput
(define N 500)
(let loop ((i 0))
  (if (< i N)
      (begin
        (run (true))
        (loop (+ i 1)))))
