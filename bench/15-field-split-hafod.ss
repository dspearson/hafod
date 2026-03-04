#!/usr/bin/env hafod
-s
!#
;;; Benchmark 15: Field splitter throughput
(import (hafod))
(define N 5000)
(define line "root:x:0:0:root:/root:/bin/bash")
(let ([split (infix-splitter (rx ":"))])
  (let loop ([i 0])
    (when (< i N)
      (split line)
      (loop (+ i 1)))))
