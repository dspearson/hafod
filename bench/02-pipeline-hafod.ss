#!/usr/bin/env hafod
-s
!#
;;; Benchmark 02: Pipeline construction throughput
(import (hafod))
(define N 200)
(let loop ([i 0])
  (when (< i N)
    (run/string (pipe (echo "hello world") (cat) (cat)))
    (loop (+ i 1))))
