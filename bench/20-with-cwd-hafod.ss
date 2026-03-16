#!/usr/bin/env hafod
-s
!#
;;; Benchmark 20: with-cwd overhead
(import (hafod))
(define N 50000)
(let loop ([i 0])
  (when (< i N)
    (with-cwd "/tmp" (cwd))
    (loop (+ i 1))))
