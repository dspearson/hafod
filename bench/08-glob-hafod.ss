#!/usr/bin/env hafod
-s
!#
;;; Benchmark 08: Glob pattern matching
(import (hafod))
(define N 100)
(let loop ([i 0])
  (when (< i N)
    (glob "/etc/*")
    (loop (+ i 1))))
