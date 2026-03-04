#!/usr/bin/env hafod
-s
!#
;;; Benchmark 03: String I/O -- run/string capture
(import (hafod))
(define N 200)
(let loop ([i 0])
  (when (< i N)
    (run/string (cat "/etc/passwd"))
    (loop (+ i 1))))
