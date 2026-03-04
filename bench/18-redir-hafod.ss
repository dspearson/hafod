#!/usr/bin/env hafod
-s
!#
;;; Benchmark 18: Output capture / redirection overhead
(import (hafod))
(define N 500)
(let loop ([i 0])
  (when (< i N)
    (run/string (echo hello))
    (loop (+ i 1))))
