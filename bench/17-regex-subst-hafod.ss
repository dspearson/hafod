#!/usr/bin/env hafod
-s
!#
;;; Benchmark 17: Regex substitution throughput
(import (hafod))
(define N 5000)
(define text "The quick brown fox jumps over the lazy dog")
(let ([re (rx (+ alphabetic))])
  (let loop ([i 0])
    (when (< i N)
      (regexp-substitute/global #f re text 'pre "WORD" 'post)
      (loop (+ i 1)))))
