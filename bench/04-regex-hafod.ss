#!/usr/bin/env hafod
-s
!#
;;; Benchmark 04: SRE regex compilation and matching
(import (hafod))
(define N 10000)
(define text "The quick brown fox jumps over the lazy dog 12345 foo@bar.com")
(let ([re (rx (: (+ alphabetic) "@" (+ alphabetic) "." (+ alphabetic)))])
  (let loop ([i 0] [count 0])
    (if (< i N)
        (loop (+ i 1) (if (regexp-search re text) (+ count 1) count))
        count)))
