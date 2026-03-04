#!/usr/bin/env hafod
-s
!#
;;; Benchmark 16: AWK macro throughput
(import (hafod))
(define N 200)
(let loop ([i 0])
  (when (< i N)
    (let ([p (open-input-file "/etc/passwd")])
      (awk (read-line p) (line) ()
        (#t #t))
      (close-input-port p))
    (loop (+ i 1))))
