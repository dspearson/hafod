#!/usr/bin/env hafod
-s
!#
;;; Benchmark 14: Read-line throughput
(import (hafod))
(define N 500)
(let loop ([i 0])
  (when (< i N)
    (let ([p (open-input-file "/etc/passwd")])
      (let rd ()
        (let ([line (read-line p)])
          (unless (eof-object? line) (rd))))
      (close-input-port p))
    (loop (+ i 1))))
