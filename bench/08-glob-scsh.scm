#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 08: Glob pattern matching
(define N 100)
(let loop ((i 0))
  (if (< i N)
      (begin
        (glob "/etc/*")
        (loop (+ i 1)))))
