#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 15: Field splitter throughput
(define N 5000)
(define line "root:x:0:0:root:/root:/bin/bash")
(let ((split (infix-splitter (rx ":"))))
  (let loop ((i 0))
    (if (< i N)
        (begin
          (split line)
          (loop (+ i 1))))))
