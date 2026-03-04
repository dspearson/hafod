#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 20: with-cwd overhead
(define N 5000)
(let loop ((i 0))
  (if (< i N)
      (begin
        (with-cwd "/tmp" (cwd))
        (loop (+ i 1)))))
