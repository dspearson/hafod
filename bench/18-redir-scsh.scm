#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 18: Output capture / redirection overhead
(define N 500)
(let loop ((i 0))
  (if (< i N)
      (begin
        (run/string (echo hello))
        (loop (+ i 1)))))
