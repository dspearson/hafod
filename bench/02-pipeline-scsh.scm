#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 02: Pipeline construction throughput
(define N 200)
(let loop ((i 0))
  (if (< i N)
      (begin
        (run/string (| (echo "hello world") (cat) (cat)))
        (loop (+ i 1)))))
