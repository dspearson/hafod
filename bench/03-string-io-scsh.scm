#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 03: String I/O -- run/string capture
(define N 200)
(let loop ((i 0))
  (if (< i N)
      (begin
        (run/string (cat "/etc/passwd"))
        (loop (+ i 1)))))
