#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 16: AWK macro throughput
(define N 200)
(let loop ((i 0))
  (if (< i N)
      (begin
        (let ((p (open-input-file "/etc/passwd")))
          (awk (read-line p) (line) ()
            (#t #t))
          (close-input-port p))
        (loop (+ i 1)))))
