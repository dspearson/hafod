#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 14: Read-line throughput
(define N 500)
(let loop ((i 0))
  (if (< i N)
      (begin
        (let ((p (open-input-file "/etc/passwd")))
          (let rd ()
            (let ((line (read-line p)))
              (if (not (eof-object? line)) (rd))))
          (close-input-port p))
        (loop (+ i 1)))))
