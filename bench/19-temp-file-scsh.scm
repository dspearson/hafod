#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 19: Temp file create/delete
(define N 2000)
(let loop ((i 0))
  (if (< i N)
      (begin
        (let ((f (create-temp-file)))
          (delete-file f))
        (loop (+ i 1)))))
