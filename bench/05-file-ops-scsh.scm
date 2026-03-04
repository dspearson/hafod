#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 05: File operations -- stat, directory listing
(define N 500)
(let loop ((i 0))
  (if (< i N)
      (begin
        (file-info "/etc/passwd")
        (directory-files "/tmp")
        (loop (+ i 1)))))
