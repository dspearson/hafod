#!/usr/bin/env hafod
-s
!#
;;; Benchmark 05: File operations -- stat, directory listing
(import (hafod))
(define N 500)
(let loop ([i 0])
  (when (< i N)
    (file-info "/etc/passwd")
    (directory-files "/tmp")
    (loop (+ i 1))))
