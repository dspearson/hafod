#!/usr/bin/env hafod
-s
!#
;;; Benchmark 19: Temp file create/delete
(import (hafod))
(define N 200)
(let loop ([i 0])
  (when (< i N)
    (let ([f (create-temp-file)])
      (delete-file f))
    (loop (+ i 1))))
