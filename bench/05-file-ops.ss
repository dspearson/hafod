#!/usr/bin/env hafod
-s
!#
;;; Benchmark 05: File operations -- stat, directory listing, glob

(import (hafod))

(define N 500)

(let ([t0 (time+ticks)])
  (let loop ([i 0])
    (when (< i N)
      (file-info "/etc/passwd")
      (directory-files "/tmp")
      (loop (+ i 1))))
  (let* ([t1 (time+ticks)]
         [elapsed (- (car t1) (car t0))])
    (format #t "stat+readdir x~a: ~a seconds~%" N elapsed)))
