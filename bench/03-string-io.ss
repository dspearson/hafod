#!/usr/bin/env hafod
-s
!#
;;; Benchmark 03: String I/O -- run/string and port->string

(import (hafod))

(define N 200)

(let ([t0 (time+ticks)])
  (let loop ([i 0])
    (when (< i N)
      (run/string (cat "/etc/passwd"))
      (loop (+ i 1))))
  (let* ([t1 (time+ticks)]
         [elapsed (- (car t1) (car t0))])
    (format #t "run/string x~a: ~a seconds~%" N elapsed)))
