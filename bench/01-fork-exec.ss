#!/usr/bin/env hafod
-s
!#
;;; Benchmark 01: Fork/exec/wait throughput
;;; Forks and execs /bin/true N times, measuring total wall time.

(import (hafod))

(define N 1000)

(let ([t0 (time+ticks)])
  (let loop ([i 0])
    (when (< i N)
      (let ([p (fork (lambda () (exec-path "true")))])
        (wait p)
        (loop (+ i 1)))))
  (let* ([t1 (time+ticks)]
         [elapsed (- (car t1) (car t0))])
    (format #t "fork/exec/wait x~a: ~a seconds~%" N elapsed)))
