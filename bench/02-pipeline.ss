#!/usr/bin/env hafod
-s
!#
;;; Benchmark 02: Pipeline construction throughput

(import (hafod))

(define N 500)

(let ([t0 (time+ticks)])
  (let loop ([i 0])
    (when (< i N)
      (run/string (pipe (echo "hello world") (cat) (cat)))
      (loop (+ i 1))))
  (let* ([t1 (time+ticks)]
         [elapsed (- (car t1) (car t0))])
    (format #t "pipeline x~a: ~a seconds~%" N elapsed)))
