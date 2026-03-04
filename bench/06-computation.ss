#!/usr/bin/env hafod
-s
!#
;;; Benchmark 06: Pure computation -- fib, list ops, string building

(import (hafod))

(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(let ([t0 (time+ticks)])
  (fib 35)
  (let* ([t1 (time+ticks)]
         [elapsed (- (car t1) (car t0))])
    (format #t "fib(35): ~a seconds (result: ~a)~%" elapsed (fib 35))))
