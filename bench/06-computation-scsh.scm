#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 06: Pure computation -- fib(35)
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(fib 35)
