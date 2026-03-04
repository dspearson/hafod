#!/usr/bin/env hafod
-s
!#
;;; Benchmark 11: Preemptive scheduling -- many concurrent threads
(import (hafod))
(define N 100)
(define WORK 10000)
(run-threads
  (lambda ()
    (let ([threads
           (let loop ([i 0] [acc '()])
             (if (< i N)
                 (loop (+ i 1)
                       (cons (spawn (lambda ()
                                      (let loop ([j 0] [s 0])
                                        (if (< j WORK) (loop (+ j 1) (+ s j)) s))))
                             acc))
                 acc))])
      (for-each thread-join threads))))
