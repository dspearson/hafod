#!/usr/bin/env hafod
-s
!#
;;; Benchmark 09: Green thread spawn/join throughput
(import (hafod))
(define N 10000)
(run-threads
  (lambda ()
    (let loop ([i 0])
      (when (< i N)
        (thread-join (spawn (lambda () i)))
        (loop (+ i 1))))))
