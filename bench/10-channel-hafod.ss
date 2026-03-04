#!/usr/bin/env hafod
-s
!#
;;; Benchmark 10: Channel throughput -- producer/consumer
(import (hafod))
(define N 50000)
(run-threads
  (lambda ()
    (let ([ch (make-channel 64)])
      (spawn (lambda ()
               (let loop ([i 0])
                 (when (< i N)
                   (channel-send ch i)
                   (loop (+ i 1))))
               (channel-send ch 'done)))
      (let loop ()
        (let ([v (channel-receive ch)])
          (unless (eq? v 'done) (loop)))))))
