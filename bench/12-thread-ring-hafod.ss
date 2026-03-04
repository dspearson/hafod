#!/usr/bin/env hafod
-s
!#
;;; Benchmark 12: Thread ring -- pass token through N threads M times
(import (hafod))
(define RING-SIZE 50)
(define PASSES 5000)
(run-threads
  (lambda ()
    (let* ([channels (map (lambda (_) (make-channel 1))
                          (make-list RING-SIZE 0))]
           [first-ch (car channels)]
           [last-ch (list-ref channels (- RING-SIZE 1))])
      ;; Spawn ring: each thread reads from ch[i], writes to ch[i+1]
      (let loop ([i 0])
        (when (< i (- RING-SIZE 1))
          (let ([in (list-ref channels i)]
                [out (list-ref channels (+ i 1))])
            (spawn (lambda ()
                     (let loop ()
                       (let ([v (channel-receive in)])
                         (channel-send out (+ v 1))
                         (loop))))))
          (loop (+ i 1))))
      ;; Main drives: send into first, read from last
      (let loop ([i 0])
        (when (< i PASSES)
          (channel-send first-ch 0)
          (channel-receive last-ch)
          (loop (+ i 1)))))))
