#!/usr/bin/env scheme-script
#!chezscheme
;;; Tests for (hafod threads)

(import (hafod threads)
        (chezscheme))

(define *pass* 0)
(define *fail* 0)

(define-syntax test
  (syntax-rules ()
    [(_ name expr expected)
     (let ([result expr])
       (if (equal? result expected)
           (set! *pass* (+ *pass* 1))
           (begin
             (set! *fail* (+ *fail* 1))
             (format #t "FAIL: ~a~%  got:      ~a~%  expected: ~a~%" name result expected))))]))

(define-syntax test-true
  (syntax-rules ()
    [(_ name expr)
     (test name expr #t)]))

;;; ---- Basic thread lifecycle ----

(format #t "~%=== Thread Lifecycle ===~%")

(let ([t #f])
  (run-threads (lambda () (set! t (current-thread))))
  (test-true "current-thread returns gthread" (thread? t)))

(let ([result #f])
  (run-threads
    (lambda ()
      (let ([t (spawn (lambda () 42))])
        (set! result (thread-join t)))))
  (test "thread-join returns value" result 42))

(let ([name #f])
  (run-threads
    (lambda ()
      (let ([t (spawn (lambda () 1) "worker")])
        (set! name (thread-name t))
        (thread-join t))))
  (test "thread-name" name "worker"))

;;; ---- Preemption ----

(format #t "~%=== Preemption ===~%")

(let ([a 0] [b 0])
  (run-threads
    (lambda ()
      (let loop ([i 0])
        (when (< i 1000)
          (set! a (+ a 1))
          (loop (+ i 1)))))
    (lambda ()
      (let loop ([i 0])
        (when (< i 1000)
          (set! b (+ b 1))
          (loop (+ i 1))))))
  (test "thread A completes" a 1000)
  (test "thread B completes" b 1000))

;;; ---- Yield ----

(format #t "~%=== Yield ===~%")

(let ([order '()])
  (run-threads
    (lambda ()
      (set! order (cons 'a1 order))
      (yield)
      (set! order (cons 'a2 order)))
    (lambda ()
      (set! order (cons 'b1 order))
      (yield)
      (set! order (cons 'b2 order))))
  (test "yield: all steps completed" (length order) 4)
  (test-true "yield: both threads ran"
    (and (memq 'a1 order) (memq 'b1 order)
         (memq 'a2 order) (memq 'b2 order) #t)))

;;; ---- Sleep ----

(format #t "~%=== Sleep ===~%")

(let ([woke #f])
  (run-threads
    (lambda ()
      (thread-sleep 0.05)
      (set! woke #t)))
  (test-true "thread-sleep wakes up" woke))

(let ([counter 0])
  (run-threads
    (lambda ()
      (thread-sleep 0.1))
    (lambda ()
      (let loop ([i 0])
        (when (< i 100)
          (set! counter (+ counter 1))
          (yield)
          (loop (+ i 1))))))
  (test "sleep doesn't block others" counter 100))

;;; ---- Exception propagation ----

(format #t "~%=== Exceptions ===~%")

(let ([caught #f])
  (run-threads
    (lambda ()
      (let ([t (spawn (lambda () (error 'test "boom")))])
        (guard (e [#t (set! caught (condition-message e))])
          (thread-join t)))))
  (test "exception propagated via join" caught "boom"))

;;; ---- Termination ----

(format #t "~%=== Termination ===~%")

(let ([ran #f])
  (run-threads
    (lambda ()
      (let ([t (spawn (lambda ()
                        (let loop () (yield) (loop))))])
        (thread-terminate! t)
        (set! ran #t))))
  (test-true "thread-terminate! works" ran))

;;; ---- Channels (buffered) ----

(format #t "~%=== Channels (buffered) ===~%")

(let ([results '()])
  (run-threads
    (lambda ()
      (let ([ch (make-channel 3)])
        (channel-send ch 1)
        (channel-send ch 2)
        (channel-send ch 3)
        (set! results (list (channel-receive ch)
                            (channel-receive ch)
                            (channel-receive ch))))))
  (test "buffered channel FIFO" results '(1 2 3)))

;;; ---- Channels (synchronous) ----

(format #t "~%=== Channels (synchronous) ===~%")

;; Sender blocks until receiver picks up
(let ([received #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel)])
        (spawn (lambda () (channel-send ch 42)))
        (yield) ; let sender run and block
        (set! received (channel-receive ch)))))
  (test "sync channel send/receive" received 42))

;; Ping-pong via buffered channel (simpler)
(let ([result #f])
  (run-threads
    (lambda ()
      (let ([ch1 (make-channel 1)]
            [ch2 (make-channel 1)])
        (spawn (lambda ()
                 (let ([v (channel-receive ch1)])
                   (channel-send ch2 (+ v 1)))))
        (channel-send ch1 10)
        (set! result (channel-receive ch2)))))
  (test "ping-pong" result 11))

;;; ---- Producer-consumer ----

(format #t "~%=== Producer-Consumer ===~%")

(let ([sum 0])
  (run-threads
    (lambda ()
      (let ([ch (make-channel 5)])
        (spawn (lambda ()
                 (let loop ([i 1])
                   (when (<= i 10)
                     (channel-send ch i)
                     (loop (+ i 1))))
                 (channel-send ch 'done)))
        (let loop ()
          (let ([v (channel-receive ch)])
            (unless (eq? v 'done)
              (set! sum (+ sum v))
              (loop)))))))
  (test "producer-consumer sum 1..10" sum 55))

;;; ---- try-send / try-receive ----

(format #t "~%=== Non-blocking channel ops ===~%")

(let ([sent #f] [recv-val #f] [recv-ok #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel 1)])
        (set! sent (channel-try-send ch 99))
        (let-values ([(v ok) (channel-try-receive ch)])
          (set! recv-val v)
          (set! recv-ok ok)))))
  (test "try-send succeeds" sent #t)
  (test "try-receive value" recv-val 99)
  (test "try-receive ok" recv-ok #t))

(let ([sent #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel 0)])
        (set! sent (channel-try-send ch 1)))))
  (test "try-send fails on full sync channel" sent #f))

(let ([val #f] [ok #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel)])
        (let-values ([(v o) (channel-try-receive ch)])
          (set! val v)
          (set! ok o)))))
  (test "try-receive on empty" val #f)
  (test "try-receive on empty ok" ok #f))

;;; ---- Thread-local storage ----

(format #t "~%=== Thread-Local Storage ===~%")

;; Default value
(let ([val #f])
  (run-threads
    (lambda ()
      (let ([tl (make-thread-local 'default)])
        (set! val (thread-local-ref tl)))))
  (test "thread-local default" val 'default))

;; Each thread has own storage
(let ([a-val #f] [b-val #f])
  (run-threads
    (lambda ()
      (let ([tl (make-thread-local 0)]
            [ch (make-channel 1)])
        (spawn (lambda ()
                 (thread-local-set! tl 42)
                 (yield) (yield) (yield)
                 (set! a-val (thread-local-ref tl))
                 (channel-send ch 'done)))
        (spawn (lambda ()
                 (thread-local-set! tl 99)
                 (yield) (yield) (yield)
                 (set! b-val (thread-local-ref tl))
                 (channel-send ch 'done)))
        (channel-receive ch)
        (channel-receive ch))))
  (test-true "thread-local isolation"
    (and (number? a-val) (number? b-val)
         (not (eqv? a-val b-val)) #t)))

;;; ---- Multiple joins ----

(format #t "~%=== Multiple Joins ===~%")

(let ([r1 #f] [r2 #f])
  (run-threads
    (lambda ()
      (let ([worker (spawn (lambda () 7))])
        (spawn (lambda () (set! r1 (thread-join worker))))
        (spawn (lambda () (set! r2 (thread-join worker)))))))
  (test "multiple join A" r1 7)
  (test "multiple join B" r2 7))

;;; ---- Stress ----

(format #t "~%=== Stress ===~%")

(let ([count 0])
  (run-threads
    (lambda ()
      (let ([threads
             (let loop ([i 0] [acc '()])
               (if (< i 50)
                   (loop (+ i 1)
                         (cons (spawn (lambda () (yield) 1)) acc))
                   acc))])
        (for-each (lambda (t)
                    (set! count (+ count (thread-join t))))
                  threads))))
  (test "50 threads all complete" count 50))

;;; ---- Channel close ----

(format #t "~%=== Channel Close ===~%")

;; channel-closed? returns #f on new channel
(let ([closed #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel 1)])
        (set! closed (channel-closed? ch)))))
  (test "new channel not closed" closed #f))

;; channel-closed? returns #t after close
(let ([closed #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel 1)])
        (channel-close ch)
        (set! closed (channel-closed? ch)))))
  (test "closed channel is closed" closed #t))

;; channel-send on closed channel raises error
(let ([caught #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel 1)])
        (channel-close ch)
        (guard (e [#t (set! caught #t)])
          (channel-send ch 42)))))
  (test-true "send on closed raises error" caught))

;; channel-receive on closed-and-empty channel raises error
(let ([caught #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel 1)])
        (channel-close ch)
        (guard (e [#t (set! caught #t)])
          (channel-receive ch)))))
  (test-true "receive on closed+empty raises error" caught))

;; channel-try-send on closed channel returns #f
(let ([result #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel 1)])
        (channel-close ch)
        (set! result (channel-try-send ch 42)))))
  (test "try-send on closed returns #f" result #f))

;; channel-try-receive on closed+empty channel returns #f
(let ([val #f] [ok #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel 1)])
        (channel-close ch)
        (let-values ([(v o) (channel-try-receive ch)])
          (set! val v)
          (set! ok o)))))
  (test "try-receive on closed val" val #f)
  (test "try-receive on closed ok" ok #f))

;; Buffered data drains before error
(let ([v1 #f] [v2 #f] [caught #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel 3)])
        (channel-send ch 'a)
        (channel-send ch 'b)
        (channel-close ch)
        (set! v1 (channel-receive ch))
        (set! v2 (channel-receive ch))
        (guard (e [#t (set! caught #t)])
          (channel-receive ch)))))
  (test "drain buffered v1" v1 'a)
  (test "drain buffered v2" v2 'b)
  (test-true "error after drain" caught))

;; Blocked receivers are woken on close
(let ([woke #f] [val #f])
  (run-threads
    (lambda ()
      (let ([ch (make-channel)])
        (spawn (lambda ()
                 (set! val (channel-receive ch))
                 (set! woke #t)))
        (yield) ; let receiver block
        (channel-close ch)
        (yield) (yield)))) ; let receiver wake
  (test-true "blocked receiver woken on close" woke)
  (test "woken receiver gets void" val (void)))

;;; ---- Summary ----

(format #t "~%=== Results: ~a passed, ~a failed ===~%~%" *pass* *fail*)
(when (> *fail* 0) (exit 1))
