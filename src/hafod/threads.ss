#!chezscheme
;;; (hafod threads) — Preemptive green threads using Chez engines
;;;
;;; Each green thread runs as a Chez engine with a fuel budget.
;;; The scheduler round-robins through ready threads, preempting
;;; when fuel expires. Channels provide Go-style communication.

(library (hafod threads)
  (export
    ;; Thread lifecycle
    spawn thread? thread-name current-thread
    ;; Scheduling
    yield thread-sleep thread-join thread-terminate!
    ;; Channels
    make-channel channel? channel-send channel-receive
    channel-try-send channel-try-receive channel-close channel-closed?
    ;; Thread-local storage
    make-thread-local thread-local-ref thread-local-set!
    ;; Scheduler control
    run-threads threads-start! threads-shutdown! threads-running?)
  (import (except (hafod internal base) thread? thread-join sleep)
          (only (chezscheme) sleep)
          (hafod exit-hooks))

  ;;; ---- FIFO Queue (vector: front, back, count) ----
  ;;; O(1) queue-count, O(1) amortised enqueue!/dequeue!

  (define (make-queue) (vector '() '() 0))
  (define (queue-empty? q) (fx=? (vector-ref q 2) 0))
  (define (queue-count q) (vector-ref q 2))

  (define (enqueue! q x)
    (vector-set! q 1 (cons x (vector-ref q 1)))
    (vector-set! q 2 (fx+ (vector-ref q 2) 1)))

  (define (dequeue! q)
    (when (null? (vector-ref q 0))
      (vector-set! q 0 (reverse (vector-ref q 1)))
      (vector-set! q 1 '()))
    (let ([v (car (vector-ref q 0))])
      (vector-set! q 0 (cdr (vector-ref q 0)))
      (vector-set! q 2 (fx- (vector-ref q 2) 1))
      v))

  (define (queue-remove! q pred)
    (let ([front (remp pred (vector-ref q 0))]
          [back (remp pred (vector-ref q 1))])
      (vector-set! q 0 front)
      (vector-set! q 1 back)
      (vector-set! q 2 (fx+ (length front) (length back)))))

  (define (queue->list q)
    (append (vector-ref q 0) (reverse (vector-ref q 1))))

  ;;; ---- Thread Record ----

  (define-record-type gthread
    (fields
      (immutable id)
      (immutable name)
      (mutable state)        ; ready | running | blocked | sleeping | dead
      (mutable engine)       ; Chez engine or #f
      (mutable result)       ; return value or exception
      (mutable exception?)   ; #t if result is unhandled exception
      (mutable waiters)      ; list of threads waiting on join
      (mutable locals)       ; eq-hashtable for thread-local storage
      (mutable wake-time))   ; #f or nanosecond timestamp
    (protocol
      (lambda (new)
        (lambda (id name engine)
          (new id name 'ready engine #f #f '() (make-eq-hashtable) #f)))))

  (define (thread? x) (gthread? x))
  (define thread-name gthread-name)

  ;;; ---- Scheduler State ----

  (define *run-queue* (make-queue))
  (define *sleep-queue* '())
  (define *all-threads* '())
  (define *current* #f)
  (define *running?* #f)
  (define *next-id* 0)
  (define *fuel* 500)

  (define (current-thread) *current*)
  (define (threads-running?) *running?*)

  (define (next-id!)
    (let ([id *next-id*])
      (set! *next-id* (+ id 1))
      id))

  (define (current-nanoseconds)
    (let ([t (current-time 'time-monotonic)])
      (+ (* (time-second t) 1000000000)
         (time-nanosecond t))))

  ;;; ---- Thread Lifecycle ----

  (define spawn
    (case-lambda
      [(thunk) (spawn thunk #f)]
      [(thunk name)
       (let* ([id (next-id!)]
              [wrapped (lambda ()
                         (guard (e [#t (engine-return (cons 'exception e))])
                           (let ([v (thunk)])
                             (engine-return (cons 'ok v)))))]
              [eng (make-engine wrapped)]
              [t (make-gthread id name eng)])
         (set! *all-threads* (cons t *all-threads*))
         (enqueue! *run-queue* t)
         t)]))

  (define (finish-thread! t tag value)
    (gthread-state-set! t 'dead)
    (gthread-engine-set! t #f)
    (case tag
      [(ok) (gthread-result-set! t value)]
      [(exception)
       (gthread-result-set! t value)
       (gthread-exception?-set! t #t)])
    (for-each (lambda (waiter)
                (gthread-state-set! waiter 'ready)
                (enqueue! *run-queue* waiter))
              (gthread-waiters t))
    (gthread-waiters-set! t '())
    (set! *all-threads* (remq t *all-threads*)))

  ;;; ---- Scheduling Primitives ----

  (define (yield)
    (when *current*
      (gthread-state-set! *current* 'ready)
      (enqueue! *run-queue* *current*)
      (engine-block)))

  (define (thread-sleep seconds)
    (when *current*
      (gthread-state-set! *current* 'sleeping)
      (gthread-wake-time-set! *current*
        (+ (current-nanoseconds)
           (exact (floor (* seconds 1000000000)))))
      (set! *sleep-queue*
        (cons *current* *sleep-queue*))
      (engine-block)))

  (define (thread-join t)
    (cond
      [(eq? (gthread-state t) 'dead)
       (if (gthread-exception? t)
           (raise (gthread-result t))
           (gthread-result t))]
      [*current*
       (gthread-state-set! *current* 'blocked)
       (gthread-waiters-set! t (cons *current* (gthread-waiters t)))
       (engine-block)
       (if (gthread-exception? t)
           (raise (gthread-result t))
           (gthread-result t))]
      [else
       (error 'thread-join "cannot join from outside scheduler")]))

  (define (thread-terminate! t)
    (unless (eq? (gthread-state t) 'dead)
      (finish-thread! t 'ok (void))
      (queue-remove! *run-queue* (lambda (x) (eq? x t)))
      (set! *sleep-queue* (remq t *sleep-queue*))))

  ;;; ---- Channel ----

  (define-record-type channel
    (fields
      (mutable buffer)     ; queue
      (immutable capacity)
      (mutable closed?)
      (mutable senders)    ; list of (thread . value)
      (mutable receivers)) ; list of threads
    (protocol
      (lambda (new)
        (case-lambda
          [() (new (make-queue) 0 #f (make-queue) (make-queue))]
          [(cap) (new (make-queue) cap #f (make-queue) (make-queue))]))))

  (define (channel-close ch)
    (channel-closed?-set! ch #t)
    (for-each (lambda (t)
                (gthread-result-set! t (void))
                (gthread-state-set! t 'ready)
                (enqueue! *run-queue* t))
              (queue->list (channel-receivers ch)))
    (channel-receivers-set! ch (make-queue))
    (for-each (lambda (pair)
                (gthread-state-set! (car pair) 'ready)
                (enqueue! *run-queue* (car pair)))
              (queue->list (channel-senders ch)))
    (channel-senders-set! ch (make-queue)))

  (define (channel-send ch value)
    (when (channel-closed? ch)
      (error 'channel-send "channel is closed"))
    (cond
      ;; Waiting receiver -- hand off directly
      [(not (queue-empty? (channel-receivers ch)))
       (let ([receiver (dequeue! (channel-receivers ch))])
         (gthread-result-set! receiver value)
         (gthread-state-set! receiver 'ready)
         (enqueue! *run-queue* receiver))]
      ;; Buffer has room
      [(< (queue-count (channel-buffer ch)) (channel-capacity ch))
       (enqueue! (channel-buffer ch) value)]
      ;; Must block
      [else
       (unless *current*
         (error 'channel-send "cannot block outside scheduler"))
       (gthread-state-set! *current* 'blocked)
       (enqueue! (channel-senders ch) (cons *current* value))
       (engine-block)]))

  (define (channel-receive ch)
    (cond
      ;; Buffer has data
      [(not (queue-empty? (channel-buffer ch)))
       (let ([value (dequeue! (channel-buffer ch))])
         ;; Wake a blocked sender if any
         (when (not (queue-empty? (channel-senders ch)))
           (let* ([sp (dequeue! (channel-senders ch))]
                  [sender (car sp)]
                  [send-val (cdr sp)])
             (enqueue! (channel-buffer ch) send-val)
             (gthread-state-set! sender 'ready)
             (enqueue! *run-queue* sender)))
         value)]
      ;; Blocked sender -- synchronous handoff
      [(not (queue-empty? (channel-senders ch)))
       (let* ([sp (dequeue! (channel-senders ch))]
              [sender (car sp)]
              [value (cdr sp)])
         (gthread-state-set! sender 'ready)
         (enqueue! *run-queue* sender)
         value)]
      ;; Closed and empty
      [(channel-closed? ch)
       (error 'channel-receive "channel is closed and empty")]
      ;; Must block
      [else
       (unless *current*
         (error 'channel-receive "cannot block outside scheduler"))
       (gthread-state-set! *current* 'blocked)
       (enqueue! (channel-receivers ch) *current*)
       (engine-block)
       (gthread-result *current*)]))

  (define (channel-try-send ch value)
    (cond
      [(channel-closed? ch) #f]
      [(not (queue-empty? (channel-receivers ch)))
       (channel-send ch value) #t]
      [(< (queue-count (channel-buffer ch)) (channel-capacity ch))
       (enqueue! (channel-buffer ch) value) #t]
      [else #f]))

  (define (channel-try-receive ch)
    (cond
      [(not (queue-empty? (channel-buffer ch)))
       (values (channel-receive ch) #t)]
      [(not (queue-empty? (channel-senders ch)))
       (values (channel-receive ch) #t)]
      [else (values #f #f)]))

  ;;; ---- Thread-Local Storage ----

  (define-record-type thread-local
    (fields
      (immutable key)      ; gensym
      (immutable default))
    (protocol
      (lambda (new)
        (lambda (default)
          (new (gensym "tl") default)))))

  (define (thread-local-ref tl)
    (let ([t (current-thread)])
      (unless t (error 'thread-local-ref "not in a thread"))
      (hashtable-ref (gthread-locals t) (thread-local-key tl)
                     (thread-local-default tl))))

  (define (thread-local-set! tl value)
    (let ([t (current-thread)])
      (unless t (error 'thread-local-set! "not in a thread"))
      (hashtable-set! (gthread-locals t) (thread-local-key tl) value)))

  ;;; ---- Scheduler ----

  (define (wake-sleepers!)
    (let ([now (current-nanoseconds)])
      (let-values ([(wake stay)
                    (partition (lambda (t)
                                 (<= (gthread-wake-time t) now))
                               *sleep-queue*)])
        (set! *sleep-queue* stay)
        (for-each (lambda (t)
                    (gthread-wake-time-set! t #f)
                    (gthread-state-set! t 'ready)
                    (enqueue! *run-queue* t))
                  wake))))

  (define (any-runnable?)
    (or (not (queue-empty? *run-queue*))
        (pair? *sleep-queue*)
        ;; If all live threads are blocked (on channels), that's a deadlock
        (exists (lambda (t)
                  (let ([s (gthread-state t)])
                    (and (not (eq? s 'dead))
                         (not (eq? s 'blocked)))))
                *all-threads*)))

  (define (earliest-wake-ns)
    (and (pair? *sleep-queue*)
         (apply min (map gthread-wake-time *sleep-queue*))))

  (define (run-one-thread t)
    (set! *current* t)
    (gthread-state-set! t 'running)
    ((gthread-engine t)
     *fuel*
     ;; complete: engine finished
     (lambda (ticks result-pair)
       (set! *current* #f)
       (finish-thread! t (car result-pair) (cdr result-pair)))
     ;; expire: fuel exhausted or engine-block called
     (lambda (new-engine)
       (set! *current* #f)
       (gthread-engine-set! t new-engine)
       (when (eq? (gthread-state t) 'running)
         ;; Normal preemption — re-enqueue
         (gthread-state-set! t 'ready)
         (enqueue! *run-queue* t)))))

  (define (scheduler-loop)
    (let loop ()
      (wake-sleepers!)
      (cond
        [(not *running?*) (void)]
        [(not (queue-empty? *run-queue*))
         (run-one-thread (dequeue! *run-queue*))
         (loop)]
        [(any-runnable?)
         ;; Sleep briefly to avoid busy-wait
         (let ([next (earliest-wake-ns)])
           (if next
               (let ([delay-ns (- next (current-nanoseconds))])
                 (when (> delay-ns 0)
                   (sleep (make-time 'time-duration
                            (min delay-ns 50000000) 0))))
               (sleep (make-time 'time-duration 10000000 0))))
         (loop)]
        [else
         (set! *running?* #f)])))

  (define (reset-scheduler!)
    (set! *run-queue* (make-queue))
    (set! *sleep-queue* '())
    (set! *all-threads* '())
    (set! *current* #f)
    (set! *next-id* 0))

  (define (threads-start!)
    (set! *running?* #t)
    (add-exit-hook! threads-shutdown!)
    (scheduler-loop))

  (define (threads-shutdown!)
    (set! *running?* #f)
    (for-each (lambda (t)
                (unless (eq? (gthread-state t) 'dead)
                  (finish-thread! t 'ok (void))))
              (list-copy *all-threads*))
    (reset-scheduler!))

  (define (run-threads . thunks)
    (reset-scheduler!)
    (for-each (lambda (thunk) (spawn thunk)) thunks)
    (threads-start!))

) ;; end library
