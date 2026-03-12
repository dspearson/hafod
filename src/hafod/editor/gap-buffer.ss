;;; (hafod editor gap-buffer) -- Gap buffer data structure for line editor
;;; A gap buffer stores text as two contiguous regions separated by a gap
;;; at the cursor position. This gives O(1) insert and delete at cursor.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor gap-buffer)
  (export make-gap-buffer gap-buffer-insert! gap-buffer-insert-string!
          gap-buffer-delete-forward!
          gap-buffer-delete-backward! gap-buffer-move-cursor!
          gap-buffer-cursor-pos gap-buffer->string
          gap-buffer-before-string gap-buffer-after-string
          gap-buffer-length gap-buffer-char-at
          gap-buffer-set-from-string! gap-buffer-clear!
          gap-buffer-delete-word-forward! gap-buffer-delete-word-backward!
          gap-buffer-kill-to-end!)
  (import (chezscheme))

  ;; Gap buffer record: buf is a mutable char vector, gap-start and gap-end
  ;; delimit the gap. Text before gap: buf[0..gap-start), after: buf[gap-end..capacity).
  (define-record-type gap-buffer
    (nongenerative)
    (fields (mutable buf) (mutable gap-start) (mutable gap-end))
    (protocol
      (lambda (new)
        (case-lambda
          [() (new (make-vector 64 #\nul) 0 64)]
          [(cap) (new (make-vector cap #\nul) 0 cap)]))))

  (define (gap-size gb)
    (fx- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb)))

  (define (capacity gb)
    (vector-length (gap-buffer-buf gb)))

  (define (gap-buffer-length gb)
    (fx- (capacity gb) (gap-size gb)))

  (define (gap-buffer-cursor-pos gb)
    (gap-buffer-gap-start gb))

  ;; Grow the buffer when gap is exhausted.
  (define (grow! gb)
    (let* ([old-buf (gap-buffer-buf gb)]
           [old-cap (vector-length old-buf)]
           [new-cap (fx* old-cap 2)]
           [new-buf (make-vector new-cap #\nul)]
           [gs (gap-buffer-gap-start gb)]
           [ge (gap-buffer-gap-end gb)]
           [after-len (fx- old-cap ge)])
      ;; Copy before-gap region
      (do ([i 0 (fx+ i 1)])
          ((fx= i gs))
        (vector-set! new-buf i (vector-ref old-buf i)))
      ;; Copy after-gap region to end of new buffer
      (let ([new-ge (fx- new-cap after-len)])
        (do ([i 0 (fx+ i 1)])
            ((fx= i after-len))
          (vector-set! new-buf (fx+ new-ge i) (vector-ref old-buf (fx+ ge i))))
        (gap-buffer-buf-set! gb new-buf)
        (gap-buffer-gap-end-set! gb new-ge))))

  (define (gap-buffer-insert! gb ch)
    (when (fx= (gap-size gb) 0)
      (grow! gb))
    (let ([gs (gap-buffer-gap-start gb)])
      (vector-set! (gap-buffer-buf gb) gs ch)
      (gap-buffer-gap-start-set! gb (fx+ gs 1))))

  (define (gap-buffer-insert-string! gb s)
    (let ([len (string-length s)])
      (do ([i 0 (fx+ i 1)])
          ((fx= i len))
        (gap-buffer-insert! gb (string-ref s i)))))

  (define (gap-buffer-delete-forward! gb)
    (let ([ge (gap-buffer-gap-end gb)])
      (when (fx< ge (capacity gb))
        (gap-buffer-gap-end-set! gb (fx+ ge 1)))))

  (define (gap-buffer-delete-backward! gb)
    (let ([gs (gap-buffer-gap-start gb)])
      (when (fx> gs 0)
        (gap-buffer-gap-start-set! gb (fx- gs 1)))))

  (define (gap-buffer-move-cursor! gb n)
    (let* ([gs (gap-buffer-gap-start gb)]
           [ge (gap-buffer-gap-end gb)]
           [buf (gap-buffer-buf gb)]
           [cap (vector-length buf)])
      (cond
        [(fx> n 0)
         ;; Move right: copy chars from after-gap to before-gap
         (let ([actual (fxmin n (fx- cap ge))])
           (do ([i 0 (fx+ i 1)])
               ((fx= i actual))
             (vector-set! buf (fx+ gs i) (vector-ref buf (fx+ ge i))))
           (gap-buffer-gap-start-set! gb (fx+ gs actual))
           (gap-buffer-gap-end-set! gb (fx+ ge actual)))]
        [(fx< n 0)
         ;; Move left: copy chars from before-gap to after-gap
         (let ([actual (fxmin (fx- n) gs)])
           (do ([i 0 (fx+ i 1)])
               ((fx= i actual))
             (let* ([src-idx (fx- gs 1 i)]
                    [dst-idx (fx- ge 1 i)])
               (vector-set! buf dst-idx (vector-ref buf src-idx))))
           (gap-buffer-gap-start-set! gb (fx- gs actual))
           (gap-buffer-gap-end-set! gb (fx- ge actual)))])))

  (define (gap-buffer->string gb)
    (let* ([gs (gap-buffer-gap-start gb)]
           [ge (gap-buffer-gap-end gb)]
           [buf (gap-buffer-buf gb)]
           [cap (vector-length buf)]
           [len (fx- cap (fx- ge gs))]
           [s (make-string len)])
      (do ([i 0 (fx+ i 1)])
          ((fx= i gs))
        (string-set! s i (vector-ref buf i)))
      (let ([after-len (fx- cap ge)])
        (do ([i 0 (fx+ i 1)])
            ((fx= i after-len))
          (string-set! s (fx+ gs i) (vector-ref buf (fx+ ge i)))))
      s))

  (define (gap-buffer-before-string gb)
    (let* ([gs (gap-buffer-gap-start gb)]
           [buf (gap-buffer-buf gb)]
           [s (make-string gs)])
      (do ([i 0 (fx+ i 1)])
          ((fx= i gs))
        (string-set! s i (vector-ref buf i)))
      s))

  (define (gap-buffer-after-string gb)
    (let* ([ge (gap-buffer-gap-end gb)]
           [buf (gap-buffer-buf gb)]
           [cap (vector-length buf)]
           [len (fx- cap ge)]
           [s (make-string len)])
      (do ([i 0 (fx+ i 1)])
          ((fx= i len))
        (string-set! s i (vector-ref buf (fx+ ge i))))
      s))

  (define (gap-buffer-char-at gb idx)
    (let* ([gs (gap-buffer-gap-start gb)]
           [ge (gap-buffer-gap-end gb)]
           [buf (gap-buffer-buf gb)])
      (if (fx< idx gs)
          (vector-ref buf idx)
          (vector-ref buf (fx+ ge (fx- idx gs))))))

  (define (gap-buffer-set-from-string! gb s)
    (let* ([len (string-length s)]
           [cap (fxmax 64 (fx* len 2))]
           [buf (make-vector cap #\nul)])
      (do ([i 0 (fx+ i 1)])
          ((fx= i len))
        (vector-set! buf i (string-ref s i)))
      (gap-buffer-buf-set! gb buf)
      (gap-buffer-gap-start-set! gb len)
      (gap-buffer-gap-end-set! gb cap)))

  (define (gap-buffer-clear! gb)
    (let ([cap (capacity gb)])
      (gap-buffer-gap-start-set! gb 0)
      (gap-buffer-gap-end-set! gb cap)))

  ;; Word operations helpers
  (define (char-word-constituent? ch)
    (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_) (char=? ch #\-)))

  (define (gap-buffer-delete-word-forward! gb)
    (let* ([ge (gap-buffer-gap-end gb)]
           [buf (gap-buffer-buf gb)]
           [cap (vector-length buf)]
           [start ge])
      ;; Skip whitespace first
      (let skip-ws ([pos start])
        (cond
          [(fx>= pos cap) (gap-buffer-gap-end-set! gb cap)]
          [(char-whitespace? (vector-ref buf pos))
           (skip-ws (fx+ pos 1))]
          [else
           ;; Then skip word chars
           (let skip-word ([pos pos])
             (cond
               [(fx>= pos cap) (gap-buffer-gap-end-set! gb cap)]
               [(char-word-constituent? (vector-ref buf pos))
                (skip-word (fx+ pos 1))]
               [else (gap-buffer-gap-end-set! gb pos)]))]))))

  (define (gap-buffer-delete-word-backward! gb)
    (let* ([gs (gap-buffer-gap-start gb)]
           [buf (gap-buffer-buf gb)]
           [start (fx- gs 1)])
      (when (fx>= start 0)
        ;; Skip whitespace backwards
        (let skip-ws ([pos start])
          (cond
            [(fx< pos 0) (gap-buffer-gap-start-set! gb 0)]
            [(char-whitespace? (vector-ref buf pos))
             (skip-ws (fx- pos 1))]
            [else
             ;; Then skip word chars backwards
             (let skip-word ([pos pos])
               (cond
                 [(fx< pos 0) (gap-buffer-gap-start-set! gb 0)]
                 [(char-word-constituent? (vector-ref buf pos))
                  (skip-word (fx- pos 1))]
                 [else (gap-buffer-gap-start-set! gb (fx+ pos 1))]))])))))

  (define (gap-buffer-kill-to-end! gb)
    (let* ([ge (gap-buffer-gap-end gb)]
           [buf (gap-buffer-buf gb)]
           [cap (vector-length buf)]
           [len (fx- cap ge)]
           [s (make-string len)])
      (do ([i 0 (fx+ i 1)])
          ((fx= i len))
        (string-set! s i (vector-ref buf (fx+ ge i))))
      (gap-buffer-gap-end-set! gb cap)
      s))

) ; end library
