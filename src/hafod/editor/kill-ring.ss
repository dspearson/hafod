;;; (hafod editor kill-ring) -- Kill ring for cut/copy/yank operations
;;; A circular buffer of killed text strings supporting yank and rotate.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor kill-ring)
  (export make-kill-ring kill-ring-push! kill-ring-yank kill-ring-rotate!)
  (import (chezscheme))

  ;; Kill ring record: ring is a vector of strings (or #f for empty slots),
  ;; head is the index of the most recently pushed item,
  ;; count is the number of items in the ring,
  ;; yank-offset is how far back from head we are (for rotate).
  (define-record-type kill-ring
    (nongenerative)
    (fields (mutable ring)
            (mutable head)
            (mutable count)
            (mutable yank-offset)
            max-size)
    (protocol
      (lambda (new)
        (case-lambda
          [() (new (make-vector 30 #f) 0 0 0 30)]
          [(n) (new (make-vector n #f) 0 0 0 n)]))))

  (define (kill-ring-push! kr text)
    (let* ([sz (kill-ring-max-size kr)]
           [ring (kill-ring-ring kr)]
           [new-head (fxmod (fx+ (kill-ring-head kr)
                                 (if (fx= (kill-ring-count kr) 0) 0 1))
                            sz)])
      (vector-set! ring new-head text)
      (kill-ring-head-set! kr new-head)
      (kill-ring-count-set! kr (fxmin (fx+ (kill-ring-count kr) 1) sz))
      (kill-ring-yank-offset-set! kr 0)))

  (define (kill-ring-yank kr)
    (if (fx= (kill-ring-count kr) 0)
        #f
        (let* ([sz (kill-ring-max-size kr)]
               [idx (fxmod (fx+ sz (fx- (kill-ring-head kr)
                                        (kill-ring-yank-offset kr)))
                           sz)])
          (vector-ref (kill-ring-ring kr) idx))))

  (define (kill-ring-rotate! kr)
    (when (fx> (kill-ring-count kr) 0)
      (let ([new-offset (fxmod (fx+ (kill-ring-yank-offset kr) 1)
                               (kill-ring-count kr))])
        (kill-ring-yank-offset-set! kr new-offset))))

) ; end library
