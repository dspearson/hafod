#!chezscheme
;;; (hafod srfi-40) -- SRFI-40: A Library of Streams
;;; Reference: https://srfi.schemers.org/srfi-40/srfi-40.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-40)
  (export stream-null stream-cons stream? stream-null? stream-pair?
          stream-car stream-cdr
          stream stream-unfoldn stream-map stream-for-each
          stream-filter)
  (import (chezscheme))

  (define-record-type stream-type
    (fields (immutable tag) (immutable val))
    (nongenerative stream-type-hafod))

  (define stream-null (make-stream-type 'null '()))

  (define-syntax stream-cons
    (syntax-rules ()
      ((_ a b)
       (make-stream-type 'pair (cons (delay a) (delay b))))))

  (define (stream? x)
    (and (stream-type? x)
         (or (eq? (stream-type-tag x) 'null)
             (eq? (stream-type-tag x) 'pair))))

  (define (stream-null? x)
    (and (stream-type? x) (eq? (stream-type-tag x) 'null)))

  (define (stream-pair? x)
    (and (stream-type? x) (eq? (stream-type-tag x) 'pair)))

  (define (stream-car s)
    (unless (stream-pair? s)
      (error 'stream-car "not a stream pair" s))
    (force (car (stream-type-val s))))

  (define (stream-cdr s)
    (unless (stream-pair? s)
      (error 'stream-cdr "not a stream pair" s))
    (force (cdr (stream-type-val s))))

  (define-syntax stream
    (syntax-rules ()
      ((_) stream-null)
      ((_ x . rest) (stream-cons x (stream . rest)))))

  (define (stream-unfoldn gen seed n)
    ;; gen: seed -> (values seed result_0 ... result_{n-1})
    ;; Each result is either (list val) for a value or '() for end
    ;; Returns n streams
    (define (unfold-stream i gen seed)
      (let-values (((new-seed . results) (gen seed)))
        (let ((r (list-ref results i)))
          (if (null? r)
              stream-null
              (stream-cons (car r)
                           (unfold-stream i gen new-seed))))))
    (let loop ((i 0) (acc '()))
      (if (= i n)
          (apply values (reverse acc))
          (loop (+ i 1)
                (cons (unfold-stream i gen seed) acc)))))

  (define (stream-map proc . streams)
    (if (any1 stream-null? streams)
        stream-null
        (stream-cons
         (apply proc (map stream-car streams))
         (apply stream-map proc (map stream-cdr streams)))))

  (define (stream-for-each proc . streams)
    (unless (any1 stream-null? streams)
      (apply proc (map stream-car streams))
      (apply stream-for-each proc (map stream-cdr streams))))

  (define (stream-filter pred s)
    (cond
      ((stream-null? s) stream-null)
      ((pred (stream-car s))
       (stream-cons (stream-car s)
                    (stream-filter pred (stream-cdr s))))
      (else (stream-filter pred (stream-cdr s)))))

  (define (any1 pred lst)
    (and (pair? lst)
         (or (pred (car lst))
             (any1 pred (cdr lst))))))
