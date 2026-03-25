#!chezscheme
;;; (hafod srfi-25) -- SRFI-25: Multi-dimensional Array Primitives
;;; Reference: https://srfi.schemers.org/srfi-25/srfi-25.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-25)
  (export make-array array? array-rank array-ref array-set!
          array-start array-end shape array)
  (import (chezscheme))

  ;; Shape: vector of (lo hi) pairs, stored flat: #(lo0 hi0 lo1 hi1 ...)
  (define (shape . bounds)
    (let ((v (list->vector bounds)))
      (unless (even? (vector-length v))
        (error 'shape "bounds must come in pairs"))
      v))

  ;; Internal array record
  (define-record-type arr
    (fields shape-vec store rank dims offsets)
    (nongenerative arr-srfi25-hafod))

  (define (array? x) (arr? x))
  (define (array-rank a) (arr-rank a))

  (define (array-start a dim)
    (vector-ref (arr-shape-vec a) (* dim 2)))

  (define (array-end a dim)
    (vector-ref (arr-shape-vec a) (+ (* dim 2) 1)))

  (define (make-array shp . fill)
    (let* ((rank (div (vector-length shp) 2))
           (dims (let loop ((d 0) (acc '()))
                   (if (= d rank) (reverse acc)
                       (loop (+ d 1)
                             (cons (- (vector-ref shp (+ (* d 2) 1))
                                      (vector-ref shp (* d 2)))
                                   acc)))))
           (size (apply * dims))
           (store (make-vector size (if (pair? fill) (car fill) (void))))
           ;; Row-major offsets
           (offsets (let loop ((d (- rank 1)) (stride 1) (acc '()))
                      (if (< d 0) acc
                          (loop (- d 1) (* stride (list-ref dims d))
                                (cons stride acc))))))
      (make-arr shp store rank (list->vector dims) (list->vector offsets))))

  (define (flat-index a indices)
    (let ((rank (arr-rank a))
          (shp (arr-shape-vec a))
          (offsets (arr-offsets a)))
      (let loop ((d 0) (idx 0) (rest indices))
        (if (= d rank) idx
            (loop (+ d 1)
                  (+ idx (* (- (car rest) (vector-ref shp (* d 2)))
                            (vector-ref offsets d)))
                  (cdr rest))))))

  (define (array-ref a . indices)
    (vector-ref (arr-store a) (flat-index a indices)))

  (define (array-set! a val . indices)
    (vector-set! (arr-store a) (flat-index a indices) val))

  ;; Construct array from shape and elements (row-major)
  (define (array shp . elts)
    (let ((a (make-array shp)))
      (let loop ((i 0) (es elts))
        (unless (null? es)
          (vector-set! (arr-store a) i (car es))
          (loop (+ i 1) (cdr es))))
      a)))
