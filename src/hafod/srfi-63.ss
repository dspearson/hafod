#!chezscheme
;;; (hafod srfi-63) -- SRFI-63: Homogeneous and Heterogeneous Arrays
;;; Reference: https://srfi.schemers.org/srfi-63/srfi-63.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Built on top of (hafod srfi-25) with additional SRFI-63 API.

(library (hafod srfi-63)
  (export make-array array? array-rank array-ref array-set!
          array-dimensions list->array array->list
          make-shared-array)
  (import (chezscheme))

  (define-record-type arr63
    (fields store dims rank)
    (nongenerative arr63-hafod))

  (define (array? x) (arr63? x))
  (define (array-rank a) (arr63-rank a))

  (define (array-dimensions a)
    (vector->list (arr63-dims a)))

  (define (make-array prototype . dims)
    (let* ((rank (length dims))
           (size (apply * dims))
           (fill (if (and (arr63? prototype) (> (vector-length (arr63-store prototype)) 0))
                     (vector-ref (arr63-store prototype) 0)
                     (void)))
           (store (make-vector size fill)))
      (make-arr63 store (list->vector dims) rank)))

  (define (flat-index a indices)
    (let ((dims (arr63-dims a)))
      (let loop ((idx 0) (rest indices) (d 0))
        (if (null? rest) idx
            (loop (+ (* idx (vector-ref dims d)) (car rest))
                  (cdr rest) (+ d 1))))))

  (define (array-ref a . indices)
    (vector-ref (arr63-store a) (flat-index a indices)))

  (define (array-set! a val . indices)
    (vector-set! (arr63-store a) (flat-index a indices) val))

  (define (list->array rank lst . opt)
    (let* ((dims (if (pair? opt) (car opt)
                     (if (= rank 1) (list (length lst))
                         (error 'list->array "dimensions required for rank > 1"))))
           (a (apply make-array (make-arr63 '#() '#() 0) dims))
           (store (arr63-store a)))
      (let loop ((i 0) (l lst))
        (unless (null? l)
          (vector-set! store i (car l))
          (loop (+ i 1) (cdr l))))
      a))

  (define (array->list a)
    (vector->list (arr63-store a)))

  (define (make-shared-array a mapper . dims)
    ;; Simplified: create a new array that maps indices through mapper
    ;; In a full implementation this would share storage
    (let* ((new-a (apply make-array (make-arr63 '#() '#() 0) dims))
           (size (vector-length (arr63-store new-a))))
      (let loop ((i 0))
        (when (< i size)
          ;; Compute indices from flat index
          (let ((indices (flat->indices (list->vector dims) i)))
            (let ((mapped (apply mapper indices)))
              (vector-set! (arr63-store new-a) i
                           (apply array-ref a mapped))))
          (loop (+ i 1))))
      new-a))

  (define (flat->indices dims flat)
    (let ((rank (vector-length dims)))
      (let loop ((d (- rank 1)) (f flat) (acc '()))
        (if (< d 0) acc
            (let ((dim (vector-ref dims d)))
              (loop (- d 1) (div f dim) (cons (mod f dim) acc))))))))
