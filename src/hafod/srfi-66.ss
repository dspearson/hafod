#!chezscheme
;;; (hafod srfi-66) -- SRFI-66: Octet Vectors
;;; Reference: https://srfi.schemers.org/srfi-66/srfi-66.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Maps SRFI-66 names to Chez bytevector operations.

(library (hafod srfi-66)
  (export make-u8vector u8vector u8vector? u8vector-length
          u8vector-ref u8vector-set!
          u8vector->list list->u8vector
          u8vector-copy u8vector-copy!
          u8vector=? u8vector-compare)
  (import (chezscheme))

  (define make-u8vector make-bytevector)
  (define (u8vector . vals) (apply bytevector vals))
  (define u8vector? bytevector?)
  (define u8vector-length bytevector-length)
  (define u8vector-ref bytevector-u8-ref)
  (define u8vector-set! bytevector-u8-set!)

  (define (u8vector->list bv)
    (let loop ((i (- (bytevector-length bv) 1)) (acc '()))
      (if (< i 0) acc
          (loop (- i 1) (cons (bytevector-u8-ref bv i) acc)))))

  (define (list->u8vector lst)
    (let* ((n (length lst))
           (bv (make-bytevector n)))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-u8-set! bv i (car l))
          (loop (cdr l) (+ i 1))))
      bv))

  (define (u8vector-copy bv)
    (bytevector-copy bv))

  (define (u8vector-copy! src src-start dest dest-start n)
    (bytevector-copy! src src-start dest dest-start n))

  (define (u8vector=? a b)
    (bytevector=? a b))

  (define (u8vector-compare a b)
    (let ((la (bytevector-length a))
          (lb (bytevector-length b)))
      (let ((len (min la lb)))
        (let loop ((i 0))
          (cond
            ((= i len) (cond ((< la lb) -1) ((> la lb) 1) (else 0)))
            ((< (bytevector-u8-ref a i) (bytevector-u8-ref b i)) -1)
            ((> (bytevector-u8-ref a i) (bytevector-u8-ref b i))  1)
            (else (loop (+ i 1)))))))))
