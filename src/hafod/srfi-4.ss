#!chezscheme
;;; (hafod srfi-4) -- SRFI-4: Homogeneous numeric vector datatypes
;;; Reference: https://srfi.schemers.org/srfi-4/srfi-4.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Maps SRFI-4 u8vector operations to Chez bytevector operations.
;;; Only u8vector is fully supported; other types are provided as
;;; thin wrappers over bytevectors with appropriate accessors.

(library (hafod srfi-4)
  (export
    ;; u8vector
    make-u8vector u8vector u8vector? u8vector-length u8vector-ref
    u8vector-set! u8vector->list list->u8vector
    ;; s8vector
    make-s8vector s8vector s8vector? s8vector-length s8vector-ref
    s8vector-set! s8vector->list list->s8vector
    ;; u16vector
    make-u16vector u16vector u16vector? u16vector-length u16vector-ref
    u16vector-set! u16vector->list list->u16vector
    ;; s16vector
    make-s16vector s16vector s16vector? s16vector-length s16vector-ref
    s16vector-set! s16vector->list list->s16vector
    ;; u32vector
    make-u32vector u32vector u32vector? u32vector-length u32vector-ref
    u32vector-set! u32vector->list list->u32vector
    ;; s32vector
    make-s32vector s32vector s32vector? s32vector-length s32vector-ref
    s32vector-set! s32vector->list list->s32vector)
  (import (chezscheme))

  ;; u8vector: direct bytevector mapping
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

  ;; s8vector: signed 8-bit
  (define make-s8vector make-bytevector)
  (define (s8vector . vals)
    (let ((bv (make-bytevector (length vals))))
      (let loop ((v vals) (i 0))
        (unless (null? v)
          (bytevector-s8-set! bv i (car v))
          (loop (cdr v) (+ i 1))))
      bv))
  (define s8vector? bytevector?)
  (define s8vector-length bytevector-length)
  (define s8vector-ref bytevector-s8-ref)
  (define s8vector-set! bytevector-s8-set!)
  (define (s8vector->list bv)
    (let loop ((i (- (bytevector-length bv) 1)) (acc '()))
      (if (< i 0) acc
          (loop (- i 1) (cons (bytevector-s8-ref bv i) acc)))))
  (define (list->s8vector lst)
    (let* ((n (length lst))
           (bv (make-bytevector n)))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-s8-set! bv i (car l))
          (loop (cdr l) (+ i 1))))
      bv))

  ;; u16vector
  (define (make-u16vector n . fill)
    (let ((bv (make-bytevector (* n 2) 0)))
      (when (pair? fill)
        (do ((i 0 (+ i 1))) ((= i n))
          (bytevector-u16-native-set! bv (* i 2) (car fill))))
      bv))
  (define (u16vector . vals)
    (let ((bv (make-bytevector (* (length vals) 2))))
      (let loop ((v vals) (i 0))
        (unless (null? v)
          (bytevector-u16-native-set! bv (* i 2) (car v))
          (loop (cdr v) (+ i 1))))
      bv))
  (define u16vector? bytevector?)
  (define (u16vector-length bv) (div (bytevector-length bv) 2))
  (define (u16vector-ref bv i) (bytevector-u16-native-ref bv (* i 2)))
  (define (u16vector-set! bv i val) (bytevector-u16-native-set! bv (* i 2) val))
  (define (u16vector->list bv)
    (let ((n (u16vector-length bv)))
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1) (cons (u16vector-ref bv i) acc))))))
  (define (list->u16vector lst)
    (let* ((n (length lst))
           (bv (make-bytevector (* n 2))))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-u16-native-set! bv (* i 2) (car l))
          (loop (cdr l) (+ i 1))))
      bv))

  ;; s16vector
  (define (make-s16vector n . fill)
    (let ((bv (make-bytevector (* n 2) 0)))
      (when (pair? fill)
        (do ((i 0 (+ i 1))) ((= i n))
          (bytevector-s16-native-set! bv (* i 2) (car fill))))
      bv))
  (define (s16vector . vals)
    (let ((bv (make-bytevector (* (length vals) 2))))
      (let loop ((v vals) (i 0))
        (unless (null? v)
          (bytevector-s16-native-set! bv (* i 2) (car v))
          (loop (cdr v) (+ i 1))))
      bv))
  (define s16vector? bytevector?)
  (define (s16vector-length bv) (div (bytevector-length bv) 2))
  (define (s16vector-ref bv i) (bytevector-s16-native-ref bv (* i 2)))
  (define (s16vector-set! bv i val) (bytevector-s16-native-set! bv (* i 2) val))
  (define (s16vector->list bv)
    (let ((n (s16vector-length bv)))
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1) (cons (s16vector-ref bv i) acc))))))
  (define (list->s16vector lst)
    (let* ((n (length lst))
           (bv (make-bytevector (* n 2))))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-s16-native-set! bv (* i 2) (car l))
          (loop (cdr l) (+ i 1))))
      bv))

  ;; u32vector
  (define (make-u32vector n . fill)
    (let ((bv (make-bytevector (* n 4) 0)))
      (when (pair? fill)
        (do ((i 0 (+ i 1))) ((= i n))
          (bytevector-u32-native-set! bv (* i 4) (car fill))))
      bv))
  (define (u32vector . vals)
    (let ((bv (make-bytevector (* (length vals) 4))))
      (let loop ((v vals) (i 0))
        (unless (null? v)
          (bytevector-u32-native-set! bv (* i 4) (car v))
          (loop (cdr v) (+ i 1))))
      bv))
  (define u32vector? bytevector?)
  (define (u32vector-length bv) (div (bytevector-length bv) 4))
  (define (u32vector-ref bv i) (bytevector-u32-native-ref bv (* i 4)))
  (define (u32vector-set! bv i val) (bytevector-u32-native-set! bv (* i 4) val))
  (define (u32vector->list bv)
    (let ((n (u32vector-length bv)))
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1) (cons (u32vector-ref bv i) acc))))))
  (define (list->u32vector lst)
    (let* ((n (length lst))
           (bv (make-bytevector (* n 4))))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-u32-native-set! bv (* i 4) (car l))
          (loop (cdr l) (+ i 1))))
      bv))

  ;; s32vector
  (define (make-s32vector n . fill)
    (let ((bv (make-bytevector (* n 4) 0)))
      (when (pair? fill)
        (do ((i 0 (+ i 1))) ((= i n))
          (bytevector-s32-native-set! bv (* i 4) (car fill))))
      bv))
  (define (s32vector . vals)
    (let ((bv (make-bytevector (* (length vals) 4))))
      (let loop ((v vals) (i 0))
        (unless (null? v)
          (bytevector-s32-native-set! bv (* i 4) (car v))
          (loop (cdr v) (+ i 1))))
      bv))
  (define s32vector? bytevector?)
  (define (s32vector-length bv) (div (bytevector-length bv) 4))
  (define (s32vector-ref bv i) (bytevector-s32-native-ref bv (* i 4)))
  (define (s32vector-set! bv i val) (bytevector-s32-native-set! bv (* i 4) val))
  (define (s32vector->list bv)
    (let ((n (s32vector-length bv)))
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1) (cons (s32vector-ref bv i) acc))))))
  (define (list->s32vector lst)
    (let* ((n (length lst))
           (bv (make-bytevector (* n 4))))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-s32-native-set! bv (* i 4) (car l))
          (loop (cdr l) (+ i 1))))
      bv)))
