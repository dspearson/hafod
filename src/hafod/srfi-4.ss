#!chezscheme
;;; (hafod srfi-4) -- SRFI-4: Homogeneous numeric vector datatypes
;;; Reference: https://srfi.schemers.org/srfi-4/srfi-4.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Maps the SRFI-4 homogeneous-vector operations to Chez bytevector operations.
;;; All ten families are fully supported -- u8/s8, u16/s16, u32/s32, u64/s64
;;; (integer) and f32/f64 (IEEE single/double) -- each a thin, native-endianness
;;; wrapper over a bytevector with the appropriate accessor. The type predicates
;;; are bytevector-backed and therefore NON-DISJOINT (e.g. u8vector? answers #t
;;; for any bytevector); this is a deliberate, documented divergence.

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
    s32vector-set! s32vector->list list->s32vector
    ;; u64vector
    make-u64vector u64vector u64vector? u64vector-length u64vector-ref
    u64vector-set! u64vector->list list->u64vector
    ;; s64vector
    make-s64vector s64vector s64vector? s64vector-length s64vector-ref
    s64vector-set! s64vector->list list->s64vector
    ;; f32vector
    make-f32vector f32vector f32vector? f32vector-length f32vector-ref
    f32vector-set! f32vector->list list->f32vector
    ;; f64vector
    make-f64vector f64vector f64vector? f64vector-length f64vector-ref
    f64vector-set! f64vector->list list->f64vector)
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
      bv))

  ;; u64vector
  (define (make-u64vector n . fill)
    (let ((bv (make-bytevector (* n 8) 0)))
      (when (pair? fill)
        (do ((i 0 (+ i 1))) ((= i n))
          (bytevector-u64-native-set! bv (* i 8) (car fill))))
      bv))
  (define (u64vector . vals)
    (let ((bv (make-bytevector (* (length vals) 8))))
      (let loop ((v vals) (i 0))
        (unless (null? v)
          (bytevector-u64-native-set! bv (* i 8) (car v))
          (loop (cdr v) (+ i 1))))
      bv))
  (define u64vector? bytevector?)
  (define (u64vector-length bv) (div (bytevector-length bv) 8))
  (define (u64vector-ref bv i) (bytevector-u64-native-ref bv (* i 8)))
  (define (u64vector-set! bv i val) (bytevector-u64-native-set! bv (* i 8) val))
  (define (u64vector->list bv)
    (let ((n (u64vector-length bv)))
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1) (cons (u64vector-ref bv i) acc))))))
  (define (list->u64vector lst)
    (let* ((n (length lst))
           (bv (make-bytevector (* n 8))))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-u64-native-set! bv (* i 8) (car l))
          (loop (cdr l) (+ i 1))))
      bv))

  ;; s64vector
  (define (make-s64vector n . fill)
    (let ((bv (make-bytevector (* n 8) 0)))
      (when (pair? fill)
        (do ((i 0 (+ i 1))) ((= i n))
          (bytevector-s64-native-set! bv (* i 8) (car fill))))
      bv))
  (define (s64vector . vals)
    (let ((bv (make-bytevector (* (length vals) 8))))
      (let loop ((v vals) (i 0))
        (unless (null? v)
          (bytevector-s64-native-set! bv (* i 8) (car v))
          (loop (cdr v) (+ i 1))))
      bv))
  (define s64vector? bytevector?)
  (define (s64vector-length bv) (div (bytevector-length bv) 8))
  (define (s64vector-ref bv i) (bytevector-s64-native-ref bv (* i 8)))
  (define (s64vector-set! bv i val) (bytevector-s64-native-set! bv (* i 8) val))
  (define (s64vector->list bv)
    (let ((n (s64vector-length bv)))
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1) (cons (s64vector-ref bv i) acc))))))
  (define (list->s64vector lst)
    (let* ((n (length lst))
           (bv (make-bytevector (* n 8))))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-s64-native-set! bv (* i 8) (car l))
          (loop (cdr l) (+ i 1))))
      bv))

  ;; f32vector: IEEE single-precision float, native endianness, stride 4
  (define (make-f32vector n . fill)
    (let ((bv (make-bytevector (* n 4) 0)))
      (when (pair? fill)
        (do ((i 0 (+ i 1))) ((= i n))
          (bytevector-ieee-single-native-set! bv (* i 4) (car fill))))
      bv))
  (define (f32vector . vals)
    (let ((bv (make-bytevector (* (length vals) 4))))
      (let loop ((v vals) (i 0))
        (unless (null? v)
          (bytevector-ieee-single-native-set! bv (* i 4) (car v))
          (loop (cdr v) (+ i 1))))
      bv))
  (define f32vector? bytevector?)
  (define (f32vector-length bv) (div (bytevector-length bv) 4))
  (define (f32vector-ref bv i) (bytevector-ieee-single-native-ref bv (* i 4)))
  (define (f32vector-set! bv i val) (bytevector-ieee-single-native-set! bv (* i 4) val))
  (define (f32vector->list bv)
    (let ((n (f32vector-length bv)))
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1) (cons (f32vector-ref bv i) acc))))))
  (define (list->f32vector lst)
    (let* ((n (length lst))
           (bv (make-bytevector (* n 4))))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-ieee-single-native-set! bv (* i 4) (car l))
          (loop (cdr l) (+ i 1))))
      bv))

  ;; f64vector: IEEE double-precision float, native endianness, stride 8
  (define (make-f64vector n . fill)
    (let ((bv (make-bytevector (* n 8) 0)))
      (when (pair? fill)
        (do ((i 0 (+ i 1))) ((= i n))
          (bytevector-ieee-double-native-set! bv (* i 8) (car fill))))
      bv))
  (define (f64vector . vals)
    (let ((bv (make-bytevector (* (length vals) 8))))
      (let loop ((v vals) (i 0))
        (unless (null? v)
          (bytevector-ieee-double-native-set! bv (* i 8) (car v))
          (loop (cdr v) (+ i 1))))
      bv))
  (define f64vector? bytevector?)
  (define (f64vector-length bv) (div (bytevector-length bv) 8))
  (define (f64vector-ref bv i) (bytevector-ieee-double-native-ref bv (* i 8)))
  (define (f64vector-set! bv i val) (bytevector-ieee-double-native-set! bv (* i 8) val))
  (define (f64vector->list bv)
    (let ((n (f64vector-length bv)))
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0) acc
            (loop (- i 1) (cons (f64vector-ref bv i) acc))))))
  (define (list->f64vector lst)
    (let* ((n (length lst))
           (bv (make-bytevector (* n 8))))
      (let loop ((l lst) (i 0))
        (unless (null? l)
          (bytevector-ieee-double-native-set! bv (* i 8) (car l))
          (loop (cdr l) (+ i 1))))
      bv)))
