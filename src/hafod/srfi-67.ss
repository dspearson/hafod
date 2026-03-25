#!chezscheme
;;; (hafod srfi-67) -- SRFI-67: Compare Procedures
;;; Reference: https://srfi.schemers.org/srfi-67/srfi-67.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-67)
  (export
    ;; Comparison predicates from a compare procedure
    =? <? >? <=? >=?
    not=? </<=? </<? <=/<?
    ;; Constructing compare procedures
    boolean-compare char-compare char-compare-ci
    string-compare string-compare-ci
    number-compare integer-compare rational-compare real-compare complex-compare
    pair-compare list-compare vector-compare
    ;; Combining
    refine-compare select-compare cond-compare
    ;; Using
    if3 if=? if<? if>?
    ;; Min/max
    pairwise-not=?
    compare-by< compare-by> compare-by<= compare-by>=
    compare-by=)
  (import (chezscheme))

  ;; Three-way dispatch
  (define-syntax if3
    (syntax-rules ()
      ((_ c less equal greater)
       (let ((v c))
         (cond ((< v 0) less) ((= v 0) equal) (else greater))))))

  (define-syntax if=?
    (syntax-rules ()
      ((_ c then) (if=? c then (void)))
      ((_ c then else) (if (zero? c) then else))))

  (define-syntax if<?
    (syntax-rules ()
      ((_ c then) (if<? c then (void)))
      ((_ c then else) (if (< c 0) then else))))

  (define-syntax if>?
    (syntax-rules ()
      ((_ c then) (if>? c then (void)))
      ((_ c then else) (if (> c 0) then else))))

  ;; Compare predicates
  (define (=? compare a b) (zero? (compare a b)))
  (define (<? compare a b) (negative? (compare a b)))
  (define (>? compare a b) (positive? (compare a b)))
  (define (<=? compare a b) (not (positive? (compare a b))))
  (define (>=? compare a b) (not (negative? (compare a b))))
  (define (not=? compare a b) (not (zero? (compare a b))))

  (define (</<=? compare a b c)
    (and (<? compare a b) (<=? compare b c)))
  (define (</<? compare a b c)
    (and (<? compare a b) (<? compare b c)))
  (define (<=/<? compare a b c)
    (and (<=? compare a b) (<? compare b c)))

  ;; Refine/select/cond
  (define-syntax refine-compare
    (syntax-rules ()
      ((_ c) c)
      ((_ c . rest)
       (let ((v c))
         (if (zero? v) (refine-compare . rest) v)))))

  (define-syntax select-compare
    (syntax-rules ()
      ((_ expr clause ...)
       (let ((val expr))
         (select-compare-helper val clause ...)))))

  (define-syntax select-compare-helper
    (syntax-rules (else)
      ((_ val (else c ...)) (refine-compare c ...))
      ((_ val) 0)
      ((_ val (pred? c ...) . rest)
       (if (pred? val)
           (refine-compare c ...)
           (select-compare-helper val . rest)))))

  (define-syntax cond-compare
    (syntax-rules (else)
      ((_ (else c ...)) (refine-compare c ...))
      ((_ ) 0)
      ((_ (test c ...) . rest)
       (if test (refine-compare c ...) (cond-compare . rest)))))

  ;; Standard compare procedures
  (define (boolean-compare a b)
    (cond ((and (not a) b) -1) ((and a (not b)) 1) (else 0)))

  (define (char-compare a b)
    (cond ((char<? a b) -1) ((char>? a b) 1) (else 0)))

  (define (char-compare-ci a b)
    (char-compare (char-downcase a) (char-downcase b)))

  (define (string-compare a b)
    (let ((la (string-length a)) (lb (string-length b)))
      (let ((len (min la lb)))
        (let loop ((i 0))
          (cond
            ((= i len) (cond ((< la lb) -1) ((> la lb) 1) (else 0)))
            ((char<? (string-ref a i) (string-ref b i)) -1)
            ((char>? (string-ref a i) (string-ref b i))  1)
            (else (loop (+ i 1))))))))

  (define (string-compare-ci a b)
    (string-compare (string-downcase a) (string-downcase b)))

  (define (number-compare a b)
    (cond ((< a b) -1) ((> a b) 1) (else 0)))

  (define integer-compare number-compare)
  (define rational-compare number-compare)
  (define real-compare number-compare)

  (define (complex-compare a b)
    (refine-compare
     (real-compare (real-part a) (real-part b))
     (real-compare (imag-part a) (imag-part b))))

  (define (pair-compare car-cmp cdr-cmp a b)
    (refine-compare
     (car-cmp (car a) (car b))
     (cdr-cmp (cdr a) (cdr b))))

  (define (list-compare cmp a b)
    (cond
      ((and (null? a) (null? b)) 0)
      ((null? a) -1)
      ((null? b)  1)
      (else (refine-compare
             (cmp (car a) (car b))
             (list-compare cmp (cdr a) (cdr b))))))

  (define (vector-compare cmp a b)
    (let ((la (vector-length a)) (lb (vector-length b)))
      (let ((len (min la lb)))
        (let loop ((i 0))
          (cond
            ((= i len) (number-compare la lb))
            (else
             (let ((c (cmp (vector-ref a i) (vector-ref b i))))
               (if (zero? c) (loop (+ i 1)) c))))))))

  (define (pairwise-not=? compare . vals)
    (let loop ((vs vals))
      (or (null? vs)
          (null? (cdr vs))
          (and (let inner ((rest (cdr vs)))
                 (or (null? rest)
                     (and (not=? compare (car vs) (car rest))
                          (inner (cdr rest)))))
               (loop (cdr vs))))))

  (define (compare-by< lt a b)
    (cond ((lt a b) -1) ((lt b a) 1) (else 0)))
  (define (compare-by> gt a b)
    (cond ((gt a b) 1) ((gt b a) -1) (else 0)))
  (define (compare-by<= le a b)
    (cond ((le a b) (if (le b a) 0 -1)) (else 1)))
  (define (compare-by>= ge a b)
    (cond ((ge a b) (if (ge b a) 0 1)) (else -1)))
  (define (compare-by= eq a b)
    (if (eq a b) 0 -1)))
