;;; test-srfi-40.ss -- Spot-check tests for the stream library (SRFI 40).
;;;
;;; SRFI 40 provides lazy streams: stream-cons defers both head and tail,
;;; stream-car / stream-cdr force them, and stream-map / stream-filter build
;;; further lazy streams. This suite spot-checks construction and access, a
;;; bounded map and filter, and laziness (a bounded prefix of an infinite
;;; stream). The audit dispositioned this library conformant.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-40) (chezscheme))

(test-begin "srfi-40")

;; Take the first n elements of a stream into a list, for assertion.
(define (stream-take s n)
  (if (or (= n 0) (stream-null? s))
      '()
      (cons (stream-car s) (stream-take (stream-cdr s) (- n 1)))))

;; ===========================================================================
;; Section A -- stream-cons / stream-car / stream-cdr and the predicates
;; ===========================================================================

(define s (stream-cons 1 (stream-cons 2 (stream-cons 3 stream-null))))

(test-equal "stream-car returns the head" 1 (stream-car s))
(test-equal "stream-cdr then stream-car returns the second element"
            2 (stream-car (stream-cdr s)))
(test-assert "stream? recognises a stream" (stream? s))
(test-assert "stream-pair? recognises a non-empty stream" (stream-pair? s))
(test-assert "stream-null? recognises the empty stream" (stream-null? stream-null))

;; The stream constructor macro builds the same shape.
(test-equal "the stream constructor collects its elements"
            '(10 20 30) (stream-take (stream 10 20 30) 3))

;; ===========================================================================
;; Section B -- stream-map and stream-filter over a bounded prefix
;; ===========================================================================

(test-equal "stream-map squares each element"
            '(1 4 9 16) (stream-take (stream-map (lambda (x) (* x x)) (stream 1 2 3 4)) 4))
(test-equal "stream-filter keeps the even elements"
            '(2 4 6) (stream-take (stream-filter even? (stream 1 2 3 4 5 6)) 3))

;; ===========================================================================
;; Section C -- laziness: a bounded prefix of an infinite stream terminates
;; ===========================================================================

(define (ints-from n) (stream-cons n (ints-from (+ n 1))))

(test-equal "a bounded prefix of an infinite stream is finite (proves laziness)"
            '(1 2 3 4 5) (stream-take (ints-from 1) 5))

(test-end)
