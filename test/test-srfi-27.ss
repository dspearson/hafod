;;; test-srfi-27.ss -- Conformance tests for the random library (SRFI 27).
;;;
;;; SRFI 27 requires random-real to fall strictly inside (0,1) and
;;; random-integer to return an exact integer in [0,n). This suite asserts
;;; those invariants over a sample of draws (never a specific random value)
;;; and exercises the source-returning accessors. The audit dispositioned the
;;; range behaviour conformant (the shared-generator note is deliberate);
;;; these assertions pin the ranges.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-27) (chezscheme))

(test-begin "srfi-27")

;; ===========================================================================
;; Section A -- random-real is strictly within (0,1) over a sample
;; ===========================================================================

(define (real-sample-ok? n)
  (let loop ((k 0) (ok #t))
    (if (or (= k n) (not ok))
        ok
        (let ((r (random-real)))
          (loop (+ k 1) (and (real? r) (> r 0.0) (< r 1.0)))))))

(test-assert "random-real is strictly in (0,1) across 2000 draws"
             (real-sample-ok? 2000))

;; ===========================================================================
;; Section B -- random-integer is an exact integer in [0,n) over a sample
;; ===========================================================================

(define (int-sample-ok? bound n)
  (let loop ((k 0) (ok #t))
    (if (or (= k n) (not ok))
        ok
        (let ((x (random-integer bound)))
          (loop (+ k 1)
                (and (integer? x) (exact? x) (>= x 0) (< x bound)))))))

(test-assert "random-integer stays in [0,100) across 2000 draws"
             (int-sample-ok? 100 2000))
(test-assert "random-integer stays in [0,2) across 2000 draws"
             (int-sample-ok? 2 2000))
(test-equal "random-integer over a bound of 1 is always 0"
            0 (random-integer 1))

;; ===========================================================================
;; Section C -- the source API is callable and yields in-range numbers
;; ===========================================================================

(test-assert "default-random-source is a random source"
             (random-source? default-random-source))

(define ints (random-source-make-integers default-random-source))
(test-assert "random-source-make-integers returns a generator producing [0,n)"
             (let ((x (ints 50))) (and (integer? x) (>= x 0) (< x 50))))

(define reals (random-source-make-reals default-random-source))
(test-assert "random-source-make-reals returns a generator producing (0,1)"
             (let ((r (reals))) (and (real? r) (> r 0.0) (< r 1.0))))

(test-assert "random-source-pseudo-randomize! is callable and leaves the source usable"
             (begin
               (random-source-pseudo-randomize! default-random-source 1 1)
               (let ((x (random-integer 10))) (and (>= x 0) (< x 10)))))

(test-end)
