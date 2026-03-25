#!chezscheme
;;; (hafod srfi-27) -- SRFI-27: Sources of Random Bits
;;; Reference: https://srfi.schemers.org/srfi-27/srfi-27.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Builds on Chez Scheme's random procedure.

(library (hafod srfi-27)
  (export random-integer random-real default-random-source
          random-source? random-source-make-integers
          random-source-make-reals random-source-randomize!
          random-source-pseudo-randomize!)
  (import (chezscheme))

  (define-record-type (random-source %make-random-source random-source?)
    (fields (mutable state)))

  (define default-random-source (%make-random-source (random-seed)))

  (define (random-integer n)
    (random n))

  (define (random-real)
    ;; Returns a value in (0, 1)
    (/ (+ 1.0 (random (- (expt 2 32) 2))) (expt 2.0 32)))

  (define (random-source-make-integers src)
    (lambda (n) (random n)))

  (define (random-source-make-reals src)
    (lambda () (random-real)))

  (define (random-source-randomize! src)
    (random-seed (time-nanosecond (current-time)))
    (random-source-state-set! src (random-seed)))

  (define (random-source-pseudo-randomize! src i j)
    (random-seed (+ (* i 65536) j))
    (random-source-state-set! src (random-seed))))
