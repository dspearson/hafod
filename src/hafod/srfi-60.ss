#!chezscheme
;;; (hafod srfi-60) -- SRFI-60: Integers as Bits
;;; Reference: https://srfi.schemers.org/srfi-60/srfi-60.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Re-exports Chez bitwise operations with SRFI-60 names.

(library (hafod srfi-60)
  (export logand logior logxor lognot
          bitwise-if
          logtest logbit?
          ash
          logcount integer-length
          bit-field
          arithmetic-shift
          bitwise-and bitwise-ior bitwise-xor bitwise-not
          bitwise-merge
          any-bits-set? bit-set?
          copy-bit copy-bit-field
          rotate-bit-field reverse-bit-field
          integer->list list->integer)
  (import (except (chezscheme)
                  bitwise-and bitwise-ior bitwise-xor bitwise-not
                  bitwise-arithmetic-shift bitwise-bit-field
                  integer-length))

  ;; SRFI-60 names mapping to Chez equivalents
  (define bitwise-and logand)
  (define bitwise-ior logior)
  (define bitwise-xor logxor)
  (define bitwise-not lognot)
  (define bitwise-merge bitwise-if)
  (define any-bits-set? logtest)
  (define bit-set? logbit?)
  (define arithmetic-shift ash)

  (define (logcount n)
    (bitwise-bit-count (if (< n 0) (lognot n) n)))

  (define integer-length bitwise-length)

  (define (bit-field n start end)
    (logand (ash n (- start))
            (- (ash 1 (- end start)) 1)))

  (define (copy-bit index n bit)
    (if (zero? bit)
        (logand n (lognot (ash 1 index)))
        (logior n (ash 1 index))))

  (define (copy-bit-field to start end from)
    (let ((mask (- (ash 1 end) (ash 1 start))))
      (logior (logand to (lognot mask))
              (logand (ash from start) mask))))

  (define (rotate-bit-field n count start end)
    (let* ((width (- end start))
           (count (if (zero? width) 0 (modulo count width)))
           (field (bit-field n start end)))
      (copy-bit-field n start end
        (logior (ash field count)
                (ash field (- count width))))))

  (define (reverse-bit-field n start end)
    (let loop ((i start) (j (- end 1)) (result n))
      (if (>= i j) result
          (let ((bi (logbit? i result))
                (bj (logbit? j result)))
            (loop (+ i 1) (- j 1)
                  (copy-bit j (copy-bit i result (if bj 1 0))
                            (if bi 1 0)))))))

  (define (integer->list n . rest)
    (let ((len (if (pair? rest) (car rest) (integer-length n))))
      (let loop ((i 0) (acc '()))
        (if (>= i len) acc
            (loop (+ i 1)
                  (cons (if (logbit? i n) 1 0) acc))))))

  (define (list->integer lst)
    (let loop ((l lst) (result 0))
      (if (null? l) result
          (loop (cdr l) (+ (* result 2) (car l)))))))
