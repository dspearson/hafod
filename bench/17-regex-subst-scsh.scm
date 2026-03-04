#!/usr/local/bin/scsh \
-s
!#
;;; Benchmark 17: Regex substitution throughput
(define N 5000)
(define text "The quick brown fox jumps over the lazy dog")
(let ((re (rx (+ alphabetic))))
  (let loop ((i 0))
    (if (< i N)
        (begin
          (regexp-substitute/global #f re text 'pre "WORD" 'post)
          (loop (+ i 1))))))
