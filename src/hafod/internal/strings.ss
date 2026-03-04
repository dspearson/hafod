;;; (hafod internal strings) -- SRFI-13 subset for hafod
;;; Provides string-index, string-index-right, string-every
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal strings)
  (export string-index string-index-right string-every)
  (import (chezscheme))

  ;; string-index str criterion [start end]
  ;; Returns the index of the first char in str[start..end) satisfying criterion,
  ;; or #f if none. criterion is a char or a predicate.
  (define (string-index str criterion . rest)
    (let ((start (if (pair? rest) (car rest) 0))
          (end (if (and (pair? rest) (pair? (cdr rest)))
                   (cadr rest)
                   (string-length str))))
      (if (char? criterion)
          (let lp ((i start))
            (and (< i end)
                 (if (char=? criterion (string-ref str i)) i
                     (lp (+ i 1)))))
          ;; criterion is a predicate
          (let lp ((i start))
            (and (< i end)
                 (if (criterion (string-ref str i)) i
                     (lp (+ i 1))))))))

  ;; string-index-right str criterion [start end]
  ;; Returns the index of the last char in str[start..end) satisfying criterion,
  ;; or #f if none.
  (define (string-index-right str criterion . rest)
    (let ((start (if (pair? rest) (car rest) 0))
          (end (if (and (pair? rest) (pair? (cdr rest)))
                   (cadr rest)
                   (string-length str))))
      (if (char? criterion)
          (let lp ((i (- end 1)))
            (and (>= i start)
                 (if (char=? criterion (string-ref str i)) i
                     (lp (- i 1)))))
          (let lp ((i (- end 1)))
            (and (>= i start)
                 (if (criterion (string-ref str i)) i
                     (lp (- i 1))))))))

  ;; string-every pred str [start end]
  ;; Returns #t if pred holds for every character in str[start..end).
  ;; Returns #t for empty range.
  (define (string-every pred str . rest)
    (let ((start (if (pair? rest) (car rest) 0))
          (end (if (and (pair? rest) (pair? (cdr rest)))
                   (cadr rest)
                   (string-length str))))
      (let lp ((i start))
        (or (>= i end)
            (and (pred (string-ref str i))
                 (lp (+ i 1)))))))

  ) ; end library
