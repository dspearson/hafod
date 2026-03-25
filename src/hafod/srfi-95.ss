#!chezscheme
;;; (hafod srfi-95) -- SRFI-95: Sorting and Merging
;;; Reference: https://srfi.schemers.org/srfi-95/srfi-95.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Wraps Chez sort/merge with SRFI-95 argument order.

(library (hafod srfi-95)
  (export sorted? merge sort)
  (import (rename (chezscheme)
            (sort chez:sort)
            (merge chez:merge)))

  ;; SRFI-95: (sort sequence less? [key])
  ;; Chez:    (sort less? list)
  (define sort
    (case-lambda
      [(seq less?)
       (if (vector? seq)
           (let ([lst (chez:sort less? (vector->list seq))])
             (list->vector lst))
           (chez:sort less? seq))]
      [(seq less? key)
       (if (vector? seq)
           (let ([lst (chez:sort (lambda (a b) (less? (key a) (key b)))
                                (vector->list seq))])
             (list->vector lst))
           (chez:sort (lambda (a b) (less? (key a) (key b))) seq))]))

  ;; SRFI-95: (merge list1 list2 less? [key])
  ;; Chez:    (merge less? list1 list2)
  (define merge
    (case-lambda
      [(lst1 lst2 less?)
       (chez:merge less? lst1 lst2)]
      [(lst1 lst2 less? key)
       (chez:merge (lambda (a b) (less? (key a) (key b))) lst1 lst2)]))

  (define (sorted? seq less? . opt-key)
    (let ((key (if (pair? opt-key) (car opt-key) values)))
      (cond
        ((vector? seq)
         (let ((n (vector-length seq)))
           (or (<= n 1)
               (let loop ((i 1))
                 (or (= i n)
                     (and (not (less? (key (vector-ref seq i))
                                      (key (vector-ref seq (- i 1)))))
                          (loop (+ i 1))))))))
        ((null? seq) #t)
        ((null? (cdr seq)) #t)
        (else
         (let loop ((prev (car seq)) (rest (cdr seq)))
           (or (null? rest)
               (and (not (less? (key (car rest)) (key prev)))
                    (loop (car rest) (cdr rest))))))))))
