#!chezscheme
;;; (hafod srfi-25) -- SRFI-25: Multi-dimensional Array Primitives
;;; Reference: https://srfi.schemers.org/srfi-25/srfi-25.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-25)
  (export make-array array? array-rank array-ref array-set!
          array-start array-end shape array share-array)
  (import (chezscheme))

  ;; Shape: vector of (lo hi) pairs, stored flat: #(lo0 hi0 lo1 hi1 ...)
  (define (shape . bounds)
    (let ((v (list->vector bounds)))
      (unless (even? (vector-length v))
        (error 'shape "bounds must come in pairs"))
      v))

  ;; Internal array record. The affine coefficients are `offsets` (a per-dimension
  ;; stride) plus a `base` seed: a store position is base + Σ_d (i_d − lo_d)·offset_d.
  ;; A plain array has base 0 and row-major offsets pointing at its own store; a
  ;; shared view (see share-array) carries a probed base + strides and points at the
  ;; parent's store, so both kinds share ONE flat-index path.
  (define-record-type arr
    (fields shape-vec store rank dims offsets base)
    (nongenerative arr-srfi25-hafod))

  (define (array? x) (arr? x))
  (define (array-rank a) (arr-rank a))

  (define (array-start a dim)
    (vector-ref (arr-shape-vec a) (* dim 2)))

  (define (array-end a dim)
    (vector-ref (arr-shape-vec a) (+ (* dim 2) 1)))

  (define (make-array shp . fill)
    (let* ((rank (div (vector-length shp) 2))
           (dims (let loop ((d 0) (acc '()))
                   (if (= d rank) (reverse acc)
                       (loop (+ d 1)
                             (cons (- (vector-ref shp (+ (* d 2) 1))
                                      (vector-ref shp (* d 2)))
                                   acc)))))
           (size (apply * dims))
           (store (make-vector size (if (pair? fill) (car fill) (void))))
           ;; Row-major offsets
           (offsets (let loop ((d (- rank 1)) (stride 1) (acc '()))
                      (if (< d 0) acc
                          (loop (- d 1) (* stride (list-ref dims d))
                                (cons stride acc))))))
      (make-arr shp store rank (list->vector dims) (list->vector offsets) 0)))

  (define (flat-index a indices)
    (let ((rank (arr-rank a))
          (shp (arr-shape-vec a))
          (offsets (arr-offsets a)))
      (let loop ((d 0) (idx (arr-base a)) (rest indices))
        (if (= d rank) idx
            (loop (+ d 1)
                  (+ idx (* (- (car rest) (vector-ref shp (* d 2)))
                            (vector-ref offsets d)))
                  (cdr rest))))))

  ;; index->list: unpack a single index object -- a vector or a rank-1 array --
  ;; into a list of subscripts (SRFI 25 allows either in the one-index form).
  (define (index->list who idx)
    (cond ((vector? idx) (vector->list idx))
          ((array? idx)
           (let ((lo (array-start idx 0))
                 (hi (array-end idx 0)))
             (let loop ((i lo) (acc '()))
               (if (= i hi) (reverse acc)
                   (loop (+ i 1) (cons (array-ref idx i) acc))))))
          (else (error who "index must be a vector or a rank-1 array" idx))))

  ;; array-ref accepts the multi-index form (array-ref a k ...) or a single index
  ;; object (array-ref a index), where index is a vector or a rank-1 array.
  (define array-ref
    (case-lambda
      ((a idx)
       (if (or (vector? idx) (array? idx))
           (vector-ref (arr-store a) (flat-index a (index->list 'array-ref idx)))
           (vector-ref (arr-store a) (flat-index a (list idx)))))
      ((a . ks)
       (vector-ref (arr-store a) (flat-index a ks)))))

  ;; array-set! puts the stored value LAST per SRFI 25: (array-set! a k ... obj)
  ;; and (array-set! a index obj). The three-argument clause serves both the
  ;; rank-1 (a k obj) and the index-object (a index obj) forms -- they agree
  ;; because the value is last; the vector?/array? test on the middle argument
  ;; picks the reading.
  (define array-set!
    (case-lambda
      ((a idx obj)
       (if (or (vector? idx) (array? idx))
           (vector-set! (arr-store a) (flat-index a (index->list 'array-set! idx)) obj)
           (vector-set! (arr-store a) (flat-index a (list idx)) obj)))
      ((a . rest)
       (let* ((r (reverse rest))
              (obj (car r))
              (ks (reverse (cdr r))))
         (vector-set! (arr-store a) (flat-index a ks) obj)))))

  ;; Construct array from shape and elements (row-major)
  (define (array shp . elts)
    (let ((a (make-array shp)))
      (let loop ((i 0) (es elts))
        (unless (null? es)
          (vector-set! (arr-store a) i (car es))
          (loop (+ i 1) (cdr es))))
      a))

  ;; share-array: build a shared view over ARRAY. NEWSHAPE gives the view's
  ;; bounds; PROC maps the view's indices to the parent's and returns them as
  ;; MULTIPLE VALUES (per SRFI 25). The map must be affine, so we recover its
  ;; coefficients by probing: PROC at the new shape's lower-bound corner gives
  ;; the base, and PROC at each unit offset gives that dimension's stride. Every
  ;; probe is mapped through the parent's own flat-index (which already folds in
  ;; the parent's base), so a share of a share composes for free. The view holds
  ;; the parent's store object, so a write through the view lands in the parent.
  (define (share-array array newshape proc)
    (let* ((m (div (vector-length newshape) 2))
           (los (let loop ((d 0) (acc '()))
                  (if (= d m) (reverse acc)
                      (loop (+ d 1) (cons (vector-ref newshape (* d 2)) acc)))))
           (dims (let loop ((d 0) (acc '()))
                   (if (= d m) (reverse acc)
                       (loop (+ d 1)
                             (cons (- (vector-ref newshape (+ (* d 2) 1))
                                      (vector-ref newshape (* d 2)))
                                   acc)))))
           (base (flat-index array
                             (call-with-values (lambda () (apply proc los)) list)))
           (strides
            (let loop ((d 0) (acc '()))
              (if (= d m) (reverse acc)
                  (let* ((probe (let bump ((k 0) (o los))
                                  (if (null? o) '()
                                      (cons (if (= k d) (+ (car o) 1) (car o))
                                            (bump (+ k 1) (cdr o))))))
                         (mapped (call-with-values (lambda () (apply proc probe)) list)))
                    (loop (+ d 1)
                          (cons (- (flat-index array mapped) base) acc)))))))
      (make-arr newshape (arr-store array) m
                (list->vector dims) (list->vector strides) base))))
