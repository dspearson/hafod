#!chezscheme
;;; (hafod srfi-63) -- SRFI-63: Homogeneous and Heterogeneous Arrays
;;; Reference: https://srfi.schemers.org/srfi-63/srfi-63.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Built on top of (hafod srfi-25) with additional SRFI-63 API.

(library (hafod srfi-63)
  (export make-array array? array-rank array-ref array-set!
          array-dimensions list->array array->list
          make-shared-array
          ;; typed array prototypes (vector-backed SRFI 63 A:* constructors)
          A:floC128b A:floC64b A:floC32b A:floC16b
          A:floR128b A:floR64b A:floR32b A:floR16b
          A:floQ128d A:floQ64d A:floQ32d
          A:fixZ64b A:fixZ32b A:fixZ16b A:fixZ8b
          A:fixN64b A:fixN32b A:fixN16b A:fixN8b
          A:bool
          ;; SLIB array-mapping companions
          array-index-map! array-for-each array-map!)
  (import (chezscheme))

  ;; Internal array record. A store position is the affine
  ;; base + Sum_d i_d*stride_d. SRFI 63 is strictly 0-based, so there is no
  ;; per-dimension lower bound to subtract. A plain array has base 0 and
  ;; row-major strides pointing at its own store; a shared view (see
  ;; make-shared-array) carries a probed base + strides and points at the
  ;; parent's store, so both kinds share ONE flat-index path.
  (define-record-type arr63
    (fields store dims rank strides base)
    (nongenerative arr63-hafod))

  (define (array? x) (arr63? x))
  (define (array-rank a) (arr63-rank a))

  (define (array-dimensions a)
    (vector->list (arr63-dims a)))

  (define (make-array prototype . dims)
    (let* ((rank (length dims))
           (size (apply * dims))
           (fill (if (and (arr63? prototype) (> (vector-length (arr63-store prototype)) 0))
                     (vector-ref (arr63-store prototype) 0)
                     (void)))
           (store (make-vector size fill))
           ;; Row-major strides: the last dimension has stride 1, and
           ;; stride_d = stride_{d+1} * dims_{d+1}.
           (strides (let loop ((d (- rank 1)) (stride 1) (acc '()))
                      (if (< d 0) acc
                          (loop (- d 1) (* stride (list-ref dims d))
                                (cons stride acc))))))
      (make-arr63 store (list->vector dims) rank (list->vector strides) 0)))

  ;; Affine flat-index: seed the accumulator at the array's base and add
  ;; i_d*stride_d for each dimension. Explicit strides give the same result as
  ;; the row-major walk for a plain array, and this one path also serves a
  ;; shared view (a probed base + strides into the parent's store).
  (define (flat-index a indices)
    (let ((strides (arr63-strides a)))
      (let loop ((idx (arr63-base a)) (rest indices) (d 0))
        (if (null? rest) idx
            (loop (+ idx (* (car rest) (vector-ref strides d)))
                  (cdr rest) (+ d 1))))))

  (define (array-ref a . indices)
    (vector-ref (arr63-store a) (flat-index a indices)))

  (define (array-set! a val . indices)
    (vector-set! (arr63-store a) (flat-index a indices) val))

  (define (list->array rank lst . opt)
    (let* ((dims (if (pair? opt) (car opt)
                     (if (= rank 1) (list (length lst))
                         (error 'list->array "dimensions required for rank > 1"))))
           (a (apply make-array (make-arr63 '#() '#() 0 '#() 0) dims))
           (store (arr63-store a)))
      (let loop ((i 0) (l lst))
        (unless (null? l)
          (vector-set! store i (car l))
          (loop (+ i 1) (cdr l))))
      a))

  (define (array->list a)
    (vector->list (arr63-store a)))

  ;; Typed array prototypes. Storage stays boxed Scheme vectors, so the "type"
  ;; is nominal -- a prototype differs only in the default fill it seeds. A
  ;; prototype is a minimal one-element array carrying the fill at its origin;
  ;; make-array reads (vector-ref (arr63-store prototype) 0) as the fill. An
  ;; optional argument overrides the default. Default fills are the
  ;; type-appropriate zero (SRFI 63 leaves the no-argument fill unspecified;
  ;; hafod picks the type-zero, a documented concretisation).
  (define (proto fill)
    (make-arr63 (vector fill) (vector 1) 1 (vector 1) 0))

  ;; inexact complex -- default 0.0+0.0i
  (define (A:floC128b . opt) (proto (if (pair? opt) (car opt) (make-rectangular 0.0 0.0))))
  (define (A:floC64b  . opt) (proto (if (pair? opt) (car opt) (make-rectangular 0.0 0.0))))
  (define (A:floC32b  . opt) (proto (if (pair? opt) (car opt) (make-rectangular 0.0 0.0))))
  (define (A:floC16b  . opt) (proto (if (pair? opt) (car opt) (make-rectangular 0.0 0.0))))
  ;; inexact real -- default 0.0
  (define (A:floR128b . opt) (proto (if (pair? opt) (car opt) 0.0)))
  (define (A:floR64b  . opt) (proto (if (pair? opt) (car opt) 0.0)))
  (define (A:floR32b  . opt) (proto (if (pair? opt) (car opt) 0.0)))
  (define (A:floR16b  . opt) (proto (if (pair? opt) (car opt) 0.0)))
  ;; exact rational -- default 0
  (define (A:floQ128d . opt) (proto (if (pair? opt) (car opt) 0)))
  (define (A:floQ64d  . opt) (proto (if (pair? opt) (car opt) 0)))
  (define (A:floQ32d  . opt) (proto (if (pair? opt) (car opt) 0)))
  ;; exact signed integer -- default 0
  (define (A:fixZ64b  . opt) (proto (if (pair? opt) (car opt) 0)))
  (define (A:fixZ32b  . opt) (proto (if (pair? opt) (car opt) 0)))
  (define (A:fixZ16b  . opt) (proto (if (pair? opt) (car opt) 0)))
  (define (A:fixZ8b   . opt) (proto (if (pair? opt) (car opt) 0)))
  ;; exact nonnegative integer -- default 0
  (define (A:fixN64b  . opt) (proto (if (pair? opt) (car opt) 0)))
  (define (A:fixN32b  . opt) (proto (if (pair? opt) (car opt) 0)))
  (define (A:fixN16b  . opt) (proto (if (pair? opt) (car opt) 0)))
  (define (A:fixN8b   . opt) (proto (if (pair? opt) (car opt) 0)))
  ;; boolean -- default #f
  (define (A:bool     . opt) (proto (if (pair? opt) (car opt) #f)))

  ;; make-shared-array: build a shared view over ARRAY. DIMS gives the view's
  ;; 0-based extents; MAPPER maps the view's indices to the parent's and returns
  ;; them as a LIST (per SRFI 63 -- not multiple values). The map must be
  ;; affine, so we recover its coefficients by probing: MAPPER at the all-zeros
  ;; origin gives the base, and MAPPER at each unit offset gives that
  ;; dimension's stride. Every probe is mapped through the parent's own
  ;; flat-index (which folds in the parent's base), so a share of a share
  ;; composes for free. The view holds the parent's store object, so a write
  ;; through the view lands in the parent (true aliasing).
  (define (make-shared-array a mapper . dims)
    (let* ((m (length dims))
           (origin (make-list m 0))
           (base (flat-index a (apply mapper origin)))
           (strides
            (let loop ((d 0) (acc '()))
              (if (= d m) (list->vector (reverse acc))
                  (let* ((probe (let bump ((k 0) (o origin))
                                  (if (null? o) '()
                                      (cons (if (= k d) (+ (car o) 1) (car o))
                                            (bump (+ k 1) (cdr o))))))
                         (mapped (apply mapper probe)))
                    (loop (+ d 1) (cons (- (flat-index a mapped) base) acc)))))))
      (make-arr63 (arr63-store a) (list->vector dims) m strides base)))

  ;; Row-major index-space walk: visit every 0-based index tuple of an array
  ;; with the given extents, in row-major order, passing each tuple (as a list)
  ;; to PROC. Reuses flat->indices to decompose each flat position.
  (define (for-each-index dims-list proc)
    (let ((dims-vec (list->vector dims-list))
          (size (apply * dims-list)))
      (let loop ((i 0))
        (when (< i size)
          (proc (flat->indices dims-vec i))
          (loop (+ i 1))))))

  ;; array-index-map!, array-for-each and array-map! are SLIB "Array Mapping"
  ;; companions to SRFI 63 (they are not defined in the SRFI 63 document
  ;; itself). array-set! is object-first, so each stores its value first.

  ;; array-index-map! stores proc(indices...) at each cell.
  (define (array-index-map! a proc)
    (for-each-index (array-dimensions a)
      (lambda (idxs) (apply array-set! a (apply proc idxs) idxs))))

  ;; array-for-each: proc FIRST; visit every element tuple in row-major order.
  (define (array-for-each proc . arrays)
    (for-each-index (array-dimensions (car arrays))
      (lambda (idxs)
        (apply proc (map (lambda (ar) (apply array-ref ar idxs)) arrays)))))

  ;; array-map!: destination FIRST; store proc(sources...) into the destination.
  (define (array-map! a0 proc . sources)
    (for-each-index (array-dimensions a0)
      (lambda (idxs)
        (apply array-set! a0
               (apply proc (map (lambda (s) (apply array-ref s idxs)) sources))
               idxs))))

  (define (flat->indices dims flat)
    (let ((rank (vector-length dims)))
      (let loop ((d (- rank 1)) (f flat) (acc '()))
        (if (< d 0) acc
            (let ((dim (vector-ref dims d)))
              (loop (- d 1) (div f dim) (cons (mod f dim) acc))))))))
