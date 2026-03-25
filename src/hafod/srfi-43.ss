#!chezscheme
;;; (hafod srfi-43) -- SRFI-43: Vector Library
;;; Reference: https://srfi.schemers.org/srfi-43/srfi-43.html
;;; Copyright (c) 2026 Dominic Pearson.
;;; Note: SRFI-43 iteration procedures take (i elem ...) not just (elem ...).

(library (hafod srfi-43)
  (export
    ;; Constructors
    vector-unfold vector-unfold-right
    vector-copy vector-reverse-copy vector-concatenate
    ;; Predicates
    vector-empty? vector=
    ;; Iteration
    vector-fold vector-fold-right
    vector-map vector-map!
    vector-for-each
    vector-count
    ;; Searching
    vector-index vector-index-right
    vector-skip vector-skip-right
    vector-binary-search
    vector-any vector-every
    ;; Mutators
    vector-swap!
    vector-reverse!
    vector-copy! vector-reverse-copy!
    ;; Conversion
    reverse-vector->list reverse-list->vector)
  (import (except (chezscheme) vector-map vector-for-each vector-copy vector-copy!))

  (define (vector-empty? v) (zero? (vector-length v)))

  (define (vector= elt=? . vecs)
    (or (null? vecs)
        (null? (cdr vecs))
        (let loop ((vs vecs))
          (or (null? (cdr vs))
              (let ((a (car vs)) (b (cadr vs)))
                (and (= (vector-length a) (vector-length b))
                     (let inner ((i 0))
                       (or (= i (vector-length a))
                           (and (elt=? (vector-ref a i) (vector-ref b i))
                                (inner (+ i 1)))))
                     (loop (cdr vs))))))))

  (define (vector-unfold f len . seeds)
    (let ((v (make-vector len)))
      (let loop ((i 0) (seeds seeds))
        (unless (= i len)
          (call-with-values
           (lambda () (apply f i seeds))
           (lambda (val . new-seeds)
             (vector-set! v i val)
             (loop (+ i 1) new-seeds)))))
      v))

  (define (vector-unfold-right f len . seeds)
    (let ((v (make-vector len)))
      (let loop ((i (- len 1)) (seeds seeds))
        (unless (< i 0)
          (call-with-values
           (lambda () (apply f i seeds))
           (lambda (val . new-seeds)
             (vector-set! v i val)
             (loop (- i 1) new-seeds)))))
      v))

  (define vector-copy
    (case-lambda
      ((v) (vector-copy v 0 (vector-length v)))
      ((v start) (vector-copy v start (vector-length v)))
      ((v start end)
       (let* ((len (- end start))
              (result (make-vector len)))
         (do ((i 0 (+ i 1)))
             ((= i len) result)
           (vector-set! result i (vector-ref v (+ start i))))))))

  (define (vector-reverse-copy v . args)
    (let* ((start (if (pair? args) (car args) 0))
           (end (if (and (pair? args) (pair? (cdr args)))
                    (cadr args) (vector-length v)))
           (len (- end start))
           (result (make-vector len)))
      (do ((i 0 (+ i 1)))
          ((= i len) result)
        (vector-set! result i (vector-ref v (+ start (- len i 1)))))))

  (define (vector-concatenate vecs)
    (apply vector-append vecs))

  (define (vector-fold kons knil . vecs)
    (let ((len (apply min (map vector-length vecs))))
      (let loop ((i 0) (acc knil))
        (if (= i len) acc
            (loop (+ i 1)
                  (apply kons i acc (map (lambda (v) (vector-ref v i)) vecs)))))))

  (define (vector-fold-right kons knil . vecs)
    (let ((len (apply min (map vector-length vecs))))
      (let loop ((i (- len 1)) (acc knil))
        (if (< i 0) acc
            (loop (- i 1)
                  (apply kons i acc (map (lambda (v) (vector-ref v i)) vecs)))))))

  ;; SRFI-43 vector-map: f takes (i elem ...) and returns new vector
  (define (vector-map f . vecs)
    (let* ((len (apply min (map vector-length vecs)))
           (result (make-vector len)))
      (do ((i 0 (+ i 1))) ((= i len) result)
        (vector-set! result i
          (apply f i (map (lambda (v) (vector-ref v i)) vecs))))))

  (define (vector-map! f . vecs)
    (let ((len (apply min (map vector-length vecs))))
      (do ((i 0 (+ i 1))) ((= i len))
        (vector-set! (car vecs) i
          (apply f i (map (lambda (v) (vector-ref v i)) vecs))))))

  ;; SRFI-43 vector-for-each: f takes (i elem ...)
  (define (vector-for-each f . vecs)
    (let ((len (apply min (map vector-length vecs))))
      (do ((i 0 (+ i 1))) ((= i len))
        (apply f i (map (lambda (v) (vector-ref v i)) vecs)))))

  (define (vector-count pred . vecs)
    (let ((len (apply min (map vector-length vecs))))
      (let loop ((i 0) (count 0))
        (if (= i len) count
            (loop (+ i 1)
                  (if (apply pred i (map (lambda (v) (vector-ref v i)) vecs))
                      (+ count 1) count))))))

  (define (vector-index pred . vecs)
    (let ((len (apply min (map vector-length vecs))))
      (let loop ((i 0))
        (cond
          ((= i len) #f)
          ((apply pred i (map (lambda (v) (vector-ref v i)) vecs)) i)
          (else (loop (+ i 1)))))))

  (define (vector-index-right pred . vecs)
    (let ((len (apply min (map vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
          ((< i 0) #f)
          ((apply pred i (map (lambda (v) (vector-ref v i)) vecs)) i)
          (else (loop (- i 1)))))))

  (define (vector-skip pred . vecs)
    (apply vector-index (lambda args (not (apply pred args))) vecs))

  (define (vector-skip-right pred . vecs)
    (apply vector-index-right (lambda args (not (apply pred args))) vecs))

  (define (vector-binary-search vec value cmp)
    (let loop ((lo 0) (hi (- (vector-length vec) 1)))
      (if (> lo hi) #f
          (let* ((mid (div (+ lo hi) 2))
                 (c (cmp (vector-ref vec mid) value)))
            (cond
              ((zero? c) mid)
              ((negative? c) (loop (+ mid 1) hi))
              (else (loop lo (- mid 1))))))))

  (define (vector-any pred . vecs)
    (let ((len (apply min (map vector-length vecs))))
      (let loop ((i 0))
        (and (< i len)
             (or (apply pred i (map (lambda (v) (vector-ref v i)) vecs))
                 (loop (+ i 1)))))))

  (define (vector-every pred . vecs)
    (let ((len (apply min (map vector-length vecs))))
      (let loop ((i 0) (result #t))
        (if (= i len) result
            (let ((r (apply pred i (map (lambda (v) (vector-ref v i)) vecs))))
              (and r (loop (+ i 1) r)))))))

  (define (vector-swap! v i j)
    (let ((tmp (vector-ref v i)))
      (vector-set! v i (vector-ref v j))
      (vector-set! v j tmp)))

  (define (vector-reverse! v . args)
    (let* ((start (if (pair? args) (car args) 0))
           (end (if (and (pair? args) (pair? (cdr args)))
                    (cadr args) (vector-length v))))
      (let loop ((i start) (j (- end 1)))
        (when (< i j)
          (vector-swap! v i j)
          (loop (+ i 1) (- j 1))))))

  (define vector-copy!
    (case-lambda
      ((target tstart source)
       (vector-copy! target tstart source 0 (vector-length source)))
      ((target tstart source sstart)
       (vector-copy! target tstart source sstart (vector-length source)))
      ((target tstart source sstart send)
       (if (<= tstart sstart)
           (do ((i sstart (+ i 1)) (j tstart (+ j 1)))
               ((= i send))
             (vector-set! target j (vector-ref source i)))
           (do ((i (- send 1) (- i 1)) (j (+ tstart (- send sstart) -1) (- j 1)))
               ((< i sstart))
             (vector-set! target j (vector-ref source i)))))))

  (define (vector-reverse-copy! target tstart source . args)
    (let* ((sstart (if (pair? args) (car args) 0))
           (send (if (and (pair? args) (pair? (cdr args)))
                     (cadr args) (vector-length source)))
           (len (- send sstart)))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (vector-set! target (+ tstart i)
                     (vector-ref source (+ sstart (- len i 1)))))))

  (define (reverse-vector->list v . args)
    (let* ((start (if (pair? args) (car args) 0))
           (end (if (and (pair? args) (pair? (cdr args)))
                    (cadr args) (vector-length v))))
      (let loop ((i start) (acc '()))
        (if (= i end) acc
            (loop (+ i 1) (cons (vector-ref v i) acc))))))

  (define (reverse-list->vector lst)
    (list->vector (reverse lst))))
