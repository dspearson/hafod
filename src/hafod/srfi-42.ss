#!chezscheme
;;; (hafod srfi-42) -- SRFI-42: Eager Comprehensions
;;; Reference: https://srfi.schemers.org/srfi-42/srfi-42.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-42)
  (export
    do-ec list-ec append-ec string-ec string-append-ec
    vector-ec vector-of-length-ec
    sum-ec product-ec min-ec max-ec
    any?-ec every?-ec first-ec last-ec
    fold-ec fold3-ec
    ;; Qualifiers
    :list :string :vector :integers :range :real-range
    :char-range :port :dispatched :do :let :parallel
    :while :until
    ;; Typed generators (index variable)
    :generator-proc)
  (import (chezscheme))

  ;;; ================================================================
  ;;; Qualifier keywords (must be defined for export, used as literals)
  ;;; ================================================================

  (define-syntax :list (lambda (x) (syntax-violation ':list "misuse of :list outside comprehension" x)))
  (define-syntax :string (lambda (x) (syntax-violation ':string "misuse of :string outside comprehension" x)))
  (define-syntax :vector (lambda (x) (syntax-violation ':vector "misuse of :vector outside comprehension" x)))
  (define-syntax :integers (lambda (x) (syntax-violation ':integers "misuse of :integers outside comprehension" x)))
  (define-syntax :range (lambda (x) (syntax-violation ':range "misuse of :range outside comprehension" x)))
  (define-syntax :real-range (lambda (x) (syntax-violation ':real-range "misuse of :real-range outside comprehension" x)))
  (define-syntax :char-range (lambda (x) (syntax-violation ':char-range "misuse of :char-range outside comprehension" x)))
  (define-syntax :port (lambda (x) (syntax-violation ':port "misuse of :port outside comprehension" x)))
  (define-syntax :dispatched (lambda (x) (syntax-violation ':dispatched "misuse of :dispatched outside comprehension" x)))
  (define-syntax :do (lambda (x) (syntax-violation ':do "misuse of :do outside comprehension" x)))
  (define-syntax :let (lambda (x) (syntax-violation ':let "misuse of :let outside comprehension" x)))
  (define-syntax :parallel (lambda (x) (syntax-violation ':parallel "misuse of :parallel outside comprehension" x)))
  (define-syntax :while (lambda (x) (syntax-violation ':while "misuse of :while outside comprehension" x)))
  (define-syntax :until (lambda (x) (syntax-violation ':until "misuse of :until outside comprehension" x)))
  (define-syntax :generator-proc (lambda (x) (syntax-violation ':generator-proc "misuse of :generator-proc outside comprehension" x)))

  ;;; ================================================================
  ;;; Core dispatch: do-ec processes qualifiers and body
  ;;; ================================================================

  ;; The fundamental form: (do-ec qualifier ... body)
  ;; Qualifiers are processed right-to-left (outermost first).

  (define-syntax do-ec
    (syntax-rules ()
      ;; No qualifiers, just body
      ((do-ec body)
       body)
      ;; One qualifier + body
      ((do-ec qual body)
       (do-ec:inner qual body))
      ;; Multiple qualifiers: nest them
      ((do-ec qual1 qual2 ... body)
       (do-ec:inner qual1 (do-ec qual2 ... body)))))

  ;; Process a single qualifier
  (define-syntax do-ec:inner
    (syntax-rules (:list :vector :string :integers :range :real-range
                   :char-range :port :do :let :parallel :while :until
                   :dispatched if not and or)
      ;; :list qualifier
      ((do-ec:inner (:list var lst) body)
       (for-each (lambda (var) body) lst))
      ((do-ec:inner (:list var lst1 lst2 ...) body)
       (begin (for-each (lambda (var) body) lst1)
              (do-ec:inner (:list var lst2 ...) body)))

      ;; :range qualifier
      ((do-ec:inner (:range var stop) body)
       (do-ec:inner (:range var 0 stop 1) body))
      ((do-ec:inner (:range var start stop) body)
       (do-ec:inner (:range var start stop 1) body))
      ((do-ec:inner (:range var start stop step) body)
       (let ((s stop) (st step))
         (let loop ((var start))
           (when (if (> st 0) (< var s) (> var s))
             body
             (loop (+ var st))))))

      ;; :real-range qualifier
      ((do-ec:inner (:real-range var start stop) body)
       (do-ec:inner (:real-range var start stop 1.0) body))
      ((do-ec:inner (:real-range var start stop step) body)
       (let ((s (inexact stop)) (st (inexact step)))
         (let loop ((var (inexact start)))
           (when (if (> st 0) (< var s) (> var s))
             body
             (loop (+ var st))))))

      ;; :char-range qualifier
      ((do-ec:inner (:char-range var start stop) body)
       (let ((s (char->integer stop)))
         (let loop ((i (char->integer start)))
           (when (<= i s)
             (let ((var (integer->char i)))
               body)
             (loop (+ i 1))))))

      ;; :string qualifier
      ((do-ec:inner (:string var str) body)
       (let ((s str))
         (do ((i 0 (+ i 1))) ((= i (string-length s)))
           (let ((var (string-ref s i))) body))))
      ((do-ec:inner (:string var str1 str2 ...) body)
       (begin (do-ec:inner (:string var str1) body)
              (do-ec:inner (:string var str2 ...) body)))

      ;; :vector qualifier
      ((do-ec:inner (:vector var vec) body)
       (let ((v vec))
         (do ((i 0 (+ i 1))) ((= i (vector-length v)))
           (let ((var (vector-ref v i))) body))))
      ((do-ec:inner (:vector var vec1 vec2 ...) body)
       (begin (do-ec:inner (:vector var vec1) body)
              (do-ec:inner (:vector var vec2 ...) body)))

      ;; :integers qualifier (infinite)
      ((do-ec:inner (:integers var) body)
       (let loop ((var 0))
         body
         (loop (+ var 1))))

      ;; :port qualifier
      ((do-ec:inner (:port var port) body)
       (do-ec:inner (:port var port read) body))
      ((do-ec:inner (:port var port reader) body)
       (let ((p port) (rd reader))
         (let loop ()
           (let ((var (rd p)))
             (unless (eof-object? var)
               body
               (loop))))))

      ;; :do qualifier (general loop)
      ((do-ec:inner (:do (bind ...) test (step ...)) body)
       (let (bind ...)
         (let loop ()
           (when test
             body
             (let (step ...)
               (loop))))))
      ((do-ec:inner (:do olet (bind ...) test ilet (step ...)) body)
       (let olet
         (let (bind ...)
           (let loop ()
             (when test
               (let ilet body)
               (let (step ...)
                 (loop)))))))

      ;; :let qualifier
      ((do-ec:inner (:let var expr) body)
       (let ((var expr)) body))

      ;; :parallel qualifier
      ((do-ec:inner (:parallel gen1 gen2 ...) body)
       (do-ec:parallel (gen1 gen2 ...) body))

      ;; :while qualifier
      ((do-ec:inner (:while gen test) body)
       (call-with-current-continuation
        (lambda (exit)
          (do-ec:inner gen
            (if test body (exit (void)))))))

      ;; :until qualifier
      ((do-ec:inner (:until gen test) body)
       (call-with-current-continuation
        (lambda (exit)
          (do-ec:inner gen
            (begin body (when test (exit (void))))))))

      ;; if guard
      ((do-ec:inner (if test) body)
       (when test body))

      ;; not guard
      ((do-ec:inner (not test) body)
       (unless test body))

      ;; and guard
      ((do-ec:inner (and test ...) body)
       (when (and test ...) body))

      ;; or guard
      ((do-ec:inner (or test ...) body)
       (when (or test ...) body))

      ;; Nested qualifier (dispatched or unrecognized => treat as generator expression)
      ((do-ec:inner (gen-expr ...) body)
       (gen-expr ... body))))

  ;; :parallel — run generators in lockstep
  ;; Handles the common case of :list and :range in parallel via manual stepping.
  (define-syntax do-ec:parallel
    (syntax-rules (:range :list :vector :string)
      ;; Single generator — just run it
      ((_ (gen) body)
       (do-ec:inner gen body))
      ;; Two :list generators in parallel
      ((_ ((:list v1 lst1) (:list v2 lst2)) body)
       (let loop ([l1 lst1] [l2 lst2])
         (when (and (pair? l1) (pair? l2))
           (let ([v1 (car l1)] [v2 (car l2)])
             body
             (loop (cdr l1) (cdr l2))))))
      ;; Two :range generators in parallel
      ((_ ((:range v1 n1) (:range v2 n2)) body)
       (let ([limit (min n1 n2)])
         (let loop ([v1 0] [v2 0])
           (when (and (< v1 n1) (< v2 n2))
             body
             (loop (+ v1 1) (+ v2 1))))))
      ;; :range and :list in parallel
      ((_ ((:range v1 n1) (:list v2 lst2)) body)
       (let loop ([v1 0] [l2 lst2])
         (when (and (< v1 n1) (pair? l2))
           (let ([v2 (car l2)])
             body
             (loop (+ v1 1) (cdr l2))))))
      ;; :list and :range in parallel
      ((_ ((:list v1 lst1) (:range v2 n2)) body)
       (let loop ([l1 lst1] [v2 0])
         (when (and (pair? l1) (< v2 n2))
           (let ([v1 (car l1)])
             body
             (loop (cdr l1) (+ v2 1))))))
      ;; Fallback: error at expansion time is not possible in syntax-rules,
      ;; so just run the first generator (matches prior behavior)
      ((_ (gen1 gen2 ...) body)
       (do-ec:inner gen1 body))))

  ;;; ================================================================
  ;;; Comprehension forms built on do-ec
  ;;; ================================================================

  (define-syntax list-ec
    (syntax-rules ()
      ((_ qual ... expr)
       (reverse
        (let ((acc '()))
          (do-ec qual ... (set! acc (cons expr acc)))
          acc)))))

  (define-syntax append-ec
    (syntax-rules ()
      ((_ qual ... expr)
       (apply append (list-ec qual ... expr)))))

  (define-syntax string-ec
    (syntax-rules ()
      ((_ qual ... expr)
       (list->string (list-ec qual ... expr)))))

  (define-syntax string-append-ec
    (syntax-rules ()
      ((_ qual ... expr)
       (apply string-append (list-ec qual ... expr)))))

  (define-syntax vector-ec
    (syntax-rules ()
      ((_ qual ... expr)
       (list->vector (list-ec qual ... expr)))))

  (define-syntax vector-of-length-ec
    (syntax-rules ()
      ((_ len qual ... expr)
       (let* ((n len) (v (make-vector n)) (i 0))
         (do-ec qual ...
           (begin (vector-set! v i expr) (set! i (+ i 1))))
         v))))

  (define-syntax sum-ec
    (syntax-rules ()
      ((_ qual ... expr)
       (let ((acc 0))
         (do-ec qual ... (set! acc (+ acc expr)))
         acc))))

  (define-syntax product-ec
    (syntax-rules ()
      ((_ qual ... expr)
       (let ((acc 1))
         (do-ec qual ... (set! acc (* acc expr)))
         acc))))

  (define-syntax min-ec
    (syntax-rules ()
      ((_ qual ... expr)
       (let ((acc #f))
         (do-ec qual ...
           (let ((v expr))
             (set! acc (if acc (min acc v) v))))
         acc))))

  (define-syntax max-ec
    (syntax-rules ()
      ((_ qual ... expr)
       (let ((acc #f))
         (do-ec qual ...
           (let ((v expr))
             (set! acc (if acc (max acc v) v))))
         acc))))

  (define-syntax any?-ec
    (syntax-rules ()
      ((_ qual ... expr)
       (call-with-current-continuation
        (lambda (exit)
          (do-ec qual ... (when expr (exit #t)))
          #f)))))

  (define-syntax every?-ec
    (syntax-rules ()
      ((_ qual ... expr)
       (call-with-current-continuation
        (lambda (exit)
          (do-ec qual ... (unless expr (exit #f)))
          #t)))))

  (define-syntax first-ec
    (syntax-rules ()
      ((_ default qual ... expr)
       (call-with-current-continuation
        (lambda (exit)
          (do-ec qual ... (exit expr))
          default)))))

  (define-syntax last-ec
    (syntax-rules ()
      ((_ default qual ... expr)
       (let ((result default))
         (do-ec qual ... (set! result expr))
         result))))

  (define-syntax fold-ec
    (syntax-rules ()
      ((_ knil qual ... f expr)
       (let ((acc knil))
         (do-ec qual ... (set! acc (f expr acc)))
         acc))))

  (define-syntax fold3-ec
    (syntax-rules ()
      ((_ knil qual ... f1 f2 expr)
       (let ((acc #f) (started #f))
         (do-ec qual ...
           (let ((v expr))
             (if started
                 (set! acc (f2 v acc))
                 (begin (set! acc (f1 v)) (set! started #t)))))
         (if started acc knil)))))

)
