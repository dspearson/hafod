;;; (hafod compat) -- Scheme48 compatibility shim for Chez Scheme
;;; Provides receive, let-optionals*, :optional, define-simple-syntax,
;;; check-arg, stringify, and vector utilities.
;;; Ported from scsh/scheme/utilities.scm and scsh/scheme/let-opt.scm
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod compat)
  (export receive let-optionals let-optionals* :optional
          define-simple-syntax check-arg stringify
          mapv mapv! vector-every? copy-vector initialize-vector vector-append
          vfold vfold-right bogus-substring-spec? deprecated-proc real->exact-integer
          define-scsh-accessors
          arithmetic-shift
          ;; scsh utility functions
          warn ascii->char char->ascii ->char-set
          ;; scsh char predicates (SRFI-14 based)
          char-letter? char-digit? char-letter+digit? char-graphic?
          char-printing? char-blank? char-iso-control? char-punctuation?
          char-symbol? char-hex-digit? char-ascii? char-alphanumeric?
          ;; SRFI-14 char-set re-exports for scsh compatibility
          char-set? char-set char-set-contains?
          char-set:letter char-set:digit char-set:letter+digit
          char-set:lower-case char-set:upper-case
          char-set:punctuation char-set:graphic char-set:printing
          char-set:control char-set:hex-digit char-set:blank
          char-set:ascii char-set:whitespace char-set:newline char-set:any
          char-set:full char-set:empty
          char-set-complement char-set-union char-set-intersection
          char-set-difference char-set-fold char-set-size char-set->list
          char-set= char-set-empty? char-set-full?
          char-set-copy char-set-adjoin char-set-adjoin! char-set-delete
          string->char-set)
  (import (hafod internal base)
          (hafod internal char-sets))

  ;;; ========== receive (SRFI-8) ==========
  ;; (receive formals expression body ...)
  ;; Binds multiple return values from expression.
  (define-syntax receive
    (syntax-rules ()
      ((receive formals expression body ...)
       (call-with-values (lambda () expression)
         (lambda formals body ...)))))

  ;;; ========== :optional ==========
  ;; (:optional rest default-exp [arg-test])
  ;; For procedures taking a single optional argument.
  (define-syntax :optional
    (syntax-rules ()
      ((:optional rest default-exp)
       (let ((maybe-arg rest))
         (if (pair? maybe-arg)
             (if (null? (cdr maybe-arg)) (car maybe-arg)
                 (error ':optional "too many optional arguments" maybe-arg))
             default-exp)))
      ((:optional rest default-exp arg-test)
       (let ((maybe-arg rest))
         (if (pair? maybe-arg)
             (if (null? (cdr maybe-arg))
                 (let ((val (car maybe-arg)))
                   (if (arg-test val) val
                       (error ':optional "Optional argument failed test"
                              'arg-test val)))
                 (error ':optional "too many optional arguments" maybe-arg))
             default-exp)))))

  ;;; ========== let-optionals* ==========
  ;; (let-optionals* args ((var default [arg-test [supplied?]]) ... [rest-var]) body ...)
  ;; Sequential optional argument destructuring.
  (define-syntax let-optionals*
    (syntax-rules ()
      ;; Base case: no more bindings
      ((let-optionals* args () body ...)
       (begin body ...))
      ;; Binding with default and arg-test and supplied? flag
      ((let-optionals* args ((var default arg-test supplied?) . rest) body ...)
       (let ((supplied? (pair? args))
             (var (if (pair? args)
                      (let ((v (car args)))
                        (if (arg-test v) v
                            (error 'let-optionals* "argument failed test" 'arg-test v)))
                      default))
             (remaining (if (pair? args) (cdr args) '())))
         (let-optionals* remaining rest body ...)))
      ;; Binding with default and arg-test
      ((let-optionals* args ((var default arg-test) . rest) body ...)
       (let ((var (if (pair? args)
                      (let ((v (car args)))
                        (if (arg-test v) v
                            (error 'let-optionals* "argument failed test" 'arg-test v)))
                      default))
             (remaining (if (pair? args) (cdr args) '())))
         (let-optionals* remaining rest body ...)))
      ;; Binding with default only
      ((let-optionals* args ((var default) . rest) body ...)
       (let ((var (if (pair? args) (car args) default))
             (remaining (if (pair? args) (cdr args) '())))
         (let-optionals* remaining rest body ...)))
      ;; Rest variable (bare symbol as final element -- must be LAST to avoid
      ;; matching (var default) pairs before the binding clauses above)
      ((let-optionals* args (rest-var) body ...)
       (let ((rest-var args))
         body ...))))

  ;;; ========== let-optionals ==========
  ;; Same as let-optionals* but with LET scope (parallel binding).
  ;; In practice, sequential and parallel rarely differ for optional args.
  ;; Implement identically to let-optionals* for now.
  (define-syntax let-optionals
    (syntax-rules ()
      ((let-optionals args () body ...)
       (begin body ...))
      ((let-optionals args ((var default arg-test supplied?) . rest) body ...)
       (let ((supplied? (pair? args))
             (var (if (pair? args)
                      (let ((v (car args)))
                        (if (arg-test v) v
                            (error 'let-optionals "argument failed test" 'arg-test v)))
                      default))
             (remaining (if (pair? args) (cdr args) '())))
         (let-optionals remaining rest body ...)))
      ((let-optionals args ((var default arg-test) . rest) body ...)
       (let ((var (if (pair? args)
                      (let ((v (car args)))
                        (if (arg-test v) v
                            (error 'let-optionals "argument failed test" 'arg-test v)))
                      default))
             (remaining (if (pair? args) (cdr args) '())))
         (let-optionals remaining rest body ...)))
      ((let-optionals args ((var default) . rest) body ...)
       (let ((var (if (pair? args) (car args) default))
             (remaining (if (pair? args) (cdr args) '())))
         (let-optionals remaining rest body ...)))
      ((let-optionals args (rest-var) body ...)
       (let ((rest-var args))
         body ...))))

  ;;; ========== define-simple-syntax ==========
  (define-syntax define-simple-syntax
    (syntax-rules ()
      ((define-simple-syntax (name . pattern) result)
       (define-syntax name (syntax-rules () ((name . pattern) result))))))

  ;;; ========== check-arg ==========
  ;; (check-arg pred val caller) => val if (pred val), else error.
  (define (check-arg pred val caller)
    (if (pred val) val
        (error caller "Bad argument" val pred)))

  ;;; ========== stringify ==========
  ;; Converts strings, symbols, and integers to strings.
  (define (stringify thing)
    (cond ((string? thing) thing)
          ((symbol? thing) (symbol->string thing))
          ((integer? thing) (number->string thing))
          (else (error 'stringify
                       "Can only stringify strings, symbols, and integers."
                       thing))))

  ;;; ========== Vector operations ==========

  (define (mapv f v)
    (let* ((len (vector-length v))
           (ans (make-vector len)))
      (do ((i 0 (+ i 1)))
          ((= i len) ans)
        (vector-set! ans i (f (vector-ref v i))))))

  (define (mapv! f v)
    (let ((len (vector-length v)))
      (do ((i 0 (+ i 1)))
          ((= i len) v)
        (vector-set! v i (f (vector-ref v i))))))

  (define (vector-every? pred v)
    (let lp ((i (- (vector-length v) 1)))
      (or (< i 0)
          (and (pred (vector-ref v i))
               (lp (- i 1))))))

  (define (copy-vector v)
    (let* ((len (vector-length v))
           (ans (make-vector len)))
      (do ((i (- len 1) (- i 1)))
          ((< i 0) ans)
        (vector-set! ans i (vector-ref v i)))))

  (define (initialize-vector len init)
    (let ((v (make-vector len)))
      (do ((i (- len 1) (- i 1)))
          ((< i 0) v)
        (vector-set! v i (init i)))))

  ;; vector-append uses fold-left (Chez) instead of SRFI-1 fold.
  ;; SRFI-1 fold: (fold (lambda (v len) ...) 0 vecs) -- (kons elem acc)
  ;; Chez fold-left: (fold-left (lambda (len v) ...) 0 vecs) -- (kons acc elem)
  (define (vector-append . vecs)
    (let* ((vlen (fold-left (lambda (len v) (+ (vector-length v) len)) 0 vecs))
           (ans (make-vector vlen)))
      (let lp1 ((vecs vecs) (to 0))
        (if (pair? vecs)
            (let* ((vec (car vecs))
                   (len (vector-length vec)))
              (let lp2 ((from 0) (to to))
                (cond ((< from len)
                       (vector-set! ans to (vector-ref vec from))
                       (lp2 (+ from 1) (+ to 1)))
                      (else (lp1 (cdr vecs) to)))))))
      ans))

  ;;; ========== vfold / vfold-right ==========

  (define (vfold kons knil v)
    (let ((len (vector-length v)))
      (do ((i 0 (+ i 1))
           (ans knil (kons (vector-ref v i) ans)))
          ((>= i len) ans))))

  (define (vfold-right kons knil v)
    (do ((i (- (vector-length v) 1) (- i 1))
         (ans knil (kons (vector-ref v i) ans)))
        ((< i 0) ans)))

  ;;; ========== bogus-substring-spec? ==========
  (define (bogus-substring-spec? s start end)
    (or (< start 0)
        (< (string-length s) end)
        (< end start)))

  ;;; ========== deprecated-proc ==========
  ;; Wraps proc with a one-time deprecation warning.
  (define (deprecated-proc proc name . maybe-preferred-msg)
    (let ((warned? #f))
      (lambda args
        (cond ((not warned?)
               (set! warned? #t)
               (display "WARNING: Deprecated procedure " (current-error-port))
               (display name (current-error-port))
               (when (pair? maybe-preferred-msg)
                 (display " -- " (current-error-port))
                 (display (car maybe-preferred-msg) (current-error-port)))
               (newline (current-error-port))))
        (apply proc args))))

  ;;; ========== real->exact-integer ==========
  (define (real->exact-integer x)
    (let ((f (round x)))
      (if (inexact? f) (inexact->exact f) f)))

  ;;; ========== define-scsh-accessors ==========
  ;; (define-scsh-accessors (alias target) ...)
  ;; Generates (define alias target) for each pair.
  (define-syntax define-scsh-accessors
    (lambda (stx)
      (syntax-case stx ()
        [(_ (alias target) ...)
         #'(begin
             (define alias target) ...)])))

  ;; scsh-compatible alias for Chez's ash (arithmetic shift)
  (define arithmetic-shift ash)

  ;;; ========== warn ==========
  ;; Non-fatal warning output (to current-error-port).
  (define (warn who message . irritants)
    (let ([p (current-error-port)])
      (display "Warning" p)
      (when who
        (display ": " p)
        (display who p))
      (display ": " p)
      (display message p)
      (for-each (lambda (x) (display " " p) (write x p)) irritants)
      (newline p)))

  ;;; ========== ascii->char / char->ascii ==========
  (define ascii->char integer->char)
  (define char->ascii char->integer)

  ;;; ========== ->char-set ==========
  ;; SRFI-14 coercion (scsh name for x->char-set)
  (define ->char-set x->char-set)

  ;;; ========== scsh char predicates ==========
  (define (char-letter? c) (char-set-contains? char-set:letter c))
  (define (char-digit? c) (char-set-contains? char-set:digit c))
  (define (char-letter+digit? c) (char-set-contains? char-set:letter+digit c))
  (define char-alphanumeric? char-letter+digit?)
  (define (char-graphic? c) (char-set-contains? char-set:graphic c))
  (define (char-printing? c) (char-set-contains? char-set:printing c))
  (define (char-blank? c) (char-set-contains? char-set:blank c))
  (define (char-iso-control? c) (char-set-contains? char-set:control c))
  (define (char-punctuation? c) (char-set-contains? char-set:punctuation c))
  (define (char-hex-digit? c) (char-set-contains? char-set:hex-digit c))
  (define (char-ascii? c) (char-set-contains? char-set:ascii c))

  ;; char-symbol?: ASCII symbol characters (Sm, Sc, Sk, So categories)
  (define char-set:symbol (string->char-set "$+<=>^`|~"))
  (define (char-symbol? c) (char-set-contains? char-set:symbol c))

  ) ; end library
