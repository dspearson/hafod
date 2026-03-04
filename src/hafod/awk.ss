;;; (hafod awk) -- AWK macro for field-oriented text processing
;;; Port of scsh/scheme/awk.scm for Chez Scheme R6RS using syntax-case.
;;; Copyright (c) 1994 by David Albertz and Olin Shivers.
;;; R6RS adaptation (c) 2026, hafod contributors.

(library (hafod awk)
  (export awk awk/posix-string
          next-range next-:range next-range: next-:range:)
  (import (hafod internal base)
          (for (hafod re) expand run)
          (hafod compat))

  ;; ======================================================================
  ;; Range helper functions
  ;; ======================================================================
  ;; Each returns (values this-record-active? next-state).

  (define (next-:range start-test stop-test state)
    (let ((new-state (if state
                         (or (not (stop-test)) (start-test))
                         (and (start-test) (not (stop-test))))))
      (values new-state new-state)))

  (define (next-range: start-test stop-test state)
    (values state
            (if state
                (or (not (stop-test)) (start-test))
                (and (start-test) (not (stop-test))))))

  (define (next-range start-test stop-test state)
    (if state
        (let ((not-stop (not (stop-test))))
          (values not-stop (or not-stop (start-test))))
        (values #f (and (start-test) (not (stop-test))))))

  (define (next-:range: start-test stop-test state)
    (if state
        (values #t (or (not (stop-test)) (start-test)))
        (let ((start? (start-test)))
          (values start? (and start? (not (stop-test)))))))

  ;; ======================================================================
  ;; AWK macro helper: test a value and update state
  ;; ======================================================================
  ;; Runtime helper that simplifies macro expansion.
  ;; If test-result is truthy:
  ;;   - call (action-thunk) which returns new state values
  ;;   - set else-fired to #f
  ;; Otherwise: return current state unchanged.
  ;; Returns (values else-state svar1 svar2 ...) as a list.

  ;; ======================================================================
  ;; AWK macro
  ;; ======================================================================

  (define-syntax awk
    (lambda (stx)

      (define (range-keyword? d)
        (and (symbol? d) (memq d '(range :range range: :range:))))

      (define (sre-form? stx)
        (let ((d (syntax->datum stx)))
          (or (string? d)
              (and (pair? d)
                   (memq (car d) '(seq or * + ? ** submatch dsm
                                    uncase w/case w/nocase posix-string
                                    - & ~ : bow eow)))
              (and (symbol? d)
                   (memq d '(any nonl bos eos bol eol
                             lower-case lower upper-case upper
                             alphabetic alpha numeric num digit
                             alphanumeric alphanum alnum blank
                             control cntrl printing print
                             punctuation punct hex-digit hex xdigit
                             graphic graph whitespace white space ascii))))))

      (syntax-case stx ()
        ((_ reader-exp (rec-var field-var ...) . rest)
         (let-values (((rec-counter state-inits clauses)
                       (let ((rest-list (syntax->list #'rest)))
                         (if (identifier? (car rest-list))
                             (values (car rest-list)
                                     (cadr rest-list)
                                     (cddr rest-list))
                             (values #f
                                     (car rest-list)
                                     (cdr rest-list))))))

           ;; Parse state var inits
           (let* ((si-list (syntax->list state-inits))
                  (svars (map (lambda (si) (car (syntax->list si))) si-list))
                  (sinits (map (lambda (si) (cadr (syntax->list si))) si-list))

                  ;; Analysis: do we need a counter? else var?
                  (clause-data (map syntax->datum clauses))
                  (needs-counter?
                    (or rec-counter
                        (exists (lambda (d)
                                  (and (pair? d)
                                       (or (integer? (car d))
                                           (and (range-keyword? (car d))
                                                (or (integer? (cadr d))
                                                    (integer? (caddr d)))))))
                                clause-data)))
                  (has-else?
                    (exists (lambda (d) (and (pair? d) (eq? (car d) 'else)))
                            clause-data))

                  ;; Counter var
                  (counter-var
                    (or rec-counter
                        (and needs-counter?
                             (datum->syntax #'rec-var (gensym "cnt")))))

                  ;; Collect SRE patterns for pre-compilation
                  (sre-pats
                    (let loop ((cls clauses) (pats '()))
                      (if (null? cls) (reverse pats)
                          (let* ((cl (car cls))
                                 (d (syntax->datum cl))
                                 (parts (syntax->list cl)))
                            (define (maybe-add-pat test-stx pats)
                              (if (sre-form? test-stx)
                                  (let ((dd (syntax->datum test-stx)))
                                    (if (member dd (map syntax->datum pats))
                                        pats
                                        (cons test-stx pats)))
                                  pats))
                            (cond
                              ((and (pair? d) (range-keyword? (car d)))
                               (loop (cdr cls)
                                     (maybe-add-pat (caddr parts)
                                       (maybe-add-pat (cadr parts) pats))))
                              ((and (pair? d) (memq (car d) '(after else)))
                               (loop (cdr cls) pats))
                              ((pair? d)
                               (loop (cdr cls)
                                     (maybe-add-pat (car parts) pats)))
                              (else (loop (cdr cls) pats)))))))

                  (sre-vars (map (lambda (p)
                                   (datum->syntax #'rec-var (gensym "re")))
                                 sre-pats))
                  (sre-map (map cons
                                (map syntax->datum sre-pats)
                                sre-vars))

                  ;; Range vars
                  (range-clauses
                    (filter (lambda (cl)
                              (let ((d (syntax->datum cl)))
                                (and (pair? d) (range-keyword? (car d)))))
                            clauses))
                  (range-vars (map (lambda (i)
                                     (datum->syntax #'rec-var (gensym "rng")))
                                   range-clauses))

                  ;; After clause
                  (after-cl
                    (let loop ((cls clauses))
                      (if (null? cls) #f
                          (let ((d (syntax->datum (car cls))))
                            (if (and (pair? d) (eq? (car d) 'after))
                                (car cls)
                                (loop (cdr cls)))))))

                  ;; Loop variables and inits
                  (loop-vars (append (if counter-var (list counter-var) '())
                                     range-vars
                                     svars))
                  (loop-inits (append (if counter-var (list #'0) '())
                                      (map (lambda (x) #'#f) range-vars)
                                      sinits))

                  ;; After expression
                  (after-expr
                    (if after-cl
                        (let ((parts (cdr (syntax->list after-cl))))
                          (if (= (length parts) 1)
                              (car parts)
                              (cons #'begin parts)))
                        (cond
                          ((null? svars) #'(void))
                          ((= (length svars) 1) (car svars))
                          (else (cons #'values svars)))))

                  ;; Else var
                  (else-var (and has-else?
                                 (datum->syntax #'rec-var (gensym "evar"))))

                  ;; Symbols
                  (lp-sym (datum->syntax #'rec-var 'lp))
                  (reader-sym (datum->syntax #'rec-var 'read-rec)))

             ;; Gen-test: convert a test form to a Scheme expression
             (define (gen-test test-stx for-value?)
               (let ((d (syntax->datum test-stx)))
                 (cond
                   ((integer? d)
                    (with-syntax ((cnt counter-var) (n test-stx))
                      #'(= cnt n)))
                   ((sre-form? test-stx)
                    (let ((re-var (cdr (assoc d sre-map))))
                      (with-syntax ((rv re-var) (rec #'rec-var))
                        (if for-value?
                            #'(regexp-search rv rec)
                            #'(regexp-search? rv rec)))))
                   ((and (pair? d) (eq? (car d) 'when))
                    (cadr (syntax->list test-stx)))
                   (else test-stx))))

             ;; Build the clause chain expression
             (define (build-chain clauses range-idx)
               (if (null? clauses)
                   ;; Loop back
                   (datum->syntax #'rec-var
                     (cons (syntax->datum lp-sym)
                           (map syntax->datum loop-vars)))
                   (let* ((clause (car clauses))
                          (rest (cdr clauses))
                          (d (syntax->datum clause))
                          (parts (syntax->list clause)))
                     (cond
                       ;; AFTER: skip
                       ((and (pair? d) (eq? (car d) 'after))
                        (build-chain rest range-idx))

                       ;; ELSE
                       ((and (pair? d) (eq? (car d) 'else))
                        (expand-else-clause parts rest range-idx))

                       ;; RANGE
                       ((and (pair? d) (range-keyword? (car d)))
                        (expand-range-clause parts rest range-idx))

                       ;; SIMPLE (including => and ==> forms)
                       (else
                        (expand-simple-clause parts rest range-idx))))))

             ;; Wrap clause result: when test fires, return (values #f sv...)
             ;; for else tracking; when it doesn't, return (values else-st sv...)
             ;; If no state vars and no else, just use when for side effects.
             (define (wrap-clause test-expr body-exprs tail has-arrow? arrow-tv)
               (let ((tail-e (datum->syntax #'rec-var tail)))
                 (cond
                   ;; No state vars, no else
                   ((and (null? svars) (not has-else?))
                    (if has-arrow?
                        (with-syntax ((te test-expr) (tl tail-e)
                                      (tv arrow-tv) ((bd ...) body-exprs))
                          #'(let ((tv te))
                              (when tv bd ...)
                              tl))
                        (with-syntax ((te test-expr) (tl tail-e)
                                      ((bd ...) body-exprs))
                          #'(begin (when te bd ...) tl))))

                   ;; No state vars, with else
                   ((and (null? svars) has-else?)
                    (if has-arrow?
                        (with-syntax ((te test-expr) (tl tail-e)
                                      (tv arrow-tv) (ev else-var)
                                      ((bd ...) body-exprs))
                          #'(let ((tv te))
                              (let ((ev (if tv (begin bd ... #f) ev)))
                                tl)))
                        (with-syntax ((te test-expr) (tl tail-e)
                                      (ev else-var) ((bd ...) body-exprs))
                          #'(let ((ev (if te (begin bd ... #f) ev)))
                              tl))))

                   ;; 1 state var, no else
                   ((and (= (length svars) 1) (not has-else?))
                    (let ((sv (car svars)))
                      (if has-arrow?
                          (with-syntax ((te test-expr) (tl tail-e)
                                        (tv arrow-tv) (s sv)
                                        ((bd ...) body-exprs))
                            #'(let ((tv te))
                                (let ((s (if tv (begin bd ...) s)))
                                  tl)))
                          (with-syntax ((te test-expr) (tl tail-e)
                                        (s sv) ((bd ...) body-exprs))
                            #'(let ((s (if te (begin bd ...) s)))
                                tl)))))

                   ;; 1 state var, with else
                   ((and (= (length svars) 1) has-else?)
                    (let ((sv (car svars)))
                      (if has-arrow?
                          (with-syntax ((te test-expr) (tl tail-e)
                                        (tv arrow-tv) (ev else-var) (s sv)
                                        ((bd ...) body-exprs))
                            #'(let ((tv te))
                                (let-values (((ev s) (if tv
                                                         (values #f (begin bd ...))
                                                         (values ev s))))
                                  tl)))
                          (with-syntax ((te test-expr) (tl tail-e)
                                        (ev else-var) (s sv) ((bd ...) body-exprs))
                            #'(let-values (((ev s) (if te
                                                        (values #f (begin bd ...))
                                                        (values ev s))))
                                tl)))))

                   ;; Multiple state vars, no else
                   ((not has-else?)
                    (if has-arrow?
                        (with-syntax ((te test-expr) (tl tail-e)
                                      (tv arrow-tv) ((s ...) svars)
                                      ((bd ...) body-exprs))
                          #'(let ((tv te))
                              (let-values (((s ...) (if tv
                                                        (begin bd ...)
                                                        (values s ...))))
                                tl)))
                        (with-syntax ((te test-expr) (tl tail-e)
                                      ((s ...) svars) ((bd ...) body-exprs))
                          #'(let-values (((s ...) (if te
                                                      (begin bd ...)
                                                      (values s ...))))
                              tl))))

                   ;; Multiple state vars, with else
                   (else
                    (if has-arrow?
                        (with-syntax ((te test-expr) (tl tail-e)
                                      (tv arrow-tv) (ev else-var)
                                      ((s ...) svars) ((bd ...) body-exprs))
                          #'(let ((tv te))
                              (let-values (((ev s ...) (if tv
                                                           (receive (s ...) (begin bd ...)
                                                             (values #f s ...))
                                                           (values ev s ...))))
                                tl)))
                        (with-syntax ((te test-expr) (tl tail-e)
                                      (ev else-var) ((s ...) svars)
                                      ((bd ...) body-exprs))
                          #'(let-values (((ev s ...) (if te
                                                         (receive (s ...) (begin bd ...)
                                                           (values #f s ...))
                                                         (values ev s ...))))
                              tl)))))))

             ;; Expand a simple clause
             (define (expand-simple-clause parts rest range-idx)
               (let* ((test-stx (car parts))
                      (test-d (syntax->datum test-stx))
                      (rest-parts (cdr parts))
                      (tail-datum (syntax->datum (build-chain rest range-idx))))
                 ;; Check for => arrow
                 (cond
                   ;; (test => proc)
                   ((and (= (length parts) 3)
                         (eq? (syntax->datum (cadr parts)) '=>))
                    (let* ((proc-e (caddr parts))
                           (test-e (gen-test test-stx #t))
                           (tv (datum->syntax #'rec-var (gensym "tv"))))
                      ;; For =>, the body is (proc tv), which returns new state
                      (wrap-clause test-e (list (with-syntax ((p proc-e) (t tv)) #'(p t)))
                                   tail-datum #t tv)))

                   ;; (test ==> (var ...) body ...)
                   ((and (> (length parts) 3)
                         (eq? (syntax->datum (cadr parts)) '==>))
                    (let* ((match-vars-stx (caddr parts))
                           (match-vars (syntax->list match-vars-stx))
                           (body (cdddr parts))
                           (test-e (gen-test test-stx #t))
                           (tv (datum->syntax #'rec-var (gensym "tv")))
                           ;; Build submatch binding indices (var . index) pairs
                           (bind-pairs
                             (let lp ((i 0) (vars match-vars) (acc '()))
                               (if (null? vars) (reverse acc)
                                   (let ((v (car vars)))
                                     (if (eq? (syntax->datum v) #f)
                                         (lp (+ i 1) (cdr vars) acc)
                                         (lp (+ i 1) (cdr vars)
                                             (cons (cons v i) acc)))))))
                           ;; Build body wrapped with let for submatch bindings
                           ;; Use with-syntax to keep tv identity consistent
                           (wrapped-body
                             (if (null? bind-pairs)
                                 body
                                 (list
                                   (with-syntax ((tvar tv)
                                                 (((bv . bi) ...) bind-pairs)
                                                 ((bd ...) body))
                                     #'(let ((bv (match:substring tvar bi)) ...)
                                         bd ...))))))
                      (wrap-clause test-e wrapped-body tail-datum #t tv)))

                   ;; Plain (test body ...)
                   (else
                    (let ((test-e (gen-test test-stx #f)))
                      (wrap-clause test-e rest-parts tail-datum #f #f))))))

             ;; Expand an else clause
             (define (expand-else-clause parts rest range-idx)
               (let* ((body (cdr parts))
                      (tail (build-chain rest range-idx))
                      (tail-datum (syntax->datum tail)))
                 (cond
                   ;; No state vars
                   ((null? svars)
                    (with-syntax ((ev else-var)
                                  ((bd ...) body)
                                  (tl (datum->syntax #'rec-var tail-datum)))
                      #'(begin (when ev bd ...) (let ((ev #t)) tl))))

                   ;; 1 state var
                   ((= (length svars) 1)
                    (with-syntax ((ev else-var) (s (car svars))
                                  ((bd ...) body)
                                  (tl (datum->syntax #'rec-var tail-datum)))
                      #'(let ((s (if ev (begin bd ...) s)))
                          (let ((ev #t)) tl))))

                   ;; Multiple state vars
                   (else
                    (with-syntax ((ev else-var) ((s ...) svars)
                                  ((bd ...) body)
                                  (tl (datum->syntax #'rec-var tail-datum)))
                      #'(let-values (((s ...) (if ev (begin bd ...) (values s ...))))
                          (let ((ev #t)) tl)))))))

             ;; Expand a range clause
             (define (expand-range-clause parts rest range-idx)
               (let* ((keyword (syntax->datum (car parts)))
                      (start-stx (cadr parts))
                      (stop-stx (caddr parts))
                      (body (cdddr parts))
                      (rvar (list-ref range-vars range-idx))
                      (tail (build-chain rest (+ range-idx 1)))
                      (tail-datum (syntax->datum tail))
                      (range-fn (case keyword
                                  ((range)  #'next-range)
                                  ((:range) #'next-:range)
                                  ((range:) #'next-range:)
                                  ((:range:) #'next-:range:)))
                      (start-e (gen-test start-stx #f))
                      (stop-e (gen-test stop-stx #f))
                      (active-sym (datum->syntax #'rec-var (gensym "act"))))

                 ;; Generate range update + conditional body execution
                 (cond
                   ;; No state vars, no else
                   ((and (null? svars) (not has-else?))
                    (with-syntax ((fn range-fn) (rv rvar) (act active-sym)
                                  (se start-e) (stp stop-e)
                                  ((bd ...) body)
                                  (tl (datum->syntax #'rec-var tail-datum)))
                      #'(let-values (((act rv) (fn (lambda () se) (lambda () stp) rv)))
                          (when act bd ...)
                          tl)))

                   ;; No state vars, with else
                   ((and (null? svars) has-else?)
                    (with-syntax ((fn range-fn) (rv rvar) (act active-sym)
                                  (ev else-var) (se start-e) (stp stop-e)
                                  ((bd ...) body)
                                  (tl (datum->syntax #'rec-var tail-datum)))
                      #'(let-values (((act rv) (fn (lambda () se) (lambda () stp) rv)))
                          (let ((ev (if act (begin bd ... #f) ev)))
                            tl))))

                   ;; 1 state var, no else
                   ((and (= (length svars) 1) (not has-else?))
                    (with-syntax ((fn range-fn) (rv rvar) (act active-sym)
                                  (s (car svars)) (se start-e) (stp stop-e)
                                  ((bd ...) body)
                                  (tl (datum->syntax #'rec-var tail-datum)))
                      #'(let-values (((act rv) (fn (lambda () se) (lambda () stp) rv)))
                          (let ((s (if act (begin bd ...) s)))
                            tl))))

                   ;; 1 state var, with else
                   ((and (= (length svars) 1) has-else?)
                    (with-syntax ((fn range-fn) (rv rvar) (act active-sym)
                                  (ev else-var) (s (car svars))
                                  (se start-e) (stp stop-e)
                                  ((bd ...) body)
                                  (tl (datum->syntax #'rec-var tail-datum)))
                      #'(let-values (((act rv) (fn (lambda () se) (lambda () stp) rv)))
                          (let-values (((ev s) (if act
                                                    (values #f (begin bd ...))
                                                    (values ev s))))
                            tl))))

                   ;; Multiple state vars, no else
                   ((not has-else?)
                    (with-syntax ((fn range-fn) (rv rvar) (act active-sym)
                                  ((s ...) svars) (se start-e) (stp stop-e)
                                  ((bd ...) body)
                                  (tl (datum->syntax #'rec-var tail-datum)))
                      #'(let-values (((act rv) (fn (lambda () se) (lambda () stp) rv)))
                          (let-values (((s ...) (if act (begin bd ...) (values s ...))))
                            tl))))

                   ;; Multiple state vars, with else
                   (else
                    (with-syntax ((fn range-fn) (rv rvar) (act active-sym)
                                  (ev else-var) ((s ...) svars)
                                  (se start-e) (stp stop-e)
                                  ((bd ...) body)
                                  (tl (datum->syntax #'rec-var tail-datum)))
                      #'(let-values (((act rv) (fn (lambda () se) (lambda () stp) rv)))
                          (let-values (((ev s ...) (if act
                                                       (receive (s ...) (begin bd ...)
                                                         (values #f s ...))
                                                       (values ev s ...))))
                            tl)))))))

             ;; Build the full expansion
             (let* ((clause-chain (build-chain clauses 0))
                    (chain-datum (syntax->datum clause-chain))
                    ;; Wrap with else-var init and counter increment
                    (loop-body-datum
                      (cond
                        ((and has-else? counter-var)
                         `(let ((,(syntax->datum counter-var) (+ ,(syntax->datum counter-var) 1))
                                (,(syntax->datum else-var) #t))
                            ,chain-datum))
                        (has-else?
                         `(let ((,(syntax->datum else-var) #t))
                            ,chain-datum))
                        (counter-var
                         `(let ((,(syntax->datum counter-var) (+ ,(syntax->datum counter-var) 1)))
                            ,chain-datum))
                        (else chain-datum)))

                    ;; Build regex init bindings
                    (re-bindings
                      (map (lambda (v p)
                             (list (syntax->datum v)
                                   `(rx ,(syntax->datum p))))
                           sre-vars sre-pats))

                    ;; Build loop var/init pairs
                    (loop-bindings
                      (map (lambda (v i) (list (syntax->datum v) (syntax->datum i)))
                           loop-vars loop-inits))

                    ;; Build the full form as datum
                    (full-form
                      `(let ((__reader__ (lambda () ,(syntax->datum #'reader-exp)))
                             ,@re-bindings)
                         (let ,(syntax->datum lp-sym) ,loop-bindings
                           (let-values (((,(syntax->datum #'rec-var)
                                          ,@(map syntax->datum (syntax->list #'(field-var ...))))
                                         (__reader__)))
                             (if (eof-object? ,(syntax->datum #'rec-var))
                                 ,(syntax->datum after-expr)
                                 ,loop-body-datum))))))

               (datum->syntax #'rec-var full-form))))))))

  ;; awk/posix-string: scsh-compatible alias (in scsh, awk delegates to awk/posix-string)
  (define-syntax awk/posix-string
    (identifier-syntax awk))

  ) ; end library
