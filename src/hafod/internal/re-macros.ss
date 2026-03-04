;;; (hafod internal re-macros) -- Regex macros for hafod
;;; Extracted from (hafod re) -- rx macro, match macros, SRE predicates.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod internal re-macros)
  (export
    rx
    let-match if-match match-cond
    if-sre-form sre-form?)
  (import (hafod internal base)
          (hafod compat)
          (for (hafod internal sre-compile) expand)
          (only (hafod internal re-engine) make-regexp match:substring))

  ;; ======================================================================
  ;; rx macro
  ;; ======================================================================

  ;; REG_ICASE constant for use in macro expansion (must be available at expand time)
  (meta define rx-REG_ICASE 2)

  (define-syntax rx
    (lambda (stx)
      (syntax-case stx ()
        ((_ sre ...)
         (let* ((datum (syntax->datum #'(sre ...)))
                (sre-form (if (= (length datum) 1) (car datum) (cons 'seq datum))))
           (let-values (((posix-str nparen fold? smap)
                         (compile-sre sre-form #f)))
             (let ((nsub (count-parens posix-str))
                   (cf (if fold? rx-REG_ICASE 0))
                   ;; Build submatch map: smap is a list of 1-based POSIX paren indices.
                   ;; If identity (1 2 3 ...), use #f. Otherwise, keep as a list for runtime.
                   (smap-data
                     (if (null? smap)
                         #f
                         (let check ((lst smap) (i 1))
                           (cond
                             ((null? lst) #f)  ; all identity
                             ((= (car lst) i) (check (cdr lst) (+ i 1)))
                             (else smap))))))  ; non-identity, keep list
               (with-syntax ((ps (datum->syntax #'_ posix-str))
                             (ns (datum->syntax #'_ nsub))
                             (cflag (datum->syntax #'_ cf)))
                 (if smap-data
                     ;; Non-identity: embed as (list->vector '(1 3 ...))
                     (with-syntax ((sm-list (datum->syntax #'_ smap-data)))
                       #'(make-regexp ps cflag ns (list->vector 'sm-list)))
                     ;; Identity or no submatches: use #f
                     #'(make-regexp ps cflag ns #f))))))))))

  ;; ======================================================================
  ;; Match macros: let-match, if-match, match-cond
  ;; Port of scsh/rx/re-match-syntax.scm
  ;; ======================================================================

  ;; let-match: bind match subfields to variables in body.
  ;; #f in the mvars list means skip that submatch index.
  (define-syntax let-match
    (syntax-rules ()
      ((let-match match-exp (mvars ...) body0 body ...)
       (let ((m match-exp))
         (let-match-aux m 0 (mvars ...) body0 body ...)))))

  (define-syntax let-match-aux
    (syntax-rules ()
      ((let-match-aux m i0 (#f mvars ...) body0 body ...)
       (let-match-aux m (+ 1 i0) (mvars ...) body0 body ...))
      ((let-match-aux m i0 (mvar0 mvars ...) body0 body ...)
       (let ((mvar0 (match:substring m i0)))
         (let-match-aux m (+ 1 i0) (mvars ...) body0 body ...)))
      ((let-match-aux m i0 () body0 body ...)
       (begin body0 body ...))))

  ;; if-match: conditional execution on match result.
  ;; If match-exp is truthy, bind mvars and eval on-match.
  ;; Otherwise eval no-match.
  (define-syntax if-match
    (syntax-rules ()
      ((if-match match-exp mvars on-match no-match)
       (cond (match-exp => (lambda (m) (let-match m mvars on-match)))
             (else no-match)))))

  ;; match-cond: cond-like multi-pattern dispatch.
  ;; Each clause is either:
  ;;   (match-exp mvars body ...) -- regex match clause
  ;;   (test cond-clause)         -- regular cond clause
  ;;   (else body ...)            -- default
  ;; match-cond implemented with syntax-case for proper keyword handling.
  ;; The 'test' and 'else' keywords are matched by symbol name (datum),
  ;; not by binding, ensuring cross-library compatibility.
  (define-syntax match-cond
    (lambda (stx)
      (define (keyword? id sym)
        (and (identifier? id) (eq? (syntax->datum id) sym)))
      (define (process-clauses clauses)
        (syntax-case clauses ()
          ;; No more clauses
          (()
           #'(cond))
          ;; (else body ...)
          (((kw body ...) . rest)
           (keyword? #'kw 'else)
           #'(cond (else body ...)))
          ;; (test . <cond-clause>)
          (((kw . cond-clause) . rest)
           (keyword? #'kw 'test)
           (with-syntax ((remaining (process-clauses #'rest)))
             (syntax-case #'remaining ()
               ((cond c ...)
                #'(cond cond-clause c ...)))))
          ;; (match-exp mvars body ...)
          (((match-exp mvars body ...) . rest)
           (with-syntax ((remaining (process-clauses #'rest)))
             (syntax-case #'remaining ()
               ((cond c ...)
                #'(cond (match-exp => (lambda (m)
                                        (let-match m mvars body ...)))
                        c ...)))))))
      (syntax-case stx ()
        ((_ clause ...)
         (process-clauses #'(clause ...))))))

  ;; ======================================================================
  ;; sre-form? and if-sre-form
  ;; Port of scsh/rx/re-syntax.scm
  ;; ======================================================================

  ;; sre-form-check : datum -> boolean
  ;; Shallow check: is the expression a valid SRE?
  ;; Shared implementation used at both expand time and runtime.
  (define (sre-form-check exp)
    (or (string? exp)
        (and (pair? exp)
             (let ((head (car exp)))
               (or (and (list? exp) (for-all string? exp) #t)
                   (and (memq head '(* + ? = >= **
                                      seq : or
                                      - & ~
                                      submatch dsm
                                      uncase w/case w/nocase
                                      unquote unquote-splicing
                                      posix-string))
                        #t)))
             #t)
        (and (symbol? exp)
             (memq exp '(any nonl bos eos bol eol
                         lower-case lower upper-case upper
                         alphabetic alpha numeric num digit
                         alphanumeric alphanum alnum
                         blank control cntrl
                         printing print punctuation punct
                         hex-digit hex xdigit
                         graphic graph whitespace white space
                         ascii))
             #t)))

  ;; Runtime-available version
  (define (sre-form? exp) (sre-form-check exp))

  ;; Expand-time version for if-sre-form macro
  (meta define (meta-sre-form? exp)
    (or (string? exp)
        (and (pair? exp)
             (let ((head (car exp)))
               (or (and (list? exp) (for-all string? exp))
                   (memq head '(* + ? = >= **
                                 seq : or
                                 - & ~
                                 submatch dsm
                                 uncase w/case w/nocase
                                 unquote unquote-splicing
                                 posix-string)))))
        (and (symbol? exp)
             (memq exp '(any nonl bos eos bol eol
                         lower-case lower upper-case upper
                         alphabetic alpha numeric num digit
                         alphanumeric alphanum alnum
                         blank control cntrl
                         printing print punctuation punct
                         hex-digit hex xdigit
                         graphic graph whitespace white space
                         ascii))
             #t)))

  ;; if-sre-form: compile-time SRE predicate macro.
  ;; If form is an SRE, expand to conseq, otherwise alt.
  (define-syntax if-sre-form
    (lambda (stx)
      (syntax-case stx ()
        ((_ form conseq alt)
         (if (meta-sre-form? (syntax->datum #'form))
             #'conseq
             #'alt)))))


  ) ; end library
