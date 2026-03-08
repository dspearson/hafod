;;; (hafod internal re-parse) -- SRE/POSIX parsers for hafod
;;; Extracted from (hafod re) -- sre->regexp, regexp->sre, posix-string->regexp.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod internal re-parse)
  (export sre->regexp regexp->sre posix-string->regexp)
  (import (hafod internal base)
          (hafod compat)
          (except (hafod internal re-records) regexp?)
          (rename (only (hafod internal re-records) regexp?)
                  (regexp? re-adt?)))

  ;; ======================================================================
  ;; sre->regexp: Parse an SRE s-expression datum into an RE ADT tree.
  ;; Port of scsh/rx/parse.scm's sre->regexp (runtime version).
  ;; Uses identity rename and equal? compare (no macro hygiene needed).
  ;; ======================================================================

  (define (sre->regexp sre)
    (parse-sre-runtime sre #t))

  ;; Main runtime SRE parser.
  ;; case-sensitive?: lexical case-sensitivity context.
  (define (parse-sre-runtime sre case-sensitive?)
    (let recur ((sre sre) (case-sensitive? case-sensitive?))
      (define (parse-seq elts)
        (re-seq (map (lambda (s) (recur s case-sensitive?)) elts)))
      (define (parse-seq/cs elts cs?)
        (re-seq (map (lambda (s) (recur s cs?)) elts)))
      (cond
        ;; String literal
        ((string? sre)
         (if case-sensitive?
             (make-re-string sre)
             (uncase-string sre)))

        ;; Char literal (scsh supports chars in runtime SREs)
        ((char? sre)
         (if case-sensitive?
             (make-re-string (string sre))
             (uncase-string (string sre))))

        ;; Symbol: named char classes and anchors
        ((symbol? sre)
         (case sre
           ((any)  re-any)
           ((nonl) re-nonl)
           ((bos)  re-bos)
           ((eos)  re-eos)
           ((bol)  re-bol)
           ((eol)  re-eol)
           ((lower-case lower)          (make-re-char-set char-set:lower-case))
           ((upper-case upper)          (make-re-char-set char-set:upper-case))
           ((alphabetic alpha)          (make-re-char-set char-set:letter))
           ((numeric digit num)         (make-re-char-set char-set:digit))
           ((alphanumeric alnum alphanum) (make-re-char-set char-set:letter+digit))
           ((punctuation punct)         (make-re-char-set char-set:punctuation))
           ((graphic graph)             (make-re-char-set char-set:graphic))
           ((blank)                     (make-re-char-set char-set:blank))
           ((whitespace space white)    (make-re-char-set char-set:whitespace))
           ((printing print)            (make-re-char-set char-set:printing))
           ((control cntrl)             (make-re-char-set char-set:control))
           ((hex-digit xdigit hex)      (make-re-char-set char-set:hex-digit))
           ((ascii)                     (make-re-char-set char-set:ascii))
           (else (error 'sre->regexp "unknown SRE symbol" sre))))

        ;; Compound forms
        ((pair? sre)
         (let ((head (car sre))
               (rest (cdr sre)))
           (cond
             ;; Char-set form: ("aeiou" "xyz") -- list of strings
             ((string? head)
              (let ((cs (apply char-set-union (map string->char-set (filter string? sre)))))
                (if case-sensitive?
                    (make-re-char-set cs)
                    (uncase-char-set cs))))

             ;; Symbolic compound forms
             ((symbol? head)
              (case head
                ;; Sequence
                ((seq :)
                 (parse-seq rest))

                ;; Alternation
                ((or)
                 (re-choice (map (lambda (s) (recur s case-sensitive?)) rest)))

                ;; Repetition
                ((*) (re-repeat 0 #f (parse-seq rest)))
                ((+) (re-repeat 1 #f (parse-seq rest)))
                ((?) (re-repeat 0 1  (parse-seq rest)))

                ;; Exact repetition: (= n re ...)
                ((=) (let ((n (car rest)))
                       (re-repeat n n (parse-seq (cdr rest)))))

                ;; At-least repetition: (>= n re ...)
                ((>=) (let ((n (car rest)))
                        (re-repeat n #f (parse-seq (cdr rest)))))

                ;; Bounded repetition: (** m n re ...)
                ((**) (let ((m (car rest))
                            (n (cadr rest)))
                        (re-repeat m n (parse-seq (cddr rest)))))

                ;; Submatch
                ((submatch) (re-submatch (parse-seq rest)))

                ;; Dead submatch
                ((dsm) (re-dsm (parse-seq (cddr rest))
                               (car rest) (cadr rest)))

                ;; Case folding context
                ((w/nocase) (parse-seq/cs rest #f))
                ((w/case)   (parse-seq/cs rest #t))

                ;; Explicit uncase
                ((uncase)   (uncase (parse-seq rest)))

                ;; Complement: (~ cset ...)
                ((~)
                 (let ((cs (apply char-set-union
                                  (map (lambda (s) (parse-sre-char-class s case-sensitive?))
                                       rest))))
                   (make-re-char-set (char-set-complement cs))))

                ;; Intersection: (& cset ...)
                ((&)
                 (let ((cs (apply char-set-intersection
                                  (map (lambda (s) (parse-sre-char-class s case-sensitive?))
                                       rest))))
                   (make-re-char-set cs)))

                ;; Difference: (- cset ...)
                ((-)
                 (if (null? rest)
                     (error 'sre->regexp "- requires at least one argument" sre)
                     (let ((cs1 (parse-sre-char-class (car rest) case-sensitive?))
                           (cs2 (if (null? (cdr rest))
                                    char-set:empty
                                    (apply char-set-union
                                           (map (lambda (s) (parse-sre-char-class s case-sensitive?))
                                                (cdr rest))))))
                       (make-re-char-set (char-set-difference cs1 cs2)))))

                ;; POSIX string passthrough
                ((posix-string)
                 (posix-string->regexp (car rest)))

                ;; Dynamic forms: for runtime, evaluate the contained value
                ((unquote)
                 (let ((val (cadr sre)))
                   (cond
                     ((string? val)   (flush-submatches (make-re-string val)))
                     ((char? val)     (flush-submatches (make-re-string (string val))))
                     ((re-adt? val)   (flush-submatches val))
                     (else (error 'sre->regexp "cannot coerce unquote value to regexp" val)))))

                (else (error 'sre->regexp "unknown SRE form" sre))))

             (else (error 'sre->regexp "illegal SRE" sre)))))

        (else (error 'sre->regexp "unrecognized SRE" sre)))))

  ;; Helper: parse an SRE in char-class context, returning a char-set.
  (define (parse-sre-char-class sre case-sensitive?)
    (cond
      ((string? sre) (string->char-set sre))
      ((char? sre) (char-set sre))
      ((symbol? sre)
       (case sre
         ((any)                         char-set:full)
         ((nonl)                        (char-set-complement (char-set #\newline)))
         ((lower-case lower)            char-set:lower-case)
         ((upper-case upper)            char-set:upper-case)
         ((alphabetic alpha)            char-set:letter)
         ((numeric digit num)           char-set:digit)
         ((alphanumeric alnum alphanum) char-set:letter+digit)
         ((punctuation punct)           char-set:punctuation)
         ((graphic graph)               char-set:graphic)
         ((blank)                       char-set:blank)
         ((whitespace space white)      char-set:whitespace)
         ((printing print)              char-set:printing)
         ((control cntrl)               char-set:control)
         ((hex-digit xdigit hex)        char-set:hex-digit)
         ((ascii)                       char-set:ascii)
         (else (error 'sre->regexp "unknown char-class name" sre))))
      ((and (pair? sre) (string? (car sre)))
       (apply char-set-union (map string->char-set (filter string? sre))))
      ((and (pair? sre) (symbol? (car sre)))
       (case (car sre)
         ((~) (char-set-complement
               (apply char-set-union
                      (map (lambda (s) (parse-sre-char-class s case-sensitive?))
                           (cdr sre)))))
         ((&) (apply char-set-intersection
                     (map (lambda (s) (parse-sre-char-class s case-sensitive?))
                          (cdr sre))))
         ((-) (let ((cs1 (parse-sre-char-class (cadr sre) case-sensitive?))
                    (cs2 (apply char-set-union
                                (map (lambda (s) (parse-sre-char-class s case-sensitive?))
                                     (cddr sre)))))
                (char-set-difference cs1 cs2)))
         ((or) (apply char-set-union
                      (map (lambda (s) (parse-sre-char-class s case-sensitive?))
                           (cdr sre))))
         (else (error 'sre->regexp "unsupported char-class form" sre))))
      (else (error 'sre->regexp "cannot parse as char-class" sre))))


  ;; ======================================================================
  ;; regexp->sre: Unparse an RE ADT tree back into an SRE s-expression.
  ;; Port of scsh/rx/parse.scm's regexp->sre.
  ;; ======================================================================

  (define (regexp->sre re)
    (regexp->sre/renamer re (lambda (x) x)))

  ;; Unparse to a *list* of SREs (representing implicit sequence body).
  (define (regexp->sres/renamer re r)
    (if (re-seq? re)
        (let ((elts (re-seq:elts re)))
          (if (pair? elts)
              (map (lambda (re) (regexp->sre/renamer re r)) elts)
              (let ((tsm (re-seq:tsm re)))
                (if (zero? tsm) '() (list (list (r 'dsm) tsm 0))))))
        (list (regexp->sre/renamer re r))))

  (define (regexp->sre/renamer re r)
    (let recur ((re re))
      (cond
        ;; Special case: re-trivial is a DSM wrapping empty-string
        ((re-trivial? re) "")

        ((re-string? re)
         (re-string:chars re))

        ((re-seq? re)
         (cons (r ':) (regexp->sres/renamer re r)))

        ((re-choice? re)
         (let ((elts (re-choice:elts re)))
           (if (pair? elts)
               (cons (r 'or) (map recur elts))
               (let ((tsm (re-choice:tsm re)))
                 (if (zero? tsm) (list (r 'or)) (list (r 'dsm) tsm 0 (list (r 'or))))))))

        ((re-char-set? re)
         (let ((cs (re-char-set:cset re)))
           (cond
             ((char-set-full? cs) (r 'any))
             ((char-set-empty? cs) (list (r 'or)))
             (else
              ;; Try to classify as a named char class
              (let ((name (try-classify-cset cs)))
                (if name
                    (r name)
                    ;; Fall back to rendering as string set for small sets
                    (let ((chars (char-set->list cs)))
                      (if (= 1 (length chars))
                          (string (car chars))
                          (list (list->string chars))))))))))

        ((re-repeat? re)
         (let ((from (re-repeat:from re))
               (to (re-repeat:to re))
               (bodies (regexp->sres/renamer (re-repeat:body re) r)))
           (cond ((and (eqv? from 0) (not to))    (cons (r '*) bodies))
                 ((and (eqv? from 0) (eqv? to 1)) (cons (r '?) bodies))
                 ((and (eqv? from 1) (not to))    (cons (r '+) bodies))
                 ((eqv? from to)                  (cons (r '=) (cons to bodies)))
                 (to                              (cons (r '**) (cons from (cons to bodies))))
                 (else                            (cons (r '>=) (cons from bodies))))))

        ((re-dsm? re)
         (cons (r 'dsm)
               (cons (re-dsm:pre-dsm re)
                     (cons (re-dsm:post-dsm re)
                           (regexp->sres/renamer (re-dsm:body re) r)))))

        ((re-submatch? re)
         (cons (r 'submatch) (regexp->sres/renamer (re-submatch:body re) r)))

        ((re-bos? re) (r 'bos))
        ((re-eos? re) (r 'eos))
        ((re-bol? re) (r 'bol))
        ((re-eol? re) (r 'eol))

        (else re))))

  ;; Try to classify a char-set as a named SRE class.
  ;; Returns a symbol name or #f.
  (define (try-classify-cset cs)
    (cond
      ((char-set= cs char-set:full)         'any)
      ((char-set= cs char-set:lower-case)   'lower-case)
      ((char-set= cs char-set:upper-case)   'upper-case)
      ((char-set= cs char-set:letter)       'alphabetic)
      ((char-set= cs char-set:digit)        'numeric)
      ((char-set= cs char-set:letter+digit) 'alphanumeric)
      ((char-set= cs char-set:punctuation)  'punctuation)
      ((char-set= cs char-set:graphic)      'graphic)
      ((char-set= cs char-set:blank)        'blank)
      ((char-set= cs char-set:whitespace)   'whitespace)
      ((char-set= cs char-set:printing)     'printing)
      ((char-set= cs char-set:control)      'control)
      ((char-set= cs char-set:hex-digit)    'hex-digit)
      ((char-set= cs char-set:ascii)        'ascii)
      (else #f)))


  ;; ======================================================================
  ;; posix-string->regexp: Parse a POSIX ERE string into an RE ADT tree.
  ;; Port of scsh/rx/spencer.scm.
  ;; ======================================================================

  (define (posix-string->regexp s)
    (let-values (((re i) (parse-posix-exp s 0)))
      (if (= i (string-length s))
          re
          (error 'posix-string->regexp "illegal POSIX regexp -- terminated early" s i))))

  ;; A complete expression is a sequence of |-separated branches.
  (define (parse-posix-exp s i)
    (let ((len (string-length s)))
      (if (< i len)
          (let lp ((i i) (branches '()))
            (let-values (((branch i) (parse-posix-branch s i)))
              (let ((branches (cons branch branches)))
                (if (and (< i len) (char=? #\| (string-ref s i)))
                    (lp (+ i 1) branches)
                    (values (re-choice (reverse branches)) i)))))
          (values re-trivial i))))

  ;; A branch is a sequence of pieces.
  (define (parse-posix-branch s i)
    (let ((len (string-length s)))
      (let lp ((i i) (pieces '()))
        (if (< i len)
            (let-values (((piece i) (parse-posix-piece s i)))
              (if piece
                  (lp i (cons piece pieces))
                  (values (re-seq (reverse pieces)) i)))
            (values (re-seq (reverse pieces)) i)))))

  ;; A piece is an atom possibly followed by a quantifier.
  (define (parse-posix-piece s i)
    (let ((len (string-length s)))
      (let-values (((atom i) (parse-posix-atom s i)))
        (if (not atom)
            (values #f i)
            (if (< i len)
                (case (string-ref s i)
                  ((#\* #\+ #\?)
                   (let-values (((from to)
                                 (case (string-ref s i)
                                   ((#\*) (values 0 #f))
                                   ((#\+) (values 1 #f))
                                   ((#\?) (values 0 1)))))
                     (values (re-repeat from to atom) (+ i 1))))
                  ((#\{)
                   (let-values (((from to i) (parse-posix-braces s (+ i 1))))
                     (values (re-repeat from to atom) i)))
                  (else (values atom i)))
                (values atom i))))))

  ;; An atom: literal, ., ^, $, [...], (...), \x
  (define (parse-posix-atom s i)
    (let ((len (string-length s)))
      (if (< i len)
          (let ((c (string-ref s i)))
            (case c
              ((#\^) (values re-bos (+ i 1)))
              ((#\$) (values re-eos (+ i 1)))
              ((#\.) (values re-any (+ i 1)))

              ((#\[) (parse-posix-bracket s (+ i 1)))

              ((#\()
               (let-values (((re i) (parse-posix-exp s (+ i 1))))
                 (if (and (< i len) (char=? #\) (string-ref s i)))
                     (values (re-submatch re) (+ i 1))
                     (error 'posix-string->regexp
                            "subexpression has no closing parenthesis" s i))))

              ((#\\)
               (let ((i2 (+ i 1)))
                 (if (< i2 len)
                     (values (make-re-string (string (string-ref s i2))) (+ i2 1))
                     (error 'posix-string->regexp "regexp may not end with backslash" s))))

              ;; ) | * + ? { are not atoms -- signal end of branch/piece
              ((#\) #\|) (values #f i))

              (else (values (make-re-string (string c)) (+ i 1)))))
          (values #f i))))

  ;; Parse a [...] or [^...] bracket expression.
  (define (parse-posix-bracket s i)
    (let ((len (string-length s)))
      (if (>= i len)
          (error 'posix-string->regexp "missing close bracket" s i)
          (let-values (((negate? i0)
                        (if (char=? (string-ref s i) #\^)
                            (values #t (+ i 1))
                            (values #f i))))
            (let lp ((i i0) (cset (char-set-copy char-set:empty)))
              (if (>= i len)
                  (error 'posix-string->regexp "missing close bracket" s i)
                  (let ((c (string-ref s i))
                        (i1 (+ i 1)))
                    (cond
                      ;; ] as first char is literal
                      ((and (char=? c #\]) (= i i0))
                       (lp i1 (char-set-adjoin! cset #\])))

                      ;; Closing ]
                      ((char=? c #\])
                       (let ((result-cset (if negate?
                                              (char-set-complement cset)
                                              cset)))
                         (values (make-re-char-set result-cset) i1)))

                      ;; - as first or last is literal
                      ((char=? c #\-)
                       (if (or (= i i0)
                               (and (< i1 len) (char=? #\] (string-ref s i1))))
                           (lp i1 (char-set-adjoin! cset #\-))
                           (error 'posix-string->regexp "illegal - in bracket" s i)))

                      ;; [ for possible POSIX class like [:alpha:]
                      ((char=? c #\[)
                       (if (and (< i1 len) (char=? (string-ref s i1) #\:))
                           ;; POSIX named class [:name:]
                           (let* ((close (posix-bracket-class-end s (+ i 2)))
                                  (name (substring s (+ i 2) close))
                                  (named-cs (posix-class-name->char-set name)))
                             (if named-cs
                                 (lp (+ close 2) (char-set-union cset named-cs))
                                 (error 'posix-string->regexp "unknown POSIX class" name)))
                           (lp i1 (char-set-adjoin! cset #\[))))

                      ;; Regular char, possibly start of range
                      (else
                       (if (and (< (+ i1 1) len)
                                (char=? #\- (string-ref s i1))
                                (not (char=? #\] (string-ref s (+ i1 1)))))
                           ;; Range c-to
                           (let ((to (string-ref s (+ i1 1))))
                             (let range-lp ((j (char->integer c)) (cs cset))
                               (if (> j (char->integer to))
                                   (lp (+ i1 2) cs)
                                   (range-lp (+ j 1) (char-set-adjoin! cs (integer->char j))))))
                           ;; Just a char
                           (lp i1 (char-set-adjoin! cset c))))))))))))

  ;; Find the end of a POSIX bracket class name: look for :] starting at i.
  (define (posix-bracket-class-end s i)
    (let ((len (string-length s)))
      (let lp ((j i))
        (if (>= j (- len 1))
            (error 'posix-string->regexp "unterminated POSIX class" s i)
            (if (and (char=? (string-ref s j) #\:)
                     (char=? (string-ref s (+ j 1)) #\]))
                j
                (lp (+ j 1)))))))

  ;; Map POSIX class name to char-set.
  (define (posix-class-name->char-set name)
    (cond
      ((string=? name "alpha")  char-set:letter)
      ((string=? name "digit")  char-set:digit)
      ((string=? name "alnum")  char-set:letter+digit)
      ((string=? name "lower")  char-set:lower-case)
      ((string=? name "upper")  char-set:upper-case)
      ((string=? name "space")  char-set:whitespace)
      ((string=? name "punct")  char-set:punctuation)
      ((string=? name "graph")  char-set:graphic)
      ((string=? name "print")  char-set:printing)
      ((string=? name "cntrl")  char-set:control)
      ((string=? name "xdigit") char-set:hex-digit)
      ((string=? name "blank")  char-set:blank)
      (else #f)))

  ;; Parse {m,n}, {m}, {m,} braces.
  (define (parse-posix-braces s i)
    (let ((len (string-length s)))
      (let find-rb ((j i))
        (if (>= j len)
            (error 'posix-string->regexp "missing close brace" s i)
            (if (char=? (string-ref s j) #\})
                ;; Found closing brace at j
                (let find-comma ((k i))
                  (if (and (< k j) (char=? (string-ref s k) #\,))
                      ;; Has comma at k
                      (let ((m (string->number (substring s i k)))
                            (rest (substring s (+ k 1) j)))
                        (if (= 0 (string-length rest))
                            (values m #f (+ j 1))        ; {m,}
                            (values m (string->number rest) (+ j 1)))) ; {m,n}
                      (if (< k j)
                          (find-comma (+ k 1))
                          ;; No comma: {m}
                          (let ((m (string->number (substring s i j))))
                            (values m m (+ j 1))))))
                (find-rb (+ j 1)))))))


  ) ; end library
