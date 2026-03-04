;;; (hafod internal re-posixstr) -- POSIX string compiler for RE ADT
;;; Extracted from (hafod re-adt) -- regexp->posix-string and all translate-* helpers.
;;; Copyright (c) 2026, hafod contributors.
;;; Original scsh code: Copyright (c) 1997, 1998 Olin Shivers.

(library (hafod internal re-posixstr)
  (export regexp->posix-string simplify-regexp)
  (import (hafod internal base)
          (hafod compat)
          (hafod internal char-sets)
          (hafod internal re-records))


  ;; ======================================================================
  ;; simplify-regexp
  ;; ======================================================================
  ;; Identity for now -- the smart constructors already produce canonical forms.
  (define (simplify-regexp re) re)


  ;; ======================================================================
  ;; POSIX string compiler: regexp->posix-string
  ;; Port of scsh/rx/posixstr.scm
  ;; Returns 4 values: string, syntax-level, paren-count, submatch-vector
  ;; ======================================================================

  ;; Pad vector V with PRE initial and POST trailing #f entries.
  (define (pad-vector pre post v)
    (if (and (= pre 0) (= post 0)) v
        (let* ((vlen (vector-length v))
               (alen (+ pre post vlen))
               (ans (make-vector alen #f)))
          (do ((from (- vlen 1) (- from 1))
               (to (+ pre vlen -1) (- to 1)))
              ((< from 0))
            (vector-set! ans to (vector-ref v from)))
          ans)))

  (define (n-falses n) (make-vector n #f))

  ;; Check if an RE can never match (simplifies to empty char-set, DSM wrapping one,
  ;; or empty choice).
  (define (simple-empty-re? re)
    (or (and (re-char-set? re)
             (char-set-empty? (re-char-set:cset re)))
        (and (re-choice? re)
             (null? (re-choice:elts re)))
        (and (re-dsm? re)
             (simple-empty-re? (re-dsm:body re)))))

  ;; POSIX special characters that need escaping in string literals.
  (define posix-specials (string->char-set "{}[.*?()|\\$^+"))

  ;; Pattern that can never match (assumes 7-bit ASCII).
  ;; Equivalent to never-match-pattern but built at runtime to avoid R6RS octal restriction.
  (define never-match-pattern
    (string #\[ #\^ (integer->char 0) #\- (integer->char 127) #\]))

  ;; Top-level entry point.
  (define (regexp->posix-string re)
    (let ((re (simplify-regexp re)))
      (if (simple-empty-re? re)
          (values #f #f #f '#())
          (translate-regexp re))))

  ;; Main dispatcher.
  (define (translate-regexp re)
    (cond
     ((re-string? re) (translate-string (re-string:chars re)))

     ((re-repeat? re)   (translate-repeat re))
     ((re-choice? re)   (translate-choice re))
     ((re-seq? re)      (translate-seq re))
     ((re-char-set? re) (translate-char-set (re-char-set:cset re)))

     ((re-submatch? re) (translate-submatch re))

     ((re-bos? re) (values "^" 1 0 '#()))
     ((re-eos? re) (values "$" 1 0 '#()))

     ((re-bol? re) (error 'regexp->posix-string
                          "Beginning-of-line not supported in this implementation."))
     ((re-eol? re) (error 'regexp->posix-string
                          "End-of-line not supported in this implementation."))

     ((re-dsm? re)
      (let ((pre-dsm (re-dsm:pre-dsm re))
            (body (re-dsm:body re)))
        (translate-dsm body pre-dsm
                       (- (re-dsm:tsm re)
                          (+ pre-dsm (re-tsm body))))))

     (else (error 'regexp->posix-string "Illegal regular expression" re))))


  ;; ======================================================================
  ;; Translate string literals
  ;; ======================================================================

  (define (translate-string s)
    (let ((len (string-length s)))
      (if (zero? len)
          (values "()" 0 1 '#())  ; Empty string -> "()"
          (let* ((s2 (let loop ((i 0) (acc '()))
                       (if (>= i len)
                           (apply string-append (reverse acc))
                           (let ((c (string-ref s i)))
                             (if (char-set-contains? posix-specials c)
                                 (loop (+ i 1) (cons (string c) (cons "\\" acc)))
                                 (loop (+ i 1) (cons (string c) acc))))))))
            (values s2 (if (= len 1) 1 2)
                    0 '#())))))


  ;; ======================================================================
  ;; Force level < 3 by wrapping in parens if needed
  ;; ======================================================================

  (define (paren-if-necessary s lev pcount submatches)
    (if (< lev 3)
        (values s lev pcount submatches)
        (values (string-append "(" s ")")
                0
                (+ pcount 1)
                (mapv (lambda (sm) (and sm (+ 1 sm)))
                      submatches))))


  ;; ======================================================================
  ;; Translate sequence: (seq re1 ... ren)
  ;; ======================================================================

  (define (translate-seq re)
    (let ((elts (re-seq:elts re)))
      (let recur ((elts elts) (prev-pcount 0) (prev-smcount 0))
        (if (pair? elts)
            (let* ((elt (car elts))
                   (elts (cdr elts)))
              (receive (s1 level1 pcount1 submatches1) (translate-regexp elt)
                (receive (s1 level1 pcount1 submatches1)
                         (paren-if-necessary s1 level1 pcount1 submatches1)
                  (receive (s level pcount submatches)
                           (recur elts
                                  (+ pcount1 prev-pcount)
                                  (+ prev-smcount (re-tsm elt)))
                    (values (string-append s1 s)
                            2
                            (+ pcount1 pcount)
                            (vector-append
                             (mapv (lambda (p) (and p (+ p prev-pcount)))
                                   submatches1)
                             submatches))))))
            (values "" 2 0 '#())))))  ; Empty seq


  ;; ======================================================================
  ;; Translate choice: (or re1 ... ren)
  ;; ======================================================================

  (define (translate-choice re)
    (let ((elts (re-choice:elts re))
          (tsm (re-choice:tsm re)))
      (if (pair? elts)
          (let recur ((elts elts) (prev-pcount 0) (prev-smcount 0))
            (let ((elt (car elts)) (tail (cdr elts)))
              (receive (s1 level1 pcount1 submatches1) (translate-regexp elt)
                (let ((submatches1 (mapv (lambda (sm) (and sm (+ sm prev-pcount)))
                                         submatches1)))
                  (if (pair? tail)
                      (receive (s level pcount submatches)
                               (recur tail
                                      (+ pcount1 prev-pcount)
                                      (+ prev-smcount (re-tsm elt)))
                        (values (string-append s1 "|" s) 3
                                (+ pcount1 pcount)
                                (vector-append submatches1 submatches)))
                      (values s1 level1 pcount1 submatches1))))))
          ;; Empty choice -- unmatchable
          (values never-match-pattern 1 0 (n-falses tsm)))))


  ;; ======================================================================
  ;; Translate repeat: *, +, ?, {n,m}
  ;; ======================================================================

  (define (translate-repeat re)
    (let ((from (re-repeat:from re))
          (to (re-repeat:to re))
          (body (re-repeat:body re))
          (tsm (re-repeat:tsm re)))
      (cond
       ;; Unsatisfiable: from > to
       ((and to (> from to))
        (values never-match-pattern 1 0 (n-falses tsm)))

       ;; RE{1,1} => RE
       ((and to (= from to 1)) (translate-regexp body))

       ;; RE{0,0} => ""
       ((and to (= to 0))
        (values "" 2 0 (n-falses tsm)))

       ;; General case
       (else
        (receive (s level pcount submatches) (translate-regexp body)
          ;; Coerce to level < 2 (a "piece") so quantifier binds correctly.
          (receive (s level pcount submatches)
                   (if (> level 1)
                       (values (string-append "(" s ")")
                               0
                               (+ pcount 1)
                               (mapv (lambda (i) (and i (+ i 1))) submatches))
                       (values s level pcount submatches))
            (values (if to
                        (cond ((and (= from 0) (= to 1)) (string-append s "?"))
                              ((= from to)
                               (string-append s "{" (number->string to) "}"))
                              (else
                               (string-append s "{" (number->string from)
                                              "," (number->string to) "}")))
                        (cond ((= from 0) (string-append s "*"))
                              ((= from 1) (string-append s "+"))
                              (else (string-append s "{" (number->string from) ",}"))))
                    1 pcount submatches)))))))


  ;; ======================================================================
  ;; Translate submatch
  ;; ======================================================================

  (define (translate-submatch re)
    (let ((body (re-submatch:body re))
          (pre-dsm (re-submatch:pre-dsm re)))

      ;; Translate body with any leading/trailing dead submatches.
      (receive (s level pcount submatches)
               (translate-dsm body
                              pre-dsm
                              (- (re-submatch:tsm re)
                                 (+ 1 pre-dsm (re-tsm body))))

        ;; If already parenthesized, reuse; otherwise wrap in parens.
        (if (= level 0)
            (values s 0 pcount (vector-append '#(1) submatches))
            (values (string-append "(" s ")")
                    0
                    (+ pcount 1)
                    (mapv! (lambda (i) (and i (+ i 1)))
                           (vector-append '#(0) submatches)))))))


  ;; ======================================================================
  ;; Translate DSM (dead submatches)
  ;; ======================================================================

  (define (translate-dsm body pre-dsm post-dsm)
    (receive (s level pcount submatches) (translate-regexp body)
      (values s level pcount (pad-vector pre-dsm post-dsm submatches))))


  ;; ======================================================================
  ;; Translate char-set
  ;; ======================================================================
  ;; This produces a POSIX bracket expression.
  ;; - Full set -> "."
  ;; - Empty set -> never-match-pattern
  ;; - Singleton -> the escaped char
  ;; - General -> try both [...] and [^...], pick shorter

  (define *nul* (integer->char 0))

  (define (translate-char-set cset)
    (if (char-set-full? cset)
        (values "." 1 0 '#())      ; Full set
        (let* ((cset (char-set-delete cset *nul*))
               (nchars (char-set-size cset)))
          (cond
           ((= 0 nchars) (values never-match-pattern 1 0 '#()))  ; Empty set

           ((= 1 nchars)  ; Singleton
            (translate-string (string (car (char-set->list cset)))))

           ;; General case: render as bracket expression
           (else
            (let ((s- (render-bracket-expr cset #t))
                  (s+ (render-bracket-expr
                       (char-set-delete (char-set-complement cset) *nul*)
                       #f)))
              (values (if (< (string-length s-) (string-length s+))
                          s- s+)
                      1 0 '#())))))))

  ;; Render a char-set as a POSIX [...] or [^...] bracket expression.
  ;; in? = #t means [...], #f means [^...]
  ;; This is a simplified version that handles the common cases correctly.
  (define (render-bracket-expr cset in?)
    (let* ((chars (char-set->list cset))
           ;; Sort chars by code point
           (sorted (sort (lambda (a b) (< (char->integer a) (char->integer b))) chars))
           ;; Build ranges from sorted list
           (ranges+loose (build-ranges sorted))
           (ranges (car ranges+loose))
           (loose (cdr ranges+loose)))
      ;; Render bracket expression
      (render-bracket-parts loose ranges in?)))

  ;; Build ranges from a sorted list of chars.
  ;; Returns (ranges . loose-chars) where ranges are (start . end) pairs
  ;; and loose-chars are individual characters not in ranges.
  (define (build-ranges sorted)
    (if (null? sorted)
        (cons '() '())
        (let loop ((chars (cdr sorted))
                   (run-start (car sorted))
                   (run-end (car sorted))
                   (ranges '())
                   (loose '()))
          (if (null? chars)
              ;; Flush final run
              (let ((run-len (+ 1 (- (char->integer run-end) (char->integer run-start)))))
                (if (>= run-len 3)
                    (cons (reverse (cons (cons run-start run-end) ranges))
                          (reverse loose))
                    ;; Too short for a range, add as loose
                    (let add-loose ((c run-start) (l loose))
                      (if (char>? c run-end)
                          (cons (reverse ranges) (reverse l))
                          (add-loose (integer->char (+ 1 (char->integer c)))
                                     (cons c l))))))
              (let ((c (car chars))
                    (next-code (+ 1 (char->integer run-end))))
                (if (= (char->integer c) next-code)
                    ;; Extend current run
                    (loop (cdr chars) run-start c ranges loose)
                    ;; End current run, start new one
                    (let ((run-len (+ 1 (- (char->integer run-end) (char->integer run-start)))))
                      (if (>= run-len 3)
                          (loop (cdr chars) c c
                                (cons (cons run-start run-end) ranges)
                                loose)
                          ;; Too short, add as loose
                          (let add-loose ((ch run-start) (l loose))
                            (if (char>? ch run-end)
                                (loop (cdr chars) c c ranges l)
                                (add-loose (integer->char (+ 1 (char->integer ch)))
                                           (cons ch l))))))))))))

  ;; Render a bracket expression from loose chars and ranges.
  ;; Handles the special chars ], -, ^ that have positional requirements.
  (define (render-bracket-parts loose ranges in?)
    (let* (;; Separate special chars from loose
           (has-rbracket (memv #\] loose))
           (has-dash (memv #\- loose))
           (has-caret (memv #\^ loose))
           ;; Remove specials from loose
           (normal (filter (lambda (c)
                             (and (not (char=? c #\]))
                                  (not (char=? c #\-))
                                  (not (char=? c #\^))))
                           loose))
           ;; Build the interior string:
           ;; ] must come first, ^ must not be first (for IN), - must be last
           (interior
            (string-append
             ;; ] first (if present)
             (if has-rbracket "]" "")
             ;; Normal chars
             (list->string normal)
             ;; ^ (safe after other chars; for IN, not first)
             (if (and has-caret
                      ;; Only safe if there's something before it OR it's NOT-IN
                      (or (not in?) has-rbracket (pair? normal)))
                 "^"
                 "")
             ;; Ranges
             (apply string-append
                    (map (lambda (r) (string (car r) #\- (cdr r))) ranges))
             ;; ^ at end if it would be first in IN and nothing precedes it
             (if (and has-caret in?
                      (not has-rbracket) (null? normal))
                 ;; ^ is the only char or first -- must handle specially
                 ;; For IN: if ranges exist, put ^ after ranges
                 ;; If no ranges and no other loose, it's a singleton (shouldn't get here)
                 "^"
                 "")
             ;; - last
             (if has-dash "-" ""))))
      (string-append (if in? "[" "[^") interior "]")))


  ) ; end library
