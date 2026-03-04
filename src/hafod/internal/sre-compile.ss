;;; (hafod internal sre-compile) -- SRE-to-POSIX ERE compiler
;;; Used at macro expansion time by the rx macro in (hafod re).
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal sre-compile)
  (export compile-sre count-parens)
  (import (chezscheme))

  ;; POSIX ERE special characters that need backslash-escaping in literals.
  (define posix-special-chars
    (list #\. #\[ #\* #\+ #\? #\{ #\| #\( #\) #\^ #\$ #\\))

  ;; Escape POSIX ERE special characters in a string literal.
  (define (posix-quote-string s)
    (let loop ((i 0) (acc '()))
      (if (>= i (string-length s))
          (apply string-append (reverse acc))
          (let ((c (string-ref s i)))
            (if (memv c posix-special-chars)
                (loop (+ i 1) (cons (string c) (cons "\\" acc)))
                (loop (+ i 1) (cons (string c) acc)))))))

  ;; Escape characters for use inside a POSIX bracket expression [...].
  ;; ] must be first, ^ must not be first, - must be last.
  (define (posix-bracket-chars str)
    (let* ((chars (string->list str))
           (has-rbracket (memv #\] chars))
           (has-caret (memv #\^ chars))
           (has-dash (memv #\- chars))
           (others (filter (lambda (c)
                             (and (not (char=? c #\]))
                                  (not (char=? c #\^))
                                  (not (char=? c #\-))))
                           chars))
           ;; Build bracket interior: ] first, then others, then ^, then - last
           (parts (append
                    (if has-rbracket '(#\]) '())
                    others
                    (if has-caret '(#\^) '())
                    (if has-dash '(#\-) '()))))
      (list->string parts)))

  ;; Named character class -> POSIX bracket expression
  (define (named-class->posix name)
    (case name
      ((alpha alphabetic) "[[:alpha:]]")
      ((digit numeric num) "[[:digit:]]")
      ((alnum alphanumeric alphanum) "[[:alnum:]]")
      ((lower lower-case) "[[:lower:]]")
      ((upper upper-case) "[[:upper:]]")
      ((space white whitespace) "[[:space:]]")
      ((punct punctuation) "[[:punct:]]")
      ((graph graphic) "[[:graph:]]")
      ((print printing) "[[:print:]]")
      ((cntrl control) "[[:cntrl:]]")
      ((xdigit hex-digit hex) "[[:xdigit:]]")
      ((blank) "[[:blank:]]")
      ((ascii) "[\\x00-\\x7f]")
      ((any) ".")
      ((nonl) "[^\n]")
      (else #f)))

  ;; Check if a POSIX regex string contains an unparenthesized | at the top level
  (define (string-contains-unescaped-pipe? s)
    (let ((len (string-length s)))
      (let loop ((i 0) (depth 0))
        (and (< i len)
             (let ((c (string-ref s i)))
               (cond
                 ((and (char=? c #\\) (< (+ i 1) len))
                  (loop (+ i 2) depth))
                 ((char=? c #\()
                  (loop (+ i 1) (+ depth 1)))
                 ((char=? c #\))
                  (loop (+ i 1) (- depth 1)))
                 ((char=? c #\[)
                  ;; Skip bracket expression
                  (let bloop ((j (+ i 1)))
                    (if (>= j len) #f
                        (if (char=? (string-ref s j) #\])
                            (loop (+ j 1) depth)
                            (bloop (+ j 1))))))
                 ((and (char=? c #\|) (= depth 0))
                  #t)
                 (else (loop (+ i 1) depth))))))))

  ;; For sequences: wrap alternations in non-capturing parens (but POSIX ERE
  ;; has no non-capturing groups, so we must use capturing parens).
  ;; Returns (values pattern nparen).
  (define (ensure-piece-for-seq pattern nparen)
    (if (and (> (string-length pattern) 0)
             (string-contains-unescaped-pipe? pattern))
        (values (string-append "(" pattern ")")
                (+ nparen 1))
        (values pattern nparen)))

  ;; Skip over one atom in a POSIX ERE string starting at position i.
  ;; Returns the position after the atom, or #f if no atom found.
  (define (skip-atom s i len)
    (and (< i len)
         (let ((c (string-ref s i)))
           (cond
             ;; Escaped character
             ((char=? c #\\)
              (and (< (+ i 1) len) (+ i 2)))
             ;; Bracket expression
             ((char=? c #\[)
              (skip-bracket s (+ i 1) len))
             ;; Parenthesized expression
             ((char=? c #\()
              (skip-parens s (+ i 1) len 1))
             ;; Not an atom start (| or ))
             ((or (char=? c #\|) (char=? c #\)))
              #f)
             ;; Plain character
             (else (+ i 1))))))

  ;; Skip to the end of a bracket expression (past the closing ])
  ;; Handles POSIX character classes like [:alpha:] inside brackets.
  (define (skip-bracket s i len)
    ;; After [, optionally ^ for negation
    (let ((i (if (and (< i len) (char=? (string-ref s i) #\^)) (+ i 1) i)))
      ;; ] as first char in bracket is literal
      (let ((i (if (and (< i len) (char=? (string-ref s i) #\])) (+ i 1) i)))
        (let loop ((j i))
          (and (< j len)
               (let ((c (string-ref s j)))
                 (cond
                   ;; POSIX character class [: or [= or [.
                   ((and (char=? c #\[)
                         (< (+ j 1) len)
                         (memv (string-ref s (+ j 1)) '(#\: #\= #\.)))
                    (let ((closer (string-ref s (+ j 1))))
                      ;; Skip to closing :] or =] or .]
                      (let inner ((k (+ j 2)))
                        (if (>= k len) #f
                            (if (and (char=? (string-ref s k) closer)
                                     (< (+ k 1) len)
                                     (char=? (string-ref s (+ k 1)) #\]))
                                (loop (+ k 2))
                                (inner (+ k 1)))))))
                   ;; Regular closing ]
                   ((char=? c #\])
                    (+ j 1))
                   ;; Any other character
                   (else (loop (+ j 1))))))))))

  ;; Skip past matching ) for a parenthesized expression
  (define (skip-parens s i len depth)
    (and (< i len)
         (let ((c (string-ref s i)))
           (cond
             ((and (char=? c #\\) (< (+ i 1) len))
              (skip-parens s (+ i 2) len depth))
             ((char=? c #\()
              (skip-parens s (+ i 1) len (+ depth 1)))
             ((char=? c #\))
              (if (= depth 1) (+ i 1)
                  (skip-parens s (+ i 1) len (- depth 1))))
             ((char=? c #\[)
              (let ((after-bracket (skip-bracket s (+ i 1) len)))
                (and after-bracket (skip-parens s after-bracket len depth))))
             (else (skip-parens s (+ i 1) len depth))))))

  ;; Skip an optional quantifier: *, +, ?, {n}, {n,}, {n,m}
  (define (skip-quantifier s i len)
    (if (>= i len) i
        (let ((c (string-ref s i)))
          (cond
            ((or (char=? c #\*) (char=? c #\+) (char=? c #\?))
             (+ i 1))
            ((char=? c #\{)
             ;; Skip to closing }
             (let loop ((j (+ i 1)))
               (if (>= j len) i  ; malformed, return original position
                   (if (char=? (string-ref s j) #\})
                       (+ j 1)
                       (loop (+ j 1))))))
            (else i)))))

  ;; Determine if a POSIX ERE string is a single "piece" (atom + optional quantifier).
  (define (single-piece? s)
    (let ((len (string-length s)))
      (and (> len 0)
           (let-values (((atom-end) (skip-atom s 0 len)))
             (and atom-end
                  (let ((after-q (skip-quantifier s atom-end len)))
                    (= after-q len)))))))

  ;; Wrap a pattern in parens if it needs grouping for a quantifier.
  ;; Returns (values pattern new-nparen).
  (define (ensure-piece pattern nparen)
    (if (single-piece? pattern)
        (values pattern nparen)
        (if (= (string-length pattern) 0)
            (values "()" (+ nparen 1))
            (values (string-append "(" pattern ")")
                    (+ nparen 1)))))

  ;; ======================================================================
  ;; compile-sre : SRE datum x fold? -> (values posix-string nparen fold? submatch-map)
  ;;
  ;; base  = number of POSIX parens already emitted before this expression
  ;; nparen = number of POSIX parens this expression adds
  ;; submatch-map = list of 1-based POSIX paren indices for user (submatch) forms
  ;;
  ;; The top-level wrapper calls compile-sre/base with base=0.
  ;; ======================================================================

  (define (compile-sre sre fold?)
    (let-values (((s np f smap) (compile-sre/base sre fold? 0)))
      (values s np f smap)))

  ;; Compile repetition body and append quantifier
  (define (compile-repetition/base body quantifier fold? base)
    (let-values (((s p f smap) (compile-sre/base
                                 (if (= (length body) 1) (car body) (cons 'seq body))
                                 fold? base)))
      (let-values (((s2 p2) (ensure-piece s p)))
        ;; If ensure-piece added a grouping paren, adjust base offsets of all smap entries
        ;; The grouping paren is at position (base + 1), shifting all inner parens by 1
        (let ((new-smap (if (> p2 p)
                            ;; A grouping paren was added around the whole thing.
                            ;; Inner parens shift by 1 because the new outer paren is before them.
                            (map (lambda (idx) (+ idx 1)) smap)
                            smap)))
          (values (string-append s2 quantifier) p2 f new-smap)))))

  ;; Core compiler with base paren offset tracking.
  ;; base = how many ( have been emitted in the whole pattern before this subexpression
  ;; Returns (values posix-string nparen fold? submatch-map)
  ;; where submatch-map is a list of 1-based POSIX paren indices for user submatches
  (define (compile-sre/base sre fold? base)
    (cond
      ;; String literal
      ((string? sre)
       (if (= (string-length sre) 0)
           (values "" 0 fold? '())
           (values (posix-quote-string sre) 0 fold? '())))

      ;; Symbol (named classes and anchors)
      ((symbol? sre)
       (case sre
         ((any) (values "." 0 fold? '()))
         ((nonl) (values "[^\n]" 0 fold? '()))
         ((bos) (values "^" 0 fold? '()))
         ((eos) (values "$" 0 fold? '()))
         ((bol) (values "^" 0 fold? '()))
         ((eol) (values "$" 0 fold? '()))
         (else
           (let ((posix (named-class->posix sre)))
             (if posix
                 (values posix 0 fold? '())
                 (error 'compile-sre "unknown SRE symbol" sre))))))

      ;; Compound forms
      ((pair? sre)
       (let ((head (car sre))
             (rest (cdr sre)))
         ;; Check if head is a string -- char-set form like ("aeiou")
         (if (string? head)
             ;; Character set: each char in the string(s)
             (let ((all-chars (apply string-append (filter string? sre))))
               (values (string-append "[" (posix-bracket-chars all-chars) "]")
                       0 fold? '()))
             ;; Symbolic compound form
             (case head
               ;; Sequence
               ((seq :)
                (if (null? rest)
                    (values "" 0 fold? '())
                    (let loop ((forms rest) (acc "") (np 0) (smap '()))
                      (if (null? forms)
                          (values acc np fold? (reverse smap))
                          (let-values (((s p f inner-smap) (compile-sre/base (car forms) fold? (+ base np))))
                            (let-values (((s2 p2) (ensure-piece-for-seq s p)))
                              ;; If ensure-piece-for-seq added a grouping paren,
                              ;; adjust inner submatch indices
                              (let ((adj-smap (if (> p2 p)
                                                  (map (lambda (idx) (+ idx 1)) inner-smap)
                                                  inner-smap)))
                                (loop (cdr forms) (string-append acc s2) (+ np p2)
                                      (append (reverse adj-smap) smap)))))))))

               ;; Alternation
               ((or)
                (if (null? rest)
                    ;; Empty alternation: unmatchable
                    (values "[^\\x00-\\x7f]" 0 fold? '())
                    (let loop ((forms rest) (acc '()) (np 0) (first? #t) (smap '()))
                      (if (null? forms)
                          (values (apply string-append (reverse acc)) np fold? (reverse smap))
                          (let-values (((s p f inner-smap) (compile-sre/base (car forms) fold? (+ base np))))
                            (loop (cdr forms)
                                  (if first?
                                      (cons s acc)
                                      (cons s (cons "|" acc)))
                                  (+ np p)
                                  #f
                                  (append (reverse inner-smap) smap)))))))

               ;; Repetition: (* re ...)
               ((*) (compile-repetition/base rest "*" fold? base))
               ((+) (compile-repetition/base rest "+" fold? base))
               ((?) (compile-repetition/base rest "?" fold? base))

               ;; Bounded repetition: (** m n re ...)
               ((**)
                (let ((m (car rest))
                      (n (cadr rest))
                      (body (cddr rest)))
                  (let-values (((s p f smap) (compile-sre/base
                                               (if (= (length body) 1) (car body) (cons 'seq body))
                                               fold? base)))
                    (let-values (((s2 p2) (ensure-piece s p)))
                      (let ((new-smap (if (> p2 p) (map (lambda (idx) (+ idx 1)) smap) smap)))
                        (values (string-append s2 "{" (number->string m) "," (number->string n) "}")
                                p2 f new-smap))))))

               ;; Exact repetition: (= n re ...)
               ((=)
                (let ((n (car rest))
                      (body (cdr rest)))
                  (let-values (((s p f smap) (compile-sre/base
                                               (if (= (length body) 1) (car body) (cons 'seq body))
                                               fold? base)))
                    (let-values (((s2 p2) (ensure-piece s p)))
                      (let ((new-smap (if (> p2 p) (map (lambda (idx) (+ idx 1)) smap) smap)))
                        (values (string-append s2 "{" (number->string n) "}")
                                p2 f new-smap))))))

               ;; At-least repetition: (>= n re ...)
               ((>=)
                (let ((n (car rest))
                      (body (cdr rest)))
                  (let-values (((s p f smap) (compile-sre/base
                                               (if (= (length body) 1) (car body) (cons 'seq body))
                                               fold? base)))
                    (let-values (((s2 p2) (ensure-piece s p)))
                      (let ((new-smap (if (> p2 p) (map (lambda (idx) (+ idx 1)) smap) smap)))
                        (values (string-append s2 "{" (number->string n) ",}")
                                p2 f new-smap))))))

               ;; Submatch
               ((submatch)
                ;; This paren is at absolute index (base + 1)
                (let ((this-paren-idx (+ base 1)))
                  (let-values (((s p f inner-smap) (compile-sre/base
                                                     (if (= (length rest) 1) (car rest) (cons 'seq rest))
                                                     fold? (+ base 1))))
                    ;; The inner submatches are already correct (they were compiled with base+1).
                    ;; Our user submatch is at this-paren-idx, followed by any inner user submatches.
                    (values (string-append "(" s ")")
                            (+ p 1) f
                            (cons this-paren-idx inner-smap)))))

               ;; Dead submatch (ignore pre/post counts)
               ((dsm)
                (let ((body (cddr rest)))
                  (compile-sre/base (if (= (length body) 1) (car body) (cons 'seq body)) fold? base)))

               ;; Case folding
               ((w/nocase)
                (let-values (((s p f smap) (compile-sre/base
                                             (if (= (length rest) 1) (car rest) (cons 'seq rest))
                                             #t base)))
                  (values s p #t smap)))

               ((w/case)
                (let-values (((s p f smap) (compile-sre/base
                                             (if (= (length rest) 1) (car rest) (cons 'seq rest))
                                             #f base)))
                  (values s p #f smap)))

               ((uncase)
                (let-values (((s p f smap) (compile-sre/base
                                             (if (= (length rest) 1) (car rest) (cons 'seq rest))
                                             #t base)))
                  (values s p #t smap)))

               ;; Complement: (~ cset ...)
               ((~)
                (let ((inner (compile-char-set-union rest)))
                  (values (string-append "[^" inner "]") 0 fold? '())))

               ;; Intersection: (& cset ...)
               ((&)
                (error 'compile-sre "char-set intersection (&) not supported in POSIX ERE" sre))

               ;; Difference: (- cset ...)
               ((-)
                (error 'compile-sre "char-set difference (-) not supported in POSIX ERE" sre))

               ;; Posix pass-through
               ((posix-string)
                (values (car rest) 0 fold? '()))

               (else
                (error 'compile-sre "unknown SRE form" sre))))))

      (else (error 'compile-sre "unrecognized SRE" sre))))

  ;; Compile a union of char-set forms into bracket expression contents (without [])
  (define (compile-char-set-union forms)
    (apply string-append
           (map (lambda (form)
                  (cond
                    ((string? form)
                     (posix-bracket-chars form))
                    ((symbol? form)
                     (let ((posix (named-class->posix form)))
                       (if posix
                           ;; Extract interior from [...]
                           (let ((s posix))
                             (substring s 1 (- (string-length s) 1)))
                           (error 'compile-char-set-union "unknown named class" form))))
                    ((and (pair? form) (string? (car form)))
                     (posix-bracket-chars (apply string-append (filter string? form))))
                    (else (error 'compile-char-set-union "unsupported char-set form" form))))
                forms)))

  ;; Count parentheses (submatch groups) in a compiled pattern
  (define (count-parens pattern)
    (let ((len (string-length pattern)))
      (let loop ((i 0) (count 0))
        (if (>= i len)
            count
            (let ((c (string-ref pattern i)))
              (cond
                ((and (char=? c #\\) (< (+ i 1) len))
                 (loop (+ i 2) count))
                ((char=? c #\()
                 (loop (+ i 1) (+ count 1)))
                (else
                 (loop (+ i 1) count))))))))

  ) ; end library
