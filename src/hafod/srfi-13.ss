#!chezscheme
;;; (hafod srfi-13) -- SRFI-13 String Library
;;; Provides full SRFI-13 compatibility for scsh parity.
;;; Delegates to Chez Scheme builtins where possible.
;;; Reference: https://srfi.schemers.org/srfi-13/srfi-13.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-13)
  (export
    ;; Predicates
    string-null? string-every string-any
    ;; Constructors
    string-tabulate string-unfold string-unfold-right
    ;; Selection
    string-take string-drop string-take-right string-drop-right
    string-pad string-pad-right
    string-trim string-trim-right string-trim-both
    ;; Comparison
    string-compare string-compare-ci
    string= string<> string< string> string<= string>=
    string-ci= string-ci<> string-ci< string-ci> string-ci<= string-ci>=
    string-prefix-length string-suffix-length
    string-prefix-length-ci string-suffix-length-ci
    string-prefix? string-suffix?
    string-prefix-ci? string-suffix-ci?
    ;; Searching
    string-index string-index-right
    string-skip string-skip-right
    string-count string-contains string-contains-ci
    ;; Case mapping — re-export Chez builtins
    string-upcase string-downcase string-titlecase
    ;; Reverse & append
    string-reverse string-reverse!
    string-concatenate string-concatenate-reverse
    string-concatenate/shared string-concatenate-reverse/shared
    string-append/shared
    ;; Fold, unfold & map
    string-map string-for-each
    string-fold string-fold-right
    ;; Replicate & rotate
    xsubstring string-xcopy!
    ;; Miscellaneous
    string-replace string-tokenize
    string-filter string-delete
    ;; Shared/linear-update
    string-copy! string-fill! string-map!
    ;; Hashing
    string-hash string-hash-ci
    ;; Aliases
    substring/shared string-copy/shared
    ;; I/O
    string-join)

  (import
    (except (chezscheme)
      ;; We redefine these with SRFI-13 semantics (start/end bounds)
      string-for-each string-copy! string-fill!
      string-hash  ; SRFI-13 version takes optional bound/start/end
      )
    (only (hafod compat) ->char-set)
    (only (hafod srfi-1) every any))

  ;; Internal: normalise char/char-set/pred criterion to a char predicate
  (define (make-char-pred criterion)
    (cond
      [(char? criterion) (lambda (c) (char=? c criterion))]
      [(procedure? criterion) criterion]
      [else  ; assume char-set
       (let ([cs criterion])
         (lambda (c) (char-set-contains? cs c)))]))

  ;; Borrow char-set-contains? from the compat layer
  (define (char-set-contains? cs c)
    ;; Use the ->char-set infrastructure; char-sets are procedures in hafod
    (cs c))

  ;; =========================================================
  ;; Predicates
  ;; =========================================================

  (define (string-null? s) (fx= (string-length s) 0))

  (define string-every
    (case-lambda
      [(pred s) (string-every pred s 0 (string-length s))]
      [(pred s start) (string-every pred s start (string-length s))]
      [(pred s start end)
       (let ([pred (make-char-pred pred)])
         (let loop ([i start])
           (cond
             [(fx>= i end) #t]
             [(fx= i (fx- end 1)) (pred (string-ref s i))]  ; tail position
             [(pred (string-ref s i)) (loop (fx+ i 1))]
             [else #f])))]))

  (define string-any
    (case-lambda
      [(pred s) (string-any pred s 0 (string-length s))]
      [(pred s start) (string-any pred s start (string-length s))]
      [(pred s start end)
       (let ([pred (make-char-pred pred)])
         (let loop ([i start])
           (cond
             [(fx>= i end) #f]
             [(fx= i (fx- end 1)) (pred (string-ref s i))]
             [(pred (string-ref s i))]
             [else (loop (fx+ i 1))])))]))

  ;; =========================================================
  ;; Constructors
  ;; =========================================================

  (define (string-tabulate proc len)
    (let ([s (make-string len)])
      (let loop ([i 0])
        (when (fx< i len)
          (string-set! s i (proc i))
          (loop (fx+ i 1))))
      s))

  (define string-unfold
    (case-lambda
      [(p f g seed)
       (string-unfold p f g seed "" (lambda (x) ""))]
      [(p f g seed base)
       (string-unfold p f g seed base (lambda (x) ""))]
      [(p f g seed base make-final)
       (let ([port (open-output-string)])
         (display base port)
         (let loop ([seed seed])
           (if (p seed)
               (begin
                 (display (make-final seed) port)
                 (get-output-string port))
               (begin
                 (write-char (f seed) port)
                 (loop (g seed))))))]))

  (define string-unfold-right
    (case-lambda
      [(p f g seed)
       (string-unfold-right p f g seed "" (lambda (x) ""))]
      [(p f g seed base)
       (string-unfold-right p f g seed base (lambda (x) ""))]
      [(p f g seed base make-final)
       (let loop ([seed seed] [acc '()])
         (if (p seed)
             (string-append (make-final seed) (list->string acc) base)
             (loop (g seed) (cons (f seed) acc))))]))

  ;; =========================================================
  ;; Selection
  ;; =========================================================

  (define (string-take s n) (substring s 0 n))
  (define (string-drop s n) (substring s n (string-length s)))
  (define (string-take-right s n) (substring s (fx- (string-length s) n) (string-length s)))
  (define (string-drop-right s n) (substring s 0 (fx- (string-length s) n)))

  (define string-pad
    (case-lambda
      [(s len) (string-pad s len #\space 0 (string-length s))]
      [(s len char) (string-pad s len char 0 (string-length s))]
      [(s len char start) (string-pad s len char start (string-length s))]
      [(s len char start end)
       (let ([slen (fx- end start)])
         (if (fx>= slen len)
             (substring s (fx+ start (fx- slen len)) end)
             (string-append (make-string (fx- len slen) char)
                            (substring s start end))))]))

  (define string-pad-right
    (case-lambda
      [(s len) (string-pad-right s len #\space 0 (string-length s))]
      [(s len char) (string-pad-right s len char 0 (string-length s))]
      [(s len char start) (string-pad-right s len char start (string-length s))]
      [(s len char start end)
       (let ([slen (fx- end start)])
         (if (fx>= slen len)
             (substring s start (fx+ start len))
             (string-append (substring s start end)
                            (make-string (fx- len slen) char))))]))

  (define string-trim
    (case-lambda
      [(s) (string-trim s char-whitespace? 0 (string-length s))]
      [(s criterion) (string-trim s criterion 0 (string-length s))]
      [(s criterion start) (string-trim s criterion start (string-length s))]
      [(s criterion start end)
       (let ([pred (make-char-pred criterion)])
         (let loop ([i start])
           (cond
             [(fx>= i end) ""]
             [(pred (string-ref s i)) (loop (fx+ i 1))]
             [else (substring s i end)])))]))

  (define string-trim-right
    (case-lambda
      [(s) (string-trim-right s char-whitespace? 0 (string-length s))]
      [(s criterion) (string-trim-right s criterion 0 (string-length s))]
      [(s criterion start) (string-trim-right s criterion start (string-length s))]
      [(s criterion start end)
       (let ([pred (make-char-pred criterion)])
         (let loop ([i (fx- end 1)])
           (cond
             [(fx< i start) ""]
             [(pred (string-ref s i)) (loop (fx- i 1))]
             [else (substring s start (fx+ i 1))])))]))

  (define string-trim-both
    (case-lambda
      [(s) (string-trim-both s char-whitespace? 0 (string-length s))]
      [(s criterion) (string-trim-both s criterion 0 (string-length s))]
      [(s criterion start) (string-trim-both s criterion start (string-length s))]
      [(s criterion start end)
       (let ([trimmed (string-trim s criterion start end)])
         (string-trim-right trimmed criterion))]))

  ;; =========================================================
  ;; Comparison
  ;; =========================================================

  (define string-compare
    (case-lambda
      [(s1 s2 proc< proc= proc>)
       (string-compare s1 s2 proc< proc= proc> 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 proc< proc= proc> start1)
       (string-compare s1 s2 proc< proc= proc> start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 proc< proc= proc> start1 end1)
       (string-compare s1 s2 proc< proc= proc> start1 end1 0 (string-length s2))]
      [(s1 s2 proc< proc= proc> start1 end1 start2)
       (string-compare s1 s2 proc< proc= proc> start1 end1 start2 (string-length s2))]
      [(s1 s2 proc< proc= proc> start1 end1 start2 end2)
       (let ([len1 (fx- end1 start1)]
             [len2 (fx- end2 start2)])
         (let ([minlen (fxmin len1 len2)])
           (let loop ([i 0])
             (if (fx= i minlen)
                 (cond
                   [(fx< len1 len2) (proc< (fx+ start1 i))]
                   [(fx= len1 len2) (proc= (fx+ start1 i))]
                   [else (proc> (fx+ start1 i))])
                 (let ([c1 (string-ref s1 (fx+ start1 i))]
                       [c2 (string-ref s2 (fx+ start2 i))])
                   (cond
                     [(char<? c1 c2) (proc< (fx+ start1 i))]
                     [(char>? c1 c2) (proc> (fx+ start1 i))]
                     [else (loop (fx+ i 1))]))))))]))

  (define string-compare-ci
    (case-lambda
      [(s1 s2 proc< proc= proc>)
       (string-compare-ci s1 s2 proc< proc= proc> 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 proc< proc= proc> start1)
       (string-compare-ci s1 s2 proc< proc= proc> start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 proc< proc= proc> start1 end1)
       (string-compare-ci s1 s2 proc< proc= proc> start1 end1 0 (string-length s2))]
      [(s1 s2 proc< proc= proc> start1 end1 start2)
       (string-compare-ci s1 s2 proc< proc= proc> start1 end1 start2 (string-length s2))]
      [(s1 s2 proc< proc= proc> start1 end1 start2 end2)
       (let ([len1 (fx- end1 start1)]
             [len2 (fx- end2 start2)])
         (let ([minlen (fxmin len1 len2)])
           (let loop ([i 0])
             (if (fx= i minlen)
                 (cond
                   [(fx< len1 len2) (proc< (fx+ start1 i))]
                   [(fx= len1 len2) (proc= (fx+ start1 i))]
                   [else (proc> (fx+ start1 i))])
                 (let ([c1 (char-downcase (string-ref s1 (fx+ start1 i)))]
                       [c2 (char-downcase (string-ref s2 (fx+ start2 i)))])
                   (cond
                     [(char<? c1 c2) (proc< (fx+ start1 i))]
                     [(char>? c1 c2) (proc> (fx+ start1 i))]
                     [else (loop (fx+ i 1))]))))))]))

  (define string-prefix-length
    (case-lambda
      [(s1 s2) (string-prefix-length s1 s2 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1) (string-prefix-length s1 s2 start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1 end1) (string-prefix-length s1 s2 start1 end1 0 (string-length s2))]
      [(s1 s2 start1 end1 start2) (string-prefix-length s1 s2 start1 end1 start2 (string-length s2))]
      [(s1 s2 start1 end1 start2 end2)
       (let ([len (fxmin (fx- end1 start1) (fx- end2 start2))])
         (let loop ([i 0])
           (if (or (fx= i len)
                   (not (char=? (string-ref s1 (fx+ start1 i))
                                (string-ref s2 (fx+ start2 i)))))
               i
               (loop (fx+ i 1)))))]))

  (define string-suffix-length
    (case-lambda
      [(s1 s2) (string-suffix-length s1 s2 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1) (string-suffix-length s1 s2 start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1 end1) (string-suffix-length s1 s2 start1 end1 0 (string-length s2))]
      [(s1 s2 start1 end1 start2) (string-suffix-length s1 s2 start1 end1 start2 (string-length s2))]
      [(s1 s2 start1 end1 start2 end2)
       (let ([len (fxmin (fx- end1 start1) (fx- end2 start2))])
         (let loop ([i 0])
           (if (or (fx= i len)
                   (not (char=? (string-ref s1 (fx- end1 1 i))
                                (string-ref s2 (fx- end2 1 i)))))
               i
               (loop (fx+ i 1)))))]))

  (define string-prefix-length-ci
    (case-lambda
      [(s1 s2) (string-prefix-length-ci s1 s2 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1) (string-prefix-length-ci s1 s2 start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1 end1) (string-prefix-length-ci s1 s2 start1 end1 0 (string-length s2))]
      [(s1 s2 start1 end1 start2) (string-prefix-length-ci s1 s2 start1 end1 start2 (string-length s2))]
      [(s1 s2 start1 end1 start2 end2)
       (let ([len (fxmin (fx- end1 start1) (fx- end2 start2))])
         (let loop ([i 0])
           (if (or (fx= i len)
                   (not (char-ci=? (string-ref s1 (fx+ start1 i))
                                   (string-ref s2 (fx+ start2 i)))))
               i
               (loop (fx+ i 1)))))]))

  (define string-suffix-length-ci
    (case-lambda
      [(s1 s2) (string-suffix-length-ci s1 s2 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1) (string-suffix-length-ci s1 s2 start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1 end1) (string-suffix-length-ci s1 s2 start1 end1 0 (string-length s2))]
      [(s1 s2 start1 end1 start2) (string-suffix-length-ci s1 s2 start1 end1 start2 (string-length s2))]
      [(s1 s2 start1 end1 start2 end2)
       (let ([len (fxmin (fx- end1 start1) (fx- end2 start2))])
         (let loop ([i 0])
           (if (or (fx= i len)
                   (not (char-ci=? (string-ref s1 (fx- end1 1 i))
                                   (string-ref s2 (fx- end2 1 i)))))
               i
               (loop (fx+ i 1)))))]))

  (define string-prefix?
    (case-lambda
      [(s1 s2) (string-prefix? s1 s2 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1) (string-prefix? s1 s2 start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1 end1) (string-prefix? s1 s2 start1 end1 0 (string-length s2))]
      [(s1 s2 start1 end1 start2) (string-prefix? s1 s2 start1 end1 start2 (string-length s2))]
      [(s1 s2 start1 end1 start2 end2)
       (let ([len1 (fx- end1 start1)])
         (and (fx<= len1 (fx- end2 start2))
              (fx= len1 (string-prefix-length s1 s2 start1 end1 start2 end2))))]))

  (define string-suffix?
    (case-lambda
      [(s1 s2) (string-suffix? s1 s2 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1) (string-suffix? s1 s2 start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1 end1) (string-suffix? s1 s2 start1 end1 0 (string-length s2))]
      [(s1 s2 start1 end1 start2) (string-suffix? s1 s2 start1 end1 start2 (string-length s2))]
      [(s1 s2 start1 end1 start2 end2)
       (let ([len1 (fx- end1 start1)])
         (and (fx<= len1 (fx- end2 start2))
              (fx= len1 (string-suffix-length s1 s2 start1 end1 start2 end2))))]))

  (define string-prefix-ci?
    (case-lambda
      [(s1 s2) (string-prefix-ci? s1 s2 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1) (string-prefix-ci? s1 s2 start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1 end1) (string-prefix-ci? s1 s2 start1 end1 0 (string-length s2))]
      [(s1 s2 start1 end1 start2) (string-prefix-ci? s1 s2 start1 end1 start2 (string-length s2))]
      [(s1 s2 start1 end1 start2 end2)
       (let ([len1 (fx- end1 start1)])
         (and (fx<= len1 (fx- end2 start2))
              (fx= len1 (string-prefix-length-ci s1 s2 start1 end1 start2 end2))))]))

  (define string-suffix-ci?
    (case-lambda
      [(s1 s2) (string-suffix-ci? s1 s2 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1) (string-suffix-ci? s1 s2 start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1 end1) (string-suffix-ci? s1 s2 start1 end1 0 (string-length s2))]
      [(s1 s2 start1 end1 start2) (string-suffix-ci? s1 s2 start1 end1 start2 (string-length s2))]
      [(s1 s2 start1 end1 start2 end2)
       (let ([len1 (fx- end1 start1)])
         (and (fx<= len1 (fx- end2 start2))
              (fx= len1 (string-suffix-length-ci s1 s2 start1 end1 start2 end2))))]))

  ;; =========================================================
  ;; Searching
  ;; =========================================================

  (define string-index
    (case-lambda
      [(s criterion) (string-index s criterion 0 (string-length s))]
      [(s criterion start) (string-index s criterion start (string-length s))]
      [(s criterion start end)
       (let ([pred (make-char-pred criterion)])
         (let loop ([i start])
           (cond
             [(fx>= i end) #f]
             [(pred (string-ref s i)) i]
             [else (loop (fx+ i 1))])))]))

  (define string-index-right
    (case-lambda
      [(s criterion) (string-index-right s criterion 0 (string-length s))]
      [(s criterion start) (string-index-right s criterion start (string-length s))]
      [(s criterion start end)
       (let ([pred (make-char-pred criterion)])
         (let loop ([i (fx- end 1)])
           (cond
             [(fx< i start) #f]
             [(pred (string-ref s i)) i]
             [else (loop (fx- i 1))])))]))

  (define string-skip
    (case-lambda
      [(s criterion) (string-skip s criterion 0 (string-length s))]
      [(s criterion start) (string-skip s criterion start (string-length s))]
      [(s criterion start end)
       (let ([pred (make-char-pred criterion)])
         (let loop ([i start])
           (cond
             [(fx>= i end) #f]
             [(not (pred (string-ref s i))) i]
             [else (loop (fx+ i 1))])))]))

  (define string-skip-right
    (case-lambda
      [(s criterion) (string-skip-right s criterion 0 (string-length s))]
      [(s criterion start) (string-skip-right s criterion start (string-length s))]
      [(s criterion start end)
       (let ([pred (make-char-pred criterion)])
         (let loop ([i (fx- end 1)])
           (cond
             [(fx< i start) #f]
             [(not (pred (string-ref s i))) i]
             [else (loop (fx- i 1))])))]))

  (define string-count
    (case-lambda
      [(s criterion) (string-count s criterion 0 (string-length s))]
      [(s criterion start) (string-count s criterion start (string-length s))]
      [(s criterion start end)
       (let ([pred (make-char-pred criterion)])
         (let loop ([i start] [n 0])
           (if (fx>= i end) n
               (loop (fx+ i 1) (if (pred (string-ref s i)) (fx+ n 1) n)))))]))

  (define string-contains
    (case-lambda
      [(s1 s2) (string-contains s1 s2 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1) (string-contains s1 s2 start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1 end1) (string-contains s1 s2 start1 end1 0 (string-length s2))]
      [(s1 s2 start1 end1 start2) (string-contains s1 s2 start1 end1 start2 (string-length s2))]
      [(s1 s2 start1 end1 start2 end2)
       (let ([pat-len (fx- end2 start2)]
             [str-len (fx- end1 start1)])
         (if (fxzero? pat-len) start1
             (let ([last-possible (fx- end1 pat-len)])
               (let loop ([i start1])
                 (and (fx<= i last-possible)
                      (let check ([j 0])
                        (cond
                          [(fx= j pat-len) i]
                          [(char=? (string-ref s1 (fx+ i j))
                                   (string-ref s2 (fx+ start2 j)))
                           (check (fx+ j 1))]
                          [else (loop (fx+ i 1))])))))))]))

  (define string-contains-ci
    (case-lambda
      [(s1 s2) (string-contains-ci s1 s2 0 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1) (string-contains-ci s1 s2 start1 (string-length s1) 0 (string-length s2))]
      [(s1 s2 start1 end1) (string-contains-ci s1 s2 start1 end1 0 (string-length s2))]
      [(s1 s2 start1 end1 start2) (string-contains-ci s1 s2 start1 end1 start2 (string-length s2))]
      [(s1 s2 start1 end1 start2 end2)
       (let ([pat-len (fx- end2 start2)]
             [str-len (fx- end1 start1)])
         (if (fxzero? pat-len) start1
             (let ([last-possible (fx- end1 pat-len)])
               (let loop ([i start1])
                 (and (fx<= i last-possible)
                      (let check ([j 0])
                        (cond
                          [(fx= j pat-len) i]
                          [(char-ci=? (string-ref s1 (fx+ i j))
                                      (string-ref s2 (fx+ start2 j)))
                           (check (fx+ j 1))]
                          [else (loop (fx+ i 1))])))))))]))

  ;; =========================================================
  ;; Reverse & append
  ;; =========================================================

  (define string-reverse
    (case-lambda
      [(s) (string-reverse s 0 (string-length s))]
      [(s start) (string-reverse s start (string-length s))]
      [(s start end)
       (let* ([len (fx- end start)]
              [result (make-string len)])
         (let loop ([i start] [j (fx- len 1)])
           (when (fx< i end)
             (string-set! result j (string-ref s i))
             (loop (fx+ i 1) (fx- j 1))))
         result)]))

  (define string-reverse!
    (case-lambda
      [(s) (string-reverse! s 0 (string-length s))]
      [(s start) (string-reverse! s start (string-length s))]
      [(s start end)
       (let loop ([i start] [j (fx- end 1)])
         (when (fx< i j)
           (let ([tmp (string-ref s i)])
             (string-set! s i (string-ref s j))
             (string-set! s j tmp))
           (loop (fx+ i 1) (fx- j 1))))]))

  (define (string-concatenate lst)
    (apply string-append lst))

  (define string-concatenate-reverse
    (case-lambda
      [(lst) (string-concatenate (reverse lst))]
      [(lst final) (string-concatenate (reverse (cons final lst)))]
      [(lst final end) (string-concatenate (reverse (cons (substring final 0 end) lst)))]))

  ;; "shared" variants — just aliases (no sharing optimisation needed)
  (define string-concatenate/shared string-concatenate)
  (define (string-concatenate-reverse/shared . args)
    (apply string-concatenate-reverse args))
  (define (string-append/shared . strings)
    (apply string-append strings))

  ;; =========================================================
  ;; Fold & map
  ;; =========================================================

  (define string-map
    (case-lambda
      [(proc s) (string-map proc s 0 (string-length s))]
      [(proc s start) (string-map proc s start (string-length s))]
      [(proc s start end)
       (let* ([len (fx- end start)]
              [result (make-string len)])
         (let loop ([i start] [j 0])
           (when (fx< i end)
             (string-set! result j (proc (string-ref s i)))
             (loop (fx+ i 1) (fx+ j 1))))
         result)]))

  ;; string-for-each — imported from Chez (R6RS version)
  ;; But SRFI-13 version accepts start/end bounds
  (define string-for-each
    (case-lambda
      [(proc s) (string-for-each proc s 0 (string-length s))]
      [(proc s start) (string-for-each proc s start (string-length s))]
      [(proc s start end)
       (let loop ([i start])
         (when (fx< i end)
           (proc (string-ref s i))
           (loop (fx+ i 1))))]))

  (define string-fold
    (case-lambda
      [(kons knil s) (string-fold kons knil s 0 (string-length s))]
      [(kons knil s start) (string-fold kons knil s start (string-length s))]
      [(kons knil s start end)
       (let loop ([i start] [acc knil])
         (if (fx>= i end) acc
             (loop (fx+ i 1) (kons (string-ref s i) acc))))]))

  (define string-fold-right
    (case-lambda
      [(kons knil s) (string-fold-right kons knil s 0 (string-length s))]
      [(kons knil s start) (string-fold-right kons knil s start (string-length s))]
      [(kons knil s start end)
       (let loop ([i start])
         (if (fx>= i end) knil
             (kons (string-ref s i) (loop (fx+ i 1)))))]))

  ;; =========================================================
  ;; Replicate & rotate
  ;; =========================================================

  (define xsubstring
    (case-lambda
      [(s from to)
       (let* ([len (string-length s)])
         (xsubstring s from to 0 len))]
      [(s from to start)
       (xsubstring s from to start (string-length s))]
      [(s from to start end)
       (let* ([slen (fx- end start)]
              [rlen (fx- to from)]
              [result (make-string rlen)])
         (when (fxzero? slen) (error 'xsubstring "empty source" s start end))
         (let loop ([i 0])
           (when (fx< i rlen)
             (let ([idx (modulo (fx+ from i) slen)])
               (string-set! result i (string-ref s (fx+ start (if (fx< idx 0) (fx+ idx slen) idx)))))
             (loop (fx+ i 1))))
         result)]))

  (define string-xcopy!
    (case-lambda
      [(target tstart s sfrom sto)
       (string-xcopy! target tstart s sfrom sto 0 (string-length s))]
      [(target tstart s sfrom sto start)
       (string-xcopy! target tstart s sfrom sto start (string-length s))]
      [(target tstart s sfrom sto start end)
       (let ([xs (xsubstring s sfrom sto start end)])
         (string-copy! target tstart xs 0 (string-length xs)))]))

  ;; =========================================================
  ;; Miscellaneous
  ;; =========================================================

  (define string-replace
    (case-lambda
      [(s1 s2 start1 end1)
       (string-replace s1 s2 start1 end1 0 (string-length s2))]
      [(s1 s2 start1 end1 start2)
       (string-replace s1 s2 start1 end1 start2 (string-length s2))]
      [(s1 s2 start1 end1 start2 end2)
       (string-append (substring s1 0 start1)
                      (substring s2 start2 end2)
                      (substring s1 end1 (string-length s1)))]))

  (define string-tokenize
    (case-lambda
      [(s) (string-tokenize s (lambda (c) (not (char-whitespace? c))))]
      [(s token-set) (string-tokenize s token-set 0 (string-length s))]
      [(s token-set start) (string-tokenize s token-set start (string-length s))]
      [(s token-set start end)
       (let ([pred (make-char-pred token-set)])
         (let loop ([i end] [acc '()])
           (cond
             [(fx<= i start) acc]
             [else
              ;; skip non-token chars from the right
              (let skip ([j (fx- i 1)])
                (cond
                  [(fx< j start) acc]
                  [(not (pred (string-ref s j))) (skip (fx- j 1))]
                  [else
                   ;; found end of a token; scan for start
                   (let scan ([k j])
                     (cond
                       [(and (fx> k start) (pred (string-ref s (fx- k 1))))
                        (scan (fx- k 1))]
                       [else
                        (loop k (cons (substring s k (fx+ j 1)) acc))]))]))])))]))

  (define string-filter
    (case-lambda
      [(criterion s) (string-filter criterion s 0 (string-length s))]
      [(criterion s start) (string-filter criterion s start (string-length s))]
      [(criterion s start end)
       (let ([pred (make-char-pred criterion)]
             [port (open-output-string)])
         (let loop ([i start])
           (when (fx< i end)
             (let ([c (string-ref s i)])
               (when (pred c) (write-char c port)))
             (loop (fx+ i 1))))
         (get-output-string port))]))

  (define string-delete
    (case-lambda
      [(criterion s) (string-delete criterion s 0 (string-length s))]
      [(criterion s start) (string-delete criterion s start (string-length s))]
      [(criterion s start end)
       (let ([pred (make-char-pred criterion)])
         (string-filter (lambda (c) (not (pred c))) s start end))]))

  ;; string-copy! — Chez has it natively
  (define string-copy!
    (case-lambda
      [(target tstart source)
       (string-copy! target tstart source 0 (string-length source))]
      [(target tstart source sstart)
       (string-copy! target tstart source sstart (string-length source))]
      [(target tstart source sstart send)
       (let loop ([i sstart] [j tstart])
         (when (fx< i send)
           (string-set! target j (string-ref source i))
           (loop (fx+ i 1) (fx+ j 1))))]))

  ;; string-fill! — Chez has it but SRFI-13 version takes start/end
  (define string-fill!
    (case-lambda
      [(s char) (string-fill! s char 0 (string-length s))]
      [(s char start) (string-fill! s char start (string-length s))]
      [(s char start end)
       (let loop ([i start])
         (when (fx< i end)
           (string-set! s i char)
           (loop (fx+ i 1))))]))

  ;; =========================================================
  ;; I/O
  ;; =========================================================

  (define string-join
    (case-lambda
      [(lst) (string-join lst " " 'infix)]
      [(lst delim) (string-join lst delim 'infix)]
      [(lst delim grammar)
       (cond
         [(and (null? lst) (eq? grammar 'strict-infix))
          (error 'string-join "strict-infix requires non-empty list")]
         [(null? lst) ""]
         [else
          (let ([port (open-output-string)])
            (case grammar
              [(prefix)
               (for-each (lambda (s) (display delim port) (display s port)) lst)]
              [(suffix)
               (for-each (lambda (s) (display s port) (display delim port)) lst)]
              [else  ; infix, strict-infix
               (display (car lst) port)
               (for-each (lambda (s) (display delim port) (display s port)) (cdr lst))])
            (get-output-string port))])]))

  ;; =========================================================
  ;; Comparison predicates with optional start/end bounds
  ;; =========================================================

  (define (%string-compare2 s1 s2 start1 end1 start2 end2 cmp)
    (let ([len1 (fx- end1 start1)]
          [len2 (fx- end2 start2)])
      (let ([minlen (fxmin len1 len2)])
        (let loop ([i 0])
          (if (fx= i minlen)
              (cmp len1 len2)
              (let ([c1 (string-ref s1 (fx+ start1 i))]
                    [c2 (string-ref s2 (fx+ start2 i))])
                (if (char=? c1 c2)
                    (loop (fx+ i 1))
                    (cmp (char->integer c1) (char->integer c2)))))))))

  (define (%string-compare2-ci s1 s2 start1 end1 start2 end2 cmp)
    (let ([len1 (fx- end1 start1)]
          [len2 (fx- end2 start2)])
      (let ([minlen (fxmin len1 len2)])
        (let loop ([i 0])
          (if (fx= i minlen)
              (cmp len1 len2)
              (let ([c1 (char-downcase (string-ref s1 (fx+ start1 i)))]
                    [c2 (char-downcase (string-ref s2 (fx+ start2 i)))])
                (if (char=? c1 c2)
                    (loop (fx+ i 1))
                    (cmp (char->integer c1) (char->integer c2)))))))))

  (define-syntax define-string-cmp
    (syntax-rules ()
      [(_ name cmp-fn pred)
       (define name
         (case-lambda
           [(s1 s2) (name s1 s2 0 (string-length s1) 0 (string-length s2))]
           [(s1 s2 start1) (name s1 s2 start1 (string-length s1) 0 (string-length s2))]
           [(s1 s2 start1 end1) (name s1 s2 start1 end1 0 (string-length s2))]
           [(s1 s2 start1 end1 start2) (name s1 s2 start1 end1 start2 (string-length s2))]
           [(s1 s2 start1 end1 start2 end2)
            (cmp-fn s1 s2 start1 end1 start2 end2 pred)]))]))

  (define-string-cmp string=  %string-compare2    (lambda (a b) (fx= a b)))
  (define-string-cmp string<> %string-compare2    (lambda (a b) (not (fx= a b))))
  (define-string-cmp string<  %string-compare2    (lambda (a b) (fx< a b)))
  (define-string-cmp string>  %string-compare2    (lambda (a b) (fx> a b)))
  (define-string-cmp string<= %string-compare2    (lambda (a b) (fx<= a b)))
  (define-string-cmp string>= %string-compare2    (lambda (a b) (fx>= a b)))
  (define-string-cmp string-ci=  %string-compare2-ci (lambda (a b) (fx= a b)))
  (define-string-cmp string-ci<> %string-compare2-ci (lambda (a b) (not (fx= a b))))
  (define-string-cmp string-ci<  %string-compare2-ci (lambda (a b) (fx< a b)))
  (define-string-cmp string-ci>  %string-compare2-ci (lambda (a b) (fx> a b)))
  (define-string-cmp string-ci<= %string-compare2-ci (lambda (a b) (fx<= a b)))
  (define-string-cmp string-ci>= %string-compare2-ci (lambda (a b) (fx>= a b)))

  ;; =========================================================
  ;; Hashing
  ;; =========================================================

  (define string-hash
    (case-lambda
      [(s) (string-hash s 536870911)]
      [(s bound) (string-hash s bound 0 (string-length s))]
      [(s bound start) (string-hash s bound start (string-length s))]
      [(s bound start end)
       (let loop ([i start] [h 0])
         (if (fx>= i end)
             (modulo h bound)
             (loop (fx+ i 1)
                   (fx+ (fxsll h 5) (fxsra h 27)
                        (char->integer (string-ref s i))))))]))

  (define string-hash-ci
    (case-lambda
      [(s) (string-hash-ci s 536870911)]
      [(s bound) (string-hash-ci s bound 0 (string-length s))]
      [(s bound start) (string-hash-ci s bound start (string-length s))]
      [(s bound start end)
       (let loop ([i start] [h 0])
         (if (fx>= i end)
             (modulo h bound)
             (loop (fx+ i 1)
                   (fx+ (fxsll h 5) (fxsra h 27)
                        (char->integer (char-downcase (string-ref s i)))))))]))

  ;; =========================================================
  ;; Linear-update & aliases
  ;; =========================================================

  (define (string-map! proc s . rest)
    (let ([start (if (pair? rest) (car rest) 0)]
          [end (if (and (pair? rest) (pair? (cdr rest)))
                   (cadr rest)
                   (string-length s))])
      (let loop ([i start])
        (when (fx< i end)
          (string-set! s i (proc (string-ref s i)))
          (loop (fx+ i 1))))))

  (define substring/shared substring)

  (define string-copy/shared string-copy)

)
