;;; (hafod internal re-engine) -- Compiled regex engine for hafod
;;; Extracted from (hafod re) -- compiled-regexp record, match record,
;;; search/match operations, substitution, and fold/iteration.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod internal re-engine)
  (export
    ;; Public exports (re-exported by re.ss facade)
    regexp? make-regexp string->regexp
    regexp-search regexp-search? regexp-match
    match:start match:end match:substring match:count
    regexp-match?
    regexp-substitute regexp-substitute/global
    regexp-fold regexp-for-each regexp-fold-right
    re-adt->compiled-regexp
    ;; Internal exports (needed by re-macros, NOT re-exported by facade)
    coerce-regexp finalize-regexes!
    compiled-regexp-type? compiled-regexp-type-num-submatches
    compiled-regexp-type-regex-t compiled-regexp-type-submatch-map
    compiled-regexp-type-posix-string
    count-posix-parens
    regexp-match-type-string regexp-match-type-submatches
    build-match-vector)
  (import (hafod internal base)
          (hafod posix)
          (hafod compat)
          (rename (only (hafod internal re-records) regexp?)
                  (regexp? re-adt?))
          (only (hafod internal re-posixstr) regexp->posix-string))

  ;; ======================================================================
  ;; Compiled regex record
  ;; ======================================================================

  ;; submatch-map: a vector where element i (0-based) is the POSIX 1-based
  ;; paren index for user submatch i. #f means identity mapping (no remapping).
  ;; For string->regexp (no SRE), submatch-map is #f (identity).
  (define-record-type compiled-regexp-type
    (fields
      posix-string
      cflags
      (mutable regex-t)
      num-submatches
      submatch-map)
    (nongenerative hafod-compiled-regexp))

  ;; Guardian for regex_t cleanup
  (define regex-guardian (make-guardian))

  (define (finalize-regexes!)
    (let loop ()
      (let ((crx (regex-guardian)))
        (when crx
          (let ((rt (compiled-regexp-type-regex-t crx)))
            (when rt (posix-regfree rt)))
          (loop)))))

  ;; Create a compiled regexp, eagerly compiling the POSIX string.
  ;; smap is a vector mapping user submatch indices to POSIX paren indices,
  ;; or #f for identity mapping.
  (define make-regexp
    (case-lambda
      ((posix-str cflags nsub)
       (make-regexp posix-str cflags nsub #f))
      ((posix-str cflags nsub smap)
       (finalize-regexes!)
       (let* ((rt (posix-regcomp posix-str (bitwise-ior REG_EXTENDED cflags)))
              (crx (make-compiled-regexp-type posix-str cflags rt nsub smap)))
         (regex-guardian crx)
         crx))))

  ;; Convenience: compile from a POSIX regex string.
  ;; Counts unescaped parens to determine number of submatches.
  (define (string->regexp pattern . maybe-flags)
    (let ((extra-flags (if (null? maybe-flags) 0 (car maybe-flags)))
          (nsub (count-posix-parens pattern)))
      (make-regexp pattern extra-flags nsub #f)))

  ;; Count unescaped ( in a POSIX regex string (runtime version).
  (define (count-posix-parens s)
    (let ((len (string-length s)))
      (let loop ((i 0) (n 0))
        (if (>= i len)
            n
            (let ((c (string-ref s i)))
              (cond
                ((and (char=? c #\\) (< (+ i 1) len))
                 (loop (+ i 2) n))
                ((char=? c #\()
                 (loop (+ i 1) (+ n 1)))
                (else
                 (loop (+ i 1) n))))))))

  ;; Predicate: recognizes both compiled regexps and RE ADT values
  (define (regexp? x) (or (compiled-regexp-type? x) (re-adt? x)))

  ;; ======================================================================
  ;; Match object record
  ;; ======================================================================

  (define-record-type regexp-match-type
    (fields
      string
      submatches)
    (nongenerative hafod-regexp-match))

  (define (regexp-match? x) (regexp-match-type? x))

  (define (match:start m . maybe-i)
    (let* ((i (if (null? maybe-i) 0 (car maybe-i)))
           (subs (regexp-match-type-submatches m)))
      (and (< i (vector-length subs))
           (let ((sm (vector-ref subs i)))
             (and sm (car sm))))))

  (define (match:end m . maybe-i)
    (let* ((i (if (null? maybe-i) 0 (car maybe-i)))
           (subs (regexp-match-type-submatches m)))
      (and (< i (vector-length subs))
           (let ((sm (vector-ref subs i)))
             (and sm (cdr sm))))))

  (define (match:substring m . maybe-i)
    (let* ((i (if (null? maybe-i) 0 (car maybe-i)))
           (subs (regexp-match-type-submatches m)))
      (and (< i (vector-length subs))
           (let ((sm (vector-ref subs i)))
             (and sm (substring (regexp-match-type-string m) (car sm) (cdr sm)))))))

  (define (match:count m)
    (vector-length (regexp-match-type-submatches m)))

  ;; ======================================================================
  ;; Search operations
  ;; ======================================================================

  ;; Build a user-facing match vector from raw POSIX regexec results.
  ;; smap is a vector mapping user submatch indices (0-based) to POSIX paren
  ;; indices (1-based), or #f for identity mapping.
  ;; User submatch 0 is always the whole match (POSIX index 0).
  (define (build-match-vector raw-result smap start)
    (if (not smap)
        ;; Identity mapping: user index i = POSIX index i
        (let ((v (make-vector (vector-length raw-result) #f)))
          (let loop ((i 0))
            (when (< i (vector-length raw-result))
              (let ((pair (vector-ref raw-result i)))
                (when pair
                  (vector-set! v i (cons (+ start (car pair))
                                         (+ start (cdr pair))))))
              (loop (+ i 1))))
          v)
        ;; Remapped: user submatch 0 = POSIX 0 (whole match),
        ;; user submatch k = POSIX smap[k-1] for k >= 1
        (let* ((n-user (+ 1 (vector-length smap)))
               (v (make-vector n-user #f)))
          ;; User index 0 = whole match = POSIX index 0
          (let ((pair (vector-ref raw-result 0)))
            (when pair
              (vector-set! v 0 (cons (+ start (car pair))
                                     (+ start (cdr pair))))))
          ;; User index k (1-based) maps to POSIX index smap[k-1]
          ;; posix-idx can be #f for dead submatches (DSM)
          (let loop ((k 1))
            (when (<= k (vector-length smap))
              (let ((posix-idx (vector-ref smap (- k 1))))
                (when (and posix-idx (< posix-idx (vector-length raw-result)))
                  (let ((pair (vector-ref raw-result posix-idx)))
                    (when pair
                      (vector-set! v k (cons (+ start (car pair))
                                             (+ start (cdr pair))))))))
              (loop (+ k 1))))
          v)))

  ;; Bridge: compile an RE ADT value to a compiled-regexp.
  (define (re-adt->compiled-regexp re)
    (let-values (((posix-str level pcount smap) (regexp->posix-string re)))
      (if (not posix-str)
          (error 're-adt->compiled-regexp "RE can never match" re)
          (make-regexp posix-str 0 pcount
                       (if (= 0 (vector-length smap)) #f smap)))))

  ;; Coerce any regex representation to compiled-regexp-type.
  (define (coerce-regexp re)
    (cond
     ((compiled-regexp-type? re) re)
     ((re-adt? re) (re-adt->compiled-regexp re))
     ((string? re) (string->regexp re))
     (else (error 'coerce-regexp "not a regexp, RE ADT, or string" re))))

  (define (regexp-search re str . maybe-start)
    (finalize-regexes!)
    (let* ((crx (coerce-regexp re))
           (start (if (null? maybe-start) 0 (car maybe-start)))
           (search-str (if (= start 0) str (substring str start (string-length str))))
           (nsub (+ 1 (compiled-regexp-type-num-submatches crx)))
           (result (posix-regexec (compiled-regexp-type-regex-t crx) search-str nsub 0)))
      (and result
           (make-regexp-match-type
             str
             (build-match-vector result
                                 (compiled-regexp-type-submatch-map crx)
                                 start)))))

  (define (regexp-search? re str . maybe-start)
    (and (apply regexp-search re str maybe-start) #t))

  (define (regexp-match re str)
    (let ((m (regexp-search re str 0)))
      (and m (= 0 (match:start m 0)) m)))

  ;; ======================================================================
  ;; Substitution
  ;; ======================================================================

  (define (regexp-substitute port match . items)
    (let* ((str (regexp-match-type-string match))
           (range (lambda (item)
                    (cond
                      ((integer? item)
                       (let ((s (match:start match item))
                             (e (match:end match item)))
                         (if s (cons s e) (cons 0 0))))
                      ((eq? 'pre item)
                       (cons 0 (match:start match 0)))
                      ((eq? 'post item)
                       (cons (match:end match 0) (string-length str)))
                      (else (error 'regexp-substitute "illegal item" item))))))
      (if port
          (for-each
            (lambda (item)
              (cond
                ((string? item) (display item port))
                ((procedure? item) (display (item match) port))
                (else
                  (let ((r (range item)))
                    (display (substring str (car r) (cdr r)) port)))))
            items)
          (apply string-append
                 (map (lambda (item)
                        (cond
                          ((string? item) item)
                          ((procedure? item) (item match))
                          (else
                            (let ((r (range item)))
                              (substring str (car r) (cdr r))))))
                      items)))))

  (define (regexp-substitute/global port re str . items)
    (let* ((crx (coerce-regexp re))
           (str-len (string-length str)))
      (let ((pieces
              (let recur ((start 0))
                (if (> start str-len)
                    '()
                    (let ((match (regexp-search crx str start)))
                      (if match
                          (let* ((m-start (match:start match 0))
                                 (m-end (match:end match 0))
                                 (empty? (= m-start m-end)))
                            (let process-items ((items items) (acc '()))
                              (if (null? items)
                                  (reverse acc)
                                  (let ((item (car items)))
                                    (cond
                                      ((string? item)
                                       (process-items (cdr items) (cons item acc)))
                                      ((integer? item)
                                       (let ((s (match:substring match item)))
                                         (process-items (cdr items) (cons (or s "") acc))))
                                      ((eq? 'pre item)
                                       (process-items (cdr items)
                                                      (cons (substring str start m-start) acc)))
                                      ((eq? 'post item)
                                       (let* ((next-start (if empty? (+ m-end 1) m-end))
                                              (rest (recur next-start))
                                              (skip (if (and empty? (< m-end str-len))
                                                        (list (string (string-ref str m-end)))
                                                        '())))
                                         (process-items (cdr items)
                                                        (append (reverse rest) (reverse skip) acc))))
                                      ((procedure? item)
                                       (process-items (cdr items) (cons (item match) acc)))
                                      (else
                                       (error 'regexp-substitute/global "illegal item" item)))))))
                          ;; No match: return rest of string
                          (list (substring str start str-len))))))))
        (let ((result (apply string-append pieces)))
          (if port
              (display result port)
              result)))))

  ;; ======================================================================
  ;; Fold / iteration
  ;; ======================================================================

  (define (regexp-fold re kons knil s . rest)
    (let* ((crx (coerce-regexp re))
           (finish (if (and (pair? rest) (car rest))
                       (car rest)
                       (lambda (i x) x)))
           (start (if (and (pair? rest) (pair? (cdr rest)))
                      (cadr rest)
                      0)))
      (when (> start (string-length s))
        (error 'regexp-fold "start index exceeds string length" start))
      (let loop ((i start) (val knil))
        (let ((m (regexp-search crx s i)))
          (if m
              (let ((next-i (match:end m 0)))
                (when (= next-i (match:start m 0))
                  (error 'regexp-fold
                         "zero-length match would cause infinite loop"
                         s i))
                (loop next-i (kons i m val)))
              (finish i val))))))

  (define (regexp-for-each re proc s . maybe-start)
    (let* ((crx (coerce-regexp re))
           (start (if (null? maybe-start) 0 (car maybe-start))))
      (when (> start (string-length s))
        (error 'regexp-for-each "start index exceeds string length" start))
      (let loop ((i start))
        (let ((m (regexp-search crx s i)))
          (when m
            (let ((next-i (match:end m 0)))
              (when (= (match:start m 0) next-i)
                (error 'regexp-for-each
                       "zero-length match would cause infinite loop"
                       s i))
              (proc m)
              (loop next-i)))))))

  ;; ======================================================================
  ;; regexp-fold-right
  ;; ======================================================================
  ;; Port of scsh/rx/re-fold.scm regexp-fold-right.
  ;; Folds right-to-left over matches.
  ;; kons: (match next-non-match-start value) -> value
  ;; finish: (first-match-start value) -> value

  (define (regexp-fold-right re kons knil s . rest)
    (let* ((crx (coerce-regexp re))
           (finish (if (and (pair? rest) (car rest))
                       (car rest)
                       (lambda (i x) x)))
           (start (if (and (pair? rest) (pair? (cdr rest)))
                      (cadr rest)
                      0)))
      (when (> start (string-length s))
        (error 'regexp-fold-right "start index exceeds string length" start))
      (cond
       ((regexp-search crx s start) =>
        (lambda (m)
          (finish (match:start m 0)
                  (let recur ((last-m m))
                    (cond
                     ((regexp-search crx s (match:end last-m 0)) =>
                      (lambda (m)
                        (let ((i (match:start m 0)))
                          (when (= i (match:end m 0))
                            (error 'regexp-fold-right
                                   "zero-length match would cause infinite loop"
                                   s i))
                          (kons last-m i (recur m)))))
                     (else (kons last-m (string-length s) knil)))))))
       (else (finish (string-length s) knil)))))


  ) ; end library
