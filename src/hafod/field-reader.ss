;;; (hafod field-reader) -- Field splitting, record reading, string joining
;;; Port of scsh/scheme/fr.scm for Chez Scheme R6RS.
;;; Copyright (c) 1994 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod field-reader)
  (export field-splitter infix-splitter suffix-splitter sloppy-suffix-splitter
          record-reader field-reader
          join-strings)
  (import (hafod internal base)
          (hafod compat)
          (hafod re)
          (hafod rdelim))

  ;; ======================================================================
  ;; Delimiter matcher coercion
  ;; ======================================================================
  ;; A delimiter matcher is a procedure (lambda (s i) -> (values start end) or (values #f #f)).
  ;; It finds the next occurrence of the delimiter pattern at or after position i in string s.

  ;; Escape POSIX ERE special characters in a literal string.
  (define (posix-quote-literal s)
    (let ((specials '(#\. #\[ #\] #\* #\+ #\? #\{ #\} #\| #\( #\) #\^ #\$ #\\)))
      (let loop ((i 0) (acc '()))
        (if (>= i (string-length s))
            (apply string-append (reverse acc))
            (let ((c (string-ref s i)))
              (if (memv c specials)
                  (loop (+ i 1) (cons (string c) (cons "\\" acc)))
                  (loop (+ i 1) (cons (string c) acc))))))))

  ;; Convert a delimiter specification to a matcher procedure.
  (define (->delim-matcher x)
    (cond
      ((procedure? x) x)
      ((string? x)
       (let ((re (string->regexp (posix-quote-literal x))))
         (lambda (s i)
           (let ((m (regexp-search re s i)))
             (if m (values (match:start m 0) (match:end m 0))
                 (values #f #f))))))
      ((char? x)
       (->delim-matcher (string x)))
      ((char-set? x)
       ;; Match single char from set, scanning from position i
       (lambda (s i)
         (let ((len (string-length s)))
           (let lp ((j i))
             (cond
               ((>= j len) (values #f #f))
               ((char-set-contains? x (string-ref s j))
                (values j (+ j 1)))
               (else (lp (+ j 1))))))))
      ((regexp? x)
       (lambda (s i)
         (let ((m (regexp-search x s i)))
           (if m (values (match:start m 0) (match:end m 0))
               (values #f #f)))))
      (else (error 'field-reader "Illegal delimiter value" x))))

  ;; ======================================================================
  ;; Field loop procedures
  ;; ======================================================================
  ;; These implement the core logic for each splitter type.
  ;; All return the answer as a reversed list.

  ;; Field-spec loop: matches FIELDS (not delimiters).
  (define (fieldspec-field-loop s start match-field num-fields nfields-exact?)
    (let ((end (string-length s)))
      (let lp ((i start) (nfields 0) (fields '()) (last-null? #f))
        (let ((j (if last-null? (+ i 1) i))
              (finish-up (lambda ()
                           (if (and num-fields (< nfields num-fields))
                               (error 'field-splitter "Too few fields in record" num-fields s)
                               fields))))
          (cond
            ((> j end) (finish-up))

            ;; Read too many fields
            ((and nfields-exact? (> nfields num-fields))
             (error 'field-splitter "Too many fields in record" num-fields s))

            ;; Made our lower-bound quota - quit early
            ((and num-fields (= nfields num-fields) (not nfields-exact?))
             (if (= i end) fields
                 (cons (substring s i end) fields)))

            ;; Match off another field
            (else (receive (m0 m1) (match-field s j)
                    (if m0 (lp m1 (+ nfields 1)
                               (cons (substring s m0 m1) fields)
                               (= m0 m1))
                        (finish-up)))))))))

  ;; Infix loop: delimiter SEPARATES fields.
  (define (infix-field-loop s start match-delim cons-field
                            num-fields nfields-exact?)
    (let ((end (string-length s)))
      (if (= start end) '()
          (let lp ((i start) (nfields 0) (fields '()) (last-null? #f))
            (let ((finish-up (lambda ()
                               (cond
                                 ((and num-fields (< (+ nfields 1) num-fields))
                                  (error 'infix-splitter "Too few fields in record"
                                         num-fields s))
                                 ((and nfields-exact? (>= nfields num-fields))
                                  (error 'infix-splitter "Too many fields in record"
                                         num-fields s))
                                 (else
                                  (cons (substring s i end) fields)))))
                  (j (if last-null? (+ i 1) i)))
              (cond
                ;; If we've read NUM-FIELDS fields, quit early
                ((and num-fields (= nfields num-fields))
                 (if nfields-exact?
                     (error 'infix-splitter "Too many fields in record" num-fields s)
                     (cons (substring s i end) fields)))

                ((<= j end)
                 (receive (m0 m1) (match-delim s j)
                   (if m0
                       (lp m1 (+ nfields 1)
                           (cons-field s i m0 m1 fields)
                           (= m0 m1))
                       (finish-up))))

                (else (finish-up))))))))

  ;; Suffix loop: delimiter TERMINATES fields.
  (define (suffix-field-loop s start match-delim cons-field
                             num-fields nfields-exact?)
    (let ((end (string-length s)))
      (let lp ((i start) (nfields 0) (fields '()) (last-null? #f))
        (let ((j (if last-null? (+ i 1) i)))
          (cond
            ;; Done - at end of string
            ((= i end)
             (if (and num-fields (< nfields num-fields))
                 (error 'suffix-splitter "Too few fields in record" num-fields s)
                 fields))

            ;; Read too many fields
            ((and nfields-exact? (= nfields num-fields))
             (error 'suffix-splitter "Too many fields in record" num-fields s))

            ;; Made lower-bound quota - quit early
            ((and num-fields (= nfields num-fields) (not nfields-exact?))
             (cons (substring s i end) fields))

            ;; Match off another field
            (else
             (receive (m0 m1) (match-delim s j)
               (if m0 (lp m1 (+ nfields 1)
                          (cons-field s i m0 m1 fields)
                          (= m0 m1))
                   (error 'suffix-splitter "Missing field terminator" s)))))))))

  ;; Sloppy-suffix loop: skip optional initial delimiter, then suffix.
  (define (sloppy-suffix-field-loop s start match-delim cons-field
                                    num-fields nfields-exact?)
    (let ((start (receive (i j) (match-delim s start)
                   (if (and i (= i start)) j start))))
      (suffix-field-loop s start match-delim cons-field
                         num-fields nfields-exact?)))

  ;; ======================================================================
  ;; Parser generator
  ;; ======================================================================

  (define (make-field-parser-generator default-delim-matcher loop-proc)
    (lambda args
      (let-optionals* args ((delim-spec default-delim-matcher)
                            (num-fields #f)
                            (handle-delim 'trim))
        (let ((match-delim (->delim-matcher delim-spec))
              (cons-field (case handle-delim
                            ((trim)
                             (lambda (s i j k fields)
                               (cons (substring s i j) fields)))
                            ((split)
                             (lambda (s i j k fields)
                               (cons (substring s j k)
                                     (cons (substring s i j) fields))))
                            ((concat)
                             (lambda (s i j k fields)
                               (cons (substring s i k) fields)))
                            (else
                             (error 'field-splitter
                                    "Illegal handle-delim spec" handle-delim)))))
          (receive (num-fields nfields-exact?)
                   (cond ((not num-fields) (values #f #f))
                         ((not (integer? num-fields))
                          (error 'field-splitter "Illegal NUM-FIELDS value" num-fields))
                         ((<= num-fields 0) (values (- num-fields) #f))
                         (else (values num-fields #t)))
            (lambda (s . maybe-start)
              (reverse (loop-proc s (:optional maybe-start 0)
                                  match-delim cons-field
                                  num-fields nfields-exact?))))))))

  ;; ======================================================================
  ;; Default matchers
  ;; ======================================================================

  ;; Default field matcher: runs of non-whitespace
  (define default-field-matcher (->delim-matcher (rx (+ (~ white)))))

  ;; Default infix matcher: runs of whitespace
  (define default-infix-matcher (->delim-matcher (rx (+ white))))

  ;; Default suffix matcher: runs of whitespace OR end-of-string.
  ;; Use a custom procedure to avoid POSIX regex edge cases with $ matching.
  (define (default-suffix-matcher s i)
    (let ((len (string-length s)))
      (let scan ((j i))
        (cond
          ((>= j len) (values j j))  ;; eos: empty match at end
          ((char-whitespace? (string-ref s j))
           (let find-end ((k (+ j 1)))
             (if (or (>= k len) (not (char-whitespace? (string-ref s k))))
                 (values j k)
                 (find-end (+ k 1)))))
          (else (scan (+ j 1)))))))

  ;; ======================================================================
  ;; Exported splitter constructors
  ;; ======================================================================

  ;; field-splitter: matches FIELDS (not delimiters). Different API than infix/suffix.
  (define (field-splitter . args)
    (let-optionals* args ((field-spec default-field-matcher)
                          (num-fields #f))
      (let ((match-field (->delim-matcher field-spec)))
        (receive (num-fields nfields-exact?)
                 (cond ((not num-fields) (values #f #f))
                       ((not (integer? num-fields))
                        (error 'field-splitter "Illegal NUM-FIELDS value"
                               field-splitter num-fields))
                       ((<= num-fields 0) (values (- num-fields) #f))
                       (else (values num-fields #t)))
          (lambda (s . maybe-start)
            (reverse (fieldspec-field-loop s (:optional maybe-start 0)
                                           match-field num-fields nfields-exact?)))))))

  (define infix-splitter
    (make-field-parser-generator default-infix-matcher infix-field-loop))

  (define suffix-splitter
    (make-field-parser-generator default-suffix-matcher suffix-field-loop))

  (define sloppy-suffix-splitter
    (make-field-parser-generator default-suffix-matcher sloppy-suffix-field-loop))

  ;; ======================================================================
  ;; Record reader
  ;; ======================================================================

  (define default-record-delims (char-set #\newline))

  ;; (record-reader [delims elide? handle-delim]) -> reader
  ;; (reader [port]) -> string or eof  (or (values string delim) for split)
  (define (record-reader . args)
    (let-optionals* args ((delims default-record-delims)
                          (elide? #f)
                          (handle-delim 'trim))
      (let ((delims (x->char-set delims)))
        (case handle-delim
          ((trim)
           (lambda maybe-port
             (let ((s (apply read-delimited delims maybe-port)))
               (when (and (not (eof-object? s)) elide?)
                 (apply skip-char-set delims maybe-port))
               s)))

          ((concat)
           (let ((not-delims (char-set-complement delims)))
             (lambda maybe-port
               (let* ((p (:optional maybe-port (current-input-port)))
                      (s (read-delimited delims p 'concat)))
                 (if (or (not elide?) (eof-object? s)) s
                     (let ((extra-delims (read-delimited not-delims p 'peek)))
                       (if (eof-object? extra-delims) s
                           (string-append s extra-delims))))))))

          ((split)
           (let ((not-delims (char-set-complement delims)))
             (lambda maybe-port
               (let ((p (:optional maybe-port (current-input-port))))
                 (receive (s delim) (read-delimited delims p 'split)
                   (if (eof-object? s) (values s s)
                       (values s
                               (if (or (not elide?) (eof-object? delim))
                                   delim
                                   ;; Elide: slurp in extra delims
                                   (let ((delim-str (string delim))
                                         (extras (read-delimited not-delims p 'peek)))
                                     (if (eof-object? extras) delim-str
                                         (string-append delim-str extras)))))))))))

          (else
           (error 'record-reader "Illegal delimiter-action" handle-delim))))))

  ;; ======================================================================
  ;; Field reader
  ;; ======================================================================

  (define default-field-parser (field-splitter))

  ;; (field-reader [field-parser rec-reader]) -> reader
  ;; (reader [port]) -> (values raw-record parsed-fields) or (values eof '())
  (define (field-reader . args)
    (let-optionals* args ((parser default-field-parser)
                          (rec-reader read-line))
      (lambda maybe-port
        (let ((record (apply rec-reader maybe-port)))
          (if (eof-object? record)
              (values record '())
              (values record (parser record)))))))

  ;; ======================================================================
  ;; join-strings
  ;; ======================================================================

  (define join-strings
    (case-lambda
      ((lst) (join-strings lst " "))
      ((lst delim)
       (if (null? lst) ""
           (let loop ((rest (cdr lst)) (acc (car lst)))
             (if (null? rest) acc
                 (loop (cdr rest) (string-append acc delim (car rest)))))))))

  ) ; end library
