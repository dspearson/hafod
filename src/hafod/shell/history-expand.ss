;;; (hafod shell history-expand) -- History expansion (bang expansion)
;;; Expands history references in input before evaluation:
;;;   !!       — last command
;;;   !$       — last argument of last command
;;;   !n       — nth history entry (1-based)
;;;   !-n      — nth from last
;;;   !prefix  — most recent command starting with prefix
;;;   \!       — literal ! (handled by caller's escape)
;;; A ! followed by space, end-of-line, or = is not expanded (bash convention).

(library (hafod shell history-expand)
  (export history-expand)

  (import (chezscheme)
          (only (hafod srfi-13) string-prefix?))

  ;; Expand history references in str using entries (vector, most recent last).
  ;; Returns the expanded string, or str unchanged if no expansion needed.
  ;; Signals an error string via (values str error-msg) on bad references.
  (define (history-expand str entries)
    (let ([len (string-length str)]
          [n-entries (vector-length entries)])
      (if (= n-entries 0)
          str
          (let loop ([i 0] [acc '()])
            (cond
              [(>= i len)
               (list->string (reverse acc))]
              ;; Escaped !
              [(and (char=? (string-ref str i) #\\)
                    (< (+ i 1) len)
                    (char=? (string-ref str (+ i 1)) #\!))
               (loop (+ i 2) (cons #\! acc))]
              ;; ! — potential expansion
              [(char=? (string-ref str i) #\!)
               (if (>= (+ i 1) len)
                   ;; Trailing ! — literal
                   (loop (+ i 1) (cons #\! acc))
                   (let ([next (string-ref str (+ i 1))])
                     (cond
                       ;; ! followed by space/tab/= — literal (bash convention)
                       [(or (char=? next #\space)
                            (char=? next #\tab)
                            (char=? next #\=))
                        (loop (+ i 1) (cons #\! acc))]
                       ;; !! — last command
                       [(char=? next #\!)
                        (let ([last (vector-ref entries (- n-entries 1))])
                          (loop (+ i 2) (append (reverse (string->list last)) acc)))]
                       ;; !$ — last argument of last command
                       [(char=? next #\$)
                        (let* ([last (vector-ref entries (- n-entries 1))]
                               [arg (last-argument last)])
                          (loop (+ i 2) (append (reverse (string->list arg)) acc)))]
                       ;; !-n — nth from last
                       [(char=? next #\-)
                        (let-values ([(num end) (read-number str (+ i 2) len)])
                          (if (and num (> num 0) (<= num n-entries))
                              (let ([entry (vector-ref entries (- n-entries num))])
                                (loop end (append (reverse (string->list entry)) acc)))
                              ;; Bad reference — keep literal
                              (loop (+ i 1) (cons #\! acc))))]
                       ;; !n — nth entry (1-based)
                       [(char-numeric? next)
                        (let-values ([(num end) (read-number str (+ i 1) len)])
                          (if (and num (>= num 1) (<= num n-entries))
                              (let ([entry (vector-ref entries (- num 1))])
                                (loop end (append (reverse (string->list entry)) acc)))
                              ;; Bad reference — keep literal
                              (loop (+ i 1) (cons #\! acc))))]
                       ;; !prefix — most recent match
                       [(or (char-alphabetic? next) (char=? next #\_))
                        (let-values ([(prefix end) (read-prefix str (+ i 1) len)])
                          (let search ([j (- n-entries 1)])
                            (cond
                              [(< j 0)
                               ;; No match — keep literal
                               (loop (+ i 1) (cons #\! acc))]
                              [(string-prefix? prefix (vector-ref entries j))
                               (let ([entry (vector-ref entries j)])
                                 (loop end (append (reverse (string->list entry)) acc)))]
                              [else (search (- j 1))])))]
                       ;; Unknown — literal
                       [else (loop (+ i 1) (cons #\! acc))])))]
              ;; Normal character
              [else
               (loop (+ i 1) (cons (string-ref str i) acc))])))))

  ;; Read digits starting at pos, return (values number end-pos) or (values #f pos).
  (define (read-number str pos len)
    (let loop ([i pos] [chars '()])
      (if (and (< i len) (char-numeric? (string-ref str i)))
          (loop (+ i 1) (cons (string-ref str i) chars))
          (if (null? chars)
              (values #f pos)
              (values (string->number (list->string (reverse chars))) i)))))

  ;; Read a prefix word (alphanumeric + _-.) starting at pos.
  (define (read-prefix str pos len)
    (let loop ([i pos] [chars '()])
      (if (and (< i len)
               (let ([c (string-ref str i)])
                 (or (char-alphabetic? c) (char-numeric? c)
                     (char=? c #\_) (char=? c #\-) (char=? c #\.))))
          (loop (+ i 1) (cons (string-ref str i) chars))
          (values (list->string (reverse chars)) i))))

  ;; string-prefix? imported from (hafod srfi-13)

  ;; Extract the last whitespace-delimited argument from a command string.
  (define (last-argument str)
    (let ([len (string-length str)])
      (let skip-trailing ([i (- len 1)])
        (cond
          [(< i 0) str]
          [(char-whitespace? (string-ref str i))
           (skip-trailing (- i 1))]
          [else
           (let find-start ([j i])
             (cond
               [(< j 0) (substring str 0 (+ i 1))]
               [(char-whitespace? (string-ref str j))
                (substring str (+ j 1) (+ i 1))]
               [else (find-start (- j 1))]))]))))

) ; end library
