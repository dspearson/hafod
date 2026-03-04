;;; 05-word-frequency.ss -- Word frequency count using AWK macro
;;;
;;; Demonstrates: awk, field-splitter, infix-splitter, rx, regexp-search,
;;;               open-input-string, open-input-file, read-line
;;;
;;; Count word frequencies in text input -- the classic text-processing
;;; task that scsh's AWK macro handles elegantly.
;;; Source: scsh manual AWK section; inspired by "How to Use Scsh" article.

(import (hafod))

(define (section title)
  (newline)
  (display (string-append "=== " title " ===\n")))

;;; --- Word frequency in a here-string ---
(section "Word Frequency Analysis")

(define sample-text
  (string-append "the quick brown fox jumps over the lazy dog\n"
                 "the fox was quick and the dog was lazy\n"
                 "brown fox brown dog quick quick quick"))

;; Split words and count using a hash table
(let ([counts (make-hashtable string-hash string=?)])
  (let ([port (open-input-string sample-text)])
    (awk (read-line port) (line) ()
      (#t
       (let* ([splitter (field-splitter)]
              [words (splitter line)])
         (for-each
           (lambda (w)
             (let ([lw (string-downcase w)])
               (hashtable-set! counts lw
                 (+ 1 (hashtable-ref counts lw 0)))))
           words)))))
  ;; Sort by frequency (descending)
  (let* ([pairs (let-values ([(keys vals) (hashtable-entries counts)])
                  (map cons (vector->list keys) (vector->list vals)))]
         [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) pairs)])
    (display "Word frequencies (descending):\n")
    (for-each
      (lambda (p)
        (display (string-append "  " (car p) ": "
                                (number->string (cdr p)) "\n")))
      sorted)))

;;; --- AWK with pattern matching ---
(section "AWK Pattern Matching on /etc/passwd")

;; Count shells used, using AWK with regex patterns
(let ([shell-counts (make-hashtable string-hash string=?)]
      [total 0])
  (let ([port (open-input-file "/etc/passwd")])
    (awk (read-line port) (line) ()
      ;; Match comment lines and skip them (body does nothing)
      ((rx bos "#") (values))
      ;; Process all other lines
      (#t
       (let* ([fields ((infix-splitter ":") line)]
              [nfields (length fields)])
         (when (>= nfields 7)
           (let ([shell (list-ref fields 6)])
             (set! total (+ total 1))
             (hashtable-set! shell-counts shell
               (+ 1 (hashtable-ref shell-counts shell 0)))))))))
  (let* ([pairs (let-values ([(keys vals) (hashtable-entries shell-counts)])
                  (map cons (vector->list keys) (vector->list vals)))]
         [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) pairs)])
    (display (string-append "Shell usage across "
                            (number->string total) " accounts:\n"))
    (for-each
      (lambda (p)
        (display (string-append "  " (car p) ": "
                                (number->string (cdr p)) "\n")))
      sorted)))

;;; --- AWK with state variables and counter ---
(section "AWK with State: Line Statistics")

;; Compute line statistics on /etc/group
(let ([port (open-input-file "/etc/group")])
  (awk (read-line port) (line) counter
    ((total-chars 0) (max-len 0) (min-len 999999))
    (#t
     (let ([len (string-length line)])
       (values (+ total-chars len)
               (max max-len len)
               (min min-len len))))
    (after
      (display (string-append "Lines: " (number->string counter) "\n"))
      (display (string-append "Total chars: " (number->string total-chars) "\n"))
      (display (string-append "Longest line: " (number->string max-len) "\n"))
      (display (string-append "Shortest line: " (number->string min-len) "\n"))
      (when (> counter 0)
        (display (string-append "Average: "
                                (number->string (quotient total-chars counter))
                                " chars/line\n"))))))

(display "\nDone.\n")
