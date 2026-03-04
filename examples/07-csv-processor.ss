;;; 07-csv-processor.ss -- CSV processing with infix-splitter
;;;
;;; Demonstrates: infix-splitter, field-splitter, awk, read-line,
;;;               create-temp-file, open-output-file, open-input-file,
;;;               run/string*, exec-path, port->string-list
;;;
;;; Process CSV-like data using scsh's field splitting tools.
;;; Source: scsh manual field reader section.

(import (hafod))

(define (section title)
  (newline)
  (display (string-append "=== " title " ===\n")))

;;; --- Generate sample CSV data in a temp file ---
(define csv-data
  (string-append "Name,Age,City,Score\n"
                 "Alice,30,London,85\n"
                 "Bob,25,Manchester,92\n"
                 "Carol,35,Edinburgh,78\n"
                 "Dave,28,Cardiff,95\n"
                 "Eve,31,Belfast,88\n"
                 "Frank,22,Dublin,71\n"
                 "Grace,29,Bristol,90\n"
                 "Hank,33,Leeds,82\n"))

(define csv-file (create-temp-file))
(let ([port (open-output-file csv-file)])
  (display csv-data port)
  (close port))

;;; --- Parse CSV and display as table ---
(section "CSV Table Display")

(let ([splitter (infix-splitter ",")])
  (let ([port (open-input-file csv-file)])
    ;; Read header
    (let* ([header-line (read-line port)]
           [headers (splitter header-line)])
      ;; Display header
      (for-each
        (lambda (h)
          (let ([padded (if (< (string-length h) 15)
                            (string-append h (make-string (- 15 (string-length h)) #\space))
                            h)])
            (display padded)))
        headers)
      (newline)
      (display (make-string (* 15 (length headers)) #\-))
      (newline)
      ;; Display rows
      (let loop ()
        (let ([line (read-line port)])
          (when (not (eof-object? line))
            (let ([fields (splitter line)])
              (for-each
                (lambda (f)
                  (let ([padded (if (< (string-length f) 15)
                                    (string-append f (make-string (- 15 (string-length f)) #\space))
                                    f)])
                    (display padded)))
                fields)
              (newline))
            (loop)))))
    (close-input-port port)))

;;; --- Compute statistics using AWK ---
(section "CSV Statistics (AWK)")

(let ([splitter (infix-splitter ",")])
  (let ([port (open-input-file csv-file)])
    ;; Skip header
    (read-line port)
    ;; Process data rows
    (awk (read-line port) (line) counter
      ((total-score 0) (max-score 0) (youngest 999))
      (#t
       (let* ([fields (splitter line)]
              [age (string->number (list-ref fields 1))]
              [score (string->number (list-ref fields 3))])
         (values (+ total-score score)
                 (max max-score score)
                 (min youngest age))))
      (after
        (display (string-append "Records: " (number->string counter) "\n"))
        (display (string-append "Total score: " (number->string total-score) "\n"))
        (display (string-append "Highest score: " (number->string max-score) "\n"))
        (display (string-append "Youngest age: " (number->string youngest) "\n"))
        (when (> counter 0)
          (display (string-append "Average score: "
                                  (number->string (quotient total-score counter))
                                  "\n")))))))

;;; --- Filter and sort using pipeline ---
(section "Pipeline: Sort by Score")

;; Use unquote (,var) for dynamic filename in process form
(let ([sorted (run/string (sort "-t," -k4 -n -r ,csv-file))])
  (display "Sorted by score (descending):\n")
  (let ([splitter (infix-splitter ",")])
    (for-each
      (lambda (line)
        (when (> (string-length line) 0)
          (let ([fields (splitter line)])
            (when (>= (length fields) 4)
              ;; Skip header line
              (unless (string=? (list-ref fields 0) "Name")
                (display (string-append "  " (list-ref fields 0)
                                        ": score=" (list-ref fields 3)
                                        ", age=" (list-ref fields 1) "\n")))))))
      (let ([port (open-input-string sorted)])
        (port->string-list port)))))

;; Clean up
(delete-file csv-file)
(display "\nDone.\n")
