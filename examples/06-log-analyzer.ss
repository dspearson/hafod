;;; 06-log-analyzer.ss -- Parse structured data with field splitters
;;;
;;; Demonstrates: infix-splitter, field-splitter, record-reader,
;;;               field-reader, rx, regexp-search, read-line,
;;;               read-paragraph, port->string-list, open-input-string
;;;
;;; Process structured log-like data using scsh's field reader system.
;;; Source: scsh manual sections on field readers and record readers.

(import (hafod))

(define (section title)
  (newline)
  (display (string-append "=== " title " ===\n")))

;;; --- Parse /etc/passwd with infix-splitter ---
(section "Parsing /etc/passwd")

(let* ([splitter (infix-splitter ":")]
       [port (open-input-file "/etc/passwd")]
       [users '()])
  (let loop ()
    (let ([line (read-line port)])
      (when (not (eof-object? line))
        (let ([fields (splitter line)])
          (when (>= (length fields) 7)
            (let ([name (list-ref fields 0)]
                  [uid  (list-ref fields 2)]
                  [home (list-ref fields 5)]
                  [shell (list-ref fields 6)])
              ;; Collect real users (UID >= 1000 or root)
              (let ([uid-num (string->number uid)])
                (when (and uid-num (or (= uid-num 0) (>= uid-num 1000)))
                  (set! users (cons (list name uid home shell) users)))))))
        (loop))))
  (close-input-port port)
  (display "Real user accounts:\n")
  (for-each
    (lambda (u)
      (display (string-append "  " (car u)
                              " (uid=" (cadr u)
                              ", home=" (caddr u)
                              ", shell=" (cadddr u) ")\n")))
    (reverse users)))

;;; --- Parse /etc/group with infix-splitter ---
(section "Groups with Members")

(let* ([splitter (infix-splitter ":")]
       [port (open-input-file "/etc/group")])
  (let loop ([count 0])
    (let ([line (read-line port)])
      (cond
        [(eof-object? line)
         (display (string-append "Total groups with members: "
                                 (number->string count) "\n"))]
        [else
         (let* ([fields (splitter line)]
                [name (if (pair? fields) (car fields) "")]
                [members (if (>= (length fields) 4)
                             (list-ref fields 3)
                             "")])
           (if (> (string-length members) 0)
               (begin
                 (display (string-append "  " name ": " members "\n"))
                 (loop (+ count 1)))
               (loop count)))])))
  (close-input-port port))

;;; --- Parse structured text using field-splitter ---
(section "Field Splitter on Structured Text")

;; field-splitter splits on whitespace by default
(let* ([sample (string-append "DEVICE     SIZE  USED AVAIL USE% MOUNT\n"
                              "/dev/sda1  100G  80G  20G   80%  /\n"
                              "tmpfs      8G    1G   7G    12%  /tmp\n"
                              "/dev/sdb1  500G  300G 200G  60%  /data\n")]
       [lines (port->string-list (open-input-string sample))]
       [splitter (field-splitter)])
  (display "Filesystem summary (real devices):\n")
  (for-each
    (lambda (line)
      (let ([fields (splitter line)])
        (when (and (pair? fields)
                   (regexp-search (rx bos "/dev/") (car fields)))
          (display (string-append "  " (car fields)))
          (when (>= (length fields) 6)
            (display (string-append "  size=" (list-ref fields 1)
                                    "  used=" (list-ref fields 3)
                                    "  mount=" (list-ref fields 5))))
          (newline))))
    (cdr lines)))

;;; --- Record reader for multi-line records ---
(section "Record Reader (paragraph mode)")

;; Demonstrate reading paragraph-separated records
(let* ([sample (string-append
                 "Name: Alice Smith\nAge: 30\nRole: Engineer\n\n"
                 "Name: Bob Jones\nAge: 25\nRole: Designer\n\n"
                 "Name: Carol White\nAge: 35\nRole: Manager\n")]
       [port (open-input-string sample)])
  (display "People records:\n")
  (let loop ([n 1])
    (let ([para (read-paragraph port)])
      (when (not (eof-object? para))
        (display (string-append "  Record " (number->string n) ": "))
        ;; Extract name from paragraph
        (let ([m (regexp-search (rx "Name: " (submatch (+ any))) para)])
          (if m
              (display (match:substring m 1))
              (display "???")))
        (display ", ")
        ;; Extract role
        (let ([m (regexp-search (rx "Role: " (submatch (+ any))) para)])
          (if m
              (display (match:substring m 1))
              (display "???")))
        (newline)
        (loop (+ n 1))))))

;;; --- Join strings ---
(section "Join Strings")

(display (string-append "Joined: "
                        (join-strings '("alpha" "beta" "gamma") ":")
                        "\n"))
(display (string-append "Space-joined: "
                        (join-strings '("one" "two" "three"))
                        "\n"))

(display "\nDone.\n")
