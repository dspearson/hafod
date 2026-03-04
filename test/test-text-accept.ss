#!chezscheme
;;; test-text-accept.ss -- Acceptance tests: AWK, field splitting, regex pipelines, record-reader
;;; Requirements: TEXT-01, TEXT-02, TEXT-03, TEXT-04
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (hafod awk) (hafod field-reader) (hafod re) (hafod rdelim)
        (hafod syntax) (hafod process) (hafod procobj) (hafod posix)
        (hafod fd-ports) (hafod compat) (hafod internal char-sets)
        (except (chezscheme) exit vector-append open-input-file open-output-file
                getenv record-reader)
        (test runner))

(test-begin "Text Processing Acceptance")

;; =============================================================================
;; Section 1: TEXT-01 -- AWK macro with structured multi-line input
;; =============================================================================

;; 1. Filter lines matching pattern
(test-equal "awk: filter lines matching pattern"
  '("alice,30,london" "carol,35,london")
  (let ((p (open-input-string "name,age,city\nalice,30,london\nbob,25,paris\ncarol,35,london\ndave,28,tokyo\n"))
        (result '()))
    (awk (read-line p) (line) ()
      ((when (regexp-search (rx "london") line))
       (set! result (cons line result))))
    (reverse result)))

;; 2. Count lines with state variable
(test-equal "awk: count lines with state variable"
  5
  (let ((p (open-input-string "one\ntwo\nthree\nfour\nfive\n")))
    (awk (read-line p) (line) ((count 0))
      (#t (+ count 1)))))

;; 3. Sum numeric field from structured data
(test-equal "awk: sum numeric field from structured data"
  60
  (let ((p (open-input-string "item1 10\nitem2 20\nitem3 30\n"))
        (splitter (field-splitter)))
    (awk (read-line p) (line) ((total 0))
      (#t (let ((fields (splitter line)))
            (+ total (string->number (cadr fields))))))))

;; 4. Range clause activates on start/stop patterns
;; :range includes the start line but excludes the stop line
(test-equal "awk: range clause activates on start/stop patterns"
  '("BEGIN" "data1" "data2")
  (let ((p (open-input-string "header\nBEGIN\ndata1\ndata2\nEND\nfooter\n")))
    (awk (read-line p) (line) ((collected '()))
      (:range (when (string=? line "BEGIN"))
              (when (string=? line "END"))
              (cons line collected))
      (after (reverse collected)))))

;; 5. Else clause fires on non-matching lines
(test-equal "awk: else clause fires on non-matching lines"
  '("no" "no")
  (let ((p (open-input-string "yes\nno\nyes\nno\nyes\n"))
        (nons '()))
    (awk (read-line p) (line) ()
      ((when (string=? line "yes")) (void))
      (else (set! nons (cons line nons))))
    (reverse nons)))

;; 6. Multiple clauses on same input
(test-equal "awk: multiple clauses on same input"
  '(2 1)
  (let ((p (open-input-string "ERROR: disk full\nINFO: ok\nWARN: slow\nERROR: timeout\n")))
    (receive (errors warnings)
      (awk (read-line p) (line) ((errs 0) (warns 0))
        ((when (regexp-search (rx "ERROR") line)) (values (+ errs 1) warns))
        ((when (regexp-search (rx "WARN") line)) (values errs (+ warns 1))))
      (list errors warnings))))

;; =============================================================================
;; Section 2: TEXT-02 -- Field splitting with various delimiter patterns
;; =============================================================================

;; 1. Whitespace on realistic data
(test-equal "field-splitter: whitespace on realistic data"
  '("John" "Doe" "42" "Engineer")
  ((field-splitter) "  John   Doe   42   Engineer  "))

;; 2. Colon delimiter (passwd-style)
(test-equal "field-splitter: colon delimiter (passwd-style)"
  '("root" "x" "0" "0" "root" "/root" "/bin/bash")
  ((infix-splitter ":") "root:x:0:0:root:/root:/bin/bash"))

;; 3. Comma delimiter (CSV)
(test-equal "field-splitter: comma delimiter (CSV)"
  '("alice" "30" "london" "engineer")
  ((infix-splitter ",") "alice,30,london,engineer"))

;; 4. Regex delimiter (one or more colons)
(test-equal "field-splitter: regex delimiter (one or more colons)"
  '("field1" "field2" "field3")
  ((infix-splitter (rx (+ ":"))) "field1::field2:::field3"))

;; 5. Tab delimiter
(test-equal "field-splitter: tab delimiter"
  '("col1" "col2" "col3")
  ((infix-splitter "\t") "col1\tcol2\tcol3"))

;; 6. Extract specific fields from /etc/passwd-style line
(test-equal "field-splitter: extract specific fields from passwd-style"
  '("nobody" "/nonexistent")
  (let ((fields ((infix-splitter ":") "nobody:x:65534:65534:nobody:/nonexistent:/usr/sbin/nologin")))
    (list (list-ref fields 0) (list-ref fields 5))))

;; 7. Join-strings round-trip
(test-equal "field-splitter: join-strings round-trip"
  "a,b,c,d"
  (join-strings ((infix-splitter ",") "a,b,c,d") ","))

;; 8. Empty fields with infix-splitter
(test-equal "field-splitter: empty fields with infix-splitter"
  '("a" "" "b" "")
  ((infix-splitter (rx ",")) "a,,b,"))

;; =============================================================================
;; Section 3: TEXT-03 -- Regex match/substitute in pipeline context
;; =============================================================================

;; 1. Search and extract from command output
(test-assert "regex: search and extract from command output"
  (let* ((output (run/string (echo "version: 3.14.2")))
         (m (regexp-search (rx "version: " (submatch (+ (~ space)))) output)))
    (and m (string=? (match:substring m 1) "3.14.2"))))

;; 2. Global substitution on multi-line text
(test-equal "regex: global substitution on multi-line text"
  "qux bar qux baz qux"
  (regexp-substitute/global #f (rx "foo") "foo bar foo baz foo"
    'pre "qux" 'post))

;; 3. Substitute in pipeline output
(test-assert "regex: substitute in pipeline output"
  (let* ((output (run/string (echo "Hello World")))
         (result (regexp-substitute/global #f (rx "World") output
                   'pre "Universe" 'post)))
    (regexp-search? (rx "Universe") result)))

;; 4. Fold to count matches
(test-equal "regex: fold to count matches"
  3
  (regexp-fold (rx (+ digit))
    (lambda (i m acc) (+ acc 1))
    0
    "abc 123 def 456 ghi 789"))

;; 5. Fold to extract all matches
(test-equal "regex: fold to extract all matches"
  '("abc" "def" "ghi")
  (reverse
    (regexp-fold (rx (+ alpha))
      (lambda (i m acc) (cons (match:substring m 0) acc))
      '()
      "123 abc 456 def 789 ghi")))

;; 6. regexp-substitute with submatch reordering
(test-equal "regex: regexp-substitute with submatch reordering"
  "value -> key"
  (let ((m (regexp-search (rx (submatch (+ alpha)) "=" (submatch (+ any))) "key=value")))
    (regexp-substitute #f m 2 " -> " 1)))

;; 7. if-match extracts structured data
(test-equal "regex: if-match extracts structured data"
  '("2026" "03" "03")
  (if-match (regexp-search (rx (submatch (+ digit)) "-"
                               (submatch (+ digit)) "-"
                               (submatch (+ digit)))
                           "2026-03-03")
    (#f year month day) (list year month day)
    #f))

;; =============================================================================
;; Section 4: TEXT-04 -- Record-reader with configurable delimiters
;; =============================================================================

;; 1. Default newline delimiter
(test-equal "record-reader: default newline delimiter"
  '("line1" "line2" "line3")
  (let ((p (open-input-string "line1\nline2\nline3\n"))
        (rr (record-reader)))
    (let loop ((acc '()))
      (let ((r (rr p)))
        (if (eof-object? r)
            (reverse acc)
            (loop (cons r acc)))))))

;; 2. Paragraph delimiter (double newline)
(test-assert "record-reader: paragraph delimiter (double newline)"
  (let ((p (open-input-string "para1 line1\npara1 line2\n\npara2 line1\n\npara3\n")))
    (let ((para1 (read-paragraph p)))
      (and (string? para1)
           (regexp-search? (rx "para1 line1") para1)
           (regexp-search? (rx "para1 line2") para1)))))

;; 3. Custom delimiter (semicolons)
(test-equal "record-reader: custom delimiter (semicolons)"
  '("record1" "record2" "record3")
  (let ((p (open-input-string "record1;record2;record3"))
        (rr (record-reader (char-set #\;))))
    (let loop ((acc '()))
      (let ((r (rr p)))
        (if (eof-object? r)
            (reverse acc)
            (loop (cons r acc)))))))

;; 4. Colon-delimited with elide
(test-equal "record-reader: colon-delimited with elide"
  '("a" "b" "c")
  (let ((p (open-input-string "a:::b:::c"))
        (rr (record-reader (char-set #\:) #t)))
    (let loop ((acc '()))
      (let ((r (rr p)))
        (if (eof-object? r)
            (reverse acc)
            (loop (cons r acc)))))))

;; 5. Concat mode includes delimiter
(test-equal "record-reader: concat mode includes delimiter"
  "rec1\n"
  (let ((p (open-input-string "rec1\nrec2\nrec3\n")))
    ((record-reader (char-set #\newline) #f 'concat) p)))

;; 6. Split mode returns delimiter separately
(test-equal "record-reader: split mode returns delimiter separately"
  '("rec1" #\;)
  (let ((p (open-input-string "rec1;rec2;rec3")))
    (receive (rec delim)
      ((record-reader (char-set #\;) #f 'split) p)
      (list rec delim))))

;; 7. field-reader combines record-reader + field-splitter
(test-equal "record-reader: field-reader combines record-reader + field-splitter"
  '(("alice" "30") ("bob" "25") ("carol" "35"))
  (let ((p (open-input-string "alice 30\nbob 25\ncarol 35\n"))
        (fr (field-reader)))
    (let loop ((acc '()))
      (receive (raw fields) (fr p)
        (if (eof-object? raw)
            (reverse acc)
            (loop (cons fields acc)))))))

(test-end)
