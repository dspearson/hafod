;;; 08-regex-demo.ss -- SRE regex matching, substitution, folding
;;;
;;; Demonstrates: rx, regexp-search, regexp-match, match:substring,
;;;               match:start, match:end, regexp-substitute,
;;;               regexp-substitute/global, regexp-fold,
;;;               regexp-for-each, let-match, if-match, match-cond,
;;;               posix-string->regexp, sre->regexp
;;;
;;; Source: scsh manual regex section; SRFI-115 SRE specification.

(import (hafod))

(define (section title)
  (newline)
  (display (string-append "=== " title " ===\n")))

;;; --- Basic matching ---
(section "Basic SRE Matching")

;; Match a date pattern
(let ([m (regexp-search (rx (submatch (+ digit)) "-"
                            (submatch (+ digit)) "-"
                            (submatch (+ digit)))
                        "Today is 2026-03-03, a Monday.")])
  (if m
      (begin
        (display (string-append "Full match: " (match:substring m 0) "\n"))
        (display (string-append "Year: "  (match:substring m 1) "\n"))
        (display (string-append "Month: " (match:substring m 2) "\n"))
        (display (string-append "Day: "   (match:substring m 3) "\n"))
        (display (string-append "Match at position: "
                                (number->string (match:start m 0)) "-"
                                (number->string (match:end m 0)) "\n")))
      (display "No match\n")))

;; Case-insensitive matching
(let ([m (regexp-search (rx (w/nocase "hello")) "Say HELLO World")])
  (display (string-append "Case-insensitive: " (match:substring m 0) "\n")))

;; Character classes
(let ([m (regexp-search (rx (+ (or alpha digit))) "***abc123!!!")])
  (display (string-append "Alphanumeric run: " (match:substring m 0) "\n")))

;;; --- Substitution ---
(section "Regex Substitution")

;; Simple global substitution
(let ([result (regexp-substitute/global #f
                (rx "colour")
                "The colour of the colour wheel"
                'pre "color" 'post)])
  (display (string-append "British->American: " result "\n")))

;; Substitution with backreferences
(let ([result (regexp-substitute/global #f
                (rx (submatch (+ alpha)) "=" (submatch (+ (~ white))))
                "name=Alice age=30 city=London"
                'pre 2 "(" 1 ")" 'post)])
  (display (string-append "Swapped: " result "\n")))

;; Extract and reformat dates
(let ([result (regexp-substitute/global #f
                (rx (submatch digit digit) "/"
                    (submatch digit digit) "/"
                    (submatch digit digit digit digit))
                "Dates: 03/03/2026 and 25/12/2025"
                'pre 3 "-" 2 "-" 1 'post)])
  (display (string-append "Reformatted: " result "\n")))

;;; --- Folding over matches ---
(section "Regex Fold")

;; Count words using regexp-fold
(let ([word-count (regexp-fold (rx (+ alpha))
                    (lambda (i m acc) (+ acc 1))
                    0
                    "The quick brown fox jumps over the lazy dog")])
  (display (string-append "Word count: " (number->string word-count) "\n")))

;; Collect all numbers from text
(let ([numbers (regexp-fold (rx (+ digit))
                 (lambda (i m acc)
                   (cons (string->number (match:substring m 0)) acc))
                 '()
                 "There are 42 apples, 17 oranges, and 99 bananas")])
  (display "Numbers found: ")
  (for-each (lambda (n) (display (string-append (number->string n) " ")))
            (reverse numbers))
  (newline)
  (display (string-append "Sum: " (number->string (apply + numbers)) "\n")))

;;; --- regexp-for-each ---
(section "Regex Iteration")

;; List all email-like patterns
(display "Email-like patterns found:\n")
(regexp-for-each (rx (submatch (+ (or alpha digit "." "_")))
                     "@"
                     (submatch (+ (or alpha digit "."))))
  (lambda (m)
    (display (string-append "  " (match:substring m 0)
                            " (user=" (match:substring m 1)
                            ", domain=" (match:substring m 2) ")\n")))
  "Contact alice@example.com or bob.jones@mail.co.uk for info")

;;; --- if-match ---
(section "if-match")

;; if-match: bind submatches on success, run alternative on failure
(if-match (regexp-search (rx "v" (submatch (+ digit)) "." (submatch (+ digit))
                              (? "." (submatch (+ digit))))
                          "Running v3.14.159")
  (major minor patch)
  (begin
    (display (string-append "Major: " major ", Minor: " minor "\n"))
    (when patch
      (display (string-append "Patch: " patch "\n"))))
  (display "No version found\n"))

;; if-match with no submatches
(if-match (regexp-search (rx "hello") "say hello world")
  ()
  (display "Found 'hello' in string\n")
  (display "Not found\n"))

;; if-match failure case
(if-match (regexp-search (rx "xyz") "no match here")
  ()
  (display "Should not reach here\n")
  (display "Correctly fell through to alternative\n"))

;;; --- match-cond ---
(section "match-cond")

;; match-cond dispatches on multiple patterns
(define (classify-line line)
  (match-cond
    ((regexp-search (rx bos "#" (submatch (* any))) line) (comment)
     (string-append "comment:" comment))
    ((regexp-search (rx (submatch (+ digit))) line) (num)
     (string-append "has-number: " num " in '" line "'"))
    ((regexp-search (rx bos (+ upper)) line) ()
     (string-append "capitalised: " line))
    (else
     (string-append "other: " line))))

;;; --- Pattern dispatch ---
(section "Pattern Dispatch")

(for-each
  (lambda (line)
    (display (string-append "  " (classify-line line) "\n")))
  '("# This is a comment"
    "  indented line"
    "ALLCAPS heading"
    "contains 42 numbers"
    "just a plain line"))

;;; --- POSIX string regex ---
(section "POSIX String Regex")

(let* ([re (posix-string->regexp "^[A-Z][a-z]+$")]
       [tests '("Hello" "hello" "HELLO" "World" "x")])
  (for-each
    (lambda (s)
      (display (string-append "  \"" s "\" matches ^[A-Z][a-z]+$ ? "
                              (if (regexp-search? re s) "yes" "no") "\n")))
    tests))

(display "\nDone.\n")
