;; test-rdelim.ss -- Comprehensive tests for (hafod rdelim)
;; Run with: scheme --libdirs .:src --script test/test-rdelim.ss

(import (test runner) (hafod rdelim) (hafod internal char-sets) (chezscheme))

(test-begin "hafod rdelim")

;;; ========== read-line tests ==========

(test-equal "read-line default (trim)"
  "hello"
  (let ((p (open-string-input-port "hello\nworld\n")))
    (read-line p)))

(test-equal "read-line second line"
  "world"
  (let ((p (open-string-input-port "hello\nworld\n")))
    (read-line p)
    (read-line p)))

(test-assert "read-line at EOF"
  (let ((p (open-string-input-port "hello\nworld\n")))
    (read-line p)
    (read-line p)
    (eof-object? (read-line p))))

(test-equal "read-line no trailing newline"
  "hello"
  (let ((p (open-string-input-port "hello")))
    (read-line p)))

(test-assert "read-line empty string => eof"
  (let ((p (open-string-input-port "")))
    (eof-object? (read-line p))))

(test-equal "read-line empty line"
  ""
  (let ((p (open-string-input-port "\n")))
    (read-line p)))

;; Peek mode
(test-assert "read-line peek leaves newline"
  (let ((p (open-string-input-port "hello\nworld")))
    (let ((line (read-line p 'peek)))
      (and (equal? "hello" line)
           (char=? #\newline (lookahead-char p))))))

(test-equal "read-line peek then read again"
  "world"
  (let ((p (open-string-input-port "hello\nworld")))
    (read-line p 'peek)
    (get-char p) ; consume the newline
    (read-line p)))

;; Concat mode
(test-equal "read-line concat"
  "hello\n"
  (let ((p (open-string-input-port "hello\n")))
    (read-line p 'concat)))

(test-equal "read-line concat no trailing newline"
  "hello"
  (let ((p (open-string-input-port "hello")))
    (read-line p 'concat)))

;; Split mode
(test-assert "read-line split"
  (let ((p (open-string-input-port "hello\n")))
    (let-values (((line delim) (read-line p 'split)))
      (and (equal? "hello" line)
           (char=? #\newline delim)))))

(test-assert "read-line split at EOF"
  (let ((p (open-string-input-port "hello")))
    (let-values (((line delim) (read-line p 'split)))
      (and (equal? "hello" line)
           (eof-object? delim)))))

(test-assert "read-line split empty => eof"
  (let ((p (open-string-input-port "")))
    (let-values (((line delim) (read-line p 'split)))
      (and (eof-object? line)
           (eof-object? delim)))))

;;; ========== read-delimited tests ==========

(test-equal "read-delimited with string delimiter"
  "hello"
  (let ((p (open-string-input-port "hello:world")))
    (read-delimited ":" p)))

(test-equal "read-delimited with char-set delimiter"
  "abc"
  (let ((p (open-string-input-port "abc123")))
    (read-delimited (char-set #\1 #\2 #\3) p)))

(test-assert "read-delimited peek leaves delimiter"
  (let ((p (open-string-input-port "hello:world")))
    (let ((result (read-delimited ":" p 'peek)))
      (and (equal? "hello" result)
           (char=? #\: (lookahead-char p))))))

(test-equal "read-delimited concat appends delimiter"
  "hello:"
  (let ((p (open-string-input-port "hello:world")))
    (read-delimited ":" p 'concat)))

(test-assert "read-delimited split"
  (let ((p (open-string-input-port "hello:world")))
    (let-values (((str delim) (read-delimited ":" p 'split)))
      (and (equal? "hello" str)
           (char=? #\: delim)))))

(test-equal "read-delimited multiple delimiters"
  "hello"
  (let ((p (open-string-input-port "hello world")))
    (read-delimited " \t" p)))

;; Long string requiring buffer growth (> 80 chars default buffer)
(test-equal "read-delimited long string"
  (make-string 200 #\a)
  (let ((p (open-string-input-port (string-append (make-string 200 #\a) "\n"))))
    (read-delimited "\n" p)))

;;; ========== read-delimited! tests ==========

(test-equal "read-delimited! basic fill"
  5
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "hello\n")))
    (read-delimited! "\n" buf p)))

(test-assert "read-delimited! buffer contents"
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "hello\n")))
    (read-delimited! "\n" buf p)
    (equal? "hello....." buf)))

(test-assert "read-delimited! buffer overflow returns #f"
  (let ((buf (make-string 3 #\.))
        (p (open-string-input-port "hello\n")))
    (not (read-delimited! "\n" buf p))))

(test-equal "read-delimited! exact buffer then delimiter"
  5
  (let ((buf (make-string 5 #\.))
        (p (open-string-input-port "hello\n")))
    (read-delimited! "\n" buf p)))

(test-equal "read-delimited! concat"
  6
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "hello\n")))
    (read-delimited! "\n" buf p 'concat)))

(test-assert "read-delimited! concat buffer contents"
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "hello\n")))
    (read-delimited! "\n" buf p 'concat)
    (equal? (string-ref buf 5) #\newline)))

(test-equal "read-delimited! with start/end"
  3
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "abc\n")))
    (read-delimited! "\n" buf p 'trim 2 7)))

(test-assert "read-delimited! with start/end contents"
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "abc\n")))
    (read-delimited! "\n" buf p 'trim 2 7)
    (and (char=? #\a (string-ref buf 2))
         (char=? #\b (string-ref buf 3))
         (char=? #\c (string-ref buf 4)))))

(test-assert "read-delimited! split"
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "hello\n")))
    (let-values (((count delim) (read-delimited! "\n" buf p 'split)))
      (and (= 5 count)
           (char=? #\newline delim)))))

(test-assert "read-delimited! EOF"
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "")))
    (eof-object? (read-delimited! "\n" buf p))))

;;; ========== %read-delimited! tests ==========

(test-assert "%read-delimited! returns terminator and count"
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "hello\n")))
    (let-values (((terminator num-read) (%read-delimited! "\n" buf #t p)))
      (and (char=? #\newline terminator)
           (= 5 num-read)))))

(test-assert "%read-delimited! EOF"
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "hello")))
    (let-values (((terminator num-read) (%read-delimited! "\n" buf #t p)))
      (and (eof-object? terminator)
           (= 5 num-read)))))

(test-assert "%read-delimited! buffer full returns #f"
  (let ((buf (make-string 3 #\.))
        (p (open-string-input-port "hello")))
    (let-values (((terminator num-read) (%read-delimited! "\n" buf #t p)))
      (and (not terminator)
           (= 3 num-read)))))

(test-assert "%read-delimited! gobble consumes delimiter"
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "hello\nworld")))
    (let-values (((terminator num-read) (%read-delimited! "\n" buf #t p)))
      (and (char=? #\newline terminator)
           (= 5 num-read)
           ;; Next char should be 'w' (newline consumed)
           (char=? #\w (lookahead-char p))))))

(test-assert "%read-delimited! no-gobble leaves delimiter"
  (let ((buf (make-string 10 #\.))
        (p (open-string-input-port "hello\nworld")))
    (let-values (((terminator num-read) (%read-delimited! "\n" buf #f p)))
      (and (char=? #\newline terminator)
           (= 5 num-read)
           ;; Next char should be newline (not consumed)
           (char=? #\newline (lookahead-char p))))))

;;; ========== read-paragraph tests ==========

(test-equal "read-paragraph simple"
  "line1\nline2\n"
  (let ((p (open-string-input-port "line1\nline2\n\nline3\n")))
    (read-paragraph p)))

(test-equal "read-paragraph skips leading blank lines"
  "line1\nline2\n"
  (let ((p (open-string-input-port "\n\n\nline1\nline2\n\n")))
    (read-paragraph p)))

(test-equal "read-paragraph EOF without blank line"
  "line1\nline2\n"
  (let ((p (open-string-input-port "line1\nline2\n")))
    (read-paragraph p)))

;; Note: line without trailing newline gets concat treatment from read-line
(test-equal "read-paragraph no trailing newline"
  "line1\nline2"
  (let ((p (open-string-input-port "line1\nline2")))
    (read-paragraph p)))

(test-assert "read-paragraph empty input => eof"
  (let ((p (open-string-input-port "")))
    (eof-object? (read-paragraph p))))

(test-assert "read-paragraph all blank lines => eof"
  (let ((p (open-string-input-port "\n\n\n")))
    (eof-object? (read-paragraph p))))

(test-assert "read-paragraph split"
  (let ((p (open-string-input-port "line1\nline2\n\nline3\n")))
    (let-values (((para delim) (read-paragraph p 'split)))
      (and (equal? "line1\nline2\n" para)
           (equal? "\n" delim)))))

(test-equal "read-paragraph concat"
  "line1\nline2\n\n"
  (let ((p (open-string-input-port "line1\nline2\n\nline3\n")))
    (read-paragraph p 'concat)))

;;; ========== skip-char-set tests ==========

(test-equal "skip-char-set spaces"
  3
  (let ((p (open-string-input-port "   hello")))
    (skip-char-set " " p)))

(test-assert "skip-char-set port position after skip"
  (let ((p (open-string-input-port "   hello")))
    (skip-char-set " " p)
    (equal? "hello" (read-line p))))

(test-equal "skip-char-set nothing to skip"
  0
  (let ((p (open-string-input-port "hello")))
    (skip-char-set " " p)))

(test-equal "skip-char-set all"
  3
  (let ((p (open-string-input-port "   ")))
    (skip-char-set " " p)))

(test-equal "skip-char-set empty"
  0
  (let ((p (open-string-input-port "")))
    (skip-char-set " " p)))

(test-equal "skip-char-set with char-set:whitespace"
  4
  (let ((p (open-string-input-port "\t  \nhello")))
    (skip-char-set char-set:whitespace p)))

(test-end)
