#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod field-reader)
        (hafod re)
        (hafod rdelim)
        (hafod compat)
        (hafod internal char-sets)
        (only (hafod posix)
              posix-fork posix-getpid posix-kill posix-waitpid
              posix-sleep posix-_exit SIGKILL)
        (chezscheme))

;; Fail-fast watchdog: fork a sibling that SIGKILLs this process after `seconds`,
;; so a join that degrades to a growing-accumulator O(n^2) build dies
;; deterministically instead of stalling the suite. Cancels the watchdog on the
;; happy path and returns the thunk's value.
;;
;; victim is captured in an OUTER let, fully bound before the inner (posix-fork):
;; Chez evaluates let inits right-to-left, so a single flat let would run the fork
;; before (posix-getpid) and the watchdog child would capture its own pid and
;; SIGKILL itself, never the test process. The nested lets fix the order.
(define (with-watchdog seconds thunk)
  (let ([victim (posix-getpid)])
    (let ([wpid (posix-fork)])
      (if (zero? wpid)
          (begin (posix-sleep seconds) (posix-kill victim SIGKILL) (posix-_exit 0))
          (let ([result (thunk)])
            (posix-kill wpid SIGKILL)
            (posix-waitpid wpid 0)
            result)))))

(test-begin "Field Readers")

;; ========== join-strings ==========

(test-equal "join-strings: basic join with delimiter"
  "a:b:c"
  (join-strings '("a" "b" "c") ":"))

(test-equal "join-strings: default space delimiter"
  "a b c"
  (join-strings '("a" "b" "c")))

(test-equal "join-strings: empty list"
  ""
  (join-strings '()))

(test-equal "join-strings: single element"
  "only"
  (join-strings '("only")))

(test-equal "join-strings: two elements"
  "hello world"
  (join-strings '("hello" "world")))

;; Single-pass build: exercise both arities and the delimiter grammar so a
;; port-accumulator rewrite keeps the exact n-1-delimiters-for-n-elements shape.

(test-equal "join-strings: single element ignores delimiter"
  "a"
  (join-strings '("a")))

(test-equal "join-strings: comma delimiter"
  "a,b,c"
  (join-strings '("a" "b" "c") ","))

(test-equal "join-strings: empty delimiter concatenates"
  "ab"
  (join-strings '("a" "b") ""))

;; Large-input throughput guard: a growing-accumulator join copies the whole
;; result on every element (O(n^2)) and blows past the watchdog at this size,
;; while the single-pass port build returns in milliseconds. The watchdog
;; SIGKILLs the suite if the join has not returned within the bound, so a
;; quadratic regression fails deterministically rather than hanging. The result
;; length is 300000 one-char elements plus 299999 one-char delimiters.
(test-equal "join-strings: large input joins in one linear pass"
  (+ (* 300000 1) (* 299999 1))
  (with-watchdog 8
    (lambda ()
      (string-length (join-strings (make-list 300000 "x") ",")))))

;; ========== field-splitter ==========

(test-equal "field-splitter: default whitespace splitting"
  '("hello" "world")
  ((field-splitter) "  hello   world  "))

(test-equal "field-splitter: single word"
  '("one")
  ((field-splitter) "one"))

(test-equal "field-splitter: empty string"
  '()
  ((field-splitter) ""))

(test-equal "field-splitter: leading/trailing whitespace"
  '("a" "b" "c")
  ((field-splitter) "  a  b  c  "))

(test-equal "field-splitter: tabs and spaces"
  '("x" "y" "z")
  ((field-splitter) "x\ty\t z"))

(test-equal "field-splitter: custom regex - digits"
  '("123" "456")
  ((field-splitter (rx (+ digit))) "abc123def456ghi"))

(test-equal "field-splitter: with num-fields (negative = at-least)"
  '("hello" "world" " extra")
  ((field-splitter (rx (+ (~ white))) -2) "hello world extra"))

(test-equal "field-splitter: with num-fields (positive = exact)"
  '("hello" "world")
  ((field-splitter (rx (+ (~ white))) 2) "hello world"))

;; ========== infix-splitter ==========

(test-equal "infix-splitter: default whitespace"
  '("hello" "world")
  ((infix-splitter) "hello   world"))

(test-equal "infix-splitter: colon delimiter"
  '("a" "b" "c")
  ((infix-splitter ":") "a:b:c"))

(test-equal "infix-splitter: empty string"
  '()
  ((infix-splitter ":") ""))

(test-equal "infix-splitter: single field (no delimiter)"
  '("abc")
  ((infix-splitter ":") "abc"))

(test-equal "infix-splitter: handle-delim split"
  '("a" ":" "b" ":" "c")
  ((infix-splitter ":" #f 'split) "a:b:c"))

(test-equal "infix-splitter: handle-delim concat"
  '("a:" "b:" "c")
  ((infix-splitter ":" #f 'concat) "a:b:c"))

(test-equal "infix-splitter: handle-delim trim (explicit)"
  '("a" "b" "c")
  ((infix-splitter ":" #f 'trim) "a:b:c"))

(test-equal "infix-splitter: PATH-style splitting"
  '("/usr/bin" "/usr/local/bin" "/home/user/bin")
  ((infix-splitter ":") "/usr/bin:/usr/local/bin:/home/user/bin"))

(test-equal "infix-splitter: with regex delimiter"
  '("a" "b" "c")
  ((infix-splitter (rx (+ ","))) "a,,b,c"))

;; ========== suffix-splitter ==========

(test-equal "suffix-splitter: newline-terminated fields"
  '("a" "b" "c")
  ((suffix-splitter "\n") "a\nb\nc\n"))

(test-equal "suffix-splitter: default whitespace suffix"
  '("hello" "world")
  ((suffix-splitter) "hello world "))

(test-equal "suffix-splitter: handle-delim split"
  '("a" "\n" "b" "\n")
  ((suffix-splitter "\n" #f 'split) "a\nb\n"))

(test-equal "suffix-splitter: handle-delim concat"
  '("a\n" "b\n")
  ((suffix-splitter "\n" #f 'concat) "a\nb\n"))

;; ========== sloppy-suffix-splitter ==========

(test-equal "sloppy-suffix-splitter: skips initial delimiter"
  '("a" "b")
  ((sloppy-suffix-splitter "\n") "\na\nb\n"))

(test-equal "sloppy-suffix-splitter: no initial delimiter"
  '("a" "b")
  ((sloppy-suffix-splitter "\n") "a\nb\n"))

(test-equal "sloppy-suffix-splitter: default whitespace"
  '("hello" "world")
  ((sloppy-suffix-splitter) " hello world "))

;; ========== record-reader ==========

(test-equal "record-reader: default reads lines"
  "line1"
  (let ((p (open-input-string "line1\nline2\nline3\n")))
    ((record-reader) p)))

(test-equal "record-reader: reads successive lines"
  '("line1" "line2" "line3")
  (let ((p (open-input-string "line1\nline2\nline3\n"))
        (rr (record-reader)))
    (let loop ((acc '()))
      (let ((r (rr p)))
        (if (eof-object? r)
            (reverse acc)
            (loop (cons r acc)))))))

(test-assert "record-reader: returns eof at end"
  (let ((p (open-input-string "")))
    (eof-object? ((record-reader) p))))

(test-equal "record-reader: with elide skips consecutive delimiters"
  '("a" "b")
  (let ((p (open-input-string "a\n\n\nb\n"))
        (rr (record-reader (char-set #\newline) #t)))
    (let loop ((acc '()))
      (let ((r (rr p)))
        (if (eof-object? r)
            (reverse acc)
            (loop (cons r acc)))))))

(test-equal "record-reader: handle-delim concat"
  "line1\n"
  (let ((p (open-input-string "line1\nline2\n")))
    ((record-reader (char-set #\newline) #f 'concat) p)))

(test-equal "record-reader: handle-delim split"
  '("line1" #\newline)
  (let ((p (open-input-string "line1\nline2\n")))
    (receive (rec delim)
             ((record-reader (char-set #\newline) #f 'split) p)
      (list rec delim))))

(test-equal "record-reader: custom colon delimiter"
  '("field1" "field2" "field3")
  (let ((p (open-input-string "field1:field2:field3:"))
        (rr (record-reader ":")))
    (let loop ((acc '()))
      (let ((r (rr p)))
        (if (eof-object? r)
            (reverse acc)
            (loop (cons r acc)))))))

;; ========== field-reader ==========

(test-equal "field-reader: reads and splits"
  '("hello" "world")
  (let ((p (open-input-string "hello world\n")))
    (receive (raw fields) ((field-reader) p)
      fields)))

(test-equal "field-reader: raw record preserved"
  "hello world"
  (let ((p (open-input-string "hello world\n")))
    (receive (raw fields) ((field-reader) p)
      raw)))

(test-assert "field-reader: eof returns eof and empty list"
  (let ((p (open-input-string "")))
    (receive (raw fields) ((field-reader) p)
      (and (eof-object? raw) (null? fields)))))

(test-equal "field-reader: multiple records"
  '(("a" "b") ("c" "d"))
  (let ((p (open-input-string "a b\nc d\n"))
        (fr (field-reader)))
    (let loop ((acc '()))
      (receive (raw fields) (fr p)
        (if (eof-object? raw)
            (reverse acc)
            (loop (cons fields acc)))))))

(test-equal "field-reader: custom parser and reader"
  '("x" "y" "z")
  (let ((p (open-input-string "x:y:z\n")))
    (receive (raw fields) ((field-reader (infix-splitter ":") read-line) p)
      fields)))

;; ========== Edge cases ==========

(test-equal "infix-splitter: empty fields between delimiters"
  '("a" "" "b")
  ((infix-splitter (rx ",")) "a,,b"))

(test-equal "field-splitter: whitespace-only string"
  '()
  ((field-splitter) "   "))

(test-equal "infix-splitter: multi-char delimiter"
  '("hello" "world")
  ((infix-splitter "::") "hello::world"))

(test-end)
