#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod re)
        (hafod posix)
        (hafod internal char-sets)
        (chezscheme))

(test-begin "SRE Regex Engine")

;; ========== POSIX FFI basics ==========

(test-assert "posix-regcomp compiles a simple pattern"
  (let ((rt (posix-regcomp "hello" REG_EXTENDED)))
    (and (bytevector? rt)
         (begin (posix-regfree rt) #t))))

(test-assert "posix-regexec finds match"
  (let* ((rt (posix-regcomp "hello" REG_EXTENDED))
         (result (posix-regexec rt "say hello world" 1 0)))
    (posix-regfree rt)
    (and (vector? result)
         (equal? (vector-ref result 0) '(4 . 9)))))

(test-assert "posix-regexec returns #f for no match"
  (let* ((rt (posix-regcomp "xyz" REG_EXTENDED))
         (result (posix-regexec rt "hello world" 1 0)))
    (posix-regfree rt)
    (eq? result #f)))

(test-assert "posix-regcomp with REG_ICASE"
  (let* ((rt (posix-regcomp "hello" (bitwise-ior REG_EXTENDED REG_ICASE)))
         (result (posix-regexec rt "HELLO" 1 0)))
    (posix-regfree rt)
    (and (vector? result)
         (equal? (vector-ref result 0) '(0 . 5)))))

;; ========== Char-set algebra ==========

(test-assert "char-set-complement works"
  (and (not (char-set-contains? (char-set-complement char-set:digit) #\5))
       (char-set-contains? (char-set-complement char-set:digit) #\a)))

(test-assert "char-set-union works"
  (let ((cs (char-set-union char-set:digit char-set:letter)))
    (and (char-set-contains? cs #\5)
         (char-set-contains? cs #\a)
         (not (char-set-contains? cs #\!)))))

(test-assert "char-set-intersection works"
  (let ((cs (char-set-intersection char-set:ascii char-set:letter)))
    (and (char-set-contains? cs #\a)
         (not (char-set-contains? cs #\5)))))

(test-assert "char-set-difference works"
  (let ((cs (char-set-difference char-set:ascii char-set:digit)))
    (and (char-set-contains? cs #\a)
         (not (char-set-contains? cs #\5)))))

;; ========== rx macro: string literals ==========

(test-assert "rx string literal matches"
  (regexp-search? (rx "hello") "say hello world"))

(test-assert "rx string literal does not match absent text"
  (not (regexp-search? (rx "xyz") "say hello world")))

(test-assert "rx string escapes special chars"
  (regexp-search? (rx "a.b") "a.b"))

(test-assert "rx string special chars don't match as regex"
  (not (regexp-search? (rx "a.b") "aXb")))

;; ========== rx macro: any ==========

(test-assert "rx any matches any char"
  (regexp-search? (rx any) "x"))

(test-assert "rx any matches in sequence"
  (let ((m (regexp-search (rx (seq "a" any "c")) "abc")))
    (and m (equal? (match:substring m) "abc"))))

;; ========== rx macro: sequence ==========

(test-assert "rx seq concatenates"
  (let ((m (regexp-search (rx (seq "foo" "bar")) "foobar")))
    (and m (equal? (match:substring m) "foobar"))))

(test-assert "rx seq multiple parts"
  (regexp-search? (rx (seq "a" "b" "c")) "abc"))

;; ========== rx macro: alternation ==========

(test-assert "rx or matches first alternative"
  (regexp-search? (rx (or "cat" "dog")) "the cat"))

(test-assert "rx or matches second alternative"
  (regexp-search? (rx (or "cat" "dog")) "the dog"))

(test-assert "rx or does not match absent alternatives"
  (not (regexp-search? (rx (or "cat" "dog")) "the bird")))

;; ========== rx macro: quantifiers ==========

(test-assert "rx * matches zero occurrences"
  (regexp-search? (rx (seq "a" (* "b") "c")) "ac"))

(test-assert "rx * matches multiple occurrences"
  (regexp-search? (rx (seq "a" (* "b") "c")) "abbbbc"))

(test-assert "rx + matches one or more"
  (regexp-search? (rx (seq "a" (+ "b") "c")) "abc"))

(test-assert "rx + does not match zero"
  (not (regexp-search? (rx (seq "a" (+ "b") "c")) "ac")))

(test-assert "rx ? matches zero"
  (regexp-search? (rx (seq "a" (? "b") "c")) "ac"))

(test-assert "rx ? matches one"
  (regexp-search? (rx (seq "a" (? "b") "c")) "abc"))

;; ========== rx macro: bounded repetition ==========

(test-assert "rx ** bounded"
  (let ((m (regexp-search (rx (** 2 4 "a")) "aaaa")))
    (and m (equal? (match:substring m) "aaaa"))))

(test-assert "rx = exact count"
  (let ((m (regexp-search (rx (seq bos (= 3 "a") eos)) "aaa")))
    (and m (equal? (match:substring m) "aaa"))))

(test-assert "rx = exact count does not match wrong count"
  (not (regexp-search? (rx (seq bos (= 3 "a") eos)) "aa")))

(test-assert "rx >= at least n"
  (regexp-search? (rx (seq bos (>= 2 "a"))) "aaaa"))

(test-assert "rx >= at least n fails"
  (not (regexp-search? (rx (seq bos (>= 3 "a") eos)) "aa")))

;; ========== rx macro: anchors ==========

(test-assert "rx bos matches start"
  (regexp-search? (rx (seq bos "hello")) "hello world"))

(test-assert "rx bos does not match mid-string"
  (not (regexp-search? (rx (seq bos "world")) "hello world")))

(test-assert "rx eos matches end"
  (regexp-search? (rx (seq "world" eos)) "hello world"))

(test-assert "rx eos does not match mid-string"
  (not (regexp-search? (rx (seq "hello" eos)) "hello world")))

;; ========== rx macro: submatch ==========

(test-assert "rx submatch captures"
  (let ((m (regexp-search (rx (seq (submatch (+ alpha)) " " (submatch (+ digit))))
                          "abc 123 def")))
    (and m
         (equal? (match:substring m 1) "abc")
         (equal? (match:substring m 2) "123"))))

(test-assert "match:start and match:end correct"
  (let ((m (regexp-search (rx (submatch "hello")) "say hello there")))
    (and m
         (= (match:start m 1) 4)
         (= (match:end m 1) 9))))

(test-assert "match:count includes all groups"
  (let ((m (regexp-search (rx (seq (submatch "a") (submatch "b"))) "ab")))
    (and m (= (match:count m) 3)))) ; group 0 (whole) + 2 submatches

;; ========== rx macro: character sets ==========

(test-assert "rx char-set from string"
  (regexp-search? (rx ("aeiou")) "hello"))

(test-assert "rx char-set does not match absent chars"
  (not (regexp-search? (rx (seq bos ("aeiou") eos)) "x")))

;; ========== rx macro: named character classes ==========

(test-assert "rx alpha matches letters"
  (regexp-search? (rx alpha) "abc"))

(test-assert "rx alpha does not match digits"
  (not (regexp-search? (rx (seq bos alpha eos)) "5")))

(test-assert "rx digit matches digits"
  (regexp-search? (rx digit) "abc123"))

(test-assert "rx alnum matches both"
  (and (regexp-search? (rx alnum) "abc")
       (regexp-search? (rx alnum) "123")))

;; ========== rx macro: case insensitive ==========

(test-assert "rx w/nocase matches case-insensitively"
  (regexp-search? (rx (w/nocase "hello")) "HELLO"))

(test-assert "rx w/nocase matches mixed case"
  (regexp-search? (rx (w/nocase "hello")) "HeLLo"))

;; ========== rx macro: posix-string pass-through ==========

(test-assert "rx posix-string passes through"
  (regexp-search? (rx (posix-string "fo+")) "foo"))

(test-assert "rx posix-string with groups"
  (let ((m (regexp-search (rx (posix-string "(fo+)(bar)")) "foobar")))
    (and m
         (equal? (match:substring m 1) "foo")
         (equal? (match:substring m 2) "bar"))))

;; ========== Match with start offset ==========

(test-assert "regexp-search with start offset"
  (let ((m (regexp-search (rx "hello") "hello hello" 1)))
    (and m (= (match:start m) 6))))

;; ========== Non-participating submatches ==========

(test-assert "non-participating submatch returns #f"
  (let ((m (regexp-search (rx (or (submatch "a") (submatch "b"))) "b")))
    (and m
         (not (match:start m 1))
         (equal? (match:substring m 2) "b"))))

;; ========== regexp? predicate ==========

(test-assert "regexp? on compiled regex"
  (regexp? (rx "hello")))

(test-assert "regexp? on non-regex"
  (not (regexp? "hello")))

;; ========== string->regexp ==========

(test-assert "string->regexp works"
  (regexp-search? (string->regexp "hel+o") "hello"))

;; ========== Complex patterns ==========

(test-assert "email-like pattern"
  (let ((m (regexp-search (rx (seq (submatch (+ (or alpha digit "." "-")))
                                   "@"
                                   (submatch (+ (or alpha digit "." "-")))))
                          "user@host.com")))
    (and m
         (equal? (match:substring m 1) "user")
         (equal? (match:substring m 2) "host.com"))))

(test-assert "nested quantifiers"
  (regexp-search? (rx (seq bos (+ (seq alpha (* digit))) eos)) "a1b2c"))

(test-end)
