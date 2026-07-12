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

;; A slice end past the subject buffer would make regexec (REG_STARTEND) read
;; past the bytevector; the primitive must reject the offsets, not over-read.
(test-error "posix-regexec rejects an end past the subject buffer"
  (let ((rt (posix-regcomp "a" REG_EXTENDED)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (posix-regexec rt (string->utf8 "abc") 1 0 0 100))
      (lambda () (posix-regfree rt)))))

;; nmatch = 0 is a bare "does it match at all?" query: REG_STARTEND still needs
;; an internal pmatch[0] to seed, but the caller asked for no groups, so a match
;; yields an empty result vector (truthy) and a non-match yields #f -- not an
;; opaque bytevector-index error.
(test-assert "posix-regexec with nmatch=0 reports a match as an empty vector"
  (let* ((rt (posix-regcomp "hello" REG_EXTENDED))
         (result (posix-regexec rt "say hello world" 0 0)))
    (posix-regfree rt)
    (and (vector? result) (= 0 (vector-length result)))))

(test-assert "posix-regexec with nmatch=0 reports a non-match as #f"
  (let* ((rt (posix-regcomp "xyz" REG_EXTENDED))
         (result (posix-regexec rt "hello world" 0 0)))
    (posix-regfree rt)
    (eq? result #f)))

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

;; An out-of-range start index is a caller error (fail-loud), matching the
;; pre-rewrite substring contract -- not a silent #f or a glibc-clamped match.
(test-error "regexp-search raises on a start past the subject"
  (regexp-search (rx "a") "abc" 10))
(test-error "regexp-search raises on a negative start"
  (regexp-search (rx "a") "abc" -1))
;; regexp-search? shares the entry, so it fails loud on a bad start too.
(test-error "regexp-search? raises on a start past the subject"
  (regexp-search? (rx "a") "abc" 10))
;; The bound is the CHARACTER length the old substring used: a start past the
;; char length still fails loud even when it is a valid UTF-8 byte offset (the
;; 2-char "a\xe9;" encodes to 3 bytes, so index 3 is in byte range but not char
;; range).
(test-error "regexp-search raises on a start past the character length"
  (regexp-search (rx "a") "a\xe9;" 3))

;; ========== Anchored / absolute-offset search from a non-zero start ==========

;; A begin-of-string anchor searched from a non-zero start must NOT match at
;; the offset -- the offset is mid-subject, not a line start.
(test-assert "bos does not match at a non-zero start offset"
  (not (regexp-search? (rx bos "abc") "xxxabc" 3)))

;; Offsets returned from a non-zero start are absolute (relative to the whole
;; subject), so "bc" first found at index 4 reports 4..6, not 7..9.
(test-assert "match offsets from a non-zero start are absolute"
  (let ((m (regexp-search (rx "bc") "xxxabcabc" 3)))
    (and m
         (= (match:start m) 4)
         (= (match:end m) 6))))

;; An iterated search over a long subject must encode it once, not once per
;; step; the exact match count over 20000 repeats is the correctness proxy.
(test-assert "iterated search over a long subject counts every match"
  (let ((s (let ((p (open-output-string)))
             (do ((i 0 (+ i 1))) ((= i 20000))
               (display "ab" p))
             (get-output-string p)))
        (count 0))
    (regexp-for-each (rx "ab") (lambda (m) (set! count (+ count 1))) s)
    (= count 20000)))

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

;; ========== char-class bracket emission edge cases ==========
;; These exercise the runtime SRE compiler (sre->regexp -> regexp->posix-string),
;; where a char-set value is turned into a POSIX bracket string.

;; A bracket whose only members are ^ and - must be a POSITIVE class matching ^
;; and - only -- not a negation.  A caret leading a positive bracket would read
;; as "not ...", so the compiler leads with a literal - instead: [-^].
(test-assert "caret-dash class matches a dash"
  (regexp-search? (sre->regexp '("^-")) "-"))
(test-assert "caret-dash class matches a caret"
  (regexp-search? (sre->regexp '("^-")) "^"))
(test-equal "caret-dash class does not match a letter"
  #f (regexp-search? (sre->regexp '("^-")) "x"))
(test-equal "caret-dash class compiles to a positive bracket"
  (string #\[ #\- #\^ #\])
  (let-values (((s lev pc sm) (regexp->posix-string (sre->regexp '("^-"))))) s))

;; A nested unmatchable -- (seq "a" (or)) -- must COMPILE and never match.  The
;; never-match bracket must carry no NUL byte: a literal NUL truncates the
;; pattern at regcomp to an unterminated "[^", which the engine rejects.
(test-assert "nested unmatchable compiles and never matches"
  (not (regexp-search? (sre->regexp '(seq "a" (or))) "axyz")))
(test-assert "nested empty alternation in a sequence compiles without error"
  (guard (e (#t #f))
    (begin (regexp-search? (sre->regexp '(seq "a" (or))) "") #t)))
(test-equal "never-match bracket is free of a NUL byte"
  (string #\a #\[ #\^ (integer->char 1) #\- (integer->char 127) #\])
  (let-values (((s lev pc sm) (regexp->posix-string (sre->regexp '(seq "a" (or)))))) s))

;; A zero-or-more of an unmatchable still matches the empty string (the bracket
;; stays a single atom the quantifier can bind to).
(test-assert "star of an unmatchable matches the empty string"
  (regexp-search? (sre->regexp '(* (or))) ""))

;; Regression guard: the ascii class was never the bug -- it must still compile
;; to a valid [\x01-\x7f] range and match an ASCII character.
(test-assert "ascii still matches an ASCII character"
  (regexp-search? (sre->regexp 'ascii) "a"))
(test-equal "ascii still compiles to the expected range bracket"
  (string #\[ (integer->char 1) #\- (integer->char 127) #\])
  (let-values (((s lev pc sm) (regexp->posix-string (sre->regexp 'ascii)))) s))

;; ========== rx-macro char-class bracket emission (expand-time compiler) =====
;; The rx macro compiles its SRE literal at expand time through the separate
;; (hafod internal sre-compile) compiler, not the runtime regexp->posix-string
;; path exercised above.  These pin the same bracket edges on that macro path.

;; A {^,-} char-set literal must expand to a POSITIVE bracket matching ^ and -
;; only, led by a literal - so the leading ^ is not misread as a negation.
(test-assert "rx caret-dash class matches a dash"
  (regexp-search? (rx ("^-")) "-"))
(test-assert "rx caret-dash class matches a caret"
  (regexp-search? (rx ("^-")) "^"))
(test-equal "rx caret-dash class does not match a letter"
  #f (regexp-search? (rx ("^-")) "x"))

;; The ascii class must expand to a real byte range, not the literal text
;; [\x00-\x7f] (POSIX ERE has no \xHH escape); a real [\x01-\x7f] matches an
;; ordinary ASCII character.
(test-assert "rx ascii matches an ASCII character"
  (regexp-search? (rx ascii) "a"))

;; A nested empty alternation must expand to a never-match bracket built from
;; real bytes (no literal NUL), so it compiles yet never matches a subject.
(test-assert "rx nested empty alternation compiles and never matches"
  (not (regexp-search? (rx (seq "a" (or))) "ay")))
(test-assert "rx nested empty alternation compiles without error"
  (guard (e (#t #f))
    (begin (regexp-search? (rx (seq "a" (or))) "") #t)))

;; A zero-or-more of that unmatchable still matches the empty string (the bracket
;; stays a single atom the quantifier binds to).
(test-assert "rx star of an empty alternation matches the empty string"
  (regexp-search? (rx (* (or))) ""))

(test-end)
