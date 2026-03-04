#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod re)
        (hafod re-adt)
        (hafod compat)
        (hafod internal char-sets)
        (chezscheme))

(test-begin "Regex Conversion APIs")

;; ========== sre->regexp ==========

(test-assert "sre->regexp: returns regexp? value"
  (regexp? (sre->regexp '(: "foobar" "baz"))))

(test-assert "sre->regexp: string literal"
  (regexp-search? (sre->regexp '"hello") "hello world"))

(test-assert "sre->regexp: seq with any"
  (regexp-search? (sre->regexp '(: "foo" any "bar")) "foo_bar"))

(test-assert "sre->regexp: or"
  (regexp-search? (sre->regexp '(or "cat" "dog")) "dog"))

(test-assert "sre->regexp: star"
  (regexp-search? (sre->regexp '(* "a")) "aaa"))

(test-assert "sre->regexp: plus"
  (regexp-search? (sre->regexp '(+ "a")) "aaa"))

(test-assert "sre->regexp: question matches with optional"
  (regexp-search? (sre->regexp '(: "colou" (? "r"))) "colour"))

(test-assert "sre->regexp: question matches without optional"
  (regexp-search? (sre->regexp '(: "colou" (? "r"))) "colou"))

(test-equal "sre->regexp: submatch extraction"
  "hello"
  (match:substring (regexp-search (sre->regexp '(submatch (+ alpha))) "hello123") 1))

(test-assert "sre->regexp: named class alpha"
  (regexp-search? (sre->regexp 'alpha) "a"))

(test-assert "sre->regexp: named class digit"
  (regexp-search? (sre->regexp '(+ digit)) "42"))

(test-assert "sre->regexp: bos anchor"
  (regexp-search? (sre->regexp '(: bos "hello")) "hello world"))

(test-assert "sre->regexp: bos anchor fail"
  (not (regexp-search? (sre->regexp '(: bos "world")) "hello world")))

(test-assert "sre->regexp: eos anchor"
  (regexp-search? (sre->regexp '(: "world" eos)) "hello world"))

(test-assert "sre->regexp: exact repetition"
  (regexp-search? (sre->regexp '(= 3 "a")) "aaa"))

(test-assert "sre->regexp: at-least repetition"
  (regexp-search? (sre->regexp '(>= 2 alpha)) "hello"))

(test-assert "sre->regexp: bounded repetition"
  (regexp-search? (sre->regexp '(** 2 4 digit)) "123"))

(test-assert "sre->regexp: w/nocase"
  (regexp-search? (sre->regexp '(w/nocase "hello")) "HELLO"))

(test-assert "sre->regexp: w/case restores"
  (not (regexp-search? (sre->regexp '(w/nocase (w/case "hello"))) "HELLO")))

(test-assert "sre->regexp: dsm"
  (regexp-search? (sre->regexp '(dsm 1 0 "foo")) "foo"))

(test-assert "sre->regexp: char-set form"
  (regexp-search? (sre->regexp '("aeiou")) "i"))

(test-assert "sre->regexp: complement ~"
  (not (regexp-search? (sre->regexp '(: bos (~ alpha) eos)) "a")))

(test-assert "sre->regexp: posix-string passthrough"
  (regexp-search? (sre->regexp '(posix-string "fo+")) "foo"))

(test-assert "sre->regexp: seq alias :"
  (regexp-search? (sre->regexp '(seq "a" "b")) "ab"))

;; ========== regexp->sre ==========

(test-equal "regexp->sre: simple string"
  "hello"
  (regexp->sre (make-re-string "hello")))

(test-equal "regexp->sre: trivial (empty string)"
  ""
  (regexp->sre re-trivial))

(test-equal "regexp->sre: bos"
  'bos
  (regexp->sre re-bos))

(test-equal "regexp->sre: eos"
  'eos
  (regexp->sre re-eos))

(test-equal "regexp->sre: star"
  '(* "a")
  (regexp->sre (re-repeat 0 #f (make-re-string "a"))))

(test-equal "regexp->sre: plus"
  '(+ "a")
  (regexp->sre (re-repeat 1 #f (make-re-string "a"))))

(test-equal "regexp->sre: question"
  '(? "a")
  (regexp->sre (re-repeat 0 1 (make-re-string "a"))))

(test-equal "regexp->sre: exact repeat"
  '(= 3 "a")
  (regexp->sre (make-re-repeat 3 3 (make-re-string "a"))))

(test-equal "regexp->sre: bounded repeat"
  '(** 2 5 "a")
  (regexp->sre (make-re-repeat 2 5 (make-re-string "a"))))

(test-equal "regexp->sre: at-least repeat"
  '(>= 3 "a")
  (regexp->sre (make-re-repeat 3 #f (make-re-string "a"))))

(test-equal "regexp->sre: submatch"
  '(submatch "foo")
  (regexp->sre (re-submatch (make-re-string "foo"))))

(test-equal "regexp->sre: submatch sequence (implicit seq)"
  '(submatch "foo" "bar")
  (regexp->sre (re-submatch (re-seq (list (make-re-string "foo") (make-re-string "bar"))))))

(test-equal "regexp->sre: named char-class any"
  'any
  (regexp->sre re-any))

;; regexp->sre round-trip: parse SRE then unparse
(test-assert "sre->regexp then regexp->sre round-trip works for search"
  (let* ((sre '(: "foo" (+ alpha)))
         (re (sre->regexp sre))
         (sre2 (regexp->sre re)))
    (and (regexp-search? re "foobar")
         (regexp-search? (sre->regexp sre2) "foobar"))))

;; ========== posix-string->regexp ==========

(test-assert "posix-string->regexp: simple literal"
  (regexp-search? (posix-string->regexp "hello") "hello world"))

(test-assert "posix-string->regexp: alternation"
  (regexp-search? (posix-string->regexp "g(ee|oo)se") "goose"))

(test-assert "posix-string->regexp: anchored"
  (regexp-search? (posix-string->regexp "^foo$") "foo"))

(test-assert "posix-string->regexp: char range"
  (regexp-search? (posix-string->regexp "[a-z]+") "hello"))

(test-assert "posix-string->regexp: dot"
  (regexp-search? (posix-string->regexp "a.b") "axb"))

(test-assert "posix-string->regexp: star"
  (regexp-search? (posix-string->regexp "ab*c") "ac"))

(test-assert "posix-string->regexp: plus"
  (regexp-search? (posix-string->regexp "ab+c") "abbc"))

(test-assert "posix-string->regexp: question"
  (regexp-search? (posix-string->regexp "colou?r") "color"))

(test-assert "posix-string->regexp: backslash escape"
  (regexp-search? (posix-string->regexp "a\\.b") "a.b"))

(test-assert "posix-string->regexp: negated bracket"
  (not (regexp-search? (posix-string->regexp "^[^a-z]+$") "hello")))

(test-assert "posix-string->regexp: submatch extraction"
  (let ((m (regexp-search (posix-string->regexp "(foo)(bar)") "foobar")))
    (and (equal? "foo" (match:substring m 1))
         (equal? "bar" (match:substring m 2)))))

(test-assert "posix-string->regexp: braces {m,n}"
  (regexp-search? (posix-string->regexp "a{2,4}") "aaa"))

(test-assert "posix-string->regexp: braces {m}"
  (regexp-search? (posix-string->regexp "a{3}") "aaa"))

(test-assert "posix-string->regexp: braces {m,}"
  (regexp-search? (posix-string->regexp "a{2,}") "aaaa"))

(test-assert "posix-string->regexp: POSIX class [:alpha:]"
  (regexp-search? (posix-string->regexp "[[:alpha:]]+") "hello"))

(test-assert "posix-string->regexp: POSIX class [:digit:]"
  (regexp-search? (posix-string->regexp "[[:digit:]]+") "42"))

(test-assert "posix-string->regexp: empty string matches everything"
  (regexp-search? (posix-string->regexp "") "anything"))

(test-assert "posix-string->regexp: nested groups"
  (let ((m (regexp-search (posix-string->regexp "((a)(b))") "ab")))
    (and (equal? "ab" (match:substring m 1))
         (equal? "a" (match:substring m 2))
         (equal? "b" (match:substring m 3)))))

;; ========== regexp->posix-string ==========

(test-equal "regexp->posix-string: simple string"
  "hello"
  (let-values (((str level pcount smap) (regexp->posix-string (make-re-string "hello"))))
    str))

(test-equal "regexp->posix-string: sequence with alternation"
  "g(ee|oo)se"
  (let-values (((str level pcount smap)
                (regexp->posix-string (sre->regexp '(: "g" (or "ee" "oo") "se")))))
    str))

(test-assert "regexp->posix-string: returns 4 values"
  (let-values (((str level pcount smap) (regexp->posix-string (make-re-string "test"))))
    (and (string? str)
         (number? level)
         (number? pcount)
         (vector? smap))))

;; ========== flush-submatches (re-export) ==========

(test-assert "flush-submatches: removes submatches"
  (let ((re (flush-submatches (sre->regexp '(submatch "foo")))))
    (not (re-submatch? re))))

(test-assert "flush-submatches: search still works"
  (regexp-search? (flush-submatches (sre->regexp '(: (submatch "foo") (submatch "bar")))) "foobar"))

(test-assert "flush-submatches: no submatches in result"
  (let ((m (regexp-search (flush-submatches (sre->regexp '(: (submatch "foo") (submatch "bar") (submatch "baz")))) "foobarbaz")))
    (and m
         ;; The match count should be 1 (just the whole match)
         (= 1 (match:count m)))))

;; ========== uncase (re-export) ==========

(test-assert "uncase: case-insensitive match"
  (regexp-search? (uncase (sre->regexp '"bar")) "BaR"))

(test-equal "uncase: extracts correct text"
  "BaR"
  (match:substring (regexp-search (uncase (sre->regexp '"bar")) "BaR")))

;; ========== uncase-string (re-export) ==========

(test-assert "uncase-string: produces RE that matches case-insensitively"
  (regexp-search? (uncase-string "blah") "Blah"))

(test-equal "uncase-string: correct extraction"
  "Blah"
  (match:substring (regexp-search (uncase-string "blah") "Blah")))

;; ========== uncase-char-set (re-export) ==========

(test-assert "uncase-char-set: produces case-folded char-set RE"
  (let ((re (uncase-char-set (char-set #\a #\b))))
    (and (re-char-set? re)
         (regexp-search? re "A"))))

;; ========== Integration / round-trip tests ==========

(test-assert "round-trip: sre->regexp -> search matches same as rx"
  (let* ((sre-re (sre->regexp '(: "foo" (+ digit))))
         (rx-re (rx (: "foo" (+ digit)))))
    (and (regexp-search? sre-re "foo123")
         (regexp-search? rx-re "foo123")
         (not (regexp-search? sre-re "foobar"))
         (not (regexp-search? rx-re "foobar")))))

(test-assert "round-trip: posix-string->regexp -> regexp->posix-string"
  (let* ((re (posix-string->regexp "a(b|c)d"))
         (m (regexp-search re "acd")))
    (and m (equal? "acd" (match:substring m 0)))))

(test-assert "sre->regexp with complex SRE"
  (regexp-search? (sre->regexp '(: bos (+ (or alpha digit)) eos)) "hello42"))

(test-assert "posix-string->regexp bracket ] as first"
  (regexp-search? (posix-string->regexp "[]abc]") "]"))

(test-assert "posix-string->regexp bracket - as first"
  (regexp-search? (posix-string->regexp "[-abc]") "-"))

(test-end)
