#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod re)
        (hafod fileinfo)
        (chezscheme))

(test-begin "Regex API Completion")

;; ========== let-match ==========

(test-equal "let-match: basic binding with two submatches"
  '("foobar" "foo" "bar")
  (let-match (regexp-search (rx (submatch "foo") (submatch "bar")) "foobar")
    (whole f b)
    (list whole f b)))

(test-equal "let-match: single submatch"
  '("hello" "hello")
  (let-match (regexp-search (rx (submatch "hello")) "say hello world")
    (whole h)
    (list whole h)))

(test-equal "let-match: whole match only (no submatches)"
  "bar"
  (let-match (regexp-search (rx "bar") "foobar")
    (whole)
    whole))

(test-equal "let-match: with #f don't-care (skip index 0 and 2)"
  '("inner1" "inner3")
  (let-match (regexp-search (rx (submatch "inner1") (submatch "mid") (submatch "inner3"))
                            "inner1midinner3")
    (#f a #f b)
    (list a b)))

(test-equal "let-match: skip whole match with #f"
  "part"
  (let-match (regexp-search (rx (submatch "part")) "apart")
    (#f p)
    p))

;; ========== if-match ==========

(test-equal "if-match: match success"
  "bar"
  (if-match (regexp-search (rx "bar") "bar")
    (bar) bar 'no-match))

(test-equal "if-match: match failure"
  'no-match
  (if-match (regexp-search (rx "xyz") "baz")
    (bar) bar 'no-match))

(test-equal "if-match: with submatches on success"
  '("foo" "bar")
  (if-match (regexp-search (rx (submatch "foo") (submatch "bar")) "foobar")
    (whole f b) (list f b)
    'no-match))

(test-equal "if-match: failure returns alt"
  42
  (if-match (regexp-search (rx "nope") "hello")
    (w) w
    42))

;; ========== match-cond ==========

(test-equal "match-cond: first match clause hits"
  "foo"
  (match-cond
    ((regexp-search (rx (submatch "foo")) "foobar") (w f) f)
    (else 'no)))

(test-equal "match-cond: else clause hits"
  'default
  (match-cond
    ((regexp-search (rx "xyz") "abc") (w) w)
    (else 'default)))

(test-equal "match-cond: test clause with truthy"
  'yes
  (match-cond
    (test #t 'yes)
    (else 'no)))

(test-equal "match-cond: test clause false falls through"
  'default
  (match-cond
    (test #f 'yes)
    (else 'default)))

(test-equal "match-cond: multiple clauses"
  "world"
  (match-cond
    ((regexp-search (rx "xyz") "hello world") (w) w)
    ((regexp-search (rx (submatch "world")) "hello world") (w s) s)
    (else 'nothing)))

(test-equal "match-cond: test with => proc"
  10
  (match-cond
    (test (assv 'b '((a . 1) (b . 10) (c . 100))) => cdr)
    (else 'no)))

;; ========== regexp-fold-right ==========

(test-equal "regexp-fold-right: collect matches into list"
  '("foo1" "foo2" "foo3")
  (regexp-fold-right (rx (: "foo" (+ digit)))
                     (lambda (m i list) (cons (match:substring m) list))
                     '()
                     "foo1 foo2 foo3"
                     (lambda (i list) list)))

(test-equal "regexp-fold-right: count matches"
  3
  (regexp-fold-right (rx (+ alpha))
                     (lambda (m i acc) (+ acc 1))
                     0
                     "abc 123 def 456 ghi"))

(test-equal "regexp-fold-right: no matches returns finish applied to knil"
  'empty
  (regexp-fold-right (rx "xyz")
                     (lambda (m i acc) (cons (match:substring m) acc))
                     '()
                     "no matches here"
                     (lambda (i val) (if (null? val) 'empty val))))

(test-equal "regexp-fold-right: finish receives first-match-start"
  5
  (regexp-fold-right (rx (+ digit))
                     (lambda (m i acc) acc)
                     'ignored
                     "hello123world"
                     (lambda (i val) i)))

(test-equal "regexp-fold-right: builds from right"
  '(("abc" . 4) ("def" . 8) ("ghi" . 11))
  (regexp-fold-right (rx (+ alpha))
                     (lambda (m i acc)
                       (cons (cons (match:substring m) i) acc))
                     '()
                     "abc def ghi"
                     (lambda (i val) val)))

;; ========== sre-form? ==========

(test-assert "sre-form?: string is SRE"
  (sre-form? '"hello"))

(test-assert "sre-form?: (: ...) is SRE"
  (sre-form? '(: "foo")))

(test-assert "sre-form?: (seq ...) is SRE"
  (sre-form? '(seq "a" "b")))

(test-assert "sre-form?: (or ...) is SRE"
  (sre-form? '(or "a" "b")))

(test-assert "sre-form?: (submatch ...) is SRE"
  (sre-form? '(submatch "foo")))

(test-assert "sre-form?: (* ...) is SRE"
  (sre-form? '(* "a")))

(test-assert "sre-form?: (+ ...) is SRE"
  (sre-form? '(+ "a")))

(test-assert "sre-form?: (? ...) is SRE"
  (sre-form? '(? "a")))

(test-assert "sre-form?: (= n ...) is SRE"
  (sre-form? '(= 3 "a")))

(test-assert "sre-form?: (>= n ...) is SRE"
  (sre-form? '(>= 2 "a")))

(test-assert "sre-form?: (** m n ...) is SRE"
  (sre-form? '(** 2 5 "a")))

(test-assert "sre-form?: (- ...) is SRE"
  (sre-form? '(- alpha "aeiou")))

(test-assert "sre-form?: (& ...) is SRE"
  (sre-form? '(& ascii alpha)))

(test-assert "sre-form?: (~ ...) is SRE"
  (sre-form? '(~ digit)))

(test-assert "sre-form?: (uncase ...) is SRE"
  (sre-form? '(uncase "hello")))

(test-assert "sre-form?: (dsm ...) is SRE"
  (sre-form? '(dsm 1 0 "a")))

(test-assert "sre-form?: string-list is SRE"
  (sre-form? '("aeiou")))

(test-assert "sre-form?: 'any is SRE"
  (sre-form? 'any))

(test-assert "sre-form?: 'nonl is SRE"
  (sre-form? 'nonl))

(test-assert "sre-form?: 'alpha is SRE"
  (sre-form? 'alpha))

(test-assert "sre-form?: 'digit is SRE"
  (sre-form? 'digit))

(test-assert "sre-form?: 'ascii is SRE"
  (sre-form? 'ascii))

(test-assert "sre-form?: 'whitespace is SRE"
  (sre-form? 'whitespace))

(test-assert "sre-form?: 'bos is SRE"
  (sre-form? 'bos))

(test-assert "sre-form?: 'eos is SRE"
  (sre-form? 'eos))

(test-assert "sre-form?: non-SRE symbol returns #f"
  (not (sre-form? 'blah)))

(test-assert "sre-form?: number returns #f"
  (not (sre-form? 42)))

(test-assert "sre-form?: #t returns #f"
  (not (sre-form? #t)))

(test-assert "sre-form?: empty list returns #f"
  (not (sre-form? '())))

(test-assert "sre-form?: (posix-string ...) is SRE"
  (sre-form? '(posix-string "foo")))

;; ========== if-sre-form ==========

(test-equal "if-sre-form: SRE compound form"
  'yes
  (if-sre-form (: "foo" "bar") 'yes 'no))

(test-equal "if-sre-form: non-SRE symbol"
  'no
  (if-sre-form blah 'yes 'no))

(test-equal "if-sre-form: named class SRE"
  'yes
  (if-sre-form alpha 'yes 'no))

(test-equal "if-sre-form: string SRE"
  'yes
  (if-sre-form "hello" 'yes 'no))

(test-equal "if-sre-form: any is SRE"
  'yes
  (if-sre-form any 'yes 'no))

(test-equal "if-sre-form: random identifier is not SRE"
  'no
  (if-sre-form my-predicate 'yes 'no))

;; Test the let-syntax pattern from sre-tools-tests: using if-sre-form to dispatch
;; between regex and predicate.
(test-equal "if-sre-form: dispatch pattern"
  #t
  (let-syntax ((check (syntax-rules ()
                         ((check form str)
                          (if-sre-form form
                            (regexp-search? (rx form) str)
                            (form str))))))
    (check alpha "a")))

(test-equal "if-sre-form: dispatch pattern with predicate"
  5
  (let-syntax ((check (syntax-rules ()
                         ((check form str)
                          (if-sre-form form
                            (regexp-search? (rx form) str)
                            (form str))))))
    (check string-length "hello")))

;; ========== file-mode=? ==========

(test-assert "file-mode=?: equal modes"
  (file-mode=? #o644 #o644))

(test-assert "file-mode=?: unequal modes"
  (not (file-mode=? #o755 #o644)))

(test-assert "file-mode=?: zero modes"
  (file-mode=? 0 0))

(test-assert "file-mode=?: all bits set"
  (file-mode=? #o7777 #o7777))

(test-assert "file-mode=?: close but not equal"
  (not (file-mode=? #o644 #o664)))

;; ========== Integration tests ==========

(test-equal "let-match + rx: digit extraction"
  '("2026" "03" "02")
  (let-match (regexp-search (rx (submatch (= 4 digit)) "-"
                                (submatch (= 2 digit)) "-"
                                (submatch (= 2 digit)))
                            "Date: 2026-03-02")
    (#f year month day)
    (list year month day)))

(test-equal "if-match + regexp-search: extract or default"
  "default"
  (if-match (regexp-search (rx (submatch (+ digit))) "no digits here")
    (whole digits) digits
    "default"))

(test-equal "match-cond: dispatch on multiple patterns"
  'email
  (let ((line "user@example.com"))
    (match-cond
      ((regexp-search (rx (submatch (+ digit))) line) (w d) 'number)
      ((regexp-search (rx (submatch (+ alpha) "@" (+ alpha) "." (+ alpha))) line) (w e) 'email)
      (else 'unknown))))

(test-end)
