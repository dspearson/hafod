#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod re)
        (chezscheme))

(test-begin "SRE Substitution and Folding")

;; ========== regexp-match ==========

(test-assert "regexp-match matches at start of string"
  (let ((m (regexp-match (rx (seq "hello")) "hello world")))
    (and m (= 0 (match:start m 0)))))

(test-assert "regexp-match returns #f when match not at start"
  (not (regexp-match (rx (seq "world")) "hello world")))

(test-assert "regexp-match with submatches"
  (let ((m (regexp-match (rx (seq (submatch (+ alpha)) " " (submatch (+ alpha)))) "hello world")))
    (and m
         (equal? "hello" (match:substring m 1))
         (equal? "world" (match:substring m 2)))))

;; ========== regexp-substitute ==========

(test-equal "regexp-substitute with string and submatch ref"
  "<<hello>>"
  (let ((m (regexp-search (rx (submatch (+ alpha))) "hello world")))
    (regexp-substitute #f m "<<" 1 ">>")))

(test-equal "regexp-substitute with 'pre and 'post"
  "[say] hello world"
  (let ((m (regexp-search (rx (submatch (+ alpha))) "say hello world")))
    (regexp-substitute #f m 'pre "[" 1 "]" 'post)))

(test-equal "regexp-substitute to port"
  "result: hello"
  (let ((m (regexp-search (rx (submatch (+ alpha))) "hello world"))
        (p (open-output-string)))
    (regexp-substitute p m "result: " 1)
    (get-output-string p)))

(test-equal "regexp-substitute 'pre is text before match"
  "before:"
  (let ((m (regexp-search (rx "X") "before:X:after")))
    (regexp-substitute #f m 'pre)))

(test-equal "regexp-substitute 'post is text after match"
  ":after"
  (let ((m (regexp-search (rx "X") "before:X:after")))
    (regexp-substitute #f m 'post)))

;; ========== regexp-substitute/global ==========

(test-equal "regexp-substitute/global simple replacement"
  "bbb"
  (regexp-substitute/global #f (rx "a") "aaa" 'pre "b" 'post))

(test-equal "regexp-substitute/global with no match returns original"
  "hello"
  (regexp-substitute/global #f (rx "z") "hello" 'pre "b" 'post))

(test-equal "regexp-substitute/global replaces capturing groups"
  "[h][w]"
  (regexp-substitute/global #f (rx (submatch alpha)) "hw"
    'pre "[" 1 "]" 'post))

(test-equal "regexp-substitute/global with procedure template"
  "HELLO"
  (regexp-substitute/global #f (rx (submatch alpha)) "hello"
    'pre (lambda (m) (string-upcase (match:substring m 0))) 'post))

;; ========== regexp-fold ==========

(test-equal "regexp-fold collects all matches"
  '("o" "l" "l" "e" "h")
  (regexp-fold (rx alpha)
    (lambda (i m acc) (cons (match:substring m 0) acc))
    '()
    "hello"))

(test-equal "regexp-fold with seed accumulator"
  5
  (regexp-fold (rx alpha)
    (lambda (i m acc) (+ acc 1))
    0
    "hello"))

(test-equal "regexp-fold on empty string"
  0
  (regexp-fold (rx alpha)
    (lambda (i m acc) (+ acc 1))
    0
    ""))

(test-equal "regexp-fold counts digit runs"
  3
  (regexp-fold (rx (+ digit))
    (lambda (i m acc) (+ acc 1))
    0
    "abc123def456ghi789"))

;; ========== regexp-for-each ==========

(test-assert "regexp-for-each iterates all matches"
  (let ((count 0))
    (regexp-for-each (rx (+ digit))
      (lambda (m) (set! count (+ count 1)))
      "abc123def456ghi789")
    (= count 3)))

(test-assert "regexp-for-each with no matches does nothing"
  (let ((count 0))
    (regexp-for-each (rx "zzz")
      (lambda (m) (set! count (+ count 1)))
      "hello world")
    (= count 0)))

(test-end)
