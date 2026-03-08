(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod editor sexp-tracker))

(test-begin "sexp-tracker")

;; Basic depth
(test-equal "balanced parens depth 0"
  0
  (sexp-depth "(+ 1 2)"))

(test-equal "one open paren depth 1"
  1
  (sexp-depth "(+ 1"))

(test-equal "nested balanced depth 0"
  0
  (sexp-depth "((()))"))

(test-equal "two open parens depth 2"
  2
  (sexp-depth "(("))

;; String handling
(test-equal "paren inside string ignored"
  0
  (sexp-depth "(+ \"(\" 2)"))

(test-equal "escaped quote in string"
  0
  (sexp-depth "(+ \"\\\"\" 2)"))

;; Line comment
(test-equal "line comment"
  0
  (sexp-depth "(; comment\n)"))

(test-equal "paren in line comment ignored"
  1
  (sexp-depth "(; )\n"))

;; Block comment
(test-equal "block comment"
  0
  (sexp-depth "(#|) foo|# )"))

(test-equal "nested block comment"
  0
  (sexp-depth "(#| #| nested |# |# )"))

;; Char literal
(test-equal "char literal open paren"
  0
  (sexp-depth "(#\\( )"))

(test-equal "char literal close paren"
  0
  (sexp-depth "(#\\) )"))

;; Brackets
(test-equal "brackets balanced"
  0
  (sexp-depth "[1 2 3]"))

(test-equal "mixed parens and brackets"
  0
  (sexp-depth "(+ [1 2])"))

;; find-matching-paren
(test-equal "find matching open paren"
  0
  (find-matching-paren "(+ 1 2)" 6))

(test-equal "find matching with string"
  0
  (find-matching-paren "(\"(\" )" 5))

(test-equal "find matching bracket"
  3
  (find-matching-paren "(+ [1 2])" 7))

(test-equal "find matching nested"
  0
  (find-matching-paren "((()))" 5))

(test-equal "no match returns #f"
  #f
  (find-matching-paren ") extra" 0))

(test-end)
