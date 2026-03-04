;; test-compat.ss -- Comprehensive tests for (hafod compat), (hafod internal strings), (hafod internal char-sets)
;; Run with: scheme --libdirs .:src --script test/test-compat.ss

(import (test runner) (hafod compat) (hafod internal strings) (hafod internal char-sets))

(test-begin "hafod compat + internal libraries")

;;; ========== receive tests ==========
(test-equal "receive two values"
  '(1 2)
  (receive (a b) (values 1 2) (list a b)))

(test-equal "receive rest args"
  '(1 2 3)
  (receive (a . rest) (values 1 2 3) (cons a rest)))

(test-equal "receive single value"
  42
  (receive (a) (values 42) a))

(test-equal "receive all as rest"
  '(1 2 3)
  (receive all (values 1 2 3) all))

;;; ========== :optional tests ==========
(test-equal ":optional empty list with default"
  99
  (:optional '() 99))

(test-equal ":optional one-element list"
  5
  (:optional '(5) 99))

(test-equal ":optional with arg-test passing"
  5
  (:optional '(5) 99 integer?))

(test-error ":optional too many args"
  (:optional '(1 2) 99))

;;; ========== let-optionals* tests ==========
(test-equal "let-optionals* no bindings"
  42
  (let-optionals* '() () 42))

(test-equal "let-optionals* no args supplied"
  '(10 20)
  (let-optionals* '() ((x 10) (y 20)) (list x y)))

(test-equal "let-optionals* partial args"
  '(1 20)
  (let-optionals* '(1) ((x 10) (y 20)) (list x y)))

(test-equal "let-optionals* full args"
  '(1 2)
  (let-optionals* '(1 2) ((x 10) (y 20)) (list x y)))

(test-equal "let-optionals* rest variable"
  '(1 (2 3))
  (let-optionals* '(1 2 3) ((x 10) rest) (list x rest)))

(test-equal "let-optionals* empty rest"
  '(1 ())
  (let-optionals* '(1) ((x 10) rest) (list x rest)))

(test-equal "let-optionals* sequential scope (star)"
  '(5 10)
  (let-optionals* '(5) ((x 10) (y (* x 2))) (list x y)))

;;; ========== let-optionals tests ==========
(test-equal "let-optionals partial args"
  '(1 20)
  (let-optionals '(1) ((x 10) (y 20)) (list x y)))

;;; ========== define-simple-syntax test ==========
(define-simple-syntax (add1 x) (+ x 1))
(test-equal "define-simple-syntax"
  6
  (add1 5))

;;; ========== check-arg tests ==========
(test-equal "check-arg valid"
  5
  (check-arg integer? 5 'test))

(test-error "check-arg invalid"
  (check-arg integer? "x" 'test))

;;; ========== stringify tests ==========
(test-equal "stringify string"
  "hello"
  (stringify "hello"))

(test-equal "stringify symbol"
  "world"
  (stringify 'world))

(test-equal "stringify integer"
  "42"
  (stringify 42))

(test-error "stringify other type"
  (stringify '(1 2 3)))

;;; ========== Vector operation tests ==========
(test-equal "mapv"
  '#(2 3 4)
  (mapv (lambda (x) (+ x 1)) '#(1 2 3)))

(test-equal "mapv!"
  '#(2 4 6)
  (let ((v (vector 1 2 3)))
    (mapv! (lambda (x) (* x 2)) v)
    v))

(test-assert "vector-every? true"
  (vector-every? integer? '#(1 2 3)))

(test-assert "vector-every? false"
  (not (vector-every? integer? '#(1 "a" 3))))

(test-assert "vector-every? empty"
  (vector-every? integer? '#()))

(let ((orig '#(1 2 3)))
  (let ((cp (copy-vector orig)))
    (test-equal "copy-vector contents" '#(1 2 3) cp)
    (test-assert "copy-vector not eq?" (not (eq? orig cp)))))

(test-equal "initialize-vector"
  '#(0 1 4)
  (initialize-vector 3 (lambda (i) (* i i))))

(test-equal "vector-append two"
  '#(1 2 3 4)
  (vector-append '#(1 2) '#(3 4)))

(test-equal "vector-append empty"
  '#()
  (vector-append))

(test-equal "vector-append three"
  '#(1 2 3 4 5 6)
  (vector-append '#(1 2) '#(3 4) '#(5 6)))

;;; ========== vfold / vfold-right tests ==========
(test-equal "vfold"
  6
  (vfold (lambda (x acc) (+ x acc)) 0 '#(1 2 3)))

(test-equal "vfold-right"
  '(1 2 3)
  (vfold-right (lambda (x acc) (cons x acc)) '() '#(1 2 3)))

;;; ========== bogus-substring-spec? tests ==========
(test-assert "bogus-substring-spec? valid"
  (not (bogus-substring-spec? "hello" 0 5)))

(test-assert "bogus-substring-spec? negative start"
  (bogus-substring-spec? "hello" -1 5))

(test-assert "bogus-substring-spec? end too large"
  (bogus-substring-spec? "hello" 0 6))

(test-assert "bogus-substring-spec? end < start"
  (bogus-substring-spec? "hello" 3 2))

;;; ========== string-index tests ==========
(test-equal "string-index char found"
  2
  (string-index "hello" #\l))

(test-equal "string-index char found with start"
  3
  (string-index "hello" #\l 3))

(test-assert "string-index char not found"
  (not (string-index "hello" #\x)))

(test-assert "string-index predicate not found"
  (not (string-index "hello" char-upper-case?)))

(test-equal "string-index predicate found"
  0
  (string-index "hello" char-lower-case?))

;;; ========== string-index-right tests ==========
(test-equal "string-index-right char found"
  3
  (string-index-right "hello" #\l))

(test-equal "string-index-right first occurrence"
  3
  (string-index-right "abcabc" #\a))

(test-assert "string-index-right not found"
  (not (string-index-right "hello" #\x)))

;;; ========== string-every tests ==========
(test-assert "string-every true"
  (string-every char-alphabetic? "hello"))

(test-assert "string-every empty string"
  (string-every char-alphabetic? ""))

(test-assert "string-every false"
  (not (string-every char-alphabetic? "hel1o")))

;;; ========== char-set tests ==========
(test-assert "char-set-contains? member"
  (char-set-contains? (char-set #\a #\b #\c) #\b))

(test-assert "char-set-contains? non-member"
  (not (char-set-contains? (char-set #\a #\b #\c) #\d)))

(test-assert "char-set:whitespace space"
  (char-set-contains? char-set:whitespace #\space))

(test-assert "char-set:whitespace tab"
  (char-set-contains? char-set:whitespace #\tab))

(test-assert "char-set:newline newline"
  (char-set-contains? char-set:newline #\newline))

(test-assert "x->char-set string"
  (char-set-contains? (x->char-set "abc") #\b))

(test-assert "x->char-set char"
  (char-set-contains? (x->char-set #\a) #\a))

(test-assert "char-set? on x->char-set"
  (char-set? (x->char-set "xy")))

;;; ========== deprecated-proc test ==========
(test-equal "deprecated-proc works"
  6
  (let ((dp (deprecated-proc + 'old-plus "use + instead")))
    (dp 1 2 3)))

;;; ========== real->exact-integer test ==========
(test-equal "real->exact-integer"
  3
  (real->exact-integer 3.4))

(test-equal "real->exact-integer exact"
  4
  (real->exact-integer 3.5))

(test-end)
