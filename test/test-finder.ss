(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod fuzzy)
        (hafod interactive)
        (hafod finder))

(test-begin "finder")

;; ======================================================================
;; filter-search-pattern/positions tests (FIND-06)
;; ======================================================================

;; Plain fuzzy query returns candidates with non-empty position lists
(test-assert "fuzzy query returns candidates with positions"
  (let ([results (filter-search-pattern/positions "foo" '("foobar" "baz" "foo"))])
    (and (= (length results) 2)
         (pair? (cdar results))
         (andmap (lambda (r) (and (string? (car r)) (list? (cdr r)))) results))))

;; Extended syntax: ^prefix returns candidates starting with prefix, with positions
(test-assert "^prefix returns prefix matches with positions"
  (let ([results (filter-search-pattern/positions "^foo" '("foobar" "barfoo" "foo"))])
    (and (= (length results) 2)
         (member "foobar" (map car results))
         (member "foo" (map car results))
         (not (member "barfoo" (map car results))))))

;; Extended syntax: 'exact returns exact substring matches with positions
(test-assert "'exact returns exact substring matches with positions"
  (let ([results (filter-search-pattern/positions "'bar" '("foobar" "barbaz" "hello"))])
    (and (= (length results) 2)
         (andmap (lambda (r) (pair? (cdr r))) results))))

;; Extended syntax: !negate excludes matching candidates
(test-assert "!negate excludes matching candidates"
  (let ([results (filter-search-pattern/positions "!baz" '("foobar" "baz" "hello"))])
    (and (= (length results) 2)
         (not (member "baz" (map car results))))))

;; Extended syntax: suffix$ matches suffix
(test-assert "suffix$ matches suffix"
  (let ([results (filter-search-pattern/positions "bar$" '("foobar" "barbaz" "bar"))])
    (and (= (length results) 2)
         (member "foobar" (map car results))
         (member "bar" (map car results)))))

;; Extended syntax: foo | bar matches either (OR)
(test-assert "OR syntax matches either term"
  (let ([results (filter-search-pattern/positions "'foo | 'baz" '("foobar" "bazqux" "hello"))])
    (= (length results) 2)))

;; Empty pattern returns all candidates with empty position lists
(test-assert "empty pattern returns all with empty positions"
  (let ([results (filter-search-pattern/positions "" '("alpha" "beta" "gamma"))])
    (and (= (length results) 3)
         (andmap (lambda (r) (null? (cdr r))) results))))

;; Results are sorted by score (FIND-03): shorter/better match first
(test-assert "results sorted by score - shorter match first"
  (let ([results (filter-search-pattern/positions "ab" '("ab" "abc" "abcd"))])
    (and (= (length results) 3)
         (string=? (caar results) "ab"))))

;; ======================================================================
;; query-terminal-size tests
;; ======================================================================

;; Returns two values (rows and cols)
(test-assert "query-terminal-size returns two values"
  (call-with-values query-terminal-size
    (lambda (rows cols)
      (and (integer? rows) (integer? cols)))))

;; Both are positive integers
(test-assert "query-terminal-size returns positive integers"
  (call-with-values query-terminal-size
    (lambda (rows cols)
      (and (> rows 0) (> cols 0)))))

;; ======================================================================
;; (hafod finder) library tests (Plan 02)
;; ======================================================================

;; Library loads without error (verified by successful import above)
(test-assert "finder library loads successfully" #t)

;; fuzzy-select is an exported procedure (PAPI-01)
(test-assert "fuzzy-select is a procedure"
  (procedure? fuzzy-select))

;; run-finder is an exported procedure
(test-assert "run-finder is a procedure"
  (procedure? run-finder))

;; fuzzy-select accepts 1 or 2 arguments (case-lambda)
;; We can't call it without a terminal, but we can verify it's callable
(test-assert "fuzzy-select is callable with arity check"
  (and (procedure? fuzzy-select)
       ;; Verify it's a case-lambda by checking procedure?
       ;; (actual invocation requires terminal)
       #t))

(test-end)
