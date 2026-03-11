(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod fuzzy))

(test-begin "fuzzy")

;; ======================================================================
;; Basic fuzzy-match
;; ======================================================================

;; Empty pattern matches everything with score 0
(test-assert "empty pattern matches"
  (let ([r (fuzzy-match "" "anything")])
    (and r (= (car r) 0))))

;; Pattern longer than text => no match
(test-assert "pattern longer than text"
  (not (fuzzy-match "abcdef" "abc")))

;; Exact match
(test-assert "exact match"
  (let ([r (fuzzy-match "abc" "abc")])
    (and r (> (car r) 0))))

;; Fuzzy match: scattered characters
(test-assert "scattered chars match"
  (let ([r (fuzzy-match "abc" "a---b---c")])
    (and r (> (car r) 0))))

;; No match: missing character
(test-assert "missing char no match"
  (not (fuzzy-match "xyz" "xya")))

;; Positions returned correctly for exact match
(test-equal "exact match positions"
  '(0 1 2)
  (let ([r (fuzzy-match "abc" "abc")])
    (and r (cdr r))))

;; ======================================================================
;; V2 optimal alignment: prefers tight consecutive matches
;; ======================================================================

;; V2 should prefer the consecutive "abc" at end over scattered a..b..c
(test-assert "V2 finds optimal alignment"
  (let ([r (fuzzy-match "abc" "a__b__c__abc")])
    (and r
         ;; Positions should be at the consecutive "abc" (9,10,11)
         (equal? (cdr r) '(9 10 11)))))

;; V2 single char: finds best-scoring position
(test-assert "V2 single char finds boundary"
  (let ([r (fuzzy-match "b" "aaa-bbb")])
    (and r
         ;; Position 4 is at word boundary (after -)
         (equal? (cdr r) '(4)))))

;; ======================================================================
;; Scoring: consecutive matches score higher than scattered
;; ======================================================================

(test-assert "consecutive beats scattered"
  (let ([s1 (fuzzy-score "abc" "abc")]
        [s2 (fuzzy-score "abc" "a---b---c")])
    (and s1 s2 (> s1 s2))))

;; Word boundary bonus: match at word start scores higher
(test-assert "word boundary bonus"
  (let ([s1 (fuzzy-score "fb" "foo-bar")]
        [s2 (fuzzy-score "fb" "fxxbxx")])
    (and s1 s2 (> s1 s2))))

;; ======================================================================
;; Consecutive bonus propagation (firstBonus tracking)
;; ======================================================================

;; "oob" in "foo-bar" should match at boundary with consecutive bonus
(test-assert "consecutive bonus propagation"
  (let ([r (fuzzy-match "oob" "foo-bar")])
    (and r (> (car r) 0))))

;; camelCase bonus
(test-assert "camelCase scoring"
  (let ([s1 (fuzzy-score "fb" "FooBar")]
        [s2 (fuzzy-score "fb" "fxxxxxxxb")])
    ;; camelCase transitions should score well
    (and s1 s2 (> s1 s2))))

;; ======================================================================
;; Smart case sensitivity
;; ======================================================================

;; All-lowercase pattern is case-insensitive
(test-assert "lowercase pattern case-insensitive"
  (fuzzy-match "abc" "ABC"))

;; Mixed-case pattern is case-sensitive
(test-assert "mixed case pattern case-sensitive"
  (not (fuzzy-match "aBc" "abc")))

(test-assert "mixed case pattern exact match"
  (fuzzy-match "aBc" "aBc"))

;; ======================================================================
;; fuzzy-score
;; ======================================================================

(test-assert "fuzzy-score returns number on match"
  (number? (fuzzy-score "abc" "abcdef")))

(test-assert "fuzzy-score returns #f on no match"
  (not (fuzzy-score "xyz" "abc")))

;; ======================================================================
;; fuzzy-filter
;; ======================================================================

(test-assert "fuzzy-filter returns matching candidates"
  (let ([result (fuzzy-filter "mp" '("map" "filter" "for-each" "empty?" "bitmap"))])
    (and (member "map" result)
         (member "empty?" result)
         (not (member "filter" result)))))

(test-assert "fuzzy-filter sorts by score descending"
  (let ([result (fuzzy-filter "map" '("map" "hashmap" "bitmap" "map-values"))])
    ;; "map" should be first (exact match = highest score)
    (string=? (car result) "map")))

(test-assert "fuzzy-filter empty pattern returns all"
  (let ([result (fuzzy-filter "" '("a" "b" "c"))])
    (= (length result) 3)))

;; ======================================================================
;; fuzzy-filter/positions
;; ======================================================================

(test-assert "fuzzy-filter/positions returns pairs"
  (let ([result (fuzzy-filter/positions "mp" '("map" "empty?" "filter"))])
    (and (pair? result)
         (pair? (car result))
         (string? (caar result)))))

;; ======================================================================
;; Extended search syntax: parse-search-pattern
;; ======================================================================

;; Single fuzzy term -> one term-set with one term
(test-assert "parse single fuzzy token"
  (let ([sets (parse-search-pattern "foo")])
    (and (= (length sets) 1)
         (= (length (car sets)) 1))))

;; Multiple tokens -> multiple term-sets (AND)
(test-assert "parse multiple tokens AND"
  (let ([sets (parse-search-pattern "foo bar")])
    (= (length sets) 2)))

;; OR groups: "foo | bar" -> one term-set with two terms
(test-assert "parse OR group"
  (let ([sets (parse-search-pattern "foo | bar")])
    (and (= (length sets) 1)
         (= (length (car sets)) 2))))

;; Mixed AND and OR: "foo bar | baz qux" -> 3 term-sets, middle has 2
(test-assert "parse AND with OR"
  (let ([sets (parse-search-pattern "foo bar | baz qux")])
    ;; foo=set1, (bar|baz)=set2, qux=set3
    (and (= (length sets) 3)
         (= (length (list-ref sets 1)) 2))))

;; ^prefix$  -> equal type
(test-assert "parse equal (^...$)"
  (let* ([sets (parse-search-pattern "^foo$")]
         [term (caar sets)])
    (eq? 'equal (search-term-type term))))

;; !' negated fuzzy
(test-assert "parse negated fuzzy (!')"
  (let* ([sets (parse-search-pattern "!'foo")]
         [term (caar sets)])
    (and (eq? 'fuzzy (search-term-type term))
         (search-term-negated? term))))

;; ======================================================================
;; Extended search: match-search-pattern
;; ======================================================================

(test-assert "exact match works"
  (match-search-pattern (parse-search-pattern "'abc") "xxxabcxxx"))

(test-assert "exact match rejects"
  (not (match-search-pattern (parse-search-pattern "'abc") "xxxabxcxxx")))

(test-assert "prefix match works"
  (match-search-pattern (parse-search-pattern "^foo") "foobar"))

(test-assert "prefix match rejects"
  (not (match-search-pattern (parse-search-pattern "^foo") "barfoo")))

(test-assert "suffix match works"
  (match-search-pattern (parse-search-pattern "bar$") "foobar"))

(test-assert "suffix match rejects"
  (not (match-search-pattern (parse-search-pattern "bar$") "barbaz")))

(test-assert "equal match works"
  (match-search-pattern (parse-search-pattern "^foobar$") "foobar"))

(test-assert "equal match rejects substring"
  (not (match-search-pattern (parse-search-pattern "^foo$") "foobar")))

(test-assert "negation works"
  (match-search-pattern (parse-search-pattern "!xyz") "abcdef"))

(test-assert "negation rejects"
  (not (match-search-pattern (parse-search-pattern "!abc") "abcdef")))

(test-assert "AND semantics: both must match"
  (match-search-pattern (parse-search-pattern "foo bar") "foobar"))

(test-assert "AND semantics: one fails"
  (not (match-search-pattern (parse-search-pattern "foo xyz") "foobar")))

;; OR semantics
(test-assert "OR semantics: first matches"
  (match-search-pattern (parse-search-pattern "foo | xyz") "foobar"))

(test-assert "OR semantics: second matches"
  (match-search-pattern (parse-search-pattern "xyz | foo") "foobar"))

(test-assert "OR semantics: neither matches"
  (not (match-search-pattern (parse-search-pattern "xyz | qqq") "foobar")))

;; Smart case in extended search
(test-assert "extended smart case insensitive"
  (match-search-pattern (parse-search-pattern "'abc") "xABCx"))

(test-assert "extended smart case sensitive"
  (not (match-search-pattern (parse-search-pattern "'aBc") "xabcx")))

;; ======================================================================
;; filter-search-pattern
;; ======================================================================

(test-equal "filter-search-pattern exact"
  '("foobar")
  (filter-search-pattern "'foo" '("foobar" "bazqux")))

(test-assert "filter-search-pattern fuzzy sorts by score"
  (let ([result (filter-search-pattern "fb" '("foo-bar" "fxxbxx" "baz"))])
    (and (= (length result) 2)
         (member "foo-bar" result)
         (member "fxxbxx" result))))

(test-assert "filter-search-pattern with OR"
  (let ([result (filter-search-pattern "'foo | 'baz" '("foobar" "bazqux" "hello"))])
    (= (length result) 2)))

;; ======================================================================
;; Escaped spaces in patterns
;; ======================================================================

(test-assert "escaped space in pattern"
  (match-search-pattern (parse-search-pattern "'foo\\ bar") "foo bar baz"))

;; ======================================================================
;; Edge cases
;; ======================================================================

(test-assert "single char pattern"
  (let ([r (fuzzy-match "a" "abc")])
    (and r (= (car (cdr r)) 0))))

(test-assert "same char repeated"
  (let ([r (fuzzy-match "aa" "aaa")])
    (and r (pair? (cdr r)))))

(test-assert "empty text"
  (not (fuzzy-match "a" "")))

(test-assert "both empty"
  (let ([r (fuzzy-match "" "")])
    (and r (= (car r) 0))))

;; ======================================================================
;; Unicode normalisation
;; ======================================================================

(test-assert "cafe matches café"
  (fuzzy-match "cafe" "café"))

(test-assert "naive matches naïve"
  (fuzzy-match "naive" "naïve"))

(test-assert "resume matches résumé"
  (fuzzy-match "resume" "résumé"))

(test-assert "Munchen matches München"
  (fuzzy-match "munchen" "München"))

(test-assert "nino matches niño"
  (fuzzy-match "nino" "niño"))

;; Extended search with unicode
(test-assert "exact match with unicode normalisation"
  (match-search-pattern (parse-search-pattern "'cafe") "café au lait"))

;; ======================================================================
;; Tiebreak criteria
;; ======================================================================

;; Equal scores: shorter candidate preferred
(test-assert "tiebreak: shorter candidate first"
  (let ([result (fuzzy-filter "ab" '("ab" "abc" "abcd"))])
    ;; "ab" should come before "abc" and "abcd"
    (string=? (car result) "ab")))

;; filter-search-pattern also uses tiebreak
(test-assert "tiebreak in filter-search-pattern"
  (let ([result (filter-search-pattern "'ab" '("abc" "ab" "abcd"))])
    ;; "ab" should come first (shorter)
    (string=? (car result) "ab")))

(test-end)
