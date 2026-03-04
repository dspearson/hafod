#!chezscheme
;;; scsh-re-procs.ss -- Port of scsh/rx/tests/re-procs-tests.scm
;;; Tests regex procedures: search, match accessors, substitution, fold,
;;; match macros, transformations, and conversions.
;;; Original: Copyright (c) Olin Shivers. Port: Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod re)
        (hafod re-adt)
        (hafod internal char-sets)
        (except (chezscheme) vector-append))

(test-begin "scsh-re-procs")

;; ======================================================================
;; regexp? predicate
;; ======================================================================

(test-assert "regexp? compiled regex"
  (regexp? (rx "foo")))

(test-equal "regexp? string is #f"
  #f
  (regexp? "foo"))

;; ======================================================================
;; regexp-search / regexp-search?
;; ======================================================================

(test-equal "regexp-search no match returns #f"
  #f
  (regexp-search (rx "foo") "bar"))

(test-assert "regexp-search? match"
  (regexp-search? (rx "foo") "foo"))

(test-assert "regexp-search? with start offset"
  (regexp-search? (rx "bar") "foobar" 3))

(test-equal "regexp-search? start past match"
  #f
  (regexp-search? (rx "bar") "foobar" 4))

(test-assert "regexp-search? numeric class"
  (regexp-search? (rx (: (* numeric))) "123"))

(test-assert "regexp-search? bos anchor"
  (regexp-search? (rx (: bos (* numeric))) "123"))

(test-assert "regexp-search? eos anchor"
  (regexp-search? (rx (: (* numeric) eos)) "123"))

(test-assert "regexp-search returns match (and coerce to #t)"
  (and (regexp-search (rx (: (* numeric))) "123") #t))

(test-assert "regexp-search with bos returns match"
  (and (regexp-search (rx (: bos (* numeric))) "123") #t))

(test-assert "regexp-search with eos returns match"
  (and (regexp-search (rx (: (* numeric) eos)) "123") #t))

;; ======================================================================
;; Match accessors
;; ======================================================================

(let ((foobarbaz (rx (: (submatch "foo")
                        (submatch "bar")
                        (submatch "baz")))))

  ;; match:start
  (test-equal "match:start for simple search"
    3
    (match:start (regexp-search (rx "bar") "foobar")))

  (test-equal "match:start submatch 3"
    6
    (match:start (regexp-search foobarbaz "foobarbaz") 3))

  (test-equal "match:start submatch 1"
    0
    (match:start (regexp-search foobarbaz "foobarbaz") 1))

  (test-equal "match:start out-of-range returns #f"
    #f
    (match:start (regexp-search foobarbaz "foobarbaz") 4))

  ;; match:end
  (test-equal "match:end for simple search"
    2
    (match:end (regexp-search (rx "fo") "foobar")))

  (test-equal "match:end submatch 2"
    6
    (match:end (regexp-search foobarbaz "foobarbaz") 2))

  (test-equal "match:end submatch 0 (whole match)"
    9
    (match:end (regexp-search foobarbaz "foobarbaz") 0))

  (test-equal "match:end out-of-range returns #f"
    #f
    (match:end (regexp-search foobarbaz "foobarbaz") 30))

  ;; match:substring
  (test-equal "match:substring submatch 3"
    "baz"
    (match:substring (regexp-search foobarbaz "foobarbaz") 3))

  (test-equal "match:substring submatch 0 (whole match)"
    "foobarbaz"
    (match:substring (regexp-search foobarbaz "foobarbaz") 0))

  (test-equal "match:substring out-of-range returns #f"
    #f
    (match:substring (regexp-search (rx (submatch "foo")) "foo") 2))

  ;; ======================================================================
  ;; regexp-substitute
  ;; ======================================================================

  (test-equal "regexp-substitute reorder submatches"
    "bazbarfoo"
    (regexp-substitute #f (regexp-search foobarbaz "foobarbaz") 3 2 1))

  (test-equal "regexp-substitute pre/post"
    "!bar!"
    (regexp-substitute #f (regexp-search (rx "foo") "!foo!") 'pre "bar" 'post))

  (test-equal "regexp-substitute string replace"
    "bar"
    (regexp-substitute #f (regexp-search (rx "foo") "foo") "bar"))

  (test-equal "regexp-substitute empty"
    ""
    (regexp-substitute #f (regexp-search (rx "foo") "foo")))

  (test-equal "regexp-substitute to port"
    "barbazfoofrob"
    (let ((port (open-output-string)))
      (regexp-substitute port (regexp-search foobarbaz "foobarbaz") 2 3 1 "frob")
      (get-output-string port)))

  (test-equal "regexp-substitute to port pre/post"
    "!bar!"
    (let ((port (open-output-string)))
      (regexp-substitute port (regexp-search (rx "foo") "!foo!") 'pre "bar" 'post)
      (get-output-string port)))

  ;; ======================================================================
  ;; regexp-substitute/global
  ;; ======================================================================

  (test-equal "regexp-substitute/global replace all"
    "bar, bar, bar!"
    (regexp-substitute/global #f (rx "foo") "foo, foo, foo!" 'pre "bar" 'post))

  (test-equal "regexp-substitute/global with submatch"
    "baz"
    (regexp-substitute/global #f foobarbaz "foobarbaz" 3 'post))

  (test-equal "regexp-substitute/global to port"
    "what the !"
    (let ((port (open-output-string)))
      (regexp-substitute/global port (rx "foo") "what the foo!" 'pre 'post)
      (get-output-string port)))

  ;; ======================================================================
  ;; regexp-fold
  ;; ======================================================================

  (test-equal "regexp-fold count occurrences"
    2
    (regexp-fold (rx "foo") (lambda (i m count) (+ count 1))
                 0 "foo, bar, baz, foo, bar"))

  (test-equal "regexp-fold collect with finish"
    '("foo1" "foo2" "foo3")
    (regexp-fold (rx (: "foo" (+ digit)))
                 (lambda (i m list)
                   (cons (match:substring m) list))
                 '() "foo1 foo2 foo3"
                 (lambda (i list) (reverse list))))

  ;; ======================================================================
  ;; regexp-fold-right
  ;; ======================================================================

  (test-equal "regexp-fold-right reversed accumulation"
    '("foo3" "foo2" "foo1")
    (regexp-fold-right (rx (: "foo" (+ digit)))
                       (lambda (m i list)
                         (cons (match:substring m) list))
                       '() "foo1 foo2 foo3"
                       (lambda (i list) (reverse list))))

  ;; ======================================================================
  ;; regexp-for-each
  ;; ======================================================================

  (test-equal "regexp-for-each collect via mutation"
    '("foo" "foo")
    (let ((foos '()))
      (regexp-for-each (rx "foo")
                       (lambda (m)
                         (set! foos (cons (match:substring m) foos)))
                       "blahblahfooblahblahfoo")
      foos))

  ;; ======================================================================
  ;; let-match / if-match / match-cond
  ;; ======================================================================

  (test-equal "let-match binds all submatches"
    '("baz" "bar" "foo" "foobarbaz")
    (let-match (regexp-search foobarbaz "foobarbaz")
               (fbz foo bar baz)
               `(,baz ,bar ,foo ,fbz)))

  (test-equal "if-match no match takes else branch"
    'no-match
    (if-match (regexp-search foobarbaz "")
              (fbz foo bar baz)
              `(,baz ,bar ,foo ,fbz)
              'no-match))

  (test-equal "if-match with match takes then branch"
    "bar"
    (if-match (regexp-search (rx "bar") "bar")
              (bar) bar
              'no-match))

  (test-equal "match-cond no match falls through to else"
    'no-match
    (match-cond ((regexp-search foobarbaz "") (fbz foo bar baz) foo)
                (else 'no-match)))

  (test-equal "match-cond with match"
    "foo"
    (match-cond ((regexp-search foobarbaz "foobarbaz") (fbz foo bar baz) foo)
                (else 'no-match)))

) ; end let for compiled-regex tests

;; ======================================================================
;; Transformations (require RE ADT objects, not compiled regexps)
;; ======================================================================

;; Build foobarbaz as ADT for transformation/conversion tests
(let ((foobarbaz-adt (re-seq (list (re-submatch (make-re-string "foo"))
                                   (re-submatch (make-re-string "bar"))
                                   (re-submatch (make-re-string "baz"))))))

  (test-equal "flush-submatches removes subgroups"
    #f
    (match:substring (regexp-search (flush-submatches foobarbaz-adt) "foobarbaz") 1))

  (test-equal "uncase matches case-insensitively"
    "BaR"
    (match:substring (regexp-search (uncase (sre->regexp '(: "bar"))) "BaR")))

  ;; Note: rx does not support unquote; use uncase-string result as ADT directly
  ;; In scsh: (rx ,(uncase-string "blah")) -- hafod equivalent uses ADT coercion
  (test-equal "uncase-string case-folded search"
    "Blah"
    (match:substring (regexp-search (uncase-string "blah") "Blah")))

  ;; ======================================================================
  ;; Conversions
  ;; ======================================================================

  (test-equal "regexp->sre round-trip"
    '(: (submatch "foo") (submatch "bar") (submatch "baz"))
    (regexp->sre foobarbaz-adt))

  (test-assert "sre->regexp returns regexp"
    (regexp? (sre->regexp '(: "foobar" "baz"))))

  (test-assert "posix-string->regexp matches"
    (regexp-search? (posix-string->regexp "g(ee|oo)se") "goose"))

  (test-equal "regexp->posix-string"
    "g(ee|oo)se"
    (call-with-values
      (lambda () (regexp->posix-string (sre->regexp '(: "g" (or "ee" "oo") "se"))))
      (lambda (string level pcount submatches) string)))

) ; end let for ADT tests

(test-end)
