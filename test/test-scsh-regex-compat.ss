#!chezscheme
;;; test-scsh-regex-compat.ss -- Ported scsh/rx/ regex test material
;;; Requirements: SCSH-02
;;; Copyright (c) 2026 Dominic Pearson.
;;; Original scsh tests: Copyright (c) 1997, 1998 Olin Shivers.

(library-directories '(("src" . "src") ("." . ".")))
(import (hafod re) (hafod re-adt) (hafod compat)
        (except (chezscheme) vector-append)
        (test runner))

(test-begin "scsh Regex Compatibility")

;; =============================================================================
;; Section 1: SCSH-02 -- RE ADT tests (ported from scsh/rx/tests/re-adt-tests.scm)
;; =============================================================================

;; re-string constructors and predicates
(test-assert "re-string?: make-re-string" (re-string? (make-re-string "blah")))
(test-assert "re-string?: make-re-char-set is not" (not (re-string? (make-re-char-set "blah"))))
(test-assert "re-string?: make-re-string #t" (re-string? (make-re-string "blah")))
(test-equal "re-string:chars" "blah" (re-string:chars (make-re-string "blah")))

;; re-char-set constructors and predicates
(test-assert "re-char-set?: make-re-char-set" (re-char-set? (make-re-char-set "blah")))
(test-assert "re-char-set?: make-re-string is not" (not (re-char-set? (make-re-string "blah"))))
(test-assert "re-char-set?: make-re-char-set #t" (re-char-set? (make-re-char-set "blah")))
(test-equal "re-char-set:cset" "blah" (re-char-set:cset (make-re-char-set "blah")))

;; re-seq, re-choice, re-repeat, re-submatch, re-dsm
(let* ((foo (make-re-string "foo"))
       (bar (make-re-string "bar"))
       (baz (make-re-char-set "baz"))
       (bla (make-re-submatch bar)))

  ;; re-seq
  (test-assert "re-seq?: make-re-seq" (re-seq? (make-re-seq (list foo bar))))
  (test-assert "re-seq?: re-seq smart" (re-seq? (re-seq (list bar baz))))
  (test-assert "re-seq?: re-string is not" (not (re-seq? foo)))
  (test-equal "re-seq:elts empty" '() (re-seq:elts (make-re-seq '())))
  (test-equal "re-seq:elts (foo bar)" (list foo bar) (re-seq:elts (make-re-seq (list foo bar))))
  (test-equal "re-seq:tsm (baz bla)" 1 (re-seq:tsm (make-re-seq (list baz bla))))

  ;; re-choice
  (test-assert "re-choice?: make-re-choice" (re-choice? (make-re-choice (list bar baz))))
  (test-assert "re-choice?: re-choice smart" (re-choice? (re-choice (list bar baz))))
  (test-assert "re-choice?: not on baz" (not (re-choice? baz)))
  ;; re-choice with empty list returns re-empty (char-set)
  (test-assert "re-choice empty list is not re-choice" (not (re-choice? (re-choice '()))))
  ;; re-choice with single element simplifies to the element
  (test-assert "re-choice single is not re-choice" (not (re-choice? (re-choice (list baz)))))
  (test-equal "re-choice:elts" (list foo baz) (re-choice:elts (make-re-choice (list foo baz))))
  (test-equal "re-choice:tsm no submatches" 0 (re-choice:tsm (make-re-choice (list foo bar baz))))

  ;; re-repeat
  (test-assert "re-repeat?: make-re-repeat" (re-repeat? (make-re-repeat 0 1 bar)))
  (test-assert "re-repeat?: not on bar" (not (re-repeat? bar)))
  ;; re-repeat smart constructor: from > to returns re-empty
  (test-assert "re-repeat smart: from > to not re-repeat" (not (re-repeat? (re-repeat 10 1 foo))))
  (test-assert "re-repeat smart: 0 10 choice" (re-repeat? (re-repeat 0 10 (re-choice (list foo bar)))))
  (test-equal "re-repeat:from" 10 (re-repeat:from (re-repeat 10 12 baz)))
  (test-equal "re-repeat:to" 5 (re-repeat:to (re-repeat 0 5 bar)))
  (test-equal "re-repeat:tsm" 0 (re-repeat:tsm (re-repeat 0 3 bar)))

  ;; re-submatch
  (test-assert "re-submatch?: make-re-submatch" (re-submatch? (make-re-submatch bar)))
  (test-assert "re-submatch?: not on bar" (not (re-submatch? bar)))
  (test-equal "re-submatch:pre-dsm default" 0 (re-submatch:pre-dsm (re-submatch bar)))
  (test-equal "re-submatch:pre-dsm 2" 2 (re-submatch:pre-dsm (re-submatch bar 2 3)))
  (test-equal "re-submatch:post-dsm default" 0 (re-submatch:post-dsm (re-submatch foo 4)))
  (test-equal "re-submatch:post-dsm 7" 7 (re-submatch:post-dsm (re-submatch foo 4 7)))
  (test-equal "re-submatch:tsm (submatch foo)" 1 (re-submatch:tsm (re-submatch foo)))
  (test-equal "re-submatch:tsm nested" 4 (re-submatch:tsm (re-submatch bla 1 1)))

  ;; re-dsm
  (test-assert "re-dsm?: make-re-dsm" (re-dsm? (make-re-dsm bla 1 0)))
  (test-assert "re-dsm?: re-dsm smart" (re-dsm? (re-dsm bar 2 3)))
  (test-assert "re-dsm?: make-re-submatch is not" (not (re-dsm? (make-re-submatch bar 2 3))))
  (test-equal "re-dsm:body" foo (re-dsm:body (re-dsm foo 0 2)))
  (test-equal "re-dsm:pre-dsm" 2 (re-dsm:pre-dsm (re-dsm foo 2 0)))
  (test-equal "re-dsm:post-dsm" 3 (re-dsm:post-dsm (re-dsm bar 1 3)))
  (test-equal "re-dsm:tsm" 6 (re-dsm:tsm (re-dsm bla 2 3)))

  ;; Anchor singletons
  (test-assert "re-bos? re-bos" (re-bos? re-bos))
  (test-assert "re-bos? bar" (not (re-bos? bar)))
  (test-assert "re-eos? re-eos" (re-eos? re-eos))
  (test-assert "re-eos? foo" (not (re-eos? foo)))
  (test-assert "re-bol? re-bol" (re-bol? re-bol))
  (test-assert "re-bol? bla" (not (re-bol? bla)))
  (test-assert "re-eol? re-eol" (re-eol? re-eol))
  (test-assert "re-eol? baz" (not (re-eol? baz)))

  ;; Special REs
  (test-assert "re-trivial? re-trivial" (re-trivial? re-trivial))
  (test-assert "re-trivial? bla" (not (re-trivial? bla)))
  (test-assert "re-empty? re-empty" (re-empty? re-empty))
  (test-assert "re-empty? bla" (not (re-empty? bla)))
  (test-assert "re-any? re-any" (re-any? re-any))
  (test-assert "re-any? bla" (not (re-any? bla)))

  ;; re-tsm
  (test-equal "re-tsm bla" 1 (re-tsm bla))
  (test-equal "re-tsm foo" 0 (re-tsm foo))
  (test-equal "re-tsm dsm" 8 (re-tsm (re-dsm foo 4 4)))
)

;; =============================================================================
;; Section 2: SCSH-02 -- RE procedure tests (ported from scsh/rx/tests/re-procs-tests.scm)
;; =============================================================================

;; regexp? predicate
(test-assert "regexp? compiled" (regexp? (rx "foo")))
(test-assert "regexp? string is not" (not (regexp? "foo")))

;; regexp-search basic
(test-assert "regexp-search no match" (not (regexp-search (rx "foo") "bar")))
(test-assert "regexp-search? match" (regexp-search? (rx "foo") "foo"))
(test-assert "regexp-search? with start position" (regexp-search? (rx "bar") "foobar" 3))
(test-assert "regexp-search? past match" (not (regexp-search? (rx "bar") "foobar" 4)))

;; Anchored patterns
(test-assert "regexp-search? (* numeric)" (regexp-search? (rx (: (* numeric))) "123"))
(test-assert "regexp-search? bos (* numeric)" (regexp-search? (rx (: bos (* numeric))) "123"))
(test-assert "regexp-search? (* numeric) eos" (regexp-search? (rx (: (* numeric) eos)) "123"))

;; match:start, match:end, match:substring
(let ((foobarbaz (rx (: (submatch "foo") (submatch "bar") (submatch "baz")))))

  (test-equal "match:start whole" 3 (match:start (regexp-search (rx "bar") "foobar")))
  (test-equal "match:start submatch 3" 6 (match:start (regexp-search foobarbaz "foobarbaz") 3))
  (test-equal "match:start submatch 1" 0 (match:start (regexp-search foobarbaz "foobarbaz") 1))
  (test-assert "match:start out of range" (not (match:start (regexp-search foobarbaz "foobarbaz") 4)))
  (test-equal "match:end whole" 2 (match:end (regexp-search (rx "fo") "foobar")))
  (test-equal "match:end submatch 2" 6 (match:end (regexp-search foobarbaz "foobarbaz") 2))
  (test-equal "match:end submatch 0" 9 (match:end (regexp-search foobarbaz "foobarbaz") 0))
  (test-assert "match:end out of range" (not (match:end (regexp-search foobarbaz "foobarbaz") 30)))
  (test-equal "match:substring submatch 3" "baz" (match:substring (regexp-search foobarbaz "foobarbaz") 3))
  (test-equal "match:substring submatch 0" "foobarbaz" (match:substring (regexp-search foobarbaz "foobarbaz") 0))
  (test-assert "match:substring past count" (not (match:substring (regexp-search (rx (submatch "foo")) "foo") 2)))

  ;; regexp-substitute
  (test-equal "regexp-substitute reorder" "bazbarfoo"
    (regexp-substitute #f (regexp-search foobarbaz "foobarbaz") 3 2 1))
  (test-equal "regexp-substitute pre/post" "!bar!"
    (regexp-substitute #f (regexp-search (rx "foo") "!foo!") 'pre "bar" 'post))
  (test-equal "regexp-substitute replace" "bar"
    (regexp-substitute #f (regexp-search (rx "foo") "foo") "bar"))
  (test-equal "regexp-substitute empty" ""
    (regexp-substitute #f (regexp-search (rx "foo") "foo")))

  ;; regexp-substitute to port
  (test-equal "regexp-substitute to port" "barbazfoofrob"
    (let ((port (open-output-string)))
      (regexp-substitute port (regexp-search foobarbaz "foobarbaz") 2 3 1 "frob")
      (get-output-string port)))
  (test-equal "regexp-substitute to port pre/post" "!bar!"
    (let ((port (open-output-string)))
      (regexp-substitute port (regexp-search (rx "foo") "!foo!") 'pre "bar" 'post)
      (get-output-string port)))

  ;; regexp-substitute/global
  (test-equal "regexp-substitute/global multi" "bar, bar, bar!"
    (regexp-substitute/global #f (rx "foo") "foo, foo, foo!" 'pre "bar" 'post))
  (test-equal "regexp-substitute/global with submatch" "baz"
    (regexp-substitute/global #f foobarbaz "foobarbaz" 3 'post))
  (test-equal "regexp-substitute/global to port" "what the !"
    (let ((port (open-output-string)))
      (regexp-substitute/global port (rx "foo") "what the foo!" 'pre 'post)
      (get-output-string port)))

  ;; regexp-fold
  (test-equal "regexp-fold count" 2
    (regexp-fold (rx "foo") (lambda (i m count) (+ count 1)) 0 "foo, bar, baz, foo, bar"))
  (test-equal "regexp-fold collect" '("foo1" "foo2" "foo3")
    (regexp-fold (rx (: "foo" (+ digit)))
                 (lambda (i m list) (cons (match:substring m) list))
                 '() "foo1 foo2 foo3"
                 (lambda (i list) (reverse list))))

  ;; regexp-fold-right
  (test-equal "regexp-fold-right collect" '("foo3" "foo2" "foo1")
    (regexp-fold-right (rx (: "foo" (+ digit)))
                       (lambda (m i list) (cons (match:substring m) list))
                       '() "foo1 foo2 foo3"
                       (lambda (i list) (reverse list))))

  ;; regexp-for-each
  (test-equal "regexp-for-each collect" '("foo" "foo")
    (let ((foos '()))
      (regexp-for-each (rx "foo")
                       (lambda (m) (set! foos (cons (match:substring m) foos)))
                       "blahblahfooblahblahfoo")
      foos))

  ;; let-match
  (test-equal "let-match reorder" '("baz" "bar" "foo" "foobarbaz")
    (let-match (regexp-search foobarbaz "foobarbaz")
      (fbz foo bar baz)
      (list baz bar foo fbz)))

  ;; if-match no match
  (test-equal "if-match no match" 'no-match
    (if-match (regexp-search foobarbaz "")
      (fbz foo bar baz)
      (list baz bar foo fbz)
      'no-match))

  ;; if-match match
  (test-equal "if-match match" "bar"
    (if-match (regexp-search (rx "bar") "bar")
      (bar) bar
      'no-match))

  ;; match-cond no match
  (test-equal "match-cond no match" 'no-match
    (match-cond ((regexp-search foobarbaz "") (fbz foo bar baz) foo)
                (else 'no-match)))

  ;; match-cond match
  (test-equal "match-cond match" "foo"
    (match-cond ((regexp-search foobarbaz "foobarbaz") (fbz foo bar baz) foo)
                (else 'no-match)))

  ;; flush-submatches: needs RE ADT, not compiled regexp
  ;; Build foobarbaz-adt as an RE ADT for tests that require it
  (let ((foobarbaz-adt (re-seq (list (re-submatch (make-re-string "foo"))
                                     (re-submatch (make-re-string "bar"))
                                     (re-submatch (make-re-string "baz"))))))
    (test-assert "flush-submatches removes submatches"
      (not (match:substring (regexp-search (flush-submatches foobarbaz-adt) "foobarbaz") 1)))

    ;; regexp->sre round-trip (RE ADT -> SRE)
    (test-equal "regexp->sre" '(: (submatch "foo") (submatch "bar") (submatch "baz"))
      (regexp->sre foobarbaz-adt)))

  ;; uncase: case-insensitive match (needs RE ADT)
  (test-equal "uncase match" "BaR"
    (match:substring (regexp-search (uncase (make-re-string "bar")) "BaR")))

  ;; uncase-string: returns RE directly
  (test-equal "uncase-string match" "Blah"
    (match:substring (regexp-search (uncase-string "blah") "Blah")))

  ;; sre->regexp
  (test-assert "sre->regexp produces regexp" (regexp? (sre->regexp '(: "foobar" "baz"))))

  ;; posix-string->regexp
  (test-assert "posix-string->regexp goose" (regexp-search? (posix-string->regexp "g(ee|oo)se") "goose"))

  ;; regexp->posix-string (needs RE ADT)
  (test-equal "regexp->posix-string" "g(ee|oo)se"
    (receive (string level pcount submatches)
      (regexp->posix-string (re-seq (list (make-re-string "g")
                                          (re-choice (list (make-re-string "ee")
                                                           (make-re-string "oo")))
                                          (make-re-string "se"))))
      string))
)

;; =============================================================================
;; Section 3: SCSH-02 -- SRE tools tests (ported from scsh/rx/tests/sre-tools-tests.scm)
;; =============================================================================

;; if-sre-form basics
(test-equal "if-sre-form: compound SRE" 'yes
  (if-sre-form (: "foo" "bar") 'yes 'no))

;; Note: sre-tools-tests.scm tests (if-sre-form 'foo ...) which tests a quoted symbol.
;; In hafod, bare unrecognized symbols are not SRE forms.
;; But the test uses a quoted form, so let's check identifier form instead.

;; regexp-bind pattern from sre-tools-tests.scm
(test-equal "if-sre-form: regexp-bind SRE match" 2
  (let-syntax ((regexp-bind (syntax-rules ()
                              ((regexp-bind s sre body)
                               (if-sre-form sre
                                 (let ((s (rx sre)))
                                   body)
                                 (if #f #f))))))
    (regexp-bind s (: "blah" (* digit))
      (regexp-fold s (lambda (i m count) (+ count 1)) 0
                   "blah23 foo bar blah baz233"))))

;; regexp-bind with non-SRE identifier
(test-equal "if-sre-form: regexp-bind non-SRE" (void)
  (let-syntax ((regexp-bind (syntax-rules ()
                              ((regexp-bind s sre body)
                               (if-sre-form sre
                                 (let ((s (rx sre)))
                                   body)
                                 (if #f #f))))))
    (regexp-bind s blah s)))

(test-end)
