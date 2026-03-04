#!chezscheme
;;; scsh-re-adt.ss -- Port of scsh/rx/tests/re-adt-tests.scm
;;; Tests RE ADT: constructors, predicates, accessors, singletons, tsm tracking.
;;; Original: Copyright (c) Olin Shivers. Port: Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod re-adt)
        (hafod internal char-sets)
        (except (chezscheme) vector-append))

(test-begin "scsh-re-adt")

;; ======================================================================
;; re-string
;; ======================================================================

(test-assert "re-string? make-re-string"
  (re-string? (make-re-string "blah")))

(test-equal "re-string? make-re-char-set is #f"
  #f
  (re-string? (make-re-char-set "blah")))

;; Note: scsh (re-string "blah") is the smart constructor; hafod uses make-re-string
(test-assert "re-string? via make-re-string"
  (re-string? (make-re-string "blah")))

(test-equal "re-string:chars"
  "blah"
  (re-string:chars (make-re-string "blah")))

;; ======================================================================
;; re-char-set
;; ======================================================================

(test-assert "re-char-set? make-re-char-set"
  (re-char-set? (make-re-char-set "blah")))

(test-equal "re-char-set? make-re-string is #f"
  #f
  (re-char-set? (make-re-string "blah")))

(test-assert "re-char-set? via make-re-char-set"
  (re-char-set? (make-re-char-set "blah")))

(test-equal "re-char-set:cset"
  "blah"
  (re-char-set:cset (make-re-char-set "blah")))

;; ======================================================================
;; With shared bindings (matching scsh test structure)
;; ======================================================================

(let* ((foo (make-re-string "foo"))
       (bar (make-re-string "bar"))
       (baz (make-re-char-set "baz"))
       (bla (make-re-submatch bar)))

  ;; ---- re-seq ----
  (test-assert "re-seq? make-re-seq"
    (re-seq? (make-re-seq (list foo bar))))

  (test-assert "re-seq? re-seq smart"
    (re-seq? (re-seq (list bar baz))))

  (test-equal "re-seq? string is #f"
    #f (re-seq? foo))

  (test-equal "re-seq:elts empty"
    '() (re-seq:elts (make-re-seq '())))

  (test-assert "re-seq:elts elements"
    (equal? (re-seq:elts (make-re-seq (list foo bar))) (list foo bar)))

  (test-equal "re-seq:tsm with submatch"
    1 (re-seq:tsm (make-re-seq (list baz bla))))

  ;; ---- re-choice ----
  (test-assert "re-choice? make-re-choice"
    (re-choice? (make-re-choice (list bar baz))))

  (test-assert "re-choice? re-choice smart"
    (re-choice? (re-choice (list bar baz))))

  (test-equal "re-choice? char-set is #f"
    #f (re-choice? baz))

  ;; Smart constructor collapses empty to re-empty (not re-choice)
  (test-equal "re-choice? empty is #f"
    #f (re-choice? (re-choice '())))

  ;; Smart constructor collapses singleton
  (test-equal "re-choice? singleton is #f"
    #f (re-choice? (re-choice (list baz))))

  (test-assert "re-choice:elts"
    (equal? (re-choice:elts (make-re-choice (list foo baz))) (list foo baz)))

  (test-equal "re-choice:tsm all zero-tsm"
    0 (re-choice:tsm (make-re-choice (list foo bar baz))))

  ;; ---- re-repeat ----
  (test-assert "re-repeat? make-re-repeat"
    (re-repeat? (make-re-repeat 0 1 bar)))

  (test-equal "re-repeat? string is #f"
    #f (re-repeat? bar))

  ;; Smart constructor: from > to yields re-empty, not re-repeat
  (test-equal "re-repeat? from>to is #f"
    #f (re-repeat? (re-repeat 10 1 foo)))

  (test-assert "re-repeat? valid range"
    (re-repeat? (re-repeat 0 10 (re-choice (list foo bar)))))

  (test-equal "re-repeat:from"
    10 (re-repeat:from (re-repeat 10 12 baz)))

  (test-equal "re-repeat:to"
    5 (re-repeat:to (re-repeat 0 5 bar)))

  (test-equal "re-repeat:tsm zero"
    0 (re-repeat:tsm (re-repeat 0 3 bar)))

  ;; ---- re-submatch ----
  (test-assert "re-submatch? make-re-submatch"
    (re-submatch? (make-re-submatch bar)))

  (test-equal "re-submatch? string is #f"
    #f (re-submatch? bar))

  (test-equal "re-submatch:pre-dsm default 0"
    0 (re-submatch:pre-dsm (re-submatch bar)))

  (test-equal "re-submatch:pre-dsm explicit"
    2 (re-submatch:pre-dsm (re-submatch bar 2 3)))

  (test-equal "re-submatch:post-dsm default 0"
    0 (re-submatch:post-dsm (re-submatch foo 4)))

  (test-equal "re-submatch:post-dsm explicit"
    7 (re-submatch:post-dsm (re-submatch foo 4 7)))

  (test-equal "re-submatch:tsm default"
    1 (re-submatch:tsm (re-submatch foo)))

  ;; bla.tsm=1, + 1(self) + 1(pre) + 1(post) = 4
  (test-equal "re-submatch:tsm complex"
    4 (re-submatch:tsm (re-submatch bla 1 1)))

  ;; ---- re-dsm ----
  (test-assert "re-dsm? make-re-dsm"
    (re-dsm? (make-re-dsm bla 1 0)))

  (test-assert "re-dsm? re-dsm smart"
    (re-dsm? (re-dsm bar 2 3)))

  (test-equal "re-dsm? make-re-submatch is #f"
    #f (re-dsm? (make-re-submatch bar 2 3)))

  (test-assert "re-dsm:body"
    (eq? foo (re-dsm:body (re-dsm foo 0 2))))

  (test-equal "re-dsm:pre-dsm"
    2 (re-dsm:pre-dsm (re-dsm foo 2 0)))

  (test-equal "re-dsm:post-dsm"
    3 (re-dsm:post-dsm (re-dsm bar 1 3)))

  ;; bla.tsm=1, pre=2, post=3 => 2+3+1=6
  (test-equal "re-dsm:tsm"
    6 (re-dsm:tsm (re-dsm bla 2 3)))

  ;; ---- Singletons ----
  (test-assert "re-bos? positive" (re-bos? re-bos))
  (test-equal "re-bos? negative" #f (re-bos? bar))
  (test-assert "re-eos? positive" (re-eos? re-eos))
  (test-equal "re-eos? negative" #f (re-eos? foo))
  (test-assert "re-bol? positive" (re-bol? re-bol))
  (test-equal "re-bol? negative" #f (re-bol? bla))
  (test-assert "re-eol? positive" (re-eol? re-eol))
  (test-equal "re-eol? negative" #f (re-eol? baz))
  (test-assert "re-trivial? positive" (re-trivial? re-trivial))
  (test-equal "re-trivial? negative" #f (re-trivial? bla))
  (test-assert "re-empty? positive" (re-empty? re-empty))
  (test-equal "re-empty? negative" #f (re-empty? bla))
  (test-assert "re-any? positive" (re-any? re-any))
  (test-equal "re-any? negative" #f (re-any? bla))

  ;; ---- re-tsm ----
  (test-equal "re-tsm submatch" 1 (re-tsm bla))
  (test-equal "re-tsm string" 0 (re-tsm foo))
  ;; dsm foo with pre=4, post=4 => 4+4+0 = 8
  (test-equal "re-tsm dsm" 8 (re-tsm (re-dsm foo 4 4)))
)

(test-end)
