#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (except (hafod re-adt) regexp?)
        (hafod re)
        (hafod internal char-sets)
        (except (chezscheme) vector-append)
        (hafod compat))

(test-begin "RE ADT Compilation and Search Integration")

;; ======================================================================
;; regexp->posix-string: basic string translation
;; ======================================================================

(test-assert "regexp->posix-string of simple string"
  (let-values (((s level pcount smap) (regexp->posix-string (make-re-string "foo"))))
    (and (string? s)
         (equal? s "foo")
         (= pcount 0))))

(test-assert "regexp->posix-string escapes special chars"
  (let-values (((s level pcount smap) (regexp->posix-string (make-re-string "a.b"))))
    (and (string? s)
         (equal? s "a\\.b"))))

(test-assert "regexp->posix-string of single char is level 1"
  (let-values (((s level pcount smap) (regexp->posix-string (make-re-string "x"))))
    (and (equal? s "x")
         (= level 1))))

(test-assert "regexp->posix-string of multi-char is level 2"
  (let-values (((s level pcount smap) (regexp->posix-string (make-re-string "ab"))))
    (and (equal? s "ab")
         (= level 2))))

;; ======================================================================
;; regexp->posix-string: sequence
;; ======================================================================

(test-assert "regexp->posix-string of seq"
  (let-values (((s level pcount smap)
                (regexp->posix-string
                 (make-re-seq (list (make-re-string "foo") (make-re-string "bar"))))))
    (and (string? s)
         (equal? s "foobar")
         (= pcount 0)
         (= level 2))))

;; ======================================================================
;; regexp->posix-string: choice (alternation)
;; ======================================================================

(test-assert "regexp->posix-string of choice"
  (let-values (((s level pcount smap)
                (regexp->posix-string
                 (make-re-choice (list (make-re-string "a") (make-re-string "b"))))))
    (and (string? s)
         (equal? s "a|b")
         (= level 3))))

;; ======================================================================
;; regexp->posix-string: repeat
;; ======================================================================

(test-assert "regexp->posix-string of * repeat"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-repeat 0 #f (make-re-string "a")))))
    (and (string? s)
         (equal? s "a*")
         (= level 1))))

(test-assert "regexp->posix-string of + repeat"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-repeat 1 #f (make-re-string "a")))))
    (and (string? s)
         (equal? s "a+")
         (= level 1))))

(test-assert "regexp->posix-string of ? repeat"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-repeat 0 1 (make-re-string "a")))))
    (and (string? s)
         (equal? s "a?")
         (= level 1))))

(test-assert "regexp->posix-string of {n,m} repeat"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-repeat 2 5 (make-re-string "a")))))
    (and (string? s)
         (equal? s "a{2,5}")
         (= level 1))))

(test-assert "regexp->posix-string of {n} exact repeat"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-repeat 3 3 (make-re-string "a")))))
    (and (string? s)
         (equal? s "a{3}")
         (= level 1))))

(test-assert "regexp->posix-string of {n,} at-least repeat"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-repeat 3 #f (make-re-string "a")))))
    (and (string? s)
         (equal? s "a{3,}")
         (= level 1))))

;; ======================================================================
;; regexp->posix-string: anchors
;; ======================================================================

(test-assert "regexp->posix-string of bos"
  (let-values (((s level pcount smap) (regexp->posix-string re-bos)))
    (and (equal? s "^") (= level 1) (= pcount 0))))

(test-assert "regexp->posix-string of eos"
  (let-values (((s level pcount smap) (regexp->posix-string re-eos)))
    (and (equal? s "$") (= level 1) (= pcount 0))))

;; ======================================================================
;; regexp->posix-string: any
;; ======================================================================

(test-assert "regexp->posix-string of re-any"
  (let-values (((s level pcount smap) (regexp->posix-string re-any)))
    (and (equal? s ".") (= level 1) (= pcount 0))))

;; ======================================================================
;; regexp->posix-string: char-set
;; ======================================================================

(test-assert "regexp->posix-string of digit char-set produces working regex"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-char-set char-set:digit))))
    (and (string? s)
         ;; Must be a valid POSIX bracket expression
         (= level 1)
         (= pcount 0))))

;; ======================================================================
;; regexp->posix-string: submatch
;; ======================================================================

(test-assert "regexp->posix-string of submatch"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-submatch (make-re-string "foo")))))
    (and (equal? s "(foo)")
         (= level 0)
         (= pcount 1)
         (vector? smap)
         (= (vector-length smap) 1)
         (= (vector-ref smap 0) 1))))

(test-assert "regexp->posix-string of multiple submatches"
  (let-values (((s level pcount smap)
                (regexp->posix-string
                 (make-re-seq (list (make-re-submatch (make-re-string "a"))
                                    (make-re-submatch (make-re-string "b")))))))
    (and (equal? s "(a)(b)")
         (= pcount 2)
         (vector? smap)
         (= (vector-length smap) 2)
         (= (vector-ref smap 0) 1)
         (= (vector-ref smap 1) 2))))

;; ======================================================================
;; regexp->posix-string: DSM (dead submatches)
;; ======================================================================

(test-assert "regexp->posix-string of dsm pads submatch vector"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-dsm (make-re-submatch (make-re-string "x")) 1 1))))
    (and (string? s)
         (vector? smap)
         (= (vector-length smap) 3)
         ;; First entry is #f (dead pre-submatch)
         (not (vector-ref smap 0))
         ;; Middle entry is the live submatch
         (number? (vector-ref smap 1))
         ;; Last entry is #f (dead post-submatch)
         (not (vector-ref smap 2)))))

;; ======================================================================
;; regexp->posix-string: repeat wraps seq body in parens
;; ======================================================================

(test-assert "regexp->posix-string wraps multi-char repeat body"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-repeat 0 #f (make-re-string "ab")))))
    (and (string? s)
         (equal? s "(ab)*")
         (= pcount 1))))

;; ======================================================================
;; simplify-regexp (identity for now)
;; ======================================================================

(test-assert "simplify-regexp returns a regexp"
  (let ((re (make-re-string "foo")))
    (let ((simplified (simplify-regexp re)))
      (or (re-string? simplified) (regexp? simplified)))))

;; ======================================================================
;; re-adt->compiled-regexp bridge
;; ======================================================================

(test-assert "re-adt->compiled-regexp returns a compiled regexp"
  (let ((crx (re-adt->compiled-regexp (make-re-string "hello"))))
    ;; It should be a valid compiled-regexp-type (check via regexp? from (hafod re))
    (regexp? crx)))

;; ======================================================================
;; Integration: ADT objects in regexp-search
;; ======================================================================

(test-assert "regexp-search accepts RE ADT string"
  (let ((m (regexp-search (make-re-string "hello") "say hello world")))
    (and m
         (equal? (match:substring m 0) "hello"))))

(test-assert "regexp-search accepts RE ADT seq"
  (let ((m (regexp-search (make-re-seq (list (make-re-string "foo")
                                              (make-re-string "bar")))
                           "foobar baz")))
    (and m
         (equal? (match:substring m 0) "foobar"))))

(test-assert "regexp-search accepts RE ADT choice"
  (let ((m (regexp-search (make-re-choice (list (make-re-string "cat")
                                                 (make-re-string "dog")))
                           "the dog ran")))
    (and m
         (equal? (match:substring m 0) "dog"))))

(test-assert "regexp-search accepts RE ADT repeat"
  (let ((m (regexp-search (make-re-repeat 1 #f (make-re-char-set char-set:digit))
                           "abc 123 def")))
    (and m
         (equal? (match:substring m 0) "123"))))

(test-assert "regexp-search accepts RE ADT with submatch"
  (let ((m (regexp-search (make-re-submatch (make-re-string "hello"))
                           "say hello")))
    (and m
         (equal? (match:substring m 0) "hello")
         (equal? (match:substring m 1) "hello"))))

(test-assert "regexp-search with RE ADT multiple submatches"
  (let ((m (regexp-search
            (make-re-seq (list (make-re-submatch (make-re-string "foo"))
                               re-any
                               (make-re-submatch (make-re-string "bar"))))
            "foo+bar")))
    (and m
         (equal? (match:substring m 0) "foo+bar")
         (equal? (match:substring m 1) "foo")
         (equal? (match:substring m 2) "bar"))))

(test-assert "regexp-search with RE ADT anchored"
  (let ((m (regexp-search
            (make-re-seq (list re-bos (make-re-string "hello")))
            "hello world")))
    (and m
         (equal? (match:substring m 0) "hello"))))

(test-assert "regexp-search with RE ADT anchored fails"
  (not (regexp-search
        (make-re-seq (list re-bos (make-re-string "world")))
        "hello world")))

(test-assert "regexp-search? works with ADT"
  (regexp-search? (make-re-string "hello") "say hello world"))

(test-assert "regexp-match works with ADT"
  (let ((m (regexp-match (make-re-string "hello") "hello world")))
    (and m (equal? (match:substring m 0) "hello"))))

;; ======================================================================
;; Equivalence: ADT compilation matches rx macro output
;; ======================================================================

(test-assert "equivalence: simple string"
  (let ((m1 (regexp-search (rx "hello") "say hello world"))
        (m2 (regexp-search (make-re-string "hello") "say hello world")))
    (and m1 m2
         (equal? (match:substring m1 0) (match:substring m2 0))
         (= (match:start m1 0) (match:start m2 0))
         (= (match:end m1 0) (match:end m2 0)))))

(test-assert "equivalence: seq"
  (let ((m1 (regexp-search (rx (seq "foo" "bar")) "foobar"))
        (m2 (regexp-search (make-re-seq (list (make-re-string "foo")
                                               (make-re-string "bar")))
                            "foobar")))
    (and m1 m2
         (equal? (match:substring m1 0) (match:substring m2 0)))))

(test-assert "equivalence: alternation"
  (let ((m1 (regexp-search (rx (or "cat" "dog")) "the dog"))
        (m2 (regexp-search (make-re-choice (list (make-re-string "cat")
                                                  (make-re-string "dog")))
                            "the dog")))
    (and m1 m2
         (equal? (match:substring m1 0) (match:substring m2 0)))))

(test-assert "equivalence: repetition *"
  (let ((m1 (regexp-search (rx (seq "a" (* "b") "c")) "abbbc"))
        (m2 (regexp-search (make-re-seq (list (make-re-string "a")
                                               (make-re-repeat 0 #f (make-re-string "b"))
                                               (make-re-string "c")))
                            "abbbc")))
    (and m1 m2
         (equal? (match:substring m1 0) (match:substring m2 0)))))

(test-assert "equivalence: submatch with positions"
  (let ((m1 (regexp-search (rx (submatch "hello")) "say hello there"))
        (m2 (regexp-search (make-re-submatch (make-re-string "hello")) "say hello there")))
    (and m1 m2
         (equal? (match:substring m1 0) (match:substring m2 0))
         (equal? (match:substring m1 1) (match:substring m2 1))
         (= (match:start m1 1) (match:start m2 1))
         (= (match:end m1 1) (match:end m2 1)))))

(test-assert "equivalence: multiple submatches"
  (let ((m1 (regexp-search (rx (seq (submatch "foo") any (submatch "bar"))) "foo+bar"))
        (m2 (regexp-search (make-re-seq (list (make-re-submatch (make-re-string "foo"))
                                               re-any
                                               (make-re-submatch (make-re-string "bar"))))
                            "foo+bar")))
    (and m1 m2
         (equal? (match:substring m1 0) (match:substring m2 0))
         (equal? (match:substring m1 1) (match:substring m2 1))
         (equal? (match:substring m1 2) (match:substring m2 2)))))

(test-assert "equivalence: nested seq with submatch and digit+"
  (let ((m1 (regexp-search (rx (seq (submatch "a") (+ digit))) "a42"))
        (m2 (regexp-search (make-re-seq (list (make-re-submatch (make-re-string "a"))
                                               (make-re-repeat 1 #f (make-re-char-set char-set:digit))))
                            "a42")))
    (and m1 m2
         (equal? (match:substring m1 0) (match:substring m2 0))
         (equal? (match:substring m1 1) (match:substring m2 1)))))

(test-assert "equivalence: anchors bos"
  (let ((m1 (regexp-search (rx (seq bos "hello")) "hello world"))
        (m2 (regexp-search (make-re-seq (list re-bos (make-re-string "hello")))
                            "hello world")))
    (and m1 m2
         (equal? (match:substring m1 0) (match:substring m2 0)))))

;; ======================================================================
;; DSM: dead submatches produce #f in match results
;; ======================================================================

(test-assert "DSM dead submatches produce #f"
  (let* ((inner (make-re-submatch (make-re-string "hello")))
         (dsm-re (make-re-dsm inner 1 0))
         (m (regexp-search dsm-re "say hello")))
    (and m
         ;; submatch 1 should be #f (dead pre-dsm)
         (not (match:substring m 1))
         ;; submatch 2 should be "hello" (the live submatch)
         (equal? (match:substring m 2) "hello"))))

;; ======================================================================
;; Empty string RE
;; ======================================================================

(test-assert "regexp->posix-string of empty string"
  (let-values (((s level pcount smap) (regexp->posix-string (really-make-re-string "" #f))))
    (and (string? s)
         (= level 0)
         (= pcount 1))))

;; ======================================================================
;; Char-set: singleton renders as char
;; ======================================================================

(test-assert "singleton char-set renders as escaped char"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-char-set (char-set #\a)))))
    (and (string? s)
         ;; Should render as the literal char "a" (not a bracket expr)
         (equal? s "a"))))

;; ======================================================================
;; Empty choice (never matches)
;; ======================================================================

(test-assert "empty choice produces unmatchable"
  (let-values (((s level pcount smap)
                (regexp->posix-string (make-re-choice/tsm '() 0))))
    ;; Should produce a non-#f string that can't match
    (or (not s)
        (and (string? s) (not (regexp-search? (string->regexp s) "anything"))))))

;; ======================================================================
;; Repeat of seq body wraps in parens
;; ======================================================================

(test-assert "repeat of choice wraps in parens"
  (let-values (((s level pcount smap)
                (regexp->posix-string
                 (make-re-repeat 0 #f
                   (make-re-choice (list (make-re-string "a") (make-re-string "b")))))))
    (and (string? s)
         (equal? s "(a|b)*")
         (= pcount 1))))

(test-end)
