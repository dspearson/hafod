#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod re-adt)
        (hafod internal char-sets)
        (except (chezscheme) vector-append))

(test-begin "RE ADT Layer")

;; ======================================================================
;; re-string: constructor, predicate, accessor
;; ======================================================================

(test-assert "make-re-string creates re-string"
  (re-string? (really-make-re-string "hello" #f)))

(test-equal "re-string:chars returns the string"
  "hello"
  (re-string:chars (really-make-re-string "hello" #f)))

(test-assert "make-re-string with non-empty string returns re-string"
  (re-string? (make-re-string "foo")))

(test-equal "make-re-string preserves chars"
  "foo"
  (re-string:chars (make-re-string "foo")))

(test-assert "make-re-string of empty string returns re-trivial"
  (re-trivial? (make-re-string "")))

;; ======================================================================
;; re-char-set: constructor, predicate, accessor
;; ======================================================================

(test-assert "make-re-char-set creates re-char-set"
  (re-char-set? (make-re-char-set char-set:digit)))

(test-assert "re-char-set:cset returns the char-set"
  (char-set? (re-char-set:cset (make-re-char-set char-set:digit))))

(test-assert "re-char-set:cset returns correct set"
  (char-set-contains? (re-char-set:cset (make-re-char-set char-set:digit)) #\5))

;; ======================================================================
;; Singleton anchors: re-bos, re-eos, re-bol, re-eol
;; ======================================================================

(test-assert "re-bos is re-bos?"
  (re-bos? re-bos))

(test-assert "re-eos is re-eos?"
  (re-eos? re-eos))

(test-assert "re-bol is re-bol?"
  (re-bol? re-bol))

(test-assert "re-eol is re-eol?"
  (re-eol? re-eol))

(test-assert "re-bos is not re-eos"
  (not (re-eos? re-bos)))

(test-assert "re-bol is not re-eol"
  (not (re-eol? re-bol)))

;; ======================================================================
;; Singletons: re-trivial, re-empty, re-any, re-nonl
;; ======================================================================

(test-assert "re-trivial is re-trivial?"
  (re-trivial? re-trivial))

(test-assert "re-trivial is a DSM"
  (re-dsm? re-trivial))

(test-assert "re-empty is re-empty?"
  (re-empty? re-empty))

(test-assert "re-empty is a char-set RE"
  (re-char-set? re-empty))

(test-assert "re-empty char-set is empty"
  (char-set-empty? (re-char-set:cset re-empty)))

(test-assert "re-any is re-any?"
  (re-any? re-any))

(test-assert "re-any is a char-set RE"
  (re-char-set? re-any))

(test-assert "re-any matches letter"
  (char-set-contains? (re-char-set:cset re-any) #\a))

(test-assert "re-nonl is a char-set RE"
  (re-char-set? re-nonl))

(test-assert "re-nonl matches a"
  (char-set-contains? (re-char-set:cset re-nonl) #\a))

(test-assert "re-nonl does not match newline"
  (not (char-set-contains? (re-char-set:cset re-nonl) #\newline)))

;; ======================================================================
;; re-seq: constructor, predicate, accessors, TSM
;; ======================================================================

(test-assert "make-re-seq creates re-seq"
  (re-seq? (make-re-seq (list (make-re-string "a") (make-re-string "b")))))

(test-equal "re-seq:elts returns the element list"
  2
  (length (re-seq:elts (make-re-seq (list (make-re-string "a") (make-re-string "b"))))))

(test-equal "re-seq:tsm sums element TSMs (all 0)"
  0
  (re-seq:tsm (make-re-seq (list (make-re-string "a") (make-re-string "b")))))

(test-equal "re-seq:tsm sums with submatches"
  2
  (re-seq:tsm (make-re-seq (list (make-re-submatch (make-re-string "a"))
                                 (make-re-submatch (make-re-string "b"))))))

;; Smart seq constructor
(test-assert "re-seq of empty list returns re-trivial"
  (re-trivial? (re-seq '())))

(test-assert "re-seq of singleton returns the element"
  (re-string? (re-seq (list (make-re-string "a")))))

(test-equal "re-seq of singleton preserves value"
  "a"
  (re-string:chars (re-seq (list (make-re-string "a")))))

(test-assert "re-seq drops trivial elements"
  (re-string? (re-seq (list re-trivial (make-re-string "a") re-trivial))))

(test-assert "re-seq flattens nested seqs"
  (let ((inner (make-re-seq (list (make-re-string "a") (make-re-string "b"))))
        (outer-elts (list (make-re-string "x"))))
    (let ((result (re-seq (list inner (make-re-string "c")))))
      (and (re-seq? result)
           (= 3 (length (re-seq:elts result)))))))

;; ======================================================================
;; re-choice: constructor, predicate, accessors, TSM
;; ======================================================================

(test-assert "make-re-choice creates re-choice for non-char-set elts"
  (re-choice? (make-re-choice (list (make-re-string "abc") (make-re-string "def")))))

(test-assert "make-re-choice merges all-char-set elts"
  (re-char-set? (make-re-choice (list (make-re-char-set char-set:digit)
                                      (make-re-char-set char-set:letter)))))

(test-equal "re-choice:tsm sums with submatches"
  2
  (re-choice:tsm (make-re-choice (list (make-re-submatch (make-re-string "a"))
                                       (make-re-submatch (make-re-string "b"))))))

;; Smart choice constructor
(test-assert "re-choice of empty list returns re-empty"
  (re-empty? (re-choice '())))

(test-assert "re-choice of singleton returns the element"
  (re-string? (re-choice (list (make-re-string "abc")))))

(test-assert "re-choice drops empty elements"
  (re-string? (re-choice (list re-empty (make-re-string "abc") re-empty))))

(test-assert "re-choice flattens nested choices"
  (let ((inner (make-re-choice (list (make-re-string "abc") (make-re-string "def")))))
    (let ((result (re-choice (list inner (make-re-string "ghi")))))
      (and (re-choice? result)
           (= 3 (length (re-choice:elts result)))))))

(test-assert "re-choice merges char-class elements"
  (let ((result (re-choice (list (make-re-char-set char-set:digit)
                                 (make-re-char-set char-set:letter)))))
    (and (re-char-set? result)
         (char-set-contains? (re-char-set:cset result) #\5)
         (char-set-contains? (re-char-set:cset result) #\a))))

;; ======================================================================
;; re-repeat: constructor, predicate, accessors, TSM
;; ======================================================================

(test-assert "make-re-repeat creates re-repeat"
  (re-repeat? (make-re-repeat 0 #f (make-re-string "a"))))

(test-equal "re-repeat:from returns from"
  0
  (re-repeat:from (make-re-repeat 0 #f (make-re-string "a"))))

(test-equal "re-repeat:to returns to (unbounded)"
  #f
  (re-repeat:to (make-re-repeat 0 #f (make-re-string "a"))))

(test-equal "re-repeat:to returns to (bounded)"
  5
  (re-repeat:to (make-re-repeat 2 5 (make-re-string "a"))))

(test-assert "re-repeat:body returns body"
  (re-string? (re-repeat:body (make-re-repeat 0 #f (make-re-string "a")))))

(test-equal "re-repeat:tsm equals body tsm"
  1
  (re-repeat:tsm (make-re-repeat 0 #f (make-re-submatch (make-re-string "a")))))

;; ======================================================================
;; re-submatch: constructor, predicate, accessors, TSM
;; ======================================================================

(test-assert "make-re-submatch creates re-submatch"
  (re-submatch? (make-re-submatch (make-re-string "x"))))

(test-equal "re-submatch:tsm is 1 for simple body"
  1
  (re-submatch:tsm (make-re-submatch (make-re-string "x"))))

(test-equal "re-submatch:pre-dsm defaults to 0"
  0
  (re-submatch:pre-dsm (make-re-submatch (make-re-string "x"))))

(test-equal "re-submatch:post-dsm defaults to 0"
  0
  (re-submatch:post-dsm (make-re-submatch (make-re-string "x"))))

(test-assert "re-submatch:body returns body"
  (re-string? (re-submatch:body (make-re-submatch (make-re-string "x")))))

(test-equal "re-submatch with pre-dsm and post-dsm"
  5
  (re-submatch:tsm (make-re-submatch (make-re-string "x") 2 2)))

(test-equal "re-submatch:post-dsm with dsm args"
  2
  (re-submatch:post-dsm (make-re-submatch (make-re-string "x") 2 2)))

;; Nested submatch TSM
(test-equal "nested submatch TSM counts correctly"
  2
  (re-submatch:tsm
   (make-re-submatch
    (make-re-submatch (make-re-string "inner")))))

;; ======================================================================
;; re-dsm: constructor, predicate, accessors, TSM
;; ======================================================================

(test-assert "make-re-dsm creates re-dsm"
  (re-dsm? (make-re-dsm (make-re-string "x") 1 1)))

(test-equal "re-dsm:tsm is pre + body-tsm + post"
  2
  (re-dsm:tsm (make-re-dsm (make-re-string "x") 1 1)))

(test-equal "re-dsm:pre-dsm returns pre-dsm"
  1
  (re-dsm:pre-dsm (make-re-dsm (make-re-string "x") 1 1)))

(test-equal "re-dsm:post-dsm returns post-dsm"
  1
  (re-dsm:post-dsm (make-re-dsm (make-re-string "x") 1 1)))

(test-assert "re-dsm:body returns body"
  (re-string? (re-dsm:body (make-re-dsm (make-re-string "x") 1 1))))

;; Smart dsm constructor
(test-assert "re-dsm drops trivial DSM (tsm = body-tsm)"
  (re-string? (re-dsm (make-re-string "x") 0 0)))

(test-assert "re-dsm absorbs inner DSM"
  (let ((inner (make-re-dsm (make-re-string "x") 1 1)))
    (let ((result (re-dsm inner 2 2)))
      (and (re-dsm? result)
           (re-string? (re-dsm:body result))))))

;; ======================================================================
;; open-dsm: peel DSM layers
;; ======================================================================

(test-assert "open-dsm on non-DSM returns itself with 0"
  (let-values (((body pre) (open-dsm (make-re-string "x"))))
    (and (re-string? body)
         (= 0 pre))))

(test-assert "open-dsm on DSM peels and accumulates pre-dsm"
  (let-values (((body pre) (open-dsm (make-re-dsm (make-re-string "x") 3 2))))
    (and (re-string? body)
         (= 3 pre))))

(test-assert "open-dsm peels nested DSMs"
  (let* ((inner (make-re-dsm (make-re-string "x") 2 0))
         (outer (make-re-dsm inner 3 0)))
    (let-values (((body pre) (open-dsm outer)))
      (and (re-string? body)
           (= 5 pre)))))

;; ======================================================================
;; re-tsm: generic TSM accessor
;; ======================================================================

(test-equal "re-tsm of re-string is 0"
  0
  (re-tsm (make-re-string "foo")))

(test-equal "re-tsm of re-char-set is 0"
  0
  (re-tsm (make-re-char-set char-set:digit)))

(test-equal "re-tsm of re-bos is 0"
  0
  (re-tsm re-bos))

(test-equal "re-tsm of re-eos is 0"
  0
  (re-tsm re-eos))

(test-equal "re-tsm of re-bol is 0"
  0
  (re-tsm re-bol))

(test-equal "re-tsm of re-eol is 0"
  0
  (re-tsm re-eol))

(test-equal "re-tsm of re-submatch"
  1
  (re-tsm (make-re-submatch (make-re-string "x"))))

(test-equal "re-tsm of re-seq with submatches"
  2
  (re-tsm (make-re-seq (list (make-re-submatch (make-re-string "a"))
                             (make-re-submatch (make-re-string "b"))))))

(test-equal "re-tsm of re-dsm"
  3
  (re-tsm (make-re-dsm (make-re-string "x") 1 2)))

(test-equal "re-tsm of re-repeat"
  1
  (re-tsm (make-re-repeat 0 #f (make-re-submatch (make-re-string "a")))))

;; ======================================================================
;; regexp?: generic RE predicate
;; ======================================================================

(test-assert "regexp? of re-string" (regexp? (make-re-string "x")))
(test-assert "regexp? of re-char-set" (regexp? (make-re-char-set char-set:digit)))
(test-assert "regexp? of re-bos" (regexp? re-bos))
(test-assert "regexp? of re-eos" (regexp? re-eos))
(test-assert "regexp? of re-bol" (regexp? re-bol))
(test-assert "regexp? of re-eol" (regexp? re-eol))
(test-assert "regexp? of re-seq" (regexp? (make-re-seq (list (make-re-string "a")))))
(test-assert "regexp? of re-choice" (regexp? (make-re-choice (list (make-re-string "a") (make-re-string "b")))))
(test-assert "regexp? of re-repeat" (regexp? (make-re-repeat 0 #f (make-re-string "a"))))
(test-assert "regexp? of re-submatch" (regexp? (make-re-submatch (make-re-string "x"))))
(test-assert "regexp? of re-dsm" (regexp? (make-re-dsm (make-re-string "x") 1 1)))
(test-assert "regexp? of re-trivial" (regexp? re-trivial))
(test-assert "regexp? of re-empty" (regexp? re-empty))
(test-assert "regexp? of re-any" (regexp? re-any))
(test-assert "regexp? false for integer" (not (regexp? 42)))
(test-assert "regexp? false for string" (not (regexp? "hello")))
(test-assert "regexp? false for #f" (not (regexp? #f)))

;; ======================================================================
;; flush-submatches: strip all submatch nodes
;; ======================================================================

(test-equal "flush-submatches of leaf returns 0 tsm"
  0
  (re-tsm (flush-submatches (make-re-string "x"))))

(test-equal "flush-submatches of submatch returns 0 tsm"
  0
  (re-tsm (flush-submatches (make-re-submatch (make-re-string "x")))))

(test-assert "flush-submatches of submatch extracts body"
  (re-string? (flush-submatches (make-re-submatch (make-re-string "x")))))

(test-equal "flush-submatches of seq with submatches"
  0
  (re-tsm (flush-submatches
           (make-re-seq (list (make-re-submatch (make-re-string "a"))
                              (make-re-submatch (make-re-string "b")))))))

(test-equal "flush-submatches of repeat with submatch"
  0
  (re-tsm (flush-submatches
           (make-re-repeat 0 #f (make-re-submatch (make-re-string "a"))))))

(test-equal "flush-submatches of choice with submatches"
  0
  (re-tsm (flush-submatches
           (make-re-choice (list (make-re-submatch (make-re-string "a"))
                                 (make-re-submatch (make-re-string "b")))))))

(test-equal "flush-submatches of DSM"
  0
  (re-tsm (flush-submatches (make-re-dsm (make-re-string "x") 2 3))))

;; ======================================================================
;; uncase-char-set: case-insensitive char-set
;; ======================================================================

(test-assert "uncase-char-set of lowercase includes uppercase"
  (let ((result (uncase-char-set (char-set #\a))))
    (and (re-char-set? result)
         (char-set-contains? (re-char-set:cset result) #\a)
         (char-set-contains? (re-char-set:cset result) #\A))))

(test-assert "uncase-char-set of uppercase includes lowercase"
  (let ((result (uncase-char-set (char-set #\B))))
    (and (re-char-set? result)
         (char-set-contains? (re-char-set:cset result) #\b)
         (char-set-contains? (re-char-set:cset result) #\B))))

(test-assert "uncase-char-set of digit stays same"
  (let ((result (uncase-char-set char-set:digit)))
    (and (re-char-set? result)
         (char-set-contains? (re-char-set:cset result) #\5)
         (not (char-set-contains? (re-char-set:cset result) #\a)))))

;; ======================================================================
;; uncase-string: case-insensitive string matching RE
;; ======================================================================

(test-assert "uncase-string of all-lowercase produces non-string RE"
  (let ((result (uncase-string "abc")))
    (not (re-string? result))))

(test-assert "uncase-string of digits stays as string"
  (re-string? (uncase-string "123")))

(test-assert "uncase-string preserves digits"
  (equal? "123" (re-string:chars (uncase-string "123"))))

;; ======================================================================
;; uncase: case-insensitive RE transformation
;; ======================================================================

(test-assert "uncase of re-string with alpha expands"
  (not (re-string? (uncase (make-re-string "abc")))))

(test-assert "uncase of re-string with digits unchanged"
  (re-string? (uncase (make-re-string "123"))))

(test-assert "uncase of re-char-set expands cases"
  (let ((result (uncase (make-re-char-set (char-set #\a)))))
    (and (re-char-set? result)
         (char-set-contains? (re-char-set:cset result) #\A))))

(test-assert "uncase recurses into re-seq"
  (let* ((s (make-re-seq (list (make-re-string "a") (make-re-string "1"))))
         (result (uncase s)))
    (re-seq? result)))

(test-assert "uncase recurses into re-submatch"
  (let* ((sm (make-re-submatch (make-re-string "abc")))
         (result (uncase sm)))
    (re-submatch? result)))

;; ======================================================================
;; re-char-class? and static-char-class?
;; ======================================================================

(test-assert "re-char-class? for char-set RE"
  (re-char-class? (make-re-char-set char-set:digit)))

(test-assert "re-char-class? for single-char string RE"
  (re-char-class? (make-re-string "a")))

(test-assert "re-char-class? false for multi-char string"
  (not (re-char-class? (make-re-string "abc"))))

(test-assert "static-char-class? for char-set RE"
  (static-char-class? (make-re-char-set char-set:digit)))

(test-assert "static-char-class? for single-char string RE"
  (static-char-class? (make-re-string "a")))

;; ======================================================================
;; Smart repeat constructor
;; ======================================================================

(test-assert "re-repeat {1,1} reduces to body"
  (let ((result (re-repeat 1 1 (make-re-string "a"))))
    (or (re-string? result)
        (and (re-dsm? result)
             (let-values (((body pre) (open-dsm result)))
               (re-string? body))))))

(test-assert "re-repeat {0,0} reduces to empty-string equivalent"
  (let ((result (re-repeat 0 0 (make-re-string "a"))))
    ;; Result should be equivalent to matching empty string:
    ;; either re-trivial, or the raw empty-string RE, or a DSM wrapping one
    (or (re-trivial? result)
        (and (re-string? result) (string=? "" (re-string:chars result)))
        (and (re-dsm? result)
             (let-values (((body pre) (open-dsm result)))
               (or (re-trivial? body)
                   (and (re-string? body) (string=? "" (re-string:chars body)))))))))

;; ======================================================================
;; TSM integration: complex nesting
;; ======================================================================

(test-equal "complex nesting TSM"
  3
  (re-tsm
   (make-re-seq
    (list (make-re-submatch (make-re-string "a"))
          (make-re-submatch
           (make-re-seq
            (list (make-re-string "b")
                  (make-re-submatch (make-re-string "c")))))))))

(test-end)
