#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod awk)
        (hafod re)
        (hafod rdelim)
        (hafod field-reader)
        (hafod compat)
        (except (chezscheme) vector-append record-reader))

(test-begin "AWK Macro")

;; ========== Range helpers ==========

(test-equal "next-:range: basic activation"
  '(#t #t)
  (receive (this next) (next-:range (lambda () #t) (lambda () #f) #f)
    (list this next)))

(test-equal "next-:range: deactivation"
  '(#f #f)
  (receive (this next) (next-:range (lambda () #f) (lambda () #t) #t)
    (list this next)))

(test-equal "next-range: inclusive both"
  '(#f #t)
  (receive (this next) (next-range: (lambda () #t) (lambda () #f) #f)
    (list this next)))

(test-equal "next-range: state active reports active"
  '(#t #t)
  (receive (this next) (next-range: (lambda () #f) (lambda () #f) #t)
    (list this next)))

(test-equal "next-range: stop fires, current inactive"
  '(#f #f)
  (receive (this next) (next-range (lambda () #f) (lambda () #t) #t)
    (list this next)))

(test-equal "next-range: active, no stop"
  '(#t #t)
  (receive (this next) (next-range (lambda () #f) (lambda () #f) #t)
    (list this next)))

(test-equal "next-range: from inactive"
  '(#f #t)
  (receive (this next) (next-range (lambda () #t) (lambda () #f) #f)
    (list this next)))

;; ========== Basic AWK - side effects, no state vars ==========

(test-equal "awk: simple filter with (when) test"
  '("alpha" "also")
  (let ((p (open-input-string "alpha\nbeta\nalso\n"))
        (result '()))
    (awk (read-line p) (line) ()
      ((when (and (> (string-length line) 0)
                  (char=? (string-ref line 0) #\a)))
       (set! result (cons line result))))
    (reverse result)))

(test-equal "awk: no clauses reads until EOF"
  (void)
  (let ((p (open-input-string "a\nb\nc\n")))
    (awk (read-line p) (line) ()
      )))

;; ========== AWK with state variables ==========

(test-equal "awk: count all lines with state var"
  3
  (let ((p (open-input-string "a\nb\nc\n")))
    (awk (read-line p) (line) ((count 0))
      (#t (+ count 1)))))

(test-equal "awk: accumulate with (when) test"
  2
  (let ((p (open-input-string "hello\nworld\nhi\n")))
    (awk (read-line p) (line) ((count 0))
      ((when (> (string-length line) 3)) (+ count 1)))))

(test-equal "awk: two state variables"
  '(2 1)
  (let ((p (open-input-string "yes\nno\nyes\n")))
    (receive (yes-count no-count)
             (awk (read-line p) (line) ((yc 0) (nc 0))
               ((when (string=? line "yes")) (values (+ yc 1) nc))
               ((when (string=? line "no")) (values yc (+ nc 1))))
      (list yes-count no-count))))

;; ========== AWK with SRE pattern matching ==========

(test-equal "awk: SRE string pattern"
  2
  (let ((p (open-input-string "foo bar\nbaz\nfoo qux\n")))
    (awk (read-line p) (line) ((n 0))
      ("foo" (+ n 1)))))

(test-equal "awk: SRE complex pattern"
  1
  (let ((p (open-input-string "abc123\ndef\nghi456\n")))
    (awk (read-line p) (line) ((n 0))
      ((seq "abc" (+ digit)) (+ n 1)))))

;; ========== AWK with integer test (record counter) ==========

(test-equal "awk: integer test matches record number"
  "beta"
  (let ((p (open-input-string "alpha\nbeta\ngamma\n")))
    (awk (read-line p) (line) ((result ""))
      (2 line))))

(test-equal "awk: explicit record counter variable"
  '((1 . "a") (2 . "b") (3 . "c"))
  (let ((p (open-input-string "a\nb\nc\n")))
    (awk (read-line p) (line) counter ((acc '()))
      (#t (cons (cons counter line) acc))
      (after (reverse acc)))))

;; ========== AWK with else clause ==========

(test-equal "awk: else fires when no test matched"
  '(1 2)
  (let ((p (open-input-string "yes\nno\nno\n")))
    (receive (matches non-matches)
             (awk (read-line p) (line) ((m 0) (nm 0))
               ((when (string=? line "yes")) (values (+ m 1) nm))
               (else (values m (+ nm 1))))
      (list matches non-matches))))

(test-equal "awk: else with no state vars (side effects)"
  '("b" "c")
  (let ((p (open-input-string "a\nb\nc\n"))
        (result '()))
    (awk (read-line p) (line) ()
      ((when (string=? line "a")) (void))
      (else (set! result (cons line result))))
    (reverse result)))

;; ========== AWK with after clause ==========

(test-equal "awk: after clause returns final value"
  "Total: 3"
  (let ((p (open-input-string "a\nb\nc\n")))
    (awk (read-line p) (line) ((n 0))
      (#t (+ n 1))
      (after (string-append "Total: " (number->string n))))))

(test-equal "awk: after clause with no state vars"
  "done"
  (let ((p (open-input-string "x\n")))
    (awk (read-line p) (line) ()
      (after "done"))))

;; ========== AWK with => arrow ==========

(test-equal "awk: => arrow with SRE match"
  '("123" "456")
  (let ((p (open-input-string "abc123\ndef\nghi456\n")))
    (awk (read-line p) (line) ((matches '()))
      ((+ digit) => (lambda (m) (cons (match:substring m 0) matches)))
      (after (reverse matches)))))

;; ========== AWK with ==> long-arrow ==========

(test-equal "awk: ==> binds submatch variables"
  '(("x" . "1") ("y" . "2"))
  (let ((p (open-input-string "x=1\ny=2\nplain\n")))
    (awk (read-line p) (line) ((results '()))
      ;; submatch 0 = whole match, 1 = key, 2 = value
      ((seq (submatch (+ alpha)) "=" (submatch (+ any))) ==> (#f key val)
       (cons (cons key val) results))
      (after (reverse results)))))

;; ========== AWK with range clause ==========

(test-equal "awk: :range includes start, excludes stop"
  '("START" "mid1" "mid2")
  (let ((p (open-input-string "before\nSTART\nmid1\nmid2\nEND\nafter\n")))
    (awk (read-line p) (line) ((collected '()))
      (:range (when (string=? line "START"))
              (when (string=? line "END"))
              (cons line collected))
      (after (reverse collected)))))

(test-equal "awk: range: excludes start, includes stop"
  '("mid" "END")
  (let ((p (open-input-string "before\nSTART\nmid\nEND\nafter\n")))
    (awk (read-line p) (line) ((collected '()))
      (range: (when (string=? line "START"))
              (when (string=? line "END"))
              (cons line collected))
      (after (reverse collected)))))

;; ========== AWK with multiple clauses ==========

(test-equal "awk: multiple clauses each fire independently"
  '(2 1)
  (let ((p (open-input-string "hello world\nfoo\nhi there\n")))
    (receive (long-count hello-count)
             (awk (read-line p) (line) ((lc 0) (hc 0))
               ((when (> (string-length line) 5)) (values (+ lc 1) hc))
               ("hello" (values lc (+ hc 1))))
      (list long-count hello-count))))

;; ========== AWK with field vars ==========

(test-equal "awk: field-reader integration"
  '("a:1" "b:2")
  (let ((p (open-input-string "a 1\nb 2\n")))
    (awk (let-values (((r f) ((field-reader) p)))
           (if (eof-object? r) (values r '() '())
               (values r (car f) (cadr f))))
         (line f1 f2) ((result '()))
      (#t (cons (string-append f1 ":" f2) result))
      (after (reverse result)))))

;; ========== Edge cases ==========

(test-equal "awk: empty input"
  0
  (let ((p (open-input-string "")))
    (awk (read-line p) (line) ((n 0))
      (#t (+ n 1)))))

(test-equal "awk: single line"
  1
  (let ((p (open-input-string "hello\n")))
    (awk (read-line p) (line) ((n 0))
      (#t (+ n 1)))))

(test-end)
