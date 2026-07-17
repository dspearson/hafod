#!chezscheme
;;; scsh-sre-tools.ss -- Port of scsh/rx/tests/sre-tools-tests.scm
;;; Tests the shallow SRE-form predicates: the runtime sre-form? and the
;;; expand-time if-sre-form macro. The two carry independent copies of the SRE
;;; keyword list, so this suite locks the invariant that they agree, covers the
;;; negatives and the bare char-class symbols, and records where the shallow
;;; predicate and the regexp compiler deliberately disagree.
;;; Original: Copyright (c) Olin Shivers. Port: Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod re)
        (hafod re-adt)
        (except (chezscheme) vector-append))

(test-begin "scsh-sre-tools")

;; `:` is a known SRE form keyword
(test-equal "if-sre-form with SRE form :"
  'yes
  (if-sre-form (: "foo" "bar") 'yes 'no))

;; Quoted symbol is NOT an SRE form (it's just a datum)
(test-equal "if-sre-form with non-SRE quoted symbol"
  'no
  (if-sre-form 'foo 'yes 'no))

;; Test regexp-bind using let-syntax with if-sre-form
(let-syntax ((regexp-bind (syntax-rules ()
                            ((regexp-bind name sre body)
                             (if-sre-form sre
                                          (let ((name (rx sre)))
                                            body)
                                          (if #f #f))))))
  ;; Valid SRE: compile and use
  (test-equal "regexp-bind with valid SRE"
    2
    (regexp-bind s (: "blah" (* digit))
      (regexp-fold s (lambda (i m count) (+ count 1)) 0
                   "blah23 foo bar blah baz233")))

  ;; Invalid SRE: (if #f #f) = void
  (test-assert "regexp-bind with non-SRE returns void"
    (eq? (regexp-bind s blah s) (void))))

;; --- agreement invariant: runtime predicate vs expand-time macro ---
;;
;; sre-form? (runtime) and if-sre-form (expand time) carry two independent
;; copies of the SRE keyword list. They must classify every form the same way,
;; or the two copies have drifted apart. agree? pairs them: it is true exactly
;; when both accept, or both reject, the same form -- so a single dropped or
;; added keyword on either side reddens it.
(define-syntax agree?
  (syntax-rules ()
    ((_ form) (eq? (if-sre-form form #t #f) (sre-form? 'form)))))

(test-assert "sequence form: predicate and macro agree"
  (agree? (: "a" "b")))
(test-assert "alternation form: predicate and macro agree"
  (agree? (or "a" "b")))
(test-assert "bare char-class symbol: predicate and macro agree"
  (agree? digit))
(test-assert "a non-SRE list: predicate and macro agree, both rejecting"
  (agree? (foo bar)))

;; --- negatives ---
;;
;; A bare number, a character, the empty list, and a list whose head is not a
;; keyword are none of them SRE forms. (A character is a valid element inside a
;; set form but is not itself a top-level SRE.)
(test-assert "a number is not an SRE" (not (sre-form? 5)))
(test-assert "a character is not a top-level SRE" (not (sre-form? #\a)))
(test-assert "the empty list is not an SRE" (not (sre-form? '())))
(test-assert "a list with a non-keyword head is not an SRE" (not (sre-form? '(foo bar))))

;; --- bare char-class symbols the parser accepts ---
;;
;; Beyond the compound forms, a family of bare symbols name character classes
;; and are themselves SRE forms. Sweep the whole set uniformly, so a symbol
;; silently dropped from the recognised class list is caught.
(for-each
  (lambda (sym)
    (test-assert (string-append "bare char-class symbol is an SRE: "
                                (symbol->string sym))
      (sre-form? sym)))
  '(any nonl bos eos bol eol digit alpha alnum space hex-digit ascii))

;; --- DELIBERATE DIVERGENCE ---
;;
;; The shallow syntactic predicate accepts an unquote-splicing form, but the
;; regexp compiler has no arm for it and raises. Recognising the form in the
;; predicate matches scsh, whose syntactic check is likewise shallow; supplying
;; the matching compiler arm is a genuinely new capability, deferred to a later
;; release rather than added in this behaviour-preserving one. Both sides are
;; pinned here so the split cannot drift silently: the predicate accepts the
;; form, and the compiler rejects it.
(test-assert "the shallow predicate accepts an unquote-splicing form"
  (sre-form? '(unquote-splicing x)))
(test-error "the compiler has no unquote-splicing arm and raises"
  (sre->regexp '(seq (unquote-splicing (list "a" "b")))))

(test-end)
