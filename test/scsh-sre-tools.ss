#!chezscheme
;;; scsh-sre-tools.ss -- Port of scsh/rx/tests/sre-tools-tests.scm
;;; Tests if-sre-form macro dispatch.
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

(test-end)
