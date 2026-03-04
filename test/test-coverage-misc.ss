;;; Gap-fill tests for remaining misc library symbols (re, syntax, environment, awk)
;;; Phase 24 Plan 03 Task 2
;;; Copyright (c) 2026 Dominic Pearson.
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod re)
        (hafod syntax)
        (hafod environment)
        (hafod awk)
        (hafod compat)
        (hafod rdelim)
        (hafod field-reader)
        (except (chezscheme) vector-append open-input-file open-output-file getenv))

(test-begin "coverage-misc")

;; ======================================================================
;; (hafod re) -- 2 untested symbols
;; ======================================================================

;; make-regexp: takes (posix-str cflags nsub) or (posix-str cflags nsub smap)
(test-assert "make-regexp creates a regexp"
  (regexp? (make-regexp "hello" 0 0)))

;; regexp-match? is a predicate on match objects
(test-assert "regexp-match? returns #t on match object"
  (let ([m (regexp-search (rx "hello") "hello world")])
    (regexp-match? m)))
(test-assert "regexp-match? returns #f on non-match"
  (not (regexp-match? "not a match")))

;; ======================================================================
;; (hafod syntax) -- 2 untested symbols
;; ======================================================================

;; <<-port-holder is a vector box (vector #f)
(test-assert "<<-port-holder is a vector"
  (vector? <<-port-holder))

;; <<-port-holder-set! sets the box contents
(test-assert "<<-port-holder-set! is a procedure" (procedure? <<-port-holder-set!))
(let ([old (vector-ref <<-port-holder 0)])
  (<<-port-holder-set! "test-value")
  (test-equal "<<-port-holder-set! updates holder"
    "test-value" (vector-ref <<-port-holder 0))
  ;; Restore
  (<<-port-holder-set! old))

;; ======================================================================
;; (hafod environment) -- 1 untested symbol
;; ======================================================================

;; read-environ-fresh returns an alist
(test-assert "read-environ-fresh returns alist"
  (let ([env (read-environ-fresh)])
    (and (list? env)
         (pair? (car env))
         (string? (caar env)))))

;; ======================================================================
;; (hafod awk) -- 1 untested symbol
;; ======================================================================

;; next-:range: is the inclusive-both-ends range helper
(test-assert "next-:range: is a procedure" (procedure? next-:range:))

;; Test the :range: clause in awk macro (inclusive both ends)
(test-equal ":range: (inclusive range) works in awk"
  '("START" "mid" "END")
  (let ([p (open-input-string "before\nSTART\nmid\nEND\nafter\n")])
    (awk (read-line p) (line) ((collected '()))
      (:range: (when (string=? line "START"))
               (when (string=? line "END"))
               (cons line collected))
      (after (reverse collected)))))

(test-end)
