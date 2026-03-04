;;; (hafod re) -- SRE (S-expression Regular Expression) engine for hafod
;;; Provides rx macro, POSIX regex backend, match objects, search/substitute/fold.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod re)
  (export
    ;; rx macro
    rx
    ;; Regex predicates and constructors
    regexp? make-regexp string->regexp
    ;; Search and match
    regexp-search regexp-search? regexp-match
    ;; Match accessors
    match:start match:end match:substring match:count
    regexp-match?
    ;; Substitution
    regexp-substitute regexp-substitute/global
    ;; Fold / iteration
    regexp-fold regexp-for-each
    ;; ADT bridge
    re-adt->compiled-regexp
    ;; Match macros
    let-match if-match match-cond
    ;; Fold right
    regexp-fold-right
    ;; SRE form predicates
    if-sre-form sre-form?
    ;; Conversion functions
    sre->regexp regexp->sre posix-string->regexp
    ;; POSIX string compiler (re-export from re-adt, returns 4 values)
    regexp->posix-string
    ;; Transformation re-exports from re-adt
    flush-submatches uncase uncase-string uncase-char-set)
  (import
    (hafod internal re-engine)
    (hafod internal re-parse)
    (hafod internal re-macros)
    ;; Re-exports from re-adt that re.ss currently re-exports
    (only (hafod re-adt)
          regexp->posix-string flush-submatches uncase uncase-string uncase-char-set))

  ) ; end library
