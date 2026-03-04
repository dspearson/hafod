;;; (hafod re-adt) -- RE ADT (Regular Expression Abstract Data Type) for hafod
;;; Port of scsh/rx/re.scm to Chez Scheme R6RS.
;;; Provides all RE ADT record types, smart constructors, predicates,
;;; accessors, singletons, and transformation utilities.
;;; Copyright (c) 2026, hafod contributors.
;;; Original scsh code: Copyright (c) 1997, 1998 Olin Shivers.

(library (hafod re-adt)
  (export
    ;; re-string
    really-make-re-string make-re-string re-string? re-string:chars
    ;; re-char-set
    really-make-re-char-set make-re-char-set re-char-set? re-char-set:cset
    ;; Anchor singletons
    re-bos re-bos? re-eos re-eos? re-bol re-bol? re-eol re-eol?
    ;; re-dsm
    really-make-re-dsm make-re-dsm make-re-dsm/tsm
    re-dsm? re-dsm:body re-dsm:pre-dsm re-dsm:tsm re-dsm:post-dsm
    re-dsm open-dsm
    ;; re-seq
    really-make-re-seq make-re-seq make-re-seq/tsm
    re-seq? re-seq:elts re-seq:tsm
    re-seq
    ;; re-choice
    really-make-re-choice make-re-choice make-re-choice/tsm
    re-choice? re-choice:elts re-choice:tsm
    re-choice
    ;; re-repeat
    really-make-re-repeat make-re-repeat make-re-repeat/tsm
    re-repeat? re-repeat:from re-repeat:to re-repeat:body re-repeat:tsm
    re-repeat
    ;; re-submatch
    really-make-re-submatch make-re-submatch make-re-submatch/tsm
    re-submatch? re-submatch:body re-submatch:pre-dsm re-submatch:tsm
    re-submatch:post-dsm
    re-submatch
    ;; Named singletons
    re-trivial re-trivial? re-empty re-empty? re-any re-any? re-nonl
    ;; Generic
    regexp? re-tsm
    ;; Transformations
    flush-submatches uncase uncase-char-set uncase-string
    ;; Predicates
    re-char-class? static-char-class?
    ;; POSIX string compiler
    regexp->posix-string simplify-regexp
    )
  (import
    (hafod internal re-records)
    (hafod internal re-posixstr))

  ) ; end library
