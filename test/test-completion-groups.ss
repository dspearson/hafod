;;; test/test-completion-groups.ss -- Freeze suite for the pure grouped-completion
;;; contract and the header-safe grid geometry.  A completer may return either the
;;; flat entry list it returns today or a list of (label . entries) groups; the two
;;; shapes are told apart at a single predicate here.  Untagged candidates are
;;; inferred into directories / files / commands / options in a fixed, documented
;;; order, asserted below so it cannot silently drift.
;;;
;;; The load-bearing piece is the row-plan.  Today the grid maps a flat candidate
;;; index to a screen cell by pure quotient/remainder over the column count; once a
;;; group header occupies a whole row that linear map breaks.  The row-plan interleaves
;;; header rows with candidate rows and turns index<->cell into a LOOKUP: every
;;; candidate index round-trips through row-plan-locate -> row-plan-cell, a header row
;;; and an empty (ragged) cell both answer #f, and a single unlabelled group draws no
;;; header row at all.  All assertions are direct calls, entirely off a terminal.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod editor completion-groups)
              grouped-completions? infer-group group-rank group-order
              build-row-plan row-plan-rows row-plan-locate row-plan-cell)
        (chezscheme))

(test-begin "completion-groups")

;; ======================================================================
;; Helpers
;; ======================================================================

;; The two values row-plan-locate hands back, captured as a list so a single
;; test-equal can pin both the screen row and the column at once.
(define (locate->list rp flat-index)
  (let-values ([(row col) (row-plan-locate rp flat-index)])
    (list row col)))

;; #t when flat-index, mapped to its cell by row-plan-locate and then back by
;; row-plan-cell, lands on itself -- the round-trip the whole row-plan exists to keep
;; correct across header and ragged-row boundaries.
(define (round-trips? rp flat-index)
  (let-values ([(row col) (row-plan-locate rp flat-index)])
    (eqv? (row-plan-cell rp row col) flat-index)))

;; ======================================================================
;; === Grouped-vs-flat discriminator ===
;; ======================================================================

;; A grouped return is a list of (label . entries): its first element's car is a
;; string label and its second element is itself an entry whose car is a candidate
;; name.  A flat list never satisfies that -- a flat pair's cadr is an int, a flat
;; triple's cadr is a positions list whose car is an int -- so neither is mistaken
;; for grouped, and the empty list is flat by definition.
(test-assert "a labelled group list is grouped"
  (grouped-completions? '(("dirs" ("a/" 0) ("b/" 0)))))
(test-assert "a flat pair list is not grouped"
  (not (grouped-completions? '(("a" . (0)) ("b" . (0))))))
(test-assert "a flat triple list is not grouped"
  (not (grouped-completions? '(("a" (0) "desc")))))
(test-assert "the empty list is not grouped"
  (not (grouped-completions? '())))

;; ======================================================================
;; === Inferred group order (directories < files < commands < options) ===
;; ======================================================================

;; Type is consulted first, then a leading dash, then the caller's precomputed PATH
;; verdict, else a plain file.  A real directory is grouped by its type even when its
;; name carries no trailing slash.
(test-equal "a trailing slash infers a directory" 'directories
  (infer-group "src/" 'file #f))
(test-equal "a dir type infers a directory even without a slash" 'directories
  (infer-group "buildhere" 'dir #f))
(test-equal "a leading dash infers an option" 'options
  (infer-group "-x" 'file #f))
(test-equal "a name known on PATH infers a command" 'commands
  (infer-group "ls" #f #t))
(test-equal "an ordinary name infers a file" 'files
  (infer-group "readme.md" #f #f))

;; The canonical draw order is fixed and its ranks are dense and strictly ascending;
;; a stable sort of a scrambled list of the four symbols lands in exactly that order.
(test-equal "the canonical group order is fixed"
  '(directories files commands options) (group-order))
(test-assert "the four ranks strictly ascend in draw order"
  (and (< (group-rank 'directories) (group-rank 'files))
       (< (group-rank 'files)       (group-rank 'commands))
       (< (group-rank 'commands)    (group-rank 'options))))
(test-equal "sorting a scrambled list by rank yields the canonical order"
  '(directories files commands options)
  (list-sort (lambda (a b) (< (group-rank a) (group-rank b)))
             '(options commands files directories)))

;; ======================================================================
;; === Row-plan: a single unlabelled group draws no header ===
;; ======================================================================

;; Five candidates over three columns with a #f label: two candidate rows, no header
;; row, and every candidate index round-trips through locate -> cell.
(let ([rp (build-row-plan '((#f . 5)) 3)])
  (test-equal "a single #f-label group draws no header (two rows for five)" 2
    (row-plan-rows rp))
  (for-each
    (lambda (i)
      (test-assert
        (string-append "single-group index " (number->string i) " round-trips")
        (round-trips? rp i)))
    '(0 1 2 3 4)))

;; ======================================================================
;; === Row-plan: labelled groups interleave header rows ===
;; ======================================================================

;; Two labelled groups over two columns: 1 header + 1 dir row + 1 header + 2 file
;; rows = 5 screen rows.  The first file (flat-index 2) lands on the row AFTER the
;; "files" header at column 0; a header row answers #f, never a candidate; and every
;; candidate index round-trips even across the header boundary.
(let ([rp (build-row-plan '(("directories" . 2) ("files" . 3)) 2)])
  (test-equal "headers count as screen rows" 5 (row-plan-rows rp))
  (test-equal "the first file lands after the files header at column 0" '(3 0)
    (locate->list rp 2))
  (test-equal "the first (directories) header row is not a candidate" #f
    (row-plan-cell rp 0 0))
  (test-equal "the second (files) header row is not a candidate" #f
    (row-plan-cell rp 2 0))
  (for-each
    (lambda (i)
      (test-assert
        (string-append "labelled-group index " (number->string i)
                       " round-trips across the header boundary")
        (round-trips? rp i)))
    '(0 1 2 3 4)))

;; ======================================================================
;; === Row-plan: a ragged final row skips its empty cell ===
;; ======================================================================

;; Three candidates over two columns: two candidate rows, the second holding a single
;; cell.  The empty cell (second row, column 1) answers #f so navigation never lands
;; on it; the occupied cell answers its real index; every candidate round-trips.
(let ([rp (build-row-plan '((#f . 3)) 2)])
  (test-equal "three over two columns is two rows" 2 (row-plan-rows rp))
  (test-equal "the empty ragged cell is not a candidate" #f
    (row-plan-cell rp 1 1))
  (test-equal "the occupied cell of the ragged row is its real index" 2
    (row-plan-cell rp 1 0))
  (for-each
    (lambda (i)
      (test-assert
        (string-append "ragged-row index " (number->string i) " round-trips")
        (round-trips? rp i)))
    '(0 1 2)))

(test-end)
