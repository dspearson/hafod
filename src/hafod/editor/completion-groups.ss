;;; (hafod editor completion-groups) -- Pure grouped-completion contract and the
;;; header-safe grid geometry, off any terminal.
;;;
;;; A completer returns either the flat entry list it returns today -- a pair
;;; (name . positions) or a triple (name positions description) -- or a list of
;;; (label . entries) groups.  grouped-completions? tells the two shapes apart at a
;;; single predicate so the dispatch sink can normalise once.  An untagged candidate
;;; is inferred into one of directories / files / commands / options in a FIXED,
;;; documented order (a directory by its type, then a leading dash, then the caller's
;;; precomputed PATH verdict, else a plain file); the order is frozen by a test so it
;;; cannot silently drift.
;;;
;;; The load-bearing piece is the row-plan.  The grid maps a flat candidate index to
;;; a screen cell; while every row holds candidates that map is pure quotient/remainder
;;; over the column count, but once a group header occupies a whole row the linear map
;;; breaks -- and that same arithmetic is mirrored in several places.  build-row-plan
;;; lays out header rows and candidate rows once, so index<->cell becomes a LOOKUP:
;;; row-plan-locate maps a candidate index to its (row, col) and row-plan-cell maps a
;;; cell back, answering #f for a header row or an empty (ragged) cell so navigation
;;; never lands on either.  Headers count as screen rows, and a single unlabelled
;;; group draws none.
;;;
;;; This module is a pure leaf: it imports only (chezscheme) and carries a group label
;;; as an opaque string -- it writes nothing to a terminal, so label sanitisation
;;; belongs at the sink and renderer that turn a label into display text, never here.
;;; The one-way rule is that render/editor import this leaf, never the reverse, so
;;; there is no cycle.
;;;
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor completion-groups)
  (export grouped-completions? infer-group group-rank group-order
          build-row-plan row-plan-rows row-plan-locate row-plan-cell)
  (import (chezscheme))

  ;; ======================================================================
  ;; === Grouped-vs-flat discriminator ===
  ;; ======================================================================

  ;; #t only when RESULTS is a non-empty list whose first element is a group
  ;; (label . entries): its car is a string label and its second element is itself
  ;; an entry whose car is a candidate name (a string).  A flat list never satisfies
  ;; this -- a flat pair (name . positions) has an integer as its cadr, and a flat
  ;; triple (name positions description) has a positions list whose car is an integer
  ;; -- so neither is mistaken for grouped, and the empty list is flat by definition.
  (define (grouped-completions? results)
    (and (pair? results)
         (let ([e (car results)])
           (and (pair? e)
                (string? (car e))
                (pair? (cdr e))
                (pair? (cadr e))
                (string? (car (cadr e)))))))

  ;; ======================================================================
  ;; === Inferred group order (directories < files < commands < options) ===
  ;; ======================================================================

  ;; The canonical draw order for inferred groups.  Fixed and documented so it cannot
  ;; drift; group-rank yields the dense ascending key that sorts a mixed list into it.
  (define (group-order) '(directories files commands options))

  (define (group-rank sym)
    (case sym
      [(directories) 0]
      [(files)       1]
      [(commands)    2]
      [(options)     3]
      ;; An unrecognised tag sorts after the four canonical groups rather than
      ;; crashing a sort; the sink only ever supplies the four above.
      [else          4]))

  (define (ends-with-slash? s)
    (let ([len (string-length s)])
      (and (fx> len 0) (char=? (string-ref s (fx- len 1)) #\/))))

  (define (starts-with-dash? s)
    (and (fx> (string-length s) 0) (char=? (string-ref s 0) #\-)))

  ;; Infer an untagged candidate's group from data already at hand.  Type is
  ;; consulted first (so a real directory is grouped by its stat, not merely a
  ;; trailing slash), then a leading dash marks an option, then COMMAND? -- the PATH
  ;; membership the sink precomputed, keeping this module free of the completers --
  ;; marks a command; anything else is a plain file.
  (define (infer-group name type command?)
    (cond
      [(or (eq? type 'dir) (ends-with-slash? name)) 'directories]
      [(starts-with-dash? name)                     'options]
      [command?                                     'commands]
      [else                                         'files]))

  ;; ======================================================================
  ;; === Header-safe row-plan ===
  ;; ======================================================================

  ;; A row-plan is a vector of row records in draw order.  A header row carries its
  ;; label; a candidate row carries the flat index of its first cell and how many
  ;; cells it holds (fewer than the column count on a group's ragged final row).
  ;; Both stay internal -- the row-plan is opaque to callers, reached only through
  ;; the accessors below.
  (define-record-type header-row
    (nongenerative hafod-editor-completion-groups-header-row-v1)
    (fields label))

  (define-record-type candidate-row
    (nongenerative hafod-editor-completion-groups-candidate-row-v1)
    (fields first-index cell-count))

  ;; Build the row-plan from GROUP-PLAN, a list of (label . count) in draw order
  ;; where a string label draws a header row and a #f label draws none.  Each group
  ;; contributes its optional header row then ceil(count / grid-cols) candidate rows;
  ;; a group's candidates never share a screen row with the next group's, so a short
  ;; final row is left ragged rather than back-filled.  The flat candidate index runs
  ;; unbroken across groups (headers are not candidates).
  (define (build-row-plan group-plan grid-cols)
    (let ([cols (max 1 grid-cols)])
      (let group-loop ([groups group-plan]
                       [flat 0]
                       [rows '()])
        (if (null? groups)
            (list->vector (reverse rows))
            (let* ([g (car groups)]
                   [label (car g)]
                   [count (cdr g)]
                   [rows* (if (string? label)
                              (cons (make-header-row label) rows)
                              rows)])
              (let cand-loop ([remaining count]
                              [idx flat]
                              [acc rows*])
                (if (<= remaining 0)
                    (group-loop (cdr groups) (+ flat count) acc)
                    (let ([cell-count (min cols remaining)])
                      (cand-loop (- remaining cols)
                                 (+ idx cell-count)
                                 (cons (make-candidate-row idx cell-count) acc))))))))))

  ;; The total screen row count, header rows included -- the height the pager and
  ;; scroll arithmetic must reckon with.
  (define (row-plan-rows row-plan)
    (vector-length row-plan))

  ;; Map a 0-based flat candidate index to its cell as (values screen-row col) by a
  ;; lookup over the candidate rows -- never arithmetic over a header-free grid.  An
  ;; index past the last candidate is a caller bug and is surfaced as such.
  (define (row-plan-locate row-plan flat-index)
    (let ([n (vector-length row-plan)])
      (let loop ([r 0])
        (if (fx>= r n)
            (assertion-violation 'row-plan-locate
                                 "flat candidate index is out of range" flat-index)
            (let ([row (vector-ref row-plan r)])
              (if (and (candidate-row? row)
                       (>= flat-index (candidate-row-first-index row))
                       (< flat-index (+ (candidate-row-first-index row)
                                        (candidate-row-cell-count row))))
                  (values r (- flat-index (candidate-row-first-index row)))
                  (loop (fx+ r 1))))))))

  ;; Map a (screen-row, col) cell back to its flat candidate index, or #f when the
  ;; cell holds no candidate: a header row, an out-of-range coordinate, or the empty
  ;; trailing cell of a ragged row.  Navigation uses the #f answer to skip headers
  ;; and clamp a ragged row.
  (define (row-plan-cell row-plan screen-row col)
    (and (>= screen-row 0)
         (< screen-row (vector-length row-plan))
         (>= col 0)
         (let ([row (vector-ref row-plan screen-row)])
           (and (candidate-row? row)
                (< col (candidate-row-cell-count row))
                (+ (candidate-row-first-index row) col)))))

  ) ; end library
