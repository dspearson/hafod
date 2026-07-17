;;; test-srfi-9.ss -- Spot-check tests for the record library (SRFI 9).
;;;
;;; SRFI 9's define-record-type introduces a constructor (whose argument order
;;; may differ from the field order), a predicate, field accessors and optional
;;; field mutators. This suite spot-checks a mutable record end-to-end plus a
;;; second record proving the constructor reorders its arguments to fields. The
;;; audit dispositioned this library conformant.
;;; Copyright (c) 2026, hafod contributors.

;; The library redefines define-record-type with SRFI 9 syntax; take it from
;; the library and everything else from chezscheme.
(import (test runner) (except (chezscheme) define-record-type) (hafod srfi-9))

(test-begin "srfi-9")

;; ===========================================================================
;; Section A -- constructor, predicate, accessors and a mutator
;; ===========================================================================

(define-record-type point
  (make-point x y)
  point?
  (x point-x set-point-x!)
  (y point-y))

(define p (make-point 3 4))

(test-assert "the predicate accepts a constructed record" (point? p))
(test-assert "the predicate rejects a non-record" (not (point? 5)))
(test-equal "an accessor reads the first field" 3 (point-x p))
(test-equal "an accessor reads the second field" 4 (point-y p))
(set-point-x! p 10)
(test-equal "a mutator updates its field" 10 (point-x p))
(test-equal "the untouched field is unchanged after a mutation" 4 (point-y p))

;; ===========================================================================
;; Section B -- the constructor reorders its arguments onto fields
;; ===========================================================================
;; The constructor lists its arguments (y x) in the opposite order to the
;; field declarations (x then y); the value passed as y must land in field y.

(define-record-type reordered
  (make-reordered y x)
  reordered?
  (x reordered-x)
  (y reordered-y))

(define r (make-reordered 100 200))    ; y := 100, x := 200

(test-equal "constructor maps a reordered argument to its field (x)"
            200 (reordered-x r))
(test-equal "constructor maps a reordered argument to its field (y)"
            100 (reordered-y r))

(test-end)
