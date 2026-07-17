;;; test-srfi-4.ss -- Spot-check tests for the homogeneous-vector library (SRFI 4).
;;;
;;; SRFI 4 provides typed numeric vectors. hafod backs them with Chez
;;; bytevectors, so a make / set! / ref / length / list round-trip is the
;;; representative behaviour to pin. This suite exercises the u8 family fully
;;; and a wider (u16) family, then documents two deliberate/deferred points.
;;;
;;; Deliberate: the type predicates are bytevector-backed and therefore
;;; NON-DISJOINT -- u8vector? answers #t for any bytevector -- as noted in the
;;; library header. Deferred: the u64/s64/f32/f64 families are not exposed
;;; (recorded in Future); only the 8/16/32-bit integer families are present.
;;; The audit dispositioned this library conformant-with-those-notes.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-4) (chezscheme))

(test-begin "srfi-4")

;; ===========================================================================
;; Section A -- u8vector make / set! / ref / length / list round-trip
;; ===========================================================================

(define v (make-u8vector 3 0))
(u8vector-set! v 0 10)
(u8vector-set! v 1 20)
(u8vector-set! v 2 30)

(test-equal "u8vector-ref reads back a set element" 20 (u8vector-ref v 1))
(test-equal "u8vector-length reports the element count" 3 (u8vector-length v))
(test-equal "u8vector->list reflects the stored elements"
            '(10 20 30) (u8vector->list v))
(test-equal "list->u8vector then u8vector->list round-trips"
            '(1 2 3) (u8vector->list (list->u8vector '(1 2 3))))
(test-equal "the u8vector constructor stores its arguments"
            7 (u8vector-ref (u8vector 5 6 7) 2))

;; ===========================================================================
;; Section B -- a wider family (u16) rides on the same bytevector backing
;; ===========================================================================

(define w (u16vector 100 200 300))
(test-equal "u16vector-ref reads a 16-bit element" 300 (u16vector-ref w 2))
(test-equal "u16vector-length divides the byte length by two" 3 (u16vector-length w))
(test-equal "u16vector round-trips through a list"
            '(100 200 300) (u16vector->list w))

;; ===========================================================================
;; Section C -- the deliberate non-disjoint predicate
;; ===========================================================================
;; Pinned so a later move to disjoint tagged types would redden here.

(test-assert "u8vector? is true for its own constructor's result"
             (u8vector? (u8vector 1 2 3)))
(test-assert "u8vector? is bytevector-backed and non-disjoint (deliberate)"
             (u8vector? (make-bytevector 4)))

(test-end)
