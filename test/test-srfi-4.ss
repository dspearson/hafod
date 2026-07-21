;;; test-srfi-4.ss -- Spot-check tests for the homogeneous-vector library (SRFI 4).
;;;
;;; SRFI 4 provides typed numeric vectors. hafod backs them with Chez
;;; bytevectors, so a make / set! / ref / length / list round-trip is the
;;; representative behaviour to pin. This suite exercises the u8 family fully,
;;; a wider (u16) family, and the 64-bit integer (u64/s64) and IEEE float
;;; (f32/f64) families -- all ten SRFI 4 families are now exposed.
;;;
;;; The one deliberate divergence: the type predicates are bytevector-backed and
;;; therefore NON-DISJOINT -- u8vector? answers #t for any bytevector -- as noted
;;; in the library header. The 64-bit and float families use full-width exact
;;; round-trips and a single-precision narrowing witness as their non-vacuous
;;; checks.
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

;; ===========================================================================
;; Section D -- the 64-bit integer families (u64vector / s64vector)
;; ===========================================================================
;; Full-width discriminators: a 32-bit backing would truncate these, so the
;; round-trips prove genuine 64-bit storage, not a wrapper over the u32 family.

(test-equal "u64vector round-trips 2^64-1 exactly"
            18446744073709551615 (u64vector-ref (u64vector 18446744073709551615) 0))
(test-equal "s64vector round-trips -2^63 exactly"
            -9223372036854775808 (s64vector-ref (s64vector -9223372036854775808) 0))

(define u64 (u64vector 100 200 300))
(test-equal "u64vector-ref reads a 64-bit element" 300 (u64vector-ref u64 2))
(test-equal "u64vector-length divides the byte length by eight" 3 (u64vector-length u64))
(test-equal "u64vector round-trips through a list"
            '(100 200 300) (u64vector->list u64))
(test-equal "list->u64vector then u64vector->list round-trips"
            '(1 2 3) (u64vector->list (list->u64vector '(1 2 3))))

(define s64 (make-s64vector 3 0))
(s64vector-set! s64 0 -1)
(s64vector-set! s64 1 42)
(s64vector-set! s64 2 -9223372036854775808)
(test-equal "s64vector-ref reads back a set signed element" -1 (s64vector-ref s64 0))
(test-equal "s64vector-length reports the element count" 3 (s64vector-length s64))
(test-equal "s64vector round-trips signed values through a list"
            '(-1 42 -9223372036854775808) (s64vector->list s64))

(test-assert "u64vector? is bytevector-backed and non-disjoint (deliberate)"
             (u64vector? (make-bytevector 8)))
(test-assert "s64vector? is true for its own constructor's result"
             (s64vector? (s64vector 1 2 3)))

;; ===========================================================================
;; Section E -- the IEEE float families (f32vector / f64vector)
;; ===========================================================================
;; f64 stores a full-precision double; f32 narrows to single precision. The
;; (not (= 3.14 ...)) witness proves single, not double, backing -- a naive
;; f64-backed f32 would keep 3.14 exactly and redden it. Equality tests use
;; single-representable values (there is no test-approximate in the runner).

(test-equal "f64vector stores a full-precision double"
            3.141592653589793 (f64vector-ref (f64vector 3.141592653589793) 0))
(test-equal "f32vector round-trips a single-representable value"
            1.5 (f32vector-ref (f32vector 1.5) 0))
(test-assert "f32vector narrows to single precision (proves single, not double, storage)"
             (not (= 3.14 (f32vector-ref (f32vector 3.14) 0))))
(test-equal "make-f64vector applies the fill"
            2.5 (f64vector-ref (make-f64vector 3 2.5) 2))

(define f64 (f64vector 1.5 2.5 3.5))
(test-equal "f64vector-ref reads a double element" 3.5 (f64vector-ref f64 2))
(test-equal "f64vector-length divides the byte length by eight" 3 (f64vector-length f64))
(test-equal "f64vector round-trips through a list"
            '(1.5 2.5 3.5) (f64vector->list f64))
(test-equal "list->f64vector then f64vector->list round-trips"
            '(0.5 1.5 2.5) (f64vector->list (list->f64vector '(0.5 1.5 2.5))))

(define f32 (make-f32vector 3 0.0))
(f32vector-set! f32 0 1.5)
(f32vector-set! f32 1 -0.25)
(f32vector-set! f32 2 4.0)
(test-equal "f32vector-set!/ref round-trips a single-representable value"
            -0.25 (f32vector-ref f32 1))
(test-equal "f32vector-length reports the element count" 3 (f32vector-length f32))
(test-equal "f32vector round-trips single-representable values through a list"
            '(1.5 -0.25 4.0) (f32vector->list f32))

(test-assert "f64vector? is bytevector-backed and non-disjoint (deliberate)"
             (f64vector? (make-bytevector 8)))
(test-assert "f32vector? is true for its own constructor's result"
             (f32vector? (f32vector 1.5 2.5)))

(test-end)
