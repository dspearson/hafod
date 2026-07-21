;;; test-srfi-74.ss -- Spot-check tests for the blob library (SRFI 74).
;;;
;;; SRFI 74 provides octet-addressed binary objects (blobs) with single-octet
;;; accessors, fixed-width endianness-parameterised ref/set operations, the
;;; variable-width blob-uint/sint accessors, blob<->integer-list conversions,
;;; native-endianness fixed-width variants, and the blob? predicate. hafod backs
;;; blobs with Chez bytevectors. This suite spot-checks make-blob and the u8
;;; round-trip; the fixed-width u16/s16 endianness round-trip (with a byte-order
;;; cross-check proving the endianness argument is honoured); the variable-width
;;; accessors at a non-power-of-two size with a two's-complement witness; the
;;; blob<->list round-trips; and the native fixed-width variants. blob? is
;;; bytevector-backed and therefore NON-DISJOINT -- a deliberate divergence.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-74) (chezscheme))

(test-begin "srfi-74")

;; ===========================================================================
;; Section A -- make-blob, blob-length and the u8 round-trip
;; ===========================================================================

(define b (make-blob 4))
(test-equal "blob-length reports the octet count" 4 (blob-length b))

(blob-u8-set! b 0 10)
(blob-u8-set! b 1 200)
(test-equal "blob-u8-ref reads back a set octet" 10 (blob-u8-ref b 0))
(test-equal "blob-u8-ref reads back a second set octet" 200 (blob-u8-ref b 1))

;; ===========================================================================
;; Section B -- fixed-width u16 ref/set with an explicit endianness
;; ===========================================================================

(blob-u16-set! (endianness little) b 0 #x1234)
(test-equal "a little-endian u16 reads back the stored value"
            #x1234 (blob-u16-ref (endianness little) b 0))

;; The endianness argument is honoured: little-endian stores the low octet
;; first, so octet 0 is #x34 and octet 1 is #x12.
(test-equal "little-endian stored the low octet first" #x34 (blob-u8-ref b 0))
(test-equal "little-endian stored the high octet second" #x12 (blob-u8-ref b 1))

;; Big-endian round-trips the same value with the opposite octet order.
(blob-u16-set! (endianness big) b 2 #x1234)
(test-equal "a big-endian u16 reads back the stored value"
            #x1234 (blob-u16-ref (endianness big) b 2))
(test-equal "big-endian stored the high octet first" #x12 (blob-u8-ref b 2))

;; A signed 16-bit round-trip preserves a negative value.
(blob-s16-set! (endianness big) b 0 -1)
(test-equal "a signed 16-bit blob round-trips a negative value"
            -1 (blob-s16-ref (endianness big) b 0))

;; ===========================================================================
;; Section C -- blob-copy and blob=?
;; ===========================================================================

(define copy (blob-copy b))
(test-assert "blob-copy produces an equal blob" (blob=? b copy))

;; ===========================================================================
;; Section D -- blob? and the variable-width blob-uint/sint accessors
;; ===========================================================================
;; SRFI 74 variable-width order is (size endianness blob k [n]); a transposed
;; size/endianness would reverse the byte-order witnesses below.

(define bb (make-blob 8))
(blob-uint-set! 3 (endianness big) bb 0 #x010203)
(test-equal "big-endian size-3 lays the high octet first" 1 (blob-u8-ref bb 0))
(test-equal "big-endian size-3 middle octet" 2 (blob-u8-ref bb 1))
(test-equal "big-endian size-3 low octet last" 3 (blob-u8-ref bb 2))
(blob-uint-set! 3 (endianness little) bb 0 #x010203)
(test-equal "little-endian size-3 lays the low octet first" 3 (blob-u8-ref bb 0))
(test-equal "little-endian size-3 high octet last" 1 (blob-u8-ref bb 2))
(test-equal "blob-uint-ref round-trips at a non-power-of-two size"
            #x010203 (blob-uint-ref 3 (endianness little) bb 0))
(blob-sint-set! 3 (endianness big) bb 0 -1)
(test-equal "blob-sint-ref round-trips -1 (two's-complement at size 3)"
            -1 (blob-sint-ref 3 (endianness big) bb 0))
(test-assert "blob? is bytevector-backed and non-disjoint (deliberate)"
             (blob? (make-bytevector 4)))

;; ===========================================================================
;; Section E -- blob<->list conversions
;; ===========================================================================

(test-equal "blob->uint-list . uint-list->blob round-trips a list"
            '(1 2 3)
            (blob->uint-list 2 (endianness big)
                             (uint-list->blob 2 (endianness big) '(1 2 3))))
(test-equal "blob->u8-list . u8-list->blob round-trips a list"
            '(10 20 30) (blob->u8-list (u8-list->blob '(10 20 30))))
(test-equal "blob->sint-list . sint-list->blob round-trips a negative element"
            '(-1 2 -3)
            (blob->sint-list 2 (endianness little)
                             (sint-list->blob 2 (endianness little) '(-1 2 -3))))

;; ===========================================================================
;; Section F -- native fixed-width variants (host endianness)
;; ===========================================================================
;; Direct aliases of the Chez native primitives; a wrong-width binding would
;; truncate and redden the full-width round-trip.

(define nb (make-blob 8))
(blob-u32-native-set! nb 0 #x01020304)
(test-equal "blob-u32-native round-trips a full 32-bit value"
            #x01020304 (blob-u32-native-ref nb 0))
(blob-u16-native-set! nb 4 #xBEEF)
(test-equal "blob-u16-native round-trips a 16-bit value"
            #xBEEF (blob-u16-native-ref nb 4))
(blob-s64-native-set! nb 0 -1)
(test-equal "blob-s64-native round-trips a signed 64-bit value"
            -1 (blob-s64-native-ref nb 0))

(test-end)
