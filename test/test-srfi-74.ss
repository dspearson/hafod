;;; test-srfi-74.ss -- Spot-check tests for the blob library (SRFI 74).
;;;
;;; SRFI 74 provides octet-addressed binary objects (blobs) with single-octet
;;; accessors and fixed-width, endianness-parameterised ref/set operations.
;;; hafod backs blobs with Chez bytevectors. This suite spot-checks make-blob,
;;; the u8 round-trip, and a fixed-width u16/s16 endianness round-trip -- with a
;;; byte-order cross-check proving the endianness argument is honoured. The
;;; audit dispositioned this library conformant.
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

(test-end)
