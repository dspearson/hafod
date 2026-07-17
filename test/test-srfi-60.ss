;;; test-srfi-60.ss -- Conformance tests for the bit library (SRFI 60).
;;;
;;; hafod deliberately keeps a coherent 0/1-integer convention where the
;;; specification uses booleans (copy-bit / integer->list / list->integer), and
;;; keeps its own (to start end from) copy-bit-field argument order. See the
;;; deliberate-divergence note in src/hafod/srfi-60.ss. This suite PINS that
;;; current behaviour so a later silent regression to the spec's boolean
;;; convention (or argument order) would redden here. The remaining bit
;;; procedures are conformant and are spot-checked below.
;;; Copyright (c) 2026, hafod contributors.

;; integer-length is redefined by the bit library; take it (and the rest of the
;; SRFI 60 surface) from there, everything else from chezscheme.
(import (test runner) (except (chezscheme) integer-length) (hafod srfi-60))

(test-begin "srfi-60")

;; ===========================================================================
;; Section A -- the deliberate 0/1 convention (pinned, not conformant to spec)
;; ===========================================================================
;; The spec would take a boolean bit and emit/consume booleans; hafod uses 0/1.

(test-equal "copy-bit sets bit 2 using a 0/1 bit: (copy-bit 2 0 1) is 4"
            4 (copy-bit 2 0 1))
(test-equal "copy-bit clears with a 0 bit: (copy-bit 0 7 0) is 6"
            6 (copy-bit 0 7 0))
(test-equal "integer->list emits 0/1 integers, most-significant first: (integer->list 5) is (1 0 1)"
            '(1 0 1) (integer->list 5))
(test-equal "list->integer consumes 0/1 integers: (list->integer '(1 0 1)) is 5"
            5 (list->integer '(1 0 1)))
(test-equal "integer->list and list->integer round-trip a value"
            13 (list->integer (integer->list 13)))

;; ===========================================================================
;; Section B -- copy-bit-field in hafod's (to start end from) argument order
;; ===========================================================================
;; The spec orders (to from start end); hafod's callers (rotate-bit-field) depend
;; on (to start end from). Pin hafod's order.

(test-equal "copy-bit-field in hafod order (to start end from): (copy-bit-field 0 1 3 15) is 6"
            6 (copy-bit-field 0 1 3 15))

;; ===========================================================================
;; Section C -- conformant bit-operation spot-checks
;; ===========================================================================

(test-equal "logand" 8 (logand 12 10))
(test-equal "logior" 14 (logior 12 10))
(test-equal "logxor" 6 (logxor 12 10))
(test-equal "lognot" -1 (lognot 0))
(test-equal "ash left" 16 (ash 1 4))
(test-equal "ash right" 4 (ash 16 -2))
(test-equal "bit-field extracts bits [start,end)" 6 (bit-field 13 1 4))
(test-equal "integer-length of 5 is 3" 3 (integer-length 5))
(test-equal "integer-length of 0 is 0" 0 (integer-length 0))
(test-equal "logcount of 7 is 3" 3 (logcount 7))

(test-end)
