;;; test-srfi-13.ss -- Conformance tests for the string library (SRFI 13).
;;;
;;; string-hash and string-hash-ci accumulated into a fixnum, so a string longer
;;; than about a dozen characters overflowed and raised in fxsll -- a crash in a
;;; pure hashing primitive. The fix widens the accumulator to generic arithmetic
;;; while leaving the hashing recurrence intact, so short-string hash values are
;;; preserved (the pinned "hello" -> 631) and long strings no longer crash. The
;;; 500-character assertion reddened on the pre-fix tree.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-13) (chezscheme))

(test-begin "srfi-13")

;; ===========================================================================
;; Section A -- string-hash does not overflow on long input
;; ===========================================================================
;; Pre-fix (string-hash (make-string 500 #\a) 997) raised "fxsll: fixnum
;; overflow"; the result must instead be an integer in [0, 997).

(test-assert "string-hash on a 500-char string is an integer in [0,997) with no overflow"
             (let ((h (string-hash (make-string 500 #\a) 997)))
               (and (integer? h) (>= h 0) (< h 997))))

;; ===========================================================================
;; Section B -- short-string hash values are preserved by the widening
;; ===========================================================================
;; The mathematically-intended accumulator is unchanged, so the historical
;; value must survive the fix exactly.

(test-equal "string-hash \"hello\" 1000 is still 631" 631 (string-hash "hello" 1000))

;; ===========================================================================
;; Section C -- string-hash-ci widens identically and case-folds
;; ===========================================================================

(test-assert "string-hash-ci on a 500-char string is an integer in [0,997)"
             (let ((h (string-hash-ci (make-string 500 #\A) 997)))
               (and (integer? h) (>= h 0) (< h 997))))
(test-equal "string-hash-ci agrees with string-hash on already-lowercase input"
            (string-hash "hello" 1000) (string-hash-ci "hello" 1000))
(test-equal "string-hash-ci folds case: \"HELLO\" hashes as \"hello\""
            (string-hash "hello" 1000) (string-hash-ci "HELLO" 1000))

;; ===========================================================================
;; Section D -- high-value string library spot-checks
;; ===========================================================================

(test-equal "string-index finds the first matching char position"
            2 (string-index "abcabc" #\c))
(test-equal "string-contains locates a substring" 3 (string-contains "foobarbaz" "bar"))
(test-equal "string-contains reports #f when absent" #f (string-contains "foobar" "xyz"))
(test-equal "string-titlecase capitalises each word"
            "Hello World" (string-titlecase "hello world"))
(test-equal "string-reverse reverses" "cba" (string-reverse "abc"))
(test-equal "string-upcase upcases" "ABC" (string-upcase "abc"))

(test-end)
