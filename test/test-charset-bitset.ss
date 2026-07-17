;;; test-charset-bitset.ss -- Proof that the finite char-set bitset is membership-
;;; identical to the predicate it replaces, and measurably faster on a multi-member
;;; set.
;;;
;;; Two halves make the proof, in the test-poll-vacuity ethos:
;;;   1. Exhaustive equivalence.  For every code point 0-255 each finite set answers
;;;      char-set-contains? identically to its ORIGINAL predicate (kept here), plus a
;;;      sample of code points >=256 that must all answer #f.  This is trivially green
;;;      today (the real set IS that predicate) and MUST stay green once the bitset
;;;      lands -- that persistence is the byte-identity guarantee.
;;;   2. A multi-member timing witness.  A naive linear-scan reference (the "before")
;;;      is driven alongside the real membership test over millions of queries; the
;;;      bitset path must finish in well under a quarter of the naive path's time.
;;;      On the pre-bitset tree the real path IS the naive scan, so this reddens; once
;;;      the bitset lands it goes green.  A call counter and a membership-agreement
;;;      check rule the witness non-vacuous.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod internal char-sets) (chezscheme))

(test-begin "charset-bitset")

;; Wall-clock elapsed milliseconds for a thunk (the in-tree (real-time) idiom).
(define (elapsed-ms thunk)
  (let ((t0 (real-time)))
    (thunk)
    (- (real-time) t0)))

;; A memv-based reference closure over a fixed member list (proper boolean result).
(define (memv-ref members)
  (lambda (c) (and (memv c members) #t)))

;; First code point in 0-255 where SET disagrees with REF-PRED, or #f if identical.
(define (first-divergence-0-255 set ref-pred)
  (let loop ((cp 0))
    (cond ((>= cp 256) #f)
          ((eqv? (ref-pred (integer->char cp))
                 (char-set-contains? set (integer->char cp)))
           (loop (+ cp 1)))
          (else cp))))

;; =============================================================================
;; Section A -- exhaustive 0-255 equivalence against each set's ORIGINAL predicate
;; =============================================================================
;; Each reference predicate below is copied verbatim from the set's definition in
;; char-sets.ss, so the assertion is "the set still means exactly what it used to".

(define digit-ref     (lambda (c) (and (char>=? c #\0) (char<=? c #\9))))
(define blank-ref     (lambda (c) (or (char=? c #\space) (char=? c #\tab))))
(define newline-ref   (lambda (c) (char=? c #\newline)))
(define hex-digit-ref (lambda (c) (or (and (char>=? c #\0) (char<=? c #\9))
                                      (and (char>=? c #\a) (char<=? c #\f))
                                      (and (char>=? c #\A) (char<=? c #\F)))))
(define ascii-ref     (lambda (c) (< (char->integer c) 128)))

;; An explicit finite set and a string-derived set (their reference is memv over the
;; exact members / string characters).
(define explicit-members (list #\a #\b #\c #\1 #\2 #\space #\!))
(define explicit-set     (apply char-set explicit-members))
(define explicit-ref     (memv-ref explicit-members))

(define s->cs-string "hello, world! 0123")
(define s->cs-set    (string->char-set s->cs-string))
(define s->cs-ref    (memv-ref (string->list s->cs-string)))

(test-equal "char-set:newline membership identical to its predicate for every cp 0-255 (value = first diverging cp or #f)"
            #f (first-divergence-0-255 char-set:newline newline-ref))
(test-equal "char-set:blank membership identical to its predicate for every cp 0-255"
            #f (first-divergence-0-255 char-set:blank blank-ref))
(test-equal "char-set:digit membership identical to its predicate for every cp 0-255"
            #f (first-divergence-0-255 char-set:digit digit-ref))
(test-equal "char-set:hex-digit membership identical to its predicate for every cp 0-255"
            #f (first-divergence-0-255 char-set:hex-digit hex-digit-ref))
(test-equal "char-set:ascii membership identical to its predicate for every cp 0-255"
            #f (first-divergence-0-255 char-set:ascii ascii-ref))
(test-equal "explicit (char-set ...) membership identical to memv over its members for every cp 0-255"
            #f (first-divergence-0-255 explicit-set explicit-ref))
(test-equal "(string->char-set ...) membership identical to memv over its chars for every cp 0-255"
            #f (first-divergence-0-255 s->cs-set s->cs-ref))

;; =============================================================================
;; Section B -- code points >=256 answer #f, and every answer is a proper boolean
;; =============================================================================
;; The (< cp 256) guard must gate the bitset load so an astral code point is a miss,
;; and char-set-contains? must return a genuine #t/#f so downstream eq? stays valid.

(define astral-cps (list 256 955 66000))          ; U+0100, U+03BB, U+101D0 -- non-surrogate
(define finite-sets
  (list char-set:newline char-set:blank char-set:digit char-set:hex-digit
        char-set:ascii explicit-set s->cs-set))

(define (all-astral-false? set)
  (for-all (lambda (cp) (eq? #f (char-set-contains? set (integer->char cp)))) astral-cps))

(define (all-astral-boolean? set)
  (for-all (lambda (cp) (boolean? (char-set-contains? set (integer->char cp)))) astral-cps))

;; A proper boolean for every in-range code point too (not merely a truthy value).
(define (all-in-range-boolean? set)
  (let loop ((cp 0))
    (or (>= cp 256)
        (and (boolean? (char-set-contains? set (integer->char cp)))
             (loop (+ cp 1))))))

(test-assert "every finite set answers #f for a sample of code points >=256"
             (for-all all-astral-false? finite-sets))
(test-assert "every finite set answers a proper boolean for code points >=256"
             (for-all all-astral-boolean? finite-sets))
(test-assert "every finite set answers a proper boolean for every code point 0-255"
             (for-all all-in-range-boolean? finite-sets))

;; =============================================================================
;; Section C -- multi-member timing witness (sited on a >=30-member set per Pitfall 3)
;; =============================================================================
;; A trivial range set (:digit/:ascii) would NOT redden -- the bitset is a slight
;; pessimisation there.  The win shows only where the predicate is a linear O(k) scan,
;; so the witness is sited on a 42-member explicit set.

(define member-string "abcdefghijklmnopqrstuvwxyz0123456789!@#$%^")   ; 42 distinct members, all < 256
(define members       (string->list member-string))
(define big-set       (apply char-set members))

;; The naive reference -- the "before": a linear memv-style scan that increments a
;; call counter so we can prove it actually ran (non-vacuity).
(define naive-calls 0)
(define (naive-contains? c)
  (set! naive-calls (+ naive-calls 1))
  (let lp ((ms members))
    (and (pair? ms)
         (or (char=? c (car ms)) (lp (cdr ms))))))

;; big-set must mean exactly what the naive scan means, across the whole Latin-1 range.
(test-equal "the 42-member bitset set is membership-identical to its naive scan for every cp 0-255"
            #f (first-divergence-0-255 big-set (memv-ref members)))

;; Drive M membership queries against both paths.  The queries cycle every code point
;; 0-255, so most are non-members and force the naive scan to walk all 42 members.
(define M 2000000)

(define (drive-real)
  (let lp ((i 0) (acc 0))
    (if (fx=? i M) acc
        (lp (fx+ i 1)
            (if (char-set-contains? big-set (integer->char (fxand i 255)))
                (fx+ acc 1) acc)))))

(define (drive-naive)
  (let lp ((i 0) (acc 0))
    (if (fx=? i M) acc
        (lp (fx+ i 1)
            (if (naive-contains? (integer->char (fxand i 255)))
                (fx+ acc 1) acc)))))

(define real-acc 0)
(define naive-acc 0)
(define real-ms (elapsed-ms (lambda () (set! real-acc (drive-real)))))
(set! naive-calls 0)
(define naive-ms (elapsed-ms (lambda () (set! naive-acc (drive-naive)))))

;; Emit the measured pair unconditionally so it can be recorded for the SUMMARY.
(display "WITNESS multi-member membership: real-ms=") (display real-ms)
(display " naive-ms=") (display naive-ms)
(display " ratio=") (display (if (> real-ms 0) (exact->inexact (/ naive-ms real-ms)) 'inf))
(newline)

;; Non-vacuity controls (deterministic): the naive path really dispatched M times and
;; agreed with the bitset on every query, and it was measurably the slower of the two.
(test-equal "the naive reference dispatched exactly M times (it did the O(k) work)"
            M naive-calls)
(test-equal "the bitset path and the naive scan agree on the total membership count"
            naive-acc real-acc)
(test-assert "the naive linear scan is measurably slower than the bitset path"
             (> naive-ms real-ms))

;; THE witness: RED on the pre-bitset tree (real IS the naive scan), GREEN once the
;; bitset lands.  Generous 4x margin -- the probed win is 9-26x.
(test-assert "multi-member bitset membership is more than 4x faster than the naive linear scan"
             (< real-ms (/ naive-ms 4)))

(test-end)
