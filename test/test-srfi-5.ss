;;; test-srfi-5.ss -- Spot-check tests for the extended let library (SRFI 5).
;;;
;;; SRFI 5 generalises let with a single signature form that subsumes plain
;;; let, named let and a named let with a trailing rest identifier. This suite
;;; spot-checks the embedded-name signature (a named-let loop) and the trailing
;;; rest-identifier form (the rest variable captures the extra arguments of a
;;; recursive call). The audit dispositioned this library conformant.
;;; Copyright (c) 2026, hafod contributors.

;; The library redefines let with the SRFI 5 signature form; take let from the
;; library and everything else from chezscheme.
(import (test runner) (except (chezscheme) let) (hafod srfi-5))

(test-begin "srfi-5")

;; ===========================================================================
;; Section A -- plain and named let still work
;; ===========================================================================

(test-equal "plain let binds and evaluates its body"
            3 (let ((a 1) (b 2)) (+ a b)))
(test-equal "standard named let iterates"
            15 (let loop ((i 1) (sum 0))
                 (if (> i 5) sum (loop (+ i 1) (+ sum i)))))

;; ===========================================================================
;; Section B -- the embedded-name signature form (let (name (var init) ...))
;; ===========================================================================

(test-equal "embedded-name signature form loops like a named let"
            '(0 1 2 3 4)
            (let (build (i 0) (acc '()))
              (if (= i 5) (reverse acc) (build (+ i 1) (cons i acc)))))

;; ===========================================================================
;; Section C -- the trailing rest-identifier form
;; ===========================================================================
;; go is (lambda (x . rest) …): the first entry passes only x, so rest is ();
;; the recursive call passes extra arguments, which rest then captures.

(test-equal "trailing rest identifier captures the extra arguments of a call"
            '(1 (a b))
            (let (go (x 1) rest)
              (if (null? rest)
                  (go x 'a 'b)
                  (list x rest))))

(test-end)
