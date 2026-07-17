;;; test-srfi-39.ss -- Spot-check tests for the parameter library (SRFI 39).
;;;
;;; SRFI 39's make-parameter builds a parameter object with a default value and
;;; an optional converter that is applied whenever a value is installed;
;;; parameterize establishes a converted value for the dynamic extent of its
;;; body and restores the PREVIOUS (already-stored) value on exit -- crucially,
;;; without re-running the converter over it. This suite pins that behaviour: the
;;; default, dynamic scope and restore with no converter, the converter applied
;;; on set, and -- the conformant part this library guarantees itself -- that
;;; leaving a parameterize restores the saved value verbatim, so a non-idempotent
;;; converter neither re-fires on exit nor compounds across entries, including
;;; when the body unwinds non-locally.
;;; Copyright (c) 2026, hafod contributors.

;; make-parameter and parameterize are the forms under test; take them from the
;; library and everything else from chezscheme.
(import (test runner) (except (chezscheme) make-parameter parameterize) (hafod srfi-39))

(test-begin "srfi-39")

;; A parameter returns its default when called with no arguments.
(define p (make-parameter 10))
(test-equal "a parameter returns its default" 10 (p))

;; parameterize installs a value for its body and restores it afterwards.
;; (No converter here, so restore is unambiguous.)
(test-equal "parameterize establishes a dynamic value and restores it"
            '(10 20 10)
            (list (p) (parameterize ((p 20)) (p)) (p)))

;; A converter is applied when a value is SET: to the default, and to the value
;; installed by parameterize (observed inside the body).
(define c (make-parameter 5 (lambda (x) (* x 10))))
(test-equal "the converter is applied to the default" 50 (c))
(test-equal "the converter is applied to a value set by parameterize"
            30 (parameterize ((c 3)) (c)))

;; The conformant restore: leaving the body reinstalls the saved value verbatim,
;; it does NOT re-run the converter. After the parameterize above, c is back to
;; 50 -- not 500, which is what re-applying (* x 10) to the saved 50 would give.
(test-equal "restore reinstalls the saved value without re-running the converter"
            50 (c))

;; And that restore does not compound over repeated use: a second parameterize
;; converts only its own argument (4 -> 40) in the body and restores to 50 after,
;; never 500 or 5000.
(test-equal "a second parameterize converts only its own value"
            40 (parameterize ((c 4)) (c)))
(test-equal "repeated parameterize does not compound the converter" 50 (c))

;; A parameter with no converter round-trips its value through parameterize.
(define q (make-parameter 7))
(test-equal "a parameter with no converter round-trips through parameterize"
            '(7 99 7)
            (list (q) (parameterize ((q 99)) (q)) (q)))

;; Restore is escape-safe: a body that exits non-locally (here via a captured
;; continuation) still restores the saved value, and still without re-converting.
;; w's cell holds (+ 1 100) = 101; the body sees (+ 2 100) = 102; after the
;; escape it is back to 101, not 201.
(define w (make-parameter 1 (lambda (x) (+ x 100))))
(test-equal "a non-local exit restores the saved value without re-converting"
            '(102 101)
            (let ((seen #f))
              (call/cc
                (lambda (escape)
                  (parameterize ((w 2))
                    (set! seen (w))
                    (escape #f))))
              (list seen (w))))

;; The same holds when the body unwinds by raising: the dynamic extent ends and
;; the saved value is restored verbatim, not re-converted.
(define r (make-parameter 1 (lambda (x) (+ x 100))))
(test-equal "an exception unwinding the body restores the saved value"
            101
            (begin
              (guard (e (#t #f))
                (parameterize ((r 2)) (raise 'boom)))
              (r)))

(test-end)
