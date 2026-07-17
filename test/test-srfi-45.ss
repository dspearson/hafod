;;; test-srfi-45.ss -- Conformance tests for the lazy library (SRFI 45).
;;;
;;; eager must evaluate its argument EAGERLY -- the spec makes (eager expr)
;;; equivalent to (let ((value expr)) (delay value)), so any side effect runs at
;;; construction, before force. hafod expanded eager to (delay expr), deferring
;;; the effect until force; the marker-before-force assertion reddened on the
;;; pre-fix tree. lazy keeps hafod's Chez-promise representation (a documented
;;; deliberate divergence -- see the source note).
;;; Copyright (c) 2026, hafod contributors.

;; delay/force come from the lazy library (identical to Chez's); take everything
;; else from chezscheme.
(import (test runner) (except (chezscheme) delay force) (hafod srfi-45))

(test-begin "srfi-45")

;; ===========================================================================
;; Section A -- eager evaluates its argument at construction
;; ===========================================================================
;; The body sets a marker as a side effect. Post-fix the marker is already set
;; the moment the eager promise is constructed; pre-fix it stayed #f until force.

(define marker #f)
(define eager-promise (eager (begin (set! marker 'ran) 42)))

(test-equal "eager runs its argument at construction, before any force"
            'ran marker)
(test-equal "forcing an eager promise returns the value" 42 (force eager-promise))

;; ===========================================================================
;; Section B -- delay/force and lazy spot-checks
;; ===========================================================================

(test-equal "force of a delayed expression evaluates it" 3 (force (delay (+ 1 2))))
(test-equal "a lazy promise wrapping a delayed value forces through to the value"
            5 (force (lazy (delay 5))))

;; delay is itself lazy: a side effect in a delayed body must NOT run until force.
(define delay-marker #f)
(define delayed (delay (begin (set! delay-marker 'forced) 7)))
(test-equal "a delayed body does not run before force" #f delay-marker)
(test-equal "forcing the delayed body runs it and returns the value" 7 (force delayed))
(test-equal "the delayed side effect has now run" 'forced delay-marker)

(test-end)
