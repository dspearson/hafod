;;; test-srfi-23.ss -- Spot-check tests for the error library (SRFI 23).
;;;
;;; hafod's error is Chez's R6RS error, whose shape is
;;; (error who message irritant …) -- a who-first form. This is a DELIBERATE
;;; divergence from SRFI 23's message-first (error message irritant …): hafod
;;; is R6RS throughout, and a message-first error would be inconsistent with
;;; (and collide with) the umbrella's error. This suite pins that hafod's error
;;; raises a catchable R6RS condition carrying who, message and irritants.
;;; Copyright (c) 2026, hafod contributors.

;; error is a Chez native the library re-exports; take it from the library
;; under test and the condition accessors from chezscheme.
(import (test runner) (except (chezscheme) error) (hafod srfi-23))

(test-begin "srfi-23")

;; error raises an exception.
(test-error "error raises an exception" (error 'a-procedure "something went wrong"))

;; The raised condition is a compound R6RS condition. Its who/message/irritants
;; fields carry exactly the who-first arguments hafod's error was given.
(test-equal "error's condition carries who (R6RS who-first shape, deliberate)"
            'a-procedure
            (guard (e ((who-condition? e) (condition-who e)) (#t 'no-who))
              (error 'a-procedure "failed" 1 2)))
(test-equal "error's condition carries the message"
            "failed"
            (guard (e ((message-condition? e) (condition-message e)) (#t 'no-message))
              (error 'a-procedure "failed" 1 2)))
(test-equal "error's condition carries the irritants"
            '(1 2)
            (guard (e ((irritants-condition? e) (condition-irritants e)) (#t 'no-irritants))
              (error 'a-procedure "failed" 1 2)))

(test-end)
