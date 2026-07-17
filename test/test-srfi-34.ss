;;; test-srfi-34.ss -- Spot-check tests for the exceptions library (SRFI 34).
;;;
;;; SRFI 34 provides guard (catching a raised object, dispatching on it), and
;;; with-exception-handler with raise / raise-continuable. hafod re-exports
;;; Chez's native R6RS forms. This suite spot-checks a guard catch-all, a guard
;;; dispatching on the raised object, and a raise-continuable that resumes with
;;; the handler's value. The audit dispositioned this library conformant.
;;; Copyright (c) 2026, hafod contributors.

;; These forms are Chez natives the library re-exports; take them from the
;; library under test and everything else from chezscheme.
(import (test runner)
        (except (chezscheme) guard with-exception-handler raise raise-continuable)
        (hafod srfi-34))

(test-begin "srfi-34")

;; guard catches a raised object.
(test-equal "guard catches a raised object"
            '(caught boom) (guard (e (#t (list 'caught e))) (raise 'boom)))

;; guard dispatches on the raised object with clause tests.
(test-equal "guard dispatches on the raised object"
            'symbol
            (guard (e ((symbol? e) 'symbol) ((number? e) 'number) (#t 'other))
              (raise 'oops)))
(test-equal "guard falls through to a later clause"
            'number
            (guard (e ((symbol? e) 'symbol) ((number? e) 'number) (#t 'other))
              (raise 99)))

;; raise-continuable calls the handler and resumes with its returned value.
(test-equal "raise-continuable resumes with the handler's value"
            43
            (with-exception-handler
             (lambda (e) 42)
             (lambda () (+ 1 (raise-continuable 'ignored)))))

;; raise escapes when nothing catches it locally.
(test-error "raise signals an exception" (raise 'unhandled))

(test-end)
