;;; test-srfi-28.ss -- Spot-check tests for the format library (SRFI 28).
;;;
;;; SRFI 28 renders a control string with ~a (display), ~s (write), ~% (newline)
;;; and ~~ (a literal tilde), returning the formatted string. hafod re-exports
;;; Chez's native format, a superset of those directives. This suite pins each
;;; of the four directives. The audit dispositioned this library conformant.
;;; Copyright (c) 2026, hafod contributors.

;; format is a Chez native the library re-exports; take it from the library
;; under test and everything else from chezscheme.
(import (test runner) (except (chezscheme) format) (hafod srfi-28))

(test-begin "srfi-28")

;; ~a inserts a displayed object; ~s inserts a written one (strings quoted).
(test-equal "~a displays and ~s writes"
            "foo and \"bar\"" (format "~a and ~s" 'foo "bar"))
(test-equal "~a displays a number without quotes" "42" (format "~a" 42))
(test-equal "~s writes a string with its quotes" "\"hi\"" (format "~s" "hi"))

;; ~% inserts a newline.
(test-equal "~% inserts a newline" "line\n" (format "line~%"))

;; ~~ inserts a literal tilde.
(test-equal "~~ inserts a literal tilde" "100~" (format "100~~"))

;; The directives combine.
(test-equal "the directives combine in one control string"
            "k=\"v\"\n" (format "~a=~s~%" 'k "v"))

(test-end)
