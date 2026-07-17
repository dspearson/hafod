;;; test-srfi-6.ss -- Spot-check tests for the string-port library (SRFI 6).
;;;
;;; SRFI 6 provides basic string ports: open-output-string collects written
;;; output that get-output-string then returns as a string, and
;;; open-input-string reads a string as a port. hafod re-exports Chez's native
;;; implementations. This suite spot-checks the output round-trip and reading
;;; back both a datum and characters. The audit dispositioned this conformant.
;;; Copyright (c) 2026, hafod contributors.

;; These three names are Chez natives that the library re-exports; take them
;; from the library under test and everything else from chezscheme.
(import (test runner)
        (except (chezscheme) open-input-string open-output-string get-output-string)
        (hafod srfi-6))

(test-begin "srfi-6")

;; open-output-string collects everything written; get-output-string returns it.
(test-equal "an output string port accumulates written output"
            "hello 42"
            (let ((p (open-output-string)))
              (display "hello" p)
              (display #\space p)
              (display 42 p)
              (get-output-string p)))

;; open-input-string reads a datum back.
(test-equal "an input string port reads a datum"
            '(1 2 3) (read (open-input-string "(1 2 3)")))

;; open-input-string reads characters in sequence.
(test-equal "an input string port reads characters in order"
            '(#\x #\y #\z)
            (let ((ip (open-input-string "xyz")))
              (list (read-char ip) (read-char ip) (read-char ip))))

(test-end)
