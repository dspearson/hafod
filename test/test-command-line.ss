;; test-command-line.ss -- Tests for (hafod command-line)
;; Run with: scheme --libdirs .:src --script test/test-command-line.ss

(import (test runner) (hafod command-line))

(test-begin "hafod command-line")

;;; ========== command-line ==========
(test-assert "command-line returns a list"
  (list? (command-line)))

;;; ========== command-line-arguments ==========
(test-assert "command-line-arguments returns a list"
  (list? (command-line-arguments)))

;;; ========== set-command-line-args! ==========
(set-command-line-args! '("prog" "a" "b"))

(test-equal "command-line after set"
  '("prog" "a" "b")
  (command-line))

(test-equal "command-line-arguments after set"
  '("a" "b")
  (command-line-arguments))

;;; ========== arg ==========
(test-equal "arg 1"
  "x"
  (arg '("x" "y" "z") 1))

(test-equal "arg 2"
  "y"
  (arg '("x" "y" "z") 2))

(test-equal "arg 3"
  "z"
  (arg '("x" "y" "z") 3))

(test-equal "arg with default"
  "default"
  (arg '("x") 2 "default"))

(test-error "arg out of bounds"
  (arg '("x") 3))

;;; ========== arg* ==========
(test-equal "arg* 1"
  "x"
  (arg* '("x" "y") 1))

(test-equal "arg* with default thunk"
  "fallback"
  (arg* '("x") 2 (lambda () "fallback")))

(test-error "arg* out of bounds no default"
  (arg* '("x") 3))

;;; ========== argv ==========
(set-command-line-args! '("myprog" "first" "second"))

(test-equal "argv 0 (program name)"
  "myprog"
  (argv 0))

(test-equal "argv 1"
  "first"
  (argv 1))

(test-equal "argv 2"
  "second"
  (argv 2))

(test-equal "argv with default"
  "none"
  (argv 5 "none"))

(test-end)
