;;; test-srfi-8.ss -- Spot-check tests for the receive library (SRFI 8).
;;;
;;; SRFI 8's receive binds the multiple values produced by an expression to a
;;; formals list and evaluates its body with those bindings. This suite
;;; spot-checks a fixed formals list and a dotted (rest) formals list. The
;;; audit dispositioned this library conformant.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-8) (chezscheme))

(test-begin "srfi-8")

;; A fixed formals list binds each produced value in turn.
(test-equal "receive binds a fixed formals list from values"
            '(1 2) (receive (a b) (values 1 2) (list a b)))
(test-equal "receive makes the bound values usable in its body"
            3 (receive (a b) (values 1 2) (+ a b)))

;; A dotted formals list collects the surplus values as a list.
(test-equal "receive binds a dotted formals list, collecting the rest"
            '(1 (2 3)) (receive (head . tail) (values 1 2 3) (list head tail)))

;; A single value works too.
(test-equal "receive binds a single value"
            42 (receive (x) (values 42) x))

(test-end)
