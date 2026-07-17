;;; test-srfi-11.ss -- Spot-check tests for the let-values library (SRFI 11).
;;;
;;; SRFI 11's let-values binds the multiple values of each init expression in
;;; parallel; let*-values binds them sequentially so later inits can see the
;;; earlier bindings. hafod re-exports Chez's native forms. This suite
;;; spot-checks both. The audit dispositioned this library conformant.
;;; Copyright (c) 2026, hafod contributors.

;; These forms are Chez natives the library re-exports; take them from the
;; library under test and everything else from chezscheme.
(import (test runner) (except (chezscheme) let-values let*-values) (hafod srfi-11))

(test-begin "srfi-11")

;; let-values binds several multiple-value clauses in parallel.
(test-equal "let-values binds multiple values across clauses"
            '(1 2 3)
            (let-values (((a b) (values 1 2))
                         ((c) (values 3)))
              (list a b c)))

;; A dotted formals clause collects the surplus values.
(test-equal "let-values binds a dotted clause, collecting the rest"
            '(1 (2 3))
            (let-values (((head . tail) (values 1 2 3)))
              (list head tail)))

;; let*-values threads earlier bindings into later inits.
(test-equal "let*-values sees earlier bindings in later inits"
            '(5 10 15)
            (let*-values (((x) (values 5))
                          ((y z) (values (* x 2) (* x 3))))
              (list x y z)))

(test-end)
