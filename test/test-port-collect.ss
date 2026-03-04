(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod port-collect) (hafod rdelim) (hafod compat)
        (except (chezscheme) vector-append))

(test-begin "Port Collectors")

;; port->string
(test-equal "port->string reads all content"
  "hello world"
  (port->string (open-input-string "hello world")))

(test-equal "port->string on empty port returns empty string"
  ""
  (port->string (open-input-string "")))

(test-equal "port->string with newlines"
  "a\nb\nc\n"
  (port->string (open-input-string "a\nb\nc\n")))

;; port->string-list
(test-equal "port->string-list splits by newlines"
  '("a" "b" "c")
  (port->string-list (open-input-string "a\nb\nc\n")))

(test-equal "port->string-list empty input"
  '()
  (port->string-list (open-input-string "")))

(test-equal "port->string-list single line no trailing newline"
  '("hello")
  (port->string-list (open-input-string "hello")))

;; port->sexp-list
(test-equal "port->sexp-list reads multiple sexps"
  '(1 2 3)
  (port->sexp-list (open-input-string "1 2 3")))

(test-equal "port->sexp-list reads complex sexps"
  '((+ 1 2) "hello" #t)
  (port->sexp-list (open-input-string "(+ 1 2) \"hello\" #t")))

(test-equal "port->sexp-list empty input"
  '()
  (port->sexp-list (open-input-string "")))

;; port->list
(test-equal "port->list with read-line"
  '("x" "y")
  (port->list read-line (open-input-string "x\ny\n")))

(test-equal "port->list with read"
  '(a b c)
  (port->list read (open-input-string "a b c")))

;; port-fold
(test-equal "port-fold sums numbers"
  6
  (port-fold (open-input-string "1 2 3") read + 0))

(test-equal "port-fold with cons builds reversed list"
  '(3 2 1)
  (port-fold (open-input-string "1 2 3") read cons '()))

;; reduce-port is alias
(test-equal "reduce-port same as port-fold"
  6
  (reduce-port (open-input-string "1 2 3") read + 0))

(test-end)
