#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod syntax) (hafod collect) (hafod port-collect)
        (hafod process) (hafod procobj) (hafod posix) (hafod fd-ports) (hafod compat)
        (except (chezscheme) exit vector-append open-input-file open-output-file getenv))

(test-begin "High-Level Collectors")

;; ========== Procedural forms (*) ==========

;; run/string*
(test-equal "run/string* captures stdout"
  "hello\n"
  (run/string* (lambda () (exec-path "echo" "hello"))))

(test-equal "run/string* captures multi-line output"
  "a\nb\nc\n"
  (run/string* (lambda () (exec-path "printf" "a\nb\nc\n"))))

;; run/strings*
(test-equal "run/strings* returns list of lines"
  '("a" "b" "c")
  (run/strings* (lambda () (exec-path "printf" "a\nb\nc\n"))))

(test-equal "run/strings* single line"
  '("hello")
  (run/strings* (lambda () (exec-path "echo" "hello"))))

;; run/port*
(test-assert "run/port* returns an input port"
  (let ([p (run/port* (lambda () (exec-path "echo" "test")))])
    (let ([result (input-port? p)])
      (close p)
      result)))

(test-equal "run/port* port is readable"
  "test\n"
  (let ([p (run/port* (lambda () (exec-path "echo" "test")))])
    (let ([content (port->string p)])
      (close p)
      content)))

;; run/sexp*
(test-equal "run/sexp* reads one sexp"
  '(+ 1 2)
  (run/sexp* (lambda () (exec-path "echo" "(+ 1 2)"))))

(test-equal "run/sexp* reads number"
  42
  (run/sexp* (lambda () (exec-path "echo" "42"))))

;; run/sexps*
(test-equal "run/sexps* reads all sexps"
  '(1 2 3)
  (run/sexps* (lambda () (exec-path "echo" "1 2 3"))))

;; run/port+proc*
(test-assert "run/port+proc* returns port and proc"
  (receive (port proc) (run/port+proc* (lambda () (exec-path "echo" "hello")))
    (let ([ok (and (input-port? port) (proc? proc))])
      (close port)
      (wait proc)
      ok)))

(test-equal "run/port+proc* port has correct content"
  "hello\n"
  (receive (port proc) (run/port+proc* (lambda () (exec-path "echo" "hello")))
    (let ([content (port->string port)])
      (close port)
      (wait proc)
      content)))

;; run/collecting*
(test-assert "run/collecting* returns status and ports"
  (receive (status p1) (run/collecting* '(1) (lambda () (exec-path "echo" "collected")))
    (let ([ok (and (integer? status) (input-port? p1))])
      (close p1)
      ok)))

(test-equal "run/collecting* collects stdout content"
  "collected\n"
  (receive (status p1) (run/collecting* '(1) (lambda () (exec-path "echo" "collected")))
    (let ([content (port->string p1)])
      (close p1)
      content)))

;; run/collecting* with stderr (fd 2)
;; Use a shell command that writes to both stdout and stderr
(test-assert "run/collecting* collects from multiple fds"
  (receive (status p1 p2)
    (run/collecting* '(1 2)
      (lambda ()
        (exec-path "sh" "-c" "echo stdout-output; echo stderr-output >&2")))
    (let ([out (port->string p1)]
          [err (port->string p2)])
      (close p1)
      (close p2)
      (and (string=? out "stdout-output\n")
           (string=? err "stderr-output\n")))))

;; run/file*
(test-assert "run/file* returns a filename"
  (let ([fname (run/file* (lambda () (exec-path "echo" "file-content")))])
    (let ([ok (and (string? fname) (zero? (posix-access fname F_OK)))])
      (posix-unlink fname)
      ok)))

(test-equal "run/file* file has correct content"
  "file-content\n"
  (let ([fname (run/file* (lambda () (exec-path "echo" "file-content")))])
    (let ([p (open-input-file fname)])
      (let ([content (get-string-all p)])
        (close p)
        (posix-unlink fname)
        content))))

;; ========== Macro forms ==========

;; run/string macro
(test-equal "run/string macro captures stdout"
  "hello\n"
  (run/string (echo "hello")))

(test-equal "run/string macro with multi-word output"
  "hello world\n"
  (run/string (echo "hello" "world")))

;; run/strings macro
(test-equal "run/strings macro returns list of lines"
  '("a" "b" "c")
  (run/strings (printf "a\\nb\\nc\\n")))

;; run/port macro
(test-assert "run/port macro returns input port"
  (let ([p (run/port (echo "test"))])
    (let ([result (input-port? p)])
      (close p)
      result)))

(test-equal "run/port macro port readable"
  "test\n"
  (let ([p (run/port (echo "test"))])
    (let ([content (port->string p)])
      (close p)
      content)))

;; run/sexp macro
(test-equal "run/sexp macro reads sexp"
  '(+ 1 2)
  (run/sexp (echo "(+ 1 2)")))

;; run/sexps macro
(test-equal "run/sexps macro reads all sexps"
  '(1 2 3)
  (run/sexps (echo "1 2 3")))

;; run/port+proc macro
(test-assert "run/port+proc macro returns port and proc"
  (receive (port proc) (run/port+proc (echo "hello"))
    (let ([ok (and (input-port? port) (proc? proc))])
      (close port)
      (wait proc)
      ok)))

;; run/collecting macro
(test-equal "run/collecting macro collects stdout"
  "collected\n"
  (receive (status p1) (run/collecting (1) (echo "collected"))
    (let ([content (port->string p1)])
      (close p1)
      content)))

;; run/file macro
(test-assert "run/file macro returns filename"
  (let ([fname (run/file (echo "file-test"))])
    (let ([ok (string? fname)])
      (posix-unlink fname)
      ok)))

;; ========== Collectors with pipelines ==========

(test-equal "run/string with pipeline"
  "hello\n"
  (run/string (pipe (echo "hello") (cat))))

(test-equal "run/strings with pipeline"
  '("hello")
  (run/strings (pipe (echo "hello") (cat))))

;; ========== Collectors with begin (Scheme code) ==========

(test-equal "run/string with begin"
  "scheme-output"
  (run/string (begin (display "scheme-output"))))

(test-equal "run/strings with begin"
  '("line1" "line2")
  (run/strings (begin (display "line1\nline2\n"))))

(test-equal "run/sexp with begin"
  42
  (run/sexp (begin (write 42))))

(test-end)
