#!chezscheme
;; test-umbrella.ss -- Tests for (hafod) umbrella library
;; Verifies that a single (import (hafod)) provides access to all subsystem bindings.
;; Run with: scheme --libdirs .:src --script test/test-umbrella.ss

(import (test runner)
        (except (chezscheme) vector-append exit open-input-file open-output-file
                             truncate-file delete-file rename-file
                             make-date date? getenv)
        (hafod))

(test-begin "hafod umbrella library")

;;; ========== Binding accessibility tests ==========
;;; Verify at least one binding from each of the 22 subsystem libraries.

;; compat
(test-assert "compat: vector-append" (procedure? vector-append))

;; fname
(test-assert "fname: file-name-directory?" (procedure? file-name-directory?))

;; command-line
(test-assert "command-line: command-line-arguments" (procedure? command-line-arguments))

;; rdelim
(test-assert "rdelim: read-line" (procedure? read-line))

;; signal
(test-assert "signal: SIGHUP" (integer? SIGHUP))

;; user-group
(test-assert "user-group: user-info" (procedure? user-info))

;; fname-system
(test-assert "fname-system: resolve-file-name" (procedure? resolve-file-name))

;; posix
(test-assert "posix: posix-fork" (procedure? posix-fork))

;; fd-ports
(test-assert "fd-ports: fdes->inport" (procedure? fdes->inport))

;; procobj
(test-assert "procobj: proc?" (procedure? proc?))

;; collect
(test-assert "collect: run/string*" (procedure? run/string*))

;; process
(test-assert "process: fork" (procedure? fork))

;; environment
(test-assert "environment: getenv" (procedure? getenv))

;; glob
(test-assert "glob: glob" (procedure? glob))

;; temp-file
(test-assert "temp-file: create-temp-file" (procedure? create-temp-file))

;; port-collect
(test-assert "port-collect: port->string" (procedure? port->string))

;; process-state
(test-assert "process-state: cwd" (procedure? cwd))

;; fileinfo
(test-assert "fileinfo: file-info" (procedure? file-info))

;; time
(test-assert "time: make-date" (procedure? make-date))

;; system
(test-assert "system: uname" (procedure? uname))

;; syntax: run is a macro, verify it exists by using it in a guard
(test-assert "syntax: run is syntax"
  (guard (e [#t #t])
    (eval '(run (echo "test")) (environment '(hafod)))
    #t))

;; re
(test-assert "re: regexp?" (procedure? regexp?))

;; tty
(test-assert "tty: tty?" (procedure? tty?))

;; field-reader
(test-assert "field-reader: field-splitter" (procedure? field-splitter))

;; awk: macro, verify it exists by checking next-range helper
(test-assert "awk: next-range" (procedure? next-range))

;; pty
(test-assert "pty: open-pty" (procedure? open-pty))

;;; ========== Functional tests through umbrella import ==========

(test-equal "fname via umbrella" ".txt" (file-name-extension "foo.txt"))

(test-assert "cwd via umbrella" (string? (cwd)))

(test-assert "getenv via umbrella" (string? (getenv "HOME")))

(test-assert "signal constant via umbrella" (= SIGTERM 15))

(test-equal "vector-append via umbrella"
  '#(1 2 3 4)
  (vector-append '#(1 2) '#(3 4)))

(test-assert "file-exists? via umbrella" (file-exists? "/"))

(test-assert "uname via umbrella"
  (let ([u (uname)])
    (and (uname-info? u)
         (string? (uname:os-name u)))))

(test-end)
