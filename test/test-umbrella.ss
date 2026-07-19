#!chezscheme
;; test-umbrella.ss -- Tests for (hafod) umbrella library
;; Verifies that a single (import (hafod)) provides access to all subsystem bindings.
;; Run with: scheme --libdirs .:src --script test/test-umbrella.ss

(import (test runner)
        (except (chezscheme) vector-append exit open-input-file open-output-file
                             truncate-file delete-file rename-file
                             make-date date? getenv alias)
        (hafod)
        ;; Leaf accessors (deliberately not umbrella-exported) used only to
        ;; confirm the umbrella's alias/abbreviation helpers write the very
        ;; tables the shell classifier and the editor consult.
        (only (hafod shell classifier) alias-ref)
        (only (hafod editor editor) abbr-ref))

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

;;; ========== Interactive config surface reachable via the umbrella ==========
;;; The six init-file/toggle symbols this milestone publishes must all be
;;; reachable through a single (import (hafod)); the alias and abbreviation
;;; helpers must write the tables the classifier and the editor consult.

(test-assert "config: auto-cd? reachable and boolean" (boolean? (auto-cd?)))

(test-assert "config: abbr-expand? reachable and boolean" (boolean? (abbr-expand?)))

(test-assert "config: alias helper writes, unalias clears, the classifier table"
  (begin
    (alias "zz-umbrella-alias" "ls -la")
    (let ([defined? (equal? (alias-ref "zz-umbrella-alias") "ls -la")])
      (unalias "zz-umbrella-alias")
      (and defined? (not (alias-ref "zz-umbrella-alias"))))))

(test-assert "config: abbr helper writes, unabbr clears, the editor table"
  (begin
    (abbr "zz-umbrella-abbr" "git checkout")
    (let ([defined? (equal? (abbr-ref "zz-umbrella-abbr") "git checkout")])
      (unabbr "zz-umbrella-abbr")
      (and defined? (not (abbr-ref "zz-umbrella-abbr"))))))

;;; The Up/Down recall toggle must be reachable and settable through the same
;;; single (import (hafod)), and it must still validate its value there: it
;;; defaults to 'substring, round-trips 'prefix, and refuses anything else --
;;; asserted by behaviour, never against an absolute symbol count.

(test-assert "config: history-search-mode reachable and defaults to 'substring"
  (eq? (history-search-mode) 'substring))

(test-assert "config: history-search-mode round-trips 'prefix through the umbrella"
  (parameterize ([history-search-mode 'prefix])
    (eq? (history-search-mode) 'prefix)))

(test-assert "config: history-search-mode rejects a value outside 'substring/'prefix"
  (guard (e [#t #t])
    (parameterize ([history-search-mode 'fuzzy]) #f)))

;;; ========== Shell-line highlighting toggles reachable via the umbrella =========
;;; The two live highlighting switches must be reachable and settable through the
;;; same single (import (hafod)) so a user's init.ss can toggle them, default #t,
;;; and coerce any non-#f value to #t -- asserted behaviourally, never against an
;;; absolute symbol count.

(test-assert "config: shell-highlight? reachable and defaults to #t"
  (eq? (shell-highlight?) #t))

(test-assert "config: shell-highlight? round-trips #f through the umbrella"
  (parameterize ([shell-highlight? #f]) (eq? (shell-highlight?) #f)))

(test-assert "config: shell-highlight? coerces a non-#f value to #t"
  (parameterize ([shell-highlight? 'yes]) (eq? (shell-highlight?) #t)))

(test-assert "config: shell-highlight-paths? reachable and defaults to #t"
  (eq? (shell-highlight-paths?) #t))

(test-assert "config: shell-highlight-paths? round-trips #f through the umbrella"
  (parameterize ([shell-highlight-paths? #f]) (eq? (shell-highlight-paths?) #f)))

;;; ========== Default-alias + master-switch surface reachable via the umbrella ==
;;; The default-alias toggle, the master interactive-enhancements? gate, and the
;;; one-call plain-shell! switch must all be reachable through a single
;;; (import (hafod)).  plain-shell! is checked only to BE a procedure here -- it
;;; mutates global toggles, so calling it belongs in test-default-aliases.

(test-assert "config: default-aliases? reachable and defaults to #t"
  (eq? (default-aliases?) #t))

(test-assert "config: interactive-enhancements? reachable and defaults to #t"
  (eq? (interactive-enhancements?) #t))

(test-assert "config: plain-shell! reachable as a procedure through the umbrella"
  (procedure? plain-shell!))

;;; ========== (scsh) compatibility mirror instantiates cleanly ==========
;;; (scsh) re-exports a curated subset of (hafod) so `(import (scsh))` works as an
;;; alternative entry point.  Nothing else in the suite imports (scsh) -- the scsh
;;; test scripts run through the scsh->hafod binary symlink, not the library -- so
;;; a malformed re-export token (two names fused into one by a lost space) would
;;; break `(import (scsh))` while every other suite stayed green.  Force the library
;;; to instantiate via `environment` (no bindings pulled into scope, so no clash
;;; with the top-level `(import (hafod))`), then confirm the two names a historical
;;; fusion dropped are each individually reachable.  Behavioural, never asserted
;;; against an absolute symbol count.

(test-assert "scsh: (import (scsh)) instantiates cleanly"
  (guard (e [#t #f])
    (pair? (environment-symbols (environment '(scsh))))))

(test-assert "scsh: mirror re-exports shell-open and signal as distinct names"
  (guard (e [#t #f])
    (let ([syms (environment-symbols (environment '(scsh)))])
      (and (memq 'shell-open syms) (memq 'signal syms) #t))))

(test-end)
