(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod shell parser)
        (hafod environment)
        (except (chezscheme) getenv))

;; Set up controlled environment for tests
(setenv "TEST_SHELL_VAR" "test-value")
(setenv "HOME" "/home/user")

(test-begin "Shell Parser")

;; === Simple commands ===

(test-equal "simple command"
  '(run (ls))
  (parse-shell-command "ls"))

(test-equal "command with argument"
  '(run (ls "-la"))
  (parse-shell-command "ls -la"))

(test-equal "command with multiple arguments"
  '(run (echo "hello" "world"))
  (parse-shell-command "echo hello world"))

;; === Pipes ===

(test-equal "simple pipe"
  '(run (pipe (ls) (grep "foo")))
  (parse-shell-command "ls | grep foo"))

(test-equal "multi-stage pipe"
  '(run (pipe (ls) (grep "foo") (wc "-l")))
  (parse-shell-command "ls | grep foo | wc -l"))

;; === Output redirections ===

(test-equal "output redirect"
  '(run (ls) (> "out.txt"))
  (parse-shell-command "ls > out.txt"))

(test-equal "append redirect"
  '(run (ls) (>> "log.txt"))
  (parse-shell-command "ls >> log.txt"))

;; === Input redirection ===

(test-equal "input redirect"
  '(run (sort) (< "data.txt"))
  (parse-shell-command "sort < data.txt"))

;; === Quoting ===

(test-equal "double-quoted string"
  '(run (echo "hello world"))
  (parse-shell-command "echo \"hello world\""))

(test-equal "single-quoted string"
  '(run (echo "hello world"))
  (parse-shell-command "echo 'hello world'"))

(test-equal "single-quoted no expansion"
  '(run (echo "$HOME"))
  (parse-shell-command "echo '$HOME'"))

;; === Environment variable expansion ===

(test-equal "env var expansion"
  '(run (echo "/home/user"))
  (parse-shell-command "echo $HOME"))

(test-equal "env var braces expansion"
  '(run (echo "/home/user"))
  (parse-shell-command "echo ${HOME}"))

(test-equal "unset env var"
  '(run (echo ""))
  (parse-shell-command "echo $NONEXISTENT_VAR_12345"))

(test-equal "env var in double quotes"
  '(run (echo "/home/user"))
  (parse-shell-command "echo \"$HOME\""))

(test-equal "custom env var"
  '(run (echo "test-value"))
  (parse-shell-command "echo $TEST_SHELL_VAR"))

;; === Backslash escaping ===

(test-equal "backslash escape space"
  '(run (echo "hello world"))
  (parse-shell-command "echo hello\\ world"))

;; === Glob expansion ===

;; For glob tests, create temp files we can control
(let ([dir "/tmp/hafod-parser-glob-test"])
  ;; Clean up any previous run
  (guard (e [#t #f])
    (for-each (lambda (f)
                (guard (e [#t #f])
                  (delete-file (string-append dir "/" f))))
              (guard (e [#t '()])
                (directory-list dir)))
    (guard (e [#t #f])
      (delete-directory dir)))

  (mkdir dir)
  (call-with-output-file (string-append dir "/alpha.tst") (lambda (p) (display "" p)))
  (call-with-output-file (string-append dir "/beta.tst") (lambda (p) (display "" p)))

  (let ([result (parse-shell-command (string-append "ls " dir "/*.tst"))])
    ;; Glob should expand to the two files (sorted by glob)
    ;; The result should contain both files as separate arguments
    (test-assert "glob expansion produces run form"
      (and (pair? result) (eq? (car result) 'run)))
    (test-assert "glob expansion has correct program"
      (eq? (caadr result) 'ls))
    (test-assert "glob expansion has 2 files"
      (= (length (cdadr result)) 2))
    (test-assert "glob expansion contains alpha.tst"
      (member (string-append dir "/alpha.tst") (cdadr result)))
    (test-assert "glob expansion contains beta.tst"
      (member (string-append dir "/beta.tst") (cdadr result))))

  ;; Test glob no match -- literal kept
  (test-equal "glob no match keeps literal"
    '(run (ls "*.nonexistent-extension-xyz"))
    (parse-shell-command "ls *.nonexistent-extension-xyz"))

  ;; Cleanup
  (guard (e [#t #f])
    (delete-file (string-append dir "/alpha.tst"))
    (delete-file (string-append dir "/beta.tst"))
    (delete-directory dir)))

;; === Pipe with redirect ===

(test-equal "pipe with redirect"
  '(run (pipe (ls) (grep "foo")) (> "out.txt"))
  (parse-shell-command "ls | grep foo > out.txt"))

;; === Program name is a symbol ===

(test-assert "program name is a symbol"
  (let ([form (parse-shell-command "ls -la")])
    (symbol? (caadr form))))

(test-assert "arguments are strings"
  (let ([form (parse-shell-command "ls -la")])
    (string? (cadadr form))))

;; === Edge cases ===

(test-equal "leading/trailing whitespace"
  '(run (ls))
  (parse-shell-command "  ls  "))

(test-equal "multiple spaces between args"
  '(run (echo "hello" "world"))
  (parse-shell-command "echo   hello   world"))

(test-equal "redirect with no space before filename"
  '(run (ls) (> "out.txt"))
  (parse-shell-command "ls >out.txt"))

(test-equal "multiple redirections"
  '(run (sort) (< "input.txt") (> "output.txt"))
  (parse-shell-command "sort < input.txt > output.txt"))

(test-end)
