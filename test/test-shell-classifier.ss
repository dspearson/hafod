;;; Tests for shell input classifier
;;; Phase 36 Plan 01 Task 1
;;; Copyright (c) 2026 Dominic Pearson.
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod shell classifier)
        (hafod process)
        (except (chezscheme) exit open-input-file open-output-file getenv))

(test-begin "shell-classifier")

;; Setup: build the path cache
(rebuild-path-cache!)

;; --- Scheme prefix characters ---

(test-equal "empty string -> scheme"
  'scheme (classify-input ""))

(test-equal "whitespace only -> scheme"
  'scheme (classify-input "  "))

(test-equal "(define x 1) -> scheme"
  'scheme (classify-input "(define x 1)"))

(test-equal "'hello -> scheme"
  'scheme (classify-input "'hello"))

(test-equal "#t -> scheme"
  'scheme (classify-input "#t"))

(test-equal ",pp -> scheme"
  'scheme (classify-input ",pp"))

(test-equal "`(list 1) -> scheme"
  'scheme (classify-input "`(list 1)"))

(test-equal "[vector -> scheme"
  'scheme (classify-input "[vector"))

;; --- Builtins ---

(test-equal "cd /tmp -> builtin"
  'builtin (classify-input "cd /tmp"))

(test-equal "pushd /tmp -> builtin"
  'builtin (classify-input "pushd /tmp"))

(test-equal "popd -> builtin"
  'builtin (classify-input "popd"))

(test-equal "export FOO=bar -> builtin"
  'builtin (classify-input "export FOO=bar"))

;; --- Shell (PATH executables) ---

(test-equal "ls -la -> shell"
  'shell (classify-input "ls -la"))

(test-equal "grep foo bar -> shell"
  'shell (classify-input "grep foo bar"))

;; --- Scheme keywords override PATH ---

(test-equal "define -> scheme (keyword)"
  'scheme (classify-input "define"))

(test-equal "let -> scheme (keyword)"
  'scheme (classify-input "let"))

(test-equal "lambda -> scheme (keyword)"
  'scheme (classify-input "lambda"))

;; --- Self-evaluating literals ---

(test-equal "42 -> scheme (number)"
  'scheme (classify-input "42"))

(test-equal "\"hello\" -> scheme (string)"
  'scheme (classify-input "\"hello\""))

;; --- Unknown -> scheme (default) ---

(test-equal "nonexistent-command-xyz -> scheme"
  'scheme (classify-input "nonexistent-command-xyz"))

;; --- Path cache ---

(test-assert "path-cache is a hashtable"
  (hashtable? (path-cache)))

(test-assert "rebuild-path-cache! populates cache"
  (> (hashtable-size (path-cache)) 0))

(test-end)
