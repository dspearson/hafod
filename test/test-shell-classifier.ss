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

;; === command-not-found gate ===

;; A bound Scheme identifier (procedure or variable) must never trigger a
;; command-not-found suggestion -- car/list are legal Scheme, not a typo.
(test-equal "suppress car (bound procedure)"
  #t (command-not-found-suppress? "car"))

(test-equal "suppress list (bound procedure)"
  #t (command-not-found-suppress? "list"))

(test-equal "suppress define (keyword)"
  #t (command-not-found-suppress? "define"))

(test-equal "suppress 42 (numeric literal)"
  #t (command-not-found-suppress? "42"))

(test-equal "suppress a string literal"
  #t (command-not-found-suppress? "\"s\""))

(test-equal "suppress the empty token"
  #t (command-not-found-suppress? ""))

;; A user-defined variable is bound in the interaction environment the REPL
;; evaluates in, so it is suppressed too.
(eval '(define zzboundvar 1) (interaction-environment))
(test-equal "suppress a user-defined variable"
  #t (command-not-found-suppress? "zzboundvar"))

;; A genuinely-unknown alphabetic token is still eligible for a suggestion.
(test-equal "do not suppress an unknown token"
  #f (command-not-found-suppress? "zzznotacommandxx"))

;; Suggestions are a capped list drawn from the PATH key-list.
(test-assert "suggestions for ls is a list"
  (list? (command-not-found-suggestions "ls")))

(test-assert "suggestions are capped at three"
  (<= (length (command-not-found-suggestions "ls")) 3))

;; === path-cache-keys cache ===

(test-assert "path-cache-keys returns a list"
  (list? (path-cache-keys)))

(test-assert "path-cache-keys length matches the cache size"
  (= (length (path-cache-keys)) (hashtable-size (path-cache))))

;; Memoised between rebuilds: two consecutive calls return the same object.
(test-assert "path-cache-keys is memoised between rebuilds"
  (eq? (path-cache-keys) (path-cache-keys)))

;; rebuild-path-cache! drops the memoised key-list, so the next call is fresh.
(test-assert "rebuild-path-cache! invalidates the key-list"
  (let ([a (path-cache-keys)])
    (rebuild-path-cache!)
    (not (eq? a (path-cache-keys)))))

(test-end)
