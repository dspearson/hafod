;;; Tests for shell-mode tab completion
;;; Phase 36 Plan 03, Task 2
;;; Copyright (c) 2026 Dominic Pearson.
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor editor)
        (hafod shell classifier)
        (chezscheme))

(test-begin "shell-completion")

;; Initialize PATH cache for testing
(rebuild-path-cache!)

;; === shell-completions tests ===
;; shell-completions now returns (name . positions) pairs with fuzzy matching.

(test-equal "shell-completions: ls in results for prefix 'ls'"
  #t (and (assoc "ls" (shell-completions "ls")) #t))

(test-equal "shell-completions: nonexistent prefix returns empty"
  '() (shell-completions "zzz-nonexistent-cmd-xyz"))

(test-equal "shell-completions: empty prefix returns empty"
  '() (shell-completions ""))

(test-equal "shell-completions: grep in results for prefix 'gr'"
  #t (and (assoc "grep" (shell-completions "gr")) #t))

(test-assert "shell-completions: results are non-empty for common prefix"
  (> (length (shell-completions "l")) 1))

(test-end)
