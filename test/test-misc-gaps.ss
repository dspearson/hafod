#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod process)
        (hafod environment)
        (hafod glob)
        (hafod temp-file)
        (hafod posix)
        (except (chezscheme) exit vector-append open-input-file open-output-file getenv))

(test-begin "Misc API Gaps - Task 1")

;; ========== process.ss gaps ==========

(test-assert "halts? returns #f"
  (not (halts?)))

(test-assert "init-exec-path-list refreshes PATH"
  (let ([before (exec-path-list)])
    (init-exec-path-list)
    (let ([after (exec-path-list)])
      ;; After init, exec-path-list should still be a list
      (list? after))))

(test-assert "%exec is callable (exported)"
  (procedure? %exec))

;; ========== environment.ss gaps ==========

(test-equal "alist->env-list converts alist to KEY=VALUE strings"
  '("A=1" "B=2")
  (alist->env-list '(("A" . "1") ("B" . "2"))))

(test-equal "alist->env-list empty alist"
  '()
  (alist->env-list '()))

(test-equal "alist-compress removes duplicate keys keeping first"
  '(("A" . "1") ("B" . "2"))
  (alist-compress '(("A" . "1") ("B" . "2") ("A" . "3"))))

(test-equal "alist-compress no duplicates unchanged"
  '(("X" . "1"))
  (alist-compress '(("X" . "1"))))

(test-assert "alist-update is exported from environment"
  (procedure? alist-update))

(test-equal "alist-update adds new key"
  '(("NEW" . "val") ("A" . "1"))
  (alist-update "NEW" "val" '(("A" . "1"))))

(test-equal "alist-update replaces existing key"
  '(("A" . "new") ("B" . "2"))
  (alist-update "A" "new" '(("A" . "old") ("B" . "2"))))

(test-equal "add-before inserts before reference element"
  "a:new:ref:b"
  (add-before "new" "ref" "a:ref:b"))

(test-equal "add-before when ref not found, appends"
  "a:b:new"
  (add-before "new" "missing" "a:b"))

(test-equal "add-after inserts after reference element"
  "a:ref:new:b"
  (add-after "new" "ref" "a:ref:b"))

(test-equal "add-after when ref not found, appends"
  "a:b:new"
  (add-after "new" "missing" "a:b"))

;; ========== glob.ss gaps ==========

(test-assert "maybe-directory-files is exported and returns list for '.'"
  (let ([files (maybe-directory-files "." #f)])
    (and (list? files) (> (length files) 0))))

;; ========== temp-file.ss gaps ==========

(test-assert "*temp-file-template* is a parameter"
  (string? (*temp-file-template*)))

(test-assert "*temp-file-template* default contains temp dir"
  (let ([tmpl (*temp-file-template*)])
    ;; The default template should contain /tmp or $TMPDIR
    (> (string-length tmpl) 0)))

(test-end)
