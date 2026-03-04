;;; Gap-fill tests for untested file-info and stat-info accessor symbols
;;; Phase 24 Plan 01 Task 2
;;; Copyright (c) 2026 Dominic Pearson.
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod fileinfo)
        (hafod fname)
        (hafod posix)
        (hafod compat)
        (except (chezscheme) vector-append open-input-file open-output-file getenv))

(test-begin "coverage-fileinfo")

;; Setup: create a temp file for testing
(let-values ([(path fd) (posix-mkstemp "/tmp/hafod-cov-fi-XXXXXX")])
  (posix-write fd (string->utf8 "test data"))
  (posix-close fd)

  ;; ======================================================================
  ;; file-info accessors (5 symbols)
  ;; ======================================================================
  (let ([fi (file-info path)])
    (test-assert "file-info:ctime returns integer"
      (integer? (file-info:ctime fi)))
    (test-assert "file-info:device returns integer"
      (integer? (file-info:device fi)))
    (test-assert "file-info:gid returns integer"
      (integer? (file-info:gid fi)))
    (test-assert "file-info:nlinks returns positive integer"
      (> (file-info:nlinks fi) 0))
    (test-assert "file-info:uid returns integer"
      (integer? (file-info:uid fi))))

  ;; ======================================================================
  ;; file-info type predicates (3 symbols)
  ;; ======================================================================
  (test-assert "file-info-socket? on regular file returns #f"
    (not (file-info-socket? (file-info path))))
  (test-assert "file-socket? on regular file returns #f"
    (not (file-socket? path)))
  (test-assert "file-special? on regular file returns #f"
    (not (file-special? path)))

  ;; ======================================================================
  ;; stat-info accessors via posix-stat (6 symbols)
  ;; ======================================================================
  (let ([si (posix-stat path)])
    (test-assert "stat-info-atime returns integer"
      (integer? (stat-info-atime si)))
    (test-assert "stat-info-ctime returns integer"
      (integer? (stat-info-ctime si)))
    (test-assert "stat-info-dev returns integer"
      (integer? (stat-info-dev si)))
    (test-assert "stat-info-gid returns integer"
      (integer? (stat-info-gid si)))
    (test-assert "stat-info-ino returns positive integer"
      (> (stat-info-ino si) 0))
    (test-assert "stat-info-mtime returns integer"
      (integer? (stat-info-mtime si))))

  ;; ======================================================================
  ;; fname (1 symbol)
  ;; ======================================================================
  (test-equal "file-name-extension-index finds dot"
    4 (file-name-extension-index "test.txt"))
  (test-equal "file-name-extension-index with no extension returns length"
    5 (file-name-extension-index "noext"))

  ;; Cleanup
  (posix-unlink path))

(test-end)
