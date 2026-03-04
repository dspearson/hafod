;;; Ported from scsh/test/strings-and-chars-test.scm
;;; Tests for file-name-sans-extension edge cases
;;; Original: inline test in strings-and-chars-test.scm. Ported to hafod test runner.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod))

(test-begin "scsh-strings")

;; 12 edge cases for file-name-sans-extension
;; Leading dot files should be treated as having no extension

(test-equal "sans-extension: .scm (leading dot, no extension)"
  ".scm"
  (file-name-sans-extension ".scm"))

(test-equal "sans-extension: /.scm (leading dot in root)"
  "/.scm"
  (file-name-sans-extension "/.scm"))

(test-equal "sans-extension: a/.scm (leading dot in subdir)"
  "a/.scm"
  (file-name-sans-extension "a/.scm"))

(test-equal "sans-extension: t.scm (normal extension)"
  "t"
  (file-name-sans-extension "t.scm"))

(test-equal "sans-extension: a/t.scm (path with extension)"
  "a/t"
  (file-name-sans-extension "a/t.scm"))

(test-equal "sans-extension: /a/t.scm (absolute path with extension)"
  "/a/t"
  (file-name-sans-extension "/a/t.scm"))

(test-equal "sans-extension: /a/b.c/t.scm (dir has dot, file has extension)"
  "/a/b.c/t"
  (file-name-sans-extension "/a/b.c/t.scm"))

(test-equal "sans-extension: empty string"
  ""
  (file-name-sans-extension ""))

(test-equal "sans-extension: t (no extension)"
  "t"
  (file-name-sans-extension "t"))

(test-equal "sans-extension: a/t (path, no extension)"
  "a/t"
  (file-name-sans-extension "a/t"))

(test-equal "sans-extension: /a/t (absolute, no extension)"
  "/a/t"
  (file-name-sans-extension "/a/t"))

(test-equal "sans-extension: /a/b.c/t (dir has dot, file has no extension)"
  "/a/b.c/t"
  (file-name-sans-extension "/a/b.c/t"))

(test-end)
