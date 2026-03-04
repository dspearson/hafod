;;; Ported from scsh/test/file-name-manipulation-test.scm
;;; Tests for filename manipulation functions (section 5.1 of scsh manual)
;;; Original author: Christoph Hetz. Ported to hafod test runner.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod))

(test-begin "scsh-file-names")

;; --- file-name-directory? ---
(test-assert "file-name-directory?"
  (and (not (file-name-directory? "src/des"))
       (file-name-directory? "src/des/")
       (file-name-directory? "/")
       (not (file-name-directory? "."))
       (file-name-directory? "")))

;; --- file-name-non-directory? ---
(test-assert "file-name-non-directory?"
  (and (file-name-non-directory? "src/des")
       (not (file-name-non-directory? "src/des/"))
       (not (file-name-non-directory? "/"))
       (file-name-non-directory? ".")
       (file-name-non-directory? "")))

;; --- file-name-as-directory ---
(test-equal "file-name-as-directory src/des" "src/des/"
  (file-name-as-directory "src/des"))
(test-equal "file-name-as-directory src/des/" "src/des/"
  (file-name-as-directory "src/des/"))
(test-equal "file-name-as-directory ." ""
  (file-name-as-directory "."))
(test-equal "file-name-as-directory /" "/"
  (file-name-as-directory "/"))
(test-equal "file-name-as-directory empty" "/"
  (file-name-as-directory ""))

;; --- directory-as-file-name ---
(test-equal "directory-as-file-name foo/bar/" "foo/bar"
  (directory-as-file-name "foo/bar/"))
(test-equal "directory-as-file-name foo/bar" "foo/bar"
  (directory-as-file-name "foo/bar"))
(test-equal "directory-as-file-name /" "/"
  (directory-as-file-name "/"))
(test-equal "directory-as-file-name empty" "."
  (directory-as-file-name ""))

;; --- file-name-absolute? ---
(test-assert "file-name-absolute?"
  (and (file-name-absolute? "/usr/shievers")
       (not (file-name-absolute? "src/des"))
       (file-name-absolute? "/src/des")
       (file-name-absolute? "")))

;; --- file-name-directory ---
(test-equal "file-name-directory /usr/bcd" "/usr/"
  (file-name-directory "/usr/bcd"))
(test-equal "file-name-directory /usr/bcd/" "/usr/bcd/"
  (file-name-directory "/usr/bcd/"))
(test-equal "file-name-directory bdc/.login" "bdc/"
  (file-name-directory "bdc/.login"))
(test-equal "file-name-directory main.c" ""
  (file-name-directory "main.c"))
(test-equal "file-name-directory /" ""
  (file-name-directory "/"))
(test-equal "file-name-directory empty" ""
  (file-name-directory ""))

;; --- file-name-nondirectory ---
(test-equal "file-name-nondirectory /usr/ian" "ian"
  (file-name-nondirectory "/usr/ian"))
(test-equal "file-name-nondirectory /usr/ian/" ""
  (file-name-nondirectory "/usr/ian/"))
(test-equal "file-name-nondirectory ian/.login" ".login"
  (file-name-nondirectory "ian/.login"))
(test-equal "file-name-nondirectory main.c" "main.c"
  (file-name-nondirectory "main.c"))
(test-equal "file-name-nondirectory empty" ""
  (file-name-nondirectory ""))
(test-equal "file-name-nondirectory /" "/"
  (file-name-nondirectory "/"))

;; --- split-file-name ---
(test-equal "split-file-name src/des/main.c"
  '("src" "des" "main.c")
  (split-file-name "src/des/main.c"))
(test-equal "split-file-name /src/des/main.c"
  '("" "src" "des" "main.c")
  (split-file-name "/src/des/main.c"))
(test-equal "split-file-name main.c"
  '("main.c")
  (split-file-name "main.c"))
(test-equal "split-file-name /"
  '("")
  (split-file-name "/"))

;; --- path-list->file-name ---
(test-equal "path-list->file-name basic"
  "src/des/main.c"
  (path-list->file-name '("src" "des" "main.c")))
(test-equal "path-list->file-name absolute"
  "/src/des/main.c"
  (path-list->file-name '("" "src" "des" "main.c")))
(test-equal "path-list->file-name with dir"
  "/usr/shivers/src/des/main.c"
  (path-list->file-name '("src" "des" "main.c") "/usr/shivers"))

;; --- file-name-extension ---
(test-equal "file-name-extension main.c" ".c"
  (file-name-extension "main.c"))
(test-equal "file-name-extension main.c.old" ".old"
  (file-name-extension "main.c.old"))
(test-equal "file-name-extension /usr/shivers" ""
  (file-name-extension "/usr/shivers"))
(test-equal "file-name-extension foo." "."
  (file-name-extension "foo."))
(test-equal "file-name-extension foo.." "."
  (file-name-extension "foo.."))
(test-equal "file-name-extension .login" ""
  (file-name-extension "/usr/shivers/.login"))

;; --- file-name-sans-extension ---
(test-equal "file-name-sans-extension main.c" "main"
  (file-name-sans-extension "main.c"))
(test-equal "file-name-sans-extension main.c.old" "main.c"
  (file-name-sans-extension "main.c.old"))
(test-equal "file-name-sans-extension /usr/shivers" "/usr/shivers"
  (file-name-sans-extension "/usr/shivers"))
(test-equal "file-name-sans-extension foo." "foo"
  (file-name-sans-extension "foo."))
(test-equal "file-name-sans-extension foo.." "foo."
  (file-name-sans-extension "foo.."))
(test-equal "file-name-sans-extension .login" "/usr/shivers/.login"
  (file-name-sans-extension "/usr/shivers/.login"))

;; --- parse-file-name ---
(test-assert "parse-file-name"
  (let* ((fname "/usr/shivers/main.c")
         (f (file-name-nondirectory fname)))
    (equal? (list (file-name-directory fname)
                  (file-name-sans-extension f)
                  (file-name-extension f))
            (call-with-values
              (lambda () (parse-file-name fname))
              (lambda (a b c) (list a b c))))))

;; --- replace-extension ---
(test-assert "replace-extension"
  (let ((fname "/usr/shivers/main.c")
        (ext "old"))
    (equal? (string-append (file-name-sans-extension fname) ext)
            (replace-extension fname ext))))

;; --- simplify-file-name ---
(test-equal "simplify-file-name normal"
  "/usr/shivers"
  (simplify-file-name "/usr/shivers"))
(test-equal "simplify-file-name multiple slashes"
  "/usr/shivers"
  (simplify-file-name "////usr//shivers/"))
(test-equal "simplify-file-name trailing dot"
  "/usr/shivers/."
  (simplify-file-name "////usr/shivers/."))
(test-equal "simplify-file-name double leading slash"
  "//usr/shivers"
  (simplify-file-name "//usr/shivers/"))
(test-equal "simplify-file-name dotdot"
  "/usr/shivers/../test"
  (simplify-file-name "////usr/shivers/../test/"))

;; --- resolve-file-name ---
(test-equal "resolve-file-name ~"
  (home-directory)
  (resolve-file-name "~"))
(test-assert "resolve-file-name ~/c/main.c returns string"
  (string? (resolve-file-name "~/c/main.c" "/usr/bin")))

;; --- expand-file-name ---
(test-equal "expand-file-name"
  (simplify-file-name (resolve-file-name "~/..///c/bin/main.out" "/usr/bin"))
  (expand-file-name "~/..///c/bin/main.out" "/usr/bin"))

;; --- absolute-file-name ---
(test-equal "absolute-file-name"
  "/usr/local/~/c/bin/c.out"
  (absolute-file-name "~/c/bin/c.out" "/usr/local"))

;; --- home-file ---
(test-equal "home-file man"
  (resolve-file-name "~/man")
  (home-file "man"))

(test-end)
