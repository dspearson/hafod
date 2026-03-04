;; test-fname.ss -- Comprehensive tests for (hafod fname) and (hafod fname-system)
;; Ported from scsh/test/file-name-manipulation-test.scm
;; Run with: scheme --libdirs .:src --script test/test-fname.ss

(import (test runner) (hafod fname) (hafod fname-system))

(test-begin "hafod fname + fname-system")

;;; ========== file-name-directory? ==========
(test-assert "file-name-directory? src/des"
  (not (file-name-directory? "src/des")))
(test-assert "file-name-directory? src/des/"
  (file-name-directory? "src/des/"))
(test-assert "file-name-directory? /"
  (file-name-directory? "/"))
(test-assert "file-name-directory? ."
  (not (file-name-directory? ".")))
(test-assert "file-name-directory? empty"
  (file-name-directory? ""))

;;; ========== file-name-non-directory? ==========
(test-assert "file-name-non-directory? src/des"
  (file-name-non-directory? "src/des"))
(test-assert "file-name-non-directory? src/des/"
  (not (file-name-non-directory? "src/des/")))
(test-assert "file-name-non-directory? /"
  (not (file-name-non-directory? "/")))
(test-assert "file-name-non-directory? ."
  (file-name-non-directory? "."))
(test-assert "file-name-non-directory? empty"
  (file-name-non-directory? ""))

;;; ========== file-name-as-directory ==========
(test-equal "file-name-as-directory src/des"
  "src/des/"
  (file-name-as-directory "src/des"))
(test-equal "file-name-as-directory src/des/"
  "src/des/"
  (file-name-as-directory "src/des/"))
(test-equal "file-name-as-directory ."
  ""
  (file-name-as-directory "."))
(test-equal "file-name-as-directory /"
  "/"
  (file-name-as-directory "/"))
(test-equal "file-name-as-directory empty"
  "/"
  (file-name-as-directory ""))

;;; ========== directory-as-file-name ==========
(test-equal "directory-as-file-name foo/bar/"
  "foo/bar"
  (directory-as-file-name "foo/bar/"))
(test-equal "directory-as-file-name foo/bar"
  "foo/bar"
  (directory-as-file-name "foo/bar"))
(test-equal "directory-as-file-name /"
  "/"
  (directory-as-file-name "/"))
(test-equal "directory-as-file-name empty"
  "."
  (directory-as-file-name ""))

;;; ========== ensure-file-name-is-directory ==========
(test-equal "ensure-file-name-is-directory empty"
  ""
  (ensure-file-name-is-directory ""))
(test-equal "ensure-file-name-is-directory foo"
  "foo/"
  (ensure-file-name-is-directory "foo"))

;;; ========== ensure-file-name-is-nondirectory ==========
(test-equal "ensure-file-name-is-nondirectory empty"
  ""
  (ensure-file-name-is-nondirectory ""))
(test-equal "ensure-file-name-is-nondirectory foo/"
  "foo"
  (ensure-file-name-is-nondirectory "foo/"))

;;; ========== file-name-absolute? ==========
(test-assert "file-name-absolute? /usr/shivers"
  (file-name-absolute? "/usr/shivers"))
(test-assert "file-name-absolute? src/des"
  (not (file-name-absolute? "src/des")))
(test-assert "file-name-absolute? /src/des"
  (file-name-absolute? "/src/des"))
(test-assert "file-name-absolute? empty"
  (file-name-absolute? ""))
(test-assert "file-name-absolute? ~/foo"
  (file-name-absolute? "~/foo"))

;;; ========== file-name-directory ==========
(test-equal "file-name-directory /usr/bcd"
  "/usr/"
  (file-name-directory "/usr/bcd"))
(test-equal "file-name-directory /usr/bcd/"
  "/usr/bcd/"
  (file-name-directory "/usr/bcd/"))
(test-equal "file-name-directory bdc/.login"
  "bdc/"
  (file-name-directory "bdc/.login"))
(test-equal "file-name-directory main.c"
  ""
  (file-name-directory "main.c"))
(test-equal "file-name-directory /"
  ""
  (file-name-directory "/"))
(test-equal "file-name-directory empty"
  ""
  (file-name-directory ""))

;;; ========== file-name-nondirectory ==========
(test-equal "file-name-nondirectory /usr/ian"
  "ian"
  (file-name-nondirectory "/usr/ian"))
(test-equal "file-name-nondirectory /usr/ian/"
  ""
  (file-name-nondirectory "/usr/ian/"))
(test-equal "file-name-nondirectory ian/.login"
  ".login"
  (file-name-nondirectory "ian/.login"))
(test-equal "file-name-nondirectory main.c"
  "main.c"
  (file-name-nondirectory "main.c"))
(test-equal "file-name-nondirectory empty"
  ""
  (file-name-nondirectory ""))
(test-equal "file-name-nondirectory /"
  "/"
  (file-name-nondirectory "/"))

;;; ========== split-file-name ==========
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

;;; ========== path-list->file-name ==========
(test-equal "path-list->file-name relative"
  "src/des/main.c"
  (path-list->file-name '("src" "des" "main.c")))
(test-equal "path-list->file-name absolute"
  "/src/des/main.c"
  (path-list->file-name '("" "src" "des" "main.c")))
(test-equal "path-list->file-name with root"
  "/usr/shivers/src/des/main.c"
  (path-list->file-name '("src" "des" "main.c") "/usr/shivers"))

;;; ========== file-name-extension ==========
(test-equal "file-name-extension main.c"
  ".c"
  (file-name-extension "main.c"))
(test-equal "file-name-extension main.c.old"
  ".old"
  (file-name-extension "main.c.old"))
(test-equal "file-name-extension /usr/shivers"
  ""
  (file-name-extension "/usr/shivers"))
(test-equal "file-name-extension foo."
  "."
  (file-name-extension "foo."))
(test-equal "file-name-extension foo.."
  "."
  (file-name-extension "foo.."))
(test-equal "file-name-extension dotfile"
  ""
  (file-name-extension "/usr/shivers/.login"))

;;; ========== file-name-sans-extension ==========
(test-equal "file-name-sans-extension main.c"
  "main"
  (file-name-sans-extension "main.c"))
(test-equal "file-name-sans-extension main.c.old"
  "main.c"
  (file-name-sans-extension "main.c.old"))
(test-equal "file-name-sans-extension /usr/shivers"
  "/usr/shivers"
  (file-name-sans-extension "/usr/shivers"))
(test-equal "file-name-sans-extension foo."
  "foo"
  (file-name-sans-extension "foo."))
(test-equal "file-name-sans-extension foo.."
  "foo."
  (file-name-sans-extension "foo.."))
(test-equal "file-name-sans-extension dotfile"
  "/usr/shivers/.login"
  (file-name-sans-extension "/usr/shivers/.login"))

;;; ========== parse-file-name ==========
(test-assert "parse-file-name"
  (let-values (((dir base ext) (parse-file-name "/usr/shivers/main.c")))
    (and (equal? dir "/usr/shivers/")
         (equal? base "main")
         (equal? ext ".c"))))

;;; ========== replace-extension ==========
(test-equal "replace-extension"
  "/usr/shivers/main.old"
  (replace-extension "/usr/shivers/main.c" ".old"))

;;; ========== simplify-file-name ==========
(test-equal "simplify-file-name normal"
  "/usr/shivers"
  (simplify-file-name "/usr/shivers"))
(test-equal "simplify-file-name multiple slashes"
  "/usr/shivers"
  (simplify-file-name "////usr//shivers/"))
(test-equal "simplify-file-name trailing dot preserved"
  "/usr/shivers/."
  (simplify-file-name "////usr/shivers/."))
(test-equal "simplify-file-name double slash preserved"
  "//usr/shivers"
  (simplify-file-name "//usr/shivers/"))
(test-equal "simplify-file-name dotdot preserved"
  "/usr/shivers/../test"
  (simplify-file-name "////usr/shivers/../test/"))

;;; ========== resolve-tilde-file-name ==========
(test-assert "resolve-tilde-file-name ~"
  (let ((home (getenv "HOME")))
    (equal? home (resolve-tilde-file-name "~"))))

(test-assert "resolve-tilde-file-name ~/foo"
  (let ((home (getenv "HOME")))
    (equal? (string-append home "/foo") (resolve-tilde-file-name "~/foo"))))

(test-equal "resolve-tilde-file-name non-tilde"
  "/usr/bin"
  (resolve-tilde-file-name "/usr/bin"))

;;; ========== resolve-file-name ==========
(test-assert "resolve-file-name ~"
  (equal? (getenv "HOME") (resolve-file-name "~")))

(test-assert "resolve-file-name ~/c/main.c"
  (string? (resolve-file-name "~/c/main.c" "/usr/bin")))

(test-equal "resolve-file-name absolute"
  "/usr/bin"
  (resolve-file-name "/usr/bin" "/tmp"))

(test-equal "resolve-file-name relative"
  "/tmp/foo"
  (resolve-file-name "foo" "/tmp"))

;;; ========== expand-file-name ==========
(test-assert "expand-file-name simplifies"
  (let ((result (expand-file-name "~/..//c/bin/main.out" "/usr/bin")))
    (equal? result (simplify-file-name (resolve-file-name "~/..//c/bin/main.out" "/usr/bin")))))

;;; ========== absolute-file-name ==========
(test-equal "absolute-file-name with tilde"
  "/usr/local/~/c/bin/c.out"
  (absolute-file-name "~/c/bin/c.out" "/usr/local"))

(test-equal "absolute-file-name absolute"
  "/usr/bin"
  (absolute-file-name "/usr/bin" "/tmp"))

;;; ========== substitute-env-vars ==========
(test-assert "substitute-env-vars $HOME"
  (let ((home (getenv "HOME")))
    (equal? (string-append home "/bin") (substitute-env-vars "$HOME/bin"))))

(test-assert "substitute-env-vars ${HOME}"
  (let ((home (getenv "HOME")))
    (equal? (string-append home "/bin") (substitute-env-vars "${HOME}/bin"))))

(test-equal "substitute-env-vars no vars"
  "/usr/bin"
  (substitute-env-vars "/usr/bin"))

(test-end)
