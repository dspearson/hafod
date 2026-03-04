;;; Ported from scsh/test/process-state-tests.scm
;;; Tests for process state: umask, cwd (section 3.5 of scsh manual)
;;; Original author: David Frese. Ported to hafod test runner.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod))

(test-begin "scsh-process-state")

;; --- umask stuff ---

;; with-umask: set umask inside dynamic extent, verify restores
(test-assert "with-umask"
  (let ((old-umask (umask)))
    (and (with-umask 0
           (file-mode=? (umask) 0))
         (file-mode=? (umask) old-umask))))

;; set-umask: imperatively set, then restore
(test-assert "set-umask"
  (let ((old-umask (umask)))
    (set-umask #o007)
    (let ((res (umask)))
      (set-umask old-umask)
      (file-mode=? res #o007))))

;; --- cwd stuff ---

;; with-cwd: set cwd inside dynamic extent, verify restores
(test-assert "with-cwd"
  (let ((old-cwd (cwd)))
    (and (with-cwd "/"
           (equal? (cwd) "/"))
         (equal? (cwd) old-cwd))))

;; chdir: imperatively change cwd, then restore
(test-assert "chdir"
  (let ((old-cwd (cwd)))
    (chdir "/")
    (let ((res (cwd)))
      (chdir old-cwd)
      (equal? res "/"))))

(test-end)
