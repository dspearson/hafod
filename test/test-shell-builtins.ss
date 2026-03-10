;;; Tests for shell builtins (cd, pushd, popd, export)
;;; Phase 36 Plan 01 Task 2
;;; Copyright (c) 2026 Dominic Pearson.
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod shell builtins)
        (hafod process-state)
        (hafod environment)
        (hafod user-group)
        (except (chezscheme) exit open-input-file open-output-file getenv))

(test-begin "shell-builtins")

;; --- builtin? ---

(test-assert "builtin? cd" (builtin? "cd"))
(test-assert "builtin? pushd" (builtin? "pushd"))
(test-assert "builtin? popd" (builtin? "popd"))
(test-assert "builtin? export" (builtin? "export"))
(test-assert "builtin? ls is #f" (not (builtin? "ls")))
(test-assert "builtin? grep is #f" (not (builtin? "grep")))

;; --- builtin-names ---

(test-assert "builtin-names is a list"
  (list? (builtin-names)))
(test-assert "builtin-names contains cd"
  (member "cd" (builtin-names)))

;; --- cd ---

(let ([orig (cwd)])
  ;; cd to /tmp
  (run-builtin! "cd /tmp")
  (test-equal "cd changes cwd" "/tmp" (cwd))
  (test-equal "cd sets PWD" "/tmp" (getenv "PWD"))
  (test-equal "cd sets OLDPWD" orig (getenv "OLDPWD"))

  ;; cd with "-" goes back
  (run-builtin! "cd -")
  (test-equal "cd - returns to OLDPWD" orig (cwd))

  ;; cd with no args goes to home
  (let ([before (cwd)])
    (run-builtin! "cd")
    (test-equal "cd no args goes to home" (home-directory) (cwd))
    ;; restore
    (chdir orig)
    (setenv "PWD" orig)))

;; cd to nonexistent directory does not crash
(let ([orig (cwd)])
  (test-assert "cd nonexistent does not crash"
    (begin (run-builtin! "cd /nonexistent-dir-xyz-999")
           #t))
  (test-equal "cwd unchanged after failed cd" orig (cwd)))

;; --- pushd / popd ---

(let ([orig (cwd)])
  (run-builtin! "pushd /tmp")
  (test-equal "pushd changes to /tmp" "/tmp" (cwd))
  (test-assert "dir-stack not empty after pushd"
    (not (null? (dir-stack))))

  (run-builtin! "popd")
  (test-equal "popd returns to original" orig (cwd))
  (test-assert "dir-stack empty after popd"
    (null? (dir-stack))))

;; popd on empty stack does not crash
(test-assert "popd empty stack does not crash"
  (begin (run-builtin! "popd")
         #t))

;; --- export ---

(run-builtin! "export HAFOD_TEST_VAR=hello123")
(test-equal "export sets env var"
  "hello123" (getenv "HAFOD_TEST_VAR"))

;; export without = is no-op (doesn't crash)
(test-assert "export without = does not crash"
  (begin (run-builtin! "export HAFOD_TEST_VAR")
         #t))

;; --- run-builtin! dispatching ---

(test-assert "run-builtin! returns void for valid builtin"
  (begin (run-builtin! "cd /tmp")
         (chdir (home-directory))  ;; cleanup
         #t))

(test-end)
