;;; Tests for shell builtins (cd, pushd, popd, export)
;;; Copyright (c) 2026 Dominic Pearson.

;; Resolve the compiled libraries by ABSOLUTE path, derived from the launch
;; directory while it is still the repo root. A compiled library's .so is
;; located lazily -- at the first reference to one of its bindings, not at
;; import time -- so a relative libdir would be resolved against whatever the
;; working directory happens to be at that moment. Several tests below chdir
;; (cd/pushd, and the cleanup that leaves cwd at the home directory), so a
;; later first-touch of a not-yet-loaded library would otherwise try to load
;; it from <home>/src and abort the suite. Pinning absolute paths here makes
;; library resolution independent of any in-suite cd.
(library-directories
  (let ([root (current-directory)])
    (list (cons (string-append root "/src") (string-append root "/src"))
          (cons root root))))
(import (test runner)
        (hafod shell builtins)
        (hafod shell parser)
        (only (hafod shell classifier) classify-input rebuild-path-cache!)
        (only (hafod process) init-exec-path-list)
        (hafod process-state)
        (hafod environment)
        (hafod user-group)
        (except (chezscheme) exit open-input-file open-output-file getenv))

(test-begin "shell-builtins")

;; === parse-command-words / tilde ===
;; The shared parser tokeniser splits and expands argument words. Pin HOME so
;; ~, ~/x and X=~/y expand deterministically, and restore it afterwards. Run
;; this before the cd tests below so the parser library is first exercised
;; while the working directory is still the launch directory (the test harness
;; resolves the compiled library from a relative --libdirs path).

(let ([old-home (getenv "HOME")])
  (setenv "HOME" "/home/user")

  (test-equal "parse-command-words plain args"
    '("cd" "/tmp") (parse-command-words "cd /tmp"))
  (test-equal "parse-command-words bare tilde -> HOME"
    '("cd" "/home/user") (parse-command-words "cd ~"))
  (test-equal "parse-command-words tilde slash -> HOME/foo"
    '("cd" "/home/user/foo") (parse-command-words "cd ~/foo"))
  (test-equal "parse-command-words $HOME expands"
    '("cd" "/home/user") (parse-command-words "cd $HOME"))
  (test-equal "parse-command-words tilde after = in assignment"
    '("export" "X=/home/user/y") (parse-command-words "export X=~/y"))
  (test-equal "parse-command-words mid-word tilde stays literal"
    '("echo" "a~b") (parse-command-words "echo a~b"))
  (test-equal "parse-command-words unknown ~user stays literal"
    '("cat" "~zznouser/x") (parse-command-words "cat ~zznouser/x"))
  (test-equal "parse-command-words quoted tilde stays literal"
    '("echo" "~") (parse-command-words "echo '~'"))
  (test-equal "parse-command-words several assignment words"
    '("export" "AA=1" "BB=2" "CC=3")
    (parse-command-words "export AA=1 BB=2 CC=3"))

  (setenv "HOME" old-home))

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

;; === builtin argument expansion ===
;; Builtin arguments now flow through the shared parser tokeniser, so $ and ~
;; expand and quotes are stripped exactly as for an external command.

;; Marquee: `export PATH=$PATH:/x` must preserve and extend $PATH, not store the
;; literal "$PATH:/x" (the removed builtins-local parser corrupted PATH here).
;; Save and restore the real PATH so the surrounding suite is unaffected.
(let ([saved-path (getenv "PATH")])
  (setenv "PATH" "/usr/bin:/bin")
  (run-builtin! "export PATH=$PATH:/opt/x")
  (test-equal "export PATH=$PATH:/opt/x preserves and extends PATH"
    "/usr/bin:/bin:/opt/x" (getenv "PATH"))
  (setenv "PATH" saved-path))

;; $-expansion in a cd argument
(let ([orig (cwd)])
  (setenv "ZZTESTDIR" "/tmp")
  (run-builtin! "cd $ZZTESTDIR")
  (test-equal "cd $ZZTESTDIR expands the variable" "/tmp" (cwd))
  (chdir orig)
  (setenv "PWD" orig))

;; === multi-arg export + leading whitespace ===

(run-builtin! "export AA=1 BB=2 CC=3")
(test-equal "export sets the first of several assignments" "1" (getenv "AA"))
(test-equal "export sets the second of several assignments" "2" (getenv "BB"))
(test-equal "export sets the third of several assignments" "3" (getenv "CC"))

(run-builtin! "   export DD=4")
(test-equal "export tolerates leading whitespace" "4" (getenv "DD"))

(let ([orig (cwd)])
  (run-builtin! "   cd /tmp")
  (test-equal "cd tolerates leading whitespace" "/tmp" (cwd))
  (chdir orig)
  (setenv "PWD" orig))

;; A bare trailing name marks an already-set variable exported without
;; clearing it, while a preceding assignment on the same line still applies.
(setenv "EE2" "existing")
(run-builtin! "export FF2=9 EE2")
(test-equal "export assigns the pair alongside a bare name" "9" (getenv "FF2"))
(test-equal "export leaves a bare already-set name untouched"
  "existing" (getenv "EE2"))

;; --- run-builtin! dispatching ---

(test-assert "run-builtin! returns void for valid builtin"
  (begin (run-builtin! "cd /tmp")
         (chdir (home-directory))  ;; cleanup
         #t))

;; === PATH cache refresh ===
;; A PATH-changing export re-reads $PATH and rebuilds the classifier's command
;; cache, so a command that has just become reachable classifies as a shell
;; command within the session. Build a temp dir holding an oddly-named file,
;; add it to PATH via export, and watch classify-input flip 'scheme -> 'shell.
;; Restore PATH and the caches and remove the temp artefacts even on failure.

(let* ([saved-path (getenv "PATH")]
       [tmpdir "/tmp/zzhafod-pathcache-test"]
       [cmdname "zzhafodnewcmd"]
       [cmdfile (string-append tmpdir "/" cmdname)])
  (define (cleanup-fs!)
    (when (file-exists? cmdfile) (delete-file cmdfile))
    (when (file-exists? tmpdir) (delete-directory tmpdir)))
  (cleanup-fs!)              ;; clear any leftover from a prior run
  (init-exec-path-list)      ;; sync the search list and cache to the real PATH
  (rebuild-path-cache!)
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (test-equal "unknown command classifies as scheme before export"
        'scheme (classify-input (string-append cmdname " --x")))
      (mkdir tmpdir)
      (call-with-output-file cmdfile (lambda (p) #t))
      (run-builtin! (string-append "export PATH=$PATH:" tmpdir))
      (test-equal "newly PATH-added command classifies as shell after export"
        'shell (classify-input (string-append cmdname " --x")))
      (test-assert "an unrelated export does not error"
        (begin (run-builtin! "export ZZUNRELATED=1") #t)))
    (lambda ()
      (cleanup-fs!)
      (setenv "PATH" saved-path)
      (init-exec-path-list)
      (rebuild-path-cache!))))

(test-end)
