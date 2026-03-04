#!chezscheme
;;; test-system-accept.ss -- Acceptance tests: signals, state scoping, user/group, resource alignment
;;; Requirements: SYS-01, SYS-02, SYS-03, SYS-04
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (hafod signal) (hafod process-state) (hafod environment) (hafod user-group)
        (hafod process) (hafod procobj) (hafod posix) (hafod syntax)
        (hafod fd-ports) (hafod compat) (hafod rdelim)
        (except (chezscheme) exit vector-append open-input-file open-output-file getenv)
        (test runner))

(test-begin "System Operations Acceptance")

;; Helper: trim trailing newline from command output
(define (trim-newline s)
  (let ((len (string-length s)))
    (if (and (> len 0) (char=? (string-ref s (- len 1)) #\newline))
        (substring s 0 (- len 1))
        s)))

;; Helper: substring search
(define (string-contains haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

;; =============================================================================
;; Section 1: SYS-01 -- Signal delivery between parent and child
;; =============================================================================

;; 1. Parent sends SIGTERM to child, child terminates
(test-assert "signal: parent sends SIGTERM to child, child terminates"
  (let ([child (fork (lambda () (pause) (%exit 0)))])
    (signal-process child SIGTERM)
    (let ([status (wait child)])
      (eqv? (status:term-sig status) SIGTERM))))

;; 2. Parent sends SIGUSR1 to child
(test-assert "signal: parent sends SIGUSR1 to child"
  (let ([child (fork (lambda () (pause) (%exit 0)))])
    (signal-process child SIGUSR1)
    (let ([status (wait child)])
      (eqv? (status:term-sig status) SIGUSR1))))

;; 3. Signal-process with integer pid
(test-assert "signal: signal-process with integer pid"
  (let ([child (fork (lambda () (pause) (%exit 0)))])
    (signal-process (proc:pid child) SIGKILL)
    (let ([status (wait child)])
      (eqv? (status:term-sig status) SIGKILL))))

;; 4. Signal 0 checks process existence
(test-assert "signal: signal 0 checks process existence"
  (let ([child (fork (lambda () (pause) (%exit 0)))])
    ;; Child exists -- signal 0 should not raise
    (signal-process child 0)
    ;; Now kill and wait
    (signal-process child SIGKILL)
    (wait child)
    ;; After wait, child no longer exists -- signal 0 should raise
    (guard (e [#t #t])
      (signal-process (proc:pid child) 0)
      #f)))

;; 5. Kill background pipeline process
(test-assert "signal: kill background pipeline process"
  (let ([child (& (sleep "60"))])
    (and (proc? child)
         (begin
           (signal-process child SIGTERM)
           (let ([status (wait child)])
             (eqv? (status:term-sig status) SIGTERM))))))

;; =============================================================================
;; Section 2: SYS-02 -- Nested state scoping restores correctly
;; =============================================================================

;; 1. with-cwd restores after normal exit
(test-assert "with-cwd restores after normal exit"
  (let ([saved (cwd)])
    (let ([inside (with-cwd "/tmp" (cwd))])
      (and (string=? inside "/tmp")
           (string=? (cwd) saved)))))

;; 2. with-umask restores after normal exit
(test-assert "with-umask restores after normal exit"
  (let ([saved (umask)])
    (let ([inside (with-umask #o077 (umask))])
      (and (= inside #o077)
           (= (umask) saved)))))

;; 3. with-env restores after normal exit
(test-assert "with-env restores after normal exit"
  (and (not (getenv "HAFOD_TEST_SYS02"))
       (string=? "present"
                 (with-env '(("HAFOD_TEST_SYS02" . "present"))
                   (getenv "HAFOD_TEST_SYS02")))
       (not (getenv "HAFOD_TEST_SYS02"))))

;; 4. Nested: with-cwd + with-env + with-umask all restore
(test-assert "nested: with-cwd + with-env + with-umask all restore"
  (let ([saved-cwd (cwd)]
        [saved-umask (umask)])
    (let-values ([(c u e)
                  (with-cwd "/tmp"
                    (with-umask #o077
                      (with-env '(("HAFOD_NESTED" . "yes"))
                        (values (cwd) (umask) (getenv "HAFOD_NESTED")))))])
      (and (string=? c "/tmp")
           (= u #o077)
           (string=? e "yes")
           (string=? (cwd) saved-cwd)
           (= (umask) saved-umask)
           (not (getenv "HAFOD_NESTED"))))))

;; 5. Nested scoping restores on exception
(test-assert "nested scoping restores on exception"
  (let ([saved-cwd (cwd)]
        [saved-umask (umask)])
    (guard (e [#t #t])
      (with-cwd "/tmp"
        (with-umask #o077
          (error 'test "deliberate"))))
    (and (string=? (cwd) saved-cwd)
         (= (umask) saved-umask))))

;; 6. Deeply nested cwd scoping
(test-assert "deeply nested cwd scoping"
  (let ([saved (cwd)])
    (let ([r1
      (with-cwd "/"
        (let ([at-root (cwd)])
          (let ([r2
            (with-cwd "/tmp"
              (let ([at-tmp (cwd)])
                (let ([r3
                  (with-cwd "/usr"
                    (cwd))])
                  (list at-root at-tmp r3 (cwd)))))])
            (append r2 (list (cwd))))))])
      (and (string=? (list-ref r1 0) "/")
           (string=? (list-ref r1 1) "/tmp")
           (string=? (list-ref r1 2) "/usr")
           (string=? (list-ref r1 3) "/tmp")
           (string=? (list-ref r1 4) "/")
           (string=? (cwd) saved)))))

;; 7. with-total-env scoping restores entire environment
(test-assert "with-total-env scoping restores entire environment"
  (let ([saved-path (getenv "PATH")])
    (let ([inside (with-total-env '(("ONLY_VAR" . "1"))
                    (env->alist))])
      (and (= (length inside) 1)
           (string? (getenv "PATH"))
           (not (getenv "ONLY_VAR"))))))

;; =============================================================================
;; Section 3: SYS-03 -- User/group lookups by name and ID
;; =============================================================================

;; 1. user-info by name and uid are consistent
(test-assert "user-info by name and uid are consistent"
  (let ([pw-by-name (user-info "root")]
        [pw-by-uid  (user-info 0)])
    (and (string=? (passwd-info-name pw-by-name) "root")
         (= (passwd-info-uid pw-by-name) 0)
         (string=? (passwd-info-name pw-by-uid) "root")
         (= (passwd-info-uid pw-by-uid) 0))))

;; 2. ->uid and ->username roundtrip
(test-assert "->uid and ->username roundtrip"
  (and (= (->uid "root") 0)
       (string=? (->username 0) "root")
       (string=? (->username (->uid "root")) "root")))

;; 3. group-info by name and gid are consistent
(test-assert "group-info by name and gid are consistent"
  (let ([gr-by-name (group-info "root")]
        [gr-by-gid  (group-info 0)])
    (and (string=? (group-info-name gr-by-name) "root")
         (= (group-info-gid gr-by-name) 0)
         (string=? (group-info-name gr-by-gid) "root")
         (= (group-info-gid gr-by-gid) 0))))

;; 4. ->gid and ->groupname roundtrip
(test-assert "->gid and ->groupname roundtrip"
  (and (= (->gid "root") 0)
       (string=? (->groupname 0) "root")
       (string=? (->groupname (->gid "root")) "root")))

;; 5. user-info current user is consistent
(test-assert "user-info current user is consistent"
  (let ([pw (user-info (user-uid))])
    (and (string? (passwd-info-name pw))
         (> (string-length (passwd-info-name pw)) 0)
         (string? (passwd-info-dir pw))
         (= (passwd-info-uid pw) (user-uid)))))

;; 6. group-info current group is consistent
(test-assert "group-info current group is consistent"
  (let ([gr (group-info (user-gid))])
    (and (string? (group-info-name gr))
         (> (string-length (group-info-name gr)) 0)
         (= (group-info-gid gr) (user-gid)))))

;; 7. user-info home directory is valid string
(test-assert "user-info home directory is valid string"
  (let ([pw (user-info (user-uid))])
    (and (string? (passwd-info-dir pw))
         (> (string-length (passwd-info-dir pw)) 0)
         (string? (getenv "HOME"))
         (> (string-length (getenv "HOME")) 0))))

;; =============================================================================
;; Section 4: SYS-04 -- Resource alignment: forked child inherits scoped state
;; =============================================================================

;; 1. Fork inherits scoped cwd via resource alignment
(test-assert "fork inherits scoped cwd via resource alignment"
  (string=? "/tmp"
    (trim-newline
      (with-cwd "/tmp"
        (run/string (pwd))))))

;; 2. Fork inherits scoped env via resource alignment
(test-equal "fork inherits scoped env via resource alignment"
  "inherited\n"
  (with-env '(("HAFOD_FORK_TEST" . "inherited"))
    (run/string (sh "-c" "echo $HAFOD_FORK_TEST"))))

;; 3. Fork inherits scoped umask via resource alignment
(test-equal "fork inherits scoped umask via resource alignment"
  "0077\n"
  (with-umask #o077
    (run/string (sh "-c" "umask"))))

;; 4. Fork inherits all three scoped states simultaneously
(test-assert "fork inherits all three scoped states simultaneously"
  (let ([output
    (with-cwd "/tmp"
      (with-umask #o027
        (with-env '(("HAFOD_COMBO" . "yes"))
          (run/string (sh "-c" "echo CWD=$(pwd) UMASK=$(umask) ENV=$HAFOD_COMBO")))))])
    (and (string-contains output "CWD=/tmp")
         (string-contains output "UMASK=0027")
         (string-contains output "ENV=yes"))))

;; 5. Resource alignment does not leak scoped state after fork
(test-assert "resource alignment does not leak scoped state after fork"
  (let ([saved-cwd (cwd)]
        [saved-umask (umask)])
    (with-cwd "/tmp"
      (with-umask #o027
        (with-env '(("HAFOD_LEAK_TEST" . "scoped"))
          (let ([output (run/string (sh "-c" "echo ok"))])
            (void)))))
    (and (string=? (cwd) saved-cwd)
         (= (umask) saved-umask)
         (not (getenv "HAFOD_LEAK_TEST")))))

(test-end)
