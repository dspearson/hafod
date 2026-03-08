(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod process-state) (hafod environment) (hafod posix) (hafod user-group) (hafod tty))

(test-begin "process-state")

;; ---- CWD ----
(test-assert "cwd returns string"
  (string? (cwd)))

(test-equal "cwd matches posix-getcwd"
  (posix-getcwd)
  (cwd))

;; chdir to / and back
(let ([orig (cwd)])
  (test-assert "chdir to /"
    (begin (chdir "/")
           (string=? "/" (cwd))))
  (chdir orig))

;; with-cwd scoping
(let ([orig (cwd)])
  (test-equal "with-cwd / returns /" "/"
    (with-cwd "/" (cwd)))
  (test-equal "with-cwd restores" orig (cwd)))

;; chdir no args goes home
(let ([orig (cwd)])
  (test-assert "chdir no args goes home"
    (begin (chdir)
           (string=? (home-directory) (cwd))))
  (chdir orig))

;; process-cwd returns raw OS value
(test-equal "process-cwd matches cwd"
  (cwd)
  (process-cwd))

;; ---- UMASK ----
(test-assert "umask returns integer"
  (integer? (umask)))

;; set-umask and restore
(let ([orig (umask)])
  (test-equal "set-umask changes" #o077
    (begin (set-umask #o077) (umask)))
  (set-umask orig))

;; with-umask scoping
(let ([orig (umask)])
  (test-equal "with-umask scopes" #o077
    (with-umask #o077 (umask)))
  (test-equal "with-umask restores" orig (umask)))

;; ---- PID ----
(test-equal "pid matches posix-getpid"
  (posix-getpid) (pid))

(test-equal "parent-pid matches posix-getppid"
  (posix-getppid) (parent-pid))

;; ---- Process Group ----
(test-equal "process-group matches posix-getpgrp"
  (posix-getpgrp) (process-group))

;; set-process-group two-arg form (no-op: set own pid to current pgid)
(test-assert "set-process-group two-arg no-op completes"
  (begin (set-process-group (pid) (process-group)) #t))

;; set-process-group one-arg form (create own group + restore)
(let ([orig (process-group)])
  (set-process-group (pid))
  (test-equal "set-process-group one-arg creates own group"
    (pid) (process-group))
  (set-process-group (pid) orig))

;; become-session-leader -- only check it's a procedure (calling it detaches terminal)
(test-assert "become-session-leader is a procedure"
  (procedure? become-session-leader))

;; TTY-guarded tests
(when (tty? 0)
  (test-assert "tty-process-group returns positive integer"
    (let ([pgid (tty-process-group 0)])
      (and (integer? pgid) (> pgid 0))))
  (test-assert "set-tty-process-group is a procedure"
    (procedure? set-tty-process-group)))

;; ---- UID/GID ----
(test-equal "user-uid matches posix-getuid"
  (posix-getuid) (user-uid))

(test-equal "user-effective-uid matches posix-geteuid"
  (posix-geteuid) (user-effective-uid))

(test-equal "user-gid matches posix-getgid"
  (posix-getgid) (user-gid))

(test-equal "user-effective-gid matches posix-getegid"
  (posix-getegid) (user-effective-gid))

(test-assert "user-supplementary-gids returns list"
  (let ([gids (user-supplementary-gids)])
    (and (list? gids)
         (pair? gids)
         (integer? (car gids)))))

;; ---- Resources ----
;; Test with-resources-aligned runs thunk after alignment
(test-assert "with-resources-aligned runs thunk"
  (with-resources-aligned '() (lambda () #t)))

;; Test cwd resource alignment
(test-assert "cwd-resource aligns"
  (with-resources-aligned (list cwd-resource) (lambda () #t)))

;; Test umask resource alignment
(test-assert "umask-resource aligns"
  (with-resources-aligned (list umask-resource) (lambda () #t)))

;; Test environ-resource alignment (from environment module -- cons pair)
(test-assert "environ-resource aligns"
  (with-resources-aligned (list environ-resource) (lambda () #t)))

;; Test euid/egid resource alignment
(test-assert "euid-resource aligns"
  (with-resources-aligned (list euid-resource) (lambda () #t)))

(test-assert "egid-resource aligns"
  (with-resources-aligned (list egid-resource) (lambda () #t)))

;; Test multiple resources
(test-assert "multiple resources align"
  (with-resources-aligned
    (list cwd-resource umask-resource euid-resource egid-resource environ-resource)
    (lambda () #t)))

;; Test resource record type
(test-assert "resource? works"
  (resource? cwd-resource))

(test-equal "resource-name works" 'cwd
  (resource-name cwd-resource))

(test-end)
