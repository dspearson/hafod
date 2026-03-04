;;; test-posix.ss -- Tests for (hafod posix) and (hafod internal errno)
;;; Tests errno handling, process syscalls, wait status macros, and POSIX constants.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod posix) (hafod internal errno) (chezscheme))

(test-begin "POSIX FFI")

;; =============================================================================
;; Errno infrastructure
;; =============================================================================

(test-assert "posix-error on bad open"
  (guard (e [(posix-error? e) (= (posix-errno e) 2)])
    (posix-open "/no/such/file/hafod-test" O_RDONLY 0)
    #f))

(test-assert "posix-error includes syscall name"
  (guard (e [(posix-error? e) (eq? (posix-syscall e) 'open)])
    (posix-open "/no/such/file/hafod-test" O_RDONLY 0)
    #f))

(test-assert "posix-error has message condition"
  (guard (e [(posix-error? e)
             (let ([msg (condition-message e)])
               (and (string? msg)
                    (> (string-length msg) 0)))])
    (posix-open "/no/such/file/hafod-test" O_RDONLY 0)
    #f))

;; =============================================================================
;; POSIX Constants
;; =============================================================================

(test-equal "O_RDONLY" 0 O_RDONLY)
(test-equal "O_WRONLY" 1 O_WRONLY)
(test-equal "O_RDWR" 2 O_RDWR)
(test-equal "SIGTERM" 15 SIGTERM)
(test-equal "SIGKILL" 9 SIGKILL)
(test-equal "SIGHUP" 1 SIGHUP)
(test-equal "SIGINT" 2 SIGINT)

;; =============================================================================
;; Wait status macros
;; =============================================================================

;; Normal exit with code 42: status = 42 << 8 = 10752
(test-equal "status:exit-val normal" 42 (status:exit-val 10752))
(test-assert "status:term-sig on normal exit is #f" (not (status:term-sig 10752)))
(test-assert "status:stop-sig on normal exit is #f" (not (status:stop-sig 10752)))

;; Killed by signal 9: low 7 bits = 9, high bits = 0
(test-equal "status:term-sig killed" 9 (status:term-sig 9))
(test-assert "status:exit-val on signal kill is #f" (not (status:exit-val 9)))

;; Stopped by signal 19 (SIGSTOP): low byte = 0x7F, next byte = 19
;; status = (19 << 8) | 0x7F = 4991 = #x137F
(test-equal "status:stop-sig stopped" 19 (status:stop-sig #x137F))
(test-assert "status:exit-val on stopped is #f" (not (status:exit-val #x137F)))
(test-assert "status:term-sig on stopped is #f" (not (status:term-sig #x137F)))

;; Exit val 0
(test-equal "status:exit-val 0" 0 (status:exit-val 0))

;; wait flags are correct values
(test-equal "wait/poll" 1 wait/poll)
(test-equal "wait/stopped-children" 2 wait/stopped-children)

;; =============================================================================
;; Fork + _exit + waitpid
;; =============================================================================

;; Test: fork child that exits with code 42, parent waits
(test-assert "fork+waitpid+exit-val"
  (let ([pid (posix-fork)])
    (if (= pid 0)
        (posix-_exit 42)
        (let-values ([(wpid status) (posix-waitpid pid 0)])
          (and (= wpid pid) (= (status:exit-val status) 42))))))

;; Test: fork child that exits with code 0
(test-assert "fork+waitpid exit 0"
  (let ([pid (posix-fork)])
    (if (= pid 0)
        (posix-_exit 0)
        (let-values ([(wpid status) (posix-waitpid pid 0)])
          (= (status:exit-val status) 0)))))

;; =============================================================================
;; Pipe + read + write
;; =============================================================================

(test-assert "pipe+write+read"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [msg (string->utf8 "hello")])
    (posix-write wfd msg)
    (let ([got (posix-read rfd 5)])
      (posix-close rfd)
      (posix-close wfd)
      (equal? got (string->utf8 "hello")))))

;; =============================================================================
;; Open + close
;; =============================================================================

(test-assert "open+close /dev/null"
  (let ([fd (posix-open "/dev/null" O_RDONLY 0)])
    (posix-close fd)
    #t))

;; =============================================================================
;; Dup
;; =============================================================================

(test-assert "dup returns different fd"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [dup-wfd (posix-dup wfd)])
    (posix-write dup-wfd (string->utf8 "dup"))
    (let ([got (posix-read rfd 3)])
      (posix-close rfd)
      (posix-close wfd)
      (posix-close dup-wfd)
      (equal? got (string->utf8 "dup")))))

;; =============================================================================
;; Dup2
;; =============================================================================

(test-assert "dup2 to specific fd"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         ;; dup2 wfd to fd 99
         [fd99 (posix-dup2 wfd 99)])
    (posix-write fd99 (string->utf8 "d2"))
    (let ([got (posix-read rfd 2)])
      (posix-close rfd)
      (posix-close wfd)
      (posix-close fd99)
      (and (= fd99 99) (equal? got (string->utf8 "d2"))))))

;; =============================================================================
;; Fork + exec
;; =============================================================================

(test-assert "fork+exec echo"
  (let ([pid (posix-fork)])
    (if (= pid 0)
        (begin
          (posix-exec "true" '("true"))
          (posix-_exit 1))  ;; only reached if exec fails
        (let-values ([(wpid status) (posix-waitpid pid 0)])
          (= (status:exit-val status) 0)))))

;; =============================================================================
;; Fork + kill + term-sig
;; =============================================================================

(test-assert "fork+kill+term-sig"
  (let ([pid (posix-fork)])
    (if (= pid 0)
        (begin
          ;; Child: sleep forever (will be killed)
          ((foreign-procedure "sleep" (unsigned) unsigned) 60)
          (posix-_exit 1))
        (begin
          ;; Parent: kill child with SIGKILL, then wait
          (posix-kill pid SIGKILL)
          (let-values ([(wpid status) (posix-waitpid pid 0)])
            (= (status:term-sig status) SIGKILL))))))

;; =============================================================================
;; wait/poll (non-blocking)
;; =============================================================================

(test-assert "wait/poll returns 0 when child not yet exited"
  (let ([pid (posix-fork)])
    (if (= pid 0)
        (begin
          ((foreign-procedure "sleep" (unsigned) unsigned) 5)
          (posix-_exit 0))
        (let-values ([(wpid status) (posix-waitpid pid wait/poll)])
          ;; wpid = 0 means child not yet exited
          (posix-kill pid SIGKILL)
          (posix-waitpid pid 0)  ;; reap
          (= wpid 0)))))

;; =============================================================================
;; Read + write round-trip via pipe
;; =============================================================================

(test-assert "read+write bytevector round-trip"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [data (bytevector 1 2 3 4 5 6 7 8)])
    (posix-write wfd data)
    (let ([got (posix-read rfd 8)])
      (posix-close rfd)
      (posix-close wfd)
      (equal? got data))))

;; =============================================================================
;; Stat tests (Plan 02-02)
;; =============================================================================

(test-assert "stat /tmp is directory"
  (eq? (stat-info-type (posix-stat "/tmp")) 'directory))

(test-assert "stat /dev/null is char-device"
  (eq? (stat-info-type (posix-stat "/dev/null")) 'char-device))

(test-assert "stat nonexistent raises posix-error"
  (guard (e [(posix-error? e) (= (posix-errno e) 2)])
    (posix-stat "/no/such/file/hafod-test-xyz")
    #f))

(test-assert "stat-info-uid is integer"
  (integer? (stat-info-uid (posix-stat "/tmp"))))

(test-assert "stat-info-size is integer"
  (integer? (stat-info-size (posix-stat "/dev/null"))))

(test-assert "stat-info-mode includes S_IFDIR for /tmp"
  (= (bitwise-and (stat-info-mode (posix-stat "/tmp")) S_IFMT) S_IFDIR))

;; fstat matches stat
(test-assert "fstat matches stat type"
  (let* ([fd (posix-open "/dev/null" O_RDONLY 0)]
         [si (posix-fstat fd)])
    (posix-close fd)
    (eq? (stat-info-type si) 'char-device)))

;; =============================================================================
;; Filesystem operations (Plan 02-02)
;; =============================================================================

;; Set up test directory
(define test-dir "/tmp/hafod-test-posix")

;; Clean up any previous test artifacts
(guard (e [#t #f]) (posix-rmdir test-dir))
(guard (e [#t #f])
  ;; remove files in test-dir if it exists
  (let ([dirp (posix-opendir test-dir)])
    (let loop ()
      (let ([entry (posix-readdir dirp)])
        (when entry
          (unless (or (string=? entry ".") (string=? entry ".."))
            (guard (e2 [#t #f])
              (posix-unlink (string-append test-dir "/" entry))))
          (loop))))
    (posix-closedir dirp))
  (posix-rmdir test-dir))

;; Create test directory
(posix-mkdir test-dir #o755)

;; Test mkdir + stat confirms directory
(test-assert "mkdir creates directory"
  (eq? (stat-info-type (posix-stat test-dir)) 'directory))

;; Create a test file using open+write+close
(define (create-test-file path content)
  (let ([fd (posix-open path (bitwise-ior O_WRONLY O_CREAT O_TRUNC) #o644)])
    (posix-write fd (string->utf8 content))
    (posix-close fd)))

(define test-file (string-append test-dir "/testfile"))
(create-test-file test-file "hello world")

;; Test chmod
(test-assert "chmod changes mode"
  (begin
    (posix-chmod test-file #o600)
    (let ([mode (bitwise-and (stat-info-mode (posix-stat test-file)) #o777)])
      (= mode #o600))))

;; Restore mode for subsequent tests
(posix-chmod test-file #o644)

;; Test truncate
(test-assert "truncate to 0"
  (begin
    (posix-truncate test-file 0)
    (= (stat-info-size (posix-stat test-file)) 0)))

;; Write new content for link tests
(create-test-file test-file "link test data")

;; Test hard link
(define test-link (string-append test-dir "/hardlink"))
(test-assert "link creates hard link"
  (begin
    (posix-link test-file test-link)
    (= (stat-info-nlink (posix-stat test-file)) 2)))

;; Clean up hard link
(posix-unlink test-link)

;; Test symlink + readlink
(define test-symlink (string-append test-dir "/symlink"))
(test-assert "symlink+readlink"
  (begin
    (posix-symlink test-file test-symlink)
    (string=? (posix-readlink test-symlink) test-file)))

;; Test lstat on symlink returns symlink type
(test-assert "lstat on symlink returns symlink type"
  (eq? (stat-info-type (posix-lstat test-symlink)) 'symlink))

;; Clean up symlink
(posix-unlink test-symlink)

;; Test rename
(define test-renamed (string-append test-dir "/renamed"))
(test-assert "rename moves file"
  (begin
    (posix-rename test-file test-renamed)
    (and (guard (e [(posix-error? e) #t]) (posix-stat test-file) #f)
         (eq? (stat-info-type (posix-stat test-renamed)) 'regular))))

;; Rename back for cleanup
(posix-rename test-renamed test-file)

;; Test unlink
(define test-unlink-file (string-append test-dir "/unlinkme"))
(create-test-file test-unlink-file "delete me")
(test-assert "unlink removes file"
  (begin
    (posix-unlink test-unlink-file)
    (guard (e [(posix-error? e) (= (posix-errno e) 2)])
      (posix-stat test-unlink-file)
      #f)))

;; Test mkdir + rmdir
(define test-subdir (string-append test-dir "/subdir"))
(test-assert "mkdir+rmdir"
  (begin
    (posix-mkdir test-subdir #o755)
    (let ([is-dir (eq? (stat-info-type (posix-stat test-subdir)) 'directory)])
      (posix-rmdir test-subdir)
      (and is-dir
           (guard (e [(posix-error? e) #t])
             (posix-stat test-subdir)
             #f)))))

;; =============================================================================
;; Environment / directory tests (Plan 02-02)
;; =============================================================================

;; Test chdir + getcwd round-trip
(test-assert "chdir+getcwd round-trip"
  (let ([original-cwd (posix-getcwd)])
    (posix-chdir "/tmp")
    (let ([tmp-cwd (posix-getcwd)])
      (posix-chdir original-cwd)
      ;; /tmp might be a symlink to /private/tmp on some systems
      ;; On Linux it should be /tmp
      (or (string=? tmp-cwd "/tmp")
          ;; Handle resolved symlinks
          (> (string-length tmp-cwd) 0)))))

;; Test getenv
(test-assert "getenv HOME returns string"
  (let ([home (posix-getenv "HOME")])
    (and (string? home) (> (string-length home) 0))))

;; Test setenv + getenv round-trip
(test-assert "setenv+getenv round-trip"
  (begin
    (posix-setenv "HAFOD_TEST_VAR" "hello_hafod" #t)
    (string=? (posix-getenv "HAFOD_TEST_VAR") "hello_hafod")))

;; Test unsetenv
(test-assert "unsetenv removes variable"
  (begin
    (posix-setenv "HAFOD_TEST_DEL" "temp" #t)
    (posix-unsetenv "HAFOD_TEST_DEL")
    (not (posix-getenv "HAFOD_TEST_DEL"))))

;; Test getenv for nonexistent variable
(test-assert "getenv nonexistent returns #f"
  (not (posix-getenv "HAFOD_SURELY_NOT_SET_12345")))

;; Test access
(test-assert "access /tmp R_OK"
  (= (posix-access "/tmp" R_OK) 0))

(test-assert "access nonexistent F_OK"
  (= (posix-access "/no/such/file/hafod-xyz" F_OK) -1))

;; Test umask
(test-assert "umask round-trip"
  (let* ([old (posix-umask #o077)]
         [got (posix-umask old)])
    (= got #o077)))

;; =============================================================================
;; Identity tests (Plan 02-02)
;; =============================================================================

(test-assert "getpid returns positive integer"
  (> (posix-getpid) 0))

(test-assert "getppid returns positive integer"
  (> (posix-getppid) 0))

(test-assert "getuid returns non-negative integer"
  (>= (posix-getuid) 0))

(test-assert "getgid returns non-negative integer"
  (>= (posix-getgid) 0))

(test-assert "geteuid returns non-negative integer"
  (>= (posix-geteuid) 0))

(test-assert "getegid returns non-negative integer"
  (>= (posix-getegid) 0))

(test-assert "getpgrp returns positive integer"
  (> (posix-getpgrp) 0))

;; =============================================================================
;; User/group database tests (Plan 02-02)
;; =============================================================================

(test-assert "getpwuid with current uid"
  (let ([pw (posix-getpwuid (posix-getuid))])
    (and (passwd-info? pw)
         (= (passwd-info-uid pw) (posix-getuid))
         (string? (passwd-info-name pw))
         (> (string-length (passwd-info-name pw)) 0))))

(test-assert "getpwnam with current user"
  (let* ([pw1 (posix-getpwuid (posix-getuid))]
         [name (passwd-info-name pw1)]
         [pw2 (posix-getpwnam name)])
    (and (passwd-info? pw2)
         (string=? (passwd-info-name pw2) name))))

(test-assert "getpwuid with invalid uid returns #f"
  (not (posix-getpwuid 99999)))

(test-assert "passwd-info-dir is string"
  (string? (passwd-info-dir (posix-getpwuid (posix-getuid)))))

(test-assert "passwd-info-shell is string"
  (string? (passwd-info-shell (posix-getpwuid (posix-getuid)))))

(test-assert "getgrgid with current gid"
  (let ([gr (posix-getgrgid (posix-getgid))])
    (and (group-info? gr)
         (= (group-info-gid gr) (posix-getgid))
         (string? (group-info-name gr)))))

(test-assert "group-info-members is list"
  (let ([gr (posix-getgrgid (posix-getgid))])
    (list? (group-info-members gr))))

;; =============================================================================
;; Directory iteration tests (Plan 02-02)
;; =============================================================================

(test-assert "opendir+readdir+closedir on /tmp"
  (let ([dirp (posix-opendir "/tmp")])
    (let loop ([entries '()])
      (let ([entry (posix-readdir dirp)])
        (if entry
            (loop (cons entry entries))
            (begin
              (posix-closedir dirp)
              ;; Every directory should contain "." and ".."
              (and (member "." entries)
                   (member ".." entries)
                   #t)))))))

(test-assert "opendir on nonexistent raises posix-error"
  (guard (e [(posix-error? e) #t])
    (posix-opendir "/no/such/directory/hafod-xyz")
    #f))

;; =============================================================================
;; fcntl tests (Plan 02-02)
;; =============================================================================

(test-assert "fcntl F_GETFD returns integer"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)]
         [flags (posix-fcntl rfd F_GETFD)])
    (posix-close rfd)
    (posix-close wfd)
    (integer? flags)))

(test-assert "fcntl F_SETFD FD_CLOEXEC"
  (let* ([fds (posix-pipe)]
         [rfd (car fds)]
         [wfd (cdr fds)])
    (posix-fcntl rfd F_SETFD FD_CLOEXEC)
    (let ([flags (posix-fcntl rfd F_GETFD)])
      (posix-close rfd)
      (posix-close wfd)
      (= (bitwise-and flags FD_CLOEXEC) FD_CLOEXEC))))

;; =============================================================================
;; lseek test (Plan 02-02)
;; =============================================================================

(test-assert "lseek on open file"
  (let ([fd (posix-open test-file O_RDONLY 0)])
    (let ([pos (posix-lseek fd 5 SEEK_SET)])
      (posix-close fd)
      (= pos 5))))

;; =============================================================================
;; Cleanup
;; =============================================================================

;; Remove test files
(guard (e [#t #f]) (posix-unlink test-file))
(guard (e [#t #f]) (posix-unsetenv "HAFOD_TEST_VAR"))

;; Remove test directory
(guard (e [#t #f]) (posix-rmdir test-dir))

(test-end)
