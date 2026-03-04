;;; (hafod posix) -- POSIX FFI facade
;;; Re-exports all POSIX bindings from focused internal sub-modules.
;;; See src/hafod/internal/posix-*.ss for implementations.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod posix)
  (export
    ;; Re-export errno infrastructure
    &posix-error make-posix-error posix-error? posix-errno posix-syscall
    raise-posix-error posix-call __errno_location c-strerror

    ;; Process syscalls
    posix-fork posix-_exit posix-exec posix-waitpid
    posix-pipe posix-dup posix-dup2 posix-close posix-open
    posix-read posix-write posix-kill posix-pause

    ;; Wait status macros
    status:exit-val status:term-sig status:stop-sig
    wait/poll wait/stopped-children

    ;; POSIX constants -- open flags
    O_RDONLY O_WRONLY O_RDWR O_CREAT O_EXCL O_TRUNC O_APPEND O_NONBLOCK

    ;; Signal constants
    SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGBUS SIGFPE SIGKILL
    SIGUSR1 SIGSEGV SIGUSR2 SIGPIPE SIGALRM SIGTERM SIGCHLD SIGCONT
    SIGSTOP SIGTSTP SIGTTIN SIGTTOU
    SIGURG SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGIO SIGPWR SIGSYS

    ;; File mode bits
    S_IFMT S_IFDIR S_IFREG S_IFLNK S_IFIFO S_IFSOCK S_IFBLK S_IFCHR
    S_ISUID S_ISGID S_ISVTX
    S_IRUSR S_IWUSR S_IXUSR S_IRGRP S_IWGRP S_IXGRP S_IROTH S_IWOTH S_IXOTH

    ;; fcntl constants
    F_GETFD F_SETFD FD_CLOEXEC F_GETFL F_SETFL

    ;; Seek constants
    SEEK_SET SEEK_CUR SEEK_END

    ;; Access constants
    R_OK W_OK X_OK F_OK

    ;; Helpers
    ptr->string strings->c-argv free-c-argv

    ;; posix_spawn fast path
    posix-spawnp posix-spawnp/pipe

    ;; Stat
    posix-stat posix-lstat posix-fstat
    stat-info? stat-info-type stat-info-dev stat-info-ino stat-info-mode
    stat-info-nlink stat-info-uid stat-info-gid stat-info-rdev stat-info-size
    stat-info-blksize stat-info-blocks stat-info-atime stat-info-mtime
    stat-info-ctime mode->type

    ;; Filesystem operations
    posix-chmod posix-fchmod posix-chown posix-fchown
    posix-truncate posix-ftruncate
    posix-link posix-symlink posix-readlink posix-unlink posix-rename
    posix-mkdir posix-rmdir
    posix-access posix-lseek posix-umask

    ;; Environment / directory
    posix-chdir posix-getcwd posix-getenv posix-setenv posix-unsetenv

    ;; Identity
    posix-getpid posix-getppid posix-getpgrp posix-setpgid posix-setsid
    posix-getuid posix-getgid posix-geteuid posix-getegid
    posix-setuid posix-setgid posix-seteuid posix-setegid

    ;; User/group database
    posix-getpwnam posix-getpwuid posix-getgrnam posix-getgrgid
    passwd-info? passwd-info-name passwd-info-passwd passwd-info-uid
    passwd-info-gid passwd-info-gecos passwd-info-dir passwd-info-shell
    group-info? group-info-name group-info-passwd group-info-gid group-info-members

    ;; Directory iteration
    posix-opendir posix-readdir posix-closedir

    ;; fcntl
    posix-fcntl

    ;; Supplementary groups
    posix-getgroups

    ;; Environment reading
    read-environ

    ;; Exec with environment
    posix-execve

    ;; Sleep
    posix-sleep

    ;; Temp files
    posix-mkstemp

    ;; utimes
    posix-utimes

    ;; fnmatch
    posix-fnmatch FNM_PERIOD FNM_PATHNAME FNM_NOESCAPE
    posix-glob-fast

    ;; Time
    posix-localtime posix-gmtime posix-mktime posix-strftime posix-time

    ;; Login / process-times / sysconf / gettimeofday
    posix-getlogin posix-times posix-sysconf posix-gettimeofday

    ;; Filesystem: mkfifo, fsync, sync
    posix-mkfifo posix-fsync posix-sync

    ;; System identification
    posix-uname

    ;; POSIX regex
    posix-regcomp posix-regexec posix-regfree posix-regerror
    REG_EXTENDED REG_ICASE REG_NOSUB REG_NEWLINE
    REG_NOTBOL REG_NOTEOL REG_NOMATCH

    ;; TTY / termios
    posix-tcgetattr posix-tcsetattr
    posix-isatty posix-ttyname posix-ctermid
    posix-tcsendbreak posix-tcdrain posix-tcflush posix-tcflow
    posix-tcsetpgrp posix-tcgetpgrp
    TCSANOW TCSADRAIN TCSAFLUSH
    TCIFLUSH TCOFLUSH TCIOFLUSH
    TCOOFF TCOON TCIOFF TCION)

  (import
    (hafod internal errno)
    (hafod internal posix-constants)
    (hafod internal posix-core)
    (hafod internal posix-file)
    (hafod internal posix-identity)
    (hafod internal posix-user)
    (hafod internal posix-time)
    (hafod internal posix-tty)
    (hafod internal posix-regex)
    (hafod internal posix-misc))

  ) ; end library
