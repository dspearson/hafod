;;; (hafod internal posix-constants) -- POSIX numeric constants
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal posix-constants)
  (export
    ;; Open flags
    O_RDONLY O_WRONLY O_RDWR O_CREAT O_EXCL O_TRUNC O_APPEND O_NONBLOCK O_NOCTTY

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

    ;; ioctl constants
    TIOCSCTTY
    TIOCGWINSZ

    ;; Seek constants
    SEEK_SET SEEK_CUR SEEK_END

    ;; Access constants
    R_OK W_OK X_OK F_OK)

  (import (chezscheme) (hafod internal platform-constants))

  ;; ======================================================================
  ;; POSIX Constants
  ;; ======================================================================

  ;; Open flags — O_RDONLY/O_WRONLY/O_RDWR are universal;
  ;; the rest come from platform-constants.
  (define O_RDONLY     0)
  (define O_WRONLY     1)
  (define O_RDWR       2)
  (define O_CREAT     PLAT-O-CREAT)
  (define O_EXCL      PLAT-O-EXCL)
  (define O_TRUNC     PLAT-O-TRUNC)
  (define O_APPEND    PLAT-O-APPEND)
  (define O_NONBLOCK  PLAT-O-NONBLOCK)
  (define O_NOCTTY    PLAT-O-NOCTTY)

  ;; Signal constants — SIGHUP..SIGTERM (1-15) are universal on all
  ;; POSIX systems; signals >= 16 vary between Linux and macOS/BSD.
  (define SIGHUP     1)
  (define SIGINT     2)
  (define SIGQUIT    3)
  (define SIGILL     4)
  (define SIGTRAP    5)
  (define SIGABRT    6)
  (define SIGBUS     PLAT-SIGBUS)
  (define SIGFPE     8)
  (define SIGKILL    9)
  (define SIGUSR1    PLAT-SIGUSR1)
  (define SIGSEGV   11)
  (define SIGUSR2    PLAT-SIGUSR2)
  (define SIGPIPE   13)
  (define SIGALRM   14)
  (define SIGTERM   15)
  (define SIGCHLD    PLAT-SIGCHLD)
  (define SIGCONT    PLAT-SIGCONT)
  (define SIGSTOP    PLAT-SIGSTOP)
  (define SIGTSTP    PLAT-SIGTSTP)
  (define SIGTTIN    PLAT-SIGTTIN)
  (define SIGTTOU    PLAT-SIGTTOU)
  (define SIGURG     PLAT-SIGURG)
  (define SIGXCPU    PLAT-SIGXCPU)
  (define SIGXFSZ    PLAT-SIGXFSZ)
  (define SIGVTALRM  PLAT-SIGVTALRM)
  (define SIGPROF    PLAT-SIGPROF)
  (define SIGWINCH   PLAT-SIGWINCH)
  (define SIGIO      PLAT-SIGIO)
  (define SIGPWR    30)
  (define SIGSYS     PLAT-SIGSYS)

  ;; File mode bits
  (define S_IFMT   #xF000)
  (define S_IFDIR  #x4000)
  (define S_IFREG  #x8000)
  (define S_IFLNK  #xA000)
  (define S_IFIFO  #x1000)
  (define S_IFSOCK #xC000)
  (define S_IFBLK  #x6000)
  (define S_IFCHR  #x2000)
  (define S_ISUID  #o4000)
  (define S_ISGID  #o2000)
  (define S_ISVTX  #o1000)
  (define S_IRUSR  #o0400)
  (define S_IWUSR  #o0200)
  (define S_IXUSR  #o0100)
  (define S_IRGRP  #o0040)
  (define S_IWGRP  #o0020)
  (define S_IXGRP  #o0010)
  (define S_IROTH  #o0004)
  (define S_IWOTH  #o0002)
  (define S_IXOTH  #o0001)

  ;; fcntl constants
  (define F_GETFD PLAT-F-GETFD)
  (define F_SETFD PLAT-F-SETFD)
  (define FD_CLOEXEC PLAT-FD-CLOEXEC)
  (define F_GETFL PLAT-F-GETFL)
  (define F_SETFL PLAT-F-SETFL)

  ;; ioctl constants
  (define TIOCSCTTY PLAT-TIOCSCTTY)
  (define TIOCGWINSZ PLAT-TIOCGWINSZ)

  ;; Seek constants
  (define SEEK_SET 0)
  (define SEEK_CUR 1)
  (define SEEK_END 2)

  ;; Access constants
  (define R_OK 4)
  (define W_OK 2)
  (define X_OK 1)
  (define F_OK 0)

  ) ; end library
