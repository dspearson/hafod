;;; (hafod internal posix-identity) -- Environment, CWD, and process identity syscalls
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal posix-identity)
  (export
    posix-chdir posix-getcwd posix-getenv posix-setenv posix-unsetenv
    posix-getpid posix-getppid posix-getpgrp posix-setpgid posix-setsid
    posix-getuid posix-getgid posix-geteuid posix-getegid
    posix-setuid posix-setgid posix-seteuid posix-setegid)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants) (hafod internal posix-core))

  ;; ======================================================================
  ;; Environment / working directory
  ;; ======================================================================

  (define c-chdir (foreign-procedure "chdir" (string) int))
  (define c-getcwd (foreign-procedure "getcwd" (void* size_t) void*))
  (define c-getenv (foreign-procedure "getenv" (string) void*))
  (define c-setenv (foreign-procedure "setenv" (string string int) int))
  (define c-unsetenv (foreign-procedure "unsetenv" (string) int))

  (define (posix-chdir path) (posix-call chdir (c-chdir path)))

  ;; getcwd: returns the current working directory as a string.
  (define (posix-getcwd)
    (with-foreign-buffer ([buf 4096])
      (let ([result (c-getcwd buf 4096)])
        (when (= result 0)
          (let ([err (foreign-ref 'int (__errno_location) 0)])
            (raise-posix-error 'getcwd err)))
        (ptr->string buf))))

  ;; getenv: returns the value of an environment variable, or #f if not set.
  (define (posix-getenv name)
    (let ([ptr (c-getenv name)])
      (if (= ptr 0) #f (ptr->string ptr))))

  (define (posix-setenv name value overwrite)
    (posix-call setenv (c-setenv name value (if overwrite 1 0))))

  (define (posix-unsetenv name)
    (posix-call unsetenv (c-unsetenv name)))

  ;; ======================================================================
  ;; Identity syscalls
  ;; ======================================================================

  (define c-getpid (foreign-procedure "getpid" () int))
  (define c-getppid (foreign-procedure "getppid" () int))
  (define c-getpgrp (foreign-procedure "getpgrp" () int))
  (define c-setpgid (foreign-procedure "setpgid" (int int) int))
  (define c-setsid (foreign-procedure "setsid" () int))
  (define c-getuid (foreign-procedure "getuid" () unsigned-32))
  (define c-getgid (foreign-procedure "getgid" () unsigned-32))
  (define c-geteuid (foreign-procedure "geteuid" () unsigned-32))
  (define c-getegid (foreign-procedure "getegid" () unsigned-32))
  (define c-setuid (foreign-procedure "setuid" (unsigned-32) int))
  (define c-setgid (foreign-procedure "setgid" (unsigned-32) int))
  (define c-seteuid (foreign-procedure "seteuid" (unsigned-32) int))
  (define c-setegid (foreign-procedure "setegid" (unsigned-32) int))

  (define (posix-getpid) (c-getpid))
  (define (posix-getppid) (c-getppid))
  (define (posix-getpgrp) (c-getpgrp))
  (define (posix-setpgid pid pgid) (posix-call setpgid (c-setpgid pid pgid)))
  (define (posix-setsid) (posix-call setsid (c-setsid)))
  (define (posix-getuid) (c-getuid))
  (define (posix-getgid) (c-getgid))
  (define (posix-geteuid) (c-geteuid))
  (define (posix-getegid) (c-getegid))
  (define (posix-setuid uid) (posix-call setuid (c-setuid uid)))
  (define (posix-setgid gid) (posix-call setgid (c-setgid gid)))
  (define (posix-seteuid uid) (posix-call seteuid (c-seteuid uid)))
  (define (posix-setegid gid) (posix-call setegid (c-setegid gid)))

  ) ; end library
