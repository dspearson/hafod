;;; (hafod pty) -- Pseudo-terminal (PTY) support for hafod
;;; Provides open-pty, fork-pty-session, pty/tty name conversion,
;;; and make-pty-generator for successive PTY pair allocation.
;;; Ported from scsh/scheme/pty.scm
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod pty)
  (export
    ;; Core PTY operations
    open-pty
    fork-pty-session

    ;; Name conversion (legacy BSD PTY names)
    pty-name->tty-name
    tty-name->pty-name

    ;; PTY generator
    make-pty-generator)

  (import (hafod internal base)
          (hafod posix)
          (hafod fd-ports)
          (hafod tty)
          (hafod process)
          (hafod process-state))

  ;; ======================================================================
  ;; FFI bindings for Unix98 PTY operations
  ;; ======================================================================

  (define c-posix-openpt (foreign-procedure "posix_openpt" (int) int))
  (define c-grantpt      (foreign-procedure "grantpt" (int) int))
  (define c-unlockpt     (foreign-procedure "unlockpt" (int) int))
  (define c-ptsname      (foreign-procedure "ptsname" (int) void*))

  ;; O_NOCTTY is not exported from (hafod posix), define locally.
  ;; Linux value: 0x100 (256)
  (define O_NOCTTY #x100)

  ;; ======================================================================
  ;; open-pty -- Allocate a PTY master/slave pair
  ;; ======================================================================

  ;; Returns two values: (values master-input-port slave-device-name)
  ;; The master port is created as an input port but the underlying fd is
  ;; open read+write, so dup->outport works for writing to it.
  (define (open-pty)
    (let ([master-fd (c-posix-openpt (bitwise-ior O_RDWR O_NOCTTY))])
      (when (= master-fd -1)
        (let ([err (foreign-ref 'int (__errno_location) 0)])
          (raise-posix-error 'open-pty err)))
      ;; Grant access to slave and unlock it
      (let ([grant-result (c-grantpt master-fd)])
        (when (= grant-result -1)
          (let ([err (foreign-ref 'int (__errno_location) 0)])
            (posix-close master-fd)
            (raise-posix-error 'open-pty/grantpt err))))
      (let ([unlock-result (c-unlockpt master-fd)])
        (when (= unlock-result -1)
          (let ([err (foreign-ref 'int (__errno_location) 0)])
            (posix-close master-fd)
            (raise-posix-error 'open-pty/unlockpt err))))
      ;; Get slave device name
      (let ([slave-ptr (c-ptsname master-fd)])
        (when (= slave-ptr 0)
          (let ([err (foreign-ref 'int (__errno_location) 0)])
            (posix-close master-fd)
            (raise-posix-error 'open-pty/ptsname err)))
        (let ([slave-name (ptr->string slave-ptr)])
          ;; Wrap master fd as an input port (fd is open read+write)
          (values (fdes->inport master-fd) slave-name)))))

  ;; ======================================================================
  ;; fork-pty-session -- Fork with PTY as child's controlling terminal
  ;; ======================================================================

  ;; Forks a child process. The child gets the PTY slave as its controlling
  ;; terminal and stdin/stdout/stderr. Returns (values pty-port proc) to parent.
  ;;
  ;; The child:
  ;;   1. Closes master port (parent side)
  ;;   2. Becomes a session leader (setsid)
  ;;   3. Opens slave device and makes it the controlling terminal
  ;;   4. Dups slave fd to fds 0, 1, 2 (stdin/stdout/stderr)
  ;;   5. Rebinds Scheme stdio ports
  ;;   6. Runs the thunk
  (define (fork-pty-session thunk)
    (let-values ([(master-port slave-name) (open-pty)])
      ;; Open slave fd directly for the child (we need the raw fd)
      (let ([slave-fd (posix-open slave-name O_RDWR 0)])
        (let ([proc (fork
                      (lambda ()
                        ;; Child process
                        (close master-port)
                        (become-session-leader)
                        (make-control-tty slave-fd)
                        ;; Set up stdin (fd 0) as input port from slave
                        (evict-ports 0)
                        (posix-dup2 slave-fd 0)
                        (make-input-fdport 0 1)
                        ;; Set up stdout (fd 1) as output port from slave
                        (evict-ports 1)
                        (posix-dup2 slave-fd 1)
                        (make-output-fdport 1 1)
                        ;; Set up stderr (fd 2) as output port from slave
                        (evict-ports 2)
                        (posix-dup2 slave-fd 2)
                        (make-output-fdport 2 1)
                        ;; Close the original slave fd (now duped to 0/1/2)
                        (posix-close slave-fd)
                        ;; Rebind Scheme stdio ports to new fds
                        (with-stdio-ports* thunk)))])
          ;; Parent: close slave fd, return master + proc
          (posix-close slave-fd)
          (values master-port proc)))))

  ;; ======================================================================
  ;; PTY/TTY name conversion (legacy BSD naming)
  ;; ======================================================================

  ;; Convert "/dev/ptyXX" to "/dev/ttyXX" (change char at position 5)
  (define (pty-name->tty-name name)
    (let ([s (string-copy name)])
      (string-set! s 5 #\t)
      s))

  ;; Convert "/dev/ttyXX" to "/dev/ptyXX" (change char at position 5)
  (define (tty-name->pty-name name)
    (let ([s (string-copy name)])
      (string-set! s 5 #\p)
      s))

  ;; ======================================================================
  ;; make-pty-generator -- Generate successive PTY pairs
  ;; ======================================================================

  ;; Returns a thunk. Each invocation calls open-pty and returns
  ;; (values master-port slave-name). If no more PTYs are available,
  ;; returns #f.
  ;; On modern Linux with Unix98 PTYs, allocation is dynamic so exhaustion
  ;; is rare (limited by /proc/sys/kernel/pty/max).
  (define (make-pty-generator)
    (lambda ()
      (guard (e [#t #f])
        (open-pty))))

) ; end library
