;;; (hafod internal posix-tty) -- TTY/termios operations and constants
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal posix-tty)
  (export
    TCSANOW TCSADRAIN TCSAFLUSH
    TCIFLUSH TCOFLUSH TCIOFLUSH
    TCOOFF TCOON TCIOFF TCION
    posix-tcgetattr posix-tcsetattr
    posix-isatty posix-ttyname posix-ctermid
    posix-tcsendbreak posix-tcdrain posix-tcflush posix-tcflow
    posix-tcsetpgrp posix-tcgetpgrp
    c-ioctl)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants)
          (hafod internal platform-constants) (hafod internal posix-core)
          (only (hafod internal platform-ftypes) termios-t))

  ;; ======================================================================
  ;; TTY / termios
  ;; ======================================================================

  ;; termios action constants
  (define TCSANOW   PLAT-TCSANOW)
  (define TCSADRAIN PLAT-TCSADRAIN)
  (define TCSAFLUSH PLAT-TCSAFLUSH)

  ;; tcflush queue selectors
  (define TCIFLUSH  PLAT-TCIFLUSH)
  (define TCOFLUSH  PLAT-TCOFLUSH)
  (define TCIOFLUSH PLAT-TCIOFLUSH)

  ;; tcflow action constants
  (define TCOOFF PLAT-TCOOFF)
  (define TCOON  PLAT-TCOON)
  (define TCIOFF PLAT-TCIOFF)
  (define TCION  PLAT-TCION)

  (define c-tcgetattr  (foreign-procedure "tcgetattr"  (int void*) int))
  (define c-tcsetattr  (foreign-procedure "tcsetattr"  (int int void*) int))
  (define c-isatty     (foreign-procedure "isatty"     (int) int))
  (define c-ttyname    (foreign-procedure "ttyname"    (int) uptr))
  (define c-ctermid    (foreign-procedure "ctermid"    (u8*) uptr))
  (define c-tcsendbreak (foreign-procedure "tcsendbreak" (int int) int))
  (define c-tcdrain    (foreign-procedure "tcdrain"    (int) int))
  (define c-tcflush    (foreign-procedure "tcflush"    (int int) int))
  (define c-tcflow     (foreign-procedure "tcflow"     (int int) int))
  (define c-tcsetpgrp  (foreign-procedure "tcsetpgrp"  (int int) int))
  (define c-tcgetpgrp  (foreign-procedure "tcgetpgrp"  (int) int))

  ;; Shared variadic ioctl. The compiler emits the per-platform variadic call
  ;; frame; the trailing argument is typed uptr so the one binding serves both
  ;; an integer request (e.g. TIOCSCTTY 0) and a pointer request (a winsize
  ;; buffer for TIOCGWINSZ) without a Scheme-side type error.
  (define c-ioctl
    (foreign-procedure (__varargs_after 2) "ioctl" (int unsigned-long uptr) int))

  ;; posix-tcgetattr: retrieve terminal attributes.
  ;; Returns (values iflag oflag cflag lflag cc-bytevector ispeed-code ospeed-code)
  ;; The struct termios is read through the termios-t ftype: a foreign block is
  ;; wrapped once with make-ftype-pointer and each field is read with ftype-ref,
  ;; so the compiler derives every offset and width from the declared layout and
  ;; no byte offset is hand rolled. c_cc is an array field, read element by
  ;; element into a fresh bytevector. with-foreign-buffer frees the block on
  ;; every exit path.
  (define (posix-tcgetattr fd)
    (with-foreign-buffer ([buf (ftype-sizeof termios-t)])
      (posix-call tcgetattr (c-tcgetattr fd buf))
      (let ([p (make-ftype-pointer termios-t buf)])
        (let ([iflag  (ftype-ref termios-t (c_iflag) p)]
              [oflag  (ftype-ref termios-t (c_oflag) p)]
              [cflag  (ftype-ref termios-t (c_cflag) p)]
              [lflag  (ftype-ref termios-t (c_lflag) p)]
              [cc     (let ([v (make-bytevector PLAT-NCCS)])
                        (do ([i 0 (fx+ i 1)]) ((fx= i PLAT-NCCS))
                          (bytevector-u8-set! v i (ftype-ref termios-t (c_cc i) p)))
                        v)]
              [ispeed (ftype-ref termios-t (c_ispeed) p)]
              [ospeed (ftype-ref termios-t (c_ospeed) p)])
          (values iflag oflag cflag lflag cc ispeed ospeed)))))

  ;; posix-tcsetattr: apply terminal attributes.
  ;; The struct termios is written through the termios-t ftype. foreign-alloc
  ;; does not clear the block (unlike the zeroed bytevector this replaced), so we
  ;; zero-fill it first, keeping c_line and any padding at 0 so no uninitialised
  ;; bytes reach the kernel. Each field is written with ftype-set!; c_cc is an
  ;; array field, written element by element, copying at most PLAT-NCCS bytes so
  ;; a short cc keeps its partial-copy behaviour. with-foreign-buffer frees the
  ;; block on every exit path.
  (define (posix-tcsetattr fd option iflag oflag cflag lflag cc ispeed ospeed)
    (with-foreign-buffer ([buf (ftype-sizeof termios-t)])
      (do ([i 0 (fx+ i 1)]) ((fx= i (ftype-sizeof termios-t)))
        (foreign-set! 'unsigned-8 buf i 0))
      (let ([p (make-ftype-pointer termios-t buf)])
        (ftype-set! termios-t (c_iflag) p iflag)
        (ftype-set! termios-t (c_oflag) p oflag)
        (ftype-set! termios-t (c_cflag) p cflag)
        (ftype-set! termios-t (c_lflag) p lflag)
        (let ([n (fxmin (bytevector-length cc) PLAT-NCCS)])
          (do ([i 0 (fx+ i 1)]) ((fx= i n))
            (ftype-set! termios-t (c_cc i) p (bytevector-u8-ref cc i))))
        (ftype-set! termios-t (c_ispeed) p ispeed)
        (ftype-set! termios-t (c_ospeed) p ospeed)
        (posix-call tcsetattr (c-tcsetattr fd option buf)))))

  ;; posix-isatty: test if fd is a terminal.
  (define (posix-isatty fd) (= 1 (c-isatty fd)))

  ;; posix-ttyname: get terminal device name for fd.
  (define (posix-ttyname fd)
    (let ([p (c-ttyname fd)])
      (if (= p 0)
          (let ([err (foreign-ref 'int (__errno_location) 0)])
            (raise-posix-error 'ttyname err))
          (ptr->string p))))

  ;; posix-ctermid: get controlling terminal path.
  (define (posix-ctermid)
    (let ([buf (make-bytevector 256 0)])
      (let ([p (c-ctermid buf)])
        (if (= p 0)
            "/dev/tty"  ; fallback
            (let ([result (bv-cstring buf 0)])
              (if (= (string-length result) 0)
                  "/dev/tty"
                  result))))))

  ;; posix-tcsendbreak, posix-tcdrain, posix-tcflush, posix-tcflow
  (define (posix-tcsendbreak fd duration)
    (posix-call tcsendbreak (c-tcsendbreak fd duration)))

  (define (posix-tcdrain fd)
    (posix-call tcdrain (c-tcdrain fd)))

  (define (posix-tcflush fd flag)
    (posix-call tcflush (c-tcflush fd flag)))

  (define (posix-tcflow fd action)
    (posix-call tcflow (c-tcflow fd action)))

  ;; posix-tcgetpgrp / posix-tcsetpgrp
  (define (posix-tcgetpgrp fd)
    (posix-call tcgetpgrp (c-tcgetpgrp fd)))

  (define (posix-tcsetpgrp fd pid)
    (posix-call tcsetpgrp (c-tcsetpgrp fd pid)))

  ) ; end library
