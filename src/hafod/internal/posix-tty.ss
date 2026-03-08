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
    posix-tcsetpgrp posix-tcgetpgrp)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants)
          (hafod internal platform-constants) (hafod internal posix-core))

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

  (define *termios-size* SIZEOF-TERMIOS)

  (define c-tcgetattr  (foreign-procedure "tcgetattr"  (int u8*) int))
  (define c-tcsetattr  (foreign-procedure "tcsetattr"  (int int u8*) int))
  (define c-isatty     (foreign-procedure "isatty"     (int) int))
  (define c-ttyname    (foreign-procedure "ttyname"    (int) uptr))
  (define c-ctermid    (foreign-procedure "ctermid"    (u8*) uptr))
  (define c-tcsendbreak (foreign-procedure "tcsendbreak" (int int) int))
  (define c-tcdrain    (foreign-procedure "tcdrain"    (int) int))
  (define c-tcflush    (foreign-procedure "tcflush"    (int int) int))
  (define c-tcflow     (foreign-procedure "tcflow"     (int int) int))
  (define c-tcsetpgrp  (foreign-procedure "tcsetpgrp"  (int int) int))
  (define c-tcgetpgrp  (foreign-procedure "tcgetpgrp"  (int) int))

  ;; posix-tcgetattr: retrieve terminal attributes.
  ;; Returns (values iflag oflag cflag lflag cc-bytevector ispeed-code ospeed-code)
  (define (posix-tcgetattr fd)
    (let ([buf (make-bytevector *termios-size* 0)])
      (posix-call tcgetattr (c-tcgetattr fd buf))
      (let ([iflag  (bytevector-u32-native-ref buf TERMIOS-C-IFLAG)]
            [oflag  (bytevector-u32-native-ref buf TERMIOS-C-OFLAG)]
            [cflag  (bytevector-u32-native-ref buf TERMIOS-C-CFLAG)]
            [lflag  (bytevector-u32-native-ref buf TERMIOS-C-LFLAG)]
            [cc     (let ([v (make-bytevector PLAT-NCCS)])
                      (bytevector-copy! buf TERMIOS-C-CC v 0 PLAT-NCCS)
                      v)]
            [ispeed (bytevector-u32-native-ref buf TERMIOS-C-ISPEED)]
            [ospeed (bytevector-u32-native-ref buf TERMIOS-C-OSPEED)])
        (values iflag oflag cflag lflag cc ispeed ospeed))))

  ;; posix-tcsetattr: apply terminal attributes.
  (define (posix-tcsetattr fd option iflag oflag cflag lflag cc ispeed ospeed)
    (let ([buf (make-bytevector *termios-size* 0)])
      (bytevector-u32-native-set! buf TERMIOS-C-IFLAG iflag)
      (bytevector-u32-native-set! buf TERMIOS-C-OFLAG oflag)
      (bytevector-u32-native-set! buf TERMIOS-C-CFLAG cflag)
      (bytevector-u32-native-set! buf TERMIOS-C-LFLAG lflag)
      (bytevector-copy! cc 0 buf TERMIOS-C-CC (fxmin (bytevector-length cc) PLAT-NCCS))
      (bytevector-u32-native-set! buf TERMIOS-C-ISPEED ispeed)
      (bytevector-u32-native-set! buf TERMIOS-C-OSPEED ospeed)
      (posix-call tcsetattr (c-tcsetattr fd option buf))))

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
