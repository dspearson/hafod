;;; (hafod internal posix-tty) -- TTY/termios operations and constants
;;; Extracted from posix.ss during Phase 26 splitting.
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

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants) (hafod internal posix-core))

  (define load-libc (load-shared-object "libc.so.6"))

  ;; ======================================================================
  ;; TTY / termios
  ;; ======================================================================

  ;; termios action constants
  (define TCSANOW   0)
  (define TCSADRAIN 1)
  (define TCSAFLUSH 2)

  ;; tcflush queue selectors
  (define TCIFLUSH  0)
  (define TCOFLUSH  1)
  (define TCIOFLUSH 2)

  ;; tcflow action constants
  (define TCOOFF 0)
  (define TCOON  1)
  (define TCIOFF 2)
  (define TCION  3)

  ;; struct termios layout on Linux x86_64:
  ;;   c_iflag:  u32 at offset 0
  ;;   c_oflag:  u32 at offset 4
  ;;   c_cflag:  u32 at offset 8
  ;;   c_lflag:  u32 at offset 12
  ;;   c_line:   u8  at offset 16
  ;;   c_cc[32]: 32 bytes at offset 17
  ;;   c_ispeed: u32 at offset 52
  ;;   c_ospeed: u32 at offset 56
  ;;   total: 60 bytes
  (define *termios-size* 60)

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
      (let ([iflag  (bytevector-u32-native-ref buf 0)]
            [oflag  (bytevector-u32-native-ref buf 4)]
            [cflag  (bytevector-u32-native-ref buf 8)]
            [lflag  (bytevector-u32-native-ref buf 12)]
            [cc     (let ([v (make-bytevector 32)])
                      (bytevector-copy! buf 17 v 0 32)
                      v)]
            [ispeed (bytevector-u32-native-ref buf 52)]
            [ospeed (bytevector-u32-native-ref buf 56)])
        (values iflag oflag cflag lflag cc ispeed ospeed))))

  ;; posix-tcsetattr: apply terminal attributes.
  (define (posix-tcsetattr fd option iflag oflag cflag lflag cc ispeed ospeed)
    (let ([buf (make-bytevector *termios-size* 0)])
      (bytevector-u32-native-set! buf 0 iflag)
      (bytevector-u32-native-set! buf 4 oflag)
      (bytevector-u32-native-set! buf 8 cflag)
      (bytevector-u32-native-set! buf 12 lflag)
      (bytevector-copy! cc 0 buf 17 (fxmin (bytevector-length cc) 32))
      (bytevector-u32-native-set! buf 52 ispeed)
      (bytevector-u32-native-set! buf 56 ospeed)
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
