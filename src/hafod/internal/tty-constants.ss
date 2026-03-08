;;; (hafod internal tty-constants) -- TTY flag constants
;;; Pure definitions -- no FFI, no dependencies beyond (chezscheme).
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod internal tty-constants)
  (export
    ;; Control character constants
    ttychar/eof ttychar/eol ttychar/delete-char ttychar/delete-line
    ttychar/interrupt ttychar/quit ttychar/suspend
    ttychar/start ttychar/stop ttychar/min ttychar/time
    ttychar/delete-word ttychar/reprint ttychar/literal-next
    ttychar/discard ttychar/delayed-suspend ttychar/eol2
    ttychar/status
    num-ttychars disable-tty-char

    ;; Input flag constants
    ttyin/ignore-break ttyin/interrupt-on-break
    ttyin/ignore-bad-parity-chars ttyin/mark-parity-errors
    ttyin/check-parity ttyin/7bits
    ttyin/nl->cr ttyin/ignore-cr ttyin/cr->nl
    ttyin/output-flow-ctl ttyin/input-flow-ctl
    ttyin/xon-any ttyin/beep-on-overflow ttyin/lowercase

    ;; Output flag constants
    ttyout/enable ttyout/nl->crnl
    ttyout/discard-eot ttyout/expand-tabs
    ttyout/cr->nl ttyout/fill-w/del ttyout/delay-w/fill-char
    ttyout/uppercase ttyout/nl-does-cr ttyout/no-col0-cr
    ttyout/nl-delay ttyout/nl-delay0 ttyout/nl-delay1
    ttyout/tab-delay ttyout/tab-delay0 ttyout/tab-delay1
    ttyout/tab-delay2 ttyout/tab-delayx
    ttyout/cr-delay ttyout/cr-delay0 ttyout/cr-delay1
    ttyout/cr-delay2 ttyout/cr-delay3
    ttyout/vtab-delay ttyout/vtab-delay0 ttyout/vtab-delay1
    ttyout/bs-delay ttyout/bs-delay0 ttyout/bs-delay1
    ttyout/ff-delay ttyout/ff-delay0 ttyout/ff-delay1
    ttyout/all-delay

    ;; Control flag constants
    ttyc/char-size ttyc/char-size5 ttyc/char-size6
    ttyc/char-size7 ttyc/char-size8
    ttyc/2-stop-bits ttyc/enable-read
    ttyc/enable-parity ttyc/odd-parity
    ttyc/hup-on-close ttyc/no-modem-sync
    ttyc/ignore-flags
    ttyc/CTS-output-flow-ctl ttyc/RTS-input-flow-ctl
    ttyc/carrier-flow-ctl

    ;; Local flag constants
    ttyl/visual-delete ttyl/echo-delete-line
    ttyl/echo ttyl/echo-nl
    ttyl/canonical ttyl/enable-signals ttyl/extended
    ttyl/ttou-signal ttyl/no-flush-on-interrupt
    ttyl/visual-delete-line ttyl/hardcopy-delete
    ttyl/echo-ctl ttyl/flush-output
    ttyl/reprint-unread-chars
    ttyl/alt-delete-word ttyl/no-kernel-status
    ttyl/case-map

    ;; Baud rate operations
    encode-baud-rate decode-baud-rate baud-rates)

  (import (chezscheme) (hafod internal platform-constants))

  ;; ======================================================================
  ;; Control Character Constants (POSIX cc indices from platform-constants)
  ;; ======================================================================

  (define ttychar/interrupt      PLAT-VINTR)
  (define ttychar/quit           PLAT-VQUIT)
  (define ttychar/delete-char    PLAT-VERASE)
  (define ttychar/delete-line    PLAT-VKILL)
  (define ttychar/eof            PLAT-VEOF)
  (define ttychar/time           PLAT-VTIME)
  (define ttychar/min            PLAT-VMIN)
  (define ttychar/start          PLAT-VSTART)
  (define ttychar/stop           PLAT-VSTOP)
  (define ttychar/suspend        PLAT-VSUSP)
  (define ttychar/eol            PLAT-VEOL)
  (define ttychar/reprint        PLAT-VREPRINT)
  (define ttychar/discard        PLAT-VDISCARD)
  (define ttychar/delete-word    PLAT-VWERASE)
  (define ttychar/literal-next   PLAT-VLNEXT)
  (define ttychar/eol2           PLAT-VEOL2)
  (define ttychar/delayed-suspend #f) ; VDSUSP (not available on Linux)
  (define ttychar/status #f)          ; VSTATUS (BSD only, not available on Linux)

  (define num-ttychars PLAT-NCCS)
  (define disable-tty-char (integer->char 0))  ; _POSIX_VDISABLE

  ;; ======================================================================
  ;; Input Flag Constants
  ;; ======================================================================

  (define ttyin/ignore-break            PLAT-IGNBRK)
  (define ttyin/interrupt-on-break      PLAT-BRKINT)
  (define ttyin/ignore-bad-parity-chars PLAT-IGNPAR)
  (define ttyin/mark-parity-errors      PLAT-PARMRK)
  (define ttyin/check-parity            PLAT-INPCK)
  (define ttyin/7bits                   PLAT-ISTRIP)
  (define ttyin/nl->cr                  PLAT-INLCR)
  (define ttyin/ignore-cr               PLAT-IGNCR)
  (define ttyin/cr->nl                  PLAT-ICRNL)
  (define ttyin/lowercase               #x0200) ; IUCLC (Linux-specific)
  (define ttyin/output-flow-ctl         PLAT-IXON)
  (define ttyin/xon-any                 PLAT-IXANY)
  (define ttyin/input-flow-ctl          PLAT-IXOFF)
  (define ttyin/beep-on-overflow        #x2000) ; IMAXBEL

  ;; ======================================================================
  ;; Output Flag Constants
  ;; ======================================================================

  (define ttyout/enable            PLAT-OPOST)
  (define ttyout/uppercase         #x0002) ; OLCUC (Linux-specific)
  (define ttyout/nl->crnl          PLAT-ONLCR)
  (define ttyout/cr->nl            #x0008) ; OCRNL
  (define ttyout/no-col0-cr        #x0010) ; ONOCR
  (define ttyout/nl-does-cr        #x0020) ; ONLRET
  (define ttyout/delay-w/fill-char #x0040) ; OFILL
  (define ttyout/fill-w/del        #x0080) ; OFDEL
  (define ttyout/discard-eot       #f)     ; ONOEOT (not on Linux)
  (define ttyout/expand-tabs       #f)     ; OXTABS (not on Linux; use tab-delayx)

  ;; Delay masks
  (define ttyout/nl-delay          #x0100) ; NLDLY
  (define ttyout/nl-delay0         #x0000) ; NL0
  (define ttyout/nl-delay1         #x0100) ; NL1
  (define ttyout/tab-delay         #x1800) ; TABDLY
  (define ttyout/tab-delay0        #x0000) ; TAB0
  (define ttyout/tab-delay1        #x0800) ; TAB1
  (define ttyout/tab-delay2        #x1000) ; TAB2
  (define ttyout/tab-delayx        #x1800) ; TAB3 (XTABS)
  (define ttyout/cr-delay          #x0600) ; CRDLY
  (define ttyout/cr-delay0         #x0000) ; CR0
  (define ttyout/cr-delay1         #x0200) ; CR1
  (define ttyout/cr-delay2         #x0400) ; CR2
  (define ttyout/cr-delay3         #x0600) ; CR3
  (define ttyout/vtab-delay        #x4000) ; VTDLY
  (define ttyout/vtab-delay0       #x0000) ; VT0
  (define ttyout/vtab-delay1       #x4000) ; VT1
  (define ttyout/bs-delay          #x2000) ; BSDLY
  (define ttyout/bs-delay0         #x0000) ; BS0
  (define ttyout/bs-delay1         #x2000) ; BS1
  (define ttyout/ff-delay          #x8000) ; FFDLY
  (define ttyout/ff-delay0         #x0000) ; FF0
  (define ttyout/ff-delay1         #x8000) ; FF1

  (define ttyout/all-delay
    (let ([flags (list ttyout/nl-delay ttyout/tab-delay ttyout/cr-delay
                       ttyout/vtab-delay ttyout/bs-delay ttyout/ff-delay)])
      (fold-left bitwise-ior 0 flags)))

  ;; ======================================================================
  ;; Control Flag Constants
  ;; ======================================================================

  (define ttyc/char-size           PLAT-CSIZE)
  (define ttyc/char-size5          PLAT-CS5)
  (define ttyc/char-size6          PLAT-CS6)
  (define ttyc/char-size7          PLAT-CS7)
  (define ttyc/char-size8          PLAT-CS8)
  (define ttyc/2-stop-bits         PLAT-CSTOPB)
  (define ttyc/enable-read         PLAT-CREAD)
  (define ttyc/enable-parity       PLAT-PARENB)
  (define ttyc/odd-parity          PLAT-PARODD)
  (define ttyc/hup-on-close        PLAT-HUPCL)
  (define ttyc/no-modem-sync       PLAT-CLOCAL)
  (define ttyc/ignore-flags        #f)     ; CIGNORE (not on Linux)
  (define ttyc/CTS-output-flow-ctl #x80000000) ; CRTSCTS
  (define ttyc/RTS-input-flow-ctl  #f)     ; not separately available on Linux
  (define ttyc/carrier-flow-ctl    #f)     ; MDMBUF (not on Linux)

  ;; ======================================================================
  ;; Local Flag Constants
  ;; ======================================================================

  (define ttyl/enable-signals        PLAT-ISIG)
  (define ttyl/canonical             PLAT-ICANON)
  (define ttyl/case-map              #x0004) ; XCASE (Linux-specific)
  (define ttyl/echo                  PLAT-ECHO)
  (define ttyl/visual-delete         PLAT-ECHOE)
  (define ttyl/echo-delete-line      PLAT-ECHOK)
  (define ttyl/echo-nl               PLAT-ECHONL)
  (define ttyl/no-flush-on-interrupt PLAT-NOFLSH)
  (define ttyl/ttou-signal           PLAT-TOSTOP)
  (define ttyl/echo-ctl              #x0200) ; ECHOCTL (Linux-specific)
  (define ttyl/hardcopy-delete       #x0400) ; ECHOPRT (Linux-specific)
  (define ttyl/visual-delete-line    #x0800) ; ECHOKE (Linux-specific)
  (define ttyl/flush-output          #x1000) ; FLUSHO (Linux-specific)
  (define ttyl/reprint-unread-chars  #x4000) ; PENDIN (Linux-specific)
  (define ttyl/extended              PLAT-IEXTEN)
  (define ttyl/alt-delete-word       #f)     ; ALTWERASE (not on Linux)
  (define ttyl/no-kernel-status      #f)     ; NOKERNINFO (not on Linux)

  ;; ======================================================================
  ;; Baud Rate Definitions
  ;; On modern Linux (glibc), Bxxx == xxx (the code IS the speed value).
  ;; ======================================================================

  (define baud-rates
    (vector
      (cons      0      0)   ; B0
      (cons     50     50)   ; B50
      (cons     75     75)   ; B75
      (cons    110    110)   ; B110
      (cons    134    134)   ; B134
      (cons    150    150)   ; B150
      (cons    200    200)   ; B200
      (cons    300    300)   ; B300
      (cons    600    600)   ; B600
      (cons   1200   1200)   ; B1200
      (cons   1800   1800)   ; B1800
      (cons   2400   2400)   ; B2400
      (cons   4800   4800)   ; B4800
      (cons   9600   9600)   ; B9600
      (cons  19200  19200)   ; B19200
      (cons  38400  38400)   ; B38400
      (cons  57600  57600)   ; B57600
      (cons 115200 115200)   ; B115200
      (cons 230400 230400))) ; B230400

  ;; encode-baud-rate: speed (integer) -> baud code
  ;; On modern Linux glibc, the code IS the speed, but we maintain the
  ;; lookup table for compatibility with the scsh API.
  (define (encode-baud-rate speed)
    (let loop ([i (- (vector-length baud-rates) 1)])
      (cond
        [(< i 0) (error 'encode-baud-rate "Unknown baud rate" speed)]
        [(= (cdr (vector-ref baud-rates i)) speed)
         (car (vector-ref baud-rates i))]
        [else (loop (- i 1))])))

  ;; decode-baud-rate: baud code -> speed (integer)
  (define (decode-baud-rate code)
    (let loop ([i (- (vector-length baud-rates) 1)])
      (cond
        [(< i 0) (error 'decode-baud-rate "Unknown baud rate code" code)]
        [(= (car (vector-ref baud-rates i)) code)
         (cdr (vector-ref baud-rates i))]
        [else (loop (- i 1))])))

  ) ; end library
