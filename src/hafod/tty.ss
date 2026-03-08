;;; (hafod tty) -- Terminal (TTY) control interface
;;; Provides tty-info record type, termios flag constants, baud rate
;;; encoding/decoding, get/set terminal attributes, break/drain/flush/flow
;;; control, controlling terminal management, and process group operations.
;;; Ported from scsh/scheme/tty.scm and scsh/scheme/tty-consts.scm
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod tty)
  (export
    ;; tty-info record
    make-tty-info copy-tty-info tty-info?
    tty-info:control-chars  set-tty-info:control-chars
    tty-info:input-flags    set-tty-info:input-flags
    tty-info:output-flags   set-tty-info:output-flags
    tty-info:control-flags  set-tty-info:control-flags
    tty-info:local-flags    set-tty-info:local-flags
    tty-info:input-speed    set-tty-info:input-speed
    tty-info:output-speed   set-tty-info:output-speed
    tty-info:min            set-tty-info:min
    tty-info:time           set-tty-info:time

    ;; Get/set terminal attributes
    tty-info
    set-tty-info/now set-tty-info/drain set-tty-info/flush

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
    encode-baud-rate decode-baud-rate baud-rates

    ;; Terminal predicates and naming
    tty? tty-file-name control-tty-file-name

    ;; Break/drain
    send-tty-break drain-tty

    ;; Flush
    flush-tty/input flush-tty/output flush-tty/both

    ;; Flow control
    start-tty-output stop-tty-output
    start-tty-input stop-tty-input

    ;; Controlling terminal
    open-control-tty make-control-tty

    ;; Process group
    tty-process-group set-tty-process-group)

  (import (hafod internal base)
          (hafod internal tty-constants)
          (hafod posix)
          (hafod compat)
          (hafod fd-ports))

  ;; ======================================================================
  ;; tty-info Record Type
  ;; ======================================================================

  ;; Internal record type with mutable fields.
  ;; Speed fields store both the actual speed and the baud code.
  (define-record-type tty-info-rec
    (fields
      (mutable control-chars  tty-info-rec-cc       tty-info-rec-cc-set!)
      (mutable input-flags    tty-info-rec-iflags   tty-info-rec-iflags-set!)
      (mutable output-flags   tty-info-rec-oflags   tty-info-rec-oflags-set!)
      (mutable control-flags  tty-info-rec-cflags   tty-info-rec-cflags-set!)
      (mutable local-flags    tty-info-rec-lflags   tty-info-rec-lflags-set!)
      (mutable input-speed    tty-info-rec-ispeed   tty-info-rec-ispeed-set!)
      (mutable ispeed-code    tty-info-rec-iscode   tty-info-rec-iscode-set!)
      (mutable output-speed   tty-info-rec-ospeed   tty-info-rec-ospeed-set!)
      (mutable ospeed-code    tty-info-rec-oscode   tty-info-rec-oscode-set!)
      (mutable min-val        tty-info-rec-min      tty-info-rec-min-set!)
      (mutable time-val       tty-info-rec-time     tty-info-rec-time-set!))
    (nongenerative hafod-tty-info))

  ;; ---- scsh-API-compatible accessors ----

  (define (tty-info? x) (tty-info-rec? x))

  ;; Simple getters (all 9 -- including speed getters which are pure accessors)
  (define-scsh-accessors
    (tty-info:control-chars  tty-info-rec-cc)
    (tty-info:input-flags    tty-info-rec-iflags)
    (tty-info:output-flags   tty-info-rec-oflags)
    (tty-info:control-flags  tty-info-rec-cflags)
    (tty-info:local-flags    tty-info-rec-lflags)
    (tty-info:input-speed    tty-info-rec-ispeed)
    (tty-info:output-speed   tty-info-rec-ospeed)
    (tty-info:min            tty-info-rec-min)
    (tty-info:time           tty-info-rec-time))

  ;; Simple setters (no side effects -- 7 of 9)
  (define-scsh-accessors
    (set-tty-info:control-chars  tty-info-rec-cc-set!)
    (set-tty-info:input-flags    tty-info-rec-iflags-set!)
    (set-tty-info:output-flags   tty-info-rec-oflags-set!)
    (set-tty-info:control-flags  tty-info-rec-cflags-set!)
    (set-tty-info:local-flags    tty-info-rec-lflags-set!)
    (set-tty-info:min            tty-info-rec-min-set!)
    (set-tty-info:time           tty-info-rec-time-set!))

  ;; Speed setters with encode-baud-rate side effects (hand-written)
  (define (set-tty-info:input-speed info speed)
    (tty-info-rec-ispeed-set! info speed)
    (tty-info-rec-iscode-set! info (encode-baud-rate speed)))
  (define (set-tty-info:output-speed info speed)
    (tty-info-rec-ospeed-set! info speed)
    (tty-info-rec-oscode-set! info (encode-baud-rate speed)))

  ;; ---- Constructors ----

  ;; User-facing constructor (scsh API).
  (define (make-tty-info iflags oflags cflags lflags ispeed ospeed min-val time-val)
    (make-tty-info-rec
      (make-bytevector num-ttychars 0)
      iflags oflags cflags lflags
      ispeed (encode-baud-rate ispeed)
      ospeed (encode-baud-rate ospeed)
      min-val time-val))

  ;; Deep copy of a tty-info record.
  (define (copy-tty-info info)
    (make-tty-info-rec
      (bytevector-copy (tty-info-rec-cc info))
      (tty-info-rec-iflags info) (tty-info-rec-oflags info)
      (tty-info-rec-cflags info) (tty-info-rec-lflags info)
      (tty-info-rec-ispeed info) (tty-info-rec-iscode info)
      (tty-info-rec-ospeed info) (tty-info-rec-oscode info)
      (tty-info-rec-min info) (tty-info-rec-time info)))

  ;; ======================================================================
  ;; Get/Set Terminal Attributes
  ;; ======================================================================

  ;; Retrieve terminal attributes. Uses tcgetattr.
  ;; (tty-info) uses current-input-port; (tty-info port/fd) uses given port or fd.
  (define tty-info
    (let ([getter
           (lambda (fdport)
             (let ([fd (if (integer? fdport) fdport (port->fdes fdport))])
               (let-values ([(iflag oflag cflag lflag cc ispeed-code ospeed-code)
                             (posix-tcgetattr fd)])
                 (make-tty-info-rec
                   cc iflag oflag cflag lflag
                   (decode-baud-rate ispeed-code) ispeed-code
                   (decode-baud-rate ospeed-code) ospeed-code
                   (bytevector-u8-ref cc ttychar/min)
                   (bytevector-u8-ref cc ttychar/time)))))])
      (case-lambda
        [() (getter (current-input-port))]
        [(fdport) (getter fdport)])))

  ;; Apply terminal attributes with different timing options.
  (define (%set-tty-info fdport option info)
    (let ([fd (if (integer? fdport) fdport (port->fdes fdport))])
      ;; Update the cc array's VMIN and VTIME slots from the record fields
      (let ([cc (tty-info-rec-cc info)])
        (bytevector-u8-set! cc ttychar/min (tty-info-rec-min info))
        (bytevector-u8-set! cc ttychar/time (tty-info-rec-time info)))
      (posix-tcsetattr fd option
        (tty-info-rec-iflags info)
        (tty-info-rec-oflags info)
        (tty-info-rec-cflags info)
        (tty-info-rec-lflags info)
        (tty-info-rec-cc info)
        (tty-info-rec-iscode info)
        (tty-info-rec-oscode info))))

  (define (set-tty-info/now   fdport info) (%set-tty-info fdport TCSANOW   info))
  (define (set-tty-info/drain fdport info) (%set-tty-info fdport TCSADRAIN info))
  (define (set-tty-info/flush fdport info) (%set-tty-info fdport TCSAFLUSH info))

  ;; ======================================================================
  ;; Terminal Predicates and Naming
  ;; ======================================================================

  ;; Test if fd/port is a terminal.
  (define (tty? fd/port)
    (cond
      [(integer? fd/port) (posix-isatty fd/port)]
      [(port? fd/port)    (sleazy-call/fdes fd/port (lambda (fd) (posix-isatty fd)))]
      [else (error 'tty? "Expected port or fd" fd/port)]))

  ;; Get terminal device name.
  (define (tty-file-name fd/port)
    (cond
      [(integer? fd/port) (posix-ttyname fd/port)]
      [(port? fd/port)    (sleazy-call/fdes fd/port posix-ttyname)]
      [else (error 'tty-file-name "Expected port or fd" fd/port)]))

  ;; Get controlling terminal path.
  (define (control-tty-file-name) (posix-ctermid))

  ;; ======================================================================
  ;; Break and Drain
  ;; ======================================================================

  (define send-tty-break
    (case-lambda
      [()         (send-tty-break (current-output-port) 0)]
      [(tty)      (send-tty-break tty 0)]
      [(tty duration)
       (let ([fd (if (integer? tty) tty (port->fdes tty))])
         (posix-tcsendbreak fd duration))]))

  (define drain-tty
    (case-lambda
      [()    (drain-tty (current-output-port))]
      [(tty)
       (cond
         [(integer? tty) (posix-tcdrain tty)]
         [(port? tty)
          (when (output-port? tty) (flush-output-port tty))
          (sleazy-call/fdes tty (lambda (fd) (posix-tcdrain fd)))]
         [else (error 'drain-tty "Expected port or fd" tty)])]))

  ;; ======================================================================
  ;; Flush Operations
  ;; ======================================================================

  (define (make-tty-flusher flag default-port)
    (case-lambda
      [()    ((make-tty-flusher flag default-port) (default-port))]
      [(tty)
       (let ([fd (if (integer? tty) tty (port->fdes tty))])
         (posix-tcflush fd flag))]))

  (define flush-tty/input  (make-tty-flusher TCIFLUSH  current-input-port))
  (define flush-tty/output (make-tty-flusher TCOFLUSH  current-output-port))
  (define flush-tty/both   (make-tty-flusher TCIOFLUSH current-input-port))

  ;; ======================================================================
  ;; Flow Control
  ;; ======================================================================

  (define (make-flow-controller action default-port)
    (case-lambda
      [()    ((make-flow-controller action default-port) (default-port))]
      [(tty)
       (let ([fd (if (integer? tty) tty (port->fdes tty))])
         (posix-tcflow fd action))]))

  (define start-tty-output (make-flow-controller TCOON  current-output-port))
  (define stop-tty-output  (make-flow-controller TCOOFF current-output-port))
  (define start-tty-input  (make-flow-controller TCION  current-input-port))
  (define stop-tty-input   (make-flow-controller TCIOFF current-input-port))

  ;; ======================================================================
  ;; Controlling Terminal
  ;; ======================================================================

  ;; Open a controlling terminal. Opens the named tty device and
  ;; returns a port for it.
  (define open-control-tty
    (case-lambda
      [(ttyname) (open-control-tty ttyname O_RDWR)]
      [(ttyname flags)
       (let ([fd (posix-open ttyname flags 0)])
         (fdes->inport fd))]))

  ;; Make an already-open fd the controlling terminal.
  (define make-control-tty
    (let ([c-ioctl (foreign-procedure "hafod_ioctl_int" (int unsigned-long int) int)])
      (lambda (fd/port)
        (let ([fd (cond
                    [(integer? fd/port) fd/port]
                    [(port? fd/port) (sleazy-call/fdes fd/port (lambda (fd) fd))]
                    [else (error 'make-control-tty "Expected port or fd" fd/port)])])
          (posix-call make-control-tty (c-ioctl fd TIOCSCTTY 0))))))

  ;; ======================================================================
  ;; Process Group
  ;; ======================================================================

  (define (tty-process-group port/fd/fname)
    (cond
      [(integer? port/fd/fname) (posix-tcgetpgrp port/fd/fname)]
      [(string? port/fd/fname)
       (let ([p (open-input-file port/fd/fname)])
         (dynamic-wind void
           (lambda () (sleazy-call/fdes p posix-tcgetpgrp))
           (lambda () (close-input-port p))))]
      [(port? port/fd/fname)
       (sleazy-call/fdes port/fd/fname posix-tcgetpgrp)]
      [else (error 'tty-process-group "Expected port, fd, or filename" port/fd/fname)]))

  (define (set-tty-process-group port/fd/fname pgrp)
    (cond
      [(integer? port/fd/fname) (posix-tcsetpgrp port/fd/fname pgrp)]
      [(string? port/fd/fname)
       (let ([p (open-input-file port/fd/fname)])
         (dynamic-wind void
           (lambda () (sleazy-call/fdes p (lambda (fd) (posix-tcsetpgrp fd pgrp))))
           (lambda () (close-input-port p))))]
      [(port? port/fd/fname)
       (sleazy-call/fdes port/fd/fname
         (lambda (fd) (posix-tcsetpgrp fd pgrp)))]
      [else (error 'set-tty-process-group "Expected port, fd, or filename" port/fd/fname)]))

) ; end library
