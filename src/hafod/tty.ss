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

    ;; Terminal window size
    terminal-size

    ;; Raw-mode ownership and last-resort cooked-mode restore net
    with-raw-mode* current-cooked-tty-info reassert-cooked-tty!
    current-suspend-hook suspend-stopper install-terminal-guard!

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
          (hafod fd-ports)
          (only (hafod signal) with-signal-handler set-signal-handler! die-by-signal)
          (hafod exit-hooks)
          (only (hafod internal posix-tty) c-ioctl)
          (only (hafod internal platform-constants)
                WINSZ-WS-ROW WINSZ-WS-COL SIZEOF-WINSZ))

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
  ;; Raw-mode ownership and the last-resort cooked-mode restore net
  ;; ======================================================================
  ;;
  ;; This is the single owner of raw-mode entry and exit. with-raw-mode*
  ;; captures the cooked baseline once, publishes it, enters raw with the
  ;; exact flag math the line editor has always used, and restores cooked on
  ;; every ordinary exit. The published baseline (current-cooked-tty-info) is
  ;; the one source every escape route -- ordinary return, exception, a fatal
  ;; signal, or a Ctrl-Z suspend -- restores from, so the terminal is never
  ;; left stranded in raw mode. reassert-cooked-tty! is the idempotent restore
  ;; primitive the whole net reuses. The net is layered OVER the editor's own
  ;; dynamic-wind (belt-and-braces): resetting the termios twice is harmless.

  ;; The single published cooked baseline. Set once on raw entry and read by
  ;; every restore path; #f before any raw-mode session has run.
  (define current-cooked-tty-info (make-parameter #f))

  ;; Compute the raw counterpart of a cooked tty-info without mutating the
  ;; original: copy it, clear canonical (line) mode and echo from the local
  ;; flags, clear CR-to-NL translation from the input flags, and ask for one
  ;; byte at a time with no inter-byte timer. enable-signals (ISIG) is left ON,
  ;; so Ctrl-C and Ctrl-\ still arrive as signals while editing. The one signal
  ;; char we DO disable is VSUSP (Ctrl-Z): a kernel-delivered SIGTSTP would sit
  ;; unhandled until the editor's blocking read returned -- Chez runs a Scheme
  ;; signal handler only at a safe point, never mid-read -- so the suspend would
  ;; lag until the next keystroke. Disabling VSUSP delivers Ctrl-Z as the plain
  ;; byte 0x1A instead, which unblocks the read at once; the editor binds that
  ;; byte to a synchronous suspend (see current-suspend-hook). This is the exact
  ;; flag math the line editor has always used, plus that one VSUSP change.
  (define (%make-raw cooked)
    (let ([raw (copy-tty-info cooked)])
      (set-tty-info:local-flags raw
        (bitwise-and (tty-info:local-flags raw)
                     (bitwise-not (bitwise-ior ttyl/canonical ttyl/echo))))
      (set-tty-info:input-flags raw
        (bitwise-and (tty-info:input-flags raw)
                     (bitwise-not ttyin/cr->nl)))
      (set-tty-info:min raw 1)
      (set-tty-info:time raw 0)
      ;; Disable VSUSP: deliver Ctrl-Z as byte 0x1A, not a deferred SIGTSTP.
      (bytevector-u8-set! (tty-info:control-chars raw)
                          ttychar/suspend (char->integer disable-tty-char))
      raw))

  ;; Idempotent restore of the published cooked baseline for FD. A no-op when
  ;; no baseline has been published yet or FD is not a terminal (e.g. a batch
  ;; run with stdin redirected from /dev/null), and any error from the set is
  ;; swallowed, so it is always safe to call -- twice in a row, or from inside
  ;; a signal handler. This is the single restore primitive the whole net, the
  ;; post-child re-assert, and the launcher's exception guard all reuse.
  (define (reassert-cooked-tty! fd)
    (let ([cooked (current-cooked-tty-info)])
      (when (and cooked (tty? fd))
        (guard (e [#t (void)])
          (set-tty-info/now fd cooked)))))

  ;; A nullary "suspend now" action, valid only within a raw-mode session and
  ;; #f outside one. with-raw-mode* sets it to the very same dance the SIGTSTP
  ;; handler runs, so the line editor can drop to the shell synchronously the
  ;; instant it reads a Ctrl-Z byte (VSUSP is disabled, so Ctrl-Z arrives as a
  ;; byte, not a signal) instead of waiting for a deferred SIGTSTP. Calling it
  ;; directly keeps the keyboard suspend path off the async-signal machinery.
  (define current-suspend-hook (make-parameter #f))

  ;; The dance's stop step, factored behind a parameter so a test can drive the
  ;; cooked/raw choreography without ever stopping the suite. Default: stop the
  ;; whole process with SIGSTOP, the mechanism job control expects. A test
  ;; parameterizes it to a probe that samples the terminal at the stop point.
  (define suspend-stopper
    (make-parameter (lambda () (posix-kill (posix-getpid) SIGSTOP))))

  ;; Build the suspend dance used while raw. It is installed as the SIGTSTP
  ;; handler (covering an external `kill -TSTP`) and, through current-suspend-
  ;; hook, is also called directly when the editor reads a Ctrl-Z byte -- so its
  ;; one argument is ignored. On suspend it puts the terminal back into cooked
  ;; mode BEFORE the process stops, flushes any pending output, then stops the
  ;; whole process through suspend-stopper (SIGSTOP by default) -- the same
  ;; mechanism the process layer's suspend uses, so it cooperates with rather
  ;; than fights job control. A Chez signal handler runs at a safe point on the
  ;; main thread, so control resumes inline here on SIGCONT; on resume it
  ;; re-enters raw mode. There is deliberately no global SIGCONT handler --
  ;; re-rawing inline keeps the dance scoped to this raw-mode session and clear
  ;; of job control's own SIGCONT delivery to resumed children.
  (define (make-suspend-dance fd)
    (lambda (sig)
      (reassert-cooked-tty! fd)
      (flush-output-port (console-output-port))
      ((suspend-stopper))
      ;; ---- resumes here on SIGCONT ----
      (set-tty-info/now fd (%make-raw (current-cooked-tty-info)))))

  ;; The single owner of raw-mode entry/exit. Capture the cooked baseline for
  ;; FD, publish it, then dynamic-wind: enter raw before, run THUNK with the
  ;; SIGTSTP suspend-dance swapped in ONLY for this extent (the prior SIGTSTP
  ;; disposition is restored on exit, so job control's own handler is untouched
  ;; outside the editor read), and restore cooked after. The same dance is also
  ;; published as current-suspend-hook for the extent, so the editor's Ctrl-Z
  ;; byte drops to the shell synchronously; parameterize resets the hook to #f
  ;; on exit. FD is explicit so the whole path is exercisable against a PTY
  ;; slave in tests, never fd 0.
  (define (with-raw-mode* fd thunk)
    (let ([cooked (tty-info fd)])
      (current-cooked-tty-info cooked)
      (dynamic-wind
        (lambda () (set-tty-info/now fd (%make-raw cooked)))
        (lambda ()
          (let ([dance (make-suspend-dance fd)])
            (with-signal-handler SIGTSTP dance
              (lambda ()
                (parameterize ([current-suspend-hook (lambda () (dance #f))])
                  (thunk))))))
        (lambda () (set-tty-info/now fd cooked)))))

  ;; Install the last-resort net that guarantees a cooked terminal even when
  ;; control leaves through a path the editor's own dynamic-wind cannot see.
  ;; This is belt-and-braces over that innermost unwind -- resetting the
  ;; termios twice is harmless -- and is meant to be called once at start-up.
  (define (install-terminal-guard!)
    ;; (a) The ordinary (exit) path: compose the current exit-handler so it
    ;; reasserts cooked for fd 0 before the real exit runs, and register an
    ;; exit hook for the same, covering the exit path that runs the hooks.
    (let ([orig (exit-handler)])
      (exit-handler
        (lambda args
          (reassert-cooked-tty! 0)
          (apply orig args))))
    (add-exit-hook! (lambda () (reassert-cooked-tty! 0)))
    ;; (b) The catchable fatal signals: restore cooked, then die with the
    ;; signal's own default disposition so the process terminates with the
    ;; correct wait status instead of re-entering the Scheme handler. SIGINT
    ;; is deliberately NOT in this set -- it keeps its line-abort role, and any
    ;; path where it could leave the terminal raw is already covered by the
    ;; editor dynamic-wind plus the idempotent re-assert above.
    (for-each
      (lambda (sig)
        (set-signal-handler! sig
          (lambda (s)
            (reassert-cooked-tty! 0)
            (die-by-signal s))))
      (list SIGHUP SIGQUIT SIGTERM)))

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
  (define (make-control-tty fd/port)
    (let ([fd (cond
                [(integer? fd/port) fd/port]
                [(port? fd/port) (sleazy-call/fdes fd/port (lambda (fd) fd))]
                [else (error 'make-control-tty "Expected port or fd" fd/port)])])
      (posix-call make-control-tty (c-ioctl fd TIOCSCTTY 0))))

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

  ;; ======================================================================
  ;; Terminal window size
  ;; ======================================================================

  ;; Query the terminal window size via ioctl(TIOCGWINSZ) and return
  ;; (values rows cols). The struct winsize is read through the generated
  ;; SIZEOF-WINSZ/WINSZ-WS-ROW/WINSZ-WS-COL accessors so no offset is hand
  ;; rolled. Without an explicit fd we try stdout, stdin then stderr; with
  ;; an fd we query that one only. When none is a terminal -- or the
  ;; reported size is zero -- we fall back to a sensible 24x80. The buffer
  ;; is freed on every exit path. The shared variadic c-ioctl carries the
  ;; winsize buffer as a uptr, so the compiler emits the correct call frame
  ;; without a per-platform C shim.
  (define terminal-size
    (case-lambda
      [() (terminal-size #f)]
      [(fd)
       (let ([buf (foreign-alloc SIZEOF-WINSZ)])
         (let try-fd ([fds (if fd (list fd) '(1 0 2))])
           (cond
             [(null? fds)
              (foreign-free buf)
              (values 24 80)]  ;; fallback when nothing is a terminal
             [else
              (let ([rc (c-ioctl (car fds) TIOCGWINSZ buf)])
                (if (zero? rc)
                    (let ([rows (foreign-ref 'unsigned-16 buf WINSZ-WS-ROW)]
                          [cols (foreign-ref 'unsigned-16 buf WINSZ-WS-COL)])
                      (foreign-free buf)
                      (values (if (> rows 0) rows 24)
                              (if (> cols 0) cols 80)))
                    (try-fd (cdr fds))))])))]))

) ; end library
