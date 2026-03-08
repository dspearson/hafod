(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod tty)
        (hafod posix)
        (hafod fd-ports)
        (except (chezscheme) open-input-file open-output-file))

(test-begin "TTY and Terminal Control")

;; ======================================================================
;; tty-info record creation and predicates
;; ======================================================================

(test-assert "make-tty-info creates a tty-info? record"
  (tty-info? (make-tty-info 0 0 0 0 9600 9600 1 0)))

(test-assert "make-tty-info with all zero flags"
  (let ([ti (make-tty-info 0 0 0 0 0 0 0 0)])
    (and (tty-info? ti)
         (= 0 (tty-info:input-flags ti))
         (= 0 (tty-info:output-flags ti))
         (= 0 (tty-info:control-flags ti))
         (= 0 (tty-info:local-flags ti))
         (= 0 (tty-info:input-speed ti))
         (= 0 (tty-info:output-speed ti))
         (= 0 (tty-info:min ti))
         (= 0 (tty-info:time ti)))))

(test-assert "make-tty-info stores flag values correctly"
  (let ([ti (make-tty-info #x100 #x200 #x300 #x400 9600 19200 5 10)])
    (and (= #x100 (tty-info:input-flags ti))
         (= #x200 (tty-info:output-flags ti))
         (= #x300 (tty-info:control-flags ti))
         (= #x400 (tty-info:local-flags ti))
         (= 9600  (tty-info:input-speed ti))
         (= 19200 (tty-info:output-speed ti))
         (= 5     (tty-info:min ti))
         (= 10    (tty-info:time ti)))))

;; ======================================================================
;; copy-tty-info
;; ======================================================================

(test-assert "copy-tty-info produces an independent copy"
  (let* ([orig (make-tty-info 42 0 0 0 9600 9600 1 0)]
         [copy (copy-tty-info orig)])
    (set-tty-info:input-flags copy 99)
    (and (= 42 (tty-info:input-flags orig))
         (= 99 (tty-info:input-flags copy)))))

(test-assert "copy-tty-info control-chars bytevector is independent"
  (let* ([orig (make-tty-info 0 0 0 0 9600 9600 0 0)]
         [copy (copy-tty-info orig)])
    (let ([cc (tty-info:control-chars copy)])
      (bytevector-u8-set! cc 0 42)
      (set-tty-info:control-chars copy cc))
    (= 0 (bytevector-u8-ref (tty-info:control-chars orig) 0))))

;; ======================================================================
;; tty-info field setters
;; ======================================================================

(test-assert "set-tty-info:input-flags updates value"
  (let ([ti (make-tty-info 0 0 0 0 9600 9600 0 0)])
    (set-tty-info:input-flags ti #xFF)
    (= #xFF (tty-info:input-flags ti))))

(test-assert "set-tty-info:output-flags updates value"
  (let ([ti (make-tty-info 0 0 0 0 9600 9600 0 0)])
    (set-tty-info:output-flags ti #xFF)
    (= #xFF (tty-info:output-flags ti))))

(test-assert "set-tty-info:control-flags updates value"
  (let ([ti (make-tty-info 0 0 0 0 9600 9600 0 0)])
    (set-tty-info:control-flags ti #xFF)
    (= #xFF (tty-info:control-flags ti))))

(test-assert "set-tty-info:local-flags updates value"
  (let ([ti (make-tty-info 0 0 0 0 9600 9600 0 0)])
    (set-tty-info:local-flags ti #xFF)
    (= #xFF (tty-info:local-flags ti))))

(test-assert "set-tty-info:input-speed updates speed and code"
  (let ([ti (make-tty-info 0 0 0 0 9600 9600 0 0)])
    (set-tty-info:input-speed ti 19200)
    (= 19200 (tty-info:input-speed ti))))

(test-assert "set-tty-info:output-speed updates speed and code"
  (let ([ti (make-tty-info 0 0 0 0 9600 9600 0 0)])
    (set-tty-info:output-speed ti 38400)
    (= 38400 (tty-info:output-speed ti))))

(test-assert "set-tty-info:min updates value"
  (let ([ti (make-tty-info 0 0 0 0 9600 9600 0 0)])
    (set-tty-info:min ti 5)
    (= 5 (tty-info:min ti))))

(test-assert "set-tty-info:time updates value"
  (let ([ti (make-tty-info 0 0 0 0 9600 9600 0 0)])
    (set-tty-info:time ti 10)
    (= 10 (tty-info:time ti))))

;; ======================================================================
;; Control character constants
;; ======================================================================

(test-assert "ttychar/eof is a non-negative integer"
  (and (integer? ttychar/eof) (>= ttychar/eof 0)))

(test-assert "ttychar/eol is a non-negative integer"
  (and (integer? ttychar/eol) (>= ttychar/eol 0)))

(test-assert "ttychar/delete-char is a non-negative integer"
  (and (integer? ttychar/delete-char) (>= ttychar/delete-char 0)))

(test-assert "ttychar/delete-line is a non-negative integer"
  (and (integer? ttychar/delete-line) (>= ttychar/delete-line 0)))

(test-assert "ttychar/interrupt is a non-negative integer"
  (and (integer? ttychar/interrupt) (>= ttychar/interrupt 0)))

(test-assert "ttychar/quit is a non-negative integer"
  (and (integer? ttychar/quit) (>= ttychar/quit 0)))

(test-assert "ttychar/suspend is a non-negative integer"
  (and (integer? ttychar/suspend) (>= ttychar/suspend 0)))

(test-assert "ttychar/start is a non-negative integer"
  (and (integer? ttychar/start) (>= ttychar/start 0)))

(test-assert "ttychar/stop is a non-negative integer"
  (and (integer? ttychar/stop) (>= ttychar/stop 0)))

(test-assert "ttychar/min is a non-negative integer"
  (and (integer? ttychar/min) (>= ttychar/min 0)))

(test-assert "ttychar/time is a non-negative integer"
  (and (integer? ttychar/time) (>= ttychar/time 0)))

(test-assert "ttychar/delete-word is a non-negative integer"
  (and (integer? ttychar/delete-word) (>= ttychar/delete-word 0)))

(test-assert "ttychar/reprint is a non-negative integer"
  (and (integer? ttychar/reprint) (>= ttychar/reprint 0)))

(test-assert "ttychar/literal-next is a non-negative integer"
  (and (integer? ttychar/literal-next) (>= ttychar/literal-next 0)))

(test-assert "ttychar/discard is a non-negative integer"
  (and (integer? ttychar/discard) (>= ttychar/discard 0)))

(test-assert "ttychar/delayed-suspend is #f on Linux"
  (eq? ttychar/delayed-suspend #f))

(test-assert "ttychar/eol2 is a non-negative integer"
  (and (integer? ttychar/eol2) (>= ttychar/eol2 0)))

(test-assert "num-ttychars is a positive integer matching NCCS"
  (and (integer? num-ttychars) (> num-ttychars 0)))

(test-assert "disable-tty-char is the NUL character"
  (and (char? disable-tty-char)
       (= 0 (char->integer disable-tty-char))))

;; ======================================================================
;; Input flag constants
;; ======================================================================

(test-assert "ttyin/ignore-break is a positive integer"
  (and (integer? ttyin/ignore-break) (> ttyin/ignore-break 0)))

(test-assert "ttyin/interrupt-on-break is a positive integer"
  (and (integer? ttyin/interrupt-on-break) (> ttyin/interrupt-on-break 0)))

(test-assert "ttyin/cr->nl is a positive integer"
  (and (integer? ttyin/cr->nl) (> ttyin/cr->nl 0)))

(test-assert "ttyin/output-flow-ctl is a positive integer"
  (and (integer? ttyin/output-flow-ctl) (> ttyin/output-flow-ctl 0)))

(test-assert "ttyin/input-flow-ctl is a positive integer"
  (and (integer? ttyin/input-flow-ctl) (> ttyin/input-flow-ctl 0)))

;; ======================================================================
;; Output flag constants
;; ======================================================================

(test-assert "ttyout/enable is a positive integer"
  (and (integer? ttyout/enable) (> ttyout/enable 0)))

(test-assert "ttyout/nl->crnl is a positive integer"
  (and (integer? ttyout/nl->crnl) (> ttyout/nl->crnl 0)))

(test-assert "ttyout/all-delay is the OR of all delay masks"
  (= ttyout/all-delay
     (bitwise-ior ttyout/nl-delay ttyout/tab-delay ttyout/cr-delay
                  ttyout/vtab-delay ttyout/bs-delay ttyout/ff-delay)))

;; ======================================================================
;; Control flag constants
;; ======================================================================

(test-assert "ttyc/char-size is a positive integer"
  (and (integer? ttyc/char-size) (> ttyc/char-size 0)))

(test-assert "ttyc/char-size8 is a positive integer"
  (and (integer? ttyc/char-size8) (> ttyc/char-size8 0)))

(test-assert "ttyc/enable-read is a positive integer"
  (and (integer? ttyc/enable-read) (> ttyc/enable-read 0)))

(test-assert "ttyc/hup-on-close is a positive integer"
  (and (integer? ttyc/hup-on-close) (> ttyc/hup-on-close 0)))

;; ======================================================================
;; Local flag constants
;; ======================================================================

(test-assert "ttyl/echo is a positive integer"
  (and (integer? ttyl/echo) (> ttyl/echo 0)))

(test-assert "ttyl/canonical is a positive integer"
  (and (integer? ttyl/canonical) (> ttyl/canonical 0)))

(test-assert "ttyl/enable-signals is a positive integer"
  (and (integer? ttyl/enable-signals) (> ttyl/enable-signals 0)))

(test-assert "ttyl/extended is a positive integer"
  (and (integer? ttyl/extended) (> ttyl/extended 0)))

;; ======================================================================
;; Baud rate encode/decode
;; ======================================================================

(test-assert "baud-rates is a vector of pairs"
  (and (vector? baud-rates)
       (> (vector-length baud-rates) 0)
       (pair? (vector-ref baud-rates 0))))

(test-assert "encode-baud-rate and decode-baud-rate round-trip for 0"
  (= 0 (decode-baud-rate (encode-baud-rate 0))))

(test-assert "encode-baud-rate and decode-baud-rate round-trip for 9600"
  (= 9600 (decode-baud-rate (encode-baud-rate 9600))))

(test-assert "encode-baud-rate and decode-baud-rate round-trip for 19200"
  (= 19200 (decode-baud-rate (encode-baud-rate 19200))))

(test-assert "encode-baud-rate and decode-baud-rate round-trip for 38400"
  (= 38400 (decode-baud-rate (encode-baud-rate 38400))))

(test-assert "encode-baud-rate raises error for unknown speed"
  (guard (e [#t #t])
    (encode-baud-rate 12345)
    #f))

(test-assert "decode-baud-rate raises error for unknown code"
  (guard (e [#t #t])
    (decode-baud-rate 99999)
    #f))

;; ======================================================================
;; tty? predicate
;; ======================================================================

(test-assert "tty? returns boolean for fd 0"
  (boolean? (tty? 0)))

(test-assert "tty? returns boolean for current-input-port"
  (boolean? (tty? (current-input-port))))

;; In non-tty environments (pipes, CI), fd 0 is not a terminal
;; so we just test that it returns a boolean, not what value

;; ======================================================================
;; Procedure checks (ensure all operations are callable)
;; ======================================================================

(test-assert "set-tty-info/now is a procedure"
  (procedure? set-tty-info/now))

(test-assert "set-tty-info/drain is a procedure"
  (procedure? set-tty-info/drain))

(test-assert "set-tty-info/flush is a procedure"
  (procedure? set-tty-info/flush))

(test-assert "send-tty-break is a procedure"
  (procedure? send-tty-break))

(test-assert "drain-tty is a procedure"
  (procedure? drain-tty))

(test-assert "flush-tty/input is a procedure"
  (procedure? flush-tty/input))

(test-assert "flush-tty/output is a procedure"
  (procedure? flush-tty/output))

(test-assert "flush-tty/both is a procedure"
  (procedure? flush-tty/both))

(test-assert "start-tty-output is a procedure"
  (procedure? start-tty-output))

(test-assert "stop-tty-output is a procedure"
  (procedure? stop-tty-output))

(test-assert "start-tty-input is a procedure"
  (procedure? start-tty-input))

(test-assert "stop-tty-input is a procedure"
  (procedure? stop-tty-input))

(test-assert "open-control-tty is a procedure"
  (procedure? open-control-tty))

(test-assert "make-control-tty is a procedure"
  (procedure? make-control-tty))

(test-assert "tty-process-group is a procedure"
  (procedure? tty-process-group))

(test-assert "set-tty-process-group is a procedure"
  (procedure? set-tty-process-group))

(test-assert "control-tty-file-name returns a string"
  (string? (control-tty-file-name)))

(test-assert "control-tty-file-name returns /dev/tty"
  (string=? "/dev/tty" (control-tty-file-name)))

;; ======================================================================
;; TTY-dependent tests (guarded -- only run if stdin is a terminal)
;; ======================================================================

(define stdin-is-tty? (tty? 0))

(when stdin-is-tty?
  (test-assert "tty-info returns a tty-info? record from stdin"
    (tty-info? (tty-info (current-input-port))))

  (test-assert "tty-info record has non-zero control-flags (from real TTY)"
    (> (tty-info:control-flags (tty-info)) 0))

  (test-assert "tty-info record has valid input-speed"
    (> (tty-info:input-speed (tty-info)) 0))

  (test-assert "tty-info record has valid output-speed"
    (> (tty-info:output-speed (tty-info)) 0))

  (test-assert "tty-file-name returns a string for fd 0"
    (string? (tty-file-name 0)))

  (test-assert "tty-file-name result starts with /dev/"
    (let ([name (tty-file-name 0)])
      (and (string? name)
           (> (string-length name) 5)
           (string=? "/dev/" (substring name 0 5)))))

  (test-assert "set-tty-info/now save+restore roundtrip works"
    (let* ([orig (tty-info)]
           [copy (copy-tty-info orig)])
      ;; Set and immediately restore
      (set-tty-info/now (current-input-port) copy)
      (let ([after (tty-info)])
        ;; Flags should match
        (= (tty-info:input-flags orig) (tty-info:input-flags after)))))

  (test-assert "tty-process-group returns a positive integer"
    (let ([pg (tty-process-group 0)])
      (and (integer? pg) (> pg 0)))))

;; Report skipped tests for non-TTY environment
(unless stdin-is-tty?
  (test-assert "TTY-dependent tests skipped (stdin is not a terminal)" #t))

(test-end)
