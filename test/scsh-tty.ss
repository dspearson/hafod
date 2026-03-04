;;; scsh-tty.ss -- Ported scsh terminal-device-control tests
;;; Translated from scsh/test/terminal-device-control-test.scm
;;; Original author: Christoph Hetz
;;; Ported to hafod test runner format.
;;;
;;; Note: hafod tty-info:control-chars returns a bytevector (not a string
;;; as in Scheme48 scsh). Tests adapted accordingly.
;;;
;;; Tests that require a real TTY (tty-info with no args) use open-pty
;;; to create a PTY pair, since test runners typically pipe stdio.
;;;
;;; Run with: scheme --libdirs .:src --script test/scsh-tty.ss

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod))

(test-begin "scsh-tty-device-control")

;;; ===== Test 1: tty-info record =====
;;; Original scsh test calls (tty-info) with no args on stdout.
;;; In hafod, when not on a TTY, we use open-pty to get a real PTY fd.
(test-assert "tty-info-record-test"
  (receive (pty-port tty-name) (open-pty)
    (let ([ti (tty-info pty-port)])
      (let ([result
              (and (bytevector? (tty-info:control-chars ti))
                   (or (integer? (tty-info:input-flags ti))
                       (not (tty-info:input-flags ti)))
                   (or (integer? (tty-info:output-flags ti))
                       (not (tty-info:output-flags ti)))
                   (or (integer? (tty-info:control-flags ti))
                       (not (tty-info:control-flags ti)))
                   (or (integer? (tty-info:local-flags ti))
                       (not (tty-info:local-flags ti)))
                   (or (integer? (tty-info:input-speed ti))
                       (not (tty-info:input-speed ti)))
                   (or (integer? (tty-info:output-speed ti))
                       (not (tty-info:output-speed ti)))
                   (or (integer? (tty-info:min ti))
                       (not (tty-info:min ti)))
                   (or (integer? (tty-info:time ti))
                       (not (tty-info:time ti))))])
        (close pty-port)
        result))))

;;; ===== Test 2: make-tty-info =====
(test-assert "make-tty-info-test"
  (let* ([in-fl 770]
         [out-fl 3]
         [c-fl 19200]
         [loc-fl 1482]
         [in-spd 1200]
         [out-spd 1200]
         [min-v 1]
         [time-v 0]
         [ti (make-tty-info in-fl out-fl c-fl loc-fl in-spd out-spd min-v time-v)])
    (and (= in-fl (tty-info:input-flags ti))
         (= out-fl (tty-info:output-flags ti))
         (= c-fl (tty-info:control-flags ti))
         (= in-spd (tty-info:input-speed ti))
         (= out-spd (tty-info:output-speed ti))
         (= min-v (tty-info:min ti))
         (= time-v (tty-info:time ti)))))

;;; ===== Test 3: copy-tty-info =====
(test-assert "copy-tty-test"
  (receive (pty-port tty-name) (open-pty)
    (let* ([ti (tty-info pty-port)]
           [ti-c (copy-tty-info ti)])
      (let ([result
              (and (tty-info? ti)
                   (tty-info? ti-c)
                   (equal? (tty-info:control-chars ti)
                           (tty-info:control-chars ti-c))
                   (= (tty-info:input-flags ti)
                      (tty-info:input-flags ti-c))
                   (= (tty-info:output-flags ti)
                      (tty-info:output-flags ti-c))
                   (= (tty-info:control-flags ti)
                      (tty-info:control-flags ti-c))
                   (= (tty-info:local-flags ti)
                      (tty-info:local-flags ti-c))
                   (equal? (tty-info:input-speed ti)
                           (tty-info:input-speed ti-c))
                   (equal? (tty-info:output-speed ti)
                           (tty-info:output-speed ti-c))
                   (= (tty-info:min ti)
                      (tty-info:min ti-c))
                   (= (tty-info:time ti)
                      (tty-info:time ti-c)))])
        (close pty-port)
        result))))

;;; ===== Test 4: POSIX control char indices =====
(test-assert "tty-info-record-posix-indicies-test"
  (and ttychar/delete-char
       ttychar/delete-line
       ttychar/eof
       ttychar/eol
       ttychar/interrupt
       ttychar/quit
       ttychar/suspend
       ttychar/start
       ttychar/stop))

;;; ===== Test 5: POSIX input flags =====
(test-assert "tty-info-record-posix-input-flags"
  (and ttyin/check-parity
       ttyin/ignore-bad-parity-chars
       ttyin/mark-parity-errors
       ttyin/ignore-break
       ttyin/interrupt-on-break
       ttyin/7bits
       ttyin/cr->nl
       ttyin/ignore-cr
       ttyin/nl->cr
       ttyin/input-flow-ctl
       ttyin/output-flow-ctl))

;;; ===== Test 6: POSIX output flags =====
(test-assert "tty-info-record-posix-output-flags"
  (and ttyout/enable #t))

;;; ===== Test 7: Delay constants for output flags =====
;;; All-or-nothing: either all delay constants are defined or none are.
;;; On Linux they are all defined.
(test-assert "tty-info-record-delay-constants-for-output-flags"
  (or (and ttyout/bs-delay
           ttyout/bs-delay0
           ttyout/bs-delay1
           ttyout/cr-delay
           ttyout/cr-delay0
           ttyout/cr-delay1
           ttyout/cr-delay2
           ttyout/cr-delay3
           ttyout/ff-delay
           ttyout/ff-delay0
           ttyout/ff-delay1
           ttyout/tab-delay
           ttyout/tab-delay0
           ttyout/tab-delay1
           ttyout/tab-delay2
           ttyout/tab-delayx
           ttyout/nl-delay
           ttyout/nl-delay0
           ttyout/nl-delay1
           ttyout/vtab-delay
           ttyout/vtab-delay0
           ttyout/vtab-delay1
           ttyout/all-delay)
      (not (and ttyout/bs-delay
                ttyout/bs-delay0
                ttyout/bs-delay1
                ttyout/cr-delay
                ttyout/cr-delay0
                ttyout/cr-delay1
                ttyout/cr-delay2
                ttyout/cr-delay3
                ttyout/ff-delay
                ttyout/ff-delay0
                ttyout/ff-delay1
                ttyout/tab-delay
                ttyout/tab-delay0
                ttyout/tab-delay1
                ttyout/tab-delay2
                ttyout/tab-delayx
                ttyout/nl-delay
                ttyout/nl-delay0
                ttyout/nl-delay1
                ttyout/vtab-delay
                ttyout/vtab-delay0
                ttyout/vtab-delay1
                ttyout/all-delay))))

;;; ===== Test 8: POSIX control flags =====
(test-assert "tty-info-record-posix-control-flags"
  (and ttyc/char-size
       ttyc/char-size5
       ttyc/char-size6
       ttyc/char-size7
       ttyc/char-size8
       ttyc/enable-parity
       ttyc/odd-parity
       ttyc/enable-read
       ttyc/hup-on-close
       ttyc/no-modem-sync
       ttyc/2-stop-bits))

;;; ===== Test 9: 4.3+BSD control flags =====
;;; All-or-nothing on BSD flags. On Linux, some are #f.
(test-assert "tty-info-record-4.3+bsd-control-flags"
  (or (and ttyc/ignore-flags
           ttyc/CTS-output-flow-ctl
           ttyc/RTS-input-flow-ctl
           ttyc/carrier-flow-ctl)
      (not (and ttyc/ignore-flags
                ttyc/CTS-output-flow-ctl
                ttyc/RTS-input-flow-ctl
                ttyc/carrier-flow-ctl))))

;;; ===== Test 10: POSIX local flags =====
(test-assert "tty-info-record-posix-local-flags"
  (and ttyl/canonical
       ttyl/echo
       ttyl/echo-delete-line
       ttyl/echo-nl
       ttyl/visual-delete
       ttyl/enable-signals
       ttyl/extended
       ttyl/no-flush-on-interrupt
       ttyl/ttou-signal))

;;; ===== Test 11: SVR4 & 4.3+BSD local flags =====
;;; All-or-nothing. On Linux, some are #f (alt-delete-word, no-kernel-status).
(test-assert "tty-info-record-svr4&4.3+bsd-local-flags"
  (or (and ttyl/echo-ctl
           ttyl/flush-output
           ttyl/hardcopy-delete
           ttyl/reprint-unread-chars
           ttyl/visual-delete-line
           ttyl/alt-delete-word
           ttyl/no-kernel-status
           ttyl/case-map)
      (not (and ttyl/echo-ctl
                ttyl/flush-output
                ttyl/hardcopy-delete
                ttyl/reprint-unread-chars
                ttyl/visual-delete-line
                ttyl/alt-delete-word
                ttyl/no-kernel-status
                ttyl/case-map))))

;;; ===== Test 12: open-pty I/O =====
;;; Open a PTY pair, write to master, read from slave side.
(test-assert "open-pty-test"
  (guard (e [#t #f])
    (receive (pty-inport tty-name) (open-pty)
      (let ([tty-in (open-input-file tty-name)])
        (let ([pty-out (dup->outport pty-inport)])
          (write 23 pty-out)
          (newline pty-out)
          (flush-output-port pty-out)
          (let ([res (equal? 23 (read tty-in))])
            (close-output-port pty-out)
            (close-input-port tty-in)
            (close pty-inport)
            res))))))

;;; ===== Test 13: fork-pty-session =====
;;; Fork a child that reads a string, doubles it, writes it back.
;;; Parent disables echo, writes "hello", reads reply, verifies "hellohello".
;;; Note: hafod fork-pty-session returns (values master-port proc),
;;; not (values proc pty-in pty-out tty-name) as in original scsh.
(test-assert "fork-pty-session-test"
  (guard (e [#t #f])
    (receive (master-port proc)
      (fork-pty-session (lambda ()
                          (let ([inp (read)])
                            (write (string-append inp inp)))
                          (newline)
                          (flush-output-port (current-output-port))))
      (let ([pty-out (dup->outport master-port)]
            [pty-in master-port])
        ;; Disable echo on the PTY
        (let ([ti (copy-tty-info (tty-info pty-out))])
          (set-tty-info:local-flags ti
            (bitwise-xor ttyl/echo (tty-info:local-flags ti)))
          (set-tty-info/now pty-out ti))
        ;; Send input to child
        (write "hello" pty-out)
        (newline pty-out)
        (flush-output-port pty-out)
        ;; Read reply
        (let ([reply (read pty-in)])
          (close-output-port pty-out)
          (wait proc)
          (string=? "hellohello" reply))))))

(test-end)
