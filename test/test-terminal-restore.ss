;;; test-terminal-restore -- Raw-mode ownership and the cooked-mode restore net
;;;
;;; Every case exercises the owner (with-raw-mode*), the idempotent restore
;;; primitive (reassert-cooked-tty!) and the last-resort guard
;;; (install-terminal-guard!) against a PTY SLAVE fd -- never fd 0, never
;;; current-input-port. The whole path is pure tcgetattr/tcsetattr and registry
;;; reads: there is no read on any descriptor, and no fatal or stop signal is
;;; ever delivered to this process, so nothing can block or wedge the suite.
;;; Every PTY fd is closed on every exit path through dynamic-wind.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod tty)
        (hafod signal)
        (hafod pty)
        (hafod fd-ports)
        (hafod posix)
        (except (chezscheme) open-input-file open-output-file))

;; ======================================================================
;; Helpers
;; ======================================================================

;; Open a PTY and its slave read-only, hand both to PROC as (proc master slave),
;; and close every fd on every exit path. No read is ever performed on the
;; slave, so nothing can block; PROC does only pure tcgetattr/tcsetattr work
;; through the owner API. This is the reusable, signal-free, fd-0-free vehicle.
(define (call-with-pty-slave proc)
  (let-values ([(master slave-name) (open-pty)])
    (dynamic-wind
      void
      (lambda ()
        (let ([slave (open-file slave-name open/read)])
          (dynamic-wind
            void
            (lambda () (proc master slave))
            (lambda () (close slave)))))
      (lambda () (close master)))))

;; Simulate a foreground child that left the slave in raw mode: clear canonical
;; mode and echo from the local flags through the public setters. This mirrors
;; the owner's own flag math without reaching for its internal helper, so the
;; test dirties the terminal exactly the way a real child would.
(define (dirty-into-raw! slave)
  (let ([raw (copy-tty-info (tty-info slave))])
    (set-tty-info:local-flags raw
      (bitwise-and (tty-info:local-flags raw)
                   (bitwise-not (bitwise-ior ttyl/canonical ttyl/echo))))
    (set-tty-info/now slave raw)))

;; True when SLAVE currently has both canonical mode and echo cleared, i.e. it
;; is in raw mode.
(define (raw-lflags? slave)
  (let ([lf (tty-info:local-flags (tty-info slave))])
    (and (zero? (bitwise-and lf ttyl/canonical))
         (zero? (bitwise-and lf ttyl/echo)))))

;; The cooked-restore contract is the line-discipline CONFIGURATION -- canonical
;; mode, echo and signal delivery (a usable terminal) -- not the transient
;; status bits the kernel manages. macOS's line discipline raises PENDIN
;; (input-pending-retype) as a side effect of tcsetattr, a bit that is absent on
;; Linux and outside the contract, so a byte-exact c_lflag compare is not
;; portable. Assert equality over the configuration bits the restore owns,
;; drawn from the generated PLAT-* constants so the mask is correct per platform.
(define lflag-config-mask
  (bitwise-ior ttyl/canonical ttyl/echo ttyl/enable-signals))
(define (cooked-config-restored? slave cooked-lflags)
  (= (bitwise-and (tty-info:local-flags (tty-info slave)) lflag-config-mask)
     (bitwise-and cooked-lflags lflag-config-mask)))

(test-begin "Terminal raw-mode restore")

;; ======================================================================
;; with-raw-mode* round-trip (owner enters raw, restores the exact cooked)
;; ======================================================================

;; Entering through the owner clears canonical mode and echo for the duration
;; of the thunk, and leaving it restores the exact cooked baseline. The thunk
;; only samples the slave's live local flags -- it never reads a byte.
(test-assert "with-raw-mode* clears canonical+echo on entry and restores cooked on exit"
  (call-with-pty-slave
    (lambda (master slave)
      (let ([cooked-lflags (tty-info:local-flags (tty-info slave))]
            [sampled #f])
        (with-raw-mode* slave
          (lambda () (set! sampled (tty-info:local-flags (tty-info slave)))))
        (and sampled
             ;; inside the scope: raw (canonical + echo cleared)
             (zero? (bitwise-and sampled ttyl/canonical))
             (zero? (bitwise-and sampled ttyl/echo))
             ;; after the scope: the cooked configuration is back
             (cooked-config-restored? slave cooked-lflags))))))

;; ======================================================================
;; reassert-cooked-tty! is idempotent
;; ======================================================================

;; With a baseline published, dirtying the slave into raw and then calling the
;; re-assert TWICE leaves it cooked, and neither call raises.
(test-assert "reassert-cooked-tty! is idempotent -- twice leaves the slave cooked, never raises"
  (call-with-pty-slave
    (lambda (master slave)
      (let* ([cooked (tty-info slave)]
             [cooked-lflags (tty-info:local-flags cooked)])
        (current-cooked-tty-info cooked)
        (dirty-into-raw! slave)
        (reassert-cooked-tty! slave)
        (reassert-cooked-tty! slave)
        (cooked-config-restored? slave cooked-lflags)))))

;; ======================================================================
;; install-terminal-guard! registration (read the registry, deliver no signal)
;; ======================================================================

;; The guard registers a disposition for each catchable fatal, and deliberately
;; leaves SIGINT alone so it keeps its line-abort role. We read the registry
;; back with current-signal-handler; no signal is ever delivered.
(test-assert "install-terminal-guard! registers SIGHUP/SIGQUIT/SIGTERM and leaves SIGINT alone"
  (begin
    (install-terminal-guard!)
    (and (procedure? (current-signal-handler SIGHUP))
         (procedure? (current-signal-handler SIGQUIT))
         (procedure? (current-signal-handler SIGTERM))
         (not (current-signal-handler SIGINT)))))

;; ======================================================================
;; Dirty-child restore (a child left the tty raw; re-assert returns it cooked)
;; ======================================================================

;; A foreground child (vim/ssh) can crash and leave the tty raw. With the
;; baseline published, the re-assert returns the slave to cooked -- no
;; `stty sane` needed. We first confirm the slave really was dirtied.
(test-assert "a dirtied (raw) slave is returned to cooked by reassert-cooked-tty!"
  (call-with-pty-slave
    (lambda (master slave)
      (let* ([cooked (tty-info slave)]
             [cooked-lflags (tty-info:local-flags cooked)])
        (current-cooked-tty-info cooked)
        (dirty-into-raw! slave)
        (let ([was-raw (raw-lflags? slave)])
          (reassert-cooked-tty! slave)
          (and was-raw
               (cooked-config-restored? slave cooked-lflags)))))))

;; ======================================================================
;; Off-terminal safety (re-assert against a non-tty fd is a no-op)
;; ======================================================================

;; Even with a real baseline published, re-asserting against a descriptor that
;; is not a terminal (here /dev/null) must be a harmless no-op that returns
;; without raising -- the tty? guard protects a batch run with redirected stdin.
(test-assert "reassert-cooked-tty! off a terminal is a harmless no-op (no raise)"
  (call-with-pty-slave
    (lambda (master slave)
      (current-cooked-tty-info (tty-info slave))
      (let ([devnull (open-file "/dev/null" open/read)])
        (dynamic-wind
          void
          (lambda ()
            (reassert-cooked-tty! devnull)
            #t)
          (lambda () (close devnull)))))))

;; ======================================================================
;; VSUSP disabled in raw mode (Ctrl-Z becomes a byte, ISIG stays on)
;; ======================================================================

;; The owner disables VSUSP on raw entry so Ctrl-Z is delivered as byte 0x1A --
;; which unblocks the editor read at once -- instead of a SIGTSTP the blocking
;; read would defer until the next keystroke. ISIG stays ON (Ctrl-C / Ctrl-\
;; keep their signals), and the cooked VSUSP is restored on exit. We only sample
;; tcgetattr; no byte is read and no signal is delivered, so nothing can block.
(test-assert "with-raw-mode* disables VSUSP but keeps ISIG on, and restores VSUSP on exit"
  (call-with-pty-slave
    (lambda (master slave)
      (let ([cooked-vsusp (bytevector-u8-ref
                            (tty-info:control-chars (tty-info slave)) ttychar/suspend)]
            [in-vsusp #f]
            [in-isig? #f])
        (with-raw-mode* slave
          (lambda ()
            (let ([info (tty-info slave)])
              (set! in-vsusp
                (bytevector-u8-ref (tty-info:control-chars info) ttychar/suspend))
              (set! in-isig?
                (not (zero? (bitwise-and (tty-info:local-flags info)
                                         ttyl/enable-signals)))))))
        (and
          ;; inside raw: VSUSP disabled (== _POSIX_VDISABLE) and ISIG still on
          (= in-vsusp (char->integer disable-tty-char))
          in-isig?
          ;; after raw: the cooked VSUSP is back
          (= (bytevector-u8-ref (tty-info:control-chars (tty-info slave)) ttychar/suspend)
             cooked-vsusp))))))

;; ======================================================================
;; current-suspend-hook is scoped to the raw session
;; ======================================================================

;; The owner publishes the suspend dance as current-suspend-hook only for the
;; extent of a raw session, so the editor's Ctrl-Z byte has a synchronous
;; suspend to call; it is back to #f afterwards. We never call the hook (that
;; would SIGSTOP the suite) -- we only observe it is a procedure in scope.
(test-assert "current-suspend-hook is a procedure only within a raw-mode session"
  (call-with-pty-slave
    (lambda (master slave)
      (let ([before (current-suspend-hook)]
            [inside #f])
        (with-raw-mode* slave
          (lambda () (set! inside (current-suspend-hook))))
        (and (not before)
             (procedure? inside)
             (not (current-suspend-hook)))))))

;; ======================================================================
;; The suspend dance's cooked/raw choreography (stop stubbed -- hang-safe)
;; ======================================================================

;; Calling the published hook must restore the terminal to cooked BEFORE the
;; process stops and re-enter raw on resume -- the invariant that keeps Ctrl-Z
;; from stranding the terminal. We parameterize suspend-stopper so the suite
;; never actually stops: the stub samples the live mode at the stop point (must
;; be cooked, i.e. NOT raw), and we check raw is back after the dance resumes.
;; No SIGSTOP is ever delivered, so nothing can wedge the suite.
(test-assert "the suspend hook restores cooked before stopping and re-raws on resume"
  (call-with-pty-slave
    (lambda (master slave)
      (let ([cooked-at-stop #f]
            [raw-after #f])
        (parameterize ([suspend-stopper
                        (lambda () (set! cooked-at-stop (not (raw-lflags? slave))))])
          (with-raw-mode* slave
            (lambda ()
              ((current-suspend-hook))
              (set! raw-after (raw-lflags? slave)))))
        (and cooked-at-stop raw-after)))))

(test-end)
