(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod signal) (hafod posix))

(test-begin "signal")

;; Signal constants are positive integers
(test-assert "SIGTERM is positive integer" (and (integer? SIGTERM) (> SIGTERM 0)))
(test-assert "SIGINT is positive integer" (and (integer? SIGINT) (> SIGINT 0)))
(test-assert "SIGKILL is positive integer" (and (integer? SIGKILL) (> SIGKILL 0)))

;; signal-process with signal 0 (existence check)
(test-assert "signal-process self with signal 0"
  (begin (signal-process (posix-getpid) 0) #t))

;; signal-process-group with signal 0
;; Use the actual process group ID, not pid
(test-assert "signal-process-group self with signal 0"
  (begin (signal-process-group (posix-getpgrp) 0) #t))

;; signal-process with bad pid should raise posix error
(test-error "signal-process bad pid raises error"
  (signal-process 999999 SIGTERM))

;; signal-process with non-integer should raise error
(test-error "signal-process non-integer raises error"
  (signal-process "bad" 0))

;; signal-process-group with non-integer should raise error
(test-error "signal-process-group non-integer raises error"
  (signal-process-group "bad" 0))

;; The -1 sentinel marks a signal the platform does not define. Sending it
;; must fail with a clear, named message before the value reaches kill(2),
;; rather than surfacing a raw EINVAL from the syscall floor.
(define (raises-unavailable? thunk)
  (guard (e ((and (message-condition? e)
                  (let ((m (condition-message e)))
                    (and (string? m)
                         (substring? "not available on this platform" m))))
             #t)
            (#t #f))
    (thunk)
    #f))
(define (substring? needle haystack)
  (let ((nl (string-length needle)) (hl (string-length haystack)))
    (let loop ((i 0))
      (cond ((> (+ i nl) hl) #f)
            ((string=? needle (substring haystack i (+ i nl))) #t)
            (else (loop (+ i 1)))))))

;; signal-process with the -1 sentinel raises the named error (not raw EINVAL)
(test-assert "signal-process -1 sentinel names platform unavailability"
  (raises-unavailable? (lambda () (signal-process (posix-getpid) -1))))

;; signal-process-group with the -1 sentinel likewise raises the named error
(test-assert "signal-process-group -1 sentinel names platform unavailability"
  (raises-unavailable? (lambda () (signal-process-group (posix-getpgrp) -1))))

;; SIGPWR is sourced from the generated platform constant, not a literal.
;; It is an integer on every platform -- the real signal number on Linux
;; (30), the -1 platform-absent sentinel on macOS/BSD -- so the integer
;; check is unconditional.
(test-assert "SIGPWR is an integer" (integer? SIGPWR))

;; SIGPWR is strictly positive ONLY on Linux. We reuse the codebase's
;; machine-type detection idiom (errno.ss): enumerate the macOS + FreeBSD
;; machine-types and treat everything else as Linux. On a non-Linux
;; machine-type the positivity assertion passes vacuously rather than
;; failing against the -1 sentinel by design (mirrors the TIOCGWINSZ
;; platform-varying handling). Do not weaken this to integer-only and do
;; not assert a bare (> SIGPWR 0).
(define linux-machine?
  (not (memq (machine-type)
             '(ta6osx tarm64osx ti3osx a6osx arm64osx i3osx
               ta6fb  tarm64fb  ti3fb  a6fb  arm64fb  i3fb))))
(test-assert "SIGPWR is positive on Linux"
  (or (not linux-machine?) (> SIGPWR 0)))

;; The (signal pwr) macro arm resolves the NAME to SIGPWR. The value-rewire
;; (SIGPWR now sourced from PLAT-SIGPWR) leaves this expansion unaffected.
(test-equal "(signal pwr) expands to SIGPWR" SIGPWR (signal pwr))

;; Sending the -1 sentinel raises, full stop. We use a valid pid so the
;; failure is unambiguously the signal guard, not a bad-pid error.
(test-error "signal-process -1 sentinel raises not-available error"
  (signal-process (posix-getpid) -1))

;; ---------------------------------------------------------------------------
;; Signal disposition registry
;;
;; These cases prove the save-and-restore-prior contract WITHOUT delivering any
;; signal. SIGWINCH is used purely as an inert registry key: it is non-fatal,
;; the sentinel procedures are plain lambdas, and the kernel never invokes them
;; because no signal is ever sent to this process. Every case is deterministic
;; and instantaneous, so the suite cannot hang.
;; ---------------------------------------------------------------------------

(define sentinel-a (lambda (sig) 'a))
(define sentinel-b (lambda (sig) 'b))

;; The first install for a key has no recorded prior, so #f comes back.
(test-assert "set-signal-handler! returns #f when there is no prior"
  (eq? #f (set-signal-handler! SIGWINCH sentinel-a)))

;; current-signal-handler reads back the procedure just installed.
(test-assert "current-signal-handler reflects the installed handler"
  (eq? sentinel-a (current-signal-handler SIGWINCH)))

;; A second install returns the procedure recorded by the first -- the prior
;; disposition the low-level install primitive cannot itself give back.
(test-assert "set-signal-handler! returns the prior handler on reinstall"
  (eq? sentinel-a (set-signal-handler! SIGWINCH sentinel-b)))

;; ... and the registry now reflects the newer procedure.
(test-assert "current-signal-handler reflects the reinstalled handler"
  (eq? sentinel-b (current-signal-handler SIGWINCH)))

;; with-signal-handler restores the EXACT prior disposition on scope exit --
;; the save-and-restore-prior contract. Install A, run a scope with B over a
;; trivial thunk, then assert A (never a no-op) is current again afterwards.
;; This is the contract that fixes the finder's clobbered-resize regression.
(test-assert "with-signal-handler restores the prior handler after the scope"
  (begin
    (set-signal-handler! SIGWINCH sentinel-a)
    (with-signal-handler SIGWINCH sentinel-b (lambda () #t))
    (eq? sentinel-a (current-signal-handler SIGWINCH))))

;; Within the scope, the swapped-in handler is the live one.
(test-assert "with-signal-handler installs the new handler within the scope"
  (begin
    (set-signal-handler! SIGWINCH sentinel-a)
    (with-signal-handler SIGWINCH sentinel-b
      (lambda () (eq? sentinel-b (current-signal-handler SIGWINCH))))))

(test-end)
