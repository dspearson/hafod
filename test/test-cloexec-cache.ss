;;; test/test-cloexec-cache.ss -- Proof that the per-fd FD_CLOEXEC bit is cached
;;; in the fd-port table, so a reveal-count change that leaves the cloexec state
;;; unchanged no longer issues a redundant F_GETFD/F_SETFD pair.
;;;
;;; A call/fdes on an already-revealed port does two same-side set-fdport!
;;; transitions (revealed 1->2 then 2->1). Neither crosses the reveal-zero
;;; boundary, so the desired cloexec state (off, because revealed>0) never
;;; changes -- yet the old code re-issued the fcntl pair on each, four fcntls in
;;; all. With the cached bit those four collapse to zero.
;;;
;;; The witness is a routed count: cloexec-fcntl-observer fires once per fcntl
;;; that %set-cloexec actually issues, so a parameterised counter reads exactly
;;; how many syscalls the transition made. It defaults to #f (a no-op), leaving
;;; the real path byte-for-byte unchanged, and mirrors the spawn-release-observer
;;; idiom already proven by test-ffi-double-free.ss.
;;;
;;; Behaviour that must NOT change is asserted alongside: a genuine reveal-zero
;;; boundary crossing still fires its establishing pair; port-revealed reports
;;; the same counts; and %set-cloexec, called directly on a raw fd, still flips
;;; the kernel FD_CLOEXEC flag both ways -- the primitive stays dumb, the skip
;;; lives only in set-fdport!.
;;;
;;; Strictly pipe-only: no PTY, no child process, no external binary. Fresh pipe
;;; per scenario so each count is independent.
;;;
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (chezscheme)
        (only (hafod fd-ports)
              cloexec-fcntl-observer
              pipe port->fdes release-port-handle call/fdes port-revealed
              %set-cloexec)
        (only (hafod posix)
              posix-pipe posix-close posix-fcntl F_GETFD FD_CLOEXEC))

(test-begin "cloexec-cache")

;; Count the fcntls %set-cloexec issues while running thunk. The observer fires
;; once per fcntl; default #f keeps the un-parameterised path a no-op.
(define (count-cloexec-fcntls thunk)
  (let ([n 0])
    (parameterize ([cloexec-fcntl-observer (lambda () (set! n (+ n 1)))])
      (thunk))
    n))

;; ----------------------------------------------------------------------
;; 1. The optimisation: an already-revealed call/fdes issues ZERO fcntls.
;; ----------------------------------------------------------------------
;; A fresh pipe end is revealed 0 (cloexec on). Promote it to revealed 1 OUTSIDE
;; the counted region -- that establishing crossing is expected to fire. The
;; counted call/fdes then only churns 1->2->1, both same-side: desired cloexec is
;; off and the cached bit is already off, so nothing fires.
(let-values ([(r w) (pipe)])
  (port->fdes w)                       ; 0->1 boundary crossing, not counted
  (test-equal "an already-revealed call/fdes issues zero cloexec fcntls (was four)"
    0
    (count-cloexec-fcntls (lambda () (call/fdes w (lambda (fd) fd))))))

;; ----------------------------------------------------------------------
;; 2. The invariant: a reveal-zero boundary crossing still fires its pair.
;; ----------------------------------------------------------------------
;; A revealed-0 port has cloexec on; crossing to revealed 1 must turn it off,
;; so the establishing F_GETFD/F_SETFD pair genuinely runs -- exactly two fcntls.
(let-values ([(r w) (pipe)])
  (test-equal "a reveal 0->1 boundary crossing issues its establishing fcntl pair"
    2
    (count-cloexec-fcntls (lambda () (port->fdes w)))))

;; ----------------------------------------------------------------------
;; 3. The recorded "before": a call/fdes on a revealed-0 port still costs four.
;; ----------------------------------------------------------------------
;; This is the price the old code paid for EVERY call/fdes. Here both transitions
;; cross the boundary (0->1 clears cloexec, 1->0 restores it), so the pair fires
;; twice = four fcntls -- the number scenario 1 drives to zero for the hot,
;; already-revealed path.
(let-values ([(r w) (pipe)])
  (test-equal "a call/fdes on a revealed-0 port still issues its four boundary fcntls"
    4
    (count-cloexec-fcntls (lambda () (call/fdes w (lambda (fd) fd))))))

;; ----------------------------------------------------------------------
;; 4a. Regression: port-revealed reports the same counts before and after.
;; ----------------------------------------------------------------------
(let-values ([(r w) (pipe)])
  (test-assert "port-revealed is #f while revealed 0" (not (port-revealed w)))
  (port->fdes w)                       ; -> revealed 1
  (test-equal "port-revealed is 1 after one reveal" 1 (port-revealed w))
  (call/fdes w (lambda (fd) fd))       ; same-side round trip
  (test-equal "port-revealed unchanged at 1 after a same-side call/fdes"
    1 (port-revealed w)))

;; ----------------------------------------------------------------------
;; 4b. Regression: %set-cloexec stays a dumb primitive on a raw fd.
;; ----------------------------------------------------------------------
;; Called directly (no port machinery), it must always flip the kernel flag both
;; ways -- proving the skip lives in set-fdport!, not here.
(let ([fds (posix-pipe)])
  (let ([rfd (car fds)] [wfd (cdr fds)])
    (%set-cloexec rfd #t)
    (test-assert "%set-cloexec sets FD_CLOEXEC on a raw fd"
      (not (zero? (bitwise-and (posix-fcntl rfd F_GETFD) FD_CLOEXEC))))
    (%set-cloexec rfd #f)
    (test-assert "%set-cloexec clears FD_CLOEXEC on a raw fd"
      (zero? (bitwise-and (posix-fcntl rfd F_GETFD) FD_CLOEXEC)))
    (posix-close rfd)
    (posix-close wfd)))

(test-end)
