;;; test/test-ffi-leak.ss -- Foreign-resource non-leak floor for the DIR* /
;;; glob_t / winsize FFI sites: terminal-size, directory-files,
;;; maybe-directory-files and posix-glob-fast.
;;;
;;; Two layers, both PTY-free and portable -- no /proc, no platform gate, so it
;;; runs identically on every platform:
;;;   1. Behaviour floor -- after the unwind rework each site still returns the
;;;      right shape: terminal-size two positive integers, the directory
;;;      scanners and the glob a list.
;;;   2. fd-canary regression floor -- a lowest-free-fd probe (posix-open a
;;;      stable read-only target, record the slot, posix-close) is snapshotted,
;;;      each site is driven through 200 successful calls, and the probe is
;;;      retaken.  posix-open hands back the lowest free descriptor, so a
;;;      descriptor leaked on the exercised path would occupy that low slot and
;;;      bump the probe higher; an equal before/after number proves no fd (hence
;;;      no DIR*) leaked across the run.
;;;
;;; This guards the SUCCESSFUL path.  The raise-mid-operation unwind is covered
;;; structurally by each site's dynamic-wind after-thunk (closedir / globfree +
;;; free / foreign-free) and, deterministically, by the forced-error temp-leak
;;; proof in a companion suite.  A glob_t globfree omission is a heap leak this
;;; fd probe cannot see -- closing that gap is the companion suite's job; here
;;; the after-thunk is verifiable by reading the changed site.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod tty) terminal-size)
        (only (hafod fileinfo) directory-files)
        (only (hafod glob) maybe-directory-files)
        (only (hafod internal posix-misc) posix-glob-fast)
        (only (hafod posix) posix-open posix-close O_RDONLY))

(test-begin "ffi-leak")

;; A lowest-free-fd probe: opening a stable, always-present read-only target
;; returns the lowest free descriptor, and closing it hands that slot straight
;; back, so two probes with nothing leaked between them return the SAME number.
(define (probe-fd)
  (let ([fd (posix-open "/dev/null" O_RDONLY 0)])
    (posix-close fd)
    fd))

;; Drive THUNK n times for effect, discarding each result.
(define (repeat n thunk)
  (do ([i 0 (+ i 1)]) ((= i n)) (thunk)))

(define iterations 200)

;; Targets guaranteed to exist and be readable on every target platform.
(define probe-dir "/tmp")
(define probe-pat "/tmp/*")

;; ======================================================================
;; 1. Behaviour floor -- each site still returns the right shape
;; ======================================================================

(let-values ([(rows cols) (terminal-size)])
  (test-assert "terminal-size returns a positive row count"
    (and (integer? rows) (> rows 0)))
  (test-assert "terminal-size returns a positive column count"
    (and (integer? cols) (> cols 0))))

(test-assert "directory-files returns a list"
  (list? (directory-files probe-dir)))

(test-assert "maybe-directory-files returns a list"
  (list? (maybe-directory-files probe-dir #f)))

(test-assert "posix-glob-fast returns a list"
  (list? (posix-glob-fast probe-pat)))

;; ======================================================================
;; 2. fd-canary -- 200 successful calls leak no descriptor
;; ======================================================================
;; The behaviour-floor calls above have already run each site once, so any
;; one-shot lazy initialisation is done before the first snapshot is taken.

;; directory-files opens (and must closedir) a DIR* on every call.
(let ([before (probe-fd)])
  (repeat iterations (lambda () (directory-files probe-dir)))
  (test-equal "directory-files leaks no fd over 200 calls"
    before (probe-fd)))

;; maybe-directory-files closes its DIR* inside the error-swallowing guard.
(let ([before (probe-fd)])
  (repeat iterations (lambda () (maybe-directory-files probe-dir #f)))
  (test-equal "maybe-directory-files leaks no fd over 200 calls"
    before (probe-fd)))

;; posix-glob-fast: glob(3) opens directories internally; the after-thunk
;; globfree + free must leave no descriptor behind.
(let ([before (probe-fd)])
  (repeat iterations (lambda () (posix-glob-fast probe-pat)))
  (test-equal "posix-glob-fast leaks no fd over 200 calls"
    before (probe-fd)))

;; terminal-size ioctls existing descriptors and frees a winsize buffer on
;; every exit; it opens no fd, so this drives the freed-on-every-exit path 200x
;; and confirms it neither opens nor leaks a descriptor.
(let ([before (probe-fd)])
  (repeat iterations (lambda () (let-values ([(r c) (terminal-size)]) #f)))
  (test-equal "terminal-size leaks no fd over 200 calls"
    before (probe-fd)))

(test-end)
