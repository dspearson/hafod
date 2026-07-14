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
;;; Layers 1-2 guard the SUCCESSFUL path.  A third layer drives a real raise
;;; through each unwind so the after-thunk is proven by a run, not argued from
;;; shape -- each case goes RED if its after-thunk (the close / free) is deleted.
;;; DIR* is fd-backed, so the lowest-free-fd canary observes it directly under a
;;; readdir fault seam.  glob_t and winsize are pure foreign memory -- a heap
;;; leak the fd canary cannot see (globfree / foreign-free release memory, not a
;;; descriptor) -- so each of those is proven by a routed-free observer seam that
;;; counts the after-thunk's free on the forced-raise unwind.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod tty) terminal-size winsize-fault winsize-release)
        (only (hafod fileinfo) directory-files)
        (only (hafod glob) maybe-directory-files)
        (only (hafod internal posix-misc)
              posix-glob-fast readdir-fault-after glob-fault glob-release)
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

;; ======================================================================
;; 3. forced-raise unwind proofs -- a real error enters each unwind, so the
;;    after-thunk is proven by a run, not argued from shape.  Each case goes
;;    RED if its after-thunk (the closedir / routed free) is deleted.
;; ======================================================================

;; DIR* -- fd-backed, so the fd canary is the observer.  readdir-fault-after N
;; makes the (N+1)th posix-readdir raise, so directory-files fails mid-loop with
;; its DIR* still open and the dynamic-wind after-thunk (posix-closedir) runs on
;; the unwind.  Closed on every forced-raise unwind -> the low slot is handed
;; back -> the probe is unchanged (GREEN).  Delete the closedir and the DIR* fd
;; is held on each raise -> the probe climbs (RED).
(let ([before (probe-fd)])
  (parameterize ([readdir-fault-after 1])
    (repeat iterations
            (lambda () (guard (e [#t #f]) (directory-files probe-dir)))))
  (test-equal "DIR* closed on every forced-raise readdir unwind (fd canary)"
    before (probe-fd)))

;; glob_t -- pure foreign memory, so the fd canary is BLIND to a heap leak here
;; (a missing globfree/free leaks bytes with no ceiling: no EMFILE, no low slot
;; held).  The proof is the routed-free observer.  glob-release is the
;; after-thunk's SOLE free path; capture its default (the real c-globfree +
;; foreign-free), wrap it to count the call AND perform the real free, force a
;; post-acquire raise with glob-fault, and assert the count is exactly 1 -- freed
;; once on the unwind.  Delete the after-thunk's routed release and the count is
;; 0 (RED); the fd canary alone could never see that.
(let ([n 0]
      [real-free (glob-release)])
  (parameterize ([glob-release (lambda (b) (set! n (+ n 1)) (real-free b))]
                 [glob-fault (lambda () (error 'inject "forced glob unwind"))])
    (guard (e [#t #f]) (posix-glob-fast probe-pat)))
  (test-equal "glob_t freed exactly once on the forced-raise unwind (observer)"
    1 n))

;; winsize -- also pure foreign memory, invisible to the fd canary.  Same shape:
;; winsize-release is the after-thunk's SOLE free path; wrap its default to count
;; and free, force a raise with winsize-fault, assert exactly 1 free on the
;; unwind.  Delete the after-thunk's routed release and the count is 0 (RED).
(let ([n 0]
      [real-free (winsize-release)])
  (parameterize ([winsize-release (lambda (addr) (set! n (+ n 1)) (real-free addr))]
                 [winsize-fault (lambda () (error 'inject "forced winsize unwind"))])
    (guard (e [#t #f]) (terminal-size)))
  (test-equal "winsize freed exactly once on the forced-raise unwind (observer)"
    1 n))

(test-end)
