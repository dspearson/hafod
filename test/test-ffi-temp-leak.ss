#!chezscheme
;;; test/test-ffi-temp-leak.ss -- A forced-error proof that the temp-file and
;;; here-string helpers release the on-disk temp file when a step after mkstemp
;;; raises, instead of leaving it behind.
;;;
;;; temp-file-channel (temp-file.ss) holds TWO descriptors at once: the mkstemp
;;; descriptor wrapped as an output port PLUS a read re-open of the same path.
;;; With exactly one free descriptor the mkstemp succeeds -- the file lands on
;;; disk -- and the read re-open raises EMFILE, landing in the window after the
;;; temp file exists but before it is unlinked.  A tree that stages its unwind
;;; closes the port and unlinks the path, so its temp directory is empty; a tree
;;; that does not leaves a hafod-chan-* file behind.  This is the non-vacuous
;;; assertion: it fails on an unstaged tree and passes on a staged one.
;;;
;;; open-string-source (syntax.ss) closes its writer before re-opening for
;;; reading, so it needs only one descriptor at a time and completes even under
;;; the same pressure; the second case confirms the here-string path also leaves
;;; no temp file behind.
;;;
;;; The descriptor pressure is made deterministic by lowering the soft
;;; RLIMIT_NOFILE to a small cap, so the proof does not depend on the ambient
;;; ulimit (which may be very large).  Both helpers read TMPDIR at call time, so
;;; a fresh, dedicated directory exported as TMPDIR isolates the assertion: any
;;; surviving entry is a leak.  A fork+SIGKILL watchdog bounds a hang.  Entirely
;;; PTY-free.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (hafod temp-file) (hafod fd-ports) (hafod posix)
        (hafod compat) (hafod environment) (hafod fileinfo)
        (only (hafod syntax) open-string-source
              open-string-post-open-fault open-string-read-close)
        (only (hafod internal platform) os-family)
        (except (chezscheme) vector-append open-input-file open-output-file getenv
                file-exists? delete-file truncate-file)
        (test runner))

;; ---- Descriptor budget --------------------------------------------------
;; Expose the process's libc symbols to the FFI (the same idiom the runtime
;; uses) so getrlimit/setrlimit resolve at foreign-procedure creation time.
(load-shared-object #f)

;; struct rlimit is two 64-bit words { rlim_cur; rlim_max; } on Linux, macOS and
;; FreeBSD; the RLIMIT_NOFILE resource number is 7 on Linux and 8 on the
;; BSD-derived platforms.  getrlimit fills both words so the read-modify-write
;; below keeps rlim_max and only shrinks rlim_cur.
(define rlimit-nofile (case os-family [(macos freebsd) 8] [else 7]))
(define c-getrlimit (foreign-procedure "getrlimit" (int void*) int))
(define c-setrlimit (foreign-procedure "setrlimit" (int void*) int))

(define (cap-open-descriptors! soft)
  (let ([buf (foreign-alloc 16)])
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (c-getrlimit rlimit-nofile buf)
        (foreign-set! 'unsigned-64 buf 0 soft)   ; rlim_cur := soft; rlim_max kept
        (c-setrlimit rlimit-nofile buf))
      (lambda () (foreign-free buf)))))

;; ---- Watchdog -----------------------------------------------------------
;; Fork a sibling that SIGKILLs this process after `seconds`, so a regression
;; that hangs dies deterministically instead of stalling the suite.  victim is
;; bound in an OUTER let, fully evaluated before the inner fork (Chez runs let
;; inits right-to-left), so the child never captures its own pid and kills
;; itself.  posix-_exit avoids flushing the inherited output buffer twice.
(define (with-watchdog seconds thunk)
  (let ([victim (posix-getpid)])
    (let ([wpid (posix-fork)])
      (if (zero? wpid)
          (begin (posix-sleep seconds) (posix-kill victim SIGKILL) (posix-_exit 0))
          (let ([result (thunk)])
            (posix-kill wpid SIGKILL)
            (posix-waitpid wpid 0)
            result)))))

;; ---- Directory + descriptor helpers ------------------------------------

;; Count every entry (dot-files included) in `dir`.
(define (entry-count dir)
  (length (directory-files dir #t)))

;; Remove any surviving entries, then the directory itself; fully guarded so a
;; teardown mishap never wedges the suite.
(define (remove-tree! dir)
  (guard (e [#t #f])
    (for-each
      (lambda (f) (guard (inner [#t #f]) (delete-file (string-append dir "/" f))))
      (directory-files dir #t)))
  (guard (e [#t #f]) (delete-directory dir)))

;; Duplicate descriptor 0 until the soft cap raises; return the descriptors
;; obtained (the raise merely stops the loop).
(define (exhaust-descriptors)
  (let loop ([acc '()])
    (let ([fd (guard (e [#t #f]) (posix-dup 0))])
      (if fd (loop (cons fd acc)) acc))))

(define (close-descriptors! fds)
  (for-each (lambda (fd) (guard (e [#t #f]) (posix-close fd))) fds))

;; Invoke `make-ports` with exactly one free descriptor and TMPDIR pointed at a
;; fresh empty directory, then return that directory's surviving-entry count.
;; With one free descriptor mkstemp consumes it (the file lands on disk) and the
;; next descriptor request raises.  Descriptors and TMPDIR are restored before
;; the count is taken (directory-files itself needs a descriptor).
(define (leak-check-count label make-ports)
  (let ([dir (string-append "/tmp/hafod-tmp-leak-" label "-"
                            (number->string (posix-getpid)))]
        [saved (getenv "TMPDIR")])
    (remove-tree! dir)                ; clear any stale directory from a prior run
    (create-directory dir #o700)
    (setenv "TMPDIR" dir)
    (let ([count
           (with-watchdog 8
             (lambda ()
               (let ([dups (exhaust-descriptors)])
                 (when (pair? dups)
                   (guard (e [#t #f]) (posix-close (car dups))))
                 (guard (e [#t #f]) (make-ports))
                 (close-descriptors! dups)
                 (entry-count dir))))])
      (setenv "TMPDIR" (or saved #f))
      (remove-tree! dir)
      count)))

(test-begin "ffi-temp-leak")

;; Shrink the descriptor budget once; the process exits at test end, so the cap
;; needs no restoring.  Well above Chez's own open descriptors yet small enough
;; to exhaust in a few dozen dups.
(cap-open-descriptors! 128)

;; temp-file-channel: the non-vacuous leak proof.  The read re-open raises EMFILE
;; inside the post-mkstemp window; a staged unwind unlinks the temp path.
(test-equal "temp-file-channel leaves no temp file after a forced post-mkstemp error"
  0
  (leak-check-count "chan"
    (lambda ()
      (receive (iport oport) (temp-file-channel)
        ;; Not reached under the forced error; tidy up if the cap was too high.
        (guard (e [#t #f]) (close iport))
        (guard (e [#t #f]) (close oport))))))

;; open-string-source: the here-string path under the same descriptor pressure.
;; It self-heals (closes its writer before re-opening), so it completes and must
;; still leave no temp file behind.
(test-equal "open-string-source leaves no temp file under descriptor pressure"
  0
  (leak-check-count "heredoc"
    (lambda ()
      (let ([inp (open-string-source "here-string-body")])
        (guard (e [#t #f]) (close inp))))))

;; ---- Read-port release on a forced post-open raise ----------------------
;; The existing two cases force their error at the read-OPEN (EMFILE), a window
;; that never binds the read port, so they cannot see the read-port leak.  These
;; two drive a raise in the window AFTER the read port is opened and BEFORE the
;; unlink (the documented posix-unlink-failure trigger) via the post-open fault
;; seam, and count the staged read-port release through the routed close seam.
;;
;; The count-based observer -- not the lowest-free-fd canary -- is used here on
;; purpose.  For temp-file-channel the mkstemp descriptor takes the low fd (the
;; write port) and the read open takes a higher fd; the pre-fix unwind frees the
;; low write fd while leaking the high read fd, so lowest-free returns to the low
;; slot unchanged and the leak is invisible to that probe.  The routed close is
;; both the release and its observation, so the two cannot silently decouple.
;;
;; On the pre-fix tree the read port is opened in an inner let the guard cannot
;; see, so on the forced raise it is never closed and the observer never fires
;; (count 0) -- the `count = 1' assertion fails (RED).  Staging the read port into
;; the guard closes it once on the unwind (count 1, GREEN).  Deleting the staged
;; close returns the count to 0 (RED again).  The seam still closes the port, so
;; the test itself leaks no descriptor.

(test-equal "temp-file-channel releases the staged read port on a forced post-open error"
  1
  (let ([counter 0])
    (parameterize ([temp-read-close (lambda (p) (set! counter (+ counter 1)) (close p))]
                   [temp-file-post-open-fault (lambda () (error 'forced "post-open fault"))])
      (guard (e [#t #f])
        (receive (iport oport) (temp-file-channel)
          (guard (inner [#t #f]) (close iport))
          (guard (inner [#t #f]) (close oport)))))
    counter))

(test-equal "open-string-source releases the staged read port on a forced post-open error"
  1
  (let ([counter 0])
    (parameterize ([open-string-read-close (lambda (p) (set! counter (+ counter 1)) (close p))]
                   [open-string-post-open-fault (lambda () (error 'forced "post-open fault"))])
      (guard (e [#t #f])
        (let ([inp (open-string-source "here-string-body")])
          (guard (inner [#t #f]) (close inp)))))
    counter))

(test-end)
