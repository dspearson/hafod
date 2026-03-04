;;; (hafod dot-locking) -- File dot-locking with retry and stale detection
;;; Ported from scsh/scheme/dot-locking.scm (87 LOC).
;;; Copyright (c) 1994-2003 Olin Shivers. R6RS adaptation (c) 2026 Dominic Pearson.

(library (hafod dot-locking)
  (export obtain-dot-lock release-dot-lock break-dot-lock
          with-dot-lock with-dot-lock*)

  (import (hafod internal base)
          (hafod compat)
          (only (hafod temp-file) create-temp-file)
          (only (hafod fileinfo) create-hard-link delete-file file:last-status-change)
          (only (hafod posix) posix-time posix-sleep))

  ;; ======================================================================
  ;; Lock file naming
  ;; ======================================================================

  (define (make-lock-file-name file-name)
    (string-append file-name ".lock"))

  ;; ======================================================================
  ;; Core operations
  ;; ======================================================================

  ;; release-dot-lock: remove the lock file. Returns #t on success, #f on error.
  (define (release-dot-lock file-name)
    (guard (e [#t #f])
      (delete-file (make-lock-file-name file-name))
      #t))

  ;; maybe-obtain-dot-lock: try to atomically create the lock file.
  ;; Uses hard link for atomicity: create temp file, hard-link to lock name.
  ;; Returns #t on success, #f if lock already exists.
  (define (maybe-obtain-dot-lock file-name)
    (let ([temp-name (create-temp-file file-name)])
      (guard (e [#t (guard (e2 [#t #f]) (delete-file temp-name)) #f])
        (create-hard-link temp-name (make-lock-file-name file-name))
        (guard (e [#t #f]) (delete-file temp-name))
        #t)))

  ;; break-dot-lock: forcibly remove a lock file, ignoring errors.
  (define (break-dot-lock file-name)
    (guard (e [#t (void)])
      (delete-file (make-lock-file-name file-name))))

  ;; ======================================================================
  ;; obtain-dot-lock with retry and stale detection
  ;; ======================================================================

  ;; obtain-dot-lock: try to acquire a dot-lock on file-name.
  ;; Optional args:
  ;;   retry-seconds: seconds between retries (default 1)
  ;;   retry-number:  max retries, or #f for infinite (default #f)
  ;;   stale-time:    seconds before a lock is considered stale (default 300),
  ;;                  or #f to never break stale locks
  ;; Returns: #t on success, 'broken if stale lock was broken, #f on timeout.
  (define (obtain-dot-lock file-name . args)
    (let-optionals* args ([retry-seconds 1]
                          [retry-number #f]
                          [stale-time 300])
      (let loop ([retry-number retry-number]
                 [broken? #f])
        (cond
          [(maybe-obtain-dot-lock file-name)
           (if broken? 'broken #t)]
          ;; Check for stale lock
          [(and stale-time
                (guard (e [#t #f])
                  (> (posix-time)
                     (+ (file:last-status-change (make-lock-file-name file-name))
                        stale-time))))
           (break-dot-lock file-name)
           (loop retry-number #t)]
          [else
           ;; Sleep and retry
           (posix-sleep retry-seconds)
           (cond
             [(not retry-number) (loop retry-number broken?)]
             [(> retry-number 0) (loop (- retry-number 1) broken?)]
             [else #f])]))))

  ;; ======================================================================
  ;; with-dot-lock (dynamic-wind based)
  ;; ======================================================================

  ;; with-dot-lock*: acquire lock, run thunk, release lock.
  ;; Lock is released even on exception (via dynamic-wind).
  (define (with-dot-lock* file-name thunk)
    (dynamic-wind
      (lambda () (obtain-dot-lock file-name))
      thunk
      (lambda () (release-dot-lock file-name))))

  (define-syntax with-dot-lock
    (syntax-rules ()
      [(_ file-name body ...)
       (with-dot-lock* file-name (lambda () body ...))]))

) ;; end library
