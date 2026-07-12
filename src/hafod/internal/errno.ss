;;; (hafod internal errno) -- Thread-safe errno handling infrastructure for POSIX FFI
;;; Provides &posix-error condition type, raise-posix-error, and posix-call macro.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal errno)
  (export &posix-error make-posix-error posix-error? posix-errno posix-syscall
          raise-posix-error posix-call with-foreign-buffer __errno_location c-strerror)
  (import (chezscheme) (hafod internal platform)
          (only (hafod internal platform-constants) PLAT-EINTR))

  ;; Load libc symbols from the current process.  Using #f instead of a
  ;; library name works on every platform because libc is always linked
  ;; into the Chez Scheme executable.
  (define load-libc (load-shared-object #f))

  ;; Thread-safe errno access: returns a pointer to the thread-local
  ;; errno variable.  The accessor's foreign name (glibc/musl
  ;; __errno_location vs macOS/FreeBSD __error) is resolved by the hub and
  ;; bound directly against libc (loaded above).
  (define __errno_location
    (foreign-procedure errno-accessor-name () uptr))
  (define c-strerror (foreign-procedure "strerror" (int) string))

  ;; R6RS condition type for POSIX errors.
  ;; Carries the raw errno integer and the syscall name (as a symbol).
  (define-condition-type &posix-error &error
    make-posix-error posix-error?
    (errno posix-errno)
    (syscall posix-syscall))

  ;; Raise a POSIX error condition with errno, syscall name, and human-readable message.
  (define (raise-posix-error who err)
    (raise
      (condition
        (make-posix-error err who)
        (make-message-condition
          (format "~a: ~a (errno ~a)" who (c-strerror err) err))
        (make-irritants-condition (list err)))))

  ;; Core macro: call a POSIX function, retry a signal-interrupted call, else
  ;; check for -1 and raise a condition.
  ;;
  ;; A caught signal (a window resize, Ctrl-C, an alarm) can interrupt a
  ;; blocking syscall, which then returns -1 with errno = PLAT-EINTR. We
  ;; re-issue the call transparently so an interrupted waitpid/read/write does
  ;; not abort a foreground command with a spurious &posix-error. Only EINTR
  ;; loops; every other errno raises exactly as before, and errno is read once
  ;; per attempt. The expression is the raw c-* call; its buffers are allocated
  ;; outside posix-call (in with-foreign-buffer) and are reused across retries.
  ;;
  ;; close() is included in this uniform retry deliberately: on Linux a close
  ;; returning EINTR has already released the descriptor, so a retry could in
  ;; principle touch a reused fd -- but hafod's fd paths are single-threaded and
  ;; the only reachable EINTR here is waitpid, so the single choke-point wins.
  ;;
  ;; Usage: (posix-call name expr) where name is the syscall symbol for error messages.
  (define-syntax posix-call
    (syntax-rules ()
      [(_ name expr)
       (let retry ()
         (let ([result expr])
           (if (= result -1)
               (let ([err (foreign-ref 'int (__errno_location) 0)])
                 (if (= err PLAT-EINTR)
                     (retry)
                     (raise-posix-error 'name err)))
               result)))]))

  ;; Macro: safely allocate one or two foreign buffers with automatic cleanup.
  ;; Usage: (with-foreign-buffer ([buf size]) body ...)
  ;;        (with-foreign-buffer ([buf1 size1] [buf2 size2]) body ...)
  (define-syntax with-foreign-buffer
    (syntax-rules ()
      [(_ ([buf size]) body ...)
       (let ([buf (foreign-alloc size)])
         (dynamic-wind
           (lambda () #f)
           (lambda () body ...)
           (lambda () (foreign-free buf))))]
      [(_ ([buf1 size1] [buf2 size2]) body ...)
       (let ([buf1 (foreign-alloc size1)]
             [buf2 (foreign-alloc size2)])
         (dynamic-wind
           (lambda () #f)
           (lambda () body ...)
           (lambda ()
             (foreign-free buf1)
             (foreign-free buf2))))]))

  ) ; end library
