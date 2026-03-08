;;; (hafod internal errno) -- Thread-safe errno handling infrastructure for POSIX FFI
;;; Provides &posix-error condition type, raise-posix-error, and posix-call macro.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal errno)
  (export &posix-error make-posix-error posix-error? posix-errno posix-syscall
          raise-posix-error posix-call with-foreign-buffer __errno_location c-strerror)
  (import (chezscheme))

  ;; Load libc symbols from the current process.  Using #f instead of a
  ;; library name works on every platform because libc is always linked
  ;; into the Chez Scheme executable.
  (define load-libc (load-shared-object #f))

  ;; Thread-safe errno access: returns a pointer to the thread-local
  ;; errno variable.  The function name differs across platforms:
  ;; glibc/musl use __errno_location, macOS uses __error.
  (define __errno_location
    (foreign-procedure
      (case (machine-type)
        [(ta6osx tarm64osx ti3osx a6osx arm64osx i3osx) "__error"]
        [else "__errno_location"])
      () uptr))
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

  ;; Core macro: call a POSIX function, check for -1 return, raise condition.
  ;; Usage: (posix-call name expr) where name is the syscall symbol for error messages.
  (define-syntax posix-call
    (syntax-rules ()
      [(_ name expr)
       (let ([result expr])
         (when (= result -1)
           (let ([err (foreign-ref 'int (__errno_location) 0)])
             (raise-posix-error 'name err)))
         result)]))

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
