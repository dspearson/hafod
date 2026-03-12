;;; (hafod system) -- System identification, errno utilities, and version info
;;; Provides uname, errno-error, with-errno-handler*, with-errno-handler,
;;; and hafod version constants.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod system)
  (export
    ;; uname
    uname uname-info? uname:os-name uname:node-name uname:release
    uname:version uname:machine

    ;; errno utilities
    errno-error with-errno-handler*
    with-errno-handler

    ;; version
    hafod-major-version hafod-minor-version hafod-version-string
    ;; scsh-compatible version aliases
    scsh-major-version scsh-minor-version scsh-version-string
    scsh-release-name
    ;; System identification
    system-name)

  (import (chezscheme)
          (hafod posix))

  ;; ======================================================================
  ;; uname -- system identification record
  ;; ======================================================================

  (define-record-type uname-info
    (fields os-name node-name release version machine))

  ;; Convenient scsh-style accessors
  (define uname:os-name uname-info-os-name)
  (define uname:node-name uname-info-node-name)
  (define uname:release uname-info-release)
  (define uname:version uname-info-version)
  (define uname:machine uname-info-machine)

  ;; (uname) -> uname-info record
  (define (uname)
    (let-values ([(sysname nodename release version machine) (posix-uname)])
      (make-uname-info sysname nodename release version machine)))

  ;; ======================================================================
  ;; errno-error -- raise a structured POSIX error condition
  ;; ======================================================================

  ;; Raises a posix error condition with the given errno, who, message, and args.
  ;; The condition is compatible with posix-error? and posix-errno accessors.
  (define (errno-error errno who msg . args)
    (raise
      (condition
        (make-posix-error errno who)
        (make-message-condition
          (format "~a: ~a (errno ~a)" msg (c-strerror errno) errno))
        (make-irritants-condition args))))

  ;; ======================================================================
  ;; with-errno-handler* -- catch POSIX errors
  ;; ======================================================================

  ;; handler is (lambda (errno exn) ...) -- called when thunk raises a posix error.
  ;; Returns the handler's result on error, or the thunk's result on success.
  (define (with-errno-handler* handler thunk)
    (guard (e [(posix-error? e)
               (handler (posix-errno e) e)])
      (thunk)))

  ;; ======================================================================
  ;; with-errno-handler -- syntax for errno-dispatching error handler
  ;; ======================================================================

  ;; Usage:
  ;;   (with-errno-handler
  ;;     (((errno/noent) (display "not found"))
  ;;      ((errno/acces errno/perm) (display "no permission")))
  ;;     body ...)
  ;;
  ;; Each clause is ((errno-val ...) body ...). If the caught errno matches
  ;; any value in the list, the clause body is evaluated. If no clause matches,
  ;; the exception is re-raised.
  (define-syntax with-errno-handler
    (syntax-rules ()
      [(_ ((errno-list body ...) ...) expr ...)
       (with-errno-handler*
         (lambda (errno exn)
           (cond
             [(memv errno 'errno-list) body ...]
             ...
             [else (raise exn)]))
         (lambda () expr ...))]))

  ;; ======================================================================
  ;; Version constants
  ;; ======================================================================

  (define hafod-major-version 1)
  (define hafod-minor-version 3)
  (define hafod-version-string "hafod 1.3.2")

  ;; scsh-compatible version aliases
  (define scsh-major-version hafod-major-version)
  (define scsh-minor-version hafod-minor-version)
  (define scsh-version-string hafod-version-string)
  (define scsh-release-name "hafod")

  ;; system-name: returns the hostname (scsh-compatible)
  (define (system-name)
    (let-values ([(sysname nodename release version machine) (posix-uname)])
      nodename))

  ) ; end library
