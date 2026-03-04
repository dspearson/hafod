;;; (hafod fd-ports) -- fd-port system: mapping between Unix file descriptors
;;; and Scheme ports with revealed-count lifecycle management.
;;; Ported from scsh/scheme/newports.scm and scsh/scheme/fdports.scm
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod fd-ports)
  (export
    ;; Core port creation/retrieval
    fdes->inport fdes->outport port->fdes

    ;; Revealed count management
    release-port-handle port-revealed
    call/fdes sleazy-call/fdes

    ;; Predicates
    fdport? open-fdport? fd/port?

    ;; Close operations
    close close-fdes close-after

    ;; Pipe
    pipe

    ;; Flush
    flush-all-ports flush-all-ports-no-threads

    ;; Initialization
    init-fdports!

    ;; Guardian drain (for Phase 5 to call before fork)
    drain-port-guardian!

    ;; Move and dup (Plan 02)
    move->fdes dup dup->fdes dup->inport dup->outport

    ;; Open file (Plan 02)
    open-file open-input-file open-output-file

    ;; File option constants (Plan 02)
    open/read open/write open/read+write
    open/create open/truncate open/append open/exclusive open/non-blocking

    ;; Port rebinding macros (Plan 02)
    with-current-input-port with-current-output-port with-current-error-port
    with-current-input-port* with-current-output-port* with-current-error-port*

    ;; Shell helper (internal, for EPF later)
    shell-open

    ;; Seek/tell
    seek tell seek/set seek/delta seek/end

    ;; Error output port
    error-output-port
    with-error-output-port with-error-output-port*
    with-stdio-ports with-stdio-ports*

    ;; File option aliases
    create+trunc write+append+create read-only

    ;; scsh-compatible alias
    force-output

    ;; file-options enumeration (scsh-compatible)
    file-options file-options-on? file-options-union

    ;; Internal (needed by later phases)
    set-fdport! delete-fdport! maybe-ref-fdport evict-ports
    make-input-fdport make-output-fdport
    %set-cloexec
    *fd-table* *port-table* *port-guardian*)

  (import (hafod internal base)
          (hafod posix) (hafod compat))

  ;; ======================================================================
  ;; Data Structures
  ;; ======================================================================

  ;; Forward table: fd (integer) -> (port-or-weak . revealed-count)
  ;; When revealed=0, car is (weak-cons port #f)
  ;; When revealed>0, car is the port directly
  (define *fd-table* (make-eqv-hashtable))

  ;; Reverse table: port (object identity) -> fd (integer)
  (define *port-table* (make-eq-hashtable))

  ;; Guardian for GC-driven cleanup of unrevealed ports
  (define *port-guardian* (make-guardian))

  ;; ======================================================================
  ;; Helper: close-on-exec flag management
  ;; ======================================================================

  (define (%set-cloexec fd on?)
    (let ([flags (posix-fcntl fd F_GETFD)])
      (posix-fcntl fd F_SETFD
        (if on?
            (bitwise-ior flags FD_CLOEXEC)
            (bitwise-and flags (bitwise-not FD_CLOEXEC))))))

  ;; ======================================================================
  ;; Core table operations
  ;; ======================================================================

  ;; Sets the port and reveal count for fd, and always replaces if fd was
  ;; already in the table.
  ;; When revealed=0: store weak reference (weak-cons), set close-on-exec.
  ;; When revealed>0: store strong reference, clear close-on-exec.
  (define (set-fdport! fd port revealed)
    ;; Remove old entry first
    (delete-fdport! fd)
    (let ([entry (if (zero? revealed)
                     (cons (weak-cons port #f) revealed)
                     (cons port revealed))])
      (hashtable-set! *fd-table* fd entry)
      (hashtable-set! *port-table* port fd)
      (%set-cloexec fd (zero? revealed))
      ;; Register with guardian when unrevealed so GC can clean up
      (when (zero? revealed)
        (*port-guardian* port))))

  ;; Removes fd from both tables.
  (define (delete-fdport! fd)
    (let ([entry (hashtable-ref *fd-table* fd #f)])
      (when entry
        (let ([port-or-weak (car entry)]
              [revealed (cdr entry)])
          ;; Remove from reverse table
          (let ([port (if (zero? revealed)
                          ;; Weak reference case
                          (let ([p (car port-or-weak)])
                            (if (bwp-object? p) #f p))
                          ;; Strong reference case
                          port-or-weak)])
            (when port
              (hashtable-delete! *port-table* port))))
        (hashtable-delete! *fd-table* fd))))

  ;; Returns (port . revealed) or #f.
  ;; If weak ref has been GC'd, cleans up and returns #f.
  (define (maybe-ref-fdport fd)
    (let ([entry (hashtable-ref *fd-table* fd #f)])
      (and entry
           (let ([port-or-weak (car entry)]
                 [revealed (cdr entry)])
             (if (zero? revealed)
                 ;; Weak reference: port-or-weak is (weak-cons port #f)
                 (let ([port (car port-or-weak)])
                   (if (bwp-object? port)
                       (begin (hashtable-delete! *fd-table* fd)
                              #f)
                       (cons port revealed)))
                 ;; Strong reference: port-or-weak is the port itself
                 (cons port-or-weak revealed))))))

  ;; ======================================================================
  ;; Guardian drain
  ;; ======================================================================

  (define (drain-port-guardian!)
    (let loop ()
      (let ([port (*port-guardian*)])
        (when port
          (let ([fd (hashtable-ref *port-table* port #f)])
            (when fd
              (hashtable-delete! *fd-table* fd)
              (hashtable-delete! *port-table* port)
              ;; Close the fd if still open (ignore errors)
              (guard (e [#t #f]) (posix-close fd))))
          (loop)))))

  ;; ======================================================================
  ;; Port creation
  ;; ======================================================================

  ;; Create an input port from an fd and register it in the table.
  (define (make-input-fdport fd revealed)
    (let ([port (transcoded-port
                  (open-fd-input-port fd (buffer-mode block))
                  (make-transcoder (utf-8-codec) (eol-style none)
                                   (error-handling-mode replace)))])
      (set-fdport! fd port revealed)
      port))

  ;; Create an output port from an fd and register it in the table.
  (define (make-output-fdport fd revealed)
    (let ([port (transcoded-port
                  (open-fd-output-port fd (buffer-mode line))
                  (make-transcoder (utf-8-codec) (eol-style none)
                                   (error-handling-mode replace)))])
      (set-fdport! fd port revealed)
      port))

  ;; ======================================================================
  ;; fdes->inport / fdes->outport / port->fdes
  ;; ======================================================================

  ;; Internal: if fd already has a port, increment revealed and return it.
  ;; Otherwise create a new port using port-maker.
  (define (fdes->port fd port-maker)
    (let ([ref (maybe-ref-fdport fd)])
      (cond
        [ref
         (let ([port (car ref)])
           (increment-revealed-count port 1)
           port)]
        [else (port-maker fd 1)])))

  ;; Create or retrieve an input port for fd.
  (define (fdes->inport fd)
    (let ([port (fdes->port fd make-input-fdport)])
      (if (not (input-port? port))
          (error 'fdes->inport "fd is already assigned to an output port" fd)
          port)))

  ;; Create or retrieve an output port for fd.
  (define (fdes->outport fd)
    (let ([port (fdes->port fd make-output-fdport)])
      (if (not (output-port? port))
          (error 'fdes->outport "fd is already assigned to an input port" fd)
          port)))

  ;; Return the fd underlying a port. Increments revealed count.
  (define (port->fdes port)
    (check-arg open-fdport? port port->fdes)
    (increment-revealed-count port 1)
    (hashtable-ref *port-table* port #f))

  ;; ======================================================================
  ;; Revealed count management
  ;; ======================================================================

  (define (increment-revealed-count port delta)
    (let ([fd (hashtable-ref *port-table* port #f)])
      (when fd
        (let ([ref (maybe-ref-fdport fd)])
          (when ref
            (let ([new-count (+ (cdr ref) delta)])
              (set-fdport! fd port new-count)))))))

  ;; Decrement revealed count. If it drops to 0, switch to weak ref.
  (define (release-port-handle port)
    (let ([fd (hashtable-ref *port-table* port #f)])
      (when fd
        (let ([ref (maybe-ref-fdport fd)])
          (when (and ref (> (cdr ref) 0))
            (let ([new-rev (- (cdr ref) 1)])
              (set-fdport! fd port new-rev)))))))

  ;; Return revealed count, or #f if 0 (GC-managed).
  (define (port-revealed port)
    (check-arg fdport? port port-revealed)
    (let ([fd (hashtable-ref *port-table* port #f)])
      (and fd
           (let ([ref (maybe-ref-fdport fd)])
             (and ref
                  (let ([count (cdr ref)])
                    (and (not (zero? count)) count)))))))

  ;; ======================================================================
  ;; call/fdes and sleazy-call/fdes
  ;; ======================================================================

  ;; Safely get fd from fd/port, managing revealed count.
  (define (call/fdes fd/port proc)
    (cond
      [(integer? fd/port)
       (proc fd/port)]
      [(fdport? fd/port)
       (let ([port fd/port])
         (dynamic-wind
           (lambda ()
             (when (not port) (error 'call/fdes "Cannot re-enter call/fdes")))
           (lambda () (proc (port->fdes port)))
           (lambda ()
             (release-port-handle port)
             (set! port #f))))]
      [else (error 'call/fdes "Not a file descriptor or fdport" fd/port)]))

  ;; Like call/fdes but doesn't modify revealed count.
  (define (sleazy-call/fdes fd/port proc)
    (proc (cond
            [(integer? fd/port) fd/port]
            [(fdport? fd/port)
             (hashtable-ref *port-table* fd/port #f)]
            [else (error 'sleazy-call/fdes "Not a file descriptor or fdport" fd/port)])))

  ;; ======================================================================
  ;; Predicates
  ;; ======================================================================

  (define (fdport? x)
    (and (or (input-port? x) (output-port? x))
         (hashtable-ref *port-table* x #f)
         #t))

  (define (open-fdport? x)
    (and (fdport? x)
         (not (port-closed? x))))

  (define (fd/port? x)
    (or (and (integer? x) (>= x 0))
        (input-port? x)
        (output-port? x)))

  ;; ======================================================================
  ;; Close operations
  ;; ======================================================================

  ;; Apply f to port, close port, return f's result.
  (define (close-after port f)
    (receive vals (f port)
      (close port)
      (apply values vals)))

  ;; Remove a port from the table by port identity.
  (define (delete-fdport-by-port! port)
    (let ([fd (hashtable-ref *port-table* port #f)])
      (when fd
        (delete-fdport! fd))))

  ;; Polymorphic close: handles both ports and integer fds.
  (define (close port/fd)
    ((cond
       [(integer? port/fd) close-fdes]
       [(output-port? port/fd)
        (lambda (p)
          (when (not (port-closed? p)) (flush-output-port p))
          (close-output-port p)
          (delete-fdport-by-port! p))]
       [(input-port? port/fd)
        (lambda (p)
          (close-input-port p)
          (delete-fdport-by-port! p))]
       [else (error 'close "Not a file descriptor or port" port/fd)])
     port/fd))

  ;; Evict any port at fd to a new fd number.
  ;; Returns #t if eviction happened, #f otherwise.
  (define (evict-ports fd)
    (let ([ref (maybe-ref-fdport fd)])
      (cond
        [ref
         (let* ([port (car ref)]
                [new-fd (posix-dup fd)]
                [is-input (input-port? port)])
           ;; Remove old mapping
           (delete-fdport! fd)
           ;; Flush output before closing
           (when (output-port? port) (flush-output-port port))
           ;; Close old port (this closes old fd via Chez internals)
           (close-port port)
           ;; Create new port on the new fd
           (if is-input
               (make-input-fdport new-fd 0)
               (make-output-fdport new-fd 0))
           #t)]
        [else #f])))

  ;; Close an fd. If it has a mapped port, evict the port first.
  ;; If the fd doesn't exist and no port was evicted, ignore the error.
  (define (close-fdes fd)
    (if (evict-ports fd)
        ;; Port was evicted (dup'd to new fd, old fd freed by close-port)
        ;; The old fd is already closed by close-port, so we're done
        (void)
        ;; No port mapped -- try to close the fd (ignore EBADF)
        (guard (e [(posix-error? e) (void)])
          (posix-close fd))))

  ;; ======================================================================
  ;; Pipe
  ;; ======================================================================

  ;; Create a pipe. Returns (values read-port write-port).
  ;; Both ports start with revealed=0 (GC-managed).
  (define (pipe)
    (let ([fds (posix-pipe)])
      (let ([r (fdes->inport (car fds))]
            [w (fdes->outport (cdr fds))])
        (release-port-handle r)
        (release-port-handle w)
        (values r w))))

  ;; ======================================================================
  ;; Flush all ports
  ;; ======================================================================

  ;; Flush all tracked output ports.
  (define (flush-all-ports)
    (let-values ([(keys vals) (hashtable-entries *fd-table*)])
      (vector-for-each
        (lambda (fd entry)
          (let ([port-or-weak (car entry)]
                [revealed (cdr entry)])
            (let ([port (if (zero? revealed)
                            (let ([p (car port-or-weak)])
                              (if (bwp-object? p) #f p))
                            port-or-weak)])
              (when (and port (output-port? port) (not (port-closed? port)))
                (flush-output-port port)))))
        keys vals)))

  ;; Non-threaded version (same for v1 since we don't have threads).
  (define flush-all-ports-no-threads flush-all-ports)

  ;; ======================================================================
  ;; Move and dup operations (Plan 02 additions)
  ;; ======================================================================

  ;; Move an fd/port to target fd. Evicts any port at target.
  (define (move->fdes fd/port target)
    (cond
      [(integer? fd/port)
       (when (not (= fd/port target))
         (evict-ports target)
         (posix-dup2 fd/port target)
         (posix-close fd/port))
       target]

      [(fdport? fd/port)
       (let ([old-fd (hashtable-ref *port-table* fd/port #f)])
         (unless old-fd
           (error 'move->fdes "port not in fd-port table" fd/port))
         (cond
           [(= old-fd target)
            ;; Already at the target fd -- just ensure revealed is 1
            (set-fdport! old-fd fd/port 1)
            fd/port]
           [else
            ;; Evict anything at target, dup2 to target
            (evict-ports target)
            (posix-dup2 old-fd target)
            ;; Remove old mapping and close old port (closes old fd)
            (let ([is-input (input-port? fd/port)])
              (delete-fdport! old-fd)
              (when (output-port? fd/port) (flush-output-port fd/port))
              (close-port fd/port)
              ;; Create new port on target
              (if is-input
                  (make-input-fdport target 1)
                  (make-output-fdport target 1)))]))]

      [else (error 'move->fdes "Not a file descriptor or fdport" fd/port)]))

  ;; Polymorphic dup: returns same type as input.
  (define (dup fd/port . maybe-target)
    (check-arg fd/port? fd/port dup)
    (apply (cond
             [(integer? fd/port) dup->fdes]
             [(input-port? fd/port) dup->inport]
             [(output-port? fd/port) dup->outport])
           fd/port maybe-target))

  ;; Always returns integer fd.
  (define (dup->fdes fd/port . maybe-target)
    (check-arg fd/port? fd/port dup->fdes)
    (if (pair? maybe-target)
        (let ([target (car maybe-target)])
          (close-fdes target)
          (sleazy-call/fdes fd/port (lambda (fd) (posix-dup2 fd target))))
        (sleazy-call/fdes fd/port (lambda (fd) (posix-dup fd)))))

  ;; Returns input port on duped fd.
  (define (dup->inport fd/port . maybe-target)
    (let ([fd (apply dup->fdes fd/port maybe-target)])
      (make-input-fdport fd (if (null? maybe-target) 0 1))))

  ;; Returns output port on duped fd.
  (define (dup->outport fd/port . maybe-target)
    (let ([fd (apply dup->fdes fd/port maybe-target)])
      (make-output-fdport fd (if (null? maybe-target) 0 1))))

  ;; ======================================================================
  ;; File option constants
  ;; ======================================================================

  (define open/read         O_RDONLY)
  (define open/write        O_WRONLY)
  (define open/read+write   O_RDWR)
  (define open/create       O_CREAT)
  (define open/truncate     O_TRUNC)
  (define open/append       O_APPEND)
  (define open/exclusive    O_EXCL)
  (define open/non-blocking O_NONBLOCK)

  ;; ======================================================================
  ;; Open file
  ;; ======================================================================

  ;; Open a file and return a tracked port.
  (define (open-file fname flags . maybe-mode)
    (let* ([mode (:optional maybe-mode #o666)]
           [fd (posix-open fname flags mode)])
      (if (or (not (zero? (bitwise-and flags O_WRONLY)))
              (not (zero? (bitwise-and flags O_RDWR))))
          (make-output-fdport fd 0)
          (make-input-fdport fd 0))))

  ;; Open a file for reading.
  (define (open-input-file fname . maybe-options)
    (let ([options (:optional maybe-options open/read)])
      (open-file fname options)))

  ;; Open a file for writing (default: create + truncate).
  (define (open-output-file fname . rest)
    (let* ([options (if (pair? rest) (car rest)
                        (bitwise-ior open/write open/create open/truncate))]
           [maybe-mode (if (and (pair? rest) (pair? (cdr rest))) (cdr rest) '())])
      (apply open-file fname options maybe-mode)))

  ;; Shell helper: open file, move to specific fd.
  (define (shell-open path flags fdes)
    (let ([port (open-file (stringify path) flags)])
      (move->fdes port fdes)))

  ;; ======================================================================
  ;; Port rebinding macros
  ;; ======================================================================

  (define-simple-syntax (with-current-input-port port body ...)
    (parameterize ([current-input-port port]) body ...))

  (define-simple-syntax (with-current-output-port port body ...)
    (parameterize ([current-output-port port]) body ...))

  (define-simple-syntax (with-current-error-port port body ...)
    (parameterize ([current-error-port port]) body ...))

  (define (with-current-input-port* port thunk)
    (parameterize ([current-input-port port]) (thunk)))

  (define (with-current-output-port* port thunk)
    (parameterize ([current-output-port port]) (thunk)))

  (define (with-current-error-port* port thunk)
    (parameterize ([current-error-port port]) (thunk)))

  ;; ======================================================================
  ;; Seek / Tell
  ;; ======================================================================

  (define seek/set  SEEK_SET)
  (define seek/delta SEEK_CUR)
  (define seek/end  SEEK_END)

  ;; Seek on an fd or port. Returns new position.
  (define (seek fd/port offset whence)
    (sleazy-call/fdes fd/port
      (lambda (fd)
        (when (and (port? fd/port) (output-port? fd/port))
          (flush-output-port fd/port))
        (posix-lseek fd offset whence))))

  ;; Tell current position of an fd or port.
  (define (tell fd/port)
    (seek fd/port 0 seek/delta))

  ;; ======================================================================
  ;; Error output port
  ;; ======================================================================

  (define (error-output-port) (current-error-port))

  (define (with-error-output-port* port thunk)
    (parameterize ([current-error-port port]) (thunk)))

  (define-simple-syntax (with-error-output-port port body ...)
    (with-error-output-port* port (lambda () body ...)))

  ;; ======================================================================
  ;; with-stdio-ports / with-stdio-ports* -- rebind all three stdio ports
  ;; ======================================================================

  (define (with-stdio-ports* thunk)
    (let ([in  (fdes->inport 0)]
          [out (fdes->outport 1)]
          [err (fdes->outport 2)])
      (with-current-input-port* in
        (lambda ()
          (with-current-output-port* out
            (lambda ()
              (with-current-error-port* err thunk)))))))

  (define-simple-syntax (with-stdio-ports body ...)
    (with-stdio-ports* (lambda () body ...)))

  ;; ======================================================================
  ;; File option aliases (compound flags)
  ;; ======================================================================

  (define create+trunc (bitwise-ior open/write open/create open/truncate))
  (define write+append+create (bitwise-ior open/write open/append open/create))
  (define read-only open/read)

  ;; ======================================================================
  ;; Initialization
  ;; ======================================================================

  ;; Install stdin/stdout/stderr in the fd-port table.
  (define (init-fdports!)
    (set-fdport! 0 (current-input-port) 1)
    (set-fdport! 1 (current-output-port) 1)
    (set-fdport! 2 (current-error-port) 1))

  ;; scsh-compatible alias
  (define force-output flush-all-ports)

  ;; ======================================================================
  ;; file-options: scsh-compatible enumerated file open flags
  ;; (file-options read-only) => open/read
  ;; (file-options create truncate write-only) => bitwise-ior of flags
  ;; ======================================================================

  (define-syntax file-options
    (lambda (stx)
      (define (option->flag opt)
        (case (syntax->datum opt)
          [(read-only)      #'open/read]
          [(write-only)     #'open/write]
          [(read-write)     #'open/read+write]
          [(create)         #'open/create]
          [(truncate)       #'open/truncate]
          [(append)         #'open/append]
          [(exclusive)      #'open/exclusive]
          [(non-blocking)   #'open/non-blocking]
          [else (syntax-violation 'file-options "unknown file option" stx opt)]))
      (syntax-case stx ()
        [(_) #'0]
        [(_ opt)
         (option->flag #'opt)]
        [(_ opt rest ...)
         (with-syntax ([f (option->flag #'opt)])
           #'(bitwise-ior f (file-options rest ...)))])))

  (define (file-options-on? options test-options)
    (= (bitwise-and options test-options) test-options))

  (define (file-options-union . option-sets)
    (fold-left bitwise-ior 0 option-sets))

  ;; Auto-initialize on library load
  (init-fdports!)

  ) ; end library
