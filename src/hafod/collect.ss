;;; (hafod collect) -- High-level process output collectors
;;; Procedural forms: run/string*, run/strings*, run/port*, run/sexp*,
;;; run/sexps*, run/port+proc*, run/collecting*, run/file*
;;; Ported from scsh/scheme/process-high-level.scm

(library (hafod collect)
  (export run/port+proc* run/port* run/string* run/strings*
          run/sexp* run/sexps* run/collecting* run/file*
          run-spawn/port run-spawn/file)

  (import (hafod internal base)
          (hafod posix) (hafod compat) (hafod procobj) (hafod fd-ports)
          (hafod process) (hafod port-collect) (hafod temp-file))

  ;; ======================================================================
  ;; run/port+proc*: fork with pipe, return (values port proc)
  ;; ======================================================================

  ;; Forks a child process. Child runs thunk with stdout connected to a pipe.
  ;; Parent receives (values input-port proc-object).
  ;; The input port reads from the child's stdout.
  (define (run/port+proc* thunk)
    (receive (r w) (pipe)
      (let ([proc (fork (lambda ()
                          (close r)
                          (move->fdes w 1)
                          (thunk)))])
        (close w)
        (values r proc))))

  ;; ======================================================================
  ;; run/port*: fork with pipe, return just the port
  ;; ======================================================================

  (define (run/port* thunk)
    (receive (port proc) (run/port+proc* thunk)
      port))

  ;; ======================================================================
  ;; Single-stream collectors
  ;; ======================================================================

  ;; run/string*: capture child stdout as string
  (define (run/string* thunk)
    (close-after (run/port* thunk) port->string))

  ;; run/strings*: capture child stdout as list of lines
  (define (run/strings* thunk)
    (close-after (run/port* thunk) port->string-list))

  ;; run/sexp*: read one S-expression from child stdout
  (define (run/sexp* thunk)
    (close-after (run/port* thunk) read))

  ;; run/sexps*: read all S-expressions from child stdout
  (define (run/sexps* thunk)
    (close-after (run/port* thunk) port->sexp-list))

  ;; ======================================================================
  ;; run/file*: run with stdout to temp file, return filename
  ;; ======================================================================

  (define (run/file* thunk)
    (let ([fname (create-temp-file)])
      ;; Fork child, redirect stdout to the temp file, run thunk
      (let ([proc (fork (lambda ()
                          (let ([port (open-file fname
                                        (bitwise-ior open/write open/create open/truncate))])
                            (move->fdes port 1)
                            (thunk))))])
        (wait proc)
        fname)))

  ;; ======================================================================
  ;; Fast-path helpers using posix_spawn (no fork overhead)
  ;; ======================================================================

  ;; Spawn a program with stdout piped back to parent.
  ;; Returns (values input-port proc-object) — same interface as run/port+proc*.
  (define (run-spawn/port prog args)
    (receive (r w) (pipe)
      (let ([rfd (port->fdes r)]
            [wfd (port->fdes w)])
        (let ([proc (spawn-program prog args
                      (list (list 'dup2 wfd 1)
                            (list 'close rfd)
                            (list 'close wfd)))])
          (close w)
          (release-port-handle r)
          (values r proc)))))

  ;; Spawn a program with stdout to a temp file. Returns filename.
  (define (run-spawn/file prog args)
    (let ([fname (create-temp-file)])
      (let ([proc (spawn-program prog args
                    (list (list 'open 1 fname
                            (bitwise-ior O_WRONLY O_CREAT O_TRUNC) #o644)))])
        (wait proc)
        fname)))

  ;; ======================================================================
  ;; run/collecting*: multi-fd collection via temp files
  ;; ======================================================================

  ;; Collect output from multiple file descriptors.
  ;; FDS is a list of fd numbers to collect from.
  ;; For each fd, a temp-file-channel is created (read port + write port).
  ;; Child: close read ports, redirect write ports to the specified fds, run thunk.
  ;; Parent: close write ports, wait for child, return (values status read-port ...).
  (define (run/collecting* fds thunk)
    (let* ([channels (map (lambda (fd)
                            (receive (r w) (temp-file-channel)
                              (cons r w)))
                          fds)]
           [read-ports (map car channels)]
           [write-ports (map cdr channels)])

      ;; Fork child
      (let ([proc (fork (lambda ()
                          ;; In child: close read ports, redirect write ports to fds
                          (for-each (lambda (port) (close port)) read-ports)
                          (for-each (lambda (w fd) (move->fdes w fd))
                                    write-ports fds)
                          (thunk)))])

        ;; In parent: close write ports, wait for child
        (for-each (lambda (port) (close port)) write-ports)
        (let ([status (wait proc)])
          ;; Return status and all read ports
          (apply values status read-ports)))))

) ;; end library
