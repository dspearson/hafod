;;; (hafod internal posix-core) -- Wait-status macros, helpers, and process syscalls
;;; Extracted from posix.ss during Phase 26 splitting.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal posix-core)
  (export
    ;; Wait status macros
    status:exit-val status:term-sig status:stop-sig
    wait/poll wait/stopped-children

    ;; Helper utilities
    ptr->string strings->c-argv free-c-argv bv-cstring

    ;; Process syscalls
    posix-fork posix-_exit posix-exec posix-execve posix-waitpid
    posix-pipe posix-dup posix-dup2 posix-close posix-open
    posix-read posix-write posix-kill posix-sleep posix-pause

    ;; posix_spawn fast path
    posix-spawnp posix-spawnp/pipe)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants))

  (define load-libc (load-shared-object "libc.so.6"))

  ;; ======================================================================
  ;; Wait status macros (ported from scsh/scheme/waitcodes.scm)
  ;; ======================================================================

  ;; wait/poll = WNOHANG, wait/stopped-children = WUNTRACED
  (define wait/poll 1)
  (define wait/stopped-children 2)

  ;; Extract exit value from normal termination: status has low 7 bits = 0
  ;; and exit code in bits 15..8.
  (define (status:exit-val status)
    (and (zero? (bitwise-and #x7F status))
         (bitwise-and #xFF (ash status -8))))

  ;; Extract stop signal: status has low byte = 0x7F
  (define (status:stop-sig status)
    (and (= #x7F (bitwise-and status #xFF))
         (bitwise-and #xFF (ash status -8))))

  ;; Extract termination signal: low 7 bits != 0 and != 0x7F
  (define (status:term-sig status)
    (let ([termsig (bitwise-and status #x7F)])
      (and (not (zero? termsig))
           (not (= #x7F (bitwise-and status #xFF)))
           termsig)))

  ;; ======================================================================
  ;; Helper utilities
  ;; ======================================================================

  ;; Extract a null-terminated C string from a pointer address.
  (define (ptr->string ptr)
    (let loop ([i 0] [chars '()])
      (let ([byte (foreign-ref 'unsigned-8 ptr i)])
        (if (= byte 0)
            (list->string (reverse chars))
            (loop (+ i 1) (cons (integer->char byte) chars))))))

  ;; Build a C char** array from a list of Scheme strings.
  ;; The array is null-terminated. Caller must free with free-c-argv.
  (define (strings->c-argv strs)
    (let* ([n (length strs)]
           [argv (foreign-alloc (* (+ n 1) 8))])  ;; 8 bytes per pointer (x86_64)
      (let loop ([i 0] [ss strs])
        (if (null? ss)
            (begin (foreign-set! 'uptr argv (* i 8) 0) argv)
            (let* ([s (car ss)]
                   [bv (string->utf8 s)]
                   [len (bytevector-length bv)]
                   [buf (foreign-alloc (+ len 1))])
              (do ([j 0 (+ j 1)])
                  ((= j len))
                (foreign-set! 'unsigned-8 buf j (bytevector-u8-ref bv j)))
              (foreign-set! 'unsigned-8 buf len 0)
              (foreign-set! 'uptr argv (* i 8) buf)
              (loop (+ i 1) (cdr ss)))))))

  ;; Free a char** array built by strings->c-argv.
  (define (free-c-argv argv n)
    (let loop ([i 0])
      (when (< i n)
        (let ([ptr (foreign-ref 'uptr argv (* i 8))])
          (when (not (= ptr 0)) (foreign-free ptr)))
        (loop (+ i 1))))
    (foreign-free argv))

  ;; Extract a null-terminated C string from a bytevector at a given offset.
  (define (bv-cstring bv offset)
    (let loop ([i 0])
      (if (or (>= (+ offset i) (bytevector-length bv))
              (= (bytevector-u8-ref bv (+ offset i)) 0))
          (let ([result (make-bytevector i)])
            (bytevector-copy! bv offset result 0 i)
            (utf8->string result))
          (loop (+ i 1)))))

  ;; ======================================================================
  ;; Internal FFI bindings -- process syscalls (c- prefix)
  ;; ======================================================================

  (define c-fork (foreign-procedure "fork" () int))
  (define c-_exit (foreign-procedure "_exit" (int) void))
  (define c-execvp (foreign-procedure "execvp" (string void*) int))
  (define c-execve (foreign-procedure "execve" (string void* void*) int))
  (define c-sleep (foreign-procedure "sleep" (unsigned-int) unsigned-int))
  (define c-waitpid (foreign-procedure "waitpid" (int void* int) int))
  (define c-pipe (foreign-procedure "pipe" (void*) int))
  (define c-dup (foreign-procedure "dup" (int) int))
  (define c-dup2 (foreign-procedure "dup2" (int int) int))
  (define c-close (foreign-procedure "close" (int) int))
  (define c-open (foreign-procedure "open" (string int int) int))
  (define c-read (foreign-procedure "read" (int void* size_t) ssize_t))
  (define c-write (foreign-procedure "write" (int void* size_t) ssize_t))
  (define c-kill (foreign-procedure "kill" (int int) int))
  (define c-pause (foreign-procedure "pause" () int))

  ;; posix_spawn FFI
  (define c-posix-spawnp
    (foreign-procedure "posix_spawnp" (uptr string uptr uptr uptr uptr) int))
  (define c-spawn-fa-init
    (foreign-procedure "posix_spawn_file_actions_init" (uptr) int))
  (define c-spawn-fa-destroy
    (foreign-procedure "posix_spawn_file_actions_destroy" (uptr) int))
  (define c-spawn-fa-adddup2
    (foreign-procedure "posix_spawn_file_actions_adddup2" (uptr int int) int))
  (define c-spawn-fa-addclose
    (foreign-procedure "posix_spawn_file_actions_addclose" (uptr int) int))
  (define c-spawn-fa-addopen
    (foreign-procedure "posix_spawn_file_actions_addopen" (uptr int string int unsigned-32) int))

  ;; ======================================================================
  ;; Public wrappers -- process syscalls
  ;; ======================================================================

  ;; Fork a new process. Returns 0 in child, child pid in parent.
  (define (posix-fork) (posix-call fork (c-fork)))

  ;; Terminate the current process immediately (no stdio flush).
  ;; Never returns; no error check needed.
  (define (posix-_exit status) (c-_exit status))

  ;; Close a file descriptor.
  (define (posix-close fd) (posix-call close (c-close fd)))

  ;; Duplicate a file descriptor.
  (define (posix-dup fd) (posix-call dup (c-dup fd)))

  ;; Duplicate a file descriptor to a specific number.
  (define (posix-dup2 oldfd newfd) (posix-call dup2 (c-dup2 oldfd newfd)))

  ;; Open a file. Returns a file descriptor.
  (define (posix-open path flags mode) (posix-call open (c-open path flags mode)))

  ;; Send a signal to a process.
  (define (posix-kill pid sig) (posix-call kill (c-kill pid sig)))

  ;; Suspend until a signal is delivered. Always returns -1/EINTR.
  (define (posix-pause) (c-pause))

  ;; Wait for a child process.
  ;; Returns (values pid status) where status is the raw wait status integer.
  (define (posix-waitpid pid options)
    (with-foreign-buffer ([buf 4])
      (let ([wpid (posix-call waitpid (c-waitpid pid buf options))])
        (let ([status (foreign-ref 'int buf 0)])
          (values wpid status)))))

  ;; Create a pipe. Returns (cons read-fd write-fd).
  (define (posix-pipe)
    (with-foreign-buffer ([buf 8])
      (posix-call pipe (c-pipe buf))
      (let ([rfd (foreign-ref 'int buf 0)]
            [wfd (foreign-ref 'int buf 4)])
        (cons rfd wfd))))

  ;; Read from a file descriptor. Returns a bytevector of bytes actually read.
  (define (posix-read fd count)
    (with-foreign-buffer ([buf count])
      (let ([n (posix-call read (c-read fd buf count))])
        (let ([bv (make-bytevector n)])
          (do ([i 0 (+ i 1)])
              ((= i n))
            (bytevector-u8-set! bv i (foreign-ref 'unsigned-8 buf i)))
          bv))))

  ;; Write to a file descriptor. Accepts a bytevector.
  ;; Returns count of bytes actually written.
  (define (posix-write fd bv)
    (let* ([len (bytevector-length bv)])
      (with-foreign-buffer ([buf len])
        (do ([i 0 (+ i 1)])
            ((= i len))
          (foreign-set! 'unsigned-8 buf i (bytevector-u8-ref bv i)))
        (posix-call write (c-write fd buf len)))))

  ;; Replace the current process with a new program.
  ;; argv-list must include the program name as first element.
  ;; Only returns on failure (raises &posix-error).
  (define (posix-exec program argv-list)
    (let ([argv (strings->c-argv argv-list)]
          [n (length argv-list)])
      (c-execvp program argv)
      ;; If we get here, exec failed
      (let ([err (foreign-ref 'int (__errno_location) 0)])
        (free-c-argv argv n)
        (raise-posix-error 'execvp err))))

  ;; Replace the current process with a new program, using explicit environment.
  ;; argv-list must include the program name as first element.
  ;; env-list is a list of "KEY=VALUE" strings.
  ;; Only returns on failure (raises &posix-error).
  (define (posix-execve program argv-list env-list)
    (let ([argv (strings->c-argv argv-list)]
          [envp (strings->c-argv env-list)]
          [nargv (length argv-list)]
          [nenv (length env-list)])
      (c-execve program argv envp)
      ;; If we get here, exec failed
      (let ([err (foreign-ref 'int (__errno_location) 0)])
        (free-c-argv argv nargv)
        (free-c-argv envp nenv)
        (raise-posix-error 'execve err))))

  ;; Sleep for the given number of seconds.
  ;; Returns the number of unslept seconds (0 on normal completion).
  (define (posix-sleep secs) (c-sleep secs))

  ;; ======================================================================
  ;; posix_spawn fast path
  ;; ======================================================================

  ;; Size of posix_spawn_file_actions_t — 80 bytes covers Linux x86_64 (actual 76).
  ;; May need adjustment for other architectures.
  (define FILEACT-SIZE 80)

  ;; posix-spawnp: spawn a process without fork().
  ;; program: string, argv: list of strings.
  ;; Optional: file-actions (list of actions or #f), env-list (list of "K=V" strings or #f).
  ;; File actions: (dup2 oldfd newfd), (close fd), (open fd path flags mode).
  ;; Returns child pid.
  (define posix-spawnp
    (case-lambda
      [(program argv) (posix-spawnp* program argv #f #f)]
      [(program argv actions) (posix-spawnp* program argv actions #f)]
      [(program argv actions env-list) (posix-spawnp* program argv actions env-list)]))

  (define (posix-spawnp* program argv actions env-list)
    (let ([pid-buf (foreign-alloc 4)]
          [c-argv (strings->c-argv argv)]
          [nargv (length argv)]
          [c-envp (if env-list (strings->c-argv env-list) 0)]
          [nenv (if env-list (length env-list) 0)])
      (let ([fa (if actions (foreign-alloc FILEACT-SIZE) 0)])
        (when actions
          (c-spawn-fa-init fa)
          (for-each (lambda (act)
                      (case (car act)
                        [(dup2)  (c-spawn-fa-adddup2 fa (cadr act) (caddr act))]
                        [(close) (c-spawn-fa-addclose fa (cadr act))]
                        [(open)  (c-spawn-fa-addopen fa (cadr act) (caddr act)
                                   (cadddr act) (car (cddddr act)))]))
                    actions))
        (let ([rc (c-posix-spawnp pid-buf program fa 0 c-argv c-envp)])
          (when actions
            (c-spawn-fa-destroy fa)
            (foreign-free fa))
          (free-c-argv c-argv nargv)
          (when env-list (free-c-argv c-envp nenv))
          (if (zero? rc)
              (let ([pid (foreign-ref 'int pid-buf 0)])
                (foreign-free pid-buf)
                pid)
              (begin
                (foreign-free pid-buf)
                (raise-posix-error 'posix-spawnp rc)))))))

  ;; posix-spawnp/pipe: spawn with stdout piped back to parent.
  ;; Optional rest args: extra-actions, env-list.
  ;; Returns (values pid read-fd).
  (define (posix-spawnp/pipe program argv . rest)
    (let* ([extra (if (pair? rest) (car rest) '())]
           [env-list (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) #f)]
           [pfd (posix-pipe)]
           [rfd (car pfd)]
           [wfd (cdr pfd)]
           [pipe-actions (list (list 'dup2 wfd 1) (list 'close rfd) (list 'close wfd))]
           [all-actions (append pipe-actions extra)])
      (guard (e [#t (posix-close rfd) (posix-close wfd) (raise e)])
        (let ([pid (posix-spawnp program argv all-actions env-list)])
          (posix-close wfd)
          (values pid rfd)))))

  ) ; end library
