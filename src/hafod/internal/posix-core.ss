;;; (hafod internal posix-core) -- Wait-status macros, helpers, and process syscalls
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
    posix-spawnp posix-spawnp/pipe

    ;; Settable post-acquire failure hook + release observer (resource-lifetime
    ;; proof only)
    spawn-fault spawn-release-observer)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants)
          (hafod internal platform-constants))

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

  ;; Extract a null-terminated C string from a pointer address, decoding the
  ;; bytes as UTF-8.  Names read back from the kernel (directory entries, user
  ;; and group names, tty and timezone names) are byte strings; the matching
  ;; outbound path (strings->c-argv) encodes with string->utf8, so the inbound
  ;; path has to decode the same way or every non-ASCII name double-encodes --
  ;; mapping each byte straight to a character (Latin-1) turned a globbed
  ;; "name：720" into bytes no exec could match. Invalid UTF-8 is replaced, not
  ;; raised, so a stray non-UTF-8 name never aborts a directory read.
  (define (ptr->string ptr)
    ;; One strlen for the length, one memcpy for the payload, then the same
    ;; UTF-8 decode. The bytevector is sized to the strlen result first, so the
    ;; copy reads exactly the terminated string's bytes and no further.
    (let* ([len (c-strlen ptr)]
           [bv  (make-bytevector len)])
      (c-memcpy-in bv ptr len)
      (utf8->string bv)))

  ;; Build a C char** array from a list of Scheme strings.
  ;; The array is null-terminated. Caller must free with free-c-argv.
  (define (strings->c-argv strs)
    (let* ([n (length strs)]
           [argv (foreign-alloc (* (+ n 1) (foreign-sizeof 'void*)))])
      (let loop ([i 0] [ss strs])
        (if (null? ss)
            (begin (foreign-set! 'uptr argv (* i (foreign-sizeof 'void*)) 0) argv)
            (let* ([s (car ss)]
                   [bv (string->utf8 s)]
                   [len (bytevector-length bv)]
                   [buf (foreign-alloc (+ len 1))])
              (do ([j 0 (+ j 1)])
                  ((= j len))
                (foreign-set! 'unsigned-8 buf j (bytevector-u8-ref bv j)))
              (foreign-set! 'unsigned-8 buf len 0)
              (foreign-set! 'uptr argv (* i (foreign-sizeof 'void*)) buf)
              (loop (+ i 1) (cdr ss)))))))

  ;; Free a char** array built by strings->c-argv.
  (define (free-c-argv argv n)
    (let loop ([i 0])
      (when (< i n)
        (let ([ptr (foreign-ref 'uptr argv (* i (foreign-sizeof 'void*)))])
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
  (define c-open
    ;; n = 2: path + flags are fixed; mode is the single variadic arg.
    (foreign-procedure (__varargs_after 2) "open" (string int int) int))
  (define c-read (foreign-procedure "read" (int void* size_t) ssize_t))
  (define c-write (foreign-procedure "write" (int void* size_t) ssize_t))
  (define c-kill (foreign-procedure "kill" (int int) int))
  (define c-pause (foreign-procedure "pause" () int))

  ;; Bulk byte-marshalling primitives. memcpy replaces the per-byte
  ;; foreign-ref/foreign-set! loops in ptr->string, posix-read and posix-write
  ;; with a single vectorised copy; strlen scans a null-terminated C string's
  ;; length in one call. These stay PRIVATE -- deliberately absent from the
  ;; export list -- so the (hafod) umbrella count is unchanged. The bytevector
  ;; endpoint is passed as u8* (its data pointer, pinned for the call); the
  ;; foreign-alloc buffer is passed as uptr, exactly as c-read/c-write already
  ;; declare their void* buffer. Each copy length is bounded to the negotiated
  ;; count (strlen result / syscall n / bytevector-length) and the destination is
  ;; sized to that count first, so no copy can run past either endpoint.
  (define c-memcpy-in  (foreign-procedure "memcpy" (u8* uptr size_t) void)) ; dest bytevector <- src address
  (define c-memcpy-out (foreign-procedure "memcpy" (uptr u8* size_t) void)) ; dest address <- src bytevector
  (define c-strlen     (foreign-procedure "strlen" (uptr) size_t))

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
        ;; Size the result to the syscall's actual return n (a short read yields
        ;; fewer bytes than count), then bulk-copy exactly n bytes out of the
        ;; foreign buffer. n = 0 makes memcpy a defined no-op.
        (let ([bv (make-bytevector n)])
          (c-memcpy-in bv buf n)
          bv))))

  ;; Write to a file descriptor. Accepts a bytevector.
  ;; Returns count of bytes actually written.
  (define (posix-write fd bv)
    (let* ([len (bytevector-length bv)])
      (with-foreign-buffer ([buf len])
        ;; Bulk-copy the whole bytevector into the foreign buffer sized to its
        ;; length, then write; the copy count equals the buffer size exactly.
        (c-memcpy-out buf bv len)
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

  (define FILEACT-SIZE SIZEOF-SPAWN-FA)

  ;; A settable post-acquire failure hook, read live by posix-spawnp* on its
  ;; failure branch. When it holds a truthy value the wrapper forces that branch:
  ;; it skips the real spawn, runs its inline release, and raises -- so a test can
  ;; drive the failure-path unwind (the inline release followed by the guard
  ;; handler's release) without engineering a real spawn failure, whose return
  ;; convention is platform-dependent. The default #f leaves the real spawn
  ;; behaviour byte-for-byte unchanged.
  (define spawn-fault (make-parameter #f))

  ;; A release observer, run once each time posix-spawnp*'s latched release
  ;; actually performs its frees. A genuine double-free is silently survivable on
  ;; some allocators and aborts on others, so a subprocess exit code alone cannot
  ;; portably distinguish one real free from two. This makes the count observable:
  ;; with the idempotency latch the release runs its frees once, so the observer
  ;; fires once; without it, twice. Default #f is a no-op, so the real spawn path
  ;; is unchanged.
  (define spawn-release-observer (make-parameter #f))

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
      (let ([fa (if actions (foreign-alloc FILEACT-SIZE) 0)]
            [fa-live? #f]
            [released? #f])
        ;; Release every foreign block exactly once, however we leave.
        ;;
        ;; A spawn failure has to free before it raises (the raise happens inside
        ;; the guard below, whose handler also frees), so without this latch the
        ;; failure path would free each block twice. That is not a benign leak in
        ;; reverse: posix_spawn_file_actions_t is an opaque pointer to a heap
        ;; block on some platforms, so a second destroy frees an already-freed
        ;; allocation and the allocator aborts the process.
        ;;
        ;; fa-live? gates the destroy on the init having actually run: foreign-alloc
        ;; hands back uninitialised memory, so destroying before init would hand the
        ;; allocator a garbage pointer.
        (define (release!)
          (unless released?
            (set! released? #t)
            ;; Observe a real release (the frees below run exactly once per pass
            ;; of this gate); default observer is #f, so this is a no-op in
            ;; ordinary use.
            (let ([obs (spawn-release-observer)]) (when obs (obs)))
            (when fa-live? (c-spawn-fa-destroy fa))
            (when actions (foreign-free fa))
            (free-c-argv c-argv nargv)
            (when env-list (free-c-argv c-envp nenv))
            (foreign-free pid-buf)))
        (guard (e [#t (release!) (raise e)])
          (when actions
            (c-spawn-fa-init fa)
            (set! fa-live? #t)
            (for-each (lambda (act)
                        (case (car act)
                          [(dup2)  (c-spawn-fa-adddup2 fa (cadr act) (caddr act))]
                          [(close) (c-spawn-fa-addclose fa (cadr act))]
                          [(open)  (c-spawn-fa-addopen fa (cadr act) (caddr act)
                                     (cadddr act) (car (cddddr act)))]))
                      actions))
          ;; spawn-fault forces the failure branch post-acquisition (skipping the
          ;; real spawn) so the inline release + guard-handler release path is
          ;; exercised on demand; #f keeps the real call and behaviour unchanged.
          (let ([rc (if (spawn-fault)
                        1
                        (c-posix-spawnp pid-buf program fa 0 c-argv c-envp))])
            (if (zero? rc)
                ;; Read the pid out before the buffer holding it is released.
                (let ([pid (foreign-ref 'int pid-buf 0)])
                  (release!)
                  pid)
                (begin
                  (release!)
                  (raise-posix-error 'posix-spawnp rc))))))))

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
