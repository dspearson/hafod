;;; (hafod internal posix-misc) -- Directory iteration, fcntl, groups, environ, mkstemp,
;;; utimes, fnmatch, mkfifo/fsync/sync, uname
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal posix-misc)
  (export
    posix-opendir posix-readdir posix-closedir
    posix-fcntl
    fd-wait-readable
    posix-getgroups
    read-environ
    posix-mkstemp
    posix-utimes
    posix-fnmatch FNM_PERIOD FNM_PATHNAME FNM_NOESCAPE
    posix-glob-fast
    posix-mkfifo posix-fsync posix-sync
    posix-uname
    ;; Settable fault/observer hooks for the resource-lifetime proof -- read
    ;; live by the wrappers below, default inert, and deliberately NOT named in
    ;; the (hafod posix) facade nor the (hafod) umbrella (leaf-only test seams).
    readdir-fault-after
    fcntl-fault
    glob-fault glob-release)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants)
          (hafod internal platform-constants) (hafod internal posix-core)
          (hafod internal platform)
          (only (hafod internal platform-ftypes) dirent-t pollfd-t))

  ;; ======================================================================
  ;; Directory iteration
  ;; ======================================================================

  (define c-opendir (foreign-procedure "opendir" (string) void*))
  ;; readdir returns a struct dirent read through dirent-t; on 64-bit Intel macOS
  ;; readdir-symbol-name (resolved in the platform hub) is the "readdir$INODE64"
  ;; variant, so the returned entry matches the 64-bit-inode dirent-t layout.
  ;; opendir and closedir carry no dirent, so they keep the bare symbol.
  (define c-readdir (foreign-procedure readdir-symbol-name (void*) void*))
  (define c-closedir (foreign-procedure "closedir" (void*) int))

  ;; opendir: open a directory stream. Returns a DIR* pointer.
  (define (posix-opendir path)
    (let ([dirp (c-opendir path)])
      (when (= dirp 0)
        (let ([err (foreign-ref 'int (__errno_location) 0)])
          (raise-posix-error 'opendir err)))
      dirp))

  ;; A settable readdir fault hook for the resource-lifetime proof. When
  ;; readdir-fault-after holds an integer N, the (N+1)th posix-readdir call
  ;; raises, so a directory scan fails mid-loop with its DIR* still open and the
  ;; consumer's dynamic-wind after-thunk (closedir) genuinely runs on the
  ;; unwind. The default #f leaves the read path below byte-identical. The
  ;; internal counter resets when it fires, so a repeated drive faults at the
  ;; same point on every scan.
  (define readdir-fault-after (make-parameter #f))
  (define readdir-fault-count 0)

  ;; readdir: read next directory entry. Returns name as string, or #f at end.
  (define (posix-readdir dirp)
    (let ([fault-n (readdir-fault-after)])
      (when (integer? fault-n)
        (set! readdir-fault-count (+ readdir-fault-count 1))
        (when (> readdir-fault-count fault-n)
          (set! readdir-fault-count 0)
          (error 'posix-readdir "forced readdir fault for the resource-lifetime proof"))))
    ;; Reset errno to distinguish end-of-dir (NULL + errno=0) from error
    (foreign-set! 'int (__errno_location) 0 0)
    (let ([ent (c-readdir dirp)])
      (cond
        [(not (= ent 0))
         ;; ent is a libc-owned pointer into the DIR stream: wrap it as a
         ;; dirent-t and take the address of d_name -- never alloc or free it.
         (ptr->string (ftype-pointer-address
                       (ftype-&ref dirent-t (d_name) (make-ftype-pointer dirent-t ent))))]
        [else
         ;; ent is NULL: check errno
         (let ([err (foreign-ref 'int (__errno_location) 0)])
           (if (= err 0)
               #f  ;; end of directory
               (raise-posix-error 'readdir err)))])))

  ;; closedir: close a directory stream.
  (define (posix-closedir dirp)
    (posix-call closedir (c-closedir dirp)))

  ;; ======================================================================
  ;; fcntl
  ;; ======================================================================

  ;; One native binding serves both fcntl arities: n = 2 (fd + cmd fixed),
  ;; the optional arg is the single variadic. The no-arg commands ignore the
  ;; harmless dummy 0 the wrapper passes for the unused vararg.
  (define c-fcntl
    (foreign-procedure (__varargs_after 2) "fcntl" (int int int) int))

  ;; A settable fcntl fault hook for the resource-lifetime proof.  When it holds a
  ;; thunk, posix-fcntl runs it first -- a truthy thunk raises -- so a caller that
  ;; sets a descriptor non-blocking right after a spawn can be driven down its
  ;; setup-raise path with the child and its pipe already live, proving the read fd
  ;; is still closed on that unwind.  Default #f leaves the call path byte-identical.
  (define fcntl-fault (make-parameter #f))

  ;; posix-fcntl: file descriptor control.
  ;; (posix-fcntl fd cmd) or (posix-fcntl fd cmd arg)
  (define (posix-fcntl fd cmd . args)
    (let ([f (fcntl-fault)]) (when f (f)))
    (if (null? args)
        (posix-call fcntl (c-fcntl fd cmd 0))
        (posix-call fcntl (c-fcntl fd cmd (car args)))))

  ;; ======================================================================
  ;; poll(2) readiness -- a deadline-bounded "is this fd readable yet?"
  ;; ======================================================================

  ;; Why poll-with-a-deadline rather than a blocking read guarded by SIGALRM:
  ;; hafod's green threads run on one OS thread, so a blocking child read cannot
  ;; yield, and a signal around it would need a handler that races the
  ;; job-control SIGCHLD path. A non-blocking descriptor plus a bounded poll is
  ;; instead a plain synchronous loop -- no handler, no reentrancy -- with a
  ;; deterministic kill/reap tail. Blocking the thread for up to the deadline is
  ;; intended: the user is waiting to type, so we bound the synchronous wait
  ;; rather than leaving it unbounded.
  ;;
  ;; int poll(struct pollfd *fds, nfds_t nfds, int timeout_ms). nfds is fixed at
  ;; 1 here (a single descriptor), passed as the unsigned-long nfds_t. POLLIN
  ;; (0x0001, universal) is the "data to read" event we wait on.
  (define c-poll (foreign-procedure "poll" (void* unsigned-long int) int))
  ;; POLLIN (0x001) is the "data to read" event; POLLHUP (0x010) is the peer-hangup
  ;; the kernel reports in revents (it is never requested in events).  Both are the
  ;; historical System V values, identical on Linux, macOS and the BSDs.  The error
  ;; revents -- POLLERR (0x008) / POLLNVAL (0x020) -- are deliberately not named:
  ;; revents carrying neither POLLIN nor POLLHUP is treated uniformly as a bail.
  (define POLLIN 1)
  (define POLLHUP 16)

  ;; fd-wait-readable: wait up to timeout-ms for FD to have bytes to read.
  ;; Returns 'ready (there is data to read, POLLIN, OR the peer hung up with no more
  ;; data, POLLHUP -- both let the caller's next read return that data or a clean
  ;; EOF), 'timeout (the deadline elapsed with no event, OR the fd carries an error
  ;; condition -- POLLERR / POLLNVAL -- with neither data nor a hangup: the caller
  ;; bails and reaps rather than re-polling the same persistent error to the
  ;; deadline, which a blind r>0 -> 'ready would busy-spin, since the caller's
  ;; guarded read collapses the raise back into a re-poll), or 'interrupted (poll
  ;; returned -1, e.g. EINTR from a resize or child-exit signal -- the caller
  ;; recomputes its remaining deadline and re-polls, never treating it as a real
  ;; timeout). The pollfd scratch is sized from `ftype-sizeof` and its fields set and
  ;; read through a pollfd-t pointer, so Chez owns every offset -- no hand-written
  ;; byte positions. A non-positive deadline is clamped to 0 so it returns at once:
  ;; poll treats a negative timeout as "wait forever", which must never wedge the
  ;; single OS thread.
  (define (fd-wait-readable fd timeout-ms)
    (with-foreign-buffer ([buf (ftype-sizeof pollfd-t)])
      (let ([pfd (make-ftype-pointer pollfd-t buf)])
        (ftype-set! pollfd-t (fd) pfd fd)
        (ftype-set! pollfd-t (events) pfd POLLIN)
        (ftype-set! pollfd-t (revents) pfd 0)
        (let ([r (c-poll buf 1 (if (< timeout-ms 0) 0 timeout-ms))])
          (cond
            [(< r 0) 'interrupted]
            [(= r 0) 'timeout]
            [else
             ;; poll reports r>0 for ANY revents bit, so inspect them rather than
             ;; mapping every r>0 to 'ready: POLLIN is data to read; POLLHUP with no
             ;; data yields a clean EOF on the next read -- both route through 'ready.
             ;; Anything else (POLLERR / POLLNVAL) is a finished or invalid fd with
             ;; nothing to read, so bail via 'timeout instead of re-polling the same
             ;; error to the deadline.
             (let ([revents (ftype-ref pollfd-t (revents) pfd)])
               (cond
                 [(not (zero? (bitwise-and revents POLLIN)))  'ready]
                 [(not (zero? (bitwise-and revents POLLHUP))) 'ready]
                 [else 'timeout]))])))))

  ;; ======================================================================
  ;; Supplementary groups
  ;; ======================================================================

  (define c-getgroups (foreign-procedure "getgroups" (int void*) int))

  ;; posix-getgroups: returns list of supplementary group IDs.
  ;; Calls getgroups(0, NULL) first to get count, then allocates and retrieves.
  (define (posix-getgroups)
    (let ([n (c-getgroups 0 0)])
      (when (= n -1)
        (let ([err (foreign-ref 'int (__errno_location) 0)])
          (raise-posix-error 'getgroups err)))
      (if (= n 0) '()
          (let ([gid-size (foreign-sizeof 'unsigned-int)])
            (with-foreign-buffer ([buf (* n gid-size)])
              (posix-call getgroups (c-getgroups n buf))
              (let loop ([i 0] [gids '()])
                (if (= i n)
                    (reverse gids)
                    (loop (+ i 1)
                          (cons (foreign-ref 'unsigned-32 buf (* i gid-size)) gids)))))))))

  ;; ======================================================================
  ;; Environment reading (full environ iteration)
  ;; ======================================================================

  ;; read-environ: Read the C `environ` global variable (char**) and return
  ;; an alist of (name . value) pairs.
  (define (read-environ)
    (let ([envp (foreign-ref 'uptr (foreign-entry "environ") 0)])
      (let loop ([i 0] [alist '()])
        (let ([ptr (foreign-ref 'uptr envp (* i (foreign-sizeof 'void*)))])
          (if (= ptr 0)
              (reverse alist)
              (let* ([str (ptr->string ptr)]
                     [len (string-length str)]
                     [eq-pos (let scan ([j 0])
                               (cond [(= j len) #f]
                                     [(char=? (string-ref str j) #\=) j]
                                     [else (scan (+ j 1))]))])
                (loop (+ i 1)
                      (if eq-pos
                          (cons (cons (substring str 0 eq-pos)
                                      (substring str (+ eq-pos 1) len))
                                alist)
                          alist))))))))

  ;; ======================================================================
  ;; Temp files (mkstemp)
  ;; ======================================================================

  ;; mkstemp(3) - create a unique temporary file.
  ;; Takes a template string ending in "XXXXXX", returns (values path fd).
  ;; The template is copied to a bytevector since mkstemp modifies it in place.
  (define c-mkstemp (foreign-procedure "mkstemp" (u8*) int))

  (define (posix-mkstemp template)
    (let* ([bv (string->utf8 (string-append template "\x0;"))]  ;; null-terminate
           [fd (c-mkstemp bv)])
      (when (< fd 0)
        (let ([err (foreign-ref 'int (__errno_location) 0)])
          (raise-posix-error 'mkstemp err)))
      ;; mkstemp modified the XXXXXX in bv in place; decode directly
      ;; (bv already has trailing null from the string-append above)
      (values (bv-cstring bv 0) fd)))

  ;; ======================================================================
  ;; utimes -- set file access and modification times
  ;; ======================================================================

  ;; struct timeval { long tv_sec; long tv_usec; }
  (define c-utimes (foreign-procedure "utimes" (string void*) int))
  (define sizeof-timeval (* 2 (foreign-sizeof 'long)))

  ;; posix-utimes: set atime and mtime on a file.
  ;; atime and mtime are epoch seconds (integers).
  ;; If both are #f, sets to current time (like touch).
  (define (posix-utimes path atime mtime)
    (if (and (not atime) (not mtime))
        ;; NULL pointer = set to current time
        (posix-call utimes (c-utimes path 0))
        ;; Allocate two struct timevals
        (let ([sl (foreign-sizeof 'long)])
          (with-foreign-buffer ([buf (* 2 sizeof-timeval)])
            ;; atime
            (foreign-set! 'long buf 0 (or atime 0))
            (foreign-set! 'long buf sl 0)
            ;; mtime
            (foreign-set! 'long buf sizeof-timeval (or mtime 0))
            (foreign-set! 'long buf (+ sizeof-timeval sl) 0)
            (posix-call utimes (c-utimes path buf))))))

  ;; ======================================================================
  ;; fnmatch -- glob pattern matching
  ;; ======================================================================

  (define c-fnmatch (foreign-procedure "fnmatch" (string string int) int))

  (define FNM_NOESCAPE PLAT-FNM-NOESCAPE)
  (define FNM_PATHNAME PLAT-FNM-PATHNAME)
  (define FNM_PERIOD PLAT-FNM-PERIOD)

  ;; posix-fnmatch: match a glob pattern against a string.
  ;; Returns 0 on match, FNM_NOMATCH (1) on no match.
  (define (posix-fnmatch pattern string flags)
    (c-fnmatch pattern string flags))

  ;; ======================================================================
  ;; glob(3) -- fast C-level glob
  ;; ======================================================================

  (define c-glob (foreign-procedure "glob" (string int void* void*) int))
  (define c-globfree (foreign-procedure "globfree" (void*) void))

  ;; Settable fault/observer hooks for the glob_t resource-lifetime proof.
  ;; glob-fault, when it holds a thunk, forces a post-acquire raise inside
  ;; posix-glob-fast's middle thunk so the dynamic-wind after-thunk runs on the
  ;; unwind. glob-release is the SOLE free path the after-thunk routes through;
  ;; its default runs the exact current release -- c-globfree then foreign-free
  ;; of the glob_t buffer -- so a test may count the free by wrapping the
  ;; default (the fd canary is blind to this heap free, so the routed observer
  ;; is the only proof that goes RED when the after-thunk is deleted). Both
  ;; default inert / behaviour-identical.
  (define glob-fault (make-parameter #f))
  (define glob-release (make-parameter (lambda (b) (c-globfree b) (foreign-free b))))

  ;; glob_t — struct size and offsets from platform-constants

  ;; posix-glob-fast: call C glob(3) directly.
  ;; Returns a list of matching path strings, or '() on no match.
  (define (posix-glob-fast pattern)
    (let ([buf (foreign-alloc SIZEOF-GLOB-T)])
      ;; Zero out buffer
      (do ([i 0 (+ i 1)]) ((= i SIZEOF-GLOB-T))
        (foreign-set! 'unsigned-8 buf i 0))
      ;; The glob call and the pathv decode run inside a dynamic-wind whose
      ;; after-thunk releases the glob_t through the glob-release seam, whose
      ;; default does BOTH c-globfree (releases the glob_t's own pathv / string
      ;; allocations) AND foreign-free (releases our glob_t buffer), in that
      ;; order. A glob_t needs the globfree unwind -- foreign-free alone would
      ;; leak glob's internal allocations -- so this is not a with-foreign-buffer
      ;; site. The seam is the SOLE free path (no bare free beside it), so a test
      ;; wrapping the default can count it: 1 means freed once, 0 means leaked.
      ;; The unwind runs on every exit, including a ptr->string raise mid-walk or
      ;; a forced glob-fault raise. Behaviour is preserved: the matched path list
      ;; on a zero rc, '() on a non-zero rc (globfree on a zeroed/handled glob_t
      ;; is safe, exactly as the two former branches both did).
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (let ([rc (c-glob pattern 0 0 buf)])
            ;; Resource-lifetime proof hook: a forced post-acquire raise here
            ;; drives the after-thunk's routed release on the unwind. Inert (#f)
            ;; by default.
            (let ([f (glob-fault)]) (when f (f)))
            (if (zero? rc)
                (let ([pathc (foreign-ref 'uptr buf GLOB-GL-PATHC)]
                      [pathv (foreign-ref 'uptr buf GLOB-GL-PATHV)])
                  (let loop ([i 0] [acc '()])
                    (if (= i pathc) (reverse acc)
                        (let ([ptr (foreign-ref 'uptr pathv (* i (foreign-sizeof 'void*)))])
                          (loop (+ i 1) (cons (ptr->string ptr) acc))))))
                '())))
        (lambda ()
          ((glob-release) buf)))))

  ;; ======================================================================
  ;; mkfifo, fsync, sync
  ;; ======================================================================

  (define c-mkfifo (foreign-procedure "mkfifo" (string int) int))
  (define c-fsync (foreign-procedure "fsync" (int) int))
  (define c-sync (foreign-procedure "sync" () void))

  (define (posix-mkfifo path mode) (posix-call mkfifo (c-mkfifo path mode)))
  (define (posix-fsync fd) (posix-call fsync (c-fsync fd)))
  (define (posix-sync) (c-sync))

  ;; ======================================================================
  ;; uname -- system identification
  ;; ======================================================================

  ;; struct utsname: 5 fields of SYS_NMLN bytes each (65 on Linux, 256 on macOS/FreeBSD).
  ;; Linux also has domainname (6th field), but we only need the first 5.
  ;;
  ;; FreeBSD quirk: libc's uname() symbol uses the kernel ABI (SYS_NMLN=32),
  ;; not the userland value (256).  The inline uname() in <sys/utsname.h>
  ;; calls __xuname(SYS_NMLN, buf) with the correct size.  We must do the same.
  (define UTSNAME_FIELD_LEN PLAT-UTSNAME-FIELD-LEN)
  (define UTSNAME_SIZE (* 6 UTSNAME_FIELD_LEN))

  (define c-uname
    (case os-family
      [(freebsd)
       ;; FreeBSD: call __xuname(field_len, buf)
       (let ([xuname (foreign-procedure "__xuname" (int u8*) int)])
         (lambda (buf) (xuname UTSNAME_FIELD_LEN buf)))]
      [else (foreign-procedure "uname" (u8*) int)]))

  ;; posix-uname: returns 5 values (sysname nodename release version machine).
  (define (posix-uname)
    (let ([buf (make-bytevector UTSNAME_SIZE 0)])
      (let ([ret (c-uname buf)])
        (when (< ret 0)
          (let ([err (foreign-ref 'int (__errno_location) 0)])
            (raise-posix-error 'uname err)))
        (values
          (bv-cstring buf 0)
          (bv-cstring buf (* 1 UTSNAME_FIELD_LEN))
          (bv-cstring buf (* 2 UTSNAME_FIELD_LEN))
          (bv-cstring buf (* 3 UTSNAME_FIELD_LEN))
          (bv-cstring buf (* 4 UTSNAME_FIELD_LEN))))))

  ) ; end library
