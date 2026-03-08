;;; (hafod internal posix-misc) -- Directory iteration, fcntl, groups, environ, mkstemp,
;;; utimes, fnmatch, mkfifo/fsync/sync, uname
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal posix-misc)
  (export
    posix-opendir posix-readdir posix-closedir
    posix-fcntl
    posix-getgroups
    read-environ
    posix-mkstemp
    posix-utimes
    posix-fnmatch FNM_PERIOD FNM_PATHNAME FNM_NOESCAPE
    posix-glob-fast
    posix-mkfifo posix-fsync posix-sync
    posix-uname)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants)
          (hafod internal platform-constants) (hafod internal posix-core))

  ;; ======================================================================
  ;; Directory iteration
  ;; ======================================================================

  (define c-opendir (foreign-procedure "opendir" (string) void*))
  (define c-readdir (foreign-procedure "readdir" (void*) void*))
  (define c-closedir (foreign-procedure "closedir" (void*) int))

  ;; opendir: open a directory stream. Returns a DIR* pointer.
  (define (posix-opendir path)
    (let ([dirp (c-opendir path)])
      (when (= dirp 0)
        (let ([err (foreign-ref 'int (__errno_location) 0)])
          (raise-posix-error 'opendir err)))
      dirp))

  ;; readdir: read next directory entry. Returns name as string, or #f at end.
  (define (posix-readdir dirp)
    ;; Reset errno to distinguish end-of-dir (NULL + errno=0) from error
    (foreign-set! 'int (__errno_location) 0 0)
    (let ([ent (c-readdir dirp)])
      (cond
        [(not (= ent 0))
         (ptr->string (+ ent DIRENT-D-NAME))]
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

  ;; Non-variadic wrappers for fcntl (variadic in C).
  (define c-fcntl-void (foreign-procedure "hafod_fcntl_void" (int int) int))
  (define c-fcntl-int (foreign-procedure "hafod_fcntl_int" (int int int) int))

  ;; posix-fcntl: file descriptor control.
  ;; (posix-fcntl fd cmd) or (posix-fcntl fd cmd arg)
  (define (posix-fcntl fd cmd . args)
    (if (null? args)
        (posix-call fcntl (c-fcntl-void fd cmd))
        (posix-call fcntl (c-fcntl-int fd cmd (car args)))))

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
          (with-foreign-buffer ([buf (* n 4)])
            (posix-call getgroups (c-getgroups n buf))
            (let loop ([i 0] [gids '()])
              (if (= i n)
                  (reverse gids)
                  (loop (+ i 1)
                        (cons (foreign-ref 'unsigned-32 buf (* i 4)) gids))))))))

  ;; ======================================================================
  ;; Environment reading (full environ iteration)
  ;; ======================================================================

  ;; read-environ: Read the C `environ` global variable (char**) and return
  ;; an alist of (name . value) pairs.
  (define (read-environ)
    (let ([envp (foreign-ref 'uptr (foreign-entry "environ") 0)])
      (let loop ([i 0] [alist '()])
        (let ([ptr (foreign-ref 'uptr envp (* i 8))])
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

  ;; struct timeval { long tv_sec; long tv_usec; } -- 16 bytes each, 32 bytes total
  (define c-utimes (foreign-procedure "utimes" (string void*) int))

  ;; posix-utimes: set atime and mtime on a file.
  ;; atime and mtime are epoch seconds (integers).
  ;; If both are #f, sets to current time (like touch).
  (define (posix-utimes path atime mtime)
    (if (and (not atime) (not mtime))
        ;; NULL pointer = set to current time
        (posix-call utimes (c-utimes path 0))
        ;; Allocate two struct timevals (32 bytes total)
        (with-foreign-buffer ([buf 32])
          ;; atime: tv_sec at offset 0, tv_usec at offset 8
          (foreign-set! 'long buf 0 (or atime 0))
          (foreign-set! 'long buf 8 0)
          ;; mtime: tv_sec at offset 16, tv_usec at offset 24
          (foreign-set! 'long buf 16 (or mtime 0))
          (foreign-set! 'long buf 24 0)
          (posix-call utimes (c-utimes path buf)))))

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

  ;; glob_t — struct size and offsets from platform-constants

  ;; posix-glob-fast: call C glob(3) directly.
  ;; Returns a list of matching path strings, or '() on no match.
  (define (posix-glob-fast pattern)
    (let ([buf (foreign-alloc SIZEOF-GLOB-T)])
      ;; Zero out buffer
      (do ([i 0 (+ i 1)]) ((= i SIZEOF-GLOB-T))
        (foreign-set! 'unsigned-8 buf i 0))
      (let ([rc (c-glob pattern 0 0 buf)])
        (if (zero? rc)
            (let* ([pathc (foreign-ref 'uptr buf GLOB-GL-PATHC)]
                   [pathv (foreign-ref 'uptr buf GLOB-GL-PATHV)]
                   [results (let loop ([i 0] [acc '()])
                              (if (= i pathc) (reverse acc)
                                  (let ([ptr (foreign-ref 'uptr pathv (* i 8))])
                                    (loop (+ i 1) (cons (ptr->string ptr) acc)))))])
              (c-globfree buf)
              (foreign-free buf)
              results)
            (begin
              (c-globfree buf)
              (foreign-free buf)
              '())))))

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

  ;; struct utsname: 5 fields of SYS_NMLN bytes each (65 on Linux, 256 on macOS).
  ;; Linux also has domainname (6th field), but we only need the first 5.
  (define UTSNAME_FIELD_LEN PLAT-UTSNAME-FIELD-LEN)
  (define UTSNAME_SIZE (* 6 UTSNAME_FIELD_LEN))

  (define c-uname (foreign-procedure "uname" (u8*) int))

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
