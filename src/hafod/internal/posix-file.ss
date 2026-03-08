;;; (hafod internal posix-file) -- Stat and filesystem operations
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal posix-file)
  (export
    posix-stat posix-lstat posix-fstat
    stat-info? stat-info-type stat-info-dev stat-info-ino stat-info-mode
    stat-info-nlink stat-info-uid stat-info-gid stat-info-rdev stat-info-size
    stat-info-blksize stat-info-blocks stat-info-atime stat-info-mtime
    stat-info-ctime mode->type
    posix-chmod posix-fchmod posix-chown posix-fchown
    posix-truncate posix-ftruncate
    posix-link posix-symlink posix-readlink posix-unlink posix-rename
    posix-mkdir posix-rmdir
    posix-access posix-lseek posix-umask)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants)
          (hafod internal platform-constants) (hafod internal posix-core))

  ;; ======================================================================
  ;; Stat -- struct stat handling
  ;; ======================================================================

  ;; stat-info record type
  (define-record-type stat-info
    (fields type dev ino mode nlink uid gid rdev size blksize blocks atime mtime ctime))

  ;; Convert file mode to a symbolic type name.
  (define (mode->type mode)
    (let ([fmt (bitwise-and mode S_IFMT)])
      (cond
        [(= fmt S_IFBLK)  'block-device]
        [(= fmt S_IFCHR)  'char-device]
        [(= fmt S_IFDIR)  'directory]
        [(= fmt S_IFIFO)  'fifo]
        [(= fmt S_IFREG)  'regular]
        [(= fmt S_IFSOCK) 'socket]
        [(= fmt S_IFLNK)  'symlink]
        [else 'unknown])))

  ;; Internal FFI
  (define c-stat (foreign-procedure "stat" (string void*) int))
  (define c-lstat (foreign-procedure "lstat" (string void*) int))
  (define c-fstat (foreign-procedure "fstat" (int void*) int))

  ;; Read an unsigned integer of the given byte-size from a foreign buffer.
  (define (foreign-ref-uint buf offset size)
    (case size
      [(1) (foreign-ref 'unsigned-8 buf offset)]
      [(2) (foreign-ref 'unsigned-16 buf offset)]
      [(4) (foreign-ref 'unsigned-32 buf offset)]
      [(8) (foreign-ref 'unsigned-64 buf offset)]
      [else (error 'foreign-ref-uint "unsupported size" size)]))

  ;; Read a signed integer of the given byte-size from a foreign buffer.
  (define (foreign-ref-sint buf offset size)
    (case size
      [(4) (foreign-ref 'integer-32 buf offset)]
      [(8) (foreign-ref 'integer-64 buf offset)]
      [else (error 'foreign-ref-sint "unsupported size" size)]))

  ;; Extract stat-info from a filled stat buffer.
  (define (extract-stat-info buf)
    (make-stat-info
      (mode->type (foreign-ref-uint buf STAT-ST-MODE SIZEOF-ST-MODE))
      (foreign-ref-uint buf STAT-ST-DEV SIZEOF-ST-DEV)
      (foreign-ref-uint buf STAT-ST-INO SIZEOF-ST-INO)
      (foreign-ref-uint buf STAT-ST-MODE SIZEOF-ST-MODE)
      (foreign-ref-uint buf STAT-ST-NLINK SIZEOF-ST-NLINK)
      (foreign-ref 'unsigned-32 buf STAT-ST-UID)
      (foreign-ref 'unsigned-32 buf STAT-ST-GID)
      (foreign-ref-uint buf STAT-ST-RDEV SIZEOF-ST-RDEV)
      (foreign-ref 'integer-64 buf STAT-ST-SIZE)
      (foreign-ref-sint buf STAT-ST-BLKSIZE SIZEOF-ST-BLKSIZE)
      (foreign-ref 'integer-64 buf STAT-ST-BLOCKS)
      (foreign-ref 'unsigned-64 buf STAT-ST-ATIM)
      (foreign-ref 'unsigned-64 buf STAT-ST-MTIM)
      (foreign-ref 'unsigned-64 buf STAT-ST-CTIM)))

  ;; stat: get file status by path (follows symlinks).
  (define (posix-stat path)
    (with-foreign-buffer ([buf SIZEOF-STAT])
      (posix-call stat (c-stat path buf))
      (extract-stat-info buf)))

  ;; lstat: get file status by path (does not follow symlinks).
  (define (posix-lstat path)
    (with-foreign-buffer ([buf SIZEOF-STAT])
      (posix-call lstat (c-lstat path buf))
      (extract-stat-info buf)))

  ;; fstat: get file status by file descriptor.
  (define (posix-fstat fd)
    (with-foreign-buffer ([buf SIZEOF-STAT])
      (posix-call fstat (c-fstat fd buf))
      (extract-stat-info buf)))

  ;; ======================================================================
  ;; Filesystem operations
  ;; ======================================================================

  (define c-chmod (foreign-procedure "chmod" (string int) int))
  (define c-fchmod (foreign-procedure "fchmod" (int int) int))
  (define c-chown (foreign-procedure "chown" (string int int) int))
  (define c-fchown (foreign-procedure "fchown" (int int int) int))
  (define c-truncate (foreign-procedure "truncate" (string long) int))
  (define c-ftruncate (foreign-procedure "ftruncate" (int long) int))
  (define c-link (foreign-procedure "link" (string string) int))
  (define c-symlink (foreign-procedure "symlink" (string string) int))
  (define c-readlink (foreign-procedure "readlink" (string void* size_t) ssize_t))
  (define c-unlink (foreign-procedure "unlink" (string) int))
  (define c-rename (foreign-procedure "rename" (string string) int))
  (define c-mkdir (foreign-procedure "mkdir" (string int) int))
  (define c-rmdir (foreign-procedure "rmdir" (string) int))
  (define c-access (foreign-procedure "access" (string int) int))
  (define c-lseek (foreign-procedure "lseek" (int long int) long))
  (define c-umask (foreign-procedure "umask" (int) int))

  (define (posix-chmod path mode) (posix-call chmod (c-chmod path mode)))
  (define (posix-fchmod fd mode) (posix-call fchmod (c-fchmod fd mode)))
  (define (posix-chown path uid gid) (posix-call chown (c-chown path uid gid)))
  (define (posix-fchown fd uid gid) (posix-call fchown (c-fchown fd uid gid)))
  (define (posix-truncate path len) (posix-call truncate (c-truncate path len)))
  (define (posix-ftruncate fd len) (posix-call ftruncate (c-ftruncate fd len)))
  (define (posix-link oldpath newpath) (posix-call link (c-link oldpath newpath)))
  (define (posix-symlink target linkpath) (posix-call symlink (c-symlink target linkpath)))
  (define (posix-unlink path) (posix-call unlink (c-unlink path)))
  (define (posix-rename oldpath newpath) (posix-call rename (c-rename oldpath newpath)))
  (define (posix-mkdir path mode) (posix-call mkdir (c-mkdir path mode)))
  (define (posix-rmdir path) (posix-call rmdir (c-rmdir path)))
  ;; access: returns 0 on success, -1 on failure -- caller checks return value
  (define (posix-access path mode) (c-access path mode))
  (define (posix-lseek fd offset whence) (posix-call lseek (c-lseek fd offset whence)))
  ;; umask always succeeds, returns old mask
  (define (posix-umask mask) (c-umask mask))

  ;; readlink: read the target of a symbolic link.
  (define (posix-readlink path)
    (with-foreign-buffer ([buf 4096])
      (let ([n (posix-call readlink (c-readlink path buf 4095))])
        (let loop ([i 0] [chars '()])
          (if (= i n)
              (list->string (reverse chars))
              (loop (+ i 1)
                    (cons (integer->char (foreign-ref 'unsigned-8 buf i))
                          chars)))))))

  ) ; end library
