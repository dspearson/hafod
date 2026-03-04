;;; (hafod fileinfo) -- File information, predicates, operations, and directories
;;; Wraps (hafod posix) stat/access/file-ops with scsh-compatible API.
;;; Ported from scsh/scheme/fileinfo.scm, scsh/scheme/filesys.scm,
;;; scsh/scheme/directory.scm

(library (hafod fileinfo)
  (export
    ;; File info (re-export stat-info and accessors for convenience)
    file-info
    file-info:type file-info:device file-info:inode file-info:mode
    file-info:nlinks file-info:uid file-info:gid file-info:size
    file-info:atime file-info:mtime file-info:ctime

    ;; Type predicates
    file-directory? file-regular? file-symlink?
    file-fifo? file-socket? file-special?

    ;; Permission predicates (path-based)
    file-readable? file-writable? file-executable? file-exists?

    ;; Negated path predicates
    file-not-readable? file-not-writable? file-not-executable? file-not-exists?

    ;; File-info type predicates (on stat-info records)
    file-info-directory? file-info-regular? file-info-symlink?
    file-info-fifo? file-info-socket? file-info-special?

    ;; File-info permission predicates (on stat-info records)
    file-info-readable? file-info-writable? file-info-executable?
    file-info-not-readable? file-info-not-writable? file-info-not-executable?

    ;; file:* shorthand path-based accessors
    file:type file:group file:owner file:inode file:size file:mode
    file:nlinks file:last-access file:last-mod file:last-status-change

    ;; File mode comparison
    file-mode=?

    ;; Deprecated aliases
    file-attributes file-writeable? file-not-writeable?

    ;; FIFO and sync
    create-fifo sync-file sync-file-system

    ;; File operations
    create-directory delete-directory delete-file delete-filesys-object
    create-symlink create-hard-link read-symlink rename-file
    set-file-mode set-file-owner set-file-group set-file-times truncate-file

    ;; Directory operations
    directory-files
    open-directory-stream read-directory-stream close-directory-stream
    directory-stream?

    ;; Re-export stat-info record type (for advanced use)
    stat-info? stat-info-type stat-info-dev stat-info-ino stat-info-mode
    stat-info-nlink stat-info-uid stat-info-gid stat-info-size
    stat-info-atime stat-info-mtime stat-info-ctime)

  (import (except (hafod internal base)
                   file-directory? file-regular? file-symbolic-link?
                   file-exists?)
          (hafod posix) (hafod compat) (hafod fd-ports))

  ;; ======================================================================
  ;; File info
  ;; ======================================================================

  ;; file-info: get file status. chase? defaults to #t (follow symlinks).
  ;; With chase?=#f, returns lstat result (symlink itself, not target).
  (define (file-info fname . maybe-chase?)
    (let ([chase? (:optional maybe-chase? #t)])
      (if chase?
          (posix-stat fname)
          (posix-lstat fname))))

  ;; scsh-compatible accessor names (aliased to stat-info accessors)
  (define-scsh-accessors
    (file-info:type     stat-info-type)
    (file-info:device   stat-info-dev)
    (file-info:inode    stat-info-ino)
    (file-info:mode     stat-info-mode)
    (file-info:nlinks   stat-info-nlink)
    (file-info:uid      stat-info-uid)
    (file-info:gid      stat-info-gid)
    (file-info:size     stat-info-size)
    (file-info:atime    stat-info-atime)
    (file-info:mtime    stat-info-mtime)
    (file-info:ctime    stat-info-ctime))

  ;; ======================================================================
  ;; File type predicates
  ;; ======================================================================

  (define (file-directory? fname . maybe-chase?)
    (eq? 'directory (stat-info-type (apply file-info fname maybe-chase?))))

  (define (file-regular? fname . maybe-chase?)
    (eq? 'regular (stat-info-type (apply file-info fname maybe-chase?))))

  (define (file-symlink? fname)
    ;; Always use lstat (chase?=#f) -- a symlink is only visible without chasing
    (eq? 'symlink (stat-info-type (file-info fname #f))))

  (define (file-fifo? fname . maybe-chase?)
    (eq? 'fifo (stat-info-type (apply file-info fname maybe-chase?))))

  (define (file-socket? fname . maybe-chase?)
    (eq? 'socket (stat-info-type (apply file-info fname maybe-chase?))))

  (define (file-special? fname . maybe-chase?)
    (let ([t (stat-info-type (apply file-info fname maybe-chase?))])
      (or (eq? t 'block-device) (eq? t 'char-device))))

  ;; ======================================================================
  ;; File permission predicates
  ;; ======================================================================

  ;; These use posix-access which checks real uid/gid permissions.
  ;; posix-access returns 0 on success, -1 on failure (no exception raised).
  (define (file-readable? fname)
    (zero? (posix-access fname R_OK)))

  (define (file-writable? fname)
    (zero? (posix-access fname W_OK)))

  (define (file-executable? fname)
    (zero? (posix-access fname X_OK)))

  (define (file-exists? fname)
    (zero? (posix-access fname F_OK)))

  ;; ======================================================================
  ;; File system operations
  ;; ======================================================================

  (define (create-directory dir . maybe-mode)
    (let ([mode (:optional maybe-mode #o777)])
      (posix-mkdir dir mode)))

  (define (delete-directory dir)
    (posix-rmdir dir))

  (define (delete-file fname)
    (posix-unlink fname))

  ;; delete-filesys-object: remove file, symlink, or directory.
  ;; Tries unlink first; if EISDIR or EPERM, tries rmdir.
  (define (delete-filesys-object fname)
    (guard (e [(posix-error? e)
               ;; EISDIR=21, EPERM=1 on Linux
               (let ([errno (posix-errno e)])
                 (if (or (= errno 21) (= errno 1))
                     (posix-rmdir fname)
                     (raise e)))])
      (posix-unlink fname)))

  (define (create-symlink oldname newname)
    (posix-symlink oldname newname))

  (define (create-hard-link oldname newname)
    (posix-link oldname newname))

  (define (read-symlink path)
    (posix-readlink path))

  (define (rename-file oldname newname)
    (posix-rename oldname newname))

  (define (set-file-mode path mode)
    (posix-chmod path mode))

  (define (set-file-owner path uid)
    (posix-chown path uid -1))

  (define (set-file-group path gid)
    (posix-chown path -1 gid))

  (define (set-file-times path atime mtime)
    (posix-utimes path atime mtime))

  (define (truncate-file path/fd len)
    (if (integer? path/fd)
        (posix-ftruncate path/fd len)
        (posix-truncate path/fd len)))

  ;; ======================================================================
  ;; Directory stream operations
  ;; ======================================================================

  (define-record-type directory-stream
    (fields (mutable dirp)))

  (define (open-directory-stream path)
    (make-directory-stream (posix-opendir path)))

  (define (read-directory-stream ds)
    (posix-readdir (directory-stream-dirp ds)))

  (define (close-directory-stream ds)
    (posix-closedir (directory-stream-dirp ds))
    (directory-stream-dirp-set! ds 0))

  ;; ======================================================================
  ;; directory-files
  ;; ======================================================================

  ;; List directory contents. Filters out "." and "..".
  ;; Returns a sorted list of filenames.
  ;; Optional dotfiles? parameter (default #f) controls whether dot-files are included.
  (define (directory-files . args)
    (let-optionals* args ([dir "."] [dotfiles? #f])
      (check-arg string? dir directory-files)
      (let ([dirp (posix-opendir dir)])
        (let loop ([acc '()])
          (let ([name (posix-readdir dirp)])
            (if name
                (loop (if (or (string=? name ".") (string=? name ".."))
                          acc
                          (cons name acc)))
                (begin
                  (posix-closedir dirp)
                  (let ([filtered (if dotfiles?
                                      acc
                                      (filter (lambda (f)
                                                (not (char=? (string-ref f 0) #\.)))
                                              acc))])
                    (sort string<? filtered)))))))))

  ;; ======================================================================
  ;; file-info-* type predicates (operate on stat-info records, not paths)
  ;; ======================================================================

  (define (file-info-directory? info)
    (eq? 'directory (stat-info-type info)))

  (define (file-info-regular? info)
    (eq? 'regular (stat-info-type info)))

  (define (file-info-symlink? info)
    (eq? 'symlink (stat-info-type info)))

  (define (file-info-fifo? info)
    (eq? 'fifo (stat-info-type info)))

  (define (file-info-socket? info)
    (eq? 'socket (stat-info-type info)))

  (define (file-info-special? info)
    (let ([t (stat-info-type info)])
      (or (eq? t 'block-device) (eq? t 'char-device))))

  ;; ======================================================================
  ;; file-info permission predicates (on stat-info records)
  ;; Check mode bits against current uid/gid.
  ;; ======================================================================

  (define (file-info-readable? info)
    (let ([mode (stat-info-mode info)]
          [uid  (posix-geteuid)]
          [gid  (posix-getegid)]
          [fuid (stat-info-uid info)]
          [fgid (stat-info-gid info)])
      (cond
        [(= uid 0) #t]  ;; root can read anything
        [(= uid fuid) (not (zero? (bitwise-and mode S_IRUSR)))]
        [(= gid fgid) (not (zero? (bitwise-and mode S_IRGRP)))]
        [else (not (zero? (bitwise-and mode S_IROTH)))])))

  (define (file-info-writable? info)
    (let ([mode (stat-info-mode info)]
          [uid  (posix-geteuid)]
          [gid  (posix-getegid)]
          [fuid (stat-info-uid info)]
          [fgid (stat-info-gid info)])
      (cond
        [(= uid 0) #t]  ;; root can write anything
        [(= uid fuid) (not (zero? (bitwise-and mode S_IWUSR)))]
        [(= gid fgid) (not (zero? (bitwise-and mode S_IWGRP)))]
        [else (not (zero? (bitwise-and mode S_IWOTH)))])))

  (define (file-info-executable? info)
    (let ([mode (stat-info-mode info)]
          [uid  (posix-geteuid)]
          [gid  (posix-getegid)]
          [fuid (stat-info-uid info)]
          [fgid (stat-info-gid info)])
      (cond
        [(= uid 0) (not (zero? (bitwise-and mode (bitwise-ior S_IXUSR S_IXGRP S_IXOTH))))]
        [(= uid fuid) (not (zero? (bitwise-and mode S_IXUSR)))]
        [(= gid fgid) (not (zero? (bitwise-and mode S_IXGRP)))]
        [else (not (zero? (bitwise-and mode S_IXOTH)))])))

  ;; Negated file-info permission predicates
  (define (file-info-not-readable? info) (not (file-info-readable? info)))
  (define (file-info-not-writable? info) (not (file-info-writable? info)))
  (define (file-info-not-executable? info) (not (file-info-executable? info)))

  ;; ======================================================================
  ;; Negated path-based predicates
  ;; ======================================================================

  (define (file-not-readable? f) (not (file-readable? f)))
  (define (file-not-writable? f) (not (file-writable? f)))
  (define (file-not-executable? f) (not (file-executable? f)))
  (define (file-not-exists? f) (not (file-exists? f)))

  ;; ======================================================================
  ;; file:* shorthand path-based accessors
  ;; Each calls file-info then extracts the corresponding field.
  ;; ======================================================================

  (define (file:type f . chase?)
    (file-info:type (apply file-info f chase?)))

  (define (file:group f . chase?)
    (file-info:gid (apply file-info f chase?)))

  (define (file:owner f . chase?)
    (file-info:uid (apply file-info f chase?)))

  (define (file:inode f . chase?)
    (file-info:inode (apply file-info f chase?)))

  (define (file:size f . chase?)
    (file-info:size (apply file-info f chase?)))

  (define (file:mode f . chase?)
    (file-info:mode (apply file-info f chase?)))

  (define (file:nlinks f . chase?)
    (file-info:nlinks (apply file-info f chase?)))

  (define (file:last-access f . chase?)
    (file-info:atime (apply file-info f chase?)))

  (define (file:last-mod f . chase?)
    (file-info:mtime (apply file-info f chase?)))

  (define (file:last-status-change f . chase?)
    (file-info:ctime (apply file-info f chase?)))

  ;; ======================================================================
  ;; File mode comparison
  ;; ======================================================================

  ;; file-mode=? : integer integer -> boolean
  ;; In scsh, file-mode is an opaque type. In hafod, modes are integers
  ;; from stat, so = suffices.
  (define (file-mode=? mode1 mode2)
    (= mode1 mode2))

  ;; ======================================================================
  ;; Deprecated aliases
  ;; ======================================================================

  (define file-attributes file-info)
  (define file-writeable? file-writable?)
  (define file-not-writeable? file-not-writable?)

  ;; ======================================================================
  ;; create-fifo, sync-file, sync-file-system
  ;; ======================================================================

  (define (create-fifo path . maybe-mode)
    (posix-mkfifo path (:optional maybe-mode #o666)))

  ;; sync-file: flush file data to disk.
  ;; Accepts an integer fd or a port (uses sleazy-call/fdes to extract fd).
  (define (sync-file fd/port)
    (if (integer? fd/port)
        (posix-fsync fd/port)
        (begin
          (when (output-port? fd/port)
            (flush-output-port fd/port))
          (sleazy-call/fdes fd/port
            (lambda (fd) (posix-fsync fd))))))

  (define (sync-file-system)
    (posix-sync))

) ;; end library
