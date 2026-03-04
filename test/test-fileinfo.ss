(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod fileinfo) (hafod posix) (hafod compat)
        (except (chezscheme) vector-append open-input-file open-output-file getenv
                file-directory? file-regular? file-symbolic-link?
                file-exists? delete-file delete-directory rename-file
                truncate-file))

(test-begin "File Info and Operations")

;; ---- File info ----

(test-assert "file-info returns stat-info record"
  (stat-info? (file-info "/tmp")))

(test-equal "file-info:type for /tmp is directory"
  'directory
  (file-info:type (file-info "/tmp")))

(test-assert "file-info:size is non-negative"
  (>= (file-info:size (file-info "/tmp")) 0))

(test-assert "file-info:mode is an integer"
  (integer? (file-info:mode (file-info "/tmp"))))

;; Symlink test
(let ([target "/tmp/hafod-test-fi-target"]
      [link "/tmp/hafod-test-fi-link"])
  ;; Create target file
  (receive (path fd) (posix-mkstemp (string-append target "-XXXXXX"))
    (posix-close fd)
    ;; Create symlink
    (guard (e [#t #f]) (posix-unlink link))
    (posix-symlink path link)

    (test-equal "file-info chase=default follows symlink"
      'regular
      (file-info:type (file-info link)))

    (test-equal "file-info chase=#f shows symlink"
      'symlink
      (file-info:type (file-info link #f)))

    (posix-unlink link)
    (posix-unlink path)))

;; ---- Type predicates ----

(test-assert "file-directory? /tmp is #t"
  (file-directory? "/tmp"))

(test-assert "file-directory? on regular file is #f"
  (let ([path (let-values ([(p fd) (posix-mkstemp "/tmp/hafod-fi-XXXXXX")])
                (posix-close fd) p)])
    (let ([result (not (file-directory? path))])
      (posix-unlink path)
      result)))

(test-assert "file-regular? on regular file"
  (let ([path (let-values ([(p fd) (posix-mkstemp "/tmp/hafod-fi-XXXXXX")])
                (posix-close fd) p)])
    (let ([result (file-regular? path)])
      (posix-unlink path)
      result)))

(test-assert "file-symlink? on symlink"
  (let ([target "/tmp/hafod-fi-sym-target"]
        [link "/tmp/hafod-fi-sym-link"])
    (receive (path fd) (posix-mkstemp (string-append target "-XXXXXX"))
      (posix-close fd)
      (guard (e [#t #f]) (posix-unlink link))
      (posix-symlink path link)
      (let ([result (file-symlink? link)])
        (posix-unlink link)
        (posix-unlink path)
        result))))

;; ---- Permission predicates ----

(test-assert "file-exists? on existing file"
  (file-exists? "/tmp"))

(test-assert "file-exists? on nonexistent"
  (not (file-exists? "/tmp/hafod-nonexistent-file-xyz-12345")))

(test-assert "file-readable? on readable file"
  (file-readable? "/tmp"))

(test-assert "file-writable? on writable directory"
  (file-writable? "/tmp"))

(test-assert "file-executable? on /tmp"
  (file-executable? "/tmp"))

;; ---- File operations ----

;; create-directory / delete-directory
(let ([dir "/tmp/hafod-test-mkdir"])
  (guard (e [#t #f]) (posix-rmdir dir))
  (create-directory dir)
  (test-assert "create-directory creates a directory"
    (file-directory? dir))
  (delete-directory dir)
  (test-assert "delete-directory removes it"
    (not (file-exists? dir))))

;; create-directory with mode
(let ([dir "/tmp/hafod-test-mkdir-mode"])
  (guard (e [#t #f]) (posix-rmdir dir))
  (create-directory dir #o700)
  (test-assert "create-directory with mode"
    (file-directory? dir))
  (delete-directory dir))

;; delete-file
(let ([path (let-values ([(p fd) (posix-mkstemp "/tmp/hafod-del-XXXXXX")])
              (posix-close fd) p)])
  (test-assert "file exists before delete" (file-exists? path))
  (delete-file path)
  (test-assert "delete-file removes file" (not (file-exists? path))))

;; rename-file
(let ([old (let-values ([(p fd) (posix-mkstemp "/tmp/hafod-ren-old-XXXXXX")])
             (posix-close fd) p)]
      [new "/tmp/hafod-ren-new-test"])
  (guard (e [#t #f]) (posix-unlink new))
  (rename-file old new)
  (test-assert "rename-file: old gone" (not (file-exists? old)))
  (test-assert "rename-file: new exists" (file-exists? new))
  (posix-unlink new))

;; create-symlink / read-symlink
(let ([target "/tmp/hafod-symtest-target"]
      [link "/tmp/hafod-symtest-link"])
  (receive (path fd) (posix-mkstemp (string-append target "-XXXXXX"))
    (posix-close fd)
    (guard (e [#t #f]) (posix-unlink link))
    (create-symlink path link)
    (test-assert "create-symlink creates link" (file-symlink? link))
    (test-equal "read-symlink returns target" path (read-symlink link))
    (posix-unlink link)
    (posix-unlink path)))

;; create-hard-link
(let ([target (let-values ([(p fd) (posix-mkstemp "/tmp/hafod-hardlink-XXXXXX")])
                (posix-close fd) p)]
      [link "/tmp/hafod-hardlink-link"])
  (guard (e [#t #f]) (posix-unlink link))
  (create-hard-link target link)
  (test-assert "create-hard-link creates link" (file-exists? link))
  (test-assert "hard link has same inode"
    (= (file-info:inode (file-info target))
       (file-info:inode (file-info link))))
  (posix-unlink link)
  (posix-unlink target))

;; set-file-mode
(let ([path (let-values ([(p fd) (posix-mkstemp "/tmp/hafod-chmod-XXXXXX")])
              (posix-close fd) p)])
  (set-file-mode path #o644)
  (test-equal "set-file-mode changes mode"
    #o644
    (bitwise-and #o777 (file-info:mode (file-info path))))
  (posix-unlink path))

;; set-file-times
(let ([path (let-values ([(p fd) (posix-mkstemp "/tmp/hafod-utime-XXXXXX")])
              (posix-close fd) p)])
  (set-file-times path 1000000 2000000)
  (let ([info (file-info path)])
    (test-equal "set-file-times changes atime" 1000000 (file-info:atime info))
    (test-equal "set-file-times changes mtime" 2000000 (file-info:mtime info)))
  (posix-unlink path))

;; truncate-file
(let ([path (let-values ([(p fd) (posix-mkstemp "/tmp/hafod-trunc-XXXXXX")])
              ;; Write some data
              (posix-write fd (string->utf8 "hello world"))
              (posix-close fd) p)])
  (truncate-file path 5)
  (test-equal "truncate-file reduces size" 5 (file-info:size (file-info path)))
  (posix-unlink path))

;; delete-filesys-object
(let ([path (let-values ([(p fd) (posix-mkstemp "/tmp/hafod-delobj-XXXXXX")])
              (posix-close fd) p)])
  (delete-filesys-object path)
  (test-assert "delete-filesys-object removes file" (not (file-exists? path))))

(let ([dir "/tmp/hafod-delobj-dir"])
  (guard (e [#t #f]) (posix-rmdir dir))
  (create-directory dir)
  (delete-filesys-object dir)
  (test-assert "delete-filesys-object removes directory" (not (file-exists? dir))))

;; ---- Directory operations ----

;; directory-files
(let ([dir "/tmp/hafod-dirfiles-test"])
  (guard (e [#t #f]) (delete-filesys-object dir))
  (create-directory dir)
  ;; Create some test files
  (receive (p1 fd1) (posix-mkstemp (string-append dir "/aaa-XXXXXX"))
    (posix-close fd1)
    (receive (p2 fd2) (posix-mkstemp (string-append dir "/bbb-XXXXXX"))
      (posix-close fd2)
      ;; Create a dotfile
      (let ([dotfile (string-append dir "/.hidden")])
        (receive (dp dfd) (posix-mkstemp (string-append dotfile "-XXXXXX"))
          (posix-close dfd)

          (let ([files (directory-files dir)])
            (test-assert "directory-files returns a list" (list? files))
            (test-assert "directory-files excludes . and .."
              (and (not (member "." files))
                   (not (member ".." files))))
            (test-assert "directory-files excludes dotfiles by default"
              (not (exists (lambda (f) (char=? (string-ref f 0) #\.)) files)))
            (test-assert "directory-files returns sorted"
              (let check ([lst files])
                (or (null? lst) (null? (cdr lst))
                    (and (string<? (car lst) (cadr lst))
                         (check (cdr lst)))))))

          (let ([all-files (directory-files dir #t)])
            (test-assert "directory-files with dotfiles?=#t includes dot-files"
              (exists (lambda (f) (char=? (string-ref f 0) #\.)) all-files)))

          ;; Cleanup
          (posix-unlink dp))
        (posix-unlink p2))
      (posix-unlink p1)))
  (posix-rmdir dir))

;; directory-stream operations
(test-assert "directory-stream? predicate"
  (let ([ds (open-directory-stream "/tmp")])
    (let ([result (directory-stream? ds)])
      (close-directory-stream ds)
      result)))

(test-assert "read-directory-stream reads entries"
  (let ([ds (open-directory-stream "/tmp")])
    (let ([first (read-directory-stream ds)])
      (close-directory-stream ds)
      (string? first))))

(test-assert "read-directory-stream returns #f at end"
  (let ([dir "/tmp/hafod-dstest"])
    (guard (e [#t #f]) (posix-rmdir dir))
    (posix-mkdir dir #o755)
    (let ([ds (open-directory-stream dir)])
      ;; Empty dir has . and .. only -- readdir returns them
      (let loop ()
        (let ([entry (read-directory-stream ds)])
          (if entry (loop) #t)))  ;; Should reach #f
      (close-directory-stream ds)
      (posix-rmdir dir)
      #t)))

(test-end)
