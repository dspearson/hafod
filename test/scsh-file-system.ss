;;; Ported from scsh/test/file-system-tests.scm
;;; Tests for file system operations (section 3.3 of scsh manual)
;;; Original author: David Frese. Ported to hafod test runner.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod))

(test-begin "scsh-file-system")

;; ========================================================================
;; Helper functions
;; ========================================================================

(define *temp-dir* "/tmp/hafod-scsh-test/")

(define (create-temp-dir)
  (when (file-not-exists? *temp-dir*)
    (create-directory *temp-dir*))
  *temp-dir*)

;; Compute expected mode after umask
(define (mask mode)
  (bitwise-and mode (bitwise-not (umask))))

;; Extract only permission bits from a stat mode
(define (perm-bits mode)
  (bitwise-and mode #o7777))

(define (create-file* fname)
  (close (open-output-file fname)))

;; Safe delete: ignores ENOENT (errno 2)
(define (safe-delete fname)
  (guard (e [(and (posix-error? e) (= (posix-errno e) 2)) #t])
    (delete-filesys-object fname)))

;; ========================================================================
;; Create-Directory tests
;; ========================================================================

(test-assert "create-directory-1"
  (with-cwd (create-temp-dir)
    (create-directory "dir")
    (let ((result (file-directory? "dir")))
      (delete-filesys-object "dir")
      result)))

(test-assert "create-directory-2 (with mode)"
  (with-cwd (create-temp-dir)
    (create-directory "dir" #o700)
    (let ((result (and (file-directory? "dir")
                       (file-mode=? (perm-bits (file:mode "dir"))
                                    (mask #o700)))))
      (delete-filesys-object "dir")
      result)))

;; ========================================================================
;; Create-FIFO tests
;; ========================================================================

(test-assert "create-fifo-1"
  (with-cwd (create-temp-dir)
    (create-fifo "fifo")
    (let ((result (eq? (file:type "fifo") 'fifo)))
      (delete-filesys-object "fifo")
      result)))

(test-assert "create-fifo-2 (with mode)"
  (with-cwd (create-temp-dir)
    (create-fifo "fifo" #o700)
    (let ((result (and (eq? (file:type "fifo") 'fifo)
                       (file-mode=? (perm-bits (file:mode "fifo"))
                                    (mask #o700)))))
      (delete-filesys-object "fifo")
      result)))

;; ========================================================================
;; Create-hard-link test
;; ========================================================================

(test-assert "create-hard-link"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (create-hard-link "file" "hard-link")
    (let ((result (file-exists? "hard-link")))
      (delete-filesys-object "file")
      (delete-filesys-object "hard-link")
      result)))

;; ========================================================================
;; Create-symlink test
;; ========================================================================

(test-assert "create-symlink"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (create-symlink "file" "symlink")
    (let ((result (and (file-exists? "symlink")
                       (eq? (file:type "symlink" #f) 'symlink)
                       (eq? (file:type "symlink" #t) 'regular))))
      (delete-filesys-object "file")
      (delete-filesys-object "symlink")
      result)))

;; ========================================================================
;; Delete-directory test
;; ========================================================================

(test-assert "delete-directory"
  (with-cwd (create-temp-dir)
    (create-directory "dir")
    (delete-directory "dir")
    (file-not-exists? "dir")))

;; ========================================================================
;; Delete-file test
;; ========================================================================

(test-assert "delete-file"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (delete-file "file")
    (file-not-exists? "file")))

;; ========================================================================
;; Delete-filesys-object test
;; ========================================================================

(test-assert "delete-filesys-object"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (delete-filesys-object "file")
    (file-not-exists? "file")))

;; ========================================================================
;; Read-symlink test
;; ========================================================================

(test-assert "read-symlink"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (create-symlink "file" "symlink")
    (let ((result (equal? "file" (read-symlink "symlink"))))
      (delete-filesys-object "file")
      (delete-filesys-object "symlink")
      result)))

;; ========================================================================
;; Rename-file test
;; ========================================================================

(test-assert "rename-file"
  (with-cwd (create-temp-dir)
    (create-file* "file-1")
    (rename-file "file-1" "file-2")
    (let ((result (and (file-exists? "file-2")
                       (file-not-exists? "file-1"))))
      (delete-filesys-object "file-2")
      result)))

;; ========================================================================
;; Set-file-mode tests (fname variant)
;; ========================================================================

(test-assert "set-file-mode/fname"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-mode "file" #o754)
    (let ((result (file-mode=? (perm-bits (file:mode "file")) #o754)))
      (delete-filesys-object "file")
      result)))

;; set-file-mode via posix-fchmod on fd (hafod set-file-mode only accepts paths)
(test-assert "set-file-mode/fd (posix-fchmod)"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (let ((port (open-file "file" open/write)))
      (posix-fchmod (port->fdes port) #o754)
      (let ((result (file-mode=? (perm-bits (file:mode "file")) #o754)))
        (close port)
        (delete-filesys-object "file")
        result))))

;; ========================================================================
;; Set-file-owner test (fname variant)
;; ========================================================================

(test-assert "set-file-owner/fname"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-owner "file" (user-uid))
    (let ((result (= (file:owner "file") (user-uid))))
      (delete-filesys-object "file")
      result)))

;; ========================================================================
;; Set-file-group test (fname variant)
;; ========================================================================

(test-assert "set-file-group/fname"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-group "file" (user-gid))
    (let ((result (= (file:group "file") (user-gid))))
      (delete-filesys-object "file")
      result)))

;; ========================================================================
;; Set-file-times tests
;; ========================================================================

(test-assert "set-file-times-1 (last-access)"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-times "file" 10000 0)
    (let ((result (= (file:last-access "file") 10000)))
      (delete-filesys-object "file")
      result)))

(test-assert "set-file-times-2 (last-mod)"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-times "file" 0 10000)
    (let ((result (= (file:last-mod "file") 10000)))
      (delete-filesys-object "file")
      result)))

;; ========================================================================
;; Sync-file test
;; ========================================================================

(test-assert "sync-file"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (let ((port (open-file "file" open/write)))
      (display "1" port)
      (let ((res-1 (file:size "file")))
        (sync-file port)
        (let ((res-2 (file:size "file")))
          (close port)
          (delete-filesys-object "file")
          (and (= res-1 0) (> res-2 0)))))))

;; ========================================================================
;; Truncate-file tests
;; ========================================================================

(test-assert "truncate-file/fname"
  (with-cwd (create-temp-dir)
    (let ((port (open-file "file" (bitwise-ior open/write open/create open/truncate))))
      (display (make-string 100 #\*) port)
      (sync-file port)
      (close port))
    (truncate-file "file" 10)
    (let ((result (= (file:size "file") 10)))
      (delete-filesys-object "file")
      result)))

(test-assert "truncate-file/fd"
  (with-cwd (create-temp-dir)
    (let ((port (open-file "file" (bitwise-ior open/write open/create open/truncate))))
      (display (make-string 100 #\*) port)
      (sync-file port)
      (truncate-file (port->fdes port) 10)
      (let ((result (= (file:size "file") 10)))
        (close port)
        (delete-filesys-object "file")
        result))))

;; Note: hafod truncate-file accepts path or fd, not port directly.
;; Port variant tested via port->fdes extraction.
(test-assert "truncate-file/port-via-fdes"
  (with-cwd (create-temp-dir)
    (let ((port (open-file "file" (bitwise-ior open/write open/create open/truncate))))
      (display (make-string 100 #\*) port)
      (sync-file port)
      (truncate-file (port->fdes port) 10)
      (let ((result (= (file:size "file") 10)))
        (close port)
        (delete-filesys-object "file")
        result))))

;; ========================================================================
;; file-info type tests
;; ========================================================================

(test-assert "file:type-dir"
  (with-cwd (create-temp-dir)
    (create-directory "dir")
    (let ((result (eq? (file:type "dir") 'directory)))
      (delete-filesys-object "dir")
      result)))

(test-assert "file:type-fifo"
  (with-cwd (create-temp-dir)
    (create-fifo "fifo")
    (let ((result (eq? (file:type "fifo") 'fifo)))
      (delete-filesys-object "fifo")
      result)))

(test-assert "file:type-regular"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (let ((result (eq? (file:type "file") 'regular)))
      (delete-filesys-object "file")
      result)))

(test-assert "file:type-symlink"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (create-symlink "file" "symlink")
    (let ((result (and (eq? (file:type "symlink" #f) 'symlink)
                       (eq? (file:type "symlink" #t) 'regular))))
      (delete-filesys-object "symlink")
      (delete-filesys-object "file")
      result)))

;; ========================================================================
;; file:inode test -- verify returns non-negative integer
;; ========================================================================

(test-assert "file:inode/fname"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (let ((result (and (integer? (file:inode "file"))
                       (>= (file:inode "file") 0))))
      (delete-filesys-object "file")
      result)))

;; ========================================================================
;; file:mode tests (fname/fd/port)
;; ========================================================================

(test-assert "file:mode/fname"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-mode "file" #o754)
    (let ((result (file-mode=? (perm-bits (file:mode "file")) #o754)))
      (delete-filesys-object "file")
      result)))

;; ========================================================================
;; file:nlinks test
;; ========================================================================

(test-assert "file:nlinks"
  (with-cwd (create-temp-dir)
    (create-file* "file-1")
    (create-hard-link "file-1" "file-2")
    (let ((result (= (file:nlinks "file-1") 2)))
      (delete-filesys-object "file-2")
      (delete-filesys-object "file-1")
      result)))

;; ========================================================================
;; file:owner / file:group tests
;; ========================================================================

(test-assert "file:owner"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-owner "file" (user-uid))
    (let ((result (= (file:owner "file") (user-uid))))
      (delete-filesys-object "file")
      result)))

(test-assert "file:group"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-group "file" (user-gid))
    (let ((result (= (file:group "file") (user-gid))))
      (delete-filesys-object "file")
      result)))

;; ========================================================================
;; file:size test
;; ========================================================================

(test-assert "file:size"
  (with-cwd (create-temp-dir)
    (let ((port (open-file "file" (bitwise-ior open/write open/create open/truncate))))
      (display "0123456789" port)
      (sync-file port)
      (let ((result (= (file:size "file") 10)))
        (close port)
        (delete-filesys-object "file")
        result))))

;; ========================================================================
;; file:last-access / file:last-mod tests
;; ========================================================================

(test-assert "file:last-access"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-times "file" 10000 0)
    (let ((result (= (file:last-access "file") 10000)))
      (delete-filesys-object "file")
      result)))

(test-assert "file:last-mod"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-times "file" 0 10000)
    (let ((result (= (file:last-mod "file") 10000)))
      (delete-filesys-object "file")
      result)))

;; ========================================================================
;; file:last-status-change test
;; ========================================================================

(test-assert "file:last-status-change"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (let ((before (file:last-status-change "file")))
      ;; Change file mode to trigger ctime update
      (set-file-mode "file" #o777)
      (let ((after (file:last-status-change "file")))
        (delete-filesys-object "file")
        (>= after before)))))

;; ========================================================================
;; file-not-readable? / file-not-writable? / file-not-executable? tests
;; ========================================================================

;; Normal: file with read permission should not return #t for file-not-readable?
(test-assert "file-not-readable?-normal"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-mode "file" #o444)
    (let ((result (not (file-not-readable? "file"))))
      (delete-filesys-object "file")
      result)))

;; Permission denied: file without read permission
(test-assert "file-not-readable?-permission"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-mode "file" (bitwise-and #o777 (bitwise-not #o444)))
    (let ((result (file-not-readable? "file")))
      (delete-filesys-object "file")
      result)))

;; Nonexistent file
(test-assert "file-not-readable?-nonexistent"
  (with-cwd (create-temp-dir)
    (safe-delete "file")
    (file-not-readable? "file")))

;; Normal: file with write permission
(test-assert "file-not-writable?-normal"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-mode "file" #o222)
    (let ((result (not (file-not-writable? "file"))))
      (delete-filesys-object "file")
      result)))

;; Permission denied: file without write permission
(test-assert "file-not-writable?-permission"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-mode "file" (bitwise-and #o777 (bitwise-not #o222)))
    (let ((result (file-not-writable? "file")))
      (delete-filesys-object "file")
      result)))

;; Normal: file with exec permission
(test-assert "file-not-executable?-normal"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-mode "file" #o111)
    (let ((result (not (file-not-executable? "file"))))
      (delete-filesys-object "file")
      result)))

;; Permission denied: file without exec permission
(test-assert "file-not-executable?-permission"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (set-file-mode "file" (bitwise-and #o777 (bitwise-not #o111)))
    (let ((result (file-not-executable? "file")))
      (delete-filesys-object "file")
      result)))

;; ========================================================================
;; file-exists? / file-not-exists? tests
;; ========================================================================

(test-assert "file-not-exists/exists"
  (with-cwd (create-temp-dir)
    (safe-delete "file")
    (let ((res-1 (file-not-exists? "file")))
      (create-file* "file")
      (let ((res-2 (file-exists? "file")))
        (delete-filesys-object "file")
        (and res-1 res-2)))))

;; ========================================================================
;; directory-files tests
;; ========================================================================

(test-assert "directory-files (dotfiles visible)"
  (with-cwd (create-temp-dir)
    (create-file* ".file")
    (let ((result (member ".file" (directory-files (cwd) #t))))
      (delete-filesys-object ".file")
      result)))

(test-assert "directory-files (dotfiles hidden)"
  (with-cwd (create-temp-dir)
    (create-file* "file")
    (let ((result (member "file" (directory-files (cwd) #f))))
      (delete-filesys-object "file")
      result)))

;; ========================================================================
;; create-temp-file tests
;; ========================================================================

(test-assert "create-temp-file"
  (let ((temp-dir (create-temp-dir)))
    (let ((file-1 (create-temp-file temp-dir))
          (file-2 (create-temp-file temp-dir)))
      (let ((result (and (not (equal? file-1 file-2))
                         (file-exists? file-1)
                         (file-exists? file-2))))
        (delete-filesys-object file-1)
        (delete-filesys-object file-2)
        result))))

;; ========================================================================
;; Cleanup temp directory
;; ========================================================================
(when (file-exists? *temp-dir*)
  (delete-filesys-object *temp-dir*))

(test-end)
