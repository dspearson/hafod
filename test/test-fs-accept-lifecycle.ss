#!chezscheme
;;; test-fs-accept-lifecycle.ss -- Acceptance tests: file lifecycle, glob, symlinks
;;; Requirements: FS-01, FS-02, FS-05
(library-directories '(("src" . "src") ("." . ".")))
(import (hafod fileinfo) (hafod glob) (hafod fd-ports) (hafod posix)
        (hafod compat) (hafod rdelim) (hafod fname)
        (except (chezscheme) vector-append open-input-file open-output-file getenv
                file-directory? file-regular? file-symbolic-link?
                file-exists? delete-file delete-directory rename-file
                truncate-file)
        (test runner))
(test-begin "File System & I/O Acceptance")

;;; ======================================================================
;;; Section 1: FS-01 -- Full file lifecycle
;;; ======================================================================

;; Test 1: create, write, stat, read, verify
(let ([path "/tmp/hafod-accept-fs01-lifecycle"])
  (guard (e [#t #f]) (posix-unlink path))
  (let ([op (open-output-file path)])
    (display "hello world\n" op)
    (close op))
  (let ([info (file-info path)])
    (test-equal "lifecycle: file type is regular"
      'regular
      (file-info:type info))
    (test-equal "lifecycle: file size is 12"
      12
      (file-info:size info)))
  (let ([ip (open-input-file path)])
    (test-equal "lifecycle: read-line returns content"
      "hello world"
      (read-line ip))
    (close ip))
  (posix-unlink path))

;; Test 2: chmod changes mode bits
(let ([path "/tmp/hafod-accept-fs01-chmod"])
  (guard (e [#t #f]) (posix-unlink path))
  (let ([op (open-output-file path)])
    (display "chmod-test" op)
    (close op))
  (set-file-mode path #o644)
  (test-equal "lifecycle: chmod to 644"
    #o644
    (bitwise-and (file-info:mode (file-info path)) #o777))
  (set-file-mode path #o600)
  (test-equal "lifecycle: chmod to 600"
    #o600
    (bitwise-and (file-info:mode (file-info path)) #o777))
  (posix-unlink path))

;; Test 3: rename moves file
(let ([old-path "/tmp/hafod-accept-fs01-rename-old"]
      [new-path "/tmp/hafod-accept-fs01-rename-new"])
  (guard (e [#t #f]) (posix-unlink old-path))
  (guard (e [#t #f]) (posix-unlink new-path))
  (let ([op (open-output-file old-path)])
    (display "rename-content" op)
    (close op))
  (rename-file old-path new-path)
  (test-assert "lifecycle: old path gone after rename"
    (file-not-exists? old-path))
  (test-assert "lifecycle: new path exists after rename"
    (file-exists? new-path))
  (let ([ip (open-input-file new-path)])
    (test-equal "lifecycle: renamed file has same content"
      "rename-content"
      (read-line ip))
    (close ip))
  (posix-unlink new-path))

;; Test 4: delete removes file
(let ([path "/tmp/hafod-accept-fs01-delete"])
  (guard (e [#t #f]) (posix-unlink path))
  (let ([op (open-output-file path)])
    (display "delete-me" op)
    (close op))
  (test-assert "lifecycle: file exists before delete"
    (file-exists? path))
  (delete-file path)
  (test-assert "lifecycle: file gone after delete"
    (file-not-exists? path)))

;; Test 5: truncate shrinks file
(let ([path "/tmp/hafod-accept-fs01-truncate"])
  (guard (e [#t #f]) (posix-unlink path))
  (let ([op (open-output-file path)])
    ;; Write 100 bytes
    (display (make-string 100 #\x) op)
    (close op))
  (test-equal "lifecycle: file is 100 bytes"
    100
    (file-info:size (file-info path)))
  (truncate-file path 10)
  (test-equal "lifecycle: file is 10 bytes after truncate"
    10
    (file-info:size (file-info path)))
  (let ([ip (open-input-file path)])
    (let ([content (get-string-n ip 10)])
      (test-equal "lifecycle: truncated content preserved"
        (make-string 10 #\x)
        content))
    (close ip))
  (posix-unlink path))

;; Test 6: create, write, append, verify combined content
(let ([path "/tmp/hafod-accept-fs01-append"])
  (guard (e [#t #f]) (posix-unlink path))
  ;; Write initial content
  (let ([op (open-output-file path)])
    (display "first\n" op)
    (close op))
  ;; Append more content
  (let ([op (open-file path (bitwise-ior open/append open/write))])
    (display "second\n" op)
    (close op))
  ;; Read back combined
  (let ([ip (open-input-file path)])
    (test-equal "lifecycle: first line after append"
      "first"
      (read-line ip))
    (test-equal "lifecycle: second line after append"
      "second"
      (read-line ip))
    (close ip))
  (posix-unlink path))

;; Test 7: hard link shares inode
(let ([path "/tmp/hafod-accept-fs01-hardlink-orig"]
      [link "/tmp/hafod-accept-fs01-hardlink-link"])
  (guard (e [#t #f]) (posix-unlink path))
  (guard (e [#t #f]) (posix-unlink link))
  (let ([op (open-output-file path)])
    (display "hardlink-data" op)
    (close op))
  (create-hard-link path link)
  (test-assert "lifecycle: both paths exist after hard link"
    (and (file-exists? path) (file-exists? link)))
  (test-equal "lifecycle: hard link same inode"
    (file-info:inode (file-info path))
    (file-info:inode (file-info link)))
  (test-equal "lifecycle: nlinks is 2"
    2
    (file-info:nlinks (file-info path)))
  ;; Delete original, link survives
  (delete-file path)
  (test-assert "lifecycle: hard link survives original deletion"
    (file-exists? link))
  (let ([ip (open-input-file link)])
    (test-equal "lifecycle: hard link content readable after delete"
      "hardlink-data"
      (read-line ip))
    (close ip))
  (posix-unlink link))

;; Test 8: file predicates correct at each stage
(let ([dir "/tmp/hafod-accept-fs01-predicates"]
      [file-in-dir "/tmp/hafod-accept-fs01-predicates/testfile"])
  (guard (e [#t #f]) (posix-unlink file-in-dir))
  (guard (e [#t #f]) (posix-rmdir dir))
  (create-directory dir #o755)
  (test-assert "lifecycle: directory predicate true"
    (file-directory? dir))
  (test-assert "lifecycle: regular predicate false on dir"
    (not (file-regular? dir)))
  (let ([op (open-output-file file-in-dir)])
    (display "pred-test" op)
    (close op))
  (test-assert "lifecycle: regular predicate true on file"
    (file-regular? file-in-dir))
  (delete-file file-in-dir)
  (test-assert "lifecycle: not-exists after delete"
    (file-not-exists? file-in-dir))
  (delete-directory dir))


;;; ======================================================================
;;; Section 2: FS-02 -- Recursive directory traversal with glob filtering
;;; ======================================================================

;; Set up test directory tree
(let ([base "/tmp/hafod-accept-fs02"])
  ;; Cleanup from any previous run
  (define (cleanup-fs02)
    (for-each (lambda (f) (guard (e [#t #f]) (posix-unlink f)))
      (list (string-append base "/src/lib/helper.ss")
            (string-append base "/src/main.ss")
            (string-append base "/src/util.ss")
            (string-append base "/test/test-main.ss")
            (string-append base "/test/test-util.ss")
            (string-append base "/README.txt")
            (string-append base "/notes.txt")))
    (for-each (lambda (d) (guard (e [#t #f]) (posix-rmdir d)))
      (list (string-append base "/src/lib")
            (string-append base "/src")
            (string-append base "/test")
            base)))

  (cleanup-fs02)

  ;; Create directory structure
  (create-directory base #o755)
  (create-directory (string-append base "/src") #o755)
  (create-directory (string-append base "/src/lib") #o755)
  (create-directory (string-append base "/test") #o755)

  ;; Create files using open-output-file
  (for-each (lambda (relpath)
              (let ([op (open-output-file (string-append base "/" relpath))])
                (display "test-content" op)
                (close op)))
    '("src/main.ss" "src/util.ss" "src/lib/helper.ss"
      "test/test-main.ss" "test/test-util.ss"
      "README.txt" "notes.txt"))

  ;; Test 1: glob *.ss in src/ (non-recursive)
  (let ([result (glob (string-append base "/src/*.ss"))])
    (test-equal "glob: src/*.ss finds 2 files"
      2
      (length result))
    (test-assert "glob: src/*.ss includes main.ss"
      (member (string-append base "/src/main.ss") result))
    (test-assert "glob: src/*.ss includes util.ss"
      (member (string-append base "/src/util.ss") result)))

  ;; Test 2: glob in nested directory
  (let ([result (glob (string-append base "/src/lib/*.ss"))])
    (test-equal "glob: src/lib/*.ss finds 1 file"
      1
      (length result))
    (test-assert "glob: src/lib/*.ss includes helper.ss"
      (member (string-append base "/src/lib/helper.ss") result)))

  ;; Test 3: brace expansion
  (let ([result (glob (string-append base "/{src,test}/*.ss"))])
    (test-equal "glob: brace expansion finds 4 files"
      4
      (length result)))

  ;; Test 4: ? wildcard
  (let ([result (glob (string-append base "/*.tx?"))])
    (test-equal "glob: ? wildcard finds 2 .txt files"
      2
      (length result)))

  ;; Test 5: non-matching pattern
  (let ([result (glob (string-append base "/*.xyz"))])
    (test-equal "glob: non-matching returns empty"
      '()
      result))

  ;; Test 6: directory-files lists known directory
  (let ([result (directory-files (string-append base "/src"))])
    (test-assert "glob: directory-files contains lib"
      (member "lib" result))
    (test-assert "glob: directory-files contains main.ss"
      (member "main.ss" result))
    (test-assert "glob: directory-files contains util.ss"
      (member "util.ss" result)))

  ;; Test 7: glob directory pattern with trailing slash
  (let ([result (glob (string-append base "/*/"))])
    (test-assert "glob: trailing slash finds directories"
      (>= (length result) 2))
    (test-assert "glob: trailing slash includes src/"
      (member (string-append base "/src/") result))
    (test-assert "glob: trailing slash includes test/"
      (member (string-append base "/test/") result)))

  ;; Cleanup
  (cleanup-fs02))


;;; ======================================================================
;;; Section 3: FS-05 -- Symlinks
;;; ======================================================================

;; Test 1: symlink create and read-symlink
(let ([target "/tmp/hafod-accept-fs05-target"]
      [link "/tmp/hafod-accept-fs05-link"])
  (guard (e [#t #f]) (posix-unlink link))
  (guard (e [#t #f]) (posix-unlink target))
  (let ([op (open-output-file target)])
    (display "symlink-target" op)
    (close op))
  (create-symlink target link)
  (test-equal "symlink: read-symlink returns target"
    target
    (read-symlink link))
  (posix-unlink link)
  (posix-unlink target))

;; Test 2: file-symlink? detects symlink
(let ([target "/tmp/hafod-accept-fs05-detect-tgt"]
      [link "/tmp/hafod-accept-fs05-detect-lnk"])
  (guard (e [#t #f]) (posix-unlink link))
  (guard (e [#t #f]) (posix-unlink target))
  (let ([op (open-output-file target)])
    (display "detect-test" op)
    (close op))
  (create-symlink target link)
  (test-assert "symlink: file-symlink? returns #t"
    (file-symlink? link))
  (test-assert "symlink: file-regular? follows to target"
    (file-regular? link))
  (posix-unlink link)
  (posix-unlink target))

;; Test 3: file-info chase=#f returns symlink type
(let ([target "/tmp/hafod-accept-fs05-chase-tgt"]
      [link "/tmp/hafod-accept-fs05-chase-lnk"])
  (guard (e [#t #f]) (posix-unlink link))
  (guard (e [#t #f]) (posix-unlink target))
  (let ([op (open-output-file target)])
    (display "chase-test" op)
    (close op))
  (create-symlink target link)
  (test-equal "symlink: file-info chase=#f type is symlink"
    'symlink
    (file-info:type (file-info link #f)))
  (test-equal "symlink: file-info chase=#t type is regular"
    'regular
    (file-info:type (file-info link #t)))
  (posix-unlink link)
  (posix-unlink target))

;; Test 4: symlink to directory
(let ([dir "/tmp/hafod-accept-fs05-dir"]
      [link "/tmp/hafod-accept-fs05-dirlink"]
      [file-in-dir "/tmp/hafod-accept-fs05-dir/inside.txt"])
  (guard (e [#t #f]) (posix-unlink link))
  (guard (e [#t #f]) (posix-unlink file-in-dir))
  (guard (e [#t #f]) (posix-rmdir dir))
  (create-directory dir #o755)
  (let ([op (open-output-file file-in-dir)])
    (display "inside-dir" op)
    (close op))
  (create-symlink dir link)
  (test-assert "symlink: file-directory? on dir symlink"
    (file-directory? link))
  (test-assert "symlink: file-symlink? on dir symlink"
    (file-symlink? link))
  (test-assert "symlink: directory-files through symlink"
    (member "inside.txt" (directory-files link)))
  (posix-unlink link)
  (posix-unlink file-in-dir)
  (posix-rmdir dir))

;; Test 5: symlink chain: link to link to file
(let ([target "/tmp/hafod-accept-fs05-chain-tgt"]
      [link-a "/tmp/hafod-accept-fs05-chain-a"]
      [link-b "/tmp/hafod-accept-fs05-chain-b"])
  (guard (e [#t #f]) (posix-unlink link-b))
  (guard (e [#t #f]) (posix-unlink link-a))
  (guard (e [#t #f]) (posix-unlink target))
  (let ([op (open-output-file target)])
    (display "chain-data" op)
    (close op))
  (create-symlink target link-a)
  (create-symlink link-a link-b)
  (test-equal "symlink: read-symlink on chain returns immediate target"
    link-a
    (read-symlink link-b))
  (test-equal "symlink: file-info chase on chain resolves to regular"
    'regular
    (file-info:type (file-info link-b #t)))
  (test-equal "symlink: file-info no-chase on chain is symlink"
    'symlink
    (file-info:type (file-info link-b #f)))
  (posix-unlink link-b)
  (posix-unlink link-a)
  (posix-unlink target))

;; Test 6: dangling symlink
(let ([link "/tmp/hafod-accept-fs05-dangling"])
  (guard (e [#t #f]) (posix-unlink link))
  (create-symlink "/tmp/hafod-accept-fs05-nonexistent-target" link)
  (test-assert "symlink: dangling file-exists? is false"
    (not (file-exists? link)))
  (test-assert "symlink: dangling file-symlink? is true"
    (file-symlink? link))
  (posix-unlink link))

;; Test 7: read-symlink returns exact target path (relative)
(let ([dir "/tmp/hafod-accept-fs05-reldir"]
      [link "/tmp/hafod-accept-fs05-reldir/rellink"])
  (guard (e [#t #f]) (posix-unlink link))
  (guard (e [#t #f]) (posix-rmdir dir))
  (create-directory dir #o755)
  (create-symlink "../targetfile" link)
  (test-equal "symlink: read-symlink returns exact relative target"
    "../targetfile"
    (read-symlink link))
  (posix-unlink link)
  (posix-rmdir dir))

(test-end)
