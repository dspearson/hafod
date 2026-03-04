#!chezscheme
;;; test-scsh-scripts.ss -- End-to-end realistic scsh-style script tests
;;; Requirements: SCSH-04
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (hafod syntax) (hafod process) (hafod procobj)
        (hafod posix) (hafod fd-ports) (hafod compat)
        (hafod collect) (hafod port-collect) (hafod rdelim)
        (hafod re) (hafod environment) (hafod process-state)
        (hafod fileinfo) (hafod fname) (hafod glob)
        (hafod temp-file) (hafod awk) (hafod field-reader)
        (hafod time) (hafod signal) (hafod system) (hafod user-group)
        (except (chezscheme) exit vector-append open-input-file open-output-file
                getenv record-reader)
        (test runner))

(test-begin "Realistic scsh-Style Scripts")

;; Helpers
(define (string-contains haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

(define (trim-newline s)
  (let ((len (string-length s)))
    (if (and (> len 0) (char=? (string-ref s (- len 1)) #\newline))
        (substring s 0 (- len 1))
        s)))

;; Simple string split by single character delimiter
(define (string-split str delim)
  (let ([len (string-length str)]
        [dc (string-ref delim 0)])
    (let loop ([i 0] [start 0] [acc '()])
      (cond
        [(>= i len)
         (reverse (if (> i start)
                      (cons (substring str start i) acc)
                      acc))]
        [(char=? (string-ref str i) dc)
         (loop (+ i 1) (+ i 1)
               (if (> i start)
                   (cons (substring str start i) acc)
                   acc))]
        [else (loop (+ i 1) start acc)]))))

(define base-dir (string-append "/tmp/hafod-scsh-scripts-" (number->string (pid))))

;; Create base temp directory
(create-directory base-dir)

;; =============================================================================
;; Section 1: SCSH-04 -- Sysadmin script: File inventory and monitoring
;; =============================================================================

(let* ([dir (string-append base-dir "/inventory")]
       [sub (string-append dir "/subdir")]
       [f1 (string-append dir "/file1.txt")]
       [f2 (string-append dir "/file2.txt")]
       [f3 (string-append dir "/data.csv")]
       [f4 (string-append sub "/nested.txt")]
       [link (string-append dir "/link-to-f1")])

  ;; Setup: create directory tree
  (create-directory dir)
  (create-directory sub)
  (let ([p (open-output-file f1)])
    (display "line1\nline2\nline3\n" p) (close p))
  (let ([p (open-output-file f2)])
    (display "hello\n" p) (close p))
  (let ([p (open-output-file f3)])
    (display "a,b,c\n1,2,3\n" p) (close p))
  (let ([p (open-output-file f4)])
    (display "nested content\n" p) (close p))
  (create-symlink f1 link)

  ;; Test: directory-files lists expected entries
  (let ([files (directory-files dir)])
    (test-assert "inventory: directory-files returns list"
      (list? files))
    (test-assert "inventory: contains file1.txt"
      (member "file1.txt" files))
    (test-assert "inventory: contains subdir"
      (member "subdir" files))
    (test-assert "inventory: contains link-to-f1"
      (member "link-to-f1" files)))

  ;; Test: file-info on different types
  (test-assert "inventory: file1.txt is regular"
    (file-regular? f1))
  (test-assert "inventory: subdir is directory"
    (file-directory? sub))
  (test-assert "inventory: link is symlink"
    (file-symlink? link))

  ;; Test: glob finds txt files
  (let ([txt-files (glob (string-append dir "/*.txt"))])
    (test-assert "inventory: glob finds txt files"
      (>= (length txt-files) 2)))

  ;; Test: wc -l in pipeline
  (let ([result (trim-newline (run/string (wc "-l" ,f1)))])
    (test-assert "inventory: wc -l counts lines"
      (string-contains result "3")))

  ;; Test: file size is positive
  (test-assert "inventory: file size > 0"
    (> (file-info:size (file-info f1)) 0))

  ;; Cleanup
  (posix-unlink link)
  (posix-unlink f4)
  (posix-unlink f3)
  (posix-unlink f2)
  (posix-unlink f1)
  (delete-directory sub)
  (delete-directory dir))

;; =============================================================================
;; Section 2: SCSH-04 -- Text processing pipeline: Word frequency analysis
;; =============================================================================

(let* ([dir (string-append base-dir "/wordfreq")]
       [infile (string-append dir "/text.txt")])

  ;; Setup
  (create-directory dir)
  (let ([p (open-output-file infile)])
    (display "the quick brown fox\n" p)
    (display "the lazy dog and the fox\n" p)
    (display "the quick quick fox jumps\n" p)
    (display "over the lazy dog\n" p)
    (display "the fox and the dog\n" p)
    (close p))

  ;; Use AWK to count words
  (let ([counts (make-hashtable string-hash string=?)])
    (let ([p (open-input-file infile)])
      (awk (read-line p) (line) ()
        (#t
         (let ([words (string-split line " ")])
           (for-each
             (lambda (w)
               (when (> (string-length w) 0)
                 (hashtable-set! counts w
                   (+ 1 (hashtable-ref counts w 0)))))
             words))))
      (close p))

    ;; Verify: "the" is the most frequent word
    (test-assert "wordfreq: 'the' is most frequent"
      (>= (hashtable-ref counts "the" 0) 5))

    ;; Verify: "fox" appears multiple times
    (test-assert "wordfreq: 'fox' count >= 3"
      (>= (hashtable-ref counts "fox" 0) 3))

    ;; Verify: unique word count
    (test-assert "wordfreq: at least 8 unique words"
      (>= (hashtable-size counts) 8)))

  ;; Alternative: pipeline approach
  (let ([result (run/string (pipe (cat ,infile)
                                  (tr " " "\n")
                                  (sort)
                                  (uniq "-c")
                                  (sort "-rn")
                                  (head "-1")))])
    (test-assert "wordfreq: pipeline top word contains 'the'"
      (string-contains result "the")))

  ;; Cleanup
  (posix-unlink infile)
  (delete-directory dir))

;; =============================================================================
;; Section 3: SCSH-04 -- Text processing pipeline: Log parsing with regex
;; =============================================================================

(let* ([dir (string-append base-dir "/logparse")]
       [logfile (string-append dir "/app.log")])

  ;; Setup: create synthetic log
  (create-directory dir)
  (let ([p (open-output-file logfile)])
    (display "2026-03-03 INFO startup complete\n" p)
    (display "2026-03-03 INFO request /api/users\n" p)
    (display "2026-03-03 WARN low memory 85%\n" p)
    (display "2026-03-03 ERROR disk full /data\n" p)
    (display "2026-03-03 INFO request /api/health\n" p)
    (display "2026-03-03 ERROR timeout db connection\n" p)
    (display "2026-03-03 WARN high cpu 92%\n" p)
    (display "2026-03-03 INFO request /api/status\n" p)
    (display "2026-03-03 ERROR permission denied /etc/secret\n" p)
    (display "2026-03-03 INFO shutdown graceful\n" p)
    (close p))

  ;; Count by level using AWK + regexp-search
  (let ([info-count 0]
        [warn-count 0]
        [error-count 0])
    (let ([p (open-input-file logfile)])
      (awk (read-line p) (line) ()
        ((when (regexp-search (rx "INFO") line))
         (set! info-count (+ info-count 1)))
        ((when (regexp-search (rx "WARN") line))
         (set! warn-count (+ warn-count 1)))
        ((when (regexp-search (rx "ERROR") line))
         (set! error-count (+ error-count 1))))
      (close p))

    (test-equal "logparse: INFO count" 5 info-count)
    (test-equal "logparse: WARN count" 2 warn-count)
    (test-equal "logparse: ERROR count" 3 error-count))

  ;; Cross-check with grep pipeline
  (let ([grep-errors (trim-newline (run/string (pipe (grep "ERROR" ,logfile) (wc "-l"))))])
    (test-assert "logparse: grep ERROR count"
      (string-contains grep-errors "3")))

  ;; Cleanup
  (posix-unlink logfile)
  (delete-directory dir))

;; =============================================================================
;; Section 4: SCSH-04 -- File management script: Backup with verify
;; =============================================================================

(let* ([src-dir (string-append base-dir "/backup-src")]
       [dst-dir (string-append base-dir "/backup-dst")]
       [src-f1 (string-append src-dir "/config.txt")]
       [src-f2 (string-append src-dir "/data.bin")]
       [src-f3 (string-append src-dir "/readme.txt")]
       [src-link (string-append src-dir "/config-link")])

  ;; Setup: source directory with files and symlink
  (create-directory src-dir)
  (create-directory dst-dir)

  (let ([p (open-output-file src-f1)])
    (display "key=value\nhost=localhost\nport=8080\n" p) (close p))
  (let ([p (open-output-file src-f2)])
    (display "binary data content here\n" p) (close p))
  (let ([p (open-output-file src-f3)])
    (display "Project readme\nVersion 1.0\n" p) (close p))
  (create-symlink src-f1 src-link)

  ;; Copy regular files (skip symlinks) preserving permissions
  (for-each
    (lambda (name)
      (let ([src (string-append src-dir "/" name)]
            [dst (string-append dst-dir "/" name)])
        (when (and (file-regular? src) (not (file-symlink? src)))
          (run (cp "-p" ,src ,dst)))))
    (directory-files src-dir))

  ;; Recreate symlink
  (let ([target (read-symlink src-link)])
    (create-symlink target (string-append dst-dir "/config-link")))

  ;; Verify: file count matches (non-symlink regular files)
  (let ([src-files (filter (lambda (f) (let ([p (string-append src-dir "/" f)])
                                         (and (file-regular? p) (not (file-symlink? p)))))
                           (directory-files src-dir))]
        [dst-files (filter (lambda (f) (let ([p (string-append dst-dir "/" f)])
                                         (and (file-regular? p) (not (file-symlink? p)))))
                           (directory-files dst-dir))])
    (test-equal "backup: same regular file count"
      (length src-files) (length dst-files)))

  ;; Verify: content matches (non-symlink regular files)
  (for-each
    (lambda (name)
      (let ([src (string-append src-dir "/" name)]
            [dst (string-append dst-dir "/" name)])
        (when (and (file-regular? src) (not (file-symlink? src)) (file-exists? dst))
          (let* ([p1 (open-input-file src)]
                 [c1 (get-string-all p1)]
                 [_c1 (close p1)]
                 [p2 (open-input-file dst)]
                 [c2 (get-string-all p2)]
                 [_c2 (close p2)])
            (test-equal (string-append "backup: content matches " name)
              c1 c2)))))
    (directory-files src-dir))

  ;; Verify: symlink target matches
  (test-equal "backup: symlink target preserved"
    (read-symlink src-link)
    (read-symlink (string-append dst-dir "/config-link")))

  ;; Verify: file permissions preserved
  (test-equal "backup: permissions preserved for config.txt"
    (file-info:mode (file-info src-f1))
    (file-info:mode (file-info (string-append dst-dir "/config.txt"))))

  ;; Cleanup: remove all files in dst-dir
  (for-each
    (lambda (name)
      (let ([f (string-append dst-dir "/" name)])
        (delete-filesys-object f)))
    (directory-files dst-dir))
  (posix-unlink src-link)
  (posix-unlink src-f3)
  (posix-unlink src-f2)
  (posix-unlink src-f1)
  (delete-directory dst-dir)
  (delete-directory src-dir))

;; =============================================================================
;; Section 5: SCSH-04 -- Combined: Environment + process + file workflow
;; =============================================================================

(let* ([tmpdir (string-append base-dir "/combined")]
       [outfile (string-append tmpdir "/child-output")])

  ;; Setup
  (create-directory tmpdir)

  ;; with-cwd + with-env: child sees scoped state
  (with-cwd tmpdir
    (with-env '(("HAFOD_TEST_VAR" . "test-value-42"))
      ;; Fork child that writes env var and cwd to file
      (run (sh "-c" "echo $HAFOD_TEST_VAR > child-output; pwd >> child-output"))))

  ;; Read child's output
  (let* ([p (open-input-file outfile)]
         [content (get-string-all p)]
         [_c (close p)]
         [lines (let loop ([s content] [acc '()])
                  (let ([nl (let find ([i 0])
                              (if (>= i (string-length s)) #f
                                  (if (char=? (string-ref s i) #\newline) i
                                      (find (+ i 1)))))])
                    (if nl
                        (loop (substring s (+ nl 1) (string-length s))
                              (cons (substring s 0 nl) acc))
                        (reverse (if (> (string-length s) 0)
                                     (cons s acc) acc)))))])

    ;; Verify child saw the env var
    (test-assert "combined: child saw HAFOD_TEST_VAR"
      (and (pair? lines)
           (string=? (car lines) "test-value-42")))

    ;; Verify child saw the correct cwd
    (test-assert "combined: child saw correct cwd"
      (and (>= (length lines) 2)
           (string-contains (cadr lines) "combined"))))

  ;; Verify original cwd is restored
  (test-assert "combined: original cwd restored"
    (not (string-contains (cwd) "combined")))

  ;; Cleanup
  (posix-unlink outfile)
  (delete-directory tmpdir))

;; Clean up base directory
(delete-directory base-dir)

(test-end)
