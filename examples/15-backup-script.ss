;;; 15-backup-script.ss -- Backup files to a temp directory
;;;
;;; Demonstrates: create-directory, directory-files, file-info,
;;;               file-regular?, file-info:mtime, glob,
;;;               run, run/string, pipe, with-cwd,
;;;               create-temp-file, format-date, date,
;;;               file-name-extension, read-symlink, create-symlink,
;;;               file-name-nondirectory, file-name-as-directory,
;;;               set-file-mode, file:mode, file:size
;;;
;;; A practical backup script that copies files from a source
;;; directory to a timestamped backup directory. Demonstrates
;;; file operations, globbing, and process notation together.
;;; Source: common scsh sysadmin scripts.

(import (hafod))

(define (section title)
  (newline)
  (display (string-append "=== " title " ===\n")))

(define (pad-right str width)
  (if (>= (string-length str) width) str
      (string-append str (make-string (- width (string-length str)) #\space))))

(define (format-size bytes)
  (cond
    [(>= bytes (* 1024 1024))
     (string-append (number->string (quotient bytes (* 1024 1024))) "M")]
    [(>= bytes 1024)
     (string-append (number->string (quotient bytes 1024)) "K")]
    [else (string-append (number->string bytes) "B")]))

;;; --- Set up source directory with sample files ---
(section "Setup")

(define base-dir (string-append "/tmp/hafod-backup-demo-" (number->string (pid))))
(define src-dir (string-append base-dir "/source"))
(define bak-dir (string-append base-dir "/backup-" (format-date "~Y~m~d-~H~M~S" (date))))

(create-directory base-dir)
(create-directory src-dir)

;; Create sample source files
(for-each
  (lambda (spec)
    (let ([name (car spec)]
          [content (cdr spec)])
      (let ([p (open-output-file (string-append src-dir "/" name))])
        (display content p)
        (close p))))
  '(("report.txt" . "Quarterly Report\nQ1 2026\nRevenue: up\n")
    ("data.csv" . "name,value\nalpha,100\nbeta,200\n")
    ("config.ini" . "[section]\nkey=value\ndebug=false\n")
    ("notes.md" . "# Notes\n- Item one\n- Item two\n")
    ("script.ss" . "(display \"hello\")\n")))

;; Create a symlink
(create-symlink "report.txt" (string-append src-dir "/latest-report"))

(display (string-append "Source directory: " src-dir "\n"))
(display (string-append "Backup directory: " bak-dir "\n"))

;;; --- List source files ---
(section "Source Files")

(let ([files (directory-files src-dir)])
  (display (string-append (pad-right "File" 20)
                          (pad-right "Size" 8)
                          (pad-right "Type" 12)
                          "Modified\n"))
  (display (make-string 55 #\-))
  (newline)
  (for-each
    (lambda (name)
      (let* ([path (string-append src-dir "/" name)]
             [info (file-info path #f)])
        (display (pad-right name 20))
        (display (pad-right (format-size (file-info:size info)) 8))
        (display (pad-right (symbol->string (file-info:type info)) 12))
        (display (format-date "~Y-~m-~d ~H:~M" (date (file-info:mtime info))))
        (when (eq? (file-info:type info) 'symlink)
          (display (string-append " -> " (read-symlink path))))
        (newline)))
    files))

;;; --- Perform backup ---
(section "Backup")

(create-directory bak-dir)

(let ([files (directory-files src-dir)]
      [copied 0]
      [total-bytes 0])
  (for-each
    (lambda (name)
      (let* ([src-path (string-append src-dir "/" name)]
             [bak-path (string-append bak-dir "/" name)]
             [info (file-info src-path #f)])
        (cond
          ;; Copy regular files using cp (unquote for dynamic paths)
          [(file-info-regular? info)
           (run (cp -p ,src-path ,bak-path))
           (set! copied (+ copied 1))
           (set! total-bytes (+ total-bytes (file-info:size info)))
           (display (string-append "  Copied: " name
                                   " (" (format-size (file-info:size info)) ")\n"))]
          ;; Recreate symlinks
          [(file-info-symlink? info)
           (let ([target (read-symlink src-path)])
             (create-symlink target bak-path)
             (display (string-append "  Linked: " name " -> " target "\n")))]
          [else
           (display (string-append "  Skipped: " name
                                   " (" (symbol->string (file-info:type info)) ")\n"))])))
    files)
  (display (string-append "\nBackup complete: "
                          (number->string copied) " files, "
                          (format-size total-bytes) " total\n")))

;;; --- Verify backup ---
(section "Verification")

;; Compare source and backup
(let ([src-files (sort string<? (directory-files src-dir))]
      [bak-files (sort string<? (directory-files bak-dir))])
  (display (string-append "Source files: " (number->string (length src-files)) "\n"))
  (display (string-append "Backup files: " (number->string (length bak-files)) "\n"))
  (display (string-append "Match: " (if (equal? src-files bak-files) "yes" "NO!") "\n"))

  ;; Verify file contents match using diff
  (for-each
    (lambda (name)
      (when (file-regular? (string-append src-dir "/" name))
        (let* ([a (string-append src-dir "/" name)]
               [b (string-append bak-dir "/" name)]
               [ca (let ([p (open-input-file a)]) (let ([s (port->string p)]) (close-input-port p) s))]
               [cb (let ([p (open-input-file b)]) (let ([s (port->string p)]) (close-input-port p) s))])
          (display (string-append "  " name ": "
                                  (if (string=? ca cb) "OK" "DIFFERS!") "\n")))))
    src-files))

;;; --- Cleanup ---
(section "Cleanup")

;; Remove all files in both directories
(for-each
  (lambda (dir)
    (for-each
      (lambda (f)
        (let ([path (string-append dir "/" f)])
          (guard (e [#t #f])
            (delete-filesys-object path))))
      (directory-files dir))
    (delete-directory dir))
  (list bak-dir src-dir))
(delete-directory base-dir)
(display "Cleaned up temp directories.\n")

(display "\nDone.\n")
