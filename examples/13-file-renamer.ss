;;; 13-file-renamer.ss -- Rename files using regex patterns (dry-run)
;;;
;;; Demonstrates: glob, rx, regexp-search, regexp-substitute,
;;;               file-name-directory, file-name-nondirectory,
;;;               file-name-extension, file-name-sans-extension,
;;;               parse-file-name, replace-extension,
;;;               create-temp-file, create-directory, delete-directory,
;;;               rename-file, directory-files
;;;
;;; A dry-run file renamer that shows what renames would be performed.
;;; Creates temp files to demonstrate actual renaming safely.
;;; Source: common scsh file-management scripts.

(import (hafod))

(define (section title)
  (newline)
  (display (string-append "=== " title " ===\n")))

;;; --- File name analysis ---
(section "File Name Analysis")

(define sample-names '("/home/user/document.txt"
                       "/var/log/syslog.1.gz"
                       "src/main.c"
                       "../backup/photo.2026-01-15.jpg"
                       "Makefile"
                       ".hidden-file"
                       "/usr/local/bin/my-script"))

(for-each
  (lambda (name)
    (let-values ([(dir base ext) (parse-file-name name)])
      (display (string-append "  " name "\n"))
      (display (string-append "    dir=" (or dir "\"\"")
                              " base=" base
                              " ext=" (if (string=? ext "") "(none)" ext) "\n"))))
  sample-names)

;;; --- Pattern-based rename planning ---
(section "Rename Planning (dry-run)")

(define (filter-map proc lst)
  (let loop ([l lst] [acc '()])
    (if (null? l) (reverse acc)
        (let ([result (proc (car l))])
          (loop (cdr l) (if result (cons result acc) acc))))))

(define (plan-renames files pattern replacement)
  ;; Returns list of (old-name . new-name) pairs
  (filter-map
    (lambda (file)
      (let ([base (file-name-nondirectory file)]
            [dir (file-name-directory file)])
        (let ([m (regexp-search pattern base)])
          (and m
               (let ([new-base (regexp-substitute #f m replacement)])
                 (cons file (string-append dir new-base)))))))
    files))

;; Example: plan renaming IMG_XXXX.jpg -> photo-XXXX.jpg
(let* ([files '("IMG_0001.jpg" "IMG_0002.jpg" "README.txt"
                "IMG_0003.png" "IMG_0004.jpg" "notes.md")]
       [renames (let loop ([fs files] [acc '()])
                  (if (null? fs) (reverse acc)
                      (let* ([file (car fs)]
                             [m (regexp-search
                                  (rx "IMG_" (submatch (+ digit)) "." (submatch (+ alpha)))
                                  file)])
                        (loop (cdr fs)
                              (if m
                                  (cons (cons file
                                              (regexp-substitute #f m
                                                'pre "photo-" 1 "." 2 'post))
                                        acc)
                                  acc)))))])
  (if (null? renames)
      (display "  No files match pattern\n")
      (for-each
        (lambda (pair)
          (display (string-append "  " (car pair) " -> " (cdr pair) "\n")))
        renames)))

;;; --- Extension-based operations ---
(section "Extension Operations")

(for-each
  (lambda (name)
    (display (string-append "  " name
                            " -> .bak: " (replace-extension name ".bak")
                            "\n")))
  '("report.txt" "data.csv" "script.ss" "archive.tar.gz"))

;;; --- Actual rename demo with temp files ---
(section "Actual Rename (temp files)")

;; Create a temp directory and some files
(let ([tmpdir (string-append "/tmp/hafod-rename-demo-" (number->string (pid)))])
  (guard (e [#t #f]) (create-directory tmpdir))

  ;; Create sample files
  (let ([names '("report-2025.txt" "report-2026.txt" "data-2025.csv"
                 "data-2026.csv" "notes.md")])
    (for-each
      (lambda (name)
        (let ([path (string-append tmpdir "/" name)])
          (let ([p (open-output-file path)])
            (display "sample content\n" p)
            (close p))))
      names)

    ;; Show files before rename
    (display "Before:\n")
    (for-each
      (lambda (f) (display (string-append "  " f "\n")))
      (directory-files tmpdir))

    ;; Rename: 2025 -> 2024 in filenames
    (let ([files (directory-files tmpdir)])
      (for-each
        (lambda (name)
          (let ([m (regexp-search (rx "2025") name)])
            (when m
              (let* ([new-name (regexp-substitute/global #f (rx "2025") name
                                 'pre "2024" 'post)]
                     [old-path (string-append tmpdir "/" name)]
                     [new-path (string-append tmpdir "/" new-name)])
                (rename-file old-path new-path)
                (display (string-append "  Renamed: " name " -> " new-name "\n"))))))
        files))

    ;; Show files after rename
    (display "After:\n")
    (for-each
      (lambda (f) (display (string-append "  " f "\n")))
      (directory-files tmpdir))

    ;; Clean up
    (for-each
      (lambda (f) (delete-file (string-append tmpdir "/" f)))
      (directory-files tmpdir))
    (delete-directory tmpdir)))

(display "\nDone.\n")
