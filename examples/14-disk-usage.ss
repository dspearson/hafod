;;; 14-disk-usage.ss -- Disk usage summary via pipelines
;;;
;;; Demonstrates: run/string, run/strings, pipe, field-splitter,
;;;               infix-splitter, sort, regexp-search, rx,
;;;               with-cwd, glob
;;;
;;; Use Unix commands via hafod's process notation to build
;;; a disk usage report -- the kind of practical script that
;;; scsh was designed for.
;;; Source: "How to Use Scsh" article; common sysadmin scripts.

(import (hafod))

(define (section title)
  (newline)
  (display (string-append "=== " title " ===\n")))

(define (pad-right str width)
  (if (>= (string-length str) width) str
      (string-append str (make-string (- width (string-length str)) #\space))))

(define (pad-left str width)
  (if (>= (string-length str) width) str
      (string-append (make-string (- width (string-length str)) #\space) str)))

;;; --- Overall disk usage ---
(section "Filesystem Usage")

(let ([lines (run/strings (df -h))])
  ;; Print header and data
  (when (pair? lines)
    (display (string-append (car lines) "\n"))
    (display (make-string 70 #\-))
    (newline)
    (for-each
      (lambda (line)
        ;; Only show real filesystems (skip tmpfs, devtmpfs, etc.)
        (let ([fields ((field-splitter) line)])
          (when (and (pair? fields)
                     (or (regexp-search (rx bos "/dev/") (car fields))
                         (regexp-search (rx bos "//") (car fields))))
            (display (string-append line "\n")))))
      (cdr lines))))

;;; --- Directory sizes ---
(section "Largest Directories in /tmp")

(let ([lines (guard (e [#t '()])
               (run/strings (pipe (du -h --max-depth=1 /tmp)
                                  (sort -h -r)
                                  (head -15))))])
  (for-each
    (lambda (line)
      (display (string-append "  " line "\n")))
    lines))

;;; --- File type breakdown ---
(section "File Types in Current Directory")

;; Count files by extension
(let ([ext-counts (make-hashtable string-hash string=?)])
  (guard (e [#t #f])
    (let ([files (directory-files (cwd))])
      (for-each
        (lambda (name)
          (let ([ext (file-name-extension name)])
            (let ([key (if (string=? ext "") "(no ext)" ext)])
              (hashtable-set! ext-counts key
                (+ 1 (hashtable-ref ext-counts key 0))))))
        files)))

  (let* ([pairs (let-values ([(keys vals) (hashtable-entries ext-counts)])
                  (map cons (vector->list keys) (vector->list vals)))]
         [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) pairs)])
    (when (pair? sorted)
      (display (string-append (pad-right "Extension" 15) "Count\n"))
      (display (make-string 25 #\-))
      (newline)
      (for-each
        (lambda (p)
          (display (string-append (pad-right (car p) 15)
                                  (number->string (cdr p)) "\n")))
        sorted))))

;;; --- File count by size range ---
(section "Files by Size Range (current directory)")

(let ([ranges '((0 . 1024)          ; 0-1K
                (1024 . 10240)      ; 1K-10K
                (10240 . 102400)    ; 10K-100K
                (102400 . 1048576)  ; 100K-1M
                (1048576 . #f))]    ; 1M+
      [labels '("0 - 1K" "1K - 10K" "10K - 100K" "100K - 1M" "1M+")]
      [counts (make-vector 5 0)])
  (guard (e [#t #f])
    (let ([files (directory-files (cwd))])
      (for-each
        (lambda (name)
          (guard (e [#t #f])
            (when (file-regular? name)
              (let ([size (file:size name)])
                (let loop ([i 0] [rs ranges])
                  (when (pair? rs)
                    (let ([lo (caar rs)]
                          [hi (cdar rs)])
                      (if (and (>= size lo)
                               (or (not hi) (< size hi)))
                          (vector-set! counts i (+ 1 (vector-ref counts i)))
                          (loop (+ i 1) (cdr rs))))))))))
        files)))

  (display (string-append (pad-right "Range" 15) "Files\n"))
  (display (make-string 25 #\-))
  (newline)
  (let loop ([i 0] [ls labels])
    (when (pair? ls)
      (display (string-append (pad-right (car ls) 15)
                              (number->string (vector-ref counts i)) "\n"))
      (loop (+ i 1) (cdr ls)))))

;;; --- Inode usage ---
(section "Inode Usage")

(let ([lines (guard (e [#t '()])
               (run/strings (df -i)))])
  (when (pair? lines)
    (display (string-append (car lines) "\n"))
    (for-each
      (lambda (line)
        (when (regexp-search (rx bos "/dev/") line)
          (display (string-append line "\n"))))
      (cdr lines))))

(display "\nDone.\n")
