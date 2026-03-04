;;; 04-find-large-files.ss -- Find large files using glob and file-info
;;;
;;; Demonstrates: directory-files, file-info, file-info:size, file-info:type,
;;;               file-info:mtime, file-info-regular?, file-directory?,
;;;               date, format-date, with-cwd, file-name-as-directory
;;;
;;; A practical script that finds the largest files in a directory tree.
;;; Source: common scsh filesystem traversal patterns.

(import (hafod))

(define *size-threshold* (* 10 1024))  ; 10 KB

(define (format-size bytes)
  (cond
    [(>= bytes (* 1024 1024 1024))
     (string-append (number->string (quotient bytes (* 1024 1024 1024))) " GB")]
    [(>= bytes (* 1024 1024))
     (string-append (number->string (quotient bytes (* 1024 1024))) " MB")]
    [(>= bytes 1024)
     (string-append (number->string (quotient bytes 1024)) " KB")]
    [else
     (string-append (number->string bytes) " B")]))

(define (pad-right str width)
  (if (>= (string-length str) width)
      str
      (string-append str (make-string (- width (string-length str)) #\space))))

(define (collect-files dir)
  ;; Collect regular files above threshold in dir (non-recursive, safe)
  (guard (e [#t '()])
    (let ([entries (directory-files dir)])
      (let loop ([es entries] [acc '()])
        (if (null? es)
            acc
            (let ([name (car es)])
              (let ([path (string-append (file-name-as-directory dir) name)])
                (loop (cdr es)
                      (guard (e [#t acc])
                        (let ([info (file-info path)])
                          (if (and (file-info-regular? info)
                                   (>= (file-info:size info) *size-threshold*))
                              (cons (list path (file-info:size info) (file-info:mtime info))
                                    acc)
                              acc)))))))))))

;; Scan directories that exist and are readable
(let* ([candidates (list "/tmp" (home-directory) (cwd))]
       [dirs-to-scan (filter (lambda (d)
                               (guard (e [#t #f])
                                 (file-directory? d)))
                             candidates)]
       [all-files (apply append (map collect-files dirs-to-scan))]
       [sorted (sort (lambda (a b) (> (cadr a) (cadr b))) all-files)])

  (display (string-append "Files larger than " (format-size *size-threshold*) " in:\n"))
  (for-each (lambda (d) (display (string-append "  " d "\n"))) dirs-to-scan)
  (newline)

  (display (string-append (pad-right "SIZE" 12)
                          (pad-right "MODIFIED" 22)
                          "PATH\n"))
  (display (make-string 70 #\-))
  (newline)

  (let ([shown (if (> (length sorted) 20)
                   (let loop ([n 20] [lst sorted] [acc '()])
                     (if (or (zero? n) (null? lst))
                         (reverse acc)
                         (loop (- n 1) (cdr lst) (cons (car lst) acc))))
                   sorted)])
    (for-each
      (lambda (entry)
        (let ([path (car entry)]
              [size (cadr entry)]
              [mtime (caddr entry)])
          (display (pad-right (format-size size) 12))
          (display (pad-right (format-date "~Y-~m-~d ~H:~M:~S" (date mtime)) 22))
          (display path)
          (newline)))
      shown))

  (display (string-append "\nTotal: " (number->string (length sorted))
                          " files found"
                          (if (> (length sorted) 20)
                              " (showing top 20)"
                              "")
                          "\n")))
