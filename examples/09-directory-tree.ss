;;; 09-directory-tree.ss -- Recursive directory listing with file types
;;;
;;; Demonstrates: directory-files, file-info, file-info:type, file-info:size,
;;;               file-info:mode, file-directory?, file-symlink?,
;;;               read-symlink, with-cwd, file-name-as-directory,
;;;               glob, format-date, date
;;;
;;; Display a tree-like directory listing, similar to the `tree` command.
;;; Source: common scsh filesystem traversal patterns.

(import (hafod))

(define *max-depth* 3)
(define *max-files-per-dir* 20)

(define (type-char info)
  (case (file-info:type info)
    [(directory) "d"]
    [(symlink)   "l"]
    [(fifo)      "p"]
    [(socket)    "s"]
    [(block-special)  "b"]
    [(char-special)   "c"]
    [else        "-"]))

(define (mode->string mode)
  (define (bit m c) (if (zero? (bitwise-and mode m)) "-" c))
  (string-append (bit #o400 "r") (bit #o200 "w") (bit #o100 "x")
                 (bit #o040 "r") (bit #o020 "w") (bit #o010 "x")
                 (bit #o004 "r") (bit #o002 "w") (bit #o001 "x")))

(define (format-size bytes)
  (cond
    [(>= bytes (* 1024 1024))
     (string-append (number->string (quotient bytes (* 1024 1024))) "M")]
    [(>= bytes 1024)
     (string-append (number->string (quotient bytes 1024)) "K")]
    [else (number->string bytes)]))

(define (pad-left str width)
  (if (>= (string-length str) width) str
      (string-append (make-string (- width (string-length str)) #\space) str)))

(define (walk-directory dir depth)
  (when (< depth *max-depth*)
    (guard (e [#t (display (string-append (make-string (* depth 2) #\space)
                                          "[unreadable: " dir "]\n"))])
      (let* ([entries (directory-files dir)]
             [truncated (> (length entries) *max-files-per-dir*)]
             [shown (if truncated
                        (let loop ([n *max-files-per-dir*] [lst entries] [acc '()])
                          (if (or (zero? n) (null? lst))
                              (reverse acc)
                              (loop (- n 1) (cdr lst) (cons (car lst) acc))))
                        entries)])
        (for-each
          (lambda (name)
            (let* ([path (string-append (file-name-as-directory dir) name)]
                   [indent (make-string (* depth 2) #\space)])
              (guard (e [#t (display (string-append indent "? " name " [error]\n"))])
                (let* ([info (file-info path #f)]  ; don't chase symlinks
                       [tc (type-char info)]
                       [mode (mode->string (bitwise-and (file-info:mode info) #o777))]
                       [size (pad-left (format-size (file-info:size info)) 6)])
                  (display (string-append indent tc mode " " size " " name))
                  ;; Show symlink target
                  (when (eq? (file-info:type info) 'symlink)
                    (guard (e [#t #f])
                      (display (string-append " -> " (read-symlink path)))))
                  (newline)
                  ;; Recurse into directories
                  (when (eq? (file-info:type info) 'directory)
                    (walk-directory path (+ depth 1)))))))
          shown)
        (when truncated
          (display (string-append (make-string (* depth 2) #\space)
                                  "... and " (number->string (- (length entries) *max-files-per-dir*))
                                  " more entries\n")))))))

;; Walk a few safe directories
(display "=== Directory Tree ===\n\n")

;; Walk /usr/share/doc (limited depth) or /tmp
(let ([target (cond
                [(file-directory? "/usr/share/doc") "/usr/share/doc"]
                [(file-directory? "/tmp") "/tmp"]
                [else (cwd)])])
  (display (string-append target "/\n"))
  (walk-directory target 0))

(display "\nDone.\n")
