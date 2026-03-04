;;; 02-list-executables.ss -- List all executables on PATH
;;;
;;; Demonstrates: getenv, infix-splitter, with-cwd, directory-files,
;;;               file-executable?, for-each, filter
;;;
;;; This is the classic scsh example from the scsh Wikipedia page and
;;; Olin Shivers' papers -- list all executables available on your PATH.
;;; Source: https://en.wikipedia.org/wiki/Scsh

(import (hafod))

(define (executables dir)
  ;; List executable files in a directory.
  ;; Guard against unreadable/missing directories.
  (guard (e [#t '()])
    (with-cwd dir
      (filter file-executable?
              (directory-files dir #t)))))

(define (writeln x)
  (display x)
  (newline))

;; Split PATH on colons and collect executables from each directory.
(let* ([path-str (or (getenv "PATH") "/usr/bin:/bin")]
       [dirs ((infix-splitter ":") path-str)]
       [all-exes (apply append (map executables dirs))]
       [sorted (sort string<? all-exes)]
       ;; Remove duplicates
       [unique (let loop ([lst sorted] [prev ""] [acc '()])
                 (cond
                   [(null? lst) (reverse acc)]
                   [(string=? (car lst) prev)
                    (loop (cdr lst) prev acc)]
                   [else
                    (loop (cdr lst) (car lst) (cons (car lst) acc))]))])
  (display (string-append "Found " (number->string (length unique))
                          " unique executables on PATH\n\n"))
  ;; Print first 30 as a sample
  (display "First 30:\n")
  (for-each writeln (if (> (length unique) 30)
                        (let loop ([n 30] [lst unique] [acc '()])
                          (if (or (zero? n) (null? lst))
                              (reverse acc)
                              (loop (- n 1) (cdr lst) (cons (car lst) acc))))
                        unique))
  (when (> (length unique) 30)
    (display (string-append "... and "
                            (number->string (- (length unique) 30))
                            " more\n"))))
