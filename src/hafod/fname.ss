;;; (hafod fname) -- Filename path manipulation utilities
;;; Ported from scsh/scheme/fname.scm
;;; Copyright (c) 1992 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod fname)
  (export file-name-directory? file-name-non-directory?
          file-name-as-directory directory-as-file-name
          ensure-file-name-is-directory ensure-file-name-is-nondirectory
          file-name-absolute?
          file-name-directory file-name-nondirectory
          split-file-name path-list->file-name
          parse-file-name
          file-name-extension-index file-name-sans-extension
          file-name-extension replace-extension
          simplify-file-name)
  (import (hafod internal base)
          (hafod compat)
          (hafod internal strings))

  (define (file-name-directory? fname)
    (or (string=? fname "")
        (char=? #\/ (string-ref fname (- (string-length fname) 1)))))

  (define (file-name-non-directory? fname)
    (or (string=? fname "")
        (not (char=? #\/ (string-ref fname (- (string-length fname) 1))))))

  (define (file-name-as-directory fname)
    (if (string=? fname ".")
        ""
        (let ((len (string-length fname)))
          (if (and (> len 0)
                   (char=? #\/ (string-ref fname (- len 1))))
              fname
              (string-append fname "/")))))

  ;; Return index of last non-slash char, or #f if all slashes.
  (define (last-non-slash str)
    (let lp ((i (- (string-length str) 1)))
      (and (>= i 0)
           (if (char=? #\/ (string-ref str i))
               (lp (- i 1))
               i))))

  (define (directory-as-file-name fname)
    (let ((len (string-length fname)))
      (if (zero? len)
          "."
          (cond ((last-non-slash fname) =>
                 (lambda (i)
                   (if (= i (- len 1))
                       fname
                       (substring fname 0 (+ i 1)))))
                ;; Solid slashes -- invoke weird Posix rule.
                (else (if (= len 2) "//" "/"))))))

  (define (ensure-file-name-is-directory fname)
    (if (string=? fname "")
        ""
        (file-name-as-directory fname)))

  (define (ensure-file-name-is-nondirectory fname)
    (if (string=? fname "")
        ""
        (directory-as-file-name fname)))

  (define (file-name-absolute? fname)
    (or (= (string-length fname) 0)
        (char=? #\/ (string-ref fname 0))
        (char=? #\~ (string-ref fname 0))))

  ;; Returns FNAME's directory component in *directory form.*
  (define (file-name-directory fname)
    (cond ((string-index-right fname #\/) =>
           (lambda (rslash)
             (if (last-non-slash fname)
                 (substring fname 0 (+ 1 rslash))
                 "")))
          (else "")))

  (define (file-name-nondirectory fname)
    (cond ((string-index-right fname #\/) =>
           (lambda (rslash)
             (if (last-non-slash fname)
                 (substring fname (+ 1 rslash) (string-length fname))
                 fname)))
          (else fname)))

  (define (split-file-name fname)
    (let* ((fname (ensure-file-name-is-nondirectory fname))
           (len (string-length fname)))
      (let split ((start 0))
        (cond ((>= start len) '())
              ((string-index fname #\/ start) =>
               (lambda (slash)
                 (cons (substring fname start slash)
                       (split (+ slash 1)))))
              (else (list (substring fname start len)))))))

  (define (path-list->file-name pathlist . maybe-dir)
    (let ((root (ensure-file-name-is-nondirectory (:optional maybe-dir ".")))
          (w/slashes (if (pair? pathlist)
                         (let insert-slashes ((pathlist pathlist))
                           (let ((elt (car pathlist))
                                 (pathlist (cdr pathlist)))
                             (cons elt (if (pair? pathlist)
                                           (cons "/" (insert-slashes pathlist))
                                           '()))))
                         '(""))))
      (apply string-append
             (if (and (pair? pathlist)
                      (string=? "" (car pathlist)))
                 (if (null? (cdr pathlist))
                     '("/")
                     w/slashes)
                 (cons (file-name-as-directory root) w/slashes)))))

  (define (parse-file-name fname)
    (let ((nd (file-name-nondirectory fname)))
      (values (file-name-directory fname)
              (file-name-sans-extension nd)
              (file-name-extension nd))))

  ;; Return the index of the . separating the extension from the rest.
  ;; If no extension, returns (string-length fname).
  ;; Dot-files like .login are NOT considered extensions.
  (define (file-name-extension-index fname)
    (let ((dot (string-index-right fname #\.))
          (slash (string-index-right fname #\/)))
      (if (and dot
               (> dot 0)
               (if slash (> dot slash) #t)
               (not (char=? #\/ (string-ref fname (- dot 1)))))
          dot
          (string-length fname))))

  (define (file-name-sans-extension fname)
    (substring fname 0 (file-name-extension-index fname)))

  (define (file-name-extension fname)
    (substring fname (file-name-extension-index fname)
               (string-length fname)))

  (define (replace-extension fname ext)
    (string-append (file-name-sans-extension fname) ext))

  ;; Simplify file name: remove leading multiple slashes (>2 -> 1, keep //),
  ;; remove internal dots and double slashes, keep .. and trailing dots.
  (define (simplify-file-name fname)
    (receive (slashes fname)
             (let ((len (string-length fname)))
               (if (and (> len 0) (char=? #\/ (string-ref fname 0)))
                   (let ((j (let lp ((i 1))
                              (if (and (< i len)
                                       (char=? (string-ref fname i) #\/))
                                  (lp (+ i 1))
                                  i))))
                     (if (< j 3)
                         (values (substring fname 0 j)
                                 (substring fname j len))
                         (values "/" (substring fname (- j 1) len))))
                   (values "" fname)))
      (let* ((path-list (split-file-name fname))
             (ans (if (pair? path-list)
                      (reverse (let lp ((path-list path-list)
                                        (ans (list slashes)))
                                 (let ((elt (car path-list))
                                       (path-list (cdr path-list)))
                                   (if (pair? path-list)
                                       (lp path-list
                                           (if (or (string=? "." elt)
                                                   (string=? "" elt))
                                               ans
                                               `("/" ,elt ,@ans)))
                                       (cons elt ans)))))
                      (list slashes))))
        (apply string-append ans))))

  ) ; end library
