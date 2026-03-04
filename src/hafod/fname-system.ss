;;; (hafod fname-system) -- System-dependent filename utilities
;;; Ported from scsh/scheme/fname-system.scm
;;; Copyright (c) 1992 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod fname-system)
  (export resolve-tilde-file-name resolve-file-name expand-file-name
          absolute-file-name substitute-env-vars home-directory)
  (import (hafod internal base)
          (hafod compat)
          (hafod fname)
          (hafod internal strings)
          (only (hafod user-group) home-directory user-info)
          (only (hafod process-state) cwd)
          (only (hafod environment) getenv)
          (only (hafod posix) passwd-info-dir))

  (define (resolve-tilde-file-name fname)
    (let ((len (string-length fname)))
      (if (and (> len 0) (char=? #\~ (string-ref fname 0)))
          (let ((tilde->homedir
                 (lambda (end)
                   (if (= end 1)
                       (home-directory)
                       (passwd-info-dir
                        (user-info (substring fname 1 end)))))))
            (cond ((string-index fname #\/ 1) =>
                   (lambda (slash)
                     (string-append (tilde->homedir slash) "/"
                                    (substring fname (+ slash 1) len))))
                  (else (tilde->homedir len))))
          fname)))

  (define (resolve-file-name fname . maybe-root)
    (let* ((root (ensure-file-name-is-nondirectory (:optional maybe-root ".")))
           (fname (ensure-file-name-is-nondirectory fname)))
      (if (zero? (string-length fname))
          "/"
          (let ((c (string-ref fname 0)))
            (cond ((char=? #\/ c) fname)
                  ((char=? #\~ c) (resolve-tilde-file-name fname))
                  (else (string-append (file-name-as-directory root) fname)))))))

  (define (expand-file-name fname . maybe-dir)
    (simplify-file-name (apply resolve-file-name fname maybe-dir)))

  (define (absolute-file-name fname . maybe-root)
    (let ((fname (ensure-file-name-is-nondirectory fname)))
      (if (zero? (string-length fname))
          "/"
          (simplify-file-name
           (if (char=? #\/ (string-ref fname 0))
               fname
               (let ((root (:optional maybe-root (cwd))))
                 (string-append (file-name-as-directory root) fname)))))))

  ;; Substitute environment variables: $VAR and ${VAR}
  (define (substitute-env-vars str)
    (let lp ((ans '()) (s str))
      (let ((len (string-length s)))
        (cond
         ((zero? len) (apply string-append (reverse! ans)))
         ((string-index s #\$) =>
          (lambda (i)
            (let ((ans (cons (substring s 0 i) ans))
                  (s (substring s (+ i 1) len))
                  (len (- len (+ i 1))))
              (if (zero? len)
                  (lp ans "")
                  (let ((next-char (string-ref s 0)))
                    (cond ((char=? #\{ next-char)
                           (cond ((string-index s #\}) =>
                                  (lambda (i)
                                    (lp (cons (or (getenv (substring s 1 i)) "")
                                             ans)
                                        (substring s (+ i 1) len))))
                                 (else (error 'substitute-env-vars
                                              "Unbalanced ${ delimiter in string" s))))
                          (else
                           (let ((i (or (string-index s #\/) len)))
                             (lp (cons (or (getenv (substring s 0 i)) "")
                                       ans)
                                 (substring s i len))))))))))
         (else (lp (cons s ans) ""))))))

  ) ; end library
