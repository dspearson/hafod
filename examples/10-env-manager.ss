;;; 10-env-manager.ss -- Environment variable manipulation
;;;
;;; Demonstrates: getenv, setenv, env->alist, alist->env, with-env,
;;;               with-total-env, alist-update, alist-delete,
;;;               alist-compress, add-before, add-after,
;;;               align-env!, run/string
;;;
;;; Source: scsh manual environment section.

(import (hafod))

(define (section title)
  (newline)
  (display (string-append "=== " title " ===\n")))

;;; --- Basic get/set ---
(section "Basic Environment Operations")

(display (string-append "HOME = " (or (getenv "HOME") "(not set)") "\n"))
(display (string-append "USER = " (or (getenv "USER") "(not set)") "\n"))
(display (string-append "SHELL = " (or (getenv "SHELL") "(not set)") "\n"))

;; Set a variable and read it back
(setenv "HAFOD_DEMO" "hello-world")
(display (string-append "HAFOD_DEMO = " (getenv "HAFOD_DEMO") "\n"))

;; Delete it
(setenv "HAFOD_DEMO" #f)
(display (string-append "After delete: " (or (getenv "HAFOD_DEMO") "(not set)") "\n"))

;;; --- Scoped environment with with-env ---
(section "Scoped Environment (with-env)")

(display (string-append "Before: HAFOD_X = " (or (getenv "HAFOD_X") "(not set)") "\n"))

(with-env '(("HAFOD_X" . "inside-scope") ("HAFOD_Y" . "also-scoped"))
  (display (string-append "Inside: HAFOD_X = " (getenv "HAFOD_X") "\n"))
  (display (string-append "Inside: HAFOD_Y = " (getenv "HAFOD_Y") "\n"))
  ;; Show that subprocesses see the scoped env
  (let ([result (run/string (printenv HAFOD_X))])
    (display (string-append "Subprocess sees: " result))))

(display (string-append "After: HAFOD_X = " (or (getenv "HAFOD_X") "(not set)") "\n"))

;;; --- Total environment replacement ---
(section "Total Environment Replacement")

(let ([saved (env->alist)])
  ;; Temporarily replace the entire environment
  (with-total-env* '(("ONLY_VAR" . "I-am-alone") ("PATH" . "/bin:/usr/bin"))
    (lambda ()
      (let ([env (env->alist)])
        (display (string-append "Environment size: "
                                (number->string (length env)) " vars\n"))
        (for-each
          (lambda (pair)
            (display (string-append "  " (car pair) "=" (cdr pair) "\n")))
          env))))
  ;; Verify restoration
  (display (string-append "Restored HOME: " (or (getenv "HOME") "MISSING!") "\n")))

;;; --- Alist operations ---
(section "Alist Operations")

(let ([env '(("A" . "1") ("B" . "2") ("C" . "3") ("A" . "old"))])
  (display "Original: ")
  (for-each (lambda (p) (display (string-append (car p) "=" (cdr p) " "))) env)
  (newline)

  (let ([updated (alist-update "B" "NEW" env)])
    (display "After alist-update B=NEW: ")
    (for-each (lambda (p) (display (string-append (car p) "=" (cdr p) " "))) updated)
    (newline))

  (let ([deleted (alist-delete "A" env)])
    (display "After alist-delete A: ")
    (for-each (lambda (p) (display (string-append (car p) "=" (cdr p) " "))) deleted)
    (newline))

  (let ([compressed (alist-compress env)])
    (display "After alist-compress: ")
    (for-each (lambda (p) (display (string-append (car p) "=" (cdr p) " "))) compressed)
    (newline)))

;;; --- PATH manipulation ---
(section "PATH Manipulation")

(let ([path (or (getenv "PATH") "/usr/bin:/bin")])
  (display (string-append "Current PATH has "
                          (number->string (length ((infix-splitter ":") path)))
                          " entries\n"))

  ;; add-before: insert /opt/bin before /usr/bin
  (let ([modified (add-before "/opt/bin" "/usr/bin" path)])
    (display "After add-before /opt/bin /usr/bin:\n")
    (let ([parts ((infix-splitter ":") modified)])
      (for-each (lambda (p) (display (string-append "  " p "\n")))
                (if (> (length parts) 5)
                    (append (let loop ([n 5] [l parts] [a '()])
                              (if (or (zero? n) (null? l)) (reverse a)
                                  (loop (- n 1) (cdr l) (cons (car l) a))))
                            '("..."))
                    parts)))))

;;; --- Environment count ---
(section "Environment Summary")

(let ([env (env->alist)])
  (display (string-append "Total environment variables: "
                          (number->string (length env)) "\n"))
  ;; Show longest variable names
  (let ([sorted (sort (lambda (a b) (> (string-length (car a))
                                       (string-length (car b))))
                      env)])
    (display "Longest variable names:\n")
    (for-each
      (lambda (p)
        (display (string-append "  " (car p) " ("
                                (number->string (string-length (cdr p)))
                                " chars)\n")))
      (if (> (length sorted) 5)
          (let loop ([n 5] [l sorted] [a '()])
            (if (or (zero? n) (null? l)) (reverse a)
                (loop (- n 1) (cdr l) (cons (car l) a))))
          sorted))))

(display "\nDone.\n")
