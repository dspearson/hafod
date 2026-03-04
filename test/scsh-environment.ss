;;; Ported from scsh/test/env-test-code.scm + scsh/test/env-test-add.scm
;;; Tests for environment variable operations (section 3.4 of scsh manual)
;;; Original authors: Christoph Hetz et al. Ported to hafod test runner.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod))

(test-begin "scsh-environment")

;; ========================================================================
;; Helper functions (ported from env-test-code.scm)
;; ========================================================================

;; Returns #t if liste is a list containing only strings
(define (string-list? liste)
  (and (list? liste)
       (let loop ((l liste))
         (or (null? l)
             (and (string? (car l))
                  (loop (cdr l)))))))

;; every: SRFI-1 predicate (not in hafod exports)
(define (every pred lst)
  (let loop ((l lst))
    (or (null? l)
        (and (pred (car l))
             (loop (cdr l))))))

;; Deletes equal-to-this once in list, if present
(define (delete-once equal-to-this lst)
  (if (null? lst)
      '()
      (if (equal? (car lst) equal-to-this)
          (cdr lst)
          (cons (car lst) (delete-once equal-to-this (cdr lst))))))

;; Compares two lists irrespective of order but respecting multiplicity
(define (list-equal? list1 list2)
  (if (null? list1)
      (null? list2)
      (if (member (car list1) list2)
          (list-equal? (cdr list1)
                       (delete-once (car list1) list2))
          #f)))

;; Updates the environment env-alist via env-alist-delta
(define (update-env env-alist env-alist-delta)
  (if (null? env-alist-delta)
      env-alist
      (update-env (alist-update (car (car env-alist-delta))
                                (cdr (car env-alist-delta))
                                env-alist)
                  (cdr env-alist-delta))))

;; Compares old-env-alist with actual environment (env->alist)
(define (equal-to-current-env? old-env-alist)
  (list-equal? old-env-alist (env->alist)))

;; split-colon: helper to split colon-separated string into list
(define (split-colon str)
  (let loop ((i 0) (start 0) (acc '()))
    (cond
      ((= i (string-length str))
       (reverse (cons (substring str start i) acc)))
      ((char=? (string-ref str i) #\:)
       (loop (+ i 1) (+ i 1)
             (cons (substring str start i) acc)))
      (else (loop (+ i 1) start acc)))))

;; join-colon: join list of strings with colons
(define (join-colon lst)
  (if (null? lst) ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest) acc
            (loop (cdr rest)
                  (string-append acc ":" (car rest)))))))

;; ========================================================================
;; Test definitions (ported from env-test-add.scm)
;; ========================================================================

(define alist
  '(("Test-EDITOR" . "MyEditor")
    ("Test-TERM"   . "SuperScsh")
    ("Test-EDITOR" . "HerEditor")))

;; For add-before/add-after tests: hafod operates on colon-separated strings
;; (not lists as in scsh). We use a colon-joined version.
(define number-str "Eins:Zwei:Vier:Eins:Zwei:Vier")

;; --- setenv / getenv ---
(test-equal "setenv"
  "Hello!"
  (begin (setenv "Test-Var" "Hello!")
         (getenv "Test-Var")))

;; --- env->alist ---
(test-assert "env->alist"
  (begin
    (setenv "env->alist-test-var" "env->alist-test-val")
    (let ((al (env->alist)))
      (and (every (lambda (pair)
                    (and (pair? pair)
                         (not (list? pair))
                         (string? (car pair))
                         (or (string? (cdr pair))
                             (string-list? (cdr pair)))))
                  al)
           (equal? (cdr (assoc "env->alist-test-var" al))
                   "env->alist-test-val")))))

;; --- alist-delete ---
(test-assert "alist-delete"
  (not (member "Test-EDITOR" (map car (alist-delete "Test-EDITOR" alist)))))

;; --- alist-update ---
(test-assert "alist-update"
  (letrec ((check-update
            (lambda (al)
              (if (null? al)
                  #f
                  (if (equal? "Test-EDITOR" (caar al))
                      (if (equal? "HisEditor" (cdar al))
                          (not (member "Test-EDITOR" (map car (cdr al))))
                          #f)
                      (check-update (cdr al)))))))
    (check-update (alist-update "Test-EDITOR" "HisEditor" alist))))

;; --- alist-compress ---
(test-assert "alist-compress"
  (letrec ((check-compress
            (lambda (al known-vars)
              (if (null? al)
                  #t
                  (if (member (caar al) known-vars)
                      #f
                      (check-compress (cdr al)
                                      (cons (caar al) known-vars)))))))
    (check-compress (alist-compress alist) '())))

;; --- with-env* ---
;; Three sub-tests (simple thunk, non-local exit, reinvoking)
(test-assert "with-env*"
  (let* ((env-alist-delta alist)
         (old-env-alist (env->alist))
         (expected-process-env (update-env (env->alist) env-alist-delta))
         (env-var-test (lambda ()
                         (equal-to-current-env? expected-process-env)))
         ;; Store places for continuations
         (non-local-exit-cc #f)
         (reinvoking-cc #f)
         (thunk-finished-cc #f)
         ;; Thunks for testing
         (thunk-local-return (lambda () (env-var-test)))
         (cc-thunk-non-local-return
          (lambda ()
            (non-local-exit-cc (env-var-test))
            #f))
         (cc-reinvoking-thunk
          (lambda ()
            (call-with-current-continuation
             (lambda (k)
               (set! reinvoking-cc k)
               (non-local-exit-cc #f)))
            (thunk-finished-cc (env-var-test))
            #f))
         ;; Procedure to perform tests
         (run-test
          (lambda (thunk)
            (and (thunk)
                 (equal-to-current-env? old-env-alist)))))

    (and (run-test (lambda ()
                     (with-env* env-alist-delta thunk-local-return)))
         (run-test (lambda ()
                     (call-with-current-continuation
                      (lambda (k)
                        (set! non-local-exit-cc k)
                        (with-env* env-alist-delta
                                   cc-thunk-non-local-return)))))
         (run-test (lambda ()
                     (call-with-current-continuation
                      (lambda (finished)
                        (set! thunk-finished-cc finished)
                        (call-with-current-continuation
                         (lambda (k)
                           (set! non-local-exit-cc k)
                           (with-env* env-alist-delta cc-reinvoking-thunk)))
                        (reinvoking-cc #f))))))))

;; --- with-total-env* ---
(test-assert "with-total-env*"
  (let* ((env-alist-input alist)
         (old-env-alist (env->alist))
         (expected-process-env env-alist-input)
         (env-var-test (lambda ()
                         (equal-to-current-env? expected-process-env)))
         (non-local-exit-cc #f)
         (reinvoking-cc #f)
         (thunk-finished-cc #f)
         (thunk-local-return (lambda () (env-var-test)))
         (cc-thunk-non-local-return
          (lambda ()
            (non-local-exit-cc (env-var-test))
            #f))
         (cc-reinvoking-thunk
          (lambda ()
            (call-with-current-continuation
             (lambda (k)
               (set! reinvoking-cc k)
               (non-local-exit-cc #f)))
            (thunk-finished-cc (env-var-test))
            #f))
         (run-test
          (lambda (thunk)
            (and (thunk)
                 (equal-to-current-env? old-env-alist)))))

    (and (run-test (lambda ()
                     (with-total-env* env-alist-input thunk-local-return)))
         (run-test (lambda ()
                     (call-with-current-continuation
                      (lambda (k)
                        (set! non-local-exit-cc k)
                        (with-total-env* env-alist-input
                                         cc-thunk-non-local-return)))))
         (run-test (lambda ()
                     (call-with-current-continuation
                      (lambda (finished)
                        (set! thunk-finished-cc finished)
                        (call-with-current-continuation
                         (lambda (k)
                           (set! non-local-exit-cc k)
                           (with-total-env* env-alist-input cc-reinvoking-thunk)))
                        (reinvoking-cc #f))))))))

;; --- home-directory ---
(test-assert "home-directory"
  (string? (home-directory)))

;; --- exec-path-list ---
;; scsh uses (thread-fluid exec-path-list); hafod uses (exec-path-list) parameter
(test-assert "exec-path-list"
  (let ((epl (exec-path-list)))
    (and (list? epl)
         (string-list? epl))))

;; --- add-before (infix: insert "Drei" before "Vier") ---
(test-assert "add-before-infix"
  (let* ((result (add-before "Drei" "Vier" number-str))
         (parts (split-colon result)))
    ;; "Drei" should appear immediately before first "Vier"
    (let loop ((p parts))
      (cond
        ((null? p) #f)
        ((equal? (car p) "Drei")
         (and (pair? (cdr p))
              (equal? (cadr p) "Vier")))
        (else (loop (cdr p)))))))

;; --- add-before (suffix: ref not present, element appended) ---
(test-assert "add-before-suffix"
  (let* ((result (add-before "Fuenf" "Sechs" number-str))
         (parts (split-colon result)))
    ;; "Fuenf" should be present (appended since "Sechs" not found)
    (member "Fuenf" parts)))

;; --- add-after (infix: insert "Drei" after "Zwei") ---
(test-assert "add-after-infix"
  (let* ((result (add-after "Drei" "Zwei" number-str))
         (parts (split-colon result)))
    ;; Find "Zwei" then "Drei" should follow
    (let loop ((p parts))
      (cond
        ((null? p) #f)
        ((equal? (car p) "Zwei")
         (and (pair? (cdr p))
              (equal? (cadr p) "Drei")))
        (else (loop (cdr p)))))))

;; --- add-after (prefix: ref not present, element appended) ---
(test-assert "add-after-prefix"
  (let* ((result (add-after "Null" "Nonexistent" number-str))
         (parts (split-colon result)))
    ;; "Null" should be present (appended since ref not found)
    (member "Null" parts)))

;; --- add-after preserves order ---
(test-assert "add-after-order"
  (let* ((result (add-after "NEW" "Eins" "Eins:Zwei:Drei"))
         (parts (split-colon result)))
    (equal? parts '("Eins" "NEW" "Zwei" "Drei"))))

(test-end)
