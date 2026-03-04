(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod environment) (hafod posix))

(test-begin "environment")

;; getenv for existing variable
(test-assert "getenv HOME returns string"
  (let ([home (getenv "HOME")])
    (and (string? home) (> (string-length home) 0))))

;; getenv for nonexistent variable
(test-equal "getenv nonexistent returns #f" #f
  (getenv "HAFOD_NONEXISTENT_VAR_99999"))

;; setenv and getenv roundtrip
(test-equal "setenv then getenv" "hello"
  (begin (setenv "HAFOD_T1" "hello")
         (getenv "HAFOD_T1")))

;; setenv with #f deletes
(test-equal "setenv #f deletes" #f
  (begin (setenv "HAFOD_T1" "x")
         (setenv "HAFOD_T1" #f)
         (getenv "HAFOD_T1")))

;; env->alist returns alist containing PATH
(test-assert "env->alist contains PATH"
  (assoc "PATH" (env->alist)))

;; env->alist values are strings
(test-assert "env->alist has string pairs"
  (let ([alist (env->alist)])
    (and (list? alist)
         (pair? alist)
         (pair? (car alist))
         (string? (caar alist))
         (string? (cdar alist)))))

;; alist->env replaces (save and restore)
(let ([saved (env->alist)])
  (test-assert "alist->env replaces"
    (begin
      (alist->env '(("ONLY_VAR" . "1")))
      (let ([result (env->alist)])
        (alist->env saved)
        (and (= 1 (length result))
             (equal? "1" (cdr (car result)))))))
  ;; Verify restore worked
  (test-assert "alist->env restore works"
    (assoc "PATH" (env->alist))))

;; with-env* scoping
(test-equal "with-env* adds FOO" "bar"
  (with-env* '(("HAFOD_FOO" . "bar"))
    (lambda () (getenv "HAFOD_FOO"))))

;; with-env* restores
(test-equal "with-env* restores after" #f
  (begin
    (with-env* '(("HAFOD_FOO2" . "baz"))
      (lambda () (getenv "HAFOD_FOO2")))
    (getenv "HAFOD_FOO2")))

;; with-total-env* replaces and restores
(let ([saved (env->alist)])
  (test-assert "with-total-env* replaces during thunk"
    (with-total-env* '(("ONLY" . "yes"))
      (lambda ()
        (let ([alist (env->alist)])
          (and (= 1 (length alist))
               (equal? "yes" (cdr (car alist))))))))
  ;; After with-total-env*, env should be restored
  (test-assert "with-total-env* restores after"
    (assoc "PATH" (env->alist))))

;; with-env sugar macro
(test-equal "with-env sugar" "V"
  (with-env '(("HAFOD_K" . "V"))
    (getenv "HAFOD_K")))

;; align-env! syncs
(test-assert "align-env! syncs OS"
  (begin
    (setenv "HAFOD_ALIGN_TEST" "synced")
    (align-env!)
    (let ([os-val (posix-getenv "HAFOD_ALIGN_TEST")])
      (setenv "HAFOD_ALIGN_TEST" #f)
      (equal? os-val "synced"))))

;; posix-getgroups
(test-assert "posix-getgroups returns list"
  (let ([groups (posix-getgroups)])
    (and (list? groups)
         (pair? groups)
         (integer? (car groups)))))

;; read-environ
(test-assert "read-environ returns alist of string pairs"
  (let ([env (read-environ)])
    (and (list? env)
         (pair? env)
         (pair? (car env))
         (string? (caar env))
         (string? (cdar env)))))

(test-end)
