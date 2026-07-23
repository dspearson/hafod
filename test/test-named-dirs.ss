;;; test-named-dirs.ss -- User-definable named directories (~name)
;;; Exercises the named-directories table, its register/ref/list surface, and
;;; the table-first tilde expansion that lets `cd ~proj` navigate to a
;;; registered directory -- with a named directory shadowing a same-named
;;; login, as zsh's `hash -d` does.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod shell parser)
        ;; environment/environment-symbols instantiate (hafod) to prove the
        ;; umbrella re-export without pulling its bindings into scope.
        (only (chezscheme) environment environment-symbols))

(test-begin "Named directories")

;; ======================================================================
;; register / ref / list
;; ======================================================================

(register-named-directory! "proj" "/tmp/zzp")

(test-equal "named-directory-ref returns the registered directory"
  "/tmp/zzp"
  (named-directory-ref "proj"))

(test-assert "named-directory-ref returns #f for an unregistered name"
  (not (named-directory-ref "definitely-absent-name-xyz")))

(test-assert "named-directories-list holds a (name . dir) pair per entry"
  (let ([entries (named-directories-list)])
    (and (equal? (assoc "proj" entries) '("proj" . "/tmp/zzp"))
         (for-all (lambda (e) (and (pair? e) (string? (car e)) (string? (cdr e))))
                  entries))))

;; ======================================================================
;; ~name expansion through parse-command-words
;; ======================================================================

;; The non-vacuous NAV oracle: a passwd-only expander leaves `~proj` literal
;; (there is no `proj` login), so it would yield ("cd" "~proj") and fail here.
(test-equal "cd ~proj tilde-expands to the registered directory"
  '("cd" "/tmp/zzp")
  (parse-command-words "cd ~proj"))

;; `root` exists in the passwd database on every target host; registering it as
;; a named directory must take precedence over its passwd home (table-first).
(test-assert "a named directory shadows a same-named login"
  (let ([before (cadr (parse-command-words "cd ~root"))])
    (register-named-directory! "root" "/tmp/zz-root-named")
    (let ([after (cadr (parse-command-words "cd ~root"))])
      (and (string=? after "/tmp/zz-root-named")
           (not (string=? before "/tmp/zz-root-named"))))))

;; ======================================================================
;; Umbrella reachability
;; ======================================================================

;; register-named-directory! is the public init.ss surface, so it must be
;; reachable through a single (import (hafod)); named-directory-ref and
;; named-directories-list stay leaf-only cross-module plumbing.
(test-assert "register-named-directory! is reachable through (import (hafod))"
  (and (memq 'register-named-directory!
             (environment-symbols (environment '(hafod))))
       #t))

(test-assert "named-directory-ref stays out of the (hafod) umbrella"
  (not (memq 'named-directory-ref
             (environment-symbols (environment '(hafod))))))

(test-end)

