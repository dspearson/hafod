;;; test-getpwent -- passwd-database enumeration.
;;;
;;; Proves the whole-database walk agrees with the key-based lookup: the login
;;; the current process runs as must turn up among the enumerated entries, with
;;; a home directory, and a name that is no login must not. Because both paths
;;; share one extractor and one set of struct offsets, the current-login match
;;; also stands in for the struct-layout assumption on every platform the tests
;;; run on (Linux and macOS lay the passwd fields out differently, but the
;;; generated offsets track that).
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod posix)
              posix-getpwent-all passwd-info? passwd-info-name passwd-info-dir
              posix-getpwuid posix-getuid ptr->string))

(test-begin "getpwent enumeration")

;; Walk the database once; the current user is looked up by uid the key-based
;; way, so the two paths can be compared.
(define all (posix-getpwent-all))
(define names (map passwd-info-name all))
(define me (posix-getpwuid (posix-getuid)))
(define my-name (and me (passwd-info-name me)))
(define my-entry
  (and my-name
       (find (lambda (p) (string=? (passwd-info-name p) my-name)) all)))

(test-assert "enumeration is a non-empty list of passwd-info"
  (and (pair? all) (for-all passwd-info? all)))

(test-assert "current uid resolves to a passwd entry"
  (passwd-info? me))

(test-assert "current login appears among the enumerated names"
  (and my-name (member my-name names) #t))

(test-assert "the enumerated current login has a non-empty home directory"
  (and my-entry
       (string? (passwd-info-dir my-entry))
       (positive? (string-length (passwd-info-dir my-entry)))))

(test-assert "a name that is no login is absent from the enumeration"
  (not (member "zzz-no-such-login-zzz" names)))

;; NULL-field guard: a passwd/group field pointer is NULL whenever an NSS backend
;; (LDAP, SSSD, NIS, the systemd userdb) omits an optional attribute.  The shared
;; marshaller must map that NULL to "" rather than reach strlen(NULL) -- a fault
;; that would take the shell down mid-enumeration where no guard could catch it.
;; The local files backend fills such fields with "" (non-NULL), so this is the
;; only place the NULL path is exercised directly.
(test-assert "ptr->string maps a NULL pointer to the empty string"
  (string=? (ptr->string 0) ""))

(test-end)
