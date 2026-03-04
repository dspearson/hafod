;;; Ported from scsh/test/user-and-group-db-access.scm
;;; Tests for user and group database access (section 3.6 of scsh manual)
;;; Original author: Christoph Hetz. Ported to hafod test runner.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod))

(test-begin "scsh-user-group")

;; Look up user info for current user
(let* ((user-0 (user-info (getenv "USER")))
       (user-name (passwd-info-name user-0))
       (user-id (passwd-info-uid user-0))
       (user-gid-val (passwd-info-gid user-0))
       (user-hdir (passwd-info-dir user-0))
       (user-shell-val (passwd-info-shell user-0))
       ;; Look up group info by gid from user-info
       (group-0 (group-info user-gid-val))
       (group-name-val (group-info-name group-0))
       (group-id (group-info-gid group-0))
       (group-mem (group-info-members group-0)))

  ;; Verify types of user-info fields
  (test-assert "user-info:name is string"
    (string? user-name))

  (test-assert "user-info:uid is integer"
    (integer? user-id))

  (test-assert "user-info:gid is integer"
    (integer? user-gid-val))

  (test-assert "user-info:home-dir is string"
    (string? user-hdir))

  (test-assert "user-info:shell is string"
    (string? user-shell-val))

  ;; Verify types of group-info fields
  (test-assert "group-info:name is string"
    (string? group-name-val))

  (test-assert "group-info:gid is integer"
    (integer? group-id))

  (test-assert "group-info:members is list"
    (list? group-mem))

  ;; Cross-lookup consistency: look up by uid, compare name
  (test-equal "user-info by uid matches by name"
    user-name
    (passwd-info-name (user-info user-id)))

  ;; Look up by name again, verify same name
  (test-equal "user-info by name consistent"
    user-name
    (passwd-info-name (user-info (getenv "USER"))))

  ;; Group cross-lookup: look up by name, compare gid
  (test-equal "group-info by name matches by gid"
    group-id
    (group-info-gid (group-info group-name-val))))

(test-end)
