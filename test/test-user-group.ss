(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod user-group) (hafod posix))

(test-begin "user-group")

;; user-info by name
(test-assert "user-info root by name"
  (let ([ui (user-info "root")])
    (and (passwd-info? ui)
         (= 0 (passwd-info-uid ui))
         (string=? "root" (passwd-info-name ui)))))

;; user-info by uid
(test-assert "user-info root by uid"
  (let ([ui (user-info 0)])
    (and (passwd-info? ui)
         (= 0 (passwd-info-uid ui))
         (string=? "root" (passwd-info-name ui)))))

;; user-info bad type
(test-error "user-info bad type" (user-info 3.14))

;; user-info nonexistent
(test-error "user-info nonexistent" (user-info "hafod_no_such_user_99999"))

;; group-info by name -- GID 0 is "root" on Linux, "wheel" on macOS
(define gid0-name (group-info-name (group-info 0)))

(test-assert "group-info gid0 by name"
  (let ([gi (group-info gid0-name)])
    (and (group-info? gi)
         (= 0 (group-info-gid gi)))))

;; group-info by gid
(test-assert "group-info gid0 by gid"
  (let ([gi (group-info 0)])
    (and (group-info? gi)
         (string=? gid0-name (group-info-name gi)))))

;; group-info bad type
(test-error "group-info bad type" (group-info 3.14))

;; group-info nonexistent
(test-error "group-info nonexistent" (group-info "hafod_no_such_group_99999"))

;; ->uid
(test-equal "->uid root" 0 (->uid "root"))
(test-equal "->uid 0" 0 (->uid 0))

;; ->username
(test-equal "->username 0" "root" (->username 0))
(test-equal "->username root" "root" (->username "root"))

;; ->gid
(test-equal "->gid gid0-name" 0 (->gid gid0-name))
(test-equal "->gid 0" 0 (->gid 0))

;; ->groupname
(test-equal "->groupname 0" gid0-name (->groupname 0))
(test-equal "->groupname roundtrip" gid0-name (->groupname gid0-name))

;; home-directory
(test-assert "home-directory returns non-empty string"
  (let ([h (home-directory)])
    (and (string? h) (> (string-length h) 0))))

(test-equal "home-directory matches HOME env"
  (posix-getenv "HOME")
  (home-directory))

;; home-dir
(test-equal "home-dir no args" (home-directory) (home-dir))

(test-assert "home-dir root returns string"
  (let ([h (home-dir "root")])
    (and (string? h) (> (string-length h) 0))))

;; home-file
(test-assert "home-file appends filename"
  (let ([hf (home-file "foo")])
    (and (string? hf)
         (> (string-length hf) 4)
         (string=? hf (string-append (home-directory) "/foo")))))

;; home-file with user
(test-assert "home-file with user"
  (let ([hf (home-file "root" "bar")])
    (and (string? hf)
         (> (string-length hf) 4))))

(test-end)
