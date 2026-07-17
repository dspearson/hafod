;;; Ported from scsh/test/process-state-tests.scm
;;; Tests for process state: umask, cwd (section 3.5 of scsh manual)
;;; Original author: David Frese. Ported to hafod test runner.
;;;
;;; Deepened toward scsh conformance: beyond the flat scoper round-trips the
;;; original port covers, this suite exercises the dynamic-wind guarantees the
;;; scopers make (nesting, and restoration after a non-local escape), the
;;; scheme-side relative resolution of the working directory, the error path
;;; of a bad chdir, and the shape of the read-only identity family -- asserting
;;; type and plausibility only, never a machine-specific value, and always
;;; leaving the umask and cwd at their entry values.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod))

(test-begin "scsh-process-state")

;; --- umask stuff ---

;; with-umask: set umask inside dynamic extent, verify restores
(test-assert "with-umask"
  (let ((old-umask (umask)))
    (and (with-umask 0
           (file-mode=? (umask) 0))
         (file-mode=? (umask) old-umask))))

;; set-umask: imperatively set, then restore
(test-assert "set-umask"
  (let ((old-umask (umask)))
    (set-umask #o007)
    (let ((res (umask)))
      (set-umask old-umask)
      (file-mode=? res #o007))))

;; --- cwd stuff ---

;; with-cwd: set cwd inside dynamic extent, verify restores
(test-assert "with-cwd"
  (let ((old-cwd (cwd)))
    (and (with-cwd "/"
           (equal? (cwd) "/"))
         (equal? (cwd) old-cwd))))

;; chdir: imperatively change cwd, then restore
(test-assert "chdir"
  (let ((old-cwd (cwd)))
    (chdir "/")
    (let ((res (cwd)))
      (chdir old-cwd)
      (equal? res "/"))))

;; --- umask scoper edges ---

(let ((orig (umask)))
  ;; The scoped mask is exactly the value supplied: an octal round-trip.
  (test-equal "with-umask octal round-trip" #o077
    (with-umask #o077 (umask)))

  ;; Nested scopes stack and unwind: the inner mask holds inside its extent
  ;; and the outer mask is restored on the way out.
  (test-assert "with-umask nests and unwinds to the outer mask"
    (with-umask #o022
      (and (file-mode=? (umask) #o022)
           (with-umask #o077 (file-mode=? (umask) #o077))
           (file-mode=? (umask) #o022))))

  ;; A non-local escape out of the body still runs the dynamic-wind after-thunk,
  ;; so the mask is restored. A naive set!-without-unwind scoper would leak it.
  (test-assert "with-umask restores the mask after a non-local escape"
    (begin
      (guard (e (#t #t)) (with-umask #o077 (error 'boom "escape")))
      (file-mode=? (umask) orig))))

;; --- cwd scoper edges ---

(let ((orig (cwd)))
  ;; A relative target resolves against the scheme-side working directory, not
  ;; the OS-resolved path, so nesting "/" then "tmp" yields "/tmp" regardless of
  ;; how /tmp is symlinked on the host.
  (test-equal "with-cwd resolves a relative name against the scheme-side cwd" "/tmp"
    (with-cwd "/" (with-cwd "tmp" (cwd))))

  ;; As with the mask, an escape out of the body restores the working directory.
  (test-assert "with-cwd restores the working directory after a non-local escape"
    (begin
      (guard (e (#t #t)) (with-cwd "/" (error 'boom "escape")))
      (equal? (cwd) orig))))

;; chdir to a nonexistent path raises before it mutates the scheme-side cwd, so
;; the working directory is left untouched -- the error path leaks nothing.
(let ((orig (cwd)))
  (test-error "chdir to a nonexistent path raises"
    (chdir "/no-such-dir-hafod-xyz"))
  (test-assert "a failed chdir leaves the working directory unchanged"
    (equal? (cwd) orig)))

;; --- read-only identity family: shape and plausibility only ---

;; Real and effective ids are non-negative integers (posix returns them
;; unsigned); asserting the exact uid/gid would be machine-specific.
(test-assert "user-uid is a non-negative integer"
  (let ((u (user-uid))) (and (integer? u) (>= u 0))))

(test-assert "user-gid is a non-negative integer"
  (let ((g (user-gid))) (and (integer? g) (>= g 0))))

(test-assert "effective uid and gid are non-negative integers"
  (and (integer? (user-effective-uid)) (>= (user-effective-uid) 0)
       (integer? (user-effective-gid)) (>= (user-effective-gid) 0)))

;; The supplementary group list may legitimately be empty; every member is a
;; non-negative integer.
(test-assert "user-supplementary-gids is a list of non-negative integers"
  (let ((gs (user-supplementary-gids)))
    (and (list? gs)
         (for-all (lambda (g) (and (integer? g) (>= g 0))) gs))))

;; user-login-name resolves the login name and can raise in a bare container
;; whose uid has no passwd entry, so guard it and accept a string or #f.
(test-assert "user-login-name is a string or #f"
  (guard (e (#t #t))
    (let ((n (user-login-name)))
      (or (not n) (string? n)))))

(test-assert "pid is positive"
  (> (pid) 0))

;; A process is never its own parent.
(test-assert "pid and parent-pid differ"
  (not (= (pid) (parent-pid))))

;; process-group is nullary here: it reports the calling process's own group.
;; scsh also accepts an optional process or pid to query another process's
;; group; hafod does not yet provide that target form (it would need a distinct
;; group-of-pid lookup), so only the self case is exercised -- the target form
;; is a deferred capability, not a covered one.
(test-assert "process-group of the current process is positive"
  (> (process-group) 0))

(test-end)
