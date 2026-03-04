;;; (hafod user-group) -- User and group database lookups for hafod
;;; Provides user-info, group-info, flexible uid/username/gid/groupname lookups,
;;; and home-directory/home-dir/home-file.
;;; Ported from scsh/scheme/user-group.scm
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod user-group)
  (export
    ;; User database
    user-info
    ;; scsh-compatible user-info accessors (colon-separated names)
    user-info:name user-info:uid user-info:gid
    user-info:home-dir user-info:shell
    ;; Group database
    group-info
    ;; scsh-compatible group-info accessors (colon-separated names)
    group-info:name group-info:gid group-info:members
    ;; scsh-compatible predicate
    user-info?
    ;; Lookup by name (scsh-compatible)
    name->user-info
    ;; Flexible lookups
    ->uid ->username ->gid ->groupname
    ;; Home directory
    home-directory home-dir home-file
    ;; Internal (scsh compat)
    %homedir init-home-directory)

  (import (hafod internal base)
          (hafod posix) (hafod compat) (hafod fname))

  ;; ======================================================================
  ;; User database lookups
  ;; ======================================================================

  ;; scsh-compatible accessor aliases (colon-separated names)
  (define (user-info:name ui) (passwd-info-name ui))
  (define (user-info:uid ui) (passwd-info-uid ui))
  (define (user-info:gid ui) (passwd-info-gid ui))
  (define (user-info:home-dir ui) (passwd-info-dir ui))
  (define (user-info:shell ui) (passwd-info-shell ui))

  (define (group-info:name gi) (group-info-name gi))
  (define (group-info:gid gi) (group-info-gid gi))
  (define (group-info:members gi) (group-info-members gi))

  ;; scsh-compatible predicate (alias for passwd-info?)
  (define user-info? passwd-info?)

  ;; name->user-info: scsh-compatible lookup by name
  (define (name->user-info name) (user-info name))

  ;; user-info: polymorphic -- string calls posix-getpwnam, integer calls posix-getpwuid.
  ;; Returns the passwd-info record (already defined in posix.ss).
  ;; Raises error if not found.
  (define (user-info uid/name)
    (let ([pw (cond
                [(string? uid/name)  (posix-getpwnam uid/name)]
                [(integer? uid/name) (posix-getpwuid uid/name)]
                [else (error 'user-info
                             "argument must be string or integer" uid/name)])])
      (or pw (error 'user-info "no such user" uid/name))))

  ;; ======================================================================
  ;; Group database lookups
  ;; ======================================================================

  ;; group-info: polymorphic -- string calls posix-getgrnam, integer calls posix-getgrgid.
  ;; Returns the group-info record (already defined in posix.ss -- name conflicts
  ;; with our export so we use the posix accessor functions directly).
  (define (group-info gid/name)
    (let ([gr (cond
                [(string? gid/name)  (posix-getgrnam gid/name)]
                [(integer? gid/name) (posix-getgrgid gid/name)]
                [else (error 'group-info
                             "argument must be string or integer" gid/name)])])
      (or gr (error 'group-info "no such group" gid/name))))

  ;; ======================================================================
  ;; Flexible lookups
  ;; ======================================================================

  (define (->uid uid/name)
    (cond
      [(integer? uid/name) uid/name]
      [(string? uid/name)  (passwd-info-uid (user-info uid/name))]
      [else (error '->uid "argument must be string or integer" uid/name)]))

  (define (->username uid/name)
    (cond
      [(string? uid/name)  uid/name]
      [(integer? uid/name) (passwd-info-name (user-info uid/name))]
      [else (error '->username "argument must be string or integer" uid/name)]))

  (define (->gid gid/name)
    (cond
      [(integer? gid/name) gid/name]
      [(string? gid/name)  (group-info-gid (group-info gid/name))]
      [else (error '->gid "argument must be string or integer" gid/name)]))

  (define (->groupname gid/name)
    (cond
      [(string? gid/name)  gid/name]
      [(integer? gid/name) (group-info-name (group-info gid/name))]
      [else (error '->groupname "argument must be string or integer" gid/name)]))

  ;; ======================================================================
  ;; Home directory
  ;; ======================================================================

  ;; %home-dir-value: cached current user's home directory string.
  ;; Initialized at load time from $HOME with fallback to passwd db.
  (define %home-dir-value
    (or (posix-getenv "HOME")
        (let ([pw (posix-getpwuid (posix-getuid))])
          (and pw (passwd-info-dir pw)))
        (error 'home-directory "Cannot determine home directory")))

  ;; %homedir: scsh internal name for home directory value
  (define (%homedir) %home-dir-value)

  ;; init-home-directory: re-initialize cached home directory
  (define (init-home-directory)
    (set! %home-dir-value
      (or (posix-getenv "HOME")
          (let ([pw (posix-getpwuid (posix-getuid))])
            (and pw (passwd-info-dir pw)))
          (error 'init-home-directory "Cannot determine home directory"))))

  ;; home-directory: returns the current user's home directory.
  ;; Exported as a thunk (zero-argument procedure) for compatibility
  ;; with fname-system which calls (home-directory).
  (define (home-directory) %home-dir-value)

  ;; home-dir: with optional user argument.
  ;; (home-dir) returns home-directory value.
  ;; (home-dir user) returns that user's home directory.
  (define (home-dir . maybe-user)
    (if (pair? maybe-user)
        (let ([user (car maybe-user)])
          (ensure-file-name-is-nondirectory
            (passwd-info-dir (user-info user))))
        %home-dir-value))

  ;; home-file: construct path under home directory.
  ;; (home-file fname) => home-directory + "/" + fname
  ;; (home-file user fname) => user's home + "/" + fname
  (define (home-file arg1 . maybe-arg2)
    (receive (dir fname)
             (if (pair? maybe-arg2)
                 (values (home-dir arg1) (car maybe-arg2))
                 (values %home-dir-value arg1))
      (string-append (file-name-as-directory dir) fname)))

  ) ; end library
