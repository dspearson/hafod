;;; test-cd-nav.ss -- cd navigation: logical ".." collapse, cd - / -N, $CDPATH
;;; Exercises the shared cd-to! seam's lexical parent-collapse (so no literal
;;; ".." survives in the working directory or $PWD), the cd - / cd -N stack
;;; echoes, and the $CDPATH search -- all driven through run-builtin! against a
;;; freshly created temp tree so the oracles never depend on a host path's
;;; physical identity (a matter for the macOS /tmp symlink in particular).

;; The tests chdir around a temp tree, so pin the compiled-library search to
;; ABSOLUTE paths derived from the launch directory (still the repo root here):
;; a library's .so is located lazily at the first reference to one of its
;; bindings, so a relative libdir would otherwise resolve against whatever
;; directory an in-suite cd has left us in and fail to load.
(library-directories
  (let ([root (current-directory)])
    (list (cons (string-append root "/src") (string-append root "/src"))
          (cons root root))))
(import (test runner)
        (only (hafod shell builtins) run-builtin! builtin-cd-path dir-stack)
        (only (hafod process-state) cwd chdir pid)
        (only (hafod environment) getenv setenv)
        (only (hafod fileinfo) create-directory create-symlink
                               delete-directory delete-file)
        (except (chezscheme) exit open-input-file open-output-file getenv
                delete-file delete-directory))

(test-begin "cd navigation")

;; A unique temp root, keyed by pid and a random suffix so concurrent runs never
;; collide; built as a plain lexical path (no "." / ".." / metacharacters) so an
;; assertion may compare against exactly the string we constructed.
(define (temp-root tag)
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/hafod-cdnav-" tag "-"
                 (number->string (pid)) "-"
                 (number->string (random 1000000))))

;; Remove a single path, tolerating absence and either kind: unlink first (which
;; removes a regular file or a symlink), and only if that fails try rmdir (a real
;; directory).  Guards swallow ENOENT so a partial tree still cleans up.
(define (rm! p)
  (guard (e [#t (guard (e2 [#t #f]) (delete-directory p))])
    (delete-file p)))

;; ======================================================================
;; Logical ".." collapse: cd .. / cd ../.. leave a clean absolute path
;; ======================================================================

;; Build <root>/a/b, cd into it, then cd .. -- both (cwd) and $PWD must read the
;; lexical parent <root>/a with NO literal "..".  A physical/concat impl (today's
;; chdir builds %cwd by string-appending the raw "..") leaves the ".." and fails.
(let* ([root (temp-root "collapse")]
       [a (string-append root "/a")]
       [b (string-append a "/b")]
       [orig (cwd)]
       [orig-pwd (getenv "PWD")])
  (define (cleanup!)
    (chdir orig)
    (when orig-pwd (setenv "PWD" orig-pwd))
    (rm! b) (rm! a) (rm! root))
  (cleanup!)                          ;; clear any stale leftover from a prior run
  (create-directory root)
  (create-directory a)
  (create-directory b)
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (run-builtin! (string-append "cd " b))
      (run-builtin! "cd ..")
      (test-equal "cd .. collapses to the lexical parent (cwd)"
        a (cwd))
      (test-equal "cd .. collapses to the lexical parent (PWD)"
        a (getenv "PWD"))

      (run-builtin! (string-append "cd " b))
      (run-builtin! "cd ../..")
      (test-equal "cd ../.. pops two levels (cwd)"
        root (cwd))
      (test-equal "cd ../.. pops two levels (PWD)"
        root (getenv "PWD")))
    cleanup!))

;; ======================================================================
;; Logical (not physical) parent through a symlink
;; ======================================================================

;; <root>/link -> <root>/a/b ; cd <root>/link then cd .. lands in <root> (the
;; LEXICAL parent of the symlink), never <root>/a -- which realpath/physical ".."
;; resolution would give.  Pins the zsh-without-`-P` semantics so it cannot drift
;; to physical resolution unnoticed.
(let* ([root (temp-root "symlink")]
       [a (string-append root "/a")]
       [b (string-append a "/b")]
       [link (string-append root "/link")]
       [orig (cwd)]
       [orig-pwd (getenv "PWD")])
  (define (cleanup!)
    (chdir orig)
    (when orig-pwd (setenv "PWD" orig-pwd))
    (rm! link) (rm! b) (rm! a) (rm! root))
  (cleanup!)
  (create-directory root)
  (create-directory a)
  (create-directory b)
  (create-symlink b link)
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (run-builtin! (string-append "cd " link))
      (test-equal "cd through a symlink records the symlink path"
        link (cwd))
      (run-builtin! "cd ..")
      (test-equal "cd .. from a symlink lands in its lexical parent"
        root (cwd))
      (test-assert "cd .. from a symlink is NOT the physical parent"
        (not (string=? (cwd) a))))
    cleanup!))

;; ======================================================================
;; The physical fallback leaves a clean logical path (no literal "..")
;; ======================================================================

;; True when S carries ".." anywhere -- the literal a logical cd must never leave
;; in (cwd) or $PWD.  The temp paths never contain "..", so a plain two-character
;; scan is exact.
(define (contains-dotdot? s)
  (let ([n (string-length s)])
    (let loop ([i 0])
      (cond
        [(> (+ i 2) n) #f]
        [(and (char=? (string-ref s i) #\.)
              (char=? (string-ref s (+ i 1)) #\.)) #t]
        [else (loop (+ i 1))]))))

;; <root>/link -> <root>/a/b, with <root>/a/c present but <root>/c absent.  From
;; the symlink, cd ../c collapses lexically to <root>/c (which does not exist), so
;; the seam falls back to entering the raw "../c" physically -- landing in
;; <root>/a/c.  That fallback must still leave a clean working directory: a
;; verbatim fallback records "<root>/link/../c", leaking the literal ".." into
;; (cwd) and $PWD.  A marker directory confirms the physical move happened at all,
;; so the no-".." assertion is not vacuously satisfied by a cd that merely failed.
(let* ([root (temp-root "fallback")]
       [a (string-append root "/a")]
       [b (string-append a "/b")]
       [c (string-append a "/c")]
       [cmark (string-append c "/cmark")]
       [link (string-append root "/link")]
       [orig (cwd)]
       [orig-pwd (getenv "PWD")])
  (define (cleanup!)
    (chdir orig)
    (when orig-pwd (setenv "PWD" orig-pwd))
    (rm! cmark) (rm! c) (rm! b) (rm! a) (rm! link) (rm! root))
  (cleanup!)
  (create-directory root)
  (create-directory a)
  (create-directory b)
  (create-directory c)
  (create-directory cmark)
  (create-symlink b link)
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (run-builtin! (string-append "cd " link))    ;; cwd = link (physically a/b)
      (run-builtin! "cd ../c")                       ;; lexical <root>/c is absent
      (test-assert "the physical fallback actually entered the target directory"
        (file-directory? "cmark"))
      (test-assert "the physical fallback leaves no literal .. in the working directory"
        (not (contains-dotdot? (cwd))))
      (test-assert "the physical fallback leaves no literal .. in $PWD"
        (not (contains-dotdot? (or (getenv "PWD") "")))))
    cleanup!))

;; ======================================================================
;; Normalisation is a no-op on a clean path; ".." never escapes the root
;; ======================================================================

(let* ([root (temp-root "noop")]
       [a (string-append root "/a")]
       [orig (cwd)]
       [orig-pwd (getenv "PWD")])
  (define (cleanup!)
    (chdir orig)
    (when orig-pwd (setenv "PWD" orig-pwd))
    (rm! a) (rm! root))
  (cleanup!)
  (create-directory root)
  (create-directory a)
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      ;; A clean absolute path is entered byte-for-byte -- the regression guard
      ;; for the existing cd/pushd/auto-cd/z results.
      (run-builtin! (string-append "cd " a))
      (test-equal "a clean path is left exactly as given"
        a (cwd))
      ;; cd .. never pops past "/".
      (run-builtin! "cd /")
      (run-builtin! "cd ..")
      (test-equal "cd .. never pops past root"
        "/" (cwd)))
    cleanup!))

;; Capture everything THUNK writes to the current output port as a string, so an
;; oracle can assert on the line cd prints after a non-literal jump (cd - / -N /
;; a $CDPATH hit).  cd echoes to stdout, its errors to stderr, so only the echo
;; is caught here.
(define (capture-stdout thunk)
  (let ([p (open-output-string)])
    (parameterize ([current-output-port p])
      (thunk))
    (get-output-string p)))

;; ======================================================================
;; cd - returns to $OLDPWD and echoes the directory it entered
;; ======================================================================

(let* ([root (temp-root "dash")]
       [a (string-append root "/a")]
       [b (string-append root "/b")]
       [orig (cwd)]
       [orig-pwd (getenv "PWD")]
       [orig-old (getenv "OLDPWD")])
  (define (cleanup!)
    (chdir orig)
    (when orig-pwd (setenv "PWD" orig-pwd))
    (when orig-old (setenv "OLDPWD" orig-old))
    (rm! a) (rm! b) (rm! root))
  (cleanup!)
  (create-directory root)
  (create-directory a)
  (create-directory b)
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (run-builtin! (string-append "cd " a))       ;; OLDPWD <- orig, cwd = a
      (run-builtin! (string-append "cd " b))       ;; OLDPWD <- a,    cwd = b
      (let ([out (capture-stdout (lambda () (run-builtin! "cd -")))])
        (test-equal "cd - returns to the previous directory"
          a (cwd))
        (test-equal "cd - prints the directory it entered"
          (string-append a "\n") out)))
    cleanup!))

;; ======================================================================
;; cd -N navigates the Nth directory-stack entry
;; ======================================================================

(let* ([root (temp-root "stack")]
       [a (string-append root "/a")]
       [b (string-append root "/b")]
       [c (string-append root "/c")]
       [orig (cwd)]
       [orig-pwd (getenv "PWD")])
  (define (cleanup!)
    (chdir orig)
    (when orig-pwd (setenv "PWD" orig-pwd))
    ;; drain any entries this block pushed so a later suite starts on a bare stack
    (let drain () (unless (null? (dir-stack)) (run-builtin! "popd") (drain)))
    (chdir orig)
    (rm! a) (rm! b) (rm! c) (rm! root))
  (cleanup!)
  (create-directory root)
  (create-directory a)
  (create-directory b)
  (create-directory c)
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (run-builtin! (string-append "cd " c))       ;; cwd = c
      (run-builtin! (string-append "pushd " a))    ;; stack = (c), cwd = a
      (run-builtin! (string-append "cd " b))       ;; cwd = b, stack = (c)
      (let ([out (capture-stdout (lambda () (run-builtin! "cd -1")))])
        (test-equal "cd -1 navigates to the first directory-stack entry"
          c (cwd))
        (test-equal "cd -1 prints the entry it entered"
          (string-append c "\n") out))
      ;; an out-of-range -N reports a cd: error and does not move
      (let ([here (cwd)])
        (run-builtin! "cd -9")
        (test-equal "an out-of-range cd -N does not move"
          here (cwd))))
    cleanup!))

;; ======================================================================
;; $CDPATH search: a base-only-reachable dir navigates and prints
;; ======================================================================

(let* ([root (temp-root "cdpath")]
       [base (string-append root "/base")]
       [proj (string-append base "/proj")]
       [start (string-append root "/start")]
       [orig (cwd)]
       [orig-pwd (getenv "PWD")]
       [orig-cdpath (getenv "CDPATH")])
  (define (cleanup!)
    (chdir orig)
    (when orig-pwd (setenv "PWD" orig-pwd))
    (setenv "CDPATH" (or orig-cdpath ""))
    (rm! proj) (rm! base) (rm! start) (rm! root))
  (cleanup!)
  (create-directory root)
  (create-directory base)
  (create-directory proj)
  (create-directory start)
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (setenv "CDPATH" base)
      (run-builtin! (string-append "cd " start))   ;; cwd = start; no ./proj here
      ;; cd proj is reachable only under $CDPATH/base: it navigates there and
      ;; prints the resolved absolute path (POSIX, for a non-cwd CDPATH hit).
      (let ([out (capture-stdout (lambda () (run-builtin! "cd proj")))])
        (test-equal "cd <name> found via CDPATH navigates there"
          proj (cwd))
        (test-equal "cd <name> via CDPATH prints the resolved path"
          (string-append proj "\n") out))

      ;; the Task-1 collapse stays uniform through a CDPATH hit: cd .. lands the
      ;; lexical parent (base), never a literal base/proj/..
      (run-builtin! (string-append "cd " start))
      (capture-stdout (lambda () (run-builtin! "cd proj")))
      (run-builtin! "cd ..")
      (test-equal "cd .. after a CDPATH hit collapses to the lexical parent"
        base (cwd))

      ;; CDPATH gate: ./x is entered literally, never redirected through $CDPATH,
      ;; so from start (which holds no proj) cd ./proj fails and does not move.
      (run-builtin! (string-append "cd " start))
      (let ([here (cwd)])
        (run-builtin! "cd ./proj")
        (test-equal "cd ./proj is not redirected through CDPATH"
          here (cwd)))

      ;; with $CDPATH unset a bare non-local name behaves exactly as before: it
      ;; is not found in cwd, so cd fails and does not move.
      (setenv "CDPATH" "")
      (run-builtin! (string-append "cd " start))
      (let ([here (cwd)])
        (run-builtin! "cd proj")
        (test-equal "with CDPATH unset a non-local name does not move"
          here (cwd))))
    cleanup!))

;; ======================================================================
;; An empty operand is not the root: cd "" must not jump to /
;; ======================================================================

;; cd "" reaches the change-directory seam with an empty target.  An empty name
;; is reported as absolute and the logical collapse turns it into "/", so an
;; un-guarded cd "" -- and cd "$UNSET", whose empty expansion arrives the same
;; way -- would silently land in the root directory.  From a known directory an
;; empty operand must leave the working directory exactly where it was (the seam
;; surfaces the ENOENT cd: error instead) and never move to "/".
(let* ([root (temp-root "empty")]
       [orig (cwd)]
       [orig-pwd (getenv "PWD")])
  (define (cleanup!)
    (chdir orig)
    (when orig-pwd (setenv "PWD" orig-pwd))
    (rm! root))
  (cleanup!)
  (create-directory root)
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (run-builtin! (string-append "cd " root))    ;; cwd = root, a known place
      (let ([here (cwd)])
        (run-builtin! "cd \"\"")                     ;; empty quoted operand
        (test-equal "cd \"\" leaves the working directory untouched"
          here (cwd))
        (test-assert "cd \"\" never lands at the root directory"
          (not (string=? (cwd) "/")))))
    cleanup!))

(test-end)
