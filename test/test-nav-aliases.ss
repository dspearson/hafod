;;; Tests for command-alias resolution in the shell classifier
;;; Copyright (c) 2026 Dominic Pearson.
;; Resolve the compiled libraries by ABSOLUTE path, derived from the launch
;; directory while it is still the repo root: the builtin-cd-path assertion below
;; chdirs, and a compiled library is located lazily at its first binding
;; reference, so a relative libdir could otherwise be resolved against the
;; temporary directory. Pinning absolute paths keeps resolution cd-independent.
(library-directories
  (let ([root (current-directory)])
    (list (cons (string-append root "/src") (string-append root "/src"))
          (cons root root))))
(import (test runner)
        (hafod shell classifier)
        (hafod shell builtins)
        (only (hafod process-state) chdir cwd)
        (only (hafod environment) getenv setenv)
        (except (chezscheme) exit open-input-file open-output-file getenv))

;; Is NEEDLE a substring of HAYSTACK? (a small local helper -- Chez has none.)
(define (substring? needle haystack)
  (let ([nl (string-length needle)] [hl (string-length haystack)])
    (let loop ([i 0])
      (cond [(> (+ i nl) hl) #f]
            [(string=? needle (substring haystack i (+ i nl))) #t]
            [else (loop (+ i 1))]))))

(test-begin "nav-aliases")

;; Build the PATH cache first so a real command head (e.g. ls) resolves.
(rebuild-path-cache!)

;; --- head replaced, the remaining words appended verbatim ---

(alias-set! "ll" "ls -la")

;; Only the first token is looked up; the rest of the line is appended
;; untouched, so no positional substitution takes place.
(test-equal "ll a b -> ls -la a b (head replaced, args appended verbatim)"
  "ls -la a b" (alias-expand-line "ll a b"))

(test-equal "alias-ref returns the stored expansion"
  "ls -la" (alias-ref "ll"))

(test-assert "alias-names lists a defined alias"
  (and (member "ll" (alias-names)) #t))

;; --- recursion terminates on self- and mutual-reference ---

;; A self-referential alias must return its one-step expansion, not hang: the
;; head recurs on the second pass and the in-progress head-name set stops it.
(alias-set! "ls" "ls --color")
(test-equal "self-referential alias settles and does not loop"
  "ls --color" (alias-expand-line "ls"))
(alias-remove! "ls")

;; A mutual pair settles to the fixed point the guard produces rather than
;; cycling: aa -> bb -> aa x, and the second sight of aa halts expansion.
(alias-set! "aa" "bb")
(alias-set! "bb" "aa x")
(test-equal "mutual alias pair terminates at the guard's fixed point"
  "aa x" (alias-expand-line "aa"))
(alias-remove! "aa")
(alias-remove! "bb")

;; --- an aliased head classifies as its expansion's head ---

;; ll -> ls -la, whose head ls is on PATH, so the line classifies as a shell
;; command even though ll itself is not a binary.
(test-equal "aliased head resolving to a PATH command classifies as shell"
  'shell (classify-input "ll foo"))

;; An alias whose expansion head is a builtin classifies as a builtin.
(alias-set! "up" "cd ..")
(test-equal "aliased head resolving to a builtin classifies as builtin"
  'builtin (classify-input "up"))
(alias-remove! "up")

;; --- an aliased head is a known head (never command-not-found) ---

(test-equal "an aliased head is suppressed from command-not-found"
  #t (command-not-found-suppress? "ll"))

;; --- unalias reverts every effect of the alias ---

(alias-remove! "ll")

(test-equal "after removal the expansion is gone"
  #f (alias-ref "ll"))

(test-equal "after removal the head reverts to scheme"
  'scheme (classify-input "ll"))

(test-equal "after removal the head is unknown again"
  #f (command-not-found-suppress? "ll"))

;; --- a Scheme-classified alias runs its EXPANSION, not the bare name ---

;; An alias whose expansion is a Scheme form or literal classifies as 'scheme,
;; refuting the old "an alias never classifies as scheme" invariant.  The
;; dispatch's scheme arm reads and evaluates the alias-EXPANDED line (exec-line);
;; replaying that exact step here shows the expansion evaluates to its value,
;; whereas reading the raw alias name -- the previous behaviour -- raises an
;; unbound-variable error.
(alias-set! "answer" "42")
(test-equal "an alias to a literal classifies as scheme"
  'scheme (classify-input "answer"))
(test-equal "the scheme arm's read of exec-line yields the expansion's value"
  42 (eval (read (open-input-string (alias-expand-line "answer")))
           (interaction-environment)))

(alias-set! "addup" "(+ 1 2)")
(test-equal "an alias to a Scheme form evaluates the form via exec-line"
  3 (eval (read (open-input-string (alias-expand-line "addup")))
          (interaction-environment)))

;; Reading the RAW alias name instead (the bug this replaces) evaluates an
;; unbound symbol and errors -- the failure the scheme arm used to produce.
(test-assert "evaluating the raw alias name (the old behaviour) is an error"
  (guard (e [#t #t])
    (eval (read (open-input-string "answer")) (interaction-environment))
    #f))

(alias-remove! "answer")
(alias-remove! "addup")

;; ======================================================================
;; Builtin surface: the alias/unalias builtins write the SAME classifier
;; table, and builtin-cd-path cds to a literal path.
;; ======================================================================

;; --- the equals form defines, and the definition takes effect at once ---

;; `alias ll='ls -la'` tokenises to a single operand `ll=ls -la`, split on the
;; first `=`; the definition lands in the very table classify-input reads.
(run-builtin! "alias ll='ls -la'")
(test-equal "alias name=value defines the expansion"
  "ls -la" (alias-ref "ll"))
(test-equal "a defined alias head classifies as its expansion's class"
  'shell (classify-input "ll x"))
(test-equal "a defined alias head is a known head"
  #t (command-not-found-suppress? "ll"))

;; --- the space form defines from separate operands ---

;; `alias gg 'grep -n'` gives operands ("gg" "grep -n"): the name then the
;; expansion.
(run-builtin! "alias gg 'grep -n'")
(test-equal "alias name value defines the expansion (space form)"
  "grep -n" (alias-ref "gg"))

;; --- bare alias lists every definition as name=expansion ---

(let ([listing (with-output-to-string (lambda () (run-builtin! "alias")))])
  (test-assert "bare alias lists each definition as name=expansion"
    (and (substring? "ll=ls -la" listing)
         (substring? "gg=grep -n" listing))))

;; --- a builtin is never shadowable by an alias ---

;; Defining `cd` as an alias is refused, so cd keeps classifying as a builtin
;; and nothing is written to the table.
(run-builtin! "alias cd='ls'")
(test-equal "aliasing a builtin writes nothing"
  #f (alias-ref "cd"))
(test-equal "a builtin still classifies as a builtin after an alias attempt"
  'builtin (classify-input "cd /tmp"))

;; --- unalias removes, reverting classification ---

(run-builtin! "unalias ll")
(test-equal "unalias removes the expansion"
  #f (alias-ref "ll"))
(test-equal "after unalias the head reverts to scheme"
  'scheme (classify-input "ll"))
(run-builtin! "unalias gg")   ;; tidy the remaining definition
(test-equal "unalias of the space-form alias removes it too"
  #f (alias-ref "gg"))

;; unalias of an unknown name is a no-op (idempotent), not an error.
(test-assert "unalias of an unknown name does not error"
  (begin (run-builtin! "unalias zznosuchalias") #t))

;; --- builtin-cd-path cds to a literal path (no re-tokenising) ---

;; The directory name carries a space, a `;` and a `*`; only a verbatim chdir
;; reaches it. A "cd <name>" string rebuilt and re-parsed would split on the
;; space and `;` and glob the `*`, so a matching cwd here proves the path is
;; passed to chdir literally. Cleanup restores cwd and removes the dir even on
;; failure (dynamic-wind).
(let ([orig (cwd)]
      [tmpdir "/tmp/zzhafod nav;cd*test"])
  (define (cleanup!)
    (chdir orig)
    (when (file-exists? tmpdir) (delete-directory tmpdir)))
  (when (file-exists? tmpdir) (delete-directory tmpdir))
  (mkdir tmpdir)
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (builtin-cd-path tmpdir)
      (test-equal "builtin-cd-path cds to a literal metacharacter-named dir"
        tmpdir (cwd))
      (test-equal "builtin-cd-path reuses builtin-cd's OLDPWD bookkeeping"
        orig (getenv "OLDPWD")))
    (lambda () (cleanup!))))

;; --- builtin-cd-path enters a dir literally named "-" (no OLDPWD shortcut) ---

;; builtin-cd-path performs NO argument interpretation, so "-" is a literal
;; directory name here, not builtin-cd's $OLDPWD shortcut.  Create a "-" subdir
;; with a marker directory inside, point OLDPWD elsewhere, cd into the parent,
;; then (builtin-cd-path "-"): the marker is reachable only from inside the "-"
;; directory, so seeing it proves the literal cd landed there rather than
;; jumping to OLDPWD.  Using a marker (not a cwd string compare) keeps the
;; assertion immune to any /tmp symlink canonicalisation.  Cleanup removes the
;; marker and directories and restores cwd, even on failure.
(let* ([orig (cwd)]
       [parent "/tmp/zzhafod-dashdir-test"]
       [dashdir (string-append parent "/-")]
       [marker (string-append dashdir "/MARK")])
  (define (cleanup!)
    (chdir orig)
    (when (file-exists? marker) (delete-directory marker))
    (when (file-exists? dashdir) (delete-directory dashdir))
    (when (file-exists? parent) (delete-directory parent)))
  (cleanup!)                 ;; clear any leftover from a prior run
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (mkdir parent)
      (mkdir dashdir)
      (mkdir marker)
      (chdir parent)
      (setenv "OLDPWD" orig)   ;; a target "-" WOULD jump here if interpreted
      (builtin-cd-path "-")
      (test-assert "builtin-cd-path enters a dir literally named - (no OLDPWD shortcut)"
        (and (file-exists? "MARK")           ;; reachable only inside the - dir
             (not (string=? (cwd) orig)))))  ;; and not the OLDPWD jump target
    (lambda () (cleanup!))))

(test-end)
