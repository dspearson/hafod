;;; Tests for auto-cd: a bare shell-mode line naming an existing directory
;;; changes into it, unless a real command, builtin, alias, or bound Scheme
;;; identifier claims the token first, and always via a literal chdir.
;;; Copyright (c) 2026 Dominic Pearson.

;; Resolve the compiled libraries by ABSOLUTE path, derived from the launch
;; directory while it is still the repo root: the filesystem assertions below
;; chdir into a temporary directory, and a compiled library is located lazily at
;; its first binding reference, so a relative libdir could otherwise be resolved
;; against that temporary directory.  Pinning absolute paths keeps resolution
;; independent of any in-suite cd.
(library-directories
  (let ([root (current-directory)])
    (list (cons (string-append root "/src") (string-append root "/src"))
          (cons root root))))
(import (test runner)
        (only (hafod interactive) auto-cd? auto-cd-target auto-cd-decision run-auto-cd!)
        (only (hafod shell builtins) builtin-cd-path)
        (only (hafod shell classifier) rebuild-path-cache!)
        (hafod process-state)
        (hafod environment)
        (except (chezscheme) exit open-input-file open-output-file getenv))

(test-begin "nav-auto-cd")

;; Build the PATH cache first, so command-not-found-suppress? (which auto-cd
;; consults through auto-cd-target) sees the real command heads and a genuine
;; command name would decline auto-cd.  The directory names chosen below are not
;; real commands, so they remain eligible.
(rebuild-path-cache!)

;; --- default toggle ---

(test-equal "auto-cd? defaults on"
  #t (auto-cd?))

;; Everything filesystem-touching happens inside a temporary parent directory P
;; and is unwound on exit: cwd restored, every artefact removed, even on failure.
(let* ([orig (cwd)]
       [P "/tmp/zzhafod-autocd-test"]
       [banana (string-append P "/banana")]
       [carrot (string-append P "/carrot")]
       ;; A directory whose LITERAL name carries a space and a `;` -- the shape a
       ;; naive "cd <name>" rebuild-and-reparse would split and execute.
       [evil (string-append P "/; touch PWNED")]
       [pwned (string-append P "/PWNED")])
  (define (rmdir-if d) (when (file-exists? d) (delete-directory d)))
  (define (cleanup!)
    (chdir orig)
    (when (file-exists? pwned) (delete-file pwned))
    (rmdir-if banana)
    (rmdir-if carrot)
    (rmdir-if evil)
    (rmdir-if P))
  (cleanup!)                 ;; clear any leftover from a prior run
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (mkdir P)
      (mkdir banana)
      (mkdir carrot)
      (mkdir evil)
      (chdir P)              ;; the single-token tests run with cwd = P

      ;; --- a bare existing-directory token fires (target and decision) ---

      ;; auto-cd-target yields the (expanded) directory word; auto-cd-decision
      ;; adds the mode + toggle gate and yields the same word by default.
      (test-equal "a bare existing-dir token -> its path (target)"
        "banana" (auto-cd-target "banana"))
      (test-equal "a bare existing-dir token -> its path (decision)"
        "banana" (auto-cd-decision "banana"))

      ;; --- a non-directory token declines ---

      (test-equal "a token naming no directory declines"
        #f (auto-cd-target "nosuchdir-zz"))

      ;; --- a multi-word line declines (the single-bare-token gate) ---

      (test-equal "a multi-word line declines"
        #f (auto-cd-target "banana carrot"))

      ;; A line carrying a shell operator tokenises to several words (the `;` is
      ;; dropped as an operator, leaving two words), so the single-word gate
      ;; rejects it before any filesystem test -- auto-cd never fires on an
      ;; injection-shaped line.
      (test-equal "a line carrying a shell operator declines"
        #f (auto-cd-target "; touch PWNED"))

      ;; --- a bound Scheme identifier beats a same-named directory ---

      ;; With a top-level binding named `carrot` (and a subdirectory also named
      ;; carrot present), auto-cd declines: command-not-found-suppress? treats the
      ;; token as known, so a real command or a bound variable always wins.
      (eval '(define carrot 1) (interaction-environment))
      (test-equal "a bound Scheme identifier beats a same-named directory"
        #f (auto-cd-target "carrot"))

      ;; --- the toggle lives in the decision, not the target ---

      (test-equal "the toggle off disables the decision"
        #f (parameterize ([auto-cd? #f]) (auto-cd-decision "banana")))
      ;; The target itself never consults the toggle, so it still resolves.
      (test-equal "the target ignores the toggle"
        "banana" (parameterize ([auto-cd? #f]) (auto-cd-target "banana")))

      ;; --- run-auto-cd! actually changes directory ---

      (run-auto-cd! banana)
      (test-equal "run-auto-cd! leaves cwd at the directory"
        banana (cwd))

      ;; --- injection-safety: the metacharacter-named directory is entered
      ;;     literally, with no side-effect file (the headline) ---

      ;; Return to P, then enter the metacharacter-named directory by the same
      ;; literal-cd seam run-auto-cd! uses.  A "cd <name>" string rebuilt and
      ;; reparsed would split on the space and `;` and run `touch PWNED`; a
      ;; verbatim chdir cannot.  So cwd lands exactly on the directory AND no
      ;; PWNED file appears in P.
      (chdir P)
      (builtin-cd-path evil)
      (test-equal "a metacharacter-named directory is entered literally"
        evil (cwd))
      (test-assert "no side-effect file is created by the metacharacter name"
        (not (file-exists? pwned))))
    (lambda () (cleanup!))))

(test-end)
