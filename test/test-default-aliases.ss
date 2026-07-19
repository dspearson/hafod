;;; test/test-default-aliases.ss -- The default interactive alias set and the
;;; master plain-shell! enhancement switch.
;;;
;;; Proves install-default-aliases! populates the curated set (a platform-aware
;;; colourised ls, its long-listing variants, and the -i safety aliases), that
;;; those aliases actually expand on a command line, that a user's own alias is
;;; never clobbered by the default, and that plain-shell! flips every interactive
;;; enhancement off in one call.  Classification/expansion run against the real
;;; classifier alias table, PTY-free.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod shell default-aliases) default-aliases? install-default-aliases!)
        (only (hafod shell classifier)
              alias-ref alias-set! alias-remove! alias-expand-line)
        (only (hafod internal platform) os-family)
        (only (hafod config) plain-shell!)
        (only (hafod interactive) interactive-enhancements? auto-cd?)
        (only (hafod editor render) shell-highlight? shell-highlight-paths?)
        (only (hafod editor editor) abbr-expand? history-search-mode)
        (chezscheme))

;; Naive substring search (no srfi-13 dependency in this suite).
(define (has-substring? hay needle)
  (let ([hl (string-length hay)] [nl (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nl) hl) #f]
        [(string=? (substring hay i (+ i nl)) needle) #t]
        [else (loop (+ i 1))]))))

;; The colour flag this platform's ls default carries.
(define ls-colour-flag (if (eq? os-family 'linux) "--color=auto" "-G"))

(test-begin "default-aliases")

;; Start from a clean table for the names this suite touches.
(for-each alias-remove! '("ls" "ll" "la" "l" "grep" "egrep" "fgrep"
                          "df" "du" "rm" "cp" "mv"))

;; ======================================================================
;; (1) install-default-aliases! populates the curated set.
;; ======================================================================

(install-default-aliases!)

(test-equal "default: ls is the platform-colourised listing"
  (if (eq? os-family 'linux) "ls --color=auto" "ls -G")
  (alias-ref "ls"))

(test-equal "default: ll is a human long listing"
  "ls -lh" (alias-ref "ll"))

(test-equal "default: rm carries the -i safety flag"
  "rm -i" (alias-ref "rm"))

(test-equal "default: grep is colourised"
  "grep --color=auto" (alias-ref "grep"))

;; The aliases actually expand on a typed line.
(test-assert "default: an ls command expands to the colourised form"
  (has-substring? (alias-expand-line "ls /tmp") ls-colour-flag))

(test-assert "default: an ll command expands to the long-listing form"
  (has-substring? (alias-expand-line "ll /tmp") "-lh"))

;; ======================================================================
;; (2) A user's own alias is never clobbered by the default.
;; ======================================================================

(alias-remove! "ls")
(alias-set! "ls" "eza --icons --group-directories-first")
(install-default-aliases!)
(test-equal "default: a user-defined ls alias survives a default install"
  "eza --icons --group-directories-first" (alias-ref "ls"))

;; A name the user did NOT define is still installed alongside it.
(alias-remove! "du")
(install-default-aliases!)
(test-equal "default: an undefined alias is still installed"
  "du -h" (alias-ref "du"))

;; ======================================================================
;; (3) plain-shell! flips every interactive enhancement off at once.
;; ======================================================================

(plain-shell!)

(test-assert "plain: the master enhancement gate is off"
  (not (interactive-enhancements?)))
(test-assert "plain: default aliases are off"
  (not (default-aliases?)))
(test-assert "plain: shell highlighting is off"
  (not (shell-highlight?)))
(test-assert "plain: path highlighting is off"
  (not (shell-highlight-paths?)))
(test-assert "plain: auto-cd is off"
  (not (auto-cd?)))
(test-assert "plain: abbreviations are off"
  (not (abbr-expand?)))
(test-assert "plain: history recall reverts to prefix"
  (eq? (history-search-mode) 'prefix))

(test-end)
