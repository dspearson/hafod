;;; test/test-ls-colors.ss -- Freeze suite for the pure $LS_COLORS parser and its
;;; type/extension -> validated-SGR lookup.  Parsing, precedence (a type rule beats
;;; an extension rule; among extension rules the LAST-listed match wins; matching is
;;; case-insensitive), the built-in default palette used when $LS_COLORS is unset,
;;; and -- most importantly -- the hostile-input allow-list are all asserted here by
;;; direct call, entirely off a terminal.  A crafted $LS_COLORS value is untrusted:
;;; GNU ls does not sanitise it, so a value such as di=01;34m<ESC>[2J would clear the
;;; screen once it were wrapped in an SGR sequence.  Every value is allow-listed to
;;; [0-9;] before it can reach \e[...m, and the hostile-value rows below prove a
;;; rejected value yields no colour and lets through zero bytes outside that class.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod editor ls-colors)
              parse-ls-colors default-ls-colors current-ls-colors
              candidate-sgr valid-sgr?)
        (only (hafod environment) setenv)
        (chezscheme))

(test-begin "ls-colors")

;; ======================================================================
;; Helpers
;; ======================================================================

;; #t when STR carries no byte outside the SGR parameter class [0-9;].  This is the
;; same property valid-sgr? enforces, restated independently so a colour that
;; reaches the pen can be checked without trusting the procedure under test.
(define (sgr-clean? str)
  (let loop ([i 0])
    (or (= i (string-length str))
        (let ([c (string-ref str i)])
          (and (or (and (char<=? #\0 c) (char<=? c #\9))
                   (char=? c #\;))
               (loop (+ i 1)))))))

;; ======================================================================
;; === LS_COLORS parser ===
;; ======================================================================

;; Every supported key lands in the right bucket: the 2-letter type keys reach the
;; type table, a *.ext key reaches the ordered extension list.
(let ([table (parse-ls-colors "di=01;34:ln=01;36:ex=01;32:*.tar=01;31")])
  (test-equal "a directory type value parses" "01;34"
    (candidate-sgr table "anything" 'dir))
  (test-equal "a symlink type value parses" "01;36"
    (candidate-sgr table "anything" 'link))
  (test-equal "an executable type value parses" "01;32"
    (candidate-sgr table "anything" 'exec))
  (test-equal "an extension rule parses" "01;31"
    (candidate-sgr table "photo.tar" 'file)))

;; ======================================================================
;; === Precedence ===
;; ======================================================================

;; A type rule beats an extension rule: a directory named build.tar takes the
;; directory colour, never the .tar colour.
(let ([table (parse-ls-colors "di=01;34:*.tar=01;31")])
  (test-equal "type beats extension" "01;34"
    (candidate-sgr table "build.tar" 'dir)))

;; Among different suffixes the LAST-listed matching rule wins (list order, not the
;; longest suffix), so reversing the two entries swaps which colour a name resolves.
(let ([forward  (parse-ls-colors "*.gz=01;31:*.tar.gz=01;35")]
      [reversed (parse-ls-colors "*.tar.gz=01;35:*.gz=01;31")])
  (test-equal "last-listed extension wins" "01;35"
    (candidate-sgr forward "archive.tar.gz" 'file))
  (test-equal "reversing the order swaps the winner" "01;31"
    (candidate-sgr reversed "archive.tar.gz" 'file)))

;; The same suffix listed twice: the last write wins.
(let ([table (parse-ls-colors "*.log=01;31:*.log=01;32")])
  (test-equal "same-suffix last write wins" "01;32"
    (candidate-sgr table "run.log" 'file)))

;; Extension matching ignores case on both sides.
(let ([table (parse-ls-colors "*.JPG=01;35")])
  (test-equal "a lower-case name matches an upper-case rule" "01;35"
    (candidate-sgr table "photo.jpg" 'file))
  (test-equal "an upper-case name matches an upper-case rule" "01;35"
    (candidate-sgr table "PHOTO.JPG" 'file)))

;; ======================================================================
;; === Built-in default (unset $LS_COLORS) ===
;; ======================================================================

;; The default palette mirrors GNU dircolors: directory bold blue, symlink bold
;; cyan, executable bold green, and a plain file with no extension rule takes no
;; distinguishing colour (fi = 0).
(let ([table (default-ls-colors)])
  (test-equal "default directory colour"  "01;34" (candidate-sgr table "d" 'dir))
  (test-equal "default symlink colour"    "01;36" (candidate-sgr table "l" 'link))
  (test-equal "default executable colour" "01;32" (candidate-sgr table "x" 'exec))
  (test-equal "default plain-file colour" "0"      (candidate-sgr table "readme" 'file)))

;; current-ls-colors re-resolves when $LS_COLORS toggles set <-> unset: an unset
;; value is a DISTINCT cache key, so a set drive followed by an unset drive returns
;; the default rather than a stale memo of the set value.
(let ()
  (setenv "LS_COLORS" "di=01;31")
  (let ([while-set (candidate-sgr (current-ls-colors) "d" 'dir)])
    (setenv "LS_COLORS" #f)
    (let ([while-unset (candidate-sgr (current-ls-colors) "d" 'dir)])
      (test-equal "current-ls-colors honours a set value" "01;31" while-set)
      (test-equal "current-ls-colors re-resolves to the default when unset"
        "01;34" while-unset))))

;; ======================================================================
;; === Hostile-input allow-list ===
;; ======================================================================

;; valid-sgr? is the gate: it accepts an SGR parameter string and rejects anything
;; carrying a byte outside [0-9;].
(test-assert "valid-sgr? accepts a clean parameter string" (valid-sgr? "01;34"))
(test-assert "valid-sgr? accepts a bare zero" (valid-sgr? "0"))
(test-assert "valid-sgr? rejects an embedded escape and clear-screen"
  (not (valid-sgr? "01;34m\x1b;[2J")))
(test-assert "valid-sgr? rejects a non-numeric special value"
  (not (valid-sgr? "target")))

;; A crafted directory value carrying ESC + clear-screen is rejected to #f, so no
;; colour reaches the pen; a non-numeric ln=target special value is likewise dropped
;; rather than crashing.  This row is non-vacuous: an implementation that returned
;; the raw value unfiltered would hand back the escape-bearing string here.
(let ([hostile (parse-ls-colors "di=01;34m\x1b;[2J:ln=target")])
  (test-equal "a hostile directory value yields no colour" #f
    (candidate-sgr hostile "downloads" 'dir))
  (test-equal "a non-numeric link value yields no colour" #f
    (candidate-sgr hostile "link" 'link))
  ;; Whatever candidate-sgr returns anywhere, it never lets an unsafe byte through:
  ;; every non-#f result is [0-9;] only.
  (let ([r (candidate-sgr hostile "downloads" 'dir)])
    (test-assert "a returned colour carries no byte outside [0-9;]"
      (or (not r) (sgr-clean? r)))))

(test-end)
