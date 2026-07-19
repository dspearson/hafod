;;; test/test-shell-lex.ss -- The pure shell-line lexer and its role-colour map.
;;;
;;; shell-lex splits a shell command line into a list of (role start end) spans
;;; in a single left-to-right pass, marking WHERE the head, flags, redirections,
;;; quoted strings and path-shaped arguments sit.  It is the foundation the live
;;; shell-line colouriser rests on, so this suite proves it in isolation -- no
;;; renderer, no terminal, no filesystem -- driving the function directly and
;;; asserting each span by role AND by its (start end) offsets.  The offset checks
;;; are what make the proof non-vacuous: a naive tokeniser that lumped every word
;;; into one role, or missed the flag/redirection/path distinctions, or stopped a
;;; quoted run at the first inner quote, would fail here even though it "split the
;;; line into tokens".
;;;
;;; It also proves the fixed role->256-colour map the renderer will emit, and that
;;; the spans TILE the whole line -- every character covered by exactly one span,
;;; with no gap and no overlap -- so the windowed renderer draws each character
;;; precisely once.
;;; Copyright (c) 2026, hafod contributors.

;; Resolve the compiled libraries by ABSOLUTE path from the launch directory, so
;; library resolution stays independent of any later cd -- the idiom the nav
;; suites use.  This suite never cds, but the pinning costs nothing and keeps the
;; header consistent across suites.
(library-directories
  (let ([root (current-directory)])
    (list (cons (string-append root "/src") (string-append root "/src"))
          (cons root root))))
(import (test runner)
        (only (hafod editor render) shell-lex shell-role-colour)
        (chezscheme))

(test-begin "shell-lex")

;; ======================================================================
;; Helpers
;; ======================================================================

;; The role of the FINAL span of LINE.  Used to check how an argument classifies
;; without the first-word head rule masking it: put the token of interest second
;; ("x <tok>") and read back the role assigned to it.
(define (final-role line)
  (let ([spans (shell-lex line)])
    (and (pair? spans) (caar (reverse spans)))))

;; Do LINE's spans tile [0, len) exactly?  Walks the (role start end) list
;; checking each span is non-empty and begins where the previous ended, the first
;; at 0 and the last at (string-length line) -- so every character is covered once
;; with no gap and no overlap.  The empty line (no spans, length 0) tiles.
(define (tiles? line)
  (let ([spans (shell-lex line)] [len (string-length line)])
    (let loop ([spans spans] [pos 0])
      (if (null? spans)
          (= pos len)
          (let ([s (cadr (car spans))] [e (caddr (car spans))])
            (and (= s pos) (< s e) (loop (cdr spans) e)))))))

;; ======================================================================
;; Span roles and offsets
;; ======================================================================

;; The headline: a plain command line splits into a positional head, a flag, and
;; a plain argument, with the whitespace runs between them each their own span.
;; Asserted as the whole list -- role and offsets -- so nothing about the split is
;; left to chance.
(test-equal "head, flag and arg split with their offsets"
  '((head 0 4) (whitespace 4 5) (flag 5 7) (whitespace 7 8) (arg 8 11))
  (shell-lex "grep -n foo"))

;; Every distinguishing role at once: a quoted string (spaces and all), a flag, a
;; redirection operator, and a path-shaped final argument.  A tokeniser that did
;; not separate these -- that let the space in 'a b' break the word, or treated
;; '>' as an ordinary character, or did not notice the slash in /tmp/x -- produces
;; a different list and fails.
(test-equal "string, flag, redirection and path-arg each get their own span"
  '((head 0 3) (whitespace 3 4) (string 4 9) (whitespace 9 10)
    (flag 10 12) (whitespace 12 13) (redir 13 14) (whitespace 14 15)
    (path-arg 15 21))
  (shell-lex "cat 'a b' -n > /tmp/x"))

;; ======================================================================
;; Redirection variants -- each is one redir span
;; ======================================================================

(test-equal "append '>>' is a single redirection span"
  '((head 0 1) (whitespace 1 2) (redir 2 4) (whitespace 4 5) (arg 5 6))
  (shell-lex "a >> b"))

(test-equal "input '<' is a single redirection span"
  '((head 0 1) (whitespace 1 2) (redir 2 3) (whitespace 3 4) (arg 4 5))
  (shell-lex "a < b"))

(test-equal "fd redirection '2>' is a single redirection span"
  '((head 0 1) (whitespace 1 2) (redir 2 4) (whitespace 4 5) (arg 5 6))
  (shell-lex "a 2> b"))

(test-equal "combined redirection '&>' is a single redirection span"
  '((head 0 1) (whitespace 1 2) (redir 2 4) (whitespace 4 5) (arg 5 6))
  (shell-lex "a &> b"))

;; ======================================================================
;; Path-shaped arguments vs plain arguments
;; ======================================================================

;; A word carrying a slash -- whether a './' or '../' or '~/' prefix or an
;; interior 'a/b' -- is path-shaped; a bare word is not.  The head-position rule
;; is kept out of the way by reading the SECOND word's role.

(test-equal "a './'-prefixed argument is path-shaped"
  'path-arg (final-role "x ./rel"))

(test-equal "a '../'-prefixed argument is path-shaped"
  'path-arg (final-role "x ../up"))

(test-equal "a '~/'-prefixed argument is path-shaped"
  'path-arg (final-role "x ~/home"))

(test-equal "an argument with an interior slash is path-shaped"
  'path-arg (final-role "x a/b"))

(test-equal "a bare argument is a plain arg, not path-shaped"
  'arg (final-role "x plain"))

;; ======================================================================
;; Edge cases
;; ======================================================================

(test-equal "the empty line lexes to no spans"
  '() (shell-lex ""))

(test-equal "leading whitespace is its own span before the head"
  '((whitespace 0 2) (head 2 4))
  (shell-lex "  ls"))

;; An unterminated quote runs to the end of the string rather than swallowing the
;; rest as words or dropping the tail.
(test-equal "an unterminated quote runs to end-of-string as one string span"
  '((head 0 4) (whitespace 4 5) (string 5 10))
  (shell-lex "echo 'oops"))

;; A backslash-escaped quote inside a double-quoted run does NOT close it: the
;; string span runs past the escaped quote to the real closing quote.  A scanner
;; that ignored the backslash would close early at offset 8 and give (string 5 9),
;; so this offset is the whole point of the assertion.
(test-equal "a backslash-escaped quote stays inside the string span"
  '((head 0 4) (whitespace 4 5) (string 5 11))
  (shell-lex "echo \"a\\\"b\""))

;; ======================================================================
;; Tiling -- every character covered exactly once
;; ======================================================================

(test-assert "a plain line tiles [0, len) with no gap or overlap"
  (tiles? "grep -n foo"))

(test-assert "a line with a string, flag, redirection and path tiles exactly"
  (tiles? "cat 'a b' -n > /tmp/x"))

(test-assert "the empty line tiles"
  (tiles? ""))

(test-assert "leading whitespace tiles"
  (tiles? "  ls"))

(test-assert "an unterminated quote still tiles to end-of-string"
  (tiles? "echo 'oops"))

(test-assert "an escaped quote inside a string still tiles"
  (tiles? "echo \"a\\\"b\""))

;; ======================================================================
;; Role -> 256-colour index map
;; ======================================================================

;; The head is the one role whose colour depends on a fact the renderer supplies:
;; resolvable heads are green (2), unresolved heads red (1).  The map itself makes
;; no lookup -- resolvability is passed in.
(test-equal "a resolvable head is green (256-index 2)"
  2 (shell-role-colour 'head #t))

(test-equal "an unresolved head is red (256-index 1)"
  1 (shell-role-colour 'head #f))

;; The remaining coloured roles are fixed regardless of the resolvability argument.
(test-equal "a quoted string is yellow (256-index 3)"
  3 (shell-role-colour 'string #f))

(test-equal "a flag is cyan (256-index 6)"
  6 (shell-role-colour 'flag #f))

(test-equal "a redirection is magenta (256-index 5)"
  5 (shell-role-colour 'redir #f))

;; Uncoloured roles return #f.  path-arg is deliberately uncoloured here: a
;; missing-path style is a filesystem decision the renderer makes, never this map.
(test-assert "a plain arg is uncoloured"
  (not (shell-role-colour 'arg #f)))

(test-assert "a path-arg is uncoloured by the map (existence is the renderer's call)"
  (not (shell-role-colour 'path-arg #f)))

(test-assert "whitespace is uncoloured"
  (not (shell-role-colour 'whitespace #f)))

(test-end)
