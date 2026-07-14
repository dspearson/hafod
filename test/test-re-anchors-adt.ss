#!chezscheme
;;; test-re-anchors-adt.ss -- Line-anchor (bol/eol) matching through the ADT SRE
;;; compiler (the sre->regexp / regexp->posix-string path).
;;;
;;; hafod turns an SRE into a POSIX pattern in two independent compilers, and a
;;; line-anchor fix in one does nothing for the other. This suite exercises the
;;; ADT compiler, the sibling of the rx macro's compiler covered by the rx suite.
;;; Where the rx compiler silently degrades bol/eol to string anchors, the ADT
;;; compiler currently RAISES ("Beginning-of-line/End-of-line not supported"), so
;;; every line-anchor assertion here fails today by raising -- a clean failure the
;;; runner reports as an exception. Each becomes a real interior-boundary match
;;; once the ADT compiler learns line anchors.
;;;
;;; It carries the same behaviour-preserving guards as the rx suite so a fix that
;;; leaks the newline flag into anchor-free patterns is caught on this path too.
;;;
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod re)
        (only (hafod internal re-engine)
              line-aware-anchors-ok? gnu-buffer-anchors-available?)
        (chezscheme))

(test-begin "Line anchors (bol/eol) via the ADT compiler")

;; Reduce a match object to its whole-match (start . end) span, or #f when the
;; search found nothing.
(define (span m) (and m (cons (match:start m 0) (match:end m 0))))

;; The POSIX string an ADT pattern compiles to (regexp->posix-string returns four
;; values; the emitted string is the first).
(define (posix-of re)
  (let-values (((s lev pc sm) (regexp->posix-string re))) s))

;; ===== bol/eol match at an interior line boundary =====
;; The ADT compiler raises on a line anchor today, so each of these fails by
;; raising (reported as an exception). Once bol/eol are supported they match at
;; the interior boundary shown, exactly as the rx suite requires of its compiler.

;; bol before "def" matches at the start of the second line (offset 4).
(test-equal "bol matches at the start of an interior line"
  '(4 . 7)
  (span (regexp-search (sre->regexp '(: bol "def")) "abc\ndef")))

;; eol after "abc" matches at the end of the first line (offset 3).
(test-equal "eol matches at the end of an interior line"
  '(0 . 3)
  (span (regexp-search (sre->regexp '(: "abc" eol)) "abc\ndef")))

;; ===== bos stays string-anchored beside a line anchor (the crux) =====
;;
;; Mixing a STRING anchor (bos/eos) with a LINE anchor (bol/eol) is only
;; expressible where the libc has the GNU buffer anchors \` and \': under
;; REG_NEWLINE, plain "^" means line-start, so bos can only keep its string
;; meaning if there is a distinct buffer-start anchor to compile it to. A glibc
;; has them; BSD (macOS) and musl do not, and there the pattern cannot be
;; expressed AT ALL. The compiler is contracted to say so -- make-line-aware-regexp
;; raises -- rather than quietly demote bos to bol and return a wrong answer.
;;
;; Both halves of that contract are asserted below, selected by asking the engine
;; what THIS libc can express. The capability, not the platform name, is the real
;; property: a musl Linux fails the probe exactly as macOS does, and would be
;; misjudged by an os-family test.
(define gnu-anchors? (gnu-buffer-anchors-available?))

;; Run THUNK, reporting 'inexpressible if -- and only if -- it raised the
;; contracted refusal. Any OTHER exception propagates and fails the test as an
;; exception, so this cannot paper over an unrelated bug.
(define (value-or-inexpressible thunk)
  (guard (e [(and (who-condition? e)
                  (eq? (condition-who e) 'make-line-aware-regexp))
             'inexpressible])
    (thunk)))

;; bos pins the match to the string start while eol matches at the first line end
;; -- where the pattern is expressible at all.
(test-equal "bos stays at string start while eol is line-aware"
  (if gnu-anchors? '(0 . 3) 'inexpressible)
  (value-or-inexpressible
    (lambda ()
      (span (regexp-search (sre->regexp '(: bos "abc" eol)) "abc\ndef")))))

;; The companion of the crux: bos must NOT match "abc" where it begins a later
;; line ("abc" starts the second line of "xy\nabc"). Where the buffer anchors are
;; absent, refusing to compile is the correct answer -- and is the whole point of
;; the seam, since the alternative is a silent false match here.
(test-equal "bos rejects a match that starts a later line, not the string"
  (if gnu-anchors? #f 'inexpressible)
  (value-or-inexpressible
    (lambda ()
      (regexp-search? (sre->regexp '(: bos "abc" eol)) "xy\nabc"))))

;; ===== bos/eos-only patterns keep string semantics (byte-identical) =====
;; No bol/eol node, so the compiler does not raise and the pattern keeps string
;; semantics: ^abc$ over "abc\nx" does not match. Green today and after the fix.
(test-equal "bos/eos-only pattern keeps string (not line) semantics"
  #f
  (regexp-search? (sre->regexp '(: bos "abc" eos)) "abc\nx"))

;; ===== emitted POSIX strings for anchor-free patterns are unchanged =====
(test-equal "any still compiles to \".\""
  "." (posix-of (sre->regexp 'any)))
(test-equal "bos still compiles to \"^\""
  "^" (posix-of (sre->regexp 'bos)))
(test-equal "eos still compiles to \"$\""
  "$" (posix-of (sre->regexp 'eos)))
(test-equal "a negated class still compiles to \"[^x]\""
  "[^x]" (posix-of (sre->regexp '(~ "x"))))

;; ===== the newline flag's side effects stay neutralised =====
;; These mirror the rx suite's preservation guards, with the SAME teeth (the
;; wrong-fix offsets below). Unlike the rx mirrors, they are NOT reachable on this
;; path today -- the ADT compiler raises for a line anchor, so each reads as a
;; clean failure now. Once bol/eol are supported they become reachable
;; preservation guards and must hold at the offsets shown.

;; (a) A negated class inside a line-aware pattern keeps spanning the newline:
;; "[^x]+" over "a\nb" is (0 . 3). Teeth: a fix that omits the "[^Y]" ->
;; "[^Y]|<newline>" rewrite would drop this to (0 . 1).
(test-equal "a negated class spans the newline inside a line-aware pattern"
  '(0 . 3)
  (span (regexp-search (sre->regexp '(: bol (+ (~ "x")))) "a\nb")))

;; (b) nonl ("[^\n]") inside a line-aware pattern does NOT cross the newline: over
;; "ab\ncd" it stops at (0 . 2). Teeth: a fix that wrongly appends "|<newline>" to
;; nonl would span it to (0 . 5).
(test-equal "nonl stops at the newline inside a line-aware pattern"
  '(0 . 2)
  (span (regexp-search (sre->regexp '(: bol (+ nonl))) "ab\ncd")))

;; (c) any (".") inside a line-aware pattern keeps spanning the newline: ".+" over
;; "a\nb" is (0 . 3). Teeth: a fix that omits the "." -> ".|<newline>" rewrite
;; would drop this to (0 . 1).
(test-equal "any spans the newline inside a line-aware pattern"
  '(0 . 3)
  (span (regexp-search (sre->regexp '(: bol (+ any))) "a\nb")))

;; ===== a negated NAMED class whose set already contains the newline =====
;; The divergence guard for (~ ascii): this compiler judges the excluded set with
;; char-set-contains?, so it already excludes the newline from (~ ascii) (ascii is
;; [\x01-\x7f], which spans 0x0A) and emits [^\x01-\x7f] without a "|<newline>"
;; restoration. Both assertions are green here before and after the sibling rx
;; fix -- their job is to LOCK the two compilers into agreement, so a future
;; regression on either path (rx or ADT) trips against the same expectation.
(test-assert "a negated ascii class does not match a newline inside a line-aware pattern"
  (not (regexp-search? (sre->regexp '(: bol (~ ascii))) "\n")))
(test-assert "a negated ascii class still matches a non-ascii byte under line-aware anchors"
  (regexp-search? (sre->regexp '(: bol (~ ascii))) "\xff;"))

;; ===== the fail-loud capability seam (the pure predicate) =====
;; A mixed pattern (a line anchor beside a string anchor) needs the GNU buffer
;; anchors to keep bos/eos string-anchored under REG_NEWLINE. Where the libc
;; lacks them the ADT compiler fails loud rather than silently mis-match. The
;; decision is the pure predicate below, exercised here with an injected
;; capability flag so the mixed-and-absent case is covered even on a libc that
;; HAS the anchors.

;; Mixed line+string anchors with no GNU buffer anchors: not expressible.
(test-equal "mixed line and string anchors need the GNU buffer anchors"
  #f
  (line-aware-anchors-ok? #t #t #f))

;; The same mixed pattern IS expressible where the GNU buffer anchors are present.
(test-equal "mixed line and string anchors are fine with the GNU buffer anchors"
  #t
  (line-aware-anchors-ok? #t #t #t))

;; A pure line-anchor pattern (no bos/eos) is portable without the GNU anchors.
(test-equal "a pure line-anchor pattern needs no GNU buffer anchors"
  #t
  (line-aware-anchors-ok? #t #f #f))

;; A pattern that is not line-aware never needs the GNU anchors.
(test-equal "a pattern that is not line-aware never needs the GNU anchors"
  #t
  (line-aware-anchors-ok? #f #t #f))

(test-end)
