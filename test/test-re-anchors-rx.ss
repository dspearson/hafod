#!chezscheme
;;; test-re-anchors-rx.ss -- Line-anchor (bol/eol) matching through the rx macro's
;;; SRE compiler.
;;;
;;; bol and eol are LINE anchors: in multi-line text they match at every interior
;;; line boundary, not only at the string ends. This suite drives the rx macro's
;;; expand-time SRE compiler and asserts a match AT an interior newline, so a
;;; compiler that quietly degrades bol/eol to the string anchors bos/eos (a bare
;;; ^/$ with no newline handling) is caught -- such a degradation returns #f here
;;; because the multi-line subject neither starts nor ends with the anchored text.
;;;
;;; The suite also guards the behaviour that must NOT change when line-awareness is
;;; switched on: bos/eos stay string-anchored, an anchor-free "." keeps matching a
;;; newline, negated classes keep spanning it, and the POSIX strings emitted for
;;; anchor-free patterns are byte-for-byte unchanged.
;;;
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod re)
        (only (hafod internal re-engine)
              compiled-regexp-type-posix-string gnu-buffer-anchors-available?)
        (chezscheme))

(test-begin "Line anchors (bol/eol) via the rx macro")

;; Reduce a match object to its whole-match (start . end) span, or #f when the
;; search found nothing -- the shape every line-anchor assertion compares against.
;; A degraded string anchor yields #f, which is visibly distinct from any span.
(define (span m) (and m (cons (match:start m 0) (match:end m 0))))

;; ===== bol/eol match at an interior line boundary =====
;; Each subject carries an embedded newline and anchors the literal on a line that
;; is NOT the first or last, so only a genuine line anchor can match it.

;; bol before "def" matches at the start of the second line (offset 4), never at
;; the string start -- "abc\ndef" does not begin with "def".
(test-equal "bol matches at the start of an interior line"
  '(4 . 7)
  (span (regexp-search (rx (: bol "def")) "abc\ndef")))

;; eol after "abc" matches at the end of the first line (offset 3), before the
;; interior newline -- not only at the string end.
(test-equal "eol matches at the end of an interior line"
  '(0 . 3)
  (span (regexp-search (rx (: "abc" eol)) "abc\ndef")))

;; bol and eol together bracket a whole interior line: "abc" is the middle line of
;; "x\nabc\ny", so the match spans offsets 2..5.
(test-equal "bol and eol bracket a whole interior line"
  '(2 . 5)
  (span (regexp-search (rx (: bol "abc" eol)) "x\nabc\ny")))

;; ===== bos stays string-anchored beside a line anchor (the crux) =====
;; Mixing the string-start anchor bos with the line-end anchor eol: bos must stay
;; a STRING anchor even though the same pattern is line-aware.
;;
;; That is only expressible where the libc has the GNU buffer anchors \` and \':
;; under REG_NEWLINE a plain "^" means line-start, so bos keeps its string meaning
;; only if there is a distinct buffer-start anchor to compile it to. A glibc has
;; them; BSD (macOS) and musl do not, and there the pattern cannot be expressed AT
;; ALL -- so the compiler refuses it loudly instead of quietly demoting bos to bol
;; and returning a wrong answer. Both halves of the contract are asserted, selected
;; by asking the engine what THIS libc can express: the capability, not the platform
;; name, is the real property (a musl Linux fails the probe exactly as macOS does).
(define gnu-anchors? (gnu-buffer-anchors-available?))

;; Run THUNK, reporting 'inexpressible if -- and only if -- it raised the contracted
;; refusal. Any OTHER exception propagates and fails the test as an exception, so
;; this cannot paper over an unrelated bug.
(define (value-or-inexpressible thunk)
  (guard (e [(and (who-condition? e)
                  (eq? (condition-who e) 'make-line-aware-regexp))
             'inexpressible])
    (thunk)))

;; bos pins the match to the string start while eol matches at the first line end,
;; so "abc" at the very start, closed by the interior newline, matches at (0 . 3).
(test-equal "bos stays at string start while eol is line-aware"
  (if gnu-anchors? '(0 . 3) 'inexpressible)
  (value-or-inexpressible
    (lambda () (span (regexp-search (rx (: bos "abc" eol)) "abc\ndef")))))

;; The companion of the crux: bos must NOT match "abc" merely because it begins a
;; later LINE. Here "abc" starts the second line of "xy\nabc" (a line start, not
;; the string start), so bos rejects it -- a fix that turned bos into a line anchor
;; would wrongly match here. Where the buffer anchors are absent, refusing to
;; compile is the correct answer, and is precisely what stops that false match.
(test-equal "bos rejects a match that starts a later line, not the string"
  (if gnu-anchors? #f 'inexpressible)
  (value-or-inexpressible
    (lambda () (regexp-search? (rx (: bos "abc" eol)) "xy\nabc"))))

;; ===== bos/eos-only patterns keep string semantics (byte-identical) =====
;; A pattern whose only anchors are bos/eos is not line-aware and must compile as
;; before: ^abc$ over "abc\nx" does not match, because eos is the STRING end and
;; "abc" is only the first line. Stays #f both today and after the fix.
(test-equal "bos/eos-only pattern keeps string (not line) semantics"
  #f
  (regexp-search? (rx (: bos "abc" eos)) "abc\nx"))

;; ===== emitted POSIX strings for anchor-free patterns are unchanged =====
;; Line-awareness is gated on the presence of a bol/eol node, so a pattern without
;; one must compile byte-for-byte as before. These pin the exact emission.
(test-equal "any still compiles to \".\""
  "." (compiled-regexp-type-posix-string (rx any)))
(test-equal "bos still compiles to \"^\""
  "^" (compiled-regexp-type-posix-string (rx bos)))
(test-equal "eos still compiles to \"$\""
  "$" (compiled-regexp-type-posix-string (rx eos)))
(test-equal "a negated class still compiles to \"[^x]\""
  "[^x]" (compiled-regexp-type-posix-string (rx (~ "x"))))

;; ===== the newline flag must not leak into non-line-aware patterns =====
;; "." matches a newline today (any compiles to "." with no newline flag). A
;; pattern that never mentions bol/eol must keep that: "a.b" spans "a\nb".
(test-assert "non-line-aware \".\" still matches a newline"
  (regexp-search? (rx (: "a" any "b")) "a\nb"))

;; ===== the newline flag's side effects stay neutralised =====
;; Switching the newline flag on for a line-aware pattern would, unless each side
;; effect is undone, silently change what "." and negated classes match. Each
;; guard below is GREEN today (bol degrades to a string start at offset 0 and no
;; newline flag is set, so the classes already span the newline) and must STAY at
;; the SAME offset once line-awareness lands. Each also bites a specific half-done
;; fix.

;; (a) A negated class inside a line-aware pattern keeps spanning the newline:
;; "[^x]+" over "a\nb" is the whole string (0 . 3). Teeth: a fix that sets the
;; newline flag but omits the "[^Y]" -> "[^Y]|<newline>" rewrite would drop this
;; to (0 . 1), the class stopping at the newline.
(test-equal "a negated class spans the newline inside a line-aware pattern"
  '(0 . 3)
  (span (regexp-search (rx (: bol (+ (~ "x")))) "a\nb")))

;; (b) nonl ("[^\n]") inside a line-aware pattern does NOT cross the newline: over
;; "ab\ncd" it stops at the first line (0 . 2). Teeth: a fix that wrongly appends
;; "|<newline>" to nonl would let it span the newline to (0 . 5).
(test-equal "nonl stops at the newline inside a line-aware pattern"
  '(0 . 2)
  (span (regexp-search (rx (: bol (+ nonl))) "ab\ncd")))

;; (c) any (".") inside a line-aware pattern keeps spanning the newline: ".+" over
;; "a\nb" is the whole string (0 . 3). Teeth: a fix that sets the newline flag but
;; omits the "." -> ".|<newline>" rewrite would drop this to (0 . 1).
(test-equal "any spans the newline inside a line-aware pattern"
  '(0 . 3)
  (span (regexp-search (rx (: bol (+ any))) "a\nb")))

;; ===== a negated NAMED class whose set already contains the newline =====
;; Guards (a)-(c) all negate a set that EXCLUDES the newline -- the literal "x",
;; or nonl/any -- so they only ever exercise the restoration-added branch. A
;; named class whose set INCLUDES the newline takes the other branch and is the
;; hole those guards leave open: ascii is [\x01-\x7f], which spans 0x0A, so
;; (~ ascii) is [^\x01-\x7f] and already excludes the newline. Its class must NOT
;; have the "|<newline>" restoration appended. Teeth: a compiler that fails to
;; count ascii as a newline-containing class re-adds the newline and wrongly
;; matches a bare "\n" here -- yet the newline IS ascii, so (~ ascii) must reject
;; it.
(test-assert "a negated ascii class does not match a newline inside a line-aware pattern"
  (not (regexp-search? (rx (: bol (~ ascii))) "\n")))

;; The positive companion: the fix must not over-correct into a class that never
;; matches. A high byte is not ascii, so (~ ascii) still matches it -- U+00FF
;; encodes to two bytes, both outside [\x01-\x7f], and bol pins the match to the
;; string start. Green both before and after the fix.
(test-assert "a negated ascii class still matches a non-ascii byte under line-aware anchors"
  (regexp-search? (rx (: bol (~ ascii))) "\xff;"))

(test-end)
