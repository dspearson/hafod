;;; Ported from scsh/test/strings-and-chars-test.scm
;;; Tests the scsh string/char library: the character-classification predicates
;;; (char-letter?, char-digit?, char-ascii?, ...), character<->ASCII conversion,
;;; and ->char-set membership. The file-name/extension family this port earlier
;;; exercised is covered exhaustively by the dedicated file-name suite, so only
;;; a small smoke check of it is retained here.
;;; Original: inline test in strings-and-chars-test.scm. Ported to hafod test runner.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod))

(test-begin "scsh-strings")

;; --- character-classification predicates ---
;;
;; Each predicate is checked against a documented member and a documented
;; non-member, so a predicate that collapsed to a constant would redden.

(test-assert "char-letter? -- alphabetic letters only"
  (and (char-letter? #\a) (not (char-letter? #\5))))

(test-assert "char-digit? -- decimal digits only"
  (and (char-digit? #\5) (not (char-digit? #\a))))

(test-assert "char-hex-digit? -- hexadecimal digits only"
  (and (char-hex-digit? #\f) (not (char-hex-digit? #\g))))

(test-assert "char-alphanumeric? -- letters and digits, nothing else"
  (and (char-alphanumeric? #\a) (char-alphanumeric? #\5)
       (not (char-alphanumeric? #\!))))

(test-assert "char-letter+digit? -- letters and digits, nothing else"
  (and (char-letter+digit? #\a) (char-letter+digit? #\5)
       (not (char-letter+digit? #\!))))

(test-assert "char-punctuation? -- punctuation marks only"
  (and (char-punctuation? #\!) (not (char-punctuation? #\a))))

;; A symbol character is a maths or sign glyph such as +, distinct from a
;; punctuation mark such as !, which is not classed as a symbol.
(test-assert "char-symbol? -- symbol glyphs, not letters or punctuation"
  (and (char-symbol? #\+) (not (char-symbol? #\a)) (not (char-symbol? #\!))))

;; A graphic character is a visible glyph and excludes the space; a printing
;; character additionally includes the space but still excludes a control
;; character. The two classes differ exactly on the space.
(test-assert "char-graphic? -- visible glyphs, excluding the space"
  (and (char-graphic? #\!) (not (char-graphic? #\space))))

(test-assert "char-printing? -- visible glyphs and the space, not controls"
  (and (char-printing? #\a) (char-printing? #\space)
       (not (char-printing? #\nul))))

(test-assert "char-blank? -- space and tab only"
  (and (char-blank? #\space) (char-blank? #\tab) (not (char-blank? #\a))))

(test-assert "char-iso-control? -- control characters only"
  (and (char-iso-control? #\nul) (not (char-iso-control? #\a))))

;; char-ascii? is true only in the 7-bit range: a non-ASCII letter (the Greek
;; lambda) sits outside it and is therefore not an ASCII character.
(test-assert "char-ascii? -- 7-bit range only, a non-ASCII letter excluded"
  (and (char-ascii? #\A) (not (char-ascii? #\x3bb))))

;; --- character <-> ASCII code ---

(test-equal "char->ascii of #\\A is 65" 65 (char->ascii #\A))
(test-equal "ascii->char of 65 is #\\A" #\A (ascii->char 65))
(test-equal "ascii<->char round-trips on #\\Z" #\Z
  (ascii->char (char->ascii #\Z)))

;; --- ->char-set membership ---
;;
;; ->char-set coerces a string to the set of its characters; membership then
;; holds for a character drawn from the string and fails for one outside it.
(test-assert "->char-set of a string: member versus non-member"
  (and (char-set-contains? (->char-set "abc") #\b)
       (not (char-set-contains? (->char-set "abc") #\z))))

;; --- file-name-sans-extension smoke check ---
;;
;; The file-name and extension family is exercised exhaustively by the dedicated
;; file-name suite; only a two-case smoke check is kept here -- a leading-dot
;; name has no extension to strip, and an ordinary extension is removed.
(test-equal "file-name-sans-extension keeps a leading-dot name whole" ".scm"
  (file-name-sans-extension ".scm"))
(test-equal "file-name-sans-extension strips an ordinary extension" "t"
  (file-name-sans-extension "t.scm"))

(test-end)
