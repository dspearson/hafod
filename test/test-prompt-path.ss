(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod interactive) sanitize-control prompt-path-segment)
        (only (hafod environment) getenv setenv))

(test-begin "prompt-path")

;; Pin CLICOLOR_FORCE unset, mirroring test-interactive.ss: an ambient
;; CLICOLOR_FORCE=1 could force colour on a non-tty sink and perturb any
;; capability-gated behaviour reached from here.  The pure helpers under test
;; are colour-free, but the pin keeps the suite honest against future additions.
(setenv "CLICOLOR_FORCE" #f)

;; A byte is terminal-hostile when it is a C0 control (below the space code) or
;; DEL -- exactly what sanitize-control must strip.  The suite reuses this to
;; assert the invariant directly on any sanitised string.
(define (has-terminal-control? str)
  (let ([len (string-length str)])
    (let loop ([i 0])
      (cond
        [(>= i len) #f]
        [(let ([c (char->integer (string-ref str i))])
           (or (< c #x20) (= c #x7f)))
         #t]
        [else (loop (+ i 1))]))))

;; === sanitize-control -- terminal-control byte stripper ===

;; A plain, control-free string passes through unchanged.
(test-equal "sanitize: plain string unchanged"
  "hello world" (sanitize-control "hello world"))

;; An embedded ESC (#x1b) is removed, the surrounding text kept verbatim.
(test-equal "sanitize: ESC removed"
  "abcdef"
  (sanitize-control (string-append "abc" (string (integer->char #x1b)) "def")))

;; Both carriage return and line feed are removed (they are C0 controls).
(test-equal "sanitize: CR and LF removed"
  "ab"
  (sanitize-control (string #\a #\return #\newline #\b)))

;; DEL (#x7f), though above the space code, is removed too.
(test-equal "sanitize: DEL removed"
  "xy"
  (sanitize-control (string #\x (integer->char #x7f) #\y)))

;; The C1 control range (#x80-#x9f) is stripped as well: on a UTF-8 terminal an
;; eight-bit CSI (#x9b) or OSC (#x9d) is an escape introducer, so a hostile name
;; must not smuggle one past the sanitiser.
(test-equal "sanitize: C1 CSI (#x9b) removed"
  "ab" (sanitize-control (string #\a (integer->char #x9b) #\b)))
(test-equal "sanitize: C1 range collapses to empty"
  "" (sanitize-control (string (integer->char #x80) (integer->char #x9d)
                               (integer->char #x9f))))

;; A string of only control bytes collapses to the empty string.
(test-equal "sanitize: all-control becomes empty"
  ""
  (sanitize-control (string (integer->char #x1b) #\newline #\tab
                            (integer->char #x7f) (integer->char #x01))))

;; Wide and accented characters are ordinary text and survive untouched.
(test-equal "sanitize: CJK preserved" "日本語" (sanitize-control "日本語"))
(test-equal "sanitize: accented letter preserved" "café" (sanitize-control "café"))

;; The output of a mixed string carries no ESC and no sub-space / DEL byte.
(test-assert "sanitize: no control byte survives"
  (not (has-terminal-control?
         (sanitize-control
           (string #\a (integer->char #x1b) #\b #\return #\newline #\c
                   (integer->char #x07) (integer->char #x7f) #\d)))))

;; === prompt-path-segment -- home-relative + fish-truncated path ===
;;
;; Every case is a pure call: home is passed explicitly, so nothing reads $PWD,
;; the filesystem or git.  Home is "/home/u" unless a case says otherwise.

;; $HOME itself renders exactly as "~".
(test-equal "path: home is ~"
  "~" (prompt-path-segment "/home/u" "/home/u" 40))

;; A path under home, within budget, is home-relative and untruncated.
(test-equal "path: under home within budget"
  "~/proj/src" (prompt-path-segment "/home/u/proj/src" "/home/u" 40))

;; The same path over budget is fish-truncated: each leading segment collapses
;; to its first character, the last segment is kept full, and "~" passes through.
(test-equal "path: under home over budget"
  "~/p/src" (prompt-path-segment "/home/u/proj/src" "/home/u" 6))

;; An absolute path outside home truncates with the leading slash preserved.
(test-equal "path: absolute over budget"
  "/u/l/bin" (prompt-path-segment "/usr/local/bin" "/home/u" 6))

;; Root renders as itself.
(test-equal "path: root is /"
  "/" (prompt-path-segment "/" "/home/u" 40))

;; Boundary: /home/u2 must NOT collapse to ~2 -- the ~ prefix matches only on a
;; "/" boundary or the exact home string.
(test-equal "path: home-prefix boundary is not ~2"
  "/home/u2" (prompt-path-segment "/home/u2" "/home/u" 40))

;; An empty home string (HOME unset or "") performs no substitution: an absolute
;; path renders as itself, so /etc never becomes ~/etc.
(test-equal "path: empty home does not prefix ~"
  "/etc" (prompt-path-segment "/etc" "" 40))

;; A hidden directory keeps its leading dot plus one character when abbreviated.
(test-equal "path: hidden dir keeps its dot"
  "/s/.c/app" (prompt-path-segment "/srv/.config/app" "/home/u" 8))

;; The helper never resolves symlinks: a symlinked component is returned exactly
;; as given (no readlink / canonicalisation) -- proving the logical-path rule.
(test-equal "path: symlinked input preserved"
  "~/link/deep" (prompt-path-segment "/home/u/link/deep" "/home/u" 40))

;; Wide CJK directory: the budget is measured by DISPLAY width (12 columns), not
;; character count (9), so it forces truncation and the wide segment collapses to
;; its first character.  A string-length implementation would count 9, skip
;; truncation, and wrongly leave 日本語 unabbreviated -- failing this row.
(test-equal "path: wide CJK dir truncates by display width"
  "~/日/src" (prompt-path-segment "/home/u/日本語/src" "/home/u" 10))

;; Security: an ESC byte embedded in the final directory component is stripped
;; inside the pure helper -- the rendered string carries no #x1b.
(test-assert "path: ESC in dir name is sanitised"
  (let ([rendered (prompt-path-segment
                    (string-append "/home/u/wo" (string (integer->char #x1b)) "rk")
                    "/home/u" 40)])
    (let loop ([i 0])
      (cond
        [(>= i (string-length rendered)) #t]
        [(= (char->integer (string-ref rendered i)) #x1b) #f]
        [else (loop (+ i 1))]))))

(test-end)
