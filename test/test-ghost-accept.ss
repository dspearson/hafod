;;; test-ghost-accept.ss -- accepting the history ghost through auto-inserted closers
;;;
;;; With paredit on, typing "(" leaves the buffer "()" with the cursor parked
;;; before the auto-inserted ")".  When the history ghost is showing, Right / End
;;; / Ctrl-Right must ACCEPT it -- landing exactly on the matched history entry --
;;; rather than merely stepping past the closer or stacking a doubled ")".
;;;
;;; PTY-free: we drive read-expression with string ports, exactly as the other
;;; editor suites do.  editor-history is the editor's module-level history;
;;; submitting a line appends to it, and an in-process append is always newer
;;; than anything loaded from disk, so a seeded entry is the newest match for its
;;; prefix -- deterministic regardless of any real on-disk history.  The
;;; no-ghost regression parks the cursor where the before-cursor text is empty,
;;; which withholds the ghost unconditionally, so that case needs no seeding.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor editor))

;; Control character (C-a = 1, etc.).
(define (ctrl ch)
  (integer->char (- (char->integer ch) 96)))

;; C-j (LF, char 10) submits in insert mode.
(define submit (string (integer->char 10)))

;; xterm arrow sequences (Chez \xHH; hex escapes: \x1b; is ESC).
(define RIGHT      "\x1b;[C")     ; ESC [ C        -- plain Right
(define END        "\x1b;[F")     ; ESC [ F        -- plain End
(define CTRL-RIGHT "\x1b;[1;5C")  ; ESC [ 1 ; 5 C  -- Ctrl-Right
(define TAB        (string #\tab)) ; Tab           -- opens the completion menu

;; Drive one line through the editor and return what it read.
(define (editor-simulate prompt input)
  (let ([in-port (open-input-string input)]
        [out-port (open-output-string)])
    (read-expression prompt in-port out-port)))

;; Count occurrences of a character in a string.
(define (count-char c s)
  (let loop ([i 0] [n 0])
    (if (>= i (string-length s))
        n
        (loop (+ i 1) (if (char=? (string-ref s i) c) (+ n 1) n)))))

;; Balanced round brackets: as many "(" as ")".
(define (balanced-parens? s)
  (= (count-char #\( s) (count-char #\) s)))

(test-begin "ghost-accept")

;; Seed the two entries these tests key off.  Each seeding submit auto-pairs and
;; appends to editor-history; the disjoint prefixes ("hello", "(") each resolve
;; to exactly one seeded entry.
(editor-simulate "> " (string-append "hello-world" submit))
(editor-simulate "> " (string-append "(+ 1 2" submit))  ; auto-pairs to "(+ 1 2)"

;; (a) Right accepts the ghost THROUGH paredit's auto-inserted closer.  Typing
;; "(" yields "()" with the cursor before ")"; the ghost "+ 1 2)" is showing, so
;; Right lands on "(+ 1 2)" -- no doubled ")", no cursor merely stepping past.
(test-equal "Right accepts the ghost through paredit's auto-closer"
  "(+ 1 2)"
  (editor-simulate "> " (string-append "(" RIGHT submit)))

;; (b) The explicit Ctrl-Right chord accepts the same way.
(test-equal "Ctrl-Right accepts the ghost through paredit's auto-closer"
  "(+ 1 2)"
  (editor-simulate "> " (string-append "(" CTRL-RIGHT submit)))

;; End accepts through the auto-closer too (shares the accept path with Right).
(test-equal "End accepts the ghost through paredit's auto-closer"
  "(+ 1 2)"
  (editor-simulate "> " (string-append "(" END submit)))

;; (c) Literal end-of-buffer accept still works: with nothing after the cursor,
;; the ghost suffix is simply appended, with no duplication.
(test-equal "Right accepts a plain end-of-buffer ghost (no closers)"
  "hello-world"
  (editor-simulate "> " (string-append "hello" RIGHT submit)))

;; (d) Regression guard: with NO ghost showing, Right still MOVES the cursor.
;; After C-a the before-cursor text is empty, so no ghost can show (independent
;; of any history); Right steps onto index 1 and the "X" lands there.
(test-equal "Right still moves the cursor when no ghost is showing"
  "aXbc"
  (editor-simulate "> " (string-append "abc" (string (ctrl #\a))
                                        RIGHT "X" submit)))

;; (e) The accepted paren buffer is byte-equal to the history entry and balanced.
(let ([accepted (editor-simulate "> " (string-append "(" RIGHT submit))])
  (test-equal "accepted buffer is byte-equal to the history entry"
    "(+ 1 2)" accepted)
  (test-assert "accepted buffer has balanced parentheses"
    (balanced-parens? accepted)))

;; (f) A completion menu that rewrites the line must NOT leave a stale ghost for
;; Right / End to accept.  While the menu is up the ghost is frozen -- it is not
;; recomputed -- yet Tab can change the buffer under it: fuzzy symbol completion
;; collapses the typed word to the candidates' longest common prefix.  Dismissing
;; the menu (any non-Tab key, e.g. End) must therefore drop the now-stale ghost,
;; not splice its old suffix onto whatever the cursor has come to sit on.  Before
;; the fix, End submitted the mangled concatenation ("'->number", "(->number x)").

;; Seed a longer entry so the short typed prefix has a ghost to show.
(editor-simulate "> " (string-append "'string->number" submit))

;; Positive control: with NO menu, End does accept the ghost -- proving one is
;; genuinely showing, so the guard below cannot pass vacuously.
(test-equal "End accepts the quoted ghost when no menu is up"
  "'string->number"
  (editor-simulate "> " (string-append "'string" END submit)))

;; The guard: type 'string (ghost "->number" showing), Tab collapses the word to
;; the common prefix (here just the quote) and opens the menu, then End dismisses
;; it.  The buffer must be exactly what the completion left -- the lone quote --
;; never the stale ghost spliced back on.
(test-equal "End after a menu-collapsing Tab drops the stale quoted ghost"
  "'"
  (editor-simulate "> " (string-append "'string" TAB END submit)))

;; Before-closer variant: inside parens the same collapse empties the buffered
;; word, and a stale accept would additionally delete paredit's auto-inserted ")"
;; before appending the old suffix.  After the fix the buffer is just the bare
;; auto-pair the completion left.
(editor-simulate "> " (string-append "(string->number x" submit)) ; auto-pairs -> (string->number x)
(test-equal "End accepts the paren ghost through the auto-closer when no menu is up"
  "(string->number x)"
  (editor-simulate "> " (string-append "(string" END submit)))
(test-equal "End after a menu-collapsing Tab keeps the closer and drops the ghost"
  "()"
  (editor-simulate "> " (string-append "(string" TAB END submit)))

(test-end)
