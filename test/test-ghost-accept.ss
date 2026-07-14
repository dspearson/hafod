;;; test-ghost-accept.ss -- accepting the history ghost through auto-inserted
;;; closers, and echoing the line it was showing over.
;;;
;;; With paredit on, typing "(" leaves the buffer "()" with the cursor parked
;;; before the auto-inserted ")".  When the history ghost is showing, Right / End
;;; / Ctrl-Right must ACCEPT it -- landing exactly on the matched history entry --
;;; rather than merely stepping past the closer or stacking a doubled ")".
;;;
;;; Submitting is the other half of the same story, and the final section proves
;;; it: a line committed with a ghost up must echo WHOLE.  The ghost is drawn at
;;; the cursor, so erasing it cannot mean clearing from the cursor to the end of
;;; the screen -- that takes the buffer's real closers with it, and the transcript
;;; then disagrees with what was evaluated.  Those cases read the echoed line back
;;; off a virtual-terminal screen, folded from the editor's own escape stream.
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
        (test vterm)
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

;; Drive the same input, but fold the editor's escape stream into the virtual
;; terminal (render->screen forces the capability verdict on, so a plain string
;; port receives the whole stream).  Returns two values: what read-expression
;; read, and the screen the user is left looking at.  editor-simulate above
;; cannot see the echo -- off a terminal the editor emits no escapes at all, and
;; so nothing it draws or erases is observable there.
(define (editor-echo prompt input cols)
  (let ([line #f])
    (let ([scr (render->screen cols
                 (lambda (p)
                   (set! line (read-expression prompt (open-input-string input) p))))])
      (values line scr))))

;; The 256-colour index the renderer paints the ghost with (ESC[38;5;240m).
(define ghost-grey 240)

;; Does any cell on ROW still carry the ghost's grey?  An echoed line must have
;; none: the ghost is gone, not merely painted over.
(define (row-has-ghost-grey? scr row)
  (let loop ([col 0])
    (cond
      [(>= col (vterm-cols scr)) #f]
      [(eqv? (cell-fg (vterm-cell scr row col)) ghost-grey) #t]
      [else (loop (+ col 1))])))

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

;; ======================================================================
;; (g) Submitting with a ghost up echoes the COMMITTED line, whole.  The ghost is
;; drawn at the cursor, and the cursor -- with paredit on -- sits BEFORE the
;; closers the buffer already holds.  So the ghost cannot be erased by clearing
;; from the cursor to the end of the screen: that clear reaches past the ghost and
;; takes those real closers with it, leaving a transcript that disagrees with what
;; was evaluated ("> (+ 1 2" echoed for a submitted "(+ 1 2)").  The only truthful
;; erase is a re-render of the committed line.  Read back off a folded screen,
;; because off a terminal none of this is emitted at all.
;; ======================================================================

;; Re-seed "(+ 1 2)" as the newest entry -- the section above submitted "()" last,
;; and the ghost is always the newest match for the typed prefix.
(editor-simulate "> " (string-append "(+ 1 2" submit))

;; Positive control: a ghost is genuinely showing for the prefix "(" -- End
;; accepts it -- so the echo cases below cannot pass vacuously on a ghostless line.
(test-equal "the re-seeded entry is the newest ghost for the prefix ("
  "(+ 1 2)"
  (editor-simulate "> " (string-append "(" END submit)))

;; Typed on to "(+ 1 2": the buffer auto-paired to "(+ 1 2)" with the cursor
;; before the closer, and the ghost is the entry's remaining suffix -- exactly the
;; ")" already on the screen.  Enter must echo the whole committed line.
(let-values ([(line scr) (editor-echo "> " (string-append "(+ 1 2" submit) 80)])
  (test-equal "submit: the committed line is the balanced expression"
    "(+ 1 2)" line)
  (test-equal "submit: the echo shows the whole line, closer and all"
    "> (+ 1 2)" (vterm-row-text scr 0))
  (test-assert "submit: the echoed line carries no ghost residue"
    (not (row-has-ghost-grey? scr 0))))

;; The bare auto-pair: buffer "()" with the ghost "+ 1 2)" expanding inside it.
;; Enter commits the "()" the user actually typed -- and echoes that, not the
;; truncated "(" the ghost-clear used to leave behind.
(let-values ([(line scr) (editor-echo "> " (string-append "(" submit) 80)])
  (test-equal "submit: the committed line is the bare auto-pair"
    "()" line)
  (test-equal "submit: the echo keeps the auto-inserted closer"
    "> ()" (vterm-row-text scr 0))
  (test-assert "submit: the ghost is gone from the echoed line, not painted over"
    (not (row-has-ghost-grey? scr 0))))

;; Regression, no ghost: C-a empties the before-cursor text, which withholds the
;; ghost unconditionally.  The line still echoes plainly -- the no-ghost submit
;; path emits no redraw of its own and needs none.
(let-values ([(line scr) (editor-echo "> " (string-append "abc" (string (ctrl #\a))
                                                          submit)
                                      80)])
  (test-equal "submit without a ghost: the committed line is what was typed"
    "abc" line)
  (test-equal "submit without a ghost: the line echoes plainly"
    "> abc" (vterm-row-text scr 0)))

(test-end)
