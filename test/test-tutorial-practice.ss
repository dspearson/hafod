;;; test/test-tutorial-practice.ss -- The tutorial's live practice buffer,
;;; pinned without a PTY.
;;;
;;; The tutorial seeds each practisable lesson with a REAL editor buffer and
;;; reports whether the reader's keys took it to that lesson's target.  Three
;;; separate things have to hold for that to be worth anything, and this suite
;;; asserts each at the smallest level it can:
;;;
;;;   (1) the SEAM is live.  (hafod editor editor) imports (hafod editor help),
;;;       so the tutorial cannot import the editor back; it declares three
;;;       #f-defaulted hooks and the editor fills them at load.  A test may
;;;       import BOTH sides, because a test is a script and not a library on the
;;;       cycle -- and importing the editor is what fills the seam, so every
;;;       assertion below runs against the editor's own dispatch.
;;;
;;;   (2) every checked lesson is SOLVABLE.  Each lesson's taught keystrokes are
;;;       driven through that real dispatch and must land on that lesson's
;;;       target, and no target may equal its own seed.  A target invented rather
;;;       than derived fails here rather than reaching a reader.
;;;
;;;   (3) the GUARANTEES the tutorial makes.  Advancing is available from every
;;;       lesson at every moment, whatever the buffer is doing; success is
;;;       decided by the final text alone, by any route; a re-seed restores the
;;;       start; and the handful of keys that would open the reader's history or
;;;       a full-screen finder never reach the dispatch at all.
;;;
;;; Nothing here needs a terminal.  Keystrokes are decoded from bounded string
;;; ports, so no read can block and the suite cannot hang, and the rendered pane
;;; is read back through the vterm grid rather than as raw bytes.
;;;
;;; run-tutorial itself is deliberately never called, for the same reason the
;;; navigation suite never calls it: it gates on whether descriptor 0 is a
;;; terminal, and the suites run with stdin from /dev/null, which would make the
;;; result a property of the harness rather than of the code.  Every assertion
;;; goes through the injected entry points instead.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (chezscheme)
        (only (hafod editor help)
              tutorial-lesson-count tutorial-next-index
              tutorial-lesson-seed tutorial-lesson-cursor
              tutorial-lesson-target tutorial-lesson-solution
              tutorial-practice-open tutorial-practice-feed! tutorial-practice-view
              key->practice-command tutorial-handle-key
              practice-verdict render-practice-pane
              run-tutorial/reader)
        (only (hafod editor input-decode)
              make-key-event read-key-event MOD_ALT)
        (only (hafod terminal-caps) assume-terminal-caps)
        (hafod editor editor))

;; ======================================================================
;; Helpers
;; ======================================================================

;; The one editor binding this suite names before the seam assertions below, and
;; it is named deliberately.  Chez instantiates a library the first time a
;; variable whose HOME it is gets referenced, so touching an editor binding is
;; what runs the wiring block at the foot of editor.ss.  It is also why the
;; tutorial's entry point is the editor's own procedure rather than a name
;; re-exported from (hafod editor help): a caller who reached only for the
;; re-export never instantiated the editor, and every lesson quietly fell back to
;; prose.  Anything added ABOVE this line that touches the editor would make the
;; assertions in group 1 vacuous.
(define editor-instantiated? (and editor-normal-keymap #t))

;; Terminal capabilities are forced off around everything that drives a buffer.
;; A command that changes mode writes a cursor-shape escape to the CONSOLE -- not
;; to the pane this suite reads back -- and with capabilities off nothing escapes
;; into the test log.  Nothing else in the dispatch consults the setting.
(define-syntax with-caps-off
  (syntax-rules ()
    [(_ body ...) (parameterize ([assume-terminal-caps 'off]) body ...)]))

;; Open a practice session through the LIVE seam, so what these assertions drive
;; is the editor's own state record and the editor's own dispatch.
(define (open-session seed cursor)
  (with-caps-off ((tutorial-practice-open) seed cursor)))

;; Feed KEYS to SESSION, one decoded event at a time.  Each event is handed the
;; same bounded port it was decoded from, so a command with an inline read (a
;; text object's follow-up character, a surround, a search pattern) satisfies it
;; from the rest of the string -- mirroring editor-drive-keys!'s
;; one-bounded-port-per-group discipline.
;;
;; An isolated Escape must therefore be fed as its own chunk: the decoder tells
;; Escape from Alt with char-ready?, which on a bounded string port is always
;; true mid-stream, so an Escape with anything after it in the same chunk decodes
;; as Meta.  At the end of a chunk it decodes to 'escape, exactly as a terminal
;; delivers it.
(define (feed-keys! session keys)
  (with-caps-off
    (let ([in (open-input-string keys)])
      (let lp ()
        (let ([ev (read-key-event in)])
          (unless (eof-object? ev)
            ((tutorial-practice-feed!) session ev in)
            (lp)))))))

(define (session-text session)
  (let-values ([(text cursor mode) ((tutorial-practice-view) session)]) text))

(define (session-cursor session)
  (let-values ([(text cursor mode) ((tutorial-practice-view) session)]) cursor))

(define (session-mode session)
  (let-values ([(text cursor mode) ((tutorial-practice-view) session)]) mode))

;; Seed a buffer, run KEYS at it, and answer the text it ends up holding.
(define (practise seed cursor keys)
  (let ([session (open-session seed cursor)])
    (feed-keys! session keys)
    (session-text session)))

;; A bounded, already-empty port, for handing a key that needs no follow-up read.
(define (no-more) (open-input-string ""))

(define (K type value mods) (make-key-event type value mods))

;; Does S hold NEEDLE anywhere?
(define (contains? s needle)
  (let ([sn (string-length s)] [nn (string-length needle)])
    (let lp ([i 0])
      (cond
        [(> (+ i nn) sn) #f]
        [(string=? needle (substring s i (+ i nn))) #t]
        [else (lp (+ i 1))]))))

;; Does S hold CH anywhere?
(define (holds-char? s ch)
  (let lp ([i 0])
    (cond
      [(>= i (string-length s)) #f]
      [(char=? (string-ref s i) ch) #t]
      [else (lp (+ i 1))])))

;; The pane's frame glyphs, by code point (as help.ss writes them).
(define box-vertical   #\x2502)
(define box-horizontal #\x2500)
(define verdict-tick   #\x2713)

(test-begin "tutorial-practice")

;; ======================================================================
;; 1. The seam is live
;;
;; The injection proof: with the editor instantiated, all three hooks hold
;; procedures.  This group is RED if the wiring block at the foot of editor.ss is
;; ever dropped, and RED again if the tutorial is moved somewhere the editor
;; cannot fill it from.
;; ======================================================================

(test-assert "the editor library is instantiated" editor-instantiated?)

(test-assert "the editor fills the open hook"
  (procedure? (tutorial-practice-open)))

(test-assert "the editor fills the feed hook"
  (procedure? (tutorial-practice-feed!)))

(test-assert "the editor fills the view hook"
  (procedure? (tutorial-practice-view)))

;; And what comes back through it is the editor's own state record, not a
;; tutorial-local stand-in wearing the same shape -- which is the whole point of
;; going through a seam rather than re-implementing a buffer.
(test-assert "a session is the editor's own state record"
  (let ([session (open-session "(a b)" 1)])
    (and (editor-state-gb session)
         (eq? (editor-state-mode session) 'normal))))

;; ======================================================================
;; 2. Every checked lesson is solvable
;;
;; Driven off tutorial-lesson-count rather than a hand-written list, so a lesson
;; added later is covered rather than silently skipped.  Each lesson is held to
;; the rules of the kind it belongs to.
;; ======================================================================

(let lp ([i 0] [checked 0] [open 0] [prose 0])
  (if (>= i tutorial-lesson-count)
      (begin
        ;; None of the three kinds may be empty, or the walk above would be
        ;; asserting nothing while still passing.
        (test-assert "the roster holds checked lessons" (> checked 0))
        (test-assert "the roster holds open-practice lessons" (> open 0))
        (test-assert "the roster holds prose lessons" (> prose 0)))
      (let* ([n (number->string (+ i 1))]
             [seed (tutorial-lesson-seed i)]
             [cursor (tutorial-lesson-cursor i)]
             [target (tutorial-lesson-target i)]
             [solution (tutorial-lesson-solution i)])
        (cond
          ;; Checked: its own keystrokes must reach its own target, through the
          ;; real dispatch.  If this goes red the defect is the target, and the
          ;; fix is to correct it -- never to soften what is asserted here.
          [target
           (test-equal (string-append "lesson " n ": its taught keystrokes reach its target")
             target (practise seed cursor solution))
           (test-assert (string-append "lesson " n ": its target is not already met on arrival")
             (not (string=? target seed)))
           (test-assert (string-append "lesson " n ": it names the keystrokes that reach it")
             (> (string-length solution) 0))
           (test-assert (string-append "lesson " n ": its target is not empty")
             (> (string-length target) 0))
           (lp (+ i 1) (+ checked 1) open prose)]
          ;; Open practice: a buffer to move in, and nothing to be wrong about.
          [seed
           (test-assert (string-append "lesson " n ": a buffer, no target, no solution")
             (and (> (string-length seed) 0)
                  (integer? cursor)
                  (<= 0 cursor (string-length seed))
                  (= 0 (string-length solution))))
           (lp (+ i 1) checked (+ open 1) prose)]
          ;; Prose: no buffer at all.
          [else
           (test-assert (string-append "lesson " n ": prose, so nothing to seed or check")
             (and (not seed) (not cursor) (not target) (= 0 (string-length solution))))
           (lp (+ i 1) checked open (+ prose 1))]))))

;; ======================================================================
;; 3. Result-only checking
;;
;; The locked rule made testable: the verdict reads the final text and nothing
;; else, so the route taken is never inspected.  The deleting lesson teaches dw;
;; ten x presses take the same text to the same place and count exactly as much.
;; ======================================================================

(let ([by-the-long-way (practise "(remove-me keep-this)" 1 "xxxxxxxxxx")])
  (test-equal "ten x presses reach what dw reaches"
    "(keep-this)" by-the-long-way)
  (test-equal "and the verdict is met, because the route is never looked at"
    'met (practice-verdict by-the-long-way "(keep-this)")))

(test-equal "matching text is met"
  'met (practice-verdict "(keep-this)" "(keep-this)"))

(test-equal "a half-finished edit is unmet, not a failure"
  'unmet (practice-verdict "(emove-me keep-this)" "(keep-this)"))

(test-equal "an untouched buffer is unmet"
  'unmet (practice-verdict "(remove-me keep-this)" "(keep-this)"))

(test-equal "no target means there is nothing to be right about"
  'open (practice-verdict "(anything at all)" #f))

;; ======================================================================
;; 4. The practice key table
;;
;; A separate table from the prose tutorial's: here only Enter and a named set of
;; control keys are reserved, and every printable key belongs to the buffer.
;; ======================================================================

(test-equal "Enter advances" 'next (key->practice-command (K 'special 'return 0)))
(test-equal "Ctrl-P goes back a lesson" 'prev (key->practice-command (K 'ctrl #\p 0)))
(test-equal "Ctrl-R re-seeds the lesson" 'reset (key->practice-command (K 'ctrl #\r 0)))

;; Raw mode has cleared ICANON, so Ctrl-D reaches the decoder as an ordinary
;; control byte and never as an end-of-file object.  Genuine end of input means
;; the same thing.
(test-equal "Ctrl-D leaves" 'quit (key->practice-command (K 'ctrl #\d 0)))
(test-equal "end of input leaves" 'quit (key->practice-command (eof-object)))

;; Absorbed: Up, Down and Ctrl-N reach the editor's history commands, which on a
;; one-line buffer would open the reader's real history and swap out the seeded
;; text; Tab opens a completion menu sized to a terminal the pane does not own;
;; Ctrl-T and Alt-C open a full-screen finder over the tutorial's own screen.
(test-equal "Up is absorbed" 'ignore (key->practice-command (K 'special 'up 0)))
(test-equal "Down is absorbed" 'ignore (key->practice-command (K 'special 'down 0)))
(test-equal "Ctrl-N is absorbed" 'ignore (key->practice-command (K 'ctrl #\n 0)))
(test-equal "Tab is absorbed" 'ignore (key->practice-command (K 'special 'tab 0)))
(test-equal "Ctrl-T is absorbed" 'ignore (key->practice-command (K 'ctrl #\t 0)))
(test-equal "Alt-C is absorbed" 'ignore (key->practice-command (K 'meta #\c 0)))

;; The printable four are the point of the rule.  In a practice lesson r is vi's
;; replace, not a tutorial command; q, p and d likewise belong to the buffer.
(test-equal "r reaches the buffer, where vi reads it as replace"
  'edit (key->practice-command (K 'char #\r 0)))
(test-equal "q reaches the buffer"
  'edit (key->practice-command (K 'char #\q 0)))
(test-equal "p reaches the buffer, where vi reads it as paste"
  'edit (key->practice-command (K 'char #\p 0)))
(test-equal "d reaches the buffer, where vi reads it as an operator"
  'edit (key->practice-command (K 'char #\d 0)))

;; The modifier case, and the reason every arm matches the whole event: C-t with
;; no modifier is the file picker, absorbed above; C-M-t is the s-expression
;; transpose the paredit lesson teaches.  A table testing only the type and the
;; character would swallow both and disable the lesson without failing anything.
(test-equal "C-M-t is the s-expression transpose, and still reaches the buffer"
  'edit (key->practice-command (K 'ctrl #\t MOD_ALT)))

;; ======================================================================
;; 5. Advancing is always available
;;
;; The guarantee is an ORDERING, not a table entry: the reader classifies before
;; it feeds, so a live buffer never gets the chance to swallow Enter.
;; key->practice-command is pure over the event, so asserting it three times from
;; three buffer states would be one assertion written three times and would prove
;; nothing.  What is asserted instead, through tutorial-handle-key, is that Enter
;; answers 'next AND leaves the buffer byte-identical -- the second half being
;; the real assertion, red the moment a feed precedes a classification.
;;
;; The three states are the ones a naive implementation would most plausibly get
;; wrong: insert mode with text pending, an operator waiting for its motion, and
;; an open visual selection.  Each is checked twice over, because Enter reaching
;; the dispatch does different damage in each: in insert mode it inserts a
;; newline, which the text shows; in the other two it SUBMITS the buffer, which
;; the text does not show but the done flag does.
;; ======================================================================

(let ([session (open-session "(remove-me keep-this)" 1)])
  (feed-keys! session "iZZ")
  (let ([before (session-text session)])
    (test-equal "the buffer really is mid-insert" "INSERT" (session-mode session))
    (test-equal "Enter still advances from insert mode"
      'next (tutorial-handle-key (K 'special 'return 0) (no-more) session))
    (test-equal "and the buffer did not take the newline"
      before (session-text session))
    (test-assert "nor was it submitted out from under the reader"
      (not (editor-state-done? session)))))

(let ([session (open-session "(remove-me keep-this)" 1)])
  (feed-keys! session "d")
  (let ([before (session-text session)])
    (test-equal "the buffer really has an operator pending"
      "-- DELETE --" (session-mode session))
    (test-equal "Enter still advances with an operator pending"
      'next (tutorial-handle-key (K 'special 'return 0) (no-more) session))
    (test-equal "and the operator did not consume it"
      before (session-text session))
    (test-assert "nor was the lesson's buffer submitted"
      (not (editor-state-done? session)))))

(let ([session (open-session "(select some of this text)" 7)])
  (feed-keys! session "ve")
  (let ([before (session-text session)])
    (test-equal "the buffer really has a visual selection open"
      "-- VISUAL --" (session-mode session))
    (test-equal "Enter still advances from an open visual"
      'next (tutorial-handle-key (K 'special 'return 0) (no-more) session))
    (test-equal "and the selection did not swallow it"
      before (session-text session))
    (test-assert "nor was the lesson's buffer submitted"
      (not (editor-state-done? session)))))

;; The other half of the same rule: an absorbed key is absorbed BEFORE the
;; dispatch, not merely classified.  Wiring a finder that records being called
;; makes "no picker opened over the tutorial's screen" assertable rather than
;; inferred from the buffer sitting still -- and the probe below proves the
;; recording finder would have fired had the key reached the dispatch, so the
;; assertion is not passing for want of anything to catch.
(let ([opened 0])
  (parameterize ([editor-finder-proc
                  (lambda args (set! opened (+ opened 1)) #f)])
    (let ([session (open-session "(remove-me keep-this)" 1)])
      (let ([before (session-text session)])
        (test-equal "Ctrl-T stops at the classifier"
          'stay (tutorial-handle-key (K 'ctrl #\t 0) (no-more) session))
        (test-equal "Alt-C stops at the classifier"
          'stay (tutorial-handle-key (K 'meta #\c 0) (no-more) session))
        (test-equal "Tab stops at the classifier"
          'stay (tutorial-handle-key (K 'special 'tab 0) (no-more) session))
        (test-equal "and none of them moved the buffer"
          before (session-text session))
        (test-equal "and no picker was opened over the tutorial's screen"
          0 opened)
        ;; Non-vacuity: the same key handed straight to the dispatch, bypassing
        ;; the classifier, does open one.
        (feed-keys! session (string (integer->char 20)))  ;; Ctrl-T
        (test-equal "though the very same key would have, had it got through"
          1 opened)))))

;; ======================================================================
;; 6. Re-seeding, and where a command leaves the reader
;; ======================================================================

(test-equal "a keystroke spent on the buffer keeps the reader on the lesson"
  3 (tutorial-next-index 3 tutorial-lesson-count 'stay))
(test-equal "so does a re-seed"
  3 (tutorial-next-index 3 tutorial-lesson-count 'reset))
(test-equal "next still advances"
  4 (tutorial-next-index 3 tutorial-lesson-count 'next))
(test-equal "prev still goes back"
  2 (tutorial-next-index 3 tutorial-lesson-count 'prev))
(test-equal "quit still answers the stop signal"
  #f (tutorial-next-index 3 tutorial-lesson-count 'quit))

;; What Ctrl-R promises: re-opening from the same seed and cursor reproduces the
;; start exactly, and does so after the buffer has been edited well away from it.
(let ([session (open-session "(remove-me keep-this)" 1)])
  (feed-keys! session "dw")
  (test-equal "the edit landed" "(keep-this)" (session-text session))
  (let ([re-seeded (open-session "(remove-me keep-this)" 1)])
    (test-equal "re-seeding restores the starting text"
      "(remove-me keep-this)" (session-text re-seeded))
    (test-equal "and the starting caret"
      1 (session-cursor re-seeded))
    (test-equal "and the starting mode"
      "NORMAL" (session-mode re-seeded))))

;; A re-seeded buffer inherits nothing from the one before it -- no pending
;; operator, no open visual.
(let ([abandoned (open-session "(remove-me keep-this)" 1)])
  (feed-keys! abandoned "d")
  (test-equal "the abandoned buffer had an operator pending"
    "-- DELETE --" (session-mode abandoned))
  (let ([re-seeded (open-session "(remove-me keep-this)" 1)])
    (test-equal "the re-seeded one does not inherit it"
      "NORMAL" (session-mode re-seeded))))

;; ======================================================================
;; 7. History cannot eat the buffer
;;
;; j and k on the first line of a buffer reach the editor's history commands, and
;; the history handle is opened lazily on first touch -- so without the hooks
;; held at no-ops in the feed, these two keys would open the reader's real
;; history database and replace the seeded lesson text with their last REPL line.
;; ======================================================================

(let ([session (open-session "(define hello 42)" 8)])
  (feed-keys! session "kj")
  (test-equal "k and j leave a one-line practice buffer exactly as they found it"
    "(define hello 42)" (session-text session)))

;; ======================================================================
;; 8. The piped path stays a slideshow, with the seam filled
;;
;; This suite imports the editor, so the seam is live here exactly as it is in a
;; real run -- which is the combination the navigation suite cannot reach, since
;; it runs with the seam unset and so cannot tell "no pane" from "no editor".
;;
;; Off a terminal the whole-line classifier has no edit arm, so it could never
;; drive a buffer: a pane opened on that path could only ever show a nudge no
;; input would clear.  The one-argument form must therefore stay the slideshow it
;; has always been, and the two-argument form must differ -- otherwise this group
;; would pass merely because panes never render anywhere.
;; ======================================================================

;; Walk the whole tutorial with a reader that always advances, and answer the
;; transcript.  Capabilities are forced off, so the text is plain with not one
;; escape byte in it.
(define (transcript practice?)
  (let ([sp (open-output-string)])
    (parameterize ([assume-terminal-caps 'off]
                   [current-output-port sp])
      (if practice?
          (run-tutorial/reader (lambda () 'next) #t)
          (run-tutorial/reader (lambda () 'next))))
    (get-output-string sp)))

(let ([piped (transcript #f)]
      [live (transcript #t)])
  (test-assert "the piped path frames nothing"
    (not (holds-char? piped box-vertical)))
  (test-assert "and rules nothing"
    (not (holds-char? piped box-horizontal)))
  (test-assert "and reaches no verdict"
    (not (contains? piped "Aiming for:")))
  (test-assert "it prints the example as a line instead, as it always has"
    (contains? piped "Example: (remove-me keep-this)"))
  (test-assert "and keeps its own legend"
    (contains? piped "[Enter] next   [q] quit   [p] previous"))
  ;; The same lessons through the two-argument form DO carry a pane.
  (test-assert "the practice path frames the buffer"
    (holds-char? live box-vertical))
  (test-assert "and reaches a verdict"
    (contains? live "Aiming for: (keep-this)"))
  (test-assert "and drops the printed example, the frame having replaced it"
    (not (contains? live "Example: (remove-me keep-this)")))
  (test-assert "and advertises the keys its own table reserves"
    (contains? live "[Ctrl-R] reset")))

;; ======================================================================
;; 9. The rendered pane, read through the grid
;;
;; Everything above reads the pane as text; here it is read back as a reader
;; would SEE it, folded onto an 80-column terminal grid by the vterm harness --
;; the same way the navigation suite's group 6 reads its screens.  render->screen
;; forces capabilities ON, so the colour escapes the pane emits are interpreted
;; by the grid rather than landing in the glyph rows.
;; ======================================================================

;; A pane on its own grid.  render-practice-pane writes seven rows: the top rule,
;; the framed text, the caret, the bottom rule, the mode-and-hint line, a blank,
;; and the verdict.
(define (pane-screen text cursor mode target solution)
  (render->screen 80
    (lambda (p)
      (parameterize ([current-output-port p])
        (render-practice-pane text cursor mode target solution)))))

;; The frame puts its left rule at grid column 2 and its right rule at column 55
;; -- two spaces of indent, then 1 + 52 + 1 columns of box.  The text begins one
;; column inside the left rule, at column 4.
(define frame-left-col 2)
(define frame-right-col 55)
(define text-start-col 4)

(let ([scr (pane-screen "(remove-me keep-this)" 0 "NORMAL" "(keep-this)" "dw")])
  (test-assert "the seed lands on its own row inside the box"
    (contains? (vterm-row-text scr 1) "(remove-me keep-this)"))
  (test-equal "with the left rule beside it"
    box-vertical (cell-glyph (vterm-cell scr 1 frame-left-col)))
  (test-equal "and the right rule beside it"
    box-vertical (cell-glyph (vterm-cell scr 1 frame-right-col)))
  (test-assert "the top rule is drawn"
    (holds-char? (vterm-row-text scr 0) box-horizontal))
  (test-assert "the bottom rule is drawn"
    (holds-char? (vterm-row-text scr 3) box-horizontal)))

;; The caret row marks the cursor and nothing else.  A cursor at index k over
;; ASCII text lands the caret at grid column text-start-col + k, the very column
;; the character under the cursor occupies.  Checked at the two ends and the
;; middle.
(define (caret-col scr)
  (let ([row (vterm-row-text scr 2)])
    (let lp ([i 0])
      (cond
        [(>= i (string-length row)) #f]
        [(char=? (string-ref row i) #\^) i]
        [else (lp (+ i 1))]))))

;; The caret row holds one ^ and the two frame rules, and no other glyph.
(define (caret-row-clean? scr)
  (let ([row (vterm-row-text scr 2)])
    (let lp ([i 0] [carets 0] [others 0])
      (if (>= i (string-length row))
          (and (= carets 1) (= others 0))
          (let ([ch (string-ref row i)])
            (lp (+ i 1)
                (if (char=? ch #\^) (+ carets 1) carets)
                (if (or (char=? ch #\^) (char=? ch #\space) (char=? ch box-vertical))
                    others (+ others 1))))))))

(let ([at-0 (pane-screen "(remove-me keep-this)" 0 "NORMAL" "(keep-this)" "dw")]
      [at-mid (pane-screen "(remove-me keep-this)" 10 "NORMAL" "(keep-this)" "dw")]
      [at-end (pane-screen "(remove-me keep-this)" 21 "NORMAL" "(keep-this)" "dw")])
  (test-equal "the caret sits under the first character for a cursor at 0"
    text-start-col (caret-col at-0))
  (test-equal "and moves with the cursor into the middle of the text"
    (+ text-start-col 10) (caret-col at-mid))
  (test-equal "and sits just past the last character at end of text"
    (+ text-start-col 21) (caret-col at-end))
  (test-assert "the caret row carries the caret and the frame, nothing else"
    (and (caret-row-clean? at-0) (caret-row-clean? at-mid) (caret-row-clean? at-end))))

;; The mode row shows the label it was handed, the editor's own indicator string
;; and all -- including a visual-mode indicator with its dashes.
(let ([normal (pane-screen "(a b)" 1 "NORMAL" #f "")]
      [visual (pane-screen "(a b)" 1 "-- VISUAL --" #f "")]
      [insert (pane-screen "(a b)" 1 "INSERT" #f "")])
  (test-assert "the mode row shows NORMAL"
    (contains? (vterm-row-text normal 4) "NORMAL"))
  (test-assert "the mode row shows the visual indicator verbatim"
    (contains? (vterm-row-text visual 4) "-- VISUAL --"))
  (test-assert "the mode row shows INSERT"
    (contains? (vterm-row-text insert 4) "INSERT")))

;; The verdict row says what the buffer amounts to, one line, for each of the
;; three verdicts -- and a nudge withholds nothing: it names the target in full.
(let ([met (pane-screen "(keep-this)" 0 "NORMAL" "(keep-this)" "dw")]
      [unmet (pane-screen "(emove-me keep-this)" 0 "NORMAL" "(keep-this)" "dw")]
      [open (pane-screen "(anything)" 0 "NORMAL" #f "")])
  (let ([met-row (vterm-row-text met 6)])
    (test-assert "the met row carries the tick"
      (holds-char? met-row verdict-tick))
    (test-assert "and the text the buffer now reads"
      (contains? met-row "(keep-this)")))
  (test-assert "the unmet row names the target, withholding nothing"
    (contains? (vterm-row-text unmet 6) "(keep-this)"))
  (test-assert "the open row invites free practice"
    (contains? (vterm-row-text open 6) "Practise freely")))

;; The legend a practice lesson shows, read from the grid.  The loop prints it,
;; not render-practice-pane, so it is read from a real lesson screen: the whole
;; tutorial is driven with the seam filled, and the checked "Deleting" screen is
;; folded back through a fresh grid.  Each lesson clears and homes, so its screen
;; is the bytes between one clear-home and the next.
(define clear-home (string #\x1b #\[ #\2 #\J #\x1b #\[ #\H))

(define (index-of s sub from)
  (let ([sn (string-length s)] [bn (string-length sub)])
    (let lp ([i from])
      (cond
        [(> (+ i bn) sn) #f]
        [(string=? sub (substring s i (+ i bn))) i]
        [else (lp (+ i 1))]))))

;; The screens the tutorial painted, in order: the text between each clear-home
;; and the next.
(define (split-screens s)
  (let lp ([from 0] [acc '()])
    (let ([hit (index-of s clear-home from)])
      (if hit
          (let ([next (index-of s clear-home (+ hit (string-length clear-home)))])
            (if next
                (lp next (cons (substring s (+ hit (string-length clear-home)) next) acc))
                (reverse (cons (substring s (+ hit (string-length clear-home))
                                          (string-length s)) acc))))
          (reverse acc)))))

;; The first screen whose text holds NEEDLE, folded onto a fresh 80-column grid.
(define (screen-with needle)
  (let ([sp (open-output-string)])
    (parameterize ([assume-terminal-caps 'on] [current-output-port sp])
      (run-tutorial/reader (lambda () 'next) #t))
    (let lp ([screens (split-screens (get-output-string sp))])
      (cond
        [(null? screens) #f]
        [(index-of (car screens) needle 0)
         (let ([vt (make-vterm 80)])
           (vterm-feed! vt (car screens))
           vt)]
        [else (lp (cdr screens))]))))

;; A row of GRID that holds every one of NEEDLES, or "" if none does.
(define (row-holding grid . needles)
  (let lp ([r 0])
    (cond
      [(>= r (vterm-rows grid)) ""]
      [(let ([t (vterm-row-text grid r)])
         (and (for-all (lambda (nd) (index-of t nd 0)) needles) t))]
      [else (lp (+ r 1))])))

(let ([deleting (screen-with "Deleting")])
  (test-assert "the checked lesson's screen was found" (and deleting #t))
  (let ([legend (row-holding deleting "[Ctrl-R] reset" "[Ctrl-D] quit")])
    (test-assert "its legend advertises the reset and quit its key table honours"
      (> (string-length legend) 0))))

;; The pane fits an 80-column terminal.  Driven off the LONGEST seed in the
;; roster rather than a hand-picked string, so a future example too wide for the
;; frame fails here rather than wrapping on a reader's terminal.  Rendered onto a
;; grid wider than 80 so an overrun shows as a long row rather than being hidden
;; by the vterm wrapping it.
(define longest-seed
  (let lp ([i 0] [best ""])
    (if (>= i tutorial-lesson-count)
        best
        (let ([s (tutorial-lesson-seed i)])
          (lp (+ i 1) (if (and s (> (string-length s) (string-length best))) s best))))))

(let ([scr (render->screen 120
             (lambda (p)
               (parameterize ([current-output-port p])
                 (render-practice-pane longest-seed 1 "NORMAL"
                                       "irrelevant to the width" "x"))))])
  (test-assert "no pane row overruns 80 columns, even for the roster's longest seed"
    (let lp ([r 0])
      (cond
        [(>= r (vterm-rows scr)) #t]
        [(> (string-length (vterm-row-text scr r)) 80) #f]
        [else (lp (+ r 1))]))))

(test-end)
