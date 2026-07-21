;;; test/test-tutorial-nav.ss -- The tutorial's navigation dispatch, pinned
;;; without a PTY.
;;;
;;; The tutorial's legend promises "[Enter] next   [q] quit   [p] previous".
;;; Making that true means three separate things have to hold, and this suite
;;; asserts each of them at the smallest level it can:
;;;
;;;   (1) the MOVEMENT rule -- tutorial-next-index is pure, so where a command
;;;       takes the reader is provable on its own: back clamps at the first
;;;       lesson rather than running negative, forward leaves the sequence at the
;;;       end so the loop terminates, and quit answers #f;
;;;
;;;   (2) the CLASSIFIERS -- one decoded key event, or one whole line, mapped to
;;;       one of the three commands.  Both are asserted against the same table,
;;;       including the Ctrl-D arm that exists because raw mode switches off the
;;;       terminal's own end-of-file processing, so the key arrives as a byte
;;;       rather than as an end-of-file object;
;;;
;;;   (3) the LOOP, end to end.  run-tutorial/reader takes its reader as an
;;;       argument, so the whole render-and-navigate cycle can be driven over
;;;       string ports -- no terminal, no PTY, no platform gate, and identical
;;;       behaviour everywhere.
;;;
;;; Group 4 is the regression proper, and is RED on the pre-fix tree: the prompt
;;; used to read a bare character from a cooked, line-buffered terminal, so "p"
;;; cost the reader two commands -- back one lesson, then forward one again on
;;; the leftover newline -- and the transcript never returned to a lower lesson
;;; number.  The same group's drained-input assertion is the other half of that
;;; defect: the byte left behind after "q" used to leak into the REPL as a blank
;;; line.
;;;
;;; run-tutorial itself is deliberately never called.  It gates on whether
;;; descriptor 0 is a terminal, and the suites run with stdin from /dev/null,
;;; which would make the result a property of the harness rather than of the
;;; code.  Every assertion goes through the injected-reader entry point instead.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (chezscheme)
        (only (hafod editor help)
              tutorial-lesson-count tutorial-next-index
              key->tutorial-command line->tutorial-command
              run-tutorial/reader)
        (only (hafod editor input-decode) make-key-event)
        (only (hafod terminal-caps) assume-terminal-caps))

;; ======================================================================
;; Helpers
;; ======================================================================

;; TEXT split on newlines.
(define (lines-of text)
  (let ([len (string-length text)])
    (let lp ([i 0] [start 0] [acc '()])
      (cond
        [(>= i len) (reverse (cons (substring text start len) acc))]
        [(char=? (string-ref text i) #\newline)
         (lp (+ i 1) (+ i 1) (cons (substring text start i) acc))]
        [else (lp (+ i 1) start acc)]))))

(define (starts-with? s prefix)
  (and (>= (string-length s) (string-length prefix))
       (string=? prefix (substring s 0 (string-length prefix)))))

;; The index of the first CH in S, or the length of S when it holds none.
(define (char-index s ch)
  (let ([len (string-length s)])
    (let lp ([i 0])
      (cond
        [(>= i len) len]
        [(char=? (string-ref s i) ch) i]
        [else (lp (+ i 1))]))))

(define heading-prefix "  Lesson ")

;; Is LINE one of the transcript's lines?
(define (has-line? text line)
  (let lp ([ls (lines-of text)])
    (cond
      [(null? ls) #f]
      [(string=? (car ls) line) #t]
      [else (lp (cdr ls))])))

;; Every lesson heading in TEXT reduced to its lesson NUMBER, in the order the
;; screens were painted.  That sequence is the reader's whole journey through
;; the tutorial, which is exactly what the navigation keys are meant to steer:
;; '(1 2 1) says the reader went forward once and then back once.
(define (heading-numbers text)
  (let lp ([ls (lines-of text)] [acc '()])
    (cond
      [(null? ls) (reverse acc)]
      [(starts-with? (car ls) heading-prefix)
       (let ([rest (substring (car ls) (string-length heading-prefix)
                              (string-length (car ls)))])
         (lp (cdr ls)
             (cons (string->number (substring rest 0 (char-index rest #\/))) acc)))]
      [else (lp (cdr ls) acc)])))

;; Drive the render-and-navigate loop end to end over INPUT, using the same
;; whole-line reader the non-terminal path builds.  Capabilities are forced off
;; so the transcript is plain text with not one escape byte in it.  Answers the
;; transcript and the input port -- the latter so a caller can prove nothing was
;; left sitting in it.
(define (drive-lines input)
  (let ([in (open-input-string input)]
        [sp (open-output-string)])
    (parameterize ([assume-terminal-caps 'off]
                   [current-output-port sp])
      (run-tutorial/reader (lambda () (line->tutorial-command (get-line in)))))
    (values (get-output-string sp) in)))

(test-begin "tutorial-nav")

;; ======================================================================
;; 1. The movement rule
;; ======================================================================

(test-equal "next from the first lesson advances one"
  1 (tutorial-next-index 0 tutorial-lesson-count 'next))

(test-equal "prev from the fourth lesson goes back one"
  2 (tutorial-next-index 3 tutorial-lesson-count 'prev))

(test-equal "prev at the first lesson clamps instead of going negative"
  0 (tutorial-next-index 0 tutorial-lesson-count 'prev))

(test-equal "quit answers #f, the driver's stop signal"
  #f (tutorial-next-index 5 tutorial-lesson-count 'quit))

(test-assert "next from the last lesson leaves the sequence, so the loop ends"
  (>= (tutorial-next-index (- tutorial-lesson-count 1) tutorial-lesson-count 'next)
      tutorial-lesson-count))

;; ======================================================================
;; 2. One keypress -> one command
;; ======================================================================

(test-equal "Enter advances"
  'next (key->tutorial-command (make-key-event 'special 'return 0)))

(test-equal "p goes back"
  'prev (key->tutorial-command (make-key-event 'char #\p 0)))

(test-equal "q quits"
  'quit (key->tutorial-command (make-key-event 'char #\q 0)))

;; Raw mode has cleared ICANON, so the terminal no longer performs end-of-file
;; processing: Ctrl-D reaches the decoder as an ordinary control byte.  Without
;; its own arm it would fall through to the advance default, and a reader who
;; pressed it to leave would be taken forward a lesson instead.
(test-equal "Ctrl-D quits, arriving as a byte rather than as end of input"
  'quit (key->tutorial-command (make-key-event 'ctrl #\d 0)))

(test-equal "an unbound key advances, which is what the legend implies"
  'next (key->tutorial-command (make-key-event 'char #\z 0)))

(test-equal "end of input quits"
  'quit (key->tutorial-command (eof-object)))

;; ======================================================================
;; 3. One line -> one command
;; ======================================================================

(test-equal "an empty line is the documented Enter"
  'next (line->tutorial-command ""))

(test-equal "\"p\" goes back"
  'prev (line->tutorial-command "p"))

(test-equal "\"q\" quits"
  'quit (line->tutorial-command "q"))

(test-equal "surrounding whitespace is tolerated"
  'prev (line->tutorial-command "   p  "))

(test-equal "a blank line means next"
  'next (line->tutorial-command "   "))

(test-equal "a typed word is ONE command, decided by its first character"
  'next (line->tutorial-command "next"))

(test-equal "end of input quits"
  'quit (line->tutorial-command (eof-object)))

;; ======================================================================
;; 4. The reported defect, driven end to end
;;
;; Enter, then p, then q.  The reader should go forward to lesson 2, back to
;; lesson 1, and leave -- so the painted sequence is 1, 2, 1.  Before the fix the
;; prompt read a bare character from a cooked terminal, so "p" was spent twice
;; (back one, then forward one on the leftover newline) and this sequence came
;; out 1, 2, 2: the transcript never returned to a lower lesson number.
;; ======================================================================

(let-values ([(text in) (drive-lines "\np\nq\n")])
  (test-equal "Enter then p walks 1 -> 2 -> 1: p moves back exactly one lesson"
    '(1 2 1) (heading-numbers text))
  (test-assert "the tutorial signs off after q"
    (has-line? text "  Tutorial ended."))
  ;; The other half of the same defect: the byte left unread after "q" used to
  ;; reach the REPL, which read it as a blank line.
  (test-assert "the input is fully consumed -- nothing left to leak into the REPL"
    (eof-object? (lookahead-char in))))

;; ======================================================================
;; 5. One command per line, however long the line
;; ======================================================================

(let-values ([(text in) (drive-lines "next\nq\n")])
  (test-equal "a five-character word costs ONE lesson, not five"
    '(1 2) (heading-numbers text))
  (test-assert "the input is fully consumed after the word"
    (eof-object? (lookahead-char in))))

;; ======================================================================
;; 6. The rendered screen
;;
;; Read through the vterm grid rather than as raw text, so the assertion is about
;; what the reader would SEE.  Rendering now happens outside the raw-mode extent,
;; in cooked mode; these two pin the consequence that matters -- the clear-screen
;; still homes the cursor, so each successive screen lands at the top rather than
;; scrolling down the terminal.
;; ======================================================================

;; Advancing all the way through leaves the loop at the end of the sequence,
;; without the sign-off, so the grid holds the last lesson painted.  That every
;; one of those repaints landed on row 0 is the homing proof.
(let ([scr (render->screen 80
             (lambda (p)
               (parameterize ([current-output-port p])
                 (run-tutorial/reader (lambda () 'next)))))]
      [last-heading (string-append heading-prefix
                                   (number->string tutorial-lesson-count)
                                   "/" (number->string tutorial-lesson-count) ":")])
  (test-assert "the final lesson's heading is homed on grid row 0"
    (starts-with? (vterm-row-text scr 0) last-heading)))

;; Quitting clears the screen a second time and signs off, so the sign-off is
;; what the grid holds -- again at the top.
(let ([scr (render->screen 80
             (lambda (p)
               (parameterize ([current-output-port p])
                 (run-tutorial/reader (lambda () 'quit)))))])
  (test-equal "the sign-off is homed on grid row 0"
    "  Tutorial ended." (vterm-row-text scr 0)))

(test-end)
