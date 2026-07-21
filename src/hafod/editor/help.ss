;;; (hafod editor help) -- Keybinding cheatsheet and interactive tutorial
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor help)
  (export show-keybindings run-tutorial
          ;; Exposed for the PTY-free navigation tests (white-box): the lesson
          ;; count, the pure index step, the two input classifiers, and the
          ;; render loop with its key read injected.
          tutorial-lesson-count tutorial-next-index
          key->tutorial-command line->tutorial-command
          run-tutorial/reader
          ;; The practice seam the editor fills at load (see below), the channel
          ;; that carries a live session to the reader, and the practice
          ;; lesson's classifier, verdict and pane -- all white-box, exposed so
          ;; the practice suite can drive each of them directly.
          tutorial-practice-open tutorial-practice-feed! tutorial-practice-view
          tutorial-practice-session
          key->practice-command tutorial-handle-key
          practice-verdict render-practice-pane
          ;; Per-lesson practice roster (white-box): what a lesson seeds its
          ;; buffer with, where the caret starts, what counts as done, and the
          ;; keystrokes it teaches.
          tutorial-lesson-seed tutorial-lesson-cursor
          tutorial-lesson-target tutorial-lesson-solution)
  (import (chezscheme)
          (only (hafod terminal-caps) ansi-ok? colour-ok?)
          ;; The single owner of raw-mode entry/exit, and the decoder every
          ;; editor keystroke already passes through. Both are leaf libraries:
          ;; importing (hafod editor editor) instead would close a cycle, since
          ;; that library imports this one.
          (only (hafod tty) with-raw-mode*)
          (only (hafod editor input-decode)
                read-key-event key-event-type key-event-value key-event-mods
                current-key-recording))

  ;; ======================================================================
  ;; ANSI formatting helpers
  ;; ======================================================================

  ;; Wrap S in the SGR set/reset codes, but only when the current output port is
  ;; colour-capable -- otherwise return S unchanged so no escape reaches a pipe, a
  ;; dumb terminal, or a NO_COLOR sink.
  (define (sgr set reset s)
    (if (colour-ok? (current-output-port))
        (string-append "\x1b;[" set "m" s "\x1b;[" reset "m")
        s))

  (define (bold s) (sgr "1" "22" s))
  (define (dim s) (sgr "2" "22" s))
  (define (cyan s) (sgr "36" "39" s))
  (define (yellow s) (sgr "33" "39" s))
  (define (green s) (sgr "32" "39" s))
  (define (magenta s) (sgr "35" "39" s))

  ;; Display width of a string, skipping ANSI escape sequences.
  (define (display-width s)
    (let ([len (string-length s)])
      (let lp ([i 0] [w 0])
        (cond
          [(>= i len) w]
          ;; Skip ESC [ ... final-byte sequences
          [(and (char=? (string-ref s i) #\x1b)
                (< (+ i 1) len)
                (char=? (string-ref s (+ i 1)) #\[))
           (let skip ([j (+ i 2)])
             (cond
               [(>= j len) (lp j w)]
               [(and (char>=? (string-ref s j) #\@)
                     (char<=? (string-ref s j) #\~))
                (lp (+ j 1) w)]
               [else (skip (+ j 1))]))]
          [else (lp (+ i 1) (+ w 1))]))))

  (define (pad-right s width)
    (let ([dw (display-width s)])
      (if (>= dw width) s
          (string-append s (make-string (- width dw) #\space)))))

  ;; ======================================================================
  ;; Keybinding cheatsheet
  ;; ======================================================================

  (define (show-keybindings)
    (for-each
      (lambda (line) (display line) (newline))
      (list
        ""
        (bold (cyan "  hafod keybinding reference"))
        ""
        (bold (yellow "  Vi Normal Mode"))
        (dim "  ──────────────────────────────────────────────────────────")
        (string-append "  " (pad-right (bold "h j k l") 22) "move left / history-prev / history-next / right")
        (string-append "  " (pad-right (bold "w W b B e E") 22) "word / WORD forward / backward / end")
        (string-append "  " (pad-right (bold "0 ^ $") 22) "beginning / first non-blank / end of line")
        (string-append "  " (pad-right (bold "f F t T") 22) "find / till char forward / backward")
        (string-append "  " (pad-right (bold "; ,") 22) "repeat / reverse last f/F/t/T")
        (string-append "  " (pad-right (bold "gg G") 22) "beginning / end of buffer")
        (string-append "  " (pad-right (bold "%") 22) "matching paren/bracket")
        (string-append "  " (pad-right (bold "( ) { }") 22) "backward/forward sexp, up/down list")
        ""
        (string-append "  " (pad-right (bold "i a I A") 22) "insert at cursor / after / at bol / at eol")
        (string-append "  " (pad-right (bold "o O") 22) "open line below / above")
        (string-append "  " (pad-right (bold "Esc") 22) "return to normal mode")
        ""
        (string-append "  " (pad-right (bold "d{motion}") 22) "delete (dd=line, dw=word, d$=to-end)")
        (string-append "  " (pad-right (bold "c{motion}") 22) "change (cc=line, cw=word, c$=to-end)")
        (string-append "  " (pad-right (bold "y{motion}") 22) "yank (yy=line, yw=word, y$=to-end)")
        (string-append "  " (pad-right (bold "D C Y") 22) "delete / change / yank to end of line")
        (string-append "  " (pad-right (bold "x X") 22) "delete char forward / backward")
        (string-append "  " (pad-right (bold "s S") 22) "substitute char / line")
        (string-append "  " (pad-right (bold "r{char}") 22) "replace char under cursor")
        (string-append "  " (pad-right (bold "~") 22) "toggle case")
        (string-append "  " (pad-right (bold "J") 22) "join lines")
        (string-append "  " (pad-right (bold "p P") 22) "paste after / before")
        (string-append "  " (pad-right (bold "u") 22) "undo")
        (string-append "  " (pad-right (bold "Ctrl+R") 22) "redo")
        (string-append "  " (pad-right (bold ".") 22) "repeat last edit")
        ""
        (string-append "  " (pad-right (bold "v V") 22) "visual mode (char / line)")
        (string-append "  " (pad-right (bold "d c y x") 22) "operate on visual selection")
        ""
        (string-append "  " (pad-right (bold "/{pattern}") 22) "search forward")
        (string-append "  " (pad-right (bold "?{pattern}") 22) "search backward")
        (string-append "  " (pad-right (bold "n N") 22) "next / previous search match")
        (string-append "  " (pad-right (bold "* #") 22) "search word under cursor fwd / bwd")
        ""
        (string-append "  " (pad-right (bold "\"{reg}") 22) "select register for next d/c/y/p")
        (string-append "  " (pad-right (bold "m{char}") 22) "set mark")
        (string-append "  " (pad-right (bold "'{char} `{char}") 22) "jump to mark")
        ""
        (string-append "  " (pad-right (bold "{count}{cmd}") 22) "repeat command N times (e.g. 3w, 5dd)")
        ""
        (bold (yellow "  Insert Mode"))
        (dim "  ──────────────────────────────────────────────────────────")
        (string-append "  " (pad-right (bold "Enter") 22) "smart return: newline if unbalanced, eval if balanced")
        (string-append "  " (pad-right (bold "Ctrl+J") 22) "force submit (eval regardless)")
        (string-append "  " (pad-right (bold "Tab") 22) "completion")
        (string-append "  " (pad-right (bold "Esc") 22) "enter normal mode")
        ""
        (bold (yellow "  Emacs / Shell Shortcuts (both modes)"))
        (dim "  ──────────────────────────────────────────────────────────")
        (string-append "  " (pad-right (bold "Ctrl+A / Ctrl+E") 22) "beginning / end of line")
        (string-append "  " (pad-right (bold "Ctrl+F / Ctrl+B") 22) "forward / backward char")
        (string-append "  " (pad-right (bold "Alt+F / Alt+B") 22) "forward / backward word")
        (string-append "  " (pad-right (bold "Ctrl+Left/Right") 22) "word movement (insert) / slurp-barf (normal)")
        (string-append "  " (pad-right (bold "Alt+Left/Right") 22) "word movement")
        (string-append "  " (pad-right (bold "Ctrl+K") 22) "kill to end of line")
        (string-append "  " (pad-right (bold "Ctrl+U") 22) "kill whole line")
        (string-append "  " (pad-right (bold "Ctrl+W") 22) "backward kill word")
        (string-append "  " (pad-right (bold "Alt+D") 22) "forward kill word")
        (string-append "  " (pad-right (bold "Ctrl+Y") 22) "yank (paste from kill ring)")
        (string-append "  " (pad-right (bold "Alt+Y") 22) "yank-pop (cycle kill ring)")
        (string-append "  " (pad-right (bold "Ctrl+D") 22) "delete char / EOF on empty")
        (string-append "  " (pad-right (bold "Ctrl+L") 22) "clear screen")
        (string-append "  " (pad-right (bold "Ctrl+P / Ctrl+N") 22) "history prev / next")
        (string-append "  " (pad-right (bold "Up / Down") 22) "history prev / next (prefix-filtered)")
        (string-append "  " (pad-right (bold "Ctrl+_ / Alt+/") 22) "undo / redo")
        ""
        (bold (yellow "  Fuzzy Finders"))
        (dim "  ──────────────────────────────────────────────────────────")
        (string-append "  " (pad-right (bold "Ctrl+R") 22) "fuzzy history search")
        (string-append "  " (pad-right (bold "Ctrl+T") 22) "file picker")
        (string-append "  " (pad-right (bold "Alt+C") 22) "directory picker")
        ""
        (bold (yellow "  Paredit / Structural Editing"))
        (dim "  ──────────────────────────────────────────────────────────")
        (string-append "  " (pad-right (bold "Alt+Ctrl+F / B") 22) "forward / backward sexp")
        (string-append "  " (pad-right (bold "Alt+Ctrl+U / D") 22) "up / down list")
        (string-append "  " (pad-right (bold "Alt+Ctrl+K") 22) "kill sexp")
        (string-append "  " (pad-right (bold "Ctrl+Right/Left") 22) "slurp / barf (normal mode)")
        (string-append "  " (pad-right (bold "Alt+Ctrl+Right") 22) "backward barf")
        (string-append "  " (pad-right (bold "Alt+Ctrl+Left") 22) "backward slurp")
        (string-append "  " (pad-right (bold "Alt+( / Alt+)") 22) "wrap round / forward slurp")
        (string-append "  " (pad-right (bold "Alt+S") 22) "splice sexp")
        (string-append "  " (pad-right (bold "Alt+R") 22) "raise sexp")
        (string-append "  " (pad-right (bold "Alt+Shift+S") 22) "split sexp")
        (string-append "  " (pad-right (bold "Alt+J") 22) "join sexp")
        (string-append "  " (pad-right (bold "> <") 22) "slurp / barf (normal mode)")
        ""
        (dim  "  Type (show-keybindings) to show this again.")
        (dim  "  Type (run-tutorial) for an interactive walkthrough.")
        "")))

  ;; ======================================================================
  ;; Interactive tutorial
  ;; ======================================================================

  ;; Each entry: (title instruction example hint cursor target solution)
  ;;
  ;; The first four are what a lesson has always carried.  The last three are
  ;; what makes a lesson practisable:
  ;;
  ;;   CURSOR    the index the caret starts at in the live buffer, or #f for a
  ;;             lesson that stays prose.  The field doubles as the roster's one
  ;;             statement of which lessons host a buffer at all -- a lesson
  ;;             names where the caret goes, or it does not host one.
  ;;   TARGET    the buffer text that counts as done, or #f for a lesson with
  ;;             nothing checkable to reach.
  ;;   SOLUTION  the keystrokes the instruction teaches.
  ;;
  ;; SOLUTION is read by nothing at run time except the on-screen hint.  Success
  ;; is decided by the final buffer text alone, so reaching a target by any other
  ;; route counts just as much -- deleting a word with four x presses passes the
  ;; lesson that teaches dw.  The two travel together: a lesson has both or
  ;; neither, since keystrokes worth showing are the ones that reach a target and
  ;; a target is only reachable if some keystrokes reach it.
  ;;
  ;; Every target here was derived by running its solution through the editor's
  ;; own dispatch rather than worked out on paper, and the practice suite pins
  ;; each of them by doing it again -- so a target that drifts from what the keys
  ;; actually do fails a test rather than reaching a reader.
  ;;
  ;; The roster falls into three kinds:
  ;;
  ;;   PROSE          no cursor, so no buffer.  Either the lesson has no example
  ;;                  text to put in one, or it cannot host one at all.
  ;;   OPEN PRACTICE  a buffer to move around in, and no target.  These lessons
  ;;                  teach MOTION, and a motion leaves the text exactly as it
  ;;                  found it -- so a verdict read off the final text could only
  ;;                  ever be a lie.  The caret moving under the framed text is
  ;;                  the feedback, and for a motion it is the right feedback.
  ;;   CHECKED        a buffer, a target, and the keystrokes that reach it.
  (define tutorial-lessons
    '#(
       ;; Prose: no example text to seed a buffer with.
       ("Welcome"
        "Welcome to the hafod interactive tutorial!\nThis will walk you through the key features of the editor.\nPress Enter to continue to the next lesson."
        ""
        ""
        #f #f "")
       ;; Open practice: h and l move the caret and leave the text alone.
       ("Basic movement"
        "In normal mode (press Esc first), use h/l to move left/right.\nTry: type some text, press Esc, then h and l to move around.\nPress Enter when done."
        "(define hello 42)"
        "Use h to move left, l to move right"
        8 #f "")
       ("Word movement"
        "w moves to the start of the next word, b moves back.\ne moves to the end of the current word.\nTry w, b, and e in normal mode."
        "(string-append foo bar)"
        "w=next word, b=prev word, e=end of word"
        1 #f "")
       ;; Prose: no example text to seed a buffer with.
       ("Insert mode"
        "Press i to insert at cursor, a to insert after cursor.\nI inserts at line start, A at line end.\no opens a new line below, O above."
        ""
        "i/a/I/A/o/O enter insert mode at different positions"
        #f #f "")
       ("Deleting"
        "In normal mode:\n  x  = delete char under cursor\n  dw = delete to next word\n  dd = delete whole line\n  D  = delete to end of line\n\nThe caret is on remove-me. Press dw to take the word out."
        "(remove-me keep-this)"
        "x, dw, dd, D are the delete commands"
        1 "(keep-this)" "dw")
       ("Changing"
        "c is like d but enters insert mode after:\n  cw = change word\n  cc = change whole line\n  C  = change to end of line\n\nThe caret is on old-value. Press cw, type new, then Esc."
        "(old-value)"
        "c{motion} deletes and enters insert mode"
        1 "(new)" "cwnew")
       ("Yank and paste"
        "y yanks (copies) text:\n  yw = yank word\n  yy = yank line\np pastes after cursor, P pastes before.\n\nPress yy to yank the line, $ to reach its end, then p."
        "(copy this text)"
        "yy to yank line, p to paste"
        1 "(copy this text)(copy this text)" "yy$p")
       ;; Open practice: f, t, ; and , are all motions.
       ("Find char"
        "f{char} jumps to the next occurrence of {char}.\nF{char} jumps backward. t/T stop one char before.\n; repeats the last find, , reverses it."
        "(define (factorial n) (* n (factorial (- n 1))))"
        "Try fa to jump to next 'a', then ; to repeat"
        1 #f "")
       ("Search"
        "/{pattern} searches forward, ?{pattern} backward.\nn goes to next match, N to previous.\n* searches for the word under cursor."
        "(let ([x 10] [y 20]) (+ x y))"
        "Type /let then Enter, then n for next match"
        1 #f "")
       ("Text objects"
        "In operator-pending mode (after d/c/y):\n  iw = inner word    aw = around word\n  i( = inside parens  a( = around parens\n  i\" = inside quotes   a\" = around quotes\n\nThe caret is inside the inner list. Press di( to empty it."
        "(delete (inner \"content\") here)"
        "Try di( to delete inside parens, or ci\" to change quoted text"
        9 "(delete () here)" "di(")
       ("Visual mode"
        "v enters visual mode (character selection).\nV enters visual line mode.\nMove with any motion, then d/c/y to operate.\n\nPress v, then e to reach the end of some, then d."
        "(select some of this text)"
        "v then motion then d to delete selection"
        7 "(select of this text)" "ved")
       ("Paredit"
        "Structural editing preserves balanced parens:\n  Alt+Ctrl+F/B = navigate sexps\n  Alt+( = wrap in parens\n  Alt+S = splice (remove outer parens)\n  > = slurp (pull next sexp in)\n  < = barf (push last sexp out)\n\nThe caret is inside (bar baz). Press > to slurp quux in."
        "(foo (bar baz) quux)"
        "Try Alt+( to wrap, > to slurp in normal mode"
        6 "(foo (bar baz quux))" ">")
       ;; 3w and 4l are motions, so the checked solution is the editing form.
       ("Count prefix"
        "Most commands accept a count prefix:\n  5x = delete 5 chars\n  3w = move 3 words forward\n  2dd = delete 2 lines (whole buffer, here)\n  4l = move 4 chars right\n\nThe caret is on five. Press 5x to take five characters out."
        "(five four three two one)"
        "Try 5x to delete five characters"
        1 "(four three two one)" "5x")
       ;; Prose: no example text to seed a buffer with.
       ("Emacs shortcuts"
        "These work in both modes:\n  Ctrl+A/E = start/end of line\n  Ctrl+K = kill to end\n  Ctrl+W = backward kill word\n  Ctrl+Y = yank\n  Ctrl+R = fuzzy history search"
        ""
        "These are readline/emacs compatible bindings"
        #f #f "")
       ;; Prose: this lesson is about what Enter does in the buffer, and Enter is
       ;; the reader's way out of every lesson -- so it is the one lesson that
       ;; cannot host the buffer it describes.  (Its example spans two lines
       ;; besides, and the pane frames one.)
       ("Smart return"
        "In insert mode, Enter is context-aware:\n  - If the expression is unbalanced, it inserts a newline\n    with auto-indentation\n  - If balanced and cursor is at the end, it evaluates\n  - In normal mode, Enter always evaluates"
        "(define (multi-line\n  expression)"
        "Type an open paren, press Enter to get auto-indented newline"
        #f #f "")
       ;; Open practice: setting a mark and jumping to it are both motions.
       ("Marks and registers"
        "m{a-z} sets a named mark at the cursor.\n'{a-z} jumps to that mark.\n\"{a-z} selects a register for the next d/c/y/p."
        "(mark this position and return later)"
        "ma to set mark 'a', then 'a to jump back"
        1 #f "")
       ;; Prose: no example text to seed a buffer with.
       ("Congratulations!"
        "You've completed the hafod tutorial!\n\nType (show-keybindings) at any time for a quick reference.\n\nHappy hacking!"
        ""
        ""
        #f #f "")
       ))

  ;; How many lessons there are, published so a caller need not re-derive it
  ;; from the vector or hard-code today's count.
  (define tutorial-lesson-count (vector-length tutorial-lessons))

  ;; Where lesson I's caret starts, or #f when the lesson stays prose.
  (define (tutorial-lesson-cursor i)
    (list-ref (vector-ref tutorial-lessons i) 4))

  ;; The text lesson I seeds its practice buffer with: its example, but only for
  ;; a lesson that names a starting caret and carries an example to put under it.
  ;; A prose lesson answers #f and goes on printing its example as a plain line,
  ;; exactly as it always has.
  (define (tutorial-lesson-seed i)
    (and (tutorial-lesson-cursor i)
         (let ([example (list-ref (vector-ref tutorial-lessons i) 2)])
           (and (> (string-length example) 0) example))))

  ;; The buffer text that counts as done, or #f for a lesson with no verdict.
  (define (tutorial-lesson-target i)
    (list-ref (vector-ref tutorial-lessons i) 5))

  ;; The keystrokes lesson I teaches, shown as a hint and never consulted to
  ;; decide anything.
  (define (tutorial-lesson-solution i)
    (list-ref (vector-ref tutorial-lessons i) 6))

  ;; The one command table, shared by the keypress and the whole-line readers so
  ;; the two paths cannot drift apart: q leaves, p goes back, and everything else
  ;; advances -- which is what makes the legend's [Enter] true of Space, of a
  ;; stray letter, and of an arrow key alike.
  (define (char->tutorial-command ch)
    (case ch
      [(#\q) 'quit]
      [(#\p) 'prev]
      [else 'next]))

  ;; Where a command moves the reader, kept pure so the movement rule is
  ;; provable on its own. Going back clamps at the first lesson rather than
  ;; running negative; going forward stops at N, one past the last lesson, which
  ;; is how the loop terminates; quitting answers #f, which it reads as "stop
  ;; now, and say so".
  ;;
  ;; A keystroke spent on the practice buffer, and a re-seed of it, both leave
  ;; the reader where they are. The lesson loop settles those two itself and they
  ;; never reach here, but they are named ahead of the default arm rather than
  ;; left to fall into it: the default advances, and silently advancing a lesson
  ;; on every key typed into its buffer is precisely the wrong answer.
  (define (tutorial-next-index i n cmd)
    (case cmd
      [(prev) (max 0 (- i 1))]
      [(quit) #f]
      [(stay reset) i]
      [else (min n (+ i 1))]))

  ;; Classify one decoded key event.
  ;;
  ;; Ctrl-D earns an arm of its own because raw mode has switched off the
  ;; terminal's own end-of-file processing: with ICANON clear the key arrives as
  ;; an ordinary control byte rather than as an end-of-file object, and a reader
  ;; who presses it plainly means to leave. Genuine end of input means the same.
  (define (key->tutorial-command ke)
    (cond
      [(eof-object? ke) 'quit]
      [(and (eq? (key-event-type ke) 'ctrl)
            (eqv? (key-event-value ke) #\d))
       'quit]
      [(eq? (key-event-type ke) 'char)
       (char->tutorial-command (key-event-value ke))]
      [else 'next]))

  ;; Classify one whole line, for the runs where the reader is a script rather
  ;; than a terminal. Consuming the WHOLE line is the entire point: the newline
  ;; that ends it is retired here instead of being left in the port to be read
  ;; back as a second, phantom command, and a reader who types a word spends one
  ;; lesson on it rather than one lesson per character. The first non-blank
  ;; character decides, so "  p  " still goes back, and an empty or blank line
  ;; is the Enter the legend advertises.
  (define (line->tutorial-command line)
    (if (eof-object? line)
        'quit
        (let ([len (string-length line)])
          (let lp ([i 0])
            (cond
              [(>= i len) 'next]
              [(char-whitespace? (string-ref line i)) (lp (+ i 1))]
              [else (char->tutorial-command (string-ref line i))])))))

  ;; ======================================================================
  ;; The practice seam
  ;; ======================================================================

  ;; A practice lesson wants a REAL editor buffer -- the actual editor-state, the
  ;; actual keymaps, the actual per-key dispatch -- because the whole point is
  ;; that the reader practises on the editor they are about to use, not on a
  ;; second one written to look like it. But the edge runs the other way: (hafod
  ;; editor editor) imports this library, so importing it back would close a
  ;; cycle, and a tutorial-local re-implementation would teach a buffer nobody
  ;; else has.
  ;;
  ;; The same problem is already solved once, on this same edge. vi.ss is
  ;; imported BY editor.ss and needs its snapshot, undo and insert procedures; it
  ;; declares #f-defaulted parameters and editor.ss fills them at load. This
  ;; library sits in exactly that position, so it declares the same shape: three
  ;; procedures the editor hands over, and no import at all in the other
  ;; direction.
  ;;
  ;;   tutorial-practice-open   (seed cursor)         -> session
  ;;   tutorial-practice-feed!  (session evt in-port) -> void, one real keystroke
  ;;   tutorial-practice-view   (session)             -> text, cursor, mode label
  ;;
  ;; A session is opaque here. It is an editor-state on the far side of the seam
  ;; and a token on this one, and nothing below inspects it.
  ;;
  ;; With the seam unset -- this library imported on its own, without the editor
  ;; ever being loaded -- every lesson renders as prose and the tutorial behaves
  ;; exactly as it did before there were practice buffers. That is the honest
  ;; degradation for a build that has no editor to practise on.
  (define tutorial-practice-open  (make-parameter #f))
  (define tutorial-practice-feed! (make-parameter #f))
  (define tutorial-practice-view  (make-parameter #f))

  ;; The lesson's live session, for the extent of one render-and-read.
  ;;
  ;; A parameter rather than a fourth argument to the reader, because the
  ;; reader's zero-argument contract is what lets the entire loop be driven over
  ;; string ports -- which is how the navigation suite asserts anything at all --
  ;; and that contract is already relied upon. Reaching the session through a
  ;; dynamic extent instead is the same channel the codebase already uses for the
  ;; fuzzy precompute cache and the keystroke-recording tee.
  (define tutorial-practice-session (make-parameter #f))

  ;; Does EV match this type, character and modifier set exactly?
  (define (key-event-is? ev type value mods)
    (and (eq? (key-event-type ev) type)
         (eqv? (key-event-value ev) value)
         (eqv? (key-event-mods ev) mods)))

  ;; Classify one decoded key event during a lesson that has a live buffer.
  ;;
  ;; Deliberately a SEPARATE table from key->tutorial-command, which is unchanged:
  ;; in a practice lesson only Enter and a named set of control keys are
  ;; reserved, and every printable key belongs to the buffer. That rule is what
  ;; makes r, q and p mean in the pane what vi means by them, rather than
  ;; quietly ending the lesson.
  ;;
  ;; Every arm matches the WHOLE key event, modifiers included. Ctrl-T with no
  ;; modifier is the file picker, absorbed below; C-M-t is the s-expression
  ;; transpose the Paredit lesson teaches -- the same type and character with
  ;; MOD_ALT set. A table testing only the type and the character would swallow
  ;; both, and the Paredit lesson would stop working without anything failing.
  (define (key->practice-command ev)
    (cond
      ;; Ctrl-D earns its own arm here for the reason it does above: raw mode has
      ;; cleared ICANON, so it arrives as an ordinary control byte and never as
      ;; an end-of-file object. Genuine end of input means the same thing.
      [(eof-object? ev) 'quit]
      [(key-event-is? ev 'ctrl #\d 0) 'quit]
      ;; Enter is the promise that a reader is never trapped in a lesson, so it
      ;; is classified here, ahead of the buffer -- which in insert mode would
      ;; otherwise swallow it as a newline.
      [(key-event-is? ev 'special 'return 0) 'next]
      [(key-event-is? ev 'ctrl #\p 0) 'prev]
      ;; Ctrl-R re-seeds the lesson. It is the editor's redo, and its fuzzy
      ;; history search, so both are out of reach while practising: no lesson
      ;; teaches either, and being able to start the exercise again is worth more
      ;; here than a redo stack that begins empty anyway.
      [(key-event-is? ev 'ctrl #\r 0) 'reset]
      ;; Absorbed rather than fed to the buffer, in two groups.
      ;;
      ;; Up, Down, Ctrl-P and Ctrl-N reach the editor's history commands, which
      ;; on the first line of a buffer navigate HISTORY -- and the history handle
      ;; is opened lazily on first touch, so one of these in a one-line practice
      ;; buffer would open the reader's real history database and swap the seeded
      ;; lesson text for their last REPL line. (Ctrl-P is claimed above, as the
      ;; way back a lesson.)
      ;;
      ;; Ctrl-T and Alt-C -- and Ctrl-R, claimed above -- reach the three fuzzy
      ;; pickers. Those paint a full-screen finder over the tutorial's own screen
      ;; from inside its raw-mode extent, and no guard would catch it: the finder
      ;; does not raise, it runs. One of them shells out to git ls-files on the
      ;; way, and one replaces the practice buffer outright with a cd line.
      ;;
      ;; Tab opens a completion menu sized to a terminal this pane does not own.
      ;; Ctrl-L is deliberately NOT absorbed: it is self-healing, since the next
      ;; keystroke clears and repaints the whole screen anyway.
      [(or (key-event-is? ev 'special 'up 0)
           (key-event-is? ev 'special 'down 0)
           (key-event-is? ev 'ctrl #\n 0)
           (key-event-is? ev 'special 'tab 0)
           (key-event-is? ev 'ctrl #\t 0)
           (key-event-is? ev 'meta #\c 0))
       'ignore]
      [else 'edit]))

  ;; The whole of the checking rule, and it reads nothing but the text: no target
  ;; means there is nothing to be right about, matching text means done, and
  ;; anything else means not yet. The route taken is never looked at, so four x
  ;; presses pass the lesson that teaches dw.
  (define (practice-verdict text target)
    (cond
      [(not target) 'open]
      [(string=? text target) 'met]
      [else 'unmet]))

  ;; ======================================================================
  ;; The practice pane
  ;; ======================================================================

  ;; Glyphs by code point rather than as literal characters, matching finder.ss:
  ;; the source stays ASCII, so nothing between here and a terminal can mangle
  ;; them.
  (define box-top-left     #\x250c)
  (define box-top-right    #\x2510)
  (define box-bottom-left  #\x2514)
  (define box-bottom-right #\x2518)
  (define box-horizontal   #\x2500)
  (define box-vertical     #\x2502)
  (define verdict-tick     #\x2713)

  ;; Columns between the two vertical rules. Fixed rather than sized to the
  ;; terminal, which is what lets the pane be drawn without a size query at all:
  ;; 52 holds the longest example in the roster, and the whole frame is 56
  ;; columns with its indent -- comfortably inside the 80 a terminal is assumed
  ;; to have.
  (define practice-pane-width 52)

  ;; The top or bottom rule of the frame.
  (define (practice-pane-rule left right)
    (string-append "  " (dim (string-append (string left)
                                            (make-string practice-pane-width box-horizontal)
                                            (string right)))))

  ;; One framed row: the vertical rules with CONTENT padded out between them.
  (define (practice-pane-row content)
    (string-append "  " (dim (string box-vertical))
                   (pad-right content practice-pane-width)
                   (dim (string box-vertical))))

  ;; Clip S to the frame's width, so a verdict line can never outgrow the box
  ;; above it however long a future lesson's target becomes.
  (define (clip-to-pane s)
    (if (> (string-length s) practice-pane-width)
        (substring s 0 practice-pane-width)
        s))

  ;; What the buffer's current text amounts to, said in one line.
  ;;
  ;; A nudge only ever describes. The reader can already see what the buffer
  ;; holds -- it is framed directly above -- so naming the target in full puts
  ;; both ends of the gap on the screen at once. Nothing is withheld to make the
  ;; lesson harder, and nothing here blocks: whatever this line says, Enter
  ;; carries on to the next lesson.
  (define (practice-verdict-line text target)
    (case (practice-verdict text target)
      [(met)
       (green (string-append "  " (string verdict-tick)
                             " Nice -- buffer is now " (clip-to-pane target)))]
      [(unmet)
       (dim (string-append "  Aiming for: " (clip-to-pane target)))]
      [else
       (dim "  Practise freely -- press Enter when you are done.")]))

  ;; Draw the live buffer: a framed row of text, a caret row marking the cursor,
  ;; a status row carrying the editor's own mode label beside the keystrokes this
  ;; lesson teaches, and the verdict.
  (define (render-practice-pane text cursor mode-label target solution)
    (let* ([room (- practice-pane-width 1)]   ;; one leading space inside the frame
           [shown (if (> (string-length text) room) (substring text 0 room) text)]
           [before (substring text 0 (min (max cursor 0) (string-length text)))]
           [caret-col (min room (display-width before))])
      (display (practice-pane-rule box-top-left box-top-right))
      (newline)
      (display (practice-pane-row (string-append " " shown)))
      (newline)
      (display (practice-pane-row (string-append (make-string (+ caret-col 1) #\space)
                                                 (yellow "^"))))
      (newline)
      (display (practice-pane-rule box-bottom-left box-bottom-right))
      (newline)
      (display (string-append "    " (yellow mode-label)
                              (if (> (string-length solution) 0)
                                  (dim (string-append "   try: " solution))
                                  "")))
      (newline)
      (newline)
      (display (practice-verdict-line text target))
      (newline)))

  ;; ======================================================================
  ;; Handling one keystroke
  ;; ======================================================================

  ;; Everything the keypress reader does once it holds an event, lifted clear of
  ;; the raw-mode wrapper so a suite can drive it with no terminal in sight. IN
  ;; is the port the editor's own inline reads pull their remaining bytes from.
  ;;
  ;; With no live session this is the prose tutorial's classifier, unchanged.
  ;;
  ;; With one, the event is CLASSIFIED FIRST and fed to the buffer only on the
  ;; edit arm. That ordering is the whole of the never-trapped guarantee: it is
  ;; why Enter still advances when the buffer is mid-insert, mid-operator or
  ;; holding a visual selection, any of which would have consumed the key had it
  ;; reached the dispatch. The ordering is asserted rather than assumed -- a
  ;; suite feeds Enter to a buffer in each of those states and checks both that
  ;; the answer is 'next AND that the text did not move.
  ;;
  ;; The feed is guarded: an editor command that objects to a buffer with no
  ;; history and no prompt behind it should cost the reader one inert keystroke,
  ;; not the tutorial.
  (define (tutorial-handle-key ev in session)
    (if (not session)
        (key->tutorial-command ev)
        ;; Classified here, before a single arm below can touch the buffer.
        (let ([cmd (key->practice-command ev)])
          (case cmd
            [(edit)
             (guard (e [#t (void)])
               (let ([feed (tutorial-practice-feed!)])
                 (when feed (feed session ev in))))
             'stay]
            [(ignore) 'stay]
            [else cmd]))))

  ;; Open lesson I's practice buffer, or answer #f when it has none to open --
  ;; because the lesson carries no seed, or because the seam was never filled.
  (define (open-practice-session i)
    (let ([seed (tutorial-lesson-seed i)]
          [open (tutorial-practice-open)])
      (and seed open (tutorial-practice-feed!) (tutorial-practice-view)
           (open seed (tutorial-lesson-cursor i)))))

  ;; The two legends. A prose lesson keeps the one it has always shown; a
  ;; practice lesson advertises the keys its own table actually reserves, since
  ;; q and p have gone back to being buffer keys there.
  (define tutorial-legend "  [Enter] next   [q] quit   [p] previous")
  (define practice-legend
    "  [Enter] next   [Ctrl-P] previous   [Ctrl-R] reset   [Ctrl-D] quit")

  ;; The tutorial's render-and-navigate loop, with the key read injected as
  ;; READ-COMMAND -- a thunk answering one of the three navigation commands.
  ;; Holding the terminal at arm's length this way lets the whole loop be driven
  ;; over string ports, and lets run-tutorial choose between a keypress and a
  ;; line without duplicating a line of the rendering below.
  ;;
  ;; The one-argument form is the original contract exactly, with no practice
  ;; sessions anywhere in it. The two-argument form opens one per lesson that has
  ;; a seed, when PRACTICE? says the caller can drive it.
  (define run-tutorial/reader
    (case-lambda
      [(read-command) (run-tutorial/reader read-command #f)]
      [(read-command practice?)
       (let ([n tutorial-lesson-count])
         (let lp ([i 0])
           (when (< i n)
             (let* ([lesson (vector-ref tutorial-lessons i)]
                    [title (car lesson)]
                    [instr (cadr lesson)]
                    [example (caddr lesson)]
                    [hint (cadddr lesson)]
                    [target (tutorial-lesson-target i)]
                    [solution (tutorial-lesson-solution i)]
                    ;; Paint one screen for this lesson. SESSION is its live
                    ;; buffer, or #f for a lesson that has none -- and in that
                    ;; case not a byte of this differs from what the tutorial has
                    ;; always printed.
                    [paint
                     (lambda (session)
                       (when (ansi-ok? (current-output-port))
                         (display "\x1b;[2J\x1b;[H"))  ;; clear screen (only on a capable terminal)
                       (display (bold (cyan (string-append "  Lesson " (number->string (+ i 1))
                                                           "/" (number->string n)
                                                           ": " title))))
                       (newline) (newline)
                       ;; Display instruction with proper indentation
                       (let ([lines (string-split instr #\newline)])
                         (for-each
                           (lambda (line)
                             (display "  ")
                             (display line)
                             (newline))
                           lines))
                       (newline)
                       ;; The example is printed only when there is no pane: with
                       ;; one, the same text is in the frame below, live.
                       (when (and (not session) (> (string-length example) 0))
                         (display (dim "  Example: "))
                         (display (green example))
                         (newline))
                       (when (> (string-length hint) 0)
                         (display (dim (string-append "  Hint: " hint)))
                         (newline))
                       (newline)
                       (when session
                         (let-values ([(text cursor mode-label)
                                       ((tutorial-practice-view) session)])
                           (render-practice-pane text cursor mode-label target solution))
                         (newline))
                       (display (dim (if session practice-legend tutorial-legend)))
                       (newline) (newline))])
               ;; One lesson, repainted after every keystroke that went to its
               ;; buffer, and answering the navigation command that finally ends
               ;; it -- so 'stay and 'reset are settled here and never reach the
               ;; step below. Recurring from OUTSIDE the parameterize keeps this a
               ;; tail call: a lesson can be repainted any number of times without
               ;; piling up dynamic extents.
               (let ([cmd (let again ([session (and practice? (open-practice-session i))])
                            (let ([c (parameterize ([tutorial-practice-session session])
                                       (paint session)
                                       (read-command))])
                              (case c
                                [(stay) (again session)]
                                [(reset) (again (open-practice-session i))]
                                [else c])))])
                 (let ([next-i (tutorial-next-index i n cmd)])
                   (if next-i
                       (lp next-i)
                       (begin
                         (when (ansi-ok? (current-output-port))
                           (display "\x1b;[2J\x1b;[H"))
                         (display "  Tutorial ended.\n\n")))))))))]))

  ;; The tutorial proper, gating on whether standard input is a terminal.
  ;;
  ;; On a terminal the legend is taken literally -- one keystroke, one command,
  ;; no Enter needed. The raw-mode extent covers the read AND the handling of
  ;; what was read, but no more: the editor's own inline reads (a text object's
  ;; or a surround's follow-up character, a search pattern up to its Enter) pull
  ;; their remaining bytes from this same port and must find it raw, so handing
  ;; the key on outside the extent would leave them reading a cooked, line-
  ;; buffered terminal. Rendering stays outside, in cooked mode, where the output
  ;; flags still translate the newlines the display code emits; a multi-row pane
  ;; painted inside a raw extent would staircase down the screen. So: raw mode
  ;; wraps input, cooked mode wraps output. Two attribute changes per keystroke
  ;; at reading pace cost nothing. with-raw-mode* is the single owner of that
  ;; transition: it winds the cooked baseline back on a normal return, on a
  ;; raised condition and across a suspend, and it leaves the signal-generating
  ;; characters enabled, so Ctrl-C keeps its usual disposition and unwinds
  ;; through that same restore.
  ;;
  ;; Practice buffers are opened on THIS arm only, and deliberately not on the
  ;; strength of the seam being filled: the editor is loaded in every real run,
  ;; so the seam is always filled, and gating on it would open a live pane on the
  ;; piped path too -- where the whole-line classifier has no edit arm and so
  ;; could never drive the buffer, leaving a nudge no input could ever clear.
  ;;
  ;; Off a terminal -- piped, redirected, or a dumb TERM -- there are no
  ;; attributes to set and with-raw-mode* would raise, so a scripted run reads
  ;; whole lines instead: one line, one lesson, a blank line meaning next, and
  ;; the slideshow it has always been. Degrading beats erroring.
  ;;
  ;; The gate asks about descriptor 0 by number rather than about a port object,
  ;; because the question is whether this process's standard input is a terminal
  ;; and the shared console port aliases elsewhere.
  ;;
  ;; The keystroke-recording tee is held off for the whole run. The decoder copies
  ;; every character it consumes into it, and a key pressed at a tutorial lesson
  ;; has no business landing in a half-open recording of a change made at the
  ;; REPL.
  (define (run-tutorial)
    (parameterize ([current-key-recording #f])
      (if (ansi-ok? 0)
          (run-tutorial/reader
            (lambda ()
              ;; Push the prompt out before blocking: the read below waits on a
              ;; person, so anything still buffered would leave them staring at a
              ;; half-drawn lesson.
              (flush-output-port (current-output-port))
              (with-raw-mode* 0
                (lambda ()
                  (let ([in (console-input-port)])
                    (tutorial-handle-key (read-key-event in) in
                                         (tutorial-practice-session))))))
            #t)
          (run-tutorial/reader
            (lambda ()
              (flush-output-port (current-output-port))
              (line->tutorial-command (get-line (console-input-port))))))))

  ;; Helper: split string on delimiter char
  (define (string-split str delim)
    (let ([len (string-length str)])
      (let lp ([i 0] [start 0] [acc '()])
        (cond
          [(>= i len)
           (reverse (cons (substring str start len) acc))]
          [(char=? (string-ref str i) delim)
           (lp (+ i 1) (+ i 1)
               (cons (substring str start i) acc))]
          [else (lp (+ i 1) start acc)]))))

) ; end library
