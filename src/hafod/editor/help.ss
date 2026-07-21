;;; (hafod editor help) -- Keybinding cheatsheet and interactive tutorial
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor help)
  (export show-keybindings run-tutorial
          ;; Exposed for the PTY-free navigation tests (white-box): the lesson
          ;; count, the pure index step, the two input classifiers, and the
          ;; render loop with its key read injected.
          tutorial-lesson-count tutorial-next-index
          key->tutorial-command line->tutorial-command
          run-tutorial/reader)
  (import (chezscheme)
          (only (hafod terminal-caps) ansi-ok? colour-ok?)
          ;; The single owner of raw-mode entry/exit, and the decoder every
          ;; editor keystroke already passes through. Both are leaf libraries:
          ;; importing (hafod editor editor) instead would close a cycle, since
          ;; that library imports this one.
          (only (hafod tty) with-raw-mode*)
          (only (hafod editor input-decode)
                read-key-event key-event-type key-event-value))

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

  (define tutorial-lessons
    '#(
       ;; Each entry: (title instruction example-command expected-hint)
       ("Welcome"
        "Welcome to the hafod interactive tutorial!\nThis will walk you through the key features of the editor.\nPress Enter to continue to the next lesson."
        ""
        "")
       ("Basic movement"
        "In normal mode (press Esc first), use h/l to move left/right.\nTry: type some text, press Esc, then h and l to move around.\nPress Enter when done."
        "(define hello 42)"
        "Use h to move left, l to move right")
       ("Word movement"
        "w moves to the start of the next word, b moves back.\ne moves to the end of the current word.\nTry w, b, and e in normal mode."
        "(string-append foo bar)"
        "w=next word, b=prev word, e=end of word")
       ("Insert mode"
        "Press i to insert at cursor, a to insert after cursor.\nI inserts at line start, A at line end.\no opens a new line below, O above."
        ""
        "i/a/I/A/o/O enter insert mode at different positions")
       ("Deleting"
        "In normal mode:\n  x  = delete char under cursor\n  dw = delete to next word\n  dd = delete whole line\n  D  = delete to end of line\nTry each one."
        "(remove-me keep-this)"
        "x, dw, dd, D are the delete commands")
       ("Changing"
        "c is like d but enters insert mode after:\n  cw = change word\n  cc = change whole line\n  C  = change to end of line"
        "(old-value)"
        "c{motion} deletes and enters insert mode")
       ("Yank and paste"
        "y yanks (copies) text:\n  yw = yank word\n  yy = yank line\np pastes after cursor, P pastes before."
        "(copy this text)"
        "yy to yank line, p to paste")
       ("Find char"
        "f{char} jumps to the next occurrence of {char}.\nF{char} jumps backward. t/T stop one char before.\n; repeats the last find, , reverses it."
        "(define (factorial n) (* n (factorial (- n 1))))"
        "Try fa to jump to next 'a', then ; to repeat")
       ("Search"
        "/{pattern} searches forward, ?{pattern} backward.\nn goes to next match, N to previous.\n* searches for the word under cursor."
        "(let ([x 10] [y 20]) (+ x y))"
        "Type /let then Enter, then n for next match")
       ("Text objects"
        "In operator-pending mode (after d/c/y):\n  iw = inner word    aw = around word\n  i( = inside parens  a( = around parens\n  i\" = inside quotes   a\" = around quotes"
        "(delete (inner \"content\") here)"
        "Try di( to delete inside parens, or ci\" to change quoted text")
       ("Visual mode"
        "v enters visual mode (character selection).\nV enters visual line mode.\nMove with any motion, then d/c/y to operate."
        "(select some of this text)"
        "v then motion then d to delete selection")
       ("Paredit"
        "Structural editing preserves balanced parens:\n  Alt+Ctrl+F/B = navigate sexps\n  Alt+( = wrap in parens\n  Alt+S = splice (remove outer parens)\n  > = slurp (pull next sexp in)\n  < = barf (push last sexp out)"
        "(foo (bar baz) quux)"
        "Try Alt+( to wrap, > to slurp in normal mode")
       ("Count prefix"
        "Most commands accept a count prefix:\n  3w = move 3 words forward\n  5x = delete 5 chars\n  2dd = delete 2 lines (whole buffer, here)\n  4l = move 4 chars right"
        "(one two three four five six)"
        "Try 3w to jump 3 words forward")
       ("Emacs shortcuts"
        "These work in both modes:\n  Ctrl+A/E = start/end of line\n  Ctrl+K = kill to end\n  Ctrl+W = backward kill word\n  Ctrl+Y = yank\n  Ctrl+R = fuzzy history search"
        ""
        "These are readline/emacs compatible bindings")
       ("Smart return"
        "In insert mode, Enter is context-aware:\n  - If the expression is unbalanced, it inserts a newline\n    with auto-indentation\n  - If balanced and cursor is at the end, it evaluates\n  - In normal mode, Enter always evaluates"
        "(define (multi-line\n  expression)"
        "Type an open paren, press Enter to get auto-indented newline")
       ("Marks and registers"
        "m{a-z} sets a named mark at the cursor.\n'{a-z} jumps to that mark.\n\"{a-z} selects a register for the next d/c/y/p."
        "(mark this position and return later)"
        "ma to set mark 'a', then 'a to jump back")
       ("Congratulations!"
        "You've completed the hafod tutorial!\n\nType (show-keybindings) at any time for a quick reference.\n\nHappy hacking!"
        ""
        "")
       ))

  ;; How many lessons there are, published so a caller need not re-derive it
  ;; from the vector or hard-code today's count.
  (define tutorial-lesson-count (vector-length tutorial-lessons))

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
  (define (tutorial-next-index i n cmd)
    (case cmd
      [(prev) (max 0 (- i 1))]
      [(quit) #f]
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

  ;; The tutorial's render-and-navigate loop, with the key read injected as
  ;; READ-COMMAND -- a thunk answering one of the three navigation commands.
  ;; Holding the terminal at arm's length this way lets the whole loop be driven
  ;; over string ports, and lets run-tutorial choose between a keypress and a
  ;; line without duplicating a line of the rendering below.
  (define (run-tutorial/reader read-command)
    (let ([n tutorial-lesson-count])
      (let lp ([i 0])
        (when (< i n)
          (let* ([lesson (vector-ref tutorial-lessons i)]
                 [title (car lesson)]
                 [instr (cadr lesson)]
                 [example (caddr lesson)]
                 [hint (cadddr lesson)])
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
            (when (> (string-length example) 0)
              (display (dim "  Example: "))
              (display (green example))
              (newline))
            (when (> (string-length hint) 0)
              (display (dim (string-append "  Hint: " hint)))
              (newline))
            (newline)
            (display (dim "  [Enter] next   [q] quit   [p] previous"))
            (newline) (newline)
            (let ([next-i (tutorial-next-index i n (read-command))])
              (if next-i
                  (lp next-i)
                  (begin
                    (when (ansi-ok? (current-output-port))
                      (display "\x1b;[2J\x1b;[H"))
                    (display "  Tutorial ended.\n\n")))))))))

  ;; The tutorial proper, gating on whether standard input is a terminal.
  ;;
  ;; On a terminal the legend is taken literally -- one keystroke, one command,
  ;; no Enter needed. The raw-mode extent is exactly one key read wide rather
  ;; than wrapping the loop, so every screen above is still painted in cooked
  ;; mode, where the output flags translate the newlines the display code emits;
  ;; not a byte of that rendering had to change, and echo is never live while a
  ;; screen is being drawn. Two attribute changes per lesson at reading pace cost
  ;; nothing. with-raw-mode* is the single owner of that transition: it winds the
  ;; cooked baseline back on a normal return, on a raised condition and across a
  ;; suspend, and it leaves the signal-generating characters enabled, so Ctrl-C
  ;; keeps its usual disposition and unwinds through that same restore.
  ;;
  ;; Off a terminal -- piped, redirected, or a dumb TERM -- there are no
  ;; attributes to set and with-raw-mode* would raise, so a scripted run reads
  ;; whole lines instead: one line, one lesson, a blank line meaning next.
  ;; Degrading beats erroring.
  ;;
  ;; The gate asks about descriptor 0 by number rather than about a port object,
  ;; because the question is whether this process's standard input is a terminal
  ;; and the shared console port aliases elsewhere.
  (define (run-tutorial)
    (run-tutorial/reader
      (if (ansi-ok? 0)
          (lambda ()
            ;; Push the prompt out before blocking: the read below waits on a
            ;; person, so anything still buffered would leave them staring at a
            ;; half-drawn lesson.
            (flush-output-port (current-output-port))
            (key->tutorial-command
              (with-raw-mode* 0
                (lambda () (read-key-event (console-input-port))))))
          (lambda ()
            (flush-output-port (current-output-port))
            (line->tutorial-command (get-line (console-input-port)))))))

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
