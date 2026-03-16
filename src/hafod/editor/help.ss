;;; (hafod editor help) -- Keybinding cheatsheet and interactive tutorial
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor help)
  (export show-keybindings run-tutorial)
  (import (chezscheme))

  ;; ======================================================================
  ;; ANSI formatting helpers
  ;; ======================================================================

  (define (bold s) (string-append "\x1b;[1m" s "\x1b;[22m"))
  (define (dim s) (string-append "\x1b;[2m" s "\x1b;[22m"))
  (define (cyan s) (string-append "\x1b;[36m" s "\x1b;[39m"))
  (define (yellow s) (string-append "\x1b;[33m" s "\x1b;[39m"))
  (define (green s) (string-append "\x1b;[32m" s "\x1b;[39m"))
  (define (magenta s) (string-append "\x1b;[35m" s "\x1b;[39m"))

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

  (define (run-tutorial)
    (let ([n (vector-length tutorial-lessons)])
      (let lp ([i 0])
        (when (< i n)
          (let* ([lesson (vector-ref tutorial-lessons i)]
                 [title (car lesson)]
                 [instr (cadr lesson)]
                 [example (caddr lesson)]
                 [hint (cadddr lesson)])
            (display "\x1b;[2J\x1b;[H")  ;; clear screen
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
            ;; Wait for keypress (simple blocking read)
            (let ([ch (read-char)])
              (cond
                [(or (eof-object? ch) (char=? ch #\q))
                 (display "\x1b;[2J\x1b;[H")
                 (display "  Tutorial ended.\n\n")]
                [(char=? ch #\p)
                 (lp (max 0 (- i 1)))]
                [else
                 (lp (+ i 1))])))))))

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
