(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor editor)
        (hafod editor gap-buffer)
        (hafod editor input-decode)
        (hafod editor keymap)
        (hafod editor kill-ring))

;; Helper: construct control character
(define (ctrl ch)
  (integer->char (- (char->integer ch) 96)))

;; Helper: construct meta sequence (ESC + char)
(define (meta ch)
  (string (integer->char #x1b) ch))

;; C-j (LF, char 10) submits in insert mode; #\return (CR, char 13) inserts newline.
(define submit (integer->char 10))

;; Helper: simulate editor input and return result.
;; Takes a prompt string and an input string (may contain control chars and escape sequences).
;; Returns what read-expression returns (string or eof-object).
(define (editor-simulate prompt input)
  (let ([in-port (open-input-string input)]
        [out-port (open-output-string)])
    (read-expression prompt in-port out-port)))

(test-begin "editor")

;; Test 1: Type "hello" + C-j -> "hello"
(test-equal "simple input"
  "hello"
  (editor-simulate "> " (string-append "hello" (string submit))))

;; Test 2: Type "ab" + C-b + "X" + C-j -> "aXb"
(test-equal "insert in middle"
  "aXb"
  (editor-simulate "> " (string #\a #\b (ctrl #\b) #\X submit)))

;; Test 3: Type "abc" + C-a + C-d + C-j -> "bc"
(test-equal "move to start, delete forward"
  "bc"
  (editor-simulate "> " (string #\a #\b #\c (ctrl #\a) (ctrl #\d) submit)))

;; Test 4: Type "abc" + C-a + C-k + C-y + C-j -> "abc"
(test-equal "kill line then yank"
  "abc"
  (editor-simulate "> " (string #\a #\b #\c (ctrl #\a) (ctrl #\k) (ctrl #\y) submit)))

;; Test 5: C-d on empty buffer -> eof-object
(test-assert "C-d on empty buffer returns eof"
  (eof-object? (editor-simulate "> " (string (ctrl #\d)))))

;; Test 6: Type "(+ 1 2" + C-e + C-j -> "(+ 1 2)"
;; Auto-pairing inserts matching ), so result is balanced.
(test-equal "end of line before accept"
  "(+ 1 2)"
  (editor-simulate "> " (string-append "(+ 1 2" (string (ctrl #\e) submit))))

;; Test 7: Type "abcde" + C-b + C-b + C-t + C-j -> "abdce"
(test-equal "transpose chars"
  "abdce"
  (editor-simulate "> " (string #\a #\b #\c #\d #\e (ctrl #\b) (ctrl #\b) (ctrl #\t) submit)))

;; Test 8: Type "foo bar" + M-b + M-d + C-j -> "foo "
(test-equal "backward word then kill word"
  "foo "
  (editor-simulate "> " (string-append "foo bar"
                                        (meta #\b)
                                        (meta #\d)
                                        (string submit))))

;; Test 9: Type "test" + C-k + C-a + C-k + "new" + C-y + C-j
(test-equal "kill at end then kill line and yank"
  "newtest"
  (editor-simulate "> " (string #\t #\e #\s #\t
                                 (ctrl #\k)        ; kill to end (empty) at position 4
                                 (ctrl #\a)        ; beginning of line
                                 (ctrl #\k)        ; kill "test"
                                 #\n #\e #\w       ; type "new" (#\n = char 'n')
                                 (ctrl #\y)        ; yank "test"
                                 submit)))

;; Test 10: Backspace works
(test-equal "backspace deletes backward"
  "ac"
  (editor-simulate "> " (string #\a #\b #\c
                                 (ctrl #\b)          ; back over c
                                 (integer->char #x7f) ; backspace (DEL) deletes b
                                 submit)))

;; Test 11: C-e (end-of-line) from middle
(test-equal "end of line from middle then type"
  "abcX"
  (editor-simulate "> " (string #\a #\b #\c
                                 (ctrl #\a)  ; beginning
                                 (ctrl #\e)  ; end
                                 #\X
                                 submit)))

;; Test 12: Forward word (M-f)
(test-equal "forward word"
  "hell world"
  (editor-simulate "> " (string-append "hello world"
                                        (string (ctrl #\a))  ; beginning
                                        (meta #\f)           ; forward word -> after "hello"
                                        (string (ctrl #\b))  ; back one char -> before 'o'
                                        (string (ctrl #\d))  ; delete 'o'
                                        (string submit))))

;; Test 13: Return inserts newline in insert mode (multi-line editing)
(test-equal "return inserts newline"
  "(define x\n  42)"
  (editor-simulate "> " (string-append "(define x"
                                        (string #\return)  ; newline + auto-indent
                                        "42"
                                        (string submit))))

;; Test 14: Function call aligns continuation with first arg
(test-equal "indent aligns with first arg"
  "(list 1\n      2)"
  (editor-simulate "> " (string-append "(list 1"
                                        (string #\return)
                                        "2"
                                        (string submit))))

;; Test 15: Nested let bindings align at paren + 1
;; Note: auto-pairing adds closing ) for the outer (let
(test-equal "indent nested list at paren+1"
  "(let ([x 1]\n      [y 2]))"
  (editor-simulate "> " (string-append "(let ([x 1]"
                                        (string #\return)
                                        "[y 2]"
                                        (string submit))))

(test-end)
