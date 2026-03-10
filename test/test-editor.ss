(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor editor)
        (hafod editor gap-buffer)
        (hafod editor input-decode)
        (hafod editor keymap)
        (hafod editor kill-ring)
        (hafod editor history)
        (hafod editor sexp-tracker))

;; Helper: construct control character
(define (ctrl ch)
  (integer->char (- (char->integer ch) 96)))

;; Helper: construct meta sequence (ESC + char)
(define (meta ch)
  (string (integer->char #x1b) ch))

;; C-j (LF, char 10) submits in insert mode.
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

;; ======================================================================
;; Smart Return tests
;; ======================================================================

;; Test 13: Smart Return submits balanced expression
;; "(+ 1 2" auto-pairs to "(+ 1 2)" with cursor before ")".
;; Whole buffer "(+ 1 2)" is depth=0, state='normal, non-empty -> submit.
(test-equal "smart return submits balanced expression"
  "(+ 1 2)"
  (editor-simulate "> " (string-append "(+ 1 2" (string #\return))))

;; Test 14: Smart Return on empty buffer is no-op
;; Return does nothing, then type "ok" + C-j to actually submit.
(test-equal "smart return on empty is no-op"
  "ok"
  (editor-simulate "> " (string-append (string #\return) "ok" (string submit))))

;; Test 15: Smart Return on depth < 0 (unmatched closer) inserts newline
;; Type ")" (on empty buffer, not skip-closed) -> depth = -1 -> Return inserts newline.
;; Then submit with C-j.
(test-equal "smart return on unmatched closer inserts newline"
  ")\n"
  (editor-simulate "> " (string-append ")" (string #\return) (string submit))))

;; ======================================================================
;; Bracketed paste tests
;; ======================================================================

;; Test 16: Bracketed paste inserts text literally
(test-equal "bracketed paste inserts literally"
  "hello world"
  (editor-simulate "> " (string-append
                          "\x1b;[200~"     ; paste-start
                          "hello world"
                          "\x1b;[201~"     ; paste-end
                          (string submit))))

;; Test 17: Bracketed paste does not trigger auto-pairing for parens
(test-equal "bracketed paste no auto-pair"
  "(hello"
  (editor-simulate "> " (string-append
                          "\x1b;[200~"
                          "(hello"
                          "\x1b;[201~"
                          (string submit))))

;; Test 18: C-j still works as explicit submit
(test-equal "C-j explicit submit still works"
  "hello"
  (editor-simulate "> " (string-append "hello" (string submit))))

;; ======================================================================
;; History search tests (unit tests for search functions)
;; ======================================================================

;; Create a test history with known entries using an in-memory DB
(let ([h (open-history ":memory:")])
  (history-add! h "define x 1")
  (history-add! h "define y 2")
  (history-add! h "lambda args body")
  (history-add! h "define z 3")

  ;; Test 19: history-search-backward finds substring match
  ;; Entries: #("define x 1" "define y 2" "lambda args body" "define z 3")
  ;; Search for "lambda" from end (index 3) -> should find index 2
  (test-equal "history-search-backward finds substring"
    2
    (history-search-backward h "lambda" 3))

  ;; Test 20: history-search-backward finds "define" starting from index 2
  ;; Should find index 1 ("define y 2")
  (test-equal "history-search-backward finds older match"
    1
    (history-search-backward h "define" 2))

  ;; Test 21: history-search-backward returns #f when no match
  (test-equal "history-search-backward no match returns #f"
    #f
    (history-search-backward h "nonexistent" 3))

  ;; Test 22: history-prefix-search-backward finds prefix match
  ;; Search for "def" from end -> should find index 3 ("define z 3")
  (test-equal "history-prefix-search-backward finds prefix"
    3
    (history-prefix-search-backward h "def" 3))

  ;; Test 23: history-prefix-search-backward skips non-prefix matches
  ;; Search for "lambda" from end -> should find index 2
  (test-equal "history-prefix-search-backward finds lambda"
    2
    (history-prefix-search-backward h "lambda" 3))

  (history-close! h))

;; ======================================================================
;; Tab completion helper tests
;; ======================================================================

;; Test 24: word-at-cursor extracts partial symbol before cursor
(let ([gb (make-gap-buffer)])
  (gap-buffer-set-from-string! gb "defin")
  ;; cursor is at end (position 5)
  (let-values ([(prefix start) (word-at-cursor gb)])
    (test-equal "word-at-cursor extracts partial symbol"
      "defin" prefix)
    (test-equal "word-at-cursor start position"
      0 start)))

;; Test 25: word-at-cursor with preceding content
(let ([gb (make-gap-buffer)])
  (gap-buffer-set-from-string! gb "(string-app")
  ;; cursor at end, word starts after (
  (let-values ([(prefix start) (word-at-cursor gb)])
    (test-equal "word-at-cursor after paren"
      "string-app" prefix)
    (test-equal "word-at-cursor start after paren"
      1 start)))

;; Test 26: symbol-completions with prefix "string-le" includes "string-length"
(let ([results (symbol-completions "string-le")])
  (test-assert "symbol-completions finds string-length"
    (member "string-length" results)))

;; Test 27: symbol-completions with empty prefix returns many symbols
(let ([results (symbol-completions "")])
  (test-assert "symbol-completions with empty prefix returns results"
    (> (length results) 0)))

;; Test 28: longest-common-prefix finds shared prefix
(test-equal "longest-common-prefix basic"
  "string-"
  (longest-common-prefix '("string-append" "string-length" "string-ref")))

;; Test 29: longest-common-prefix single element
(test-equal "longest-common-prefix single"
  "hello"
  (longest-common-prefix '("hello")))

;; Test 30: longest-common-prefix empty list
(test-equal "longest-common-prefix empty list"
  ""
  (longest-common-prefix '()))

;; Test 31: filename-completions on /tmp creates results
;; (We just verify it returns a list without error)
(test-assert "filename-completions returns list"
  (list? (filename-completions "/tmp/")))

;; Test 32: filename-completions on non-existent dir returns empty
(test-equal "filename-completions non-existent dir"
  '()
  (filename-completions "/nonexistent-dir-12345/"))

;; Test 33: path-at-cursor extracts path from string context
(let ([gb (make-gap-buffer)])
  (gap-buffer-set-from-string! gb "(load \"src/ha")
  ;; cursor at end; we're inside a string
  (let ([path (path-at-cursor gb)])
    (test-equal "path-at-cursor extracts path fragment"
      "src/ha" path)))

(test-end)
