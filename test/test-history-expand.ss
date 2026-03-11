(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod shell history-expand))

(test-begin "History Expansion")

;; Test entries: vector of past commands (most recent last)
(define entries (vector "echo hello" "ls -la" "grep foo bar.txt" "cd /tmp"))

;; === !! — last command ===

(test-equal "!! expands to last command"
  "cd /tmp"
  (history-expand "!!" entries))

(test-equal "!! with prefix"
  "echo cd /tmp"
  (history-expand "echo !!" entries))

;; === !$ — last argument ===

(test-equal "!$ expands to last arg"
  "echo /tmp"
  (history-expand "echo !$" entries))

(test-equal "!$ single-word command"
  (let ([e (vector "pwd")])
    (history-expand "echo !$" e))
  "echo pwd")

;; === !n — nth entry (1-based) ===

(test-equal "!1 first entry"
  "echo hello"
  (history-expand "!1" entries))

(test-equal "!3 third entry"
  "grep foo bar.txt"
  (history-expand "!3" entries))

;; === !-n — nth from last ===

(test-equal "!-1 last entry"
  "cd /tmp"
  (history-expand "!-1" entries))

(test-equal "!-2 second to last"
  "grep foo bar.txt"
  (history-expand "!-2" entries))

;; === !prefix — most recent match ===

(test-equal "!gr matches grep"
  "grep foo bar.txt"
  (history-expand "!gr" entries))

(test-equal "!e matches echo"
  "echo hello"
  (history-expand "!e" entries))

(test-equal "!ls matches ls"
  "ls -la"
  (history-expand "!ls" entries))

;; === No expansion cases ===

(test-equal "! followed by space is literal"
  "echo ! hello"
  (history-expand "echo ! hello" entries))

(test-equal "! at end of string is literal"
  "echo!"
  (history-expand "echo!" entries))

(test-equal "! followed by = is literal"
  "x!=1"
  (history-expand "x!=1" entries))

;; === Escaped ! ===

(test-equal "\\! is literal !"
  "echo !"
  (history-expand "echo \\!" entries))

;; === No match — literal ===

(test-equal "!zzz no match keeps literal"
  "!zzz"
  (history-expand "!zzz" entries))

;; === Empty history ===

(test-equal "empty history no expansion"
  "!!"
  (history-expand "!!" (vector)))

;; === Multiple expansions in one line ===

(test-equal "multiple !! in one line"
  "cd /tmp && cd /tmp"
  (history-expand "!! && !!" entries))

(test-equal "mixed expansion types"
  "cd /tmp /tmp"
  (history-expand "!! !$" entries))

;; === Out of range — literal ===

(test-equal "!99 out of range literal"
  "!99"
  (history-expand "!99" entries))

(test-equal "!-99 out of range literal"
  "!-99"
  (history-expand "!-99" entries))

(test-end)
