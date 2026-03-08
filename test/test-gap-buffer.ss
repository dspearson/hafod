(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod editor gap-buffer))

(test-begin "gap-buffer")

;; Basic insertion
(test-equal "insert single char"
  "a"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer->string gb)))

(test-equal "insert multiple chars"
  "abc"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer->string gb)))

;; Cursor position
(test-equal "cursor position after inserts"
  3
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer-cursor-pos gb)))

;; Insert at beginning
(test-equal "insert at beginning"
  "xabc"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer-move-cursor! gb -3)
    (gap-buffer-insert! gb #\x)
    (gap-buffer->string gb)))

;; Insert in middle
(test-equal "insert in middle"
  "abxc"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer-move-cursor! gb -1)
    (gap-buffer-insert! gb #\x)
    (gap-buffer->string gb)))

;; Delete forward (C-d style)
(test-equal "delete forward"
  "ac"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer-move-cursor! gb -2)
    (gap-buffer-delete-forward! gb)
    (gap-buffer->string gb)))

;; Delete backward (backspace)
(test-equal "delete backward"
  "ac"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer-move-cursor! gb -1)
    (gap-buffer-delete-backward! gb)
    (gap-buffer->string gb)))

;; Move cursor left/right
(test-equal "move cursor left"
  1
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer-move-cursor! gb -2)
    (gap-buffer-cursor-pos gb)))

(test-equal "move cursor right"
  3
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer-move-cursor! gb -2)
    (gap-buffer-move-cursor! gb 2)
    (gap-buffer-cursor-pos gb)))

;; Move to beginning/end (large negative/positive)
(test-equal "move to beginning"
  0
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-move-cursor! gb -100)
    (gap-buffer-cursor-pos gb)))

(test-equal "move to end"
  2
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-move-cursor! gb -100)
    (gap-buffer-move-cursor! gb 100)
    (gap-buffer-cursor-pos gb)))

;; Content unchanged after move
(test-equal "content unchanged after cursor move"
  "abc"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer-move-cursor! gb -2)
    (gap-buffer-move-cursor! gb 1)
    (gap-buffer->string gb)))

;; Resize: insert beyond initial capacity
(test-assert "resize on overflow"
  (let ([gb (make-gap-buffer 8)])
    (let loop ([i 0])
      (when (< i 100)
        (gap-buffer-insert! gb #\x)
        (loop (+ i 1))))
    (and (= (gap-buffer-length gb) 100)
         (= (string-length (gap-buffer->string gb)) 100))))

;; before-string / after-string
(test-equal "before-string"
  "ab"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer-move-cursor! gb -1)
    (gap-buffer-before-string gb)))

(test-equal "after-string"
  "c"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer-move-cursor! gb -1)
    (gap-buffer-after-string gb)))

;; clear!
(test-equal "clear resets to empty"
  ""
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-clear! gb)
    (gap-buffer->string gb)))

(test-equal "cursor at 0 after clear"
  0
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-clear! gb)
    (gap-buffer-cursor-pos gb)))

;; set-from-string!
(test-equal "set-from-string!"
  "hello"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb "hello")
    (gap-buffer->string gb)))

(test-equal "cursor at end after set-from-string!"
  5
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb "hello")
    (gap-buffer-cursor-pos gb)))

;; char-at
(test-equal "char-at index 0"
  #\a
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb "abc")
    (gap-buffer-char-at gb 0)))

(test-equal "char-at index 2"
  #\c
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb "abc")
    (gap-buffer-char-at gb 2)))

;; Boundary no-ops
(test-equal "delete-backward at pos 0 is no-op"
  "abc"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb "abc")
    (gap-buffer-move-cursor! gb -100)
    (gap-buffer-delete-backward! gb)
    (gap-buffer->string gb)))

(test-equal "delete-forward at end is no-op"
  "abc"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb "abc")
    (gap-buffer-delete-forward! gb)
    (gap-buffer->string gb)))

;; Word operations
(test-equal "delete-word-forward"
  "hello "
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb "hello world")
    (gap-buffer-move-cursor! gb -5)  ; cursor after "hello "
    (gap-buffer-delete-word-forward! gb)
    (gap-buffer->string gb)))

(test-equal "delete-word-backward"
  "world"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb "hello world")
    (gap-buffer-move-cursor! gb -5)  ; cursor after "hello "
    (gap-buffer-delete-word-backward! gb)
    (gap-buffer->string gb)))

;; kill-to-end
(test-equal "kill-to-end returns killed text"
  "world"
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb "hello world")
    (gap-buffer-move-cursor! gb -5)  ; cursor after "hello "
    (gap-buffer-kill-to-end! gb)))

(test-equal "kill-to-end modifies buffer"
  "hello "
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb "hello world")
    (gap-buffer-move-cursor! gb -5)  ; cursor after "hello "
    (gap-buffer-kill-to-end! gb)
    (gap-buffer->string gb)))

;; gap-buffer-length
(test-equal "length after inserts"
  3
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert! gb #\a)
    (gap-buffer-insert! gb #\b)
    (gap-buffer-insert! gb #\c)
    (gap-buffer-length gb)))

(test-end)
