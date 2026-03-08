(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod editor kill-ring))

(test-begin "kill-ring")

;; Basic push and yank
(test-equal "push and yank"
  "hello"
  (let ([kr (make-kill-ring)])
    (kill-ring-push! kr "hello")
    (kill-ring-yank kr)))

;; Yank returns most recent
(test-equal "yank returns most recent"
  "world"
  (let ([kr (make-kill-ring)])
    (kill-ring-push! kr "hello")
    (kill-ring-push! kr "world")
    (kill-ring-yank kr)))

;; Rotate cycles through ring
(test-equal "rotate retrieves previous"
  "hello"
  (let ([kr (make-kill-ring)])
    (kill-ring-push! kr "hello")
    (kill-ring-push! kr "world")
    (kill-ring-rotate! kr)
    (kill-ring-yank kr)))

;; Rotate wraps around
(test-equal "rotate wraps around"
  "world"
  (let ([kr (make-kill-ring)])
    (kill-ring-push! kr "hello")
    (kill-ring-push! kr "world")
    (kill-ring-rotate! kr)  ; -> hello
    (kill-ring-rotate! kr)  ; -> world (wrap)
    (kill-ring-yank kr)))

;; Empty ring returns #f
(test-equal "empty ring yank returns #f"
  #f
  (let ([kr (make-kill-ring)])
    (kill-ring-yank kr)))

;; Overflow drops oldest
(test-equal "overflow drops oldest - yank most recent"
  "new"
  (let ([kr (make-kill-ring 3)])
    (kill-ring-push! kr "a")
    (kill-ring-push! kr "b")
    (kill-ring-push! kr "c")
    (kill-ring-push! kr "new")  ; drops "a"
    (kill-ring-yank kr)))

(test-equal "overflow drops oldest - rotate to oldest"
  "b"
  (let ([kr (make-kill-ring 3)])
    (kill-ring-push! kr "a")
    (kill-ring-push! kr "b")
    (kill-ring-push! kr "c")
    (kill-ring-push! kr "new")  ; drops "a"
    ;; yank returns "new", rotate gives "c", rotate gives "b"
    (kill-ring-rotate! kr)
    (kill-ring-rotate! kr)
    (kill-ring-yank kr)))

(test-equal "overflow oldest gone"
  "new"
  (let ([kr (make-kill-ring 3)])
    (kill-ring-push! kr "a")
    (kill-ring-push! kr "b")
    (kill-ring-push! kr "c")
    (kill-ring-push! kr "new")
    ;; Rotate 3 times should wrap back to "new" (ring has b, c, new)
    (kill-ring-rotate! kr)   ; c
    (kill-ring-rotate! kr)   ; b
    (kill-ring-rotate! kr)   ; new (wrap)
    (kill-ring-yank kr)))

(test-end)
