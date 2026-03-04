;; test-exit-hooks.ss -- Tests for (hafod exit-hooks)
;; Run with: scheme --libdirs .:src --script test/test-exit-hooks.ss
;; Copyright (c) 2026 Dominic Pearson.

(import (test runner) (hafod exit-hooks) (chezscheme))

(test-begin "Exit Hooks")

;; call-exit-hooks! with no hooks should succeed (hooks from other tests may exist,
;; but at this point we're in a fresh process so the hooks list should be empty)
(test-assert "call-exit-hooks! with no hooks succeeds"
  (begin (call-exit-hooks!) #t))

;; add-exit-hook! registers a hook that gets called
(test-assert "add-exit-hook! registers a hook that gets called"
  (let ([box (vector #f)])
    (add-exit-hook! (lambda () (vector-set! box 0 #t)))
    (call-exit-hooks!)
    (vector-ref box 0)))

;; call-exit-hooks-and-run runs hooks then thunk
(test-assert "call-exit-hooks-and-run runs hooks then thunk"
  (let ([order '()])
    (add-exit-hook! (lambda () (set! order (cons 'hook order))))
    (call-exit-hooks-and-run
      (lambda () (set! order (cons 'thunk order))))
    ;; thunk should be last in list since it runs after hooks
    (eq? (car order) 'thunk)))

;; Multiple hooks all execute
(test-assert "multiple hooks all execute"
  (let ([count (vector 0)])
    (add-exit-hook! (lambda () (vector-set! count 0 (+ (vector-ref count 0) 1))))
    (add-exit-hook! (lambda () (vector-set! count 0 (+ (vector-ref count 0) 1))))
    (call-exit-hooks!)
    ;; At least the 2 we just added should have fired (plus earlier hooks)
    (>= (vector-ref count 0) 2)))

;; Hooks added later run first (LIFO: cons onto front, for-each front-to-back)
(test-assert "later hooks run before earlier hooks"
  (let ([order '()])
    (add-exit-hook! (lambda () (set! order (cons 'A order))))
    (add-exit-hook! (lambda () (set! order (cons 'B order))))
    (call-exit-hooks!)
    ;; B was cons'd last, so B is at head of hooks list.
    ;; for-each: B runs first -> order=(B), then A -> order=(A B), then earlier hooks.
    ;; In the final order list, scanning from front: earlier hooks, then A, then B.
    ;; So B should appear AFTER A (deeper in the list) since B ran first
    ;; and A's cons pushed it deeper.
    (let ([a-pos (let find ([lst order] [i 0])
                   (cond [(null? lst) #f]
                         [(eq? (car lst) 'A) i]
                         [else (find (cdr lst) (+ i 1))]))]
          [b-pos (let find ([lst order] [i 0])
                   (cond [(null? lst) #f]
                         [(eq? (car lst) 'B) i]
                         [else (find (cdr lst) (+ i 1))]))])
      ;; Both should be found and A should have a smaller index (closer to front)
      (and a-pos b-pos (< a-pos b-pos)))))

;; call-exit-hooks-and-run: thunk return value
(test-assert "call-exit-hooks-and-run returns thunk's value"
  (equal? 42
    (call-exit-hooks-and-run (lambda () 42))))

;; add-exit-hook! accepts any thunk (lambda of zero args)
(test-assert "add-exit-hook! accepts thunk"
  (begin
    (add-exit-hook! (lambda () (void)))
    #t))

(test-end)
