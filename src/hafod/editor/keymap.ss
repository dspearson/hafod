;;; (hafod editor keymap) -- Trie-based keymap for key sequence to command dispatch
;;; Maps key-event sequences to editor command procedures.
;;; Supports single-key bindings and multi-key chord sequences (e.g., C-x C-e).
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor keymap)
  (export make-keymap keymap? keymap-bind! keymap-lookup keymap-lookup-prefix)
  (import (chezscheme)
          (hafod editor input-decode))

  ;; Keymap: mutable alist of (key-event . command-or-keymap)
  (define-record-type keymap
    (fields (mutable bindings))
    (protocol (lambda (new)
                (lambda ()
                  (new '())))))

  ;; Find entry in bindings alist matching key-event
  (define (find-binding bindings key)
    (cond
      [(null? bindings) #f]
      [(key-event=? (caar bindings) key) (car bindings)]
      [else (find-binding (cdr bindings) key)]))

  ;; Remove entry matching key-event from bindings alist
  (define (remove-binding bindings key)
    (cond
      [(null? bindings) '()]
      [(key-event=? (caar bindings) key) (cdr bindings)]
      [else (cons (car bindings) (remove-binding (cdr bindings) key))]))

  ;; Bind a sequence of key-events to a command in the keymap
  ;; sequence: list of key-events
  ;; command: procedure to invoke
  (define (keymap-bind! km sequence command)
    (cond
      [(null? sequence)
       (error 'keymap-bind! "empty key sequence")]
      [(null? (cdr sequence))
       ;; Single key -- bind directly
       (let* ([key (car sequence)]
              [cleaned (remove-binding (keymap-bindings km) key)])
         (keymap-bindings-set! km (cons (cons key command) cleaned)))]
      [else
       ;; Multi-key -- first key maps to sub-keymap, recurse
       (let* ([key (car sequence)]
              [entry (find-binding (keymap-bindings km) key)]
              [sub (if (and entry (keymap? (cdr entry)))
                       (cdr entry)
                       (let ([new-km (make-keymap)])
                         (let ([cleaned (remove-binding (keymap-bindings km) key)])
                           (keymap-bindings-set! km (cons (cons key new-km) cleaned)))
                         new-km))])
         (keymap-bind! sub (cdr sequence) command))]))

  ;; Lookup a single key-event in the keymap
  ;; Returns: command procedure (leaf), keymap (prefix), or #f (not found)
  (define (keymap-lookup km key)
    (let ([entry (find-binding (keymap-bindings km) key)])
      (if entry (cdr entry) #f)))

  ;; Lookup prefix: returns sub-keymap if key is a prefix, #f otherwise
  (define (keymap-lookup-prefix km key)
    (let ([result (keymap-lookup km key)])
      (if (keymap? result) result #f)))

  ) ; end library
