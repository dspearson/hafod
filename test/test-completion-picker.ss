;;; test/test-completion-picker.ss -- The opt-in full-screen completion picker.
;;; A new toggle completion-picker? (default off) diverts a multi-candidate Tab
;;; through the injected fuzzy finder instead of the inline grid.  The selection
;;; splices with the SAME word-span arithmetic as the inline path, so a completed
;;; name is the same bytes whichever UI chose it.  This suite drives the
;;; completion sink (apply-completions!) directly with a synthetic two-candidate
;;; list and a stub finder, so it is entirely PTY-free -- no terminal, no real
;;; filesystem completion, deterministic on every platform.  The layers:
;;;   (a) toggle ON, a real selection: the chosen name is spliced byte-for-byte at
;;;       the word span, the cursor lands past it, and the finder was consulted
;;;       exactly once with the BARE candidate names (never group-annotated rows);
;;;   (b) toggle ON, a cancel: the buffer is untouched and no inline menu lingers;
;;;   (c) toggle OFF (the shipped default): the finder is NEVER consulted -- a stub
;;;       finder records zero calls -- and the inline grid path runs, inserting the
;;;       longest common prefix and populating the menu.  The zero-call assertion is
;;;       the out-of-the-box proof that the default experience is unchanged.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod editor gap-buffer)
              make-gap-buffer gap-buffer-insert-string!
              gap-buffer->string gap-buffer-cursor-pos)
        (only (hafod editor editor)
              apply-completions! completion-active?
              completion-picker? editor-finder-proc)
        (chezscheme))

(test-begin "completion-picker")

;; ======================================================================
;; Harness
;; ======================================================================

;; A gap buffer holding TEXT with the cursor at the end, ready to complete the
;; trailing word.  apply-completions! reads the buffer string and cursor itself.
(define (buffer-with text)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert-string! gb text)
    gb))

;; A call recorder: #(call-count last-items-seen).
(define (make-recorder) (vector 0 'unset))
(define (recorder-count r) (vector-ref r 0))
(define (recorder-items r) (vector-ref r 1))

;; A stub finder over recorder R that returns REPLY.  It mirrors the
;; editor-finder-proc call convention -- (items prompt ...) -> a chosen string or
;; a non-string cancel -- while counting invocations and remembering the exact
;; items it was handed, so a test can prove the finder saw the BARE names and was
;; consulted the expected number of times.
(define (stub-finder r reply)
  (lambda (items . _)
    (vector-set! r 0 (+ 1 (vector-ref r 0)))
    (vector-set! r 1 items)
    reply))

;; The two candidates every row completes: both plain (name . positions) pairs
;; sharing the prefix "fooba", so completion-normalise keeps them in one
;; unlabelled group with bare names in a stable order, and the inline path's
;; longest common prefix is exactly "fooba".
(define candidates (list (cons "foobar" '()) (cons "foobaz" '())))

;; The buffer completes the trailing word of "cat foo": the word span is [4,7),
;; so a splice replaces "foo" and keeps the "cat " head untouched.
(define start-text "cat foo")
(define word-start 4)
(define word-end (string-length start-text))

;; ======================================================================
;; (a) Toggle ON, a real selection -- byte-exact splice through the finder.
;; The finder returns "foobaz"; the bridge splices it over the word span exactly
;; as the inline cycle/LCP insertions would, so the buffer reads "cat foobaz" and
;; the cursor sits just past the inserted name.  The finder is consulted once and
;; is handed the two BARE names -- an annotated label would show up here and would
;; also have been spliced verbatim, so the equality is non-vacuous.
;; ======================================================================
(let ([gb (buffer-with start-text)]
      [rec (make-recorder)])
  (parameterize ([completion-picker? #t]
                 [editor-finder-proc (stub-finder rec "foobaz")])
    (apply-completions! gb start-text word-end word-start candidates))
  (test-equal "picker: the chosen candidate is spliced byte-for-byte over the word span"
    "cat foobaz" (gap-buffer->string gb))
  (test-equal "picker: the cursor lands just past the inserted name"
    (+ word-start (string-length "foobaz")) (gap-buffer-cursor-pos gb))
  (test-equal "picker: the finder is consulted exactly once"
    1 (recorder-count rec))
  (test-equal "picker: the finder is fed the bare candidate names, not annotated rows"
    '("foobar" "foobaz") (recorder-items rec))
  (test-assert "picker: a picker selection sets no inline menu state"
    (not (completion-active?))))

;; ======================================================================
;; (b) Toggle ON, a cancel -- the buffer is left exactly as it was.
;; The finder returns #f (its cancel sentinel).  The bridge inserts nothing, so
;; the buffer still reads "cat foo", and it dismisses cleanly -- no inline menu is
;; left populated.  The finder was still consulted once (the picker did engage).
;; ======================================================================
(let ([gb (buffer-with start-text)]
      [rec (make-recorder)])
  (parameterize ([completion-picker? #t]
                 [editor-finder-proc (stub-finder rec #f)])
    (apply-completions! gb start-text word-end word-start candidates))
  (test-equal "picker: a cancelled pick leaves the buffer untouched"
    start-text (gap-buffer->string gb))
  (test-equal "picker: a cancelled pick still consulted the finder once"
    1 (recorder-count rec))
  (test-assert "picker: a cancelled pick leaves no inline menu state"
    (not (completion-active?))))

;; ======================================================================
;; (c) Toggle OFF (the shipped default) -- the finder is never consulted.
;; completion-picker? is pinned #f while a finder IS installed, so a zero call
;; count proves the toggle -- not a missing finder -- kept the picker shut.  The
;; inline grid path runs instead: it inserts the longest common prefix ("fooba",
;; giving "cat fooba") and populates the menu state.  This is the out-of-the-box
;; experience, unchanged.
;; ======================================================================
(let ([gb (buffer-with start-text)]
      [rec (make-recorder)])
  (parameterize ([completion-picker? #f]
                 [editor-finder-proc (stub-finder rec "foobaz")])
    (apply-completions! gb start-text word-end word-start candidates))
  (test-equal "picker off: the finder is never consulted (the default is the inline grid)"
    0 (recorder-count rec))
  (test-equal "picker off: the inline grid inserts the longest common prefix"
    "cat fooba" (gap-buffer->string gb))
  (test-assert "picker off: the inline grid populates the menu state"
    (completion-active?)))

(test-end)
