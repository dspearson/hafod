;;; test/test-surround-keys.ss -- Coverage for the configurable surround trigger
;;; keys: surround-op-key (default #\s) and surround-visual-key (default #\S) are
;;; settable make-parameters whose values the vi dispatch reads live on every
;;; key. Because the triggers are plain char keys consumed inside vi-process-key
;;; before the keymap, configurability is proven behaviourally end-to-end -- by
;;; driving vi-process-key, not by a keymap lookup. The suite checks the defaults
;;; and the non-char coercion, that the default sequences act with no binding
;;; step, and that rebinding a trigger inside a parameterize makes the new key
;;; act while the old key falls through unchanged, with the defaults restored
;;; once the dynamic extent ends. Everything is driven through bounded string
;;; ports, so the suite needs no terminal and can never block.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor gap-buffer)
        (hafod editor kill-ring)
        (hafod editor input-decode)
        (hafod editor vi)
        ;; Importing the editor library installs vi.ss's procedure hooks
        ;; (vi-snapshot!-proc and friends) at load, so vi-process-key runs
        ;; without a null-hook error.
        (hafod editor editor))

;; Build a single-line gap-buffer holding TEXT with the cursor at INDEX.
(define (buffer-with-cursor text index)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert-string! gb text)
    ;; insert leaves the cursor at the end; step it back to the chosen index
    (gap-buffer-move-cursor! gb (- index (string-length text)))
    gb))

;; Construct a normal-mode editor-state around a buffer. The trailing #f is the
;; initial mark (no region active).
(define (state-for gb kr)
  (make-editor-state gb kr "> " (open-output-string) 0 #f #f 'normal #f))

;; Feed one character key event through the vi state machine with an empty,
;; bounded input port. Used for keys that read no follow-up (y, v, $).
(define (press! es gb kr ch)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string "") (open-output-string) gb kr))

;; Press CH with the whole trailing string REST queued on the bounded input
;; port: the successive read-key-event calls the dispatch makes consume REST in
;; order, so the operand and delimiter never reach the keymap.
(define (press-str! es gb kr ch rest)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string rest) (open-output-string) gb kr))

;; Drive a ys surround using TRIGGER as the operator key: on a fresh normal-mode
;; buffer press y, then TRIGGER with the operand+delimiter REST queued. The
;; caller wraps this in the parameterize that sets surround-op-key, so the
;; dispatch reads the live trigger on every key. Return the buffer after the
;; edit.
(define (ys-add text index trigger rest)
  (let* ([gb (buffer-with-cursor text index)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (vi-reset-session!)
    (press! es gb kr #\y)
    (press-str! es gb kr trigger rest)
    (gap-buffer->string gb)))

;; Drive a visual surround using TRIGGER as the visual key: enter characterwise
;; visual on TEXT (cursor at INDEX), extend with MOTION-CH, then press TRIGGER
;; with the delimiter REST queued. Return the buffer after the edit.
(define (visual-add text index motion-ch trigger rest)
  (let* ([gb (buffer-with-cursor text index)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (vi-reset-session!)
    (press! es gb kr #\v)
    (press! es gb kr motion-ch)
    (press-str! es gb kr trigger rest)
    (gap-buffer->string gb)))

(test-begin "surround-keys")

;; --- 1. Parameter defaults --------------------------------------------------
;; The trigger keys start on the vim-surround defaults, so surround is reachable
;; out of the box with no binding step.
(test-assert "surround-op-key defaults to #\\s"
  (char=? (surround-op-key) #\s))
(test-assert "surround-visual-key defaults to #\\S"
  (char=? (surround-visual-key) #\S))

;; --- 2. Non-char coercion ---------------------------------------------------
;; The parameter guard keeps a character; handed anything else it falls back to
;; the default, so a mistaken init value can never leave the trigger unusable.
(test-assert "surround-op-key coerces a non-char back to #\\s"
  (char=? (parameterize ([surround-op-key 42]) (surround-op-key)) #\s))
(test-assert "surround-visual-key coerces a non-char back to #\\S"
  (char=? (parameterize ([surround-visual-key 42]) (surround-visual-key)) #\S))

;; --- 3. Default-on operator surround ----------------------------------------
;; With the default #\s, y then s then "iw(" wraps the buffer word into "( word )"
;; -- the operator key acts with no re-binder call and no keymap edit.
(test-equal "default operator surround wraps the word out of the box (ysiw()"
  "( word )" (ys-add "word" 1 #\s "iw("))

;; --- 4. Default-on visual surround ------------------------------------------
;; With the default #\S, a characterwise selection then S then ")" wraps it into
;; "(word)" -- the visual key acts out of the box too.
(test-equal "default visual surround wraps the selection out of the box (S))"
  "(word)" (visual-add "word" 0 #\$ #\S ")"))

;; --- 5. Live operator-key rebind --------------------------------------------
;; Inside the parameterize the dispatch reads the NEW trigger, so y then z then
;; "iw(" wraps the word -- an init rebinds by setting the parameter, with no
;; re-binder call and no keymap edit.
(test-equal "rebinding surround-op-key to z makes the z trigger surround"
  "( word )"
  (parameterize ([surround-op-key #\z]) (ys-add "word" 1 #\z "iw(")))

;; --- 6. Operator-key falsification, same extent -----------------------------
;; With the operator key rebound to z, the old s no longer triggers surround: it
;; falls through and the buffer is left untouched. A hardcoded literal would
;; still fire here, so this proves the arm reads the parameter live.
(test-equal "with the op key rebound to z, the old s no longer surrounds"
  "word"
  (parameterize ([surround-op-key #\z]) (ys-add "word" 1 #\s "iw(")))

;; --- 7. Operator-key extent restore -----------------------------------------
;; Outside the parameterize the default holds again, so the plain s trigger
;; surrounds once more -- the override was dynamically scoped.
(test-assert "surround-op-key is #\\s again after the extent"
  (char=? (surround-op-key) #\s))
(test-equal "the default s trigger surrounds again after the extent"
  "( word )" (ys-add "word" 1 #\s "iw("))

;; --- 8. Live visual-key rebind, falsification and restore -------------------
;; The visual trigger is configurable the same way. Z is a free key -- and, like
;; the default S, a capital, so it reads naturally as a visual trigger; rebinding
;; to it makes the new key surround the selection...
(test-equal "rebinding surround-visual-key to Z makes the Z trigger surround"
  "(word)"
  (parameterize ([surround-visual-key #\Z]) (visual-add "word" 0 #\$ #\Z ")")))
;; ...while the old S no longer does -- it falls through and leaves the buffer.
(test-equal "with the visual key rebound to Z, the old S no longer surrounds"
  "word"
  (parameterize ([surround-visual-key #\Z]) (visual-add "word" 0 #\$ #\S ")")))
;; ...and the default is restored once the dynamic extent ends.
(test-assert "surround-visual-key is #\\S again after the extent"
  (char=? (surround-visual-key) #\S))

(test-end)
