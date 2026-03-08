(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor input-decode))

(test-begin "Input Decoder")

;;; Key event construction and equality

(test-assert "make-key-event creates key-event"
  (key-event? (make-key-event 'char #\a 0)))

(test-equal "key-event-type"
  'char (key-event-type (make-key-event 'char #\a 0)))

(test-equal "key-event-value"
  #\a (key-event-value (make-key-event 'char #\a 0)))

(test-equal "key-event-mods"
  0 (key-event-mods (make-key-event 'char #\a 0)))

(test-assert "key-event=? same events"
  (key-event=? (make-key-event 'char #\a 0) (make-key-event 'char #\a 0)))

(test-assert "key-event=? different events"
  (not (key-event=? (make-key-event 'char #\a 0) (make-key-event 'char #\b 0))))

;;; Plain character input

(test-assert "plain char a"
  (let ([ev (read-key-event (open-input-string "a"))])
    (and (eq? (key-event-type ev) 'char)
         (eqv? (key-event-value ev) #\a)
         (= (key-event-mods ev) 0))))

(test-assert "plain char z"
  (let ([ev (read-key-event (open-input-string "z"))])
    (and (eq? (key-event-type ev) 'char)
         (eqv? (key-event-value ev) #\z))))

;;; Control characters

(test-assert "C-a (char 1)"
  (let ([ev (read-key-event (open-input-string (string (integer->char 1))))])
    (and (eq? (key-event-type ev) 'ctrl)
         (eqv? (key-event-value ev) #\a)
         (= (key-event-mods ev) 0))))

(test-assert "C-d (char 4)"
  (let ([ev (read-key-event (open-input-string (string (integer->char 4))))])
    (and (eq? (key-event-type ev) 'ctrl)
         (eqv? (key-event-value ev) #\d))))

;;; Special control chars: tab, return, newline

(test-assert "tab (char 9)"
  (let ([ev (read-key-event (open-input-string (string (integer->char 9))))])
    (and (eq? (key-event-type ev) 'special)
         (eq? (key-event-value ev) 'tab))))

(test-assert "return/CR (char 13)"
  (let ([ev (read-key-event (open-input-string (string (integer->char 13))))])
    (and (eq? (key-event-type ev) 'special)
         (eq? (key-event-value ev) 'return))))

(test-assert "newline (char 10)"
  (let ([ev (read-key-event (open-input-string (string (integer->char 10))))])
    (and (eq? (key-event-type ev) 'special)
         (eq? (key-event-value ev) 'newline))))

;;; Backspace

(test-assert "backspace (char 127)"
  (let ([ev (read-key-event (open-input-string (string #\x7f)))])
    (and (eq? (key-event-type ev) 'special)
         (eq? (key-event-value ev) 'backspace))))

;;; Meta (ESC + char)

(test-assert "M-f (ESC f)"
  (let ([ev (read-key-event (open-input-string (string #\x1b #\f)))])
    (and (eq? (key-event-type ev) 'meta)
         (eqv? (key-event-value ev) #\f))))

;;; Arrow keys (CSI sequences)

(test-assert "up arrow (ESC [ A)"
  (let ([ev (read-key-event (open-input-string (string #\x1b #\[ #\A)))])
    (and (eq? (key-event-type ev) 'special)
         (eq? (key-event-value ev) 'up))))

(test-assert "down arrow (ESC [ B)"
  (let ([ev (read-key-event (open-input-string (string #\x1b #\[ #\B)))])
    (and (eq? (key-event-type ev) 'special)
         (eq? (key-event-value ev) 'down))))

(test-assert "right arrow (ESC [ C)"
  (let ([ev (read-key-event (open-input-string (string #\x1b #\[ #\C)))])
    (and (eq? (key-event-type ev) 'special)
         (eq? (key-event-value ev) 'right))))

(test-assert "left arrow (ESC [ D)"
  (let ([ev (read-key-event (open-input-string (string #\x1b #\[ #\D)))])
    (and (eq? (key-event-type ev) 'special)
         (eq? (key-event-value ev) 'left))))

;;; Home/End

(test-assert "home (ESC [ H)"
  (let ([ev (read-key-event (open-input-string (string #\x1b #\[ #\H)))])
    (and (eq? (key-event-type ev) 'special)
         (eq? (key-event-value ev) 'home))))

(test-assert "end (ESC [ F)"
  (let ([ev (read-key-event (open-input-string (string #\x1b #\[ #\F)))])
    (and (eq? (key-event-type ev) 'special)
         (eq? (key-event-value ev) 'end))))

;;; Delete (tilde sequence)

(test-assert "delete (ESC [ 3 ~)"
  (let ([ev (read-key-event (open-input-string (string #\x1b #\[ #\3 #\~)))])
    (and (eq? (key-event-type ev) 'special)
         (eq? (key-event-value ev) 'delete))))

;;; Display width

(test-equal "char-display-width ASCII"
  1 (char-display-width #\a))

(test-equal "char-display-width CJK U+4E2D"
  2 (char-display-width #\x4E2D))

(test-equal "string-display-width ASCII"
  5 (string-display-width "hello"))

(test-equal "string-display-width mixed ASCII+CJK"
  4 (string-display-width (string #\a #\x4E2D #\b)))

(test-end)
