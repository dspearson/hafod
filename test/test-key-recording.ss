;;; test/test-key-recording.ss -- The keystroke-recording tee inside the decoder.
;;; Proves that current-key-recording captures the exact characters that
;;; read-key-event consumes -- the top-level read AND the inline follow-up reads
;;; of a multi-key escape sequence -- and, crucially, that decoding is
;;; byte-for-byte unchanged whether the tee is recording or off. Every port is a
;;; bounded string port, so the suite is PTY-free and deterministic.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor input-decode))

;; Decode the first key-event of a byte string with the tee off (its default).
(define (decode-off bytes)
  (read-key-event (open-input-string bytes)))

;; Decode the first key-event of a byte string with the tee on, returning a pair
;; of the decoded event and the exact characters the decoder copied into the
;; recording port.
(define (decode-on bytes)
  (let ([rec (open-output-string)])
    (let ([ev (parameterize ([current-key-recording rec])
                (read-key-event (open-input-string bytes)))])
      (cons ev (get-output-string rec)))))

(test-begin "key-recording")

;; ---------------------------------------------------------------------------
;; Recording on: the tee captures exactly the characters that were consumed.
;; ---------------------------------------------------------------------------

;; Single keys: two successive reads over one recording port capture "dw", and
;; each decodes to the plain character event it should.
(test-assert "records single keys d then w"
  (let ([rec (open-output-string)]
        [port (open-input-string "dw")])
    (parameterize ([current-key-recording rec])
      ;; let* forces the two reads left-to-right; a plain let leaves the order
      ;; of the two side-effecting reads unspecified.
      (let* ([ev1 (read-key-event port)]
             [ev2 (read-key-event port)])
        (and (key-event=? ev1 (make-key-event 'char #\d 0))
             (key-event=? ev2 (make-key-event 'char #\w 0))
             (string=? (get-output-string rec) "dw"))))))

;; Multi-key escape sequence: the inline parse-csi reads are teed too, so all
;; three bytes of the ESC [ C arrow are captured, not just the leading ESC.
(test-assert "records all three bytes of an ESC [ C arrow"
  (let ([res (decode-on (string #\x1b #\[ #\C))])
    (and (key-event=? (car res) (make-key-event 'special 'right 0))
         (string=? (cdr res) (string #\x1b #\[ #\C)))))

;; A control byte is captured verbatim (one consumed byte, one recorded byte).
(test-assert "records a control byte (C-a)"
  (let ([res (decode-on (string (integer->char 1)))])
    (and (key-event=? (car res) (make-key-event 'ctrl #\a 0))
         (string=? (cdr res) (string (integer->char 1))))))

;; ---------------------------------------------------------------------------
;; Recording off (the #f default): decoding is byte-for-byte unchanged.
;; ---------------------------------------------------------------------------

(test-assert "off: plain char decodes to (char x 0)"
  (key-event=? (decode-off "x") (make-key-event 'char #\x 0)))

(test-assert "off: control byte 1 decodes to (ctrl a 0)"
  (key-event=? (decode-off (string (integer->char 1))) (make-key-event 'ctrl #\a 0)))

(test-assert "off: lone ESC decodes to (special escape 0)"
  (key-event=? (decode-off (string #\x1b)) (make-key-event 'special 'escape 0)))

(test-assert "off: ESC [ C decodes to (special right 0)"
  (key-event=? (decode-off (string #\x1b #\[ #\C)) (make-key-event 'special 'right 0)))

(test-assert "off: empty port returns the eof-object"
  (eof-object? (decode-off "")))

;; ---------------------------------------------------------------------------
;; On/off equivalence: the SAME bytes decode to the SAME event regardless of
;; whether the tee is recording. This is the behaviour-preservation contract --
;; the tee must never alter how a sequence decodes.
;; ---------------------------------------------------------------------------

(define (decode-identical? bytes)
  (key-event=? (car (decode-on bytes)) (decode-off bytes)))

(test-assert "on/off identical: plain char"
  (decode-identical? "x"))
(test-assert "on/off identical: control byte (C-a)"
  (decode-identical? (string (integer->char 1))))
(test-assert "on/off identical: tab (char 9)"
  (decode-identical? (string (integer->char 9))))
(test-assert "on/off identical: lone ESC"
  (decode-identical? (string #\x1b)))
(test-assert "on/off identical: ESC [ C arrow"
  (decode-identical? (string #\x1b #\[ #\C)))
(test-assert "on/off identical: M-f (ESC f)"
  (decode-identical? (string #\x1b #\f)))
(test-assert "on/off identical: delete (ESC [ 3 ~)"
  (decode-identical? (string #\x1b #\[ #\3 #\~)))
(test-assert "on/off identical: eof on empty port"
  (and (eof-object? (car (decode-on "")))
       (eof-object? (decode-off ""))))

;; ---------------------------------------------------------------------------
;; The tee never captures the end-of-input sentinel: a trailing ESC in a bounded
;; port still decodes to (special escape 0) with the tee on, and the recording
;; holds exactly the one ESC byte -- no spurious eof marker was ever written.
;; ---------------------------------------------------------------------------

(test-assert "trailing ESC at EOF: escape, and only the ESC byte recorded"
  (let ([res (decode-on (string #\x1b))])
    (and (key-event=? (car res) (make-key-event 'special 'escape 0))
         (= (string-length (cdr res)) 1)
         (string=? (cdr res) (string #\x1b)))))

(test-end)
