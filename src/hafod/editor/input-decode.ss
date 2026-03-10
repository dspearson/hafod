;;; (hafod editor input-decode) -- Terminal input byte sequence to key-event decoder
;;; Converts raw terminal input (ANSI escapes, UTF-8, control chars) into
;;; structured key-event records. Also provides wcwidth-based display width.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor input-decode)
  (export make-key-event key-event? key-event-type key-event-value key-event-mods
          key-event=?
          read-key-event
          char-display-width string-display-width
          MOD_SHIFT MOD_ALT MOD_CTRL)
  (import (chezscheme) (hafod internal platform-constants))

  ;; Load libc for wcwidth
  (define load-libc (load-shared-object #f))

  ;; Key-event record
  ;; type: symbol -- char, ctrl, meta, special
  ;; value: character or symbol (e.g., #\a, 'up, 'home, 'delete, 'tab, 'return, 'backspace)
  ;; mods: integer bitmask (0=none, reserved for future shift/alt/ctrl combos)
  (define-record-type key-event
    (fields type value mods)
    (protocol (lambda (new)
                (lambda (type value mods)
                  (new type value mods)))))

  (define (key-event=? a b)
    (and (eq? (key-event-type a) (key-event-type b))
         (eqv? (key-event-value a) (key-event-value b))
         (= (key-event-mods a) (key-event-mods b))))

  ;; Modifier bitmask constants (xterm encoding: modifier_param - 1)
  ;; 2=Shift, 3=Alt, 4=Shift+Alt, 5=Ctrl, 6=Ctrl+Shift, 7=Ctrl+Alt
  (define MOD_SHIFT 1)
  (define MOD_ALT   2)
  (define MOD_CTRL  4)

  ;; Extract xterm modifier from CSI params string.
  ;; Params like "1;3" -> modifier 3 -> bitmask 2 (Alt).
  ;; Returns 0 if no modifier or unparseable.
  (define (csi-extract-modifier params)
    (let ([semi (let loop ([i 0])
                  (cond
                    [(>= i (string-length params)) #f]
                    [(char=? (string-ref params i) #\;) i]
                    [else (loop (+ i 1))]))])
      (if semi
          (let ([mod-str (substring params (+ semi 1) (string-length params))])
            (let ([n (string->number mod-str)])
              (if n (- n 1) 0)))
          0)))

  ;; Extract key number from CSI params (part before semicolon).
  (define (csi-extract-key-number params)
    (let ([semi (let loop ([i 0])
                  (cond
                    [(>= i (string-length params)) #f]
                    [(char=? (string-ref params i) #\;) i]
                    [else (loop (+ i 1))]))])
      (if semi
          (string->number (substring params 0 semi))
          (string->number params))))

  ;; CSI final byte -> special key symbol
  (define (csi-final->key final)
    (case final
      [(#\A) 'up]
      [(#\B) 'down]
      [(#\C) 'right]
      [(#\D) 'left]
      [(#\H) 'home]
      [(#\F) 'end]
      [(#\Z) 'backtab]   ; Shift-Tab (ESC[Z)
      [else #f]))

  ;; Tilde sequences: key-number~ -> key symbol
  (define (csi-tilde-key n)
    (case n
      [(1) 'home]
      [(3) 'delete]
      [(4) 'end]
      [(5) 'page-up]
      [(6) 'page-down]
      [(200) 'paste-start]
      [(201) 'paste-end]
      [else #f]))

  ;; Parse CSI sequence: ESC [ already consumed, read params + final
  (define (parse-csi port)
    (let loop ([params ""])
      (let ([ch (read-char port)])
        (cond
          [(eof-object? ch)
           (make-key-event 'special 'escape 0)]
          [(and (char>=? ch #\@) (char<=? ch #\~))
           ;; Final byte -- extract modifier and key
           (let ([mods (csi-extract-modifier params)])
             (if (char=? ch #\~)
                 ;; Tilde sequence: ESC[3~, ESC[3;5~, etc.
                 (let ([key (csi-tilde-key (or (csi-extract-key-number params) 0))])
                   (if key
                       (make-key-event 'special key mods)
                       (make-key-event 'special 'escape 0)))
                 ;; Letter final: ESC[C, ESC[1;3C, etc.
                 (let ([key (csi-final->key ch)])
                   (if key
                       (make-key-event 'special key mods)
                       (make-key-event 'special 'escape 0)))))]
          [else
           ;; Parameter or intermediate byte
           (loop (string-append params (string ch)))]))))

  ;; Parse SS3 sequence: ESC O already consumed
  (define (parse-ss3 port)
    (let ([ch (read-char port)])
      (if (eof-object? ch)
          (make-key-event 'special 'escape 0)
          (case ch
            [(#\A) (make-key-event 'special 'up 0)]
            [(#\B) (make-key-event 'special 'down 0)]
            [(#\C) (make-key-event 'special 'right 0)]
            [(#\D) (make-key-event 'special 'left 0)]
            [(#\H) (make-key-event 'special 'home 0)]
            [(#\F) (make-key-event 'special 'end 0)]
            [else  (make-key-event 'meta ch 0)]))))

  ;; Read a key event from port
  (define (read-key-event port)
    (let ([ch (read-char port)])
      (cond
        [(eof-object? ch) ch]  ; propagate eof
        ;; ESC
        [(char=? ch #\x1b)
         (if (char-ready? port)
             (let ([next (read-char port)])
               (cond
                 [(eof-object? next)
                  (make-key-event 'special 'escape 0)]
                 [(char=? next #\[)
                  (parse-csi port)]
                 [(char=? next #\O)
                  (parse-ss3 port)]
                 ;; ESC DEL -> Alt+Backspace
                 [(char=? next #\x7f)
                  (make-key-event 'special 'backspace MOD_ALT)]
                 ;; ESC + control char (1-26) -> C-M-key
                 [(<= 1 (char->integer next) 26)
                  (make-key-event 'ctrl (integer->char (+ (char->integer next) 96)) MOD_ALT)]
                 [else
                  (make-key-event 'meta next 0)]))
             (make-key-event 'special 'escape 0))]
        ;; Backspace (DEL)
        [(char=? ch #\x7f)
         (make-key-event 'special 'backspace 0)]
        ;; Control characters (1-26)
        [(<= 1 (char->integer ch) 26)
         (let ([code (char->integer ch)])
           (cond
             [(= code 9)  (make-key-event 'special 'tab 0)]
             [(= code 10) (make-key-event 'special 'newline 0)]
             [(= code 13) (make-key-event 'special 'return 0)]
             [else (make-key-event 'ctrl (integer->char (+ code 96)) 0)]))]
        ;; Normal character
        [else
         (make-key-event 'char ch 0)])))

  ;; Ensure locale is set for wcwidth to handle CJK correctly
  (define c-setlocale (foreign-procedure "setlocale" (int string) string))
  (define locale-initialized (c-setlocale PLAT-LC-ALL ""))

  ;; wcwidth FFI for display width calculation
  (define c-wcwidth (foreign-procedure "wcwidth" (int) int))

  (define (char-display-width ch)
    (let ([w (c-wcwidth (char->integer ch))])
      (if (< w 0) 1 w)))  ; fallback to 1 for non-printable

  (define (string-display-width str)
    (let loop ([i 0] [w 0])
      (if (>= i (string-length str)) w
          (loop (+ i 1) (+ w (char-display-width (string-ref str i)))))))

  ) ; end library
