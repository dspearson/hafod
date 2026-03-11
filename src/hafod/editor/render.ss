;;; (hafod editor render) -- Terminal rendering with rainbow parens and identifiers
;;; Provides render-line (redraw prompt + buffer) and flash-matching-paren.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor render)
  (export render-line flash-matching-paren tokenize display-colourised
          render-completion-menu render-completion-menu/highlight
          render-line/suggestion)
  (import (chezscheme)
          (hafod editor gap-buffer)
          (hafod editor input-decode)
          (hafod editor sexp-tracker))

  ;; ======================================================================
  ;; ANSI display width (unchanged)
  ;; ======================================================================

  (define (ansi-display-width str)
    (let ([len (string-length str)]
          [esc #\x1b])
      (let loop ([i 0] [w 0])
        (cond
          [(>= i len) w]
          [(char=? (string-ref str i) esc)
           (if (and (< (+ i 1) len) (char=? (string-ref str (+ i 1)) #\[))
               (let skip ([j (+ i 2)])
                 (cond
                   [(>= j len) w]
                   [(let ([c (char->integer (string-ref str j))])
                      (and (>= c #x40) (<= c #x7E)))
                    (loop (+ j 1) w)]
                   [else (skip (+ j 1))]))
               (if (< (+ i 1) len)
                   (loop (+ i 2) w)
                   (loop (+ i 1) w)))]
          [else
           (loop (+ i 1) (+ w (char-display-width (string-ref str i))))]))))

  ;; Count newlines in a string.
  (define (count-newlines s)
    (let ([len (string-length s)])
      (let loop ([i 0] [n 0])
        (if (>= i len) n
            (loop (+ i 1) (if (char=? (string-ref s i) #\newline) (+ n 1) n))))))

  ;; Get display width of text after the last newline.
  (define (width-after-last-newline s)
    (let ([len (string-length s)])
      (let ([start (let loop ([i (- len 1)])
                     (cond [(< i 0) 0]
                           [(char=? (string-ref s i) #\newline) (+ i 1)]
                           [else (loop (- i 1))]))])
        (string-display-width (substring s start len)))))

  ;; ======================================================================
  ;; Colour palettes (Doom Emacs inspired)
  ;; ======================================================================

  ;; Rainbow paren colours — golden-angle hue spacing for maximum adjacent contrast.
  ;; Each successive level is ~137.5° apart in hue, ensuring no two neighbours
  ;; are perceptually similar.  Lightness kept high (70-85%) for dark backgrounds.
  (define paren-colours
    '#((255 153  94)    ; depth 0: orange    #ff995e  hue≈24°
       (130 170 255)    ; depth 1: blue      #82aaff  hue≈222°
       (195 232 141)    ; depth 2: green     #c3e88d  hue≈87°
       (249 137 211)    ; depth 3: pink      #f989d3  hue≈320°
       (180 249 248)    ; depth 4: cyan      #b4f9f8  hue≈179°
       (255 199 119)    ; depth 5: yellow    #ffc777  hue≈35°
       (192 153 255)    ; depth 6: purple    #c099ff  hue≈263°
       (119 224 198)    ; depth 7: teal      #77e0c6  hue≈157°
       (255 152 164)))  ; depth 8: rose      #ff98a4  hue≈353°

  ;; Token colours (doom-moonlight)
  (define string-colour  '(195 232 141))  ; green  #c3e88d
  (define comment-colour '(122 131 160))  ; grey   #7a83a0
  (define number-colour  '(255 153  94))  ; orange #ff995e
  (define boolean-colour '(180 249 248))  ; cyan   #b4f9f8

  (define num-paren-colours (vector-length paren-colours))

  ;; ======================================================================
  ;; CIE L*a*b* rainbow identifiers (matching Doom's algorithm)
  ;; ======================================================================

  ;; L*a*b* → sRGB conversion via XYZ (D65 white point)
  (define lab-pi 3.141592653589793)

  (define (lab->rgb L* a* b*)
    (let* ([delta (/ 6.0 29.0)]
           [delta2 (* delta delta)]
           [f->t (lambda (f)
                   (if (> f delta) (* f f f) (* 3.0 delta2 (- f (/ 4.0 29.0)))))]
           [fy (/ (+ L* 16.0) 116.0)]
           [fx (+ (/ a* 500.0) fy)]
           [fz (- fy (/ b* 200.0))]
           ;; D65 white point
           [X (* 0.95047 (f->t fx))]
           [Y (* 1.00000 (f->t fy))]
           [Z (* 1.08883 (f->t fz))]
           ;; XYZ → linear sRGB
           [Rl (+ (*  3.2404542 X) (* -1.5371385 Y) (* -0.4985314 Z))]
           [Gl (+ (* -0.9692660 X) (*  1.8760108 Y) (*  0.0415560 Z))]
           [Bl (+ (*  0.0556434 X) (* -0.2040259 Y) (*  1.0572252 Z))]
           ;; sRGB gamma
           [gamma (lambda (c)
                    (if (<= c 0.0031308) (* 12.92 c)
                        (- (* 1.055 (expt (max 0.0 c) (/ 1.0 2.4))) 0.055)))]
           [to-byte (lambda (v)
                      (max 0 (min 255 (inexact->exact (round (* 255.0 (gamma v)))))))])
      (list (to-byte Rl) (to-byte Gl) (to-byte Bl))))

  ;; Compute rainbow identifier colour from hash.
  ;; Matches rainbow-identifiers-cie-l*a*b*-choose-face with L=65, chroma=50.
  (define (ident-colour-from-hash h)
    (let* ([hue-deg (modulo h 360)]
           [theta (* hue-deg (/ lab-pi 180.0))]
           [a* (* 50.0 (cos theta))]
           [b* (* 50.0 (sin theta))])
      (lab->rgb 65.0 a* b*)))

  ;; ======================================================================
  ;; ANSI colour helpers
  ;; ======================================================================

  (define (fg-colour port r g b)
    (display "\x1b;[38;2;" port)
    (display r port) (display ";" port)
    (display g port) (display ";" port)
    (display b port) (display "m" port))

  (define (fg-colour-l port rgb)
    (fg-colour port (car rgb) (cadr rgb) (caddr rgb)))

  ;; ======================================================================
  ;; Tokenizer
  ;; ======================================================================

  ;; Token delimiter for splitting atoms
  (define (token-delimiter? ch)
    (or (char-whitespace? ch)
        (memv ch '(#\( #\) #\[ #\] #\" #\; #\' #\` #\,))))

  ;; djb2 string hash, kept positive via bitmask
  (define (ident-hash s)
    (let ([len (string-length s)])
      (let loop ([i 0] [h 5381])
        (if (>= i len) h
            (loop (+ i 1)
                  (bitwise-and (+ (* h 33) (char->integer (string-ref s i)))
                               #xFFFFFFFF))))))

  ;; Heuristic: does this atom span look like a number?
  (define (number-atom? text start end)
    (and (< start end)
         (let ([ch (string-ref text start)])
           (or (char-numeric? ch)
               (and (< (+ start 1) end)
                    (or (char=? ch #\-) (char=? ch #\+))
                    (char-numeric? (string-ref text (+ start 1))))))))

  ;; Heuristic: is this hash-prefixed atom a boolean or char literal?
  ;; #t #f #true #false #\x #\newline etc.
  (define (boolean-or-char? text start end)
    (and (< (+ start 1) end)
         (let ([ch2 (string-ref text (+ start 1))])
           (or (char=? ch2 #\t) (char=? ch2 #\f) (char=? ch2 #\\)))))

  ;; Tokenize text into list of (type start end depth).
  ;; Types: paren, atom, number, string, comment, boolean, whitespace, other
  (define (tokenize text)
    (let ([len (string-length text)])
      (let loop ([i 0] [depth 0] [acc '()])
        (if (>= i len)
            (reverse acc)
            (let ([ch (string-ref text i)])
              (cond
                ;; Whitespace
                [(char-whitespace? ch)
                 (let ws ([j (+ i 1)])
                   (if (and (< j len) (char-whitespace? (string-ref text j)))
                       (ws (+ j 1))
                       (loop j depth (cons (list 'whitespace i j depth) acc))))]
                ;; Open paren/bracket — colour at current depth, then depth++
                [(or (char=? ch #\() (char=? ch #\[))
                 (loop (+ i 1) (+ depth 1)
                       (cons (list 'paren i (+ i 1) depth) acc))]
                ;; Close paren/bracket — depth--, then colour at new depth
                [(or (char=? ch #\)) (char=? ch #\]))
                 (let ([d (max 0 (- depth 1))])
                   (loop (+ i 1) d
                         (cons (list 'paren i (+ i 1) d) acc)))]
                ;; String literal
                [(char=? ch #\")
                 (let str ([j (+ i 1)])
                   (cond
                     [(>= j len)
                      (loop j depth (cons (list 'string i j depth) acc))]
                     [(char=? (string-ref text j) #\\)
                      (str (+ j 2))]  ; skip escaped char
                     [(char=? (string-ref text j) #\")
                      (loop (+ j 1) depth (cons (list 'string i (+ j 1) depth) acc))]
                     [else (str (+ j 1))]))]
                ;; Line comment
                [(char=? ch #\;)
                 (let cmt ([j (+ i 1)])
                   (if (or (>= j len) (char=? (string-ref text j) #\newline))
                       (loop j depth (cons (list 'comment i j depth) acc))
                       (cmt (+ j 1))))]
                ;; Hash prefix
                [(char=? ch #\#)
                 (if (< (+ i 1) len)
                     (let ([next (string-ref text (+ i 1))])
                       (cond
                         ;; Block comment #| ... |#
                         [(char=? next #\|)
                          (let bc ([j (+ i 2)] [d 1])
                            (cond
                              [(>= j len)
                               (loop j depth (cons (list 'comment i j depth) acc))]
                              [(and (char=? (string-ref text j) #\#)
                                    (< (+ j 1) len)
                                    (char=? (string-ref text (+ j 1)) #\|))
                               (bc (+ j 2) (+ d 1))]
                              [(and (char=? (string-ref text j) #\|)
                                    (< (+ j 1) len)
                                    (char=? (string-ref text (+ j 1)) #\#))
                               (if (= d 1)
                                   (loop (+ j 2) depth
                                         (cons (list 'comment i (+ j 2) depth) acc))
                                   (bc (+ j 2) (- d 1)))]
                              [else (bc (+ j 1) d)]))]
                         ;; Datum comment #; — just highlight the #; prefix as comment
                         [(char=? next #\;)
                          (loop (+ i 2) depth
                                (cons (list 'comment i (+ i 2) depth) acc))]
                         ;; Vector/bytevector #( — emit # as other, let ( be paren
                         [(or (char=? next #\() (char=? next #\[))
                          (loop (+ i 1) depth
                                (cons (list 'other i (+ i 1) depth) acc))]
                         ;; Character literal #\ or boolean #t/#f/#true/#false
                         [else
                          (let atom ([j (+ i 1)])
                            (if (or (>= j len) (token-delimiter? (string-ref text j)))
                                (loop j depth
                                      (cons (list (if (boolean-or-char? text i j)
                                                      'boolean 'atom)
                                                  i j depth)
                                            acc))
                                (atom (+ j 1))))]))
                     ;; # at end of input
                     (loop (+ i 1) depth
                           (cons (list 'other i (+ i 1) depth) acc)))]
                ;; Quote, quasiquote, unquote
                [(or (char=? ch #\') (char=? ch #\`))
                 (loop (+ i 1) depth
                       (cons (list 'other i (+ i 1) depth) acc))]
                [(char=? ch #\,)
                 (if (and (< (+ i 1) len) (char=? (string-ref text (+ i 1)) #\@))
                     (loop (+ i 2) depth
                           (cons (list 'other i (+ i 2) depth) acc))
                     (loop (+ i 1) depth
                           (cons (list 'other i (+ i 1) depth) acc)))]
                ;; Atom (identifier or number)
                [else
                 (let atom ([j i])
                   (if (or (>= j len) (token-delimiter? (string-ref text j)))
                       (loop j depth
                             (cons (list (if (number-atom? text i j) 'number 'atom)
                                        i j depth)
                                   acc))
                       (atom (+ j 1))))]))))))

  ;; ======================================================================
  ;; Colourised display
  ;; ======================================================================

  (define (display-colourised port text tokens cursor-pos)
    (let-values ([(open-idx close-idx) (find-enclosing-parens text cursor-pos)])
      (for-each
        (lambda (tok)
          (let ([type (car tok)]
                [start (cadr tok)]
                [end (caddr tok)]
                [depth (cadddr tok)])
            (let ([span (substring text start end)])
              (case type
                [(paren)
                 (let ([col (vector-ref paren-colours
                              (modulo depth num-paren-colours))]
                       [bold? (or (eqv? start open-idx)
                                  (eqv? start close-idx))])
                   (when bold? (display "\x1b;[1m" port))
                   (fg-colour-l port col)
                   (display span port)
                   (display "\x1b;[0m" port))]
                [(atom)
                 (let ([col (ident-colour-from-hash (ident-hash span))])
                   (fg-colour-l port col)
                   (display span port)
                   (display "\x1b;[39m" port))]
                [(number)
                 (fg-colour-l port number-colour)
                 (display span port)
                 (display "\x1b;[39m" port)]
                [(boolean)
                 (fg-colour-l port boolean-colour)
                 (display span port)
                 (display "\x1b;[39m" port)]
                [(string)
                 (fg-colour-l port string-colour)
                 (display span port)
                 (display "\x1b;[39m" port)]
                [(comment)
                 (fg-colour-l port comment-colour)
                 (display span port)
                 (display "\x1b;[39m" port)]
                [else
                 (display span port)]))))
        tokens)))

  ;; ======================================================================
  ;; render-line
  ;; ======================================================================

  ;; render-line: redraw prompt + buffer.  prev-lines is the number of
  ;; lines that were on screen from the *previous* render (0 on first call).
  ;; Returns the current total-lines count for passing to the next render.
  (define (render-line port prompt gb prev-lines)
    (let* ([before (gap-buffer-before-string gb)]
           [after  (gap-buffer-after-string gb)]
           [text (string-append before after)]
           [cursor-pos (gap-buffer-cursor-pos gb)]
           [total-lines (count-newlines text)]
           [prompt-width (ansi-display-width prompt)]
           [cursor-row (count-newlines before)]
           [cursor-col (+ (if (= cursor-row 0) prompt-width 0)
                          (width-after-last-newline before)
                          1)]
           ;; Move up by whichever is larger: the previous or current line count
           [move-up (max total-lines prev-lines)])
      ;; Move up to prompt line if multi-line
      (when (> move-up 0)
        (display "\x1b;[" port)
        (display move-up port)
        (display "A" port))
      ;; CR + clear to end of screen
      (display "\r\x1b;[J" port)
      ;; Display prompt
      (display prompt port)
      ;; Display colourised text
      (let ([tokens (tokenize text)])
        (display-colourised port text tokens cursor-pos))
      ;; Position cursor
      (let ([lines-after-cursor (- total-lines cursor-row)])
        (when (> lines-after-cursor 0)
          (display "\x1b;[" port)
          (display lines-after-cursor port)
          (display "A" port)))
      (display "\x1b;[" port)
      (display cursor-col port)
      (display "G" port)
      (flush-output-port port)
      total-lines))

  ;; render-line/suggestion: like render-line but appends dim ghost text after cursor
  ;; when cursor is at end-of-buffer.
  (define (render-line/suggestion port prompt gb prev-lines suggestion)
    (let* ([before (gap-buffer-before-string gb)]
           [after  (gap-buffer-after-string gb)]
           [text (string-append before after)]
           [cursor-pos (gap-buffer-cursor-pos gb)]
           [total-lines (count-newlines text)]
           [prompt-width (ansi-display-width prompt)]
           [cursor-row (count-newlines before)]
           [cursor-col (+ (if (= cursor-row 0) prompt-width 0)
                          (width-after-last-newline before)
                          1)]
           [move-up (max total-lines prev-lines)])
      (when (> move-up 0)
        (display "\x1b;[" port)
        (display move-up port)
        (display "A" port))
      (display "\r\x1b;[J" port)
      (display prompt port)
      (let ([tokens (tokenize text)])
        (display-colourised port text tokens cursor-pos))
      ;; Display ghost suggestion if cursor is at end
      (when (and (= cursor-pos (string-length text))
                 (> (string-length suggestion) 0))
        (display "\x1b;[38;5;240m" port)  ; dim grey
        (display suggestion port)
        (display "\x1b;[39m" port))
      ;; Position cursor
      (let ([lines-after-cursor (- total-lines cursor-row)])
        (when (> lines-after-cursor 0)
          (display "\x1b;[" port)
          (display lines-after-cursor port)
          (display "A" port)))
      (display "\x1b;[" port)
      (display cursor-col port)
      (display "G" port)
      (flush-output-port port)
      total-lines))

  ;; ======================================================================
  ;; flash-matching-paren
  ;; ======================================================================

  (define (flash-matching-paren port prompt gb match-idx prev-lines)
    (let* ([text (gap-buffer->string gb)]
           [prefix (if (> match-idx 0) (substring text 0 match-idx) "")]
           [prompt-width (ansi-display-width prompt)]
           [col (+ prompt-width (string-display-width prefix) 1)]
           [match-ch (string-ref text match-idx)])
      ;; Save cursor
      (display "\x1b;7" port)
      ;; Move to column of matching paren
      (display "\x1b;[" port)
      (display col port)
      (display "G" port)
      ;; Reverse video
      (display "\x1b;[7m" port)
      (display match-ch port)
      (display "\x1b;[0m" port)
      ;; Restore cursor
      (display "\x1b;8" port)
      (flush-output-port port)
      ;; Sleep ~120ms
      (sleep (make-time 'time-duration 120000000 0))
      ;; Re-render to clear the highlight
      (render-line port prompt gb prev-lines)))

  ;; ======================================================================
  ;; render-completion-menu
  ;; ======================================================================

  ;; Render a vertical list of completion candidates below the edit line.
  ;; Highlights the selected candidate with reverse video.
  ;; Returns the number of menu lines displayed (for cleanup tracking).
  (define render-completion-menu
    (case-lambda
      [(port candidates selected-index)
       (render-completion-menu port candidates selected-index 10)]
      [(port candidates selected-index max-items)
       (let* ([n (length candidates)]
              [visible-count (min n max-items)]
              ;; Scroll window to keep selected-index visible
              [window-start (cond
                              [(< selected-index 0) 0]  ; no selection yet
                              [(< selected-index max-items) 0]
                              [else (- selected-index (- max-items 1))])]
              [window-end (+ window-start visible-count)]
              [overflow (> n max-items)])
         ;; Move to next line and emit menu entries
         (display "\n" port)
         (let loop ([i window-start] [lines 0])
           (cond
             [(or (>= i window-end) (>= i n))
              ;; Show overflow indicator if needed
              (let ([total-lines (if (and overflow (> (- n window-end) 0))
                                     (begin
                                       (display "  ... and " port)
                                       (display (- n window-end) port)
                                       (display " more\n" port)
                                       (+ lines 1))
                                     lines)])
                total-lines)]
             [else
              (let ([candidate (list-ref candidates i)])
                ;; Highlight selected candidate with reverse video
                (when (= i selected-index)
                  (display "\x1b;[7m" port))
                (display "  " port)
                (display candidate port)
                (when (= i selected-index)
                  (display "\x1b;[0m" port))
                (display "\n" port)
                (loop (+ i 1) (+ lines 1)))])))]))

  ;; ======================================================================
  ;; render-completion-menu/highlight
  ;; ======================================================================

  ;; Like render-completion-menu, but accepts (candidate . positions) pairs
  ;; and highlights the matched characters with bold/underline.
  ;; Shows n/N match count indicator on the first line.
  (define render-completion-menu/highlight
    (case-lambda
      [(port entries selected-index)
       (render-completion-menu/highlight port entries selected-index 10)]
      [(port entries selected-index max-items)
       (let* ([n (length entries)]
              [visible-count (min n max-items)]
              [window-start (cond
                              [(< selected-index 0) 0]
                              [(< selected-index max-items) 0]
                              [else (- selected-index (- max-items 1))])]
              [window-end (+ window-start visible-count)]
              [overflow (> n max-items)])
         (display "\n" port)
         ;; Match count indicator
         (display "\x1b;[38;5;240m" port)  ; dim grey
         (display "  " port)
         (if (>= selected-index 0)
             (begin (display (+ selected-index 1) port)
                    (display "/" port)
                    (display n port))
             (begin (display n port)
                    (display " match" port)
                    (when (not (= n 1)) (display "es" port))))
         (display "\x1b;[39m" port)
         (display "\n" port)
         (let loop ([i window-start] [lines 1])  ; 1 for the count line
           (cond
             [(or (>= i window-end) (>= i n))
              (let ([total-lines (if (and overflow (> (- n window-end) 0))
                                     (begin
                                       (display "  ... and " port)
                                       (display (- n window-end) port)
                                       (display " more\n" port)
                                       (+ lines 1))
                                     lines)])
                total-lines)]
             [else
              (let* ([entry (list-ref entries i)]
                     [candidate (car entry)]
                     ;; Support both (name . positions) and (name positions desc)
                     [positions (if (and (pair? (cdr entry))
                                         (pair? (cddr entry))
                                         (list? (cadr entry)))
                                    (cadr entry)
                                    (cdr entry))]
                     [description (if (and (pair? (cdr entry))
                                           (pair? (cddr entry))
                                           (list? (cadr entry)))
                                      (caddr entry)
                                      #f)])
                (when (= i selected-index)
                  (display "\x1b;[7m" port))
                (display "  " port)
                (display-with-highlights port candidate positions)
                ;; Show description in dim grey after candidate
                (when (and description (string? description))
                  (when (not (= i selected-index))
                    (display "\x1b;[38;5;240m" port))
                  (display "  " port)
                  (display description port)
                  (when (not (= i selected-index))
                    (display "\x1b;[39m" port)))
                (when (= i selected-index)
                  (display "\x1b;[0m" port))
                (display "\n" port)
                (loop (+ i 1) (+ lines 1)))])))]))

  ;; Display a string with certain character positions highlighted (bold+underline).
  (define (display-with-highlights port str positions)
    (let ([len (string-length str)]
          [pos-set (make-eq-hashtable)])
      ;; Build a set of positions for O(1) lookup
      (for-each (lambda (p) (hashtable-set! pos-set p #t)) positions)
      (let loop ([i 0] [in-highlight? #f])
        (when (< i len)
          (let ([want-hl? (hashtable-ref pos-set i #f)])
            (cond
              [(and want-hl? (not in-highlight?))
               (display "\x1b;[1;4m" port)  ; bold + underline
               (display (string-ref str i) port)
               (loop (+ i 1) #t)]
              [(and (not want-hl?) in-highlight?)
               (display "\x1b;[22;24m" port)  ; reset bold + underline
               (display (string-ref str i) port)
               (loop (+ i 1) #f)]
              [else
               (display (string-ref str i) port)
               (loop (+ i 1) in-highlight?)]))))
      ;; Reset if we ended in highlight
      (when (let loop ([i 0] [last #f])
              (cond [(>= i len) last]
                    [(hashtable-ref pos-set i #f) (loop (+ i 1) #t)]
                    [else (loop (+ i 1) #f)]))
        (display "\x1b;[22;24m" port))))

) ; end library
