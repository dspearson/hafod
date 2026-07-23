;;; (hafod editor render) -- Terminal rendering with rainbow parens and identifiers
;;; Provides render-line (redraw prompt + buffer) and flash-matching-paren.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor render)
  (export render-line flash-matching-paren tokenize display-colourised
          render-completion-menu render-completion-menu/highlight
          render-completion-grid
          render-line/suggestion
          render-line/selection
          only-closing-delimiters?
          ghost-visible-suffix
          display-with-highlights
          display-with-selection
          ;; Reusable terminal-overlay helpers (editor + tests)
          overlay-clear! overlay-draw
          ;; Geometry helpers for external consumers (editor + tests)
          count-visual-lines cursor-visual-row cursor-visual-row+col ansi-display-width
          ;; Colour constants and helpers for external consumers (e.g. finder)
          paren-colours num-paren-colours
          string-colour comment-colour number-colour boolean-colour
          fg-colour fg-colour-l
          ident-colour-from-hash ident-hash
          ;; Settings
          rainbow-identifiers?
          rainbow-parens?
          syntax-highlight?
          ;; Live shell-line highlighting toggles.  Published to the
          ;; (hafod) umbrella + (scsh) mirror by a later change; the shell
          ;; colouriser and the shape gate stay library-internal.
          shell-highlight? shell-highlight-paths?
          ;; Pure shell-line lexer + fixed role->colour map.  White-box exports
          ;; for the isolated span/colour test suite only: both are leaf helpers
          ;; and stay OFF the (hafod) umbrella and the (scsh) mirror.
          shell-lex shell-role-colour)
  (import (chezscheme)
          (hafod editor gap-buffer)
          (hafod editor input-decode)
          (hafod editor sexp-tracker)
          (only (hafod shell classifier)
                classify-input scheme-prefix-chars command-not-found-suppress?)
          (only (hafod shell completers) strip-terminal-escapes)
          ;; The pure, header-safe grid geometry: build-row-plan lays out header +
          ;; candidate rows once and row-plan-locate/row-plan-cell map index<->cell,
          ;; so the grid draws headers without re-deriving the selection maths.
          (only (hafod editor completion-groups)
                build-row-plan row-plan-rows row-plan-locate row-plan-cell)
          (only (hafod terminal-caps) ansi-ok? colour-ok?)
          ;; The pure $LS_COLORS type/extension -> validated-SGR lookup.  A leaf:
          ;; render (and editor) import it, never the reverse, so there is no cycle.
          (only (hafod editor ls-colors) current-ls-colors candidate-sgr))

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
    (let* ([len (string-length s)]
           [start (let loop ([i (- len 1)])
                    (cond [(< i 0) 0]
                          [(char=? (string-ref s i) #\newline) (+ i 1)]
                          [else (loop (- i 1))]))])
      (string-display-width (substring s start len))))

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

  ;; Toggle for rainbow identifier colouring.
  ;; When #t (default), identifiers inside parentheses get CIE L*a*b* hue
  ;; colours.  Top-level atoms (depth 0, i.e. shell commands) are always
  ;; rendered in the default foreground regardless of this setting.
  ;; Set to #f to disable rainbow identifiers entirely.
  (define rainbow-identifiers?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; Toggle for rainbow parenthesis colouring.
  ;; When #t (default), parens/brackets are coloured by nesting depth.
  ;; Set to #f to render all parens in default foreground.
  (define rainbow-parens?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; Toggle for syntax highlighting of strings, comments, numbers, booleans.
  ;; When #t (default), these token types get distinct colours.
  ;; Set to #f to render everything in default foreground (except rainbow
  ;; parens/identifiers which have their own toggles).
  (define syntax-highlight?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; Toggle for live shell-line highlighting.  When #t (default) a
  ;; shell-shaped command line is coloured by role -- the head green when it
  ;; resolves on PATH / builtin / alias and red when it does not, plus distinct
  ;; colours for quoted strings, options/flags and redirections.  Set to #f to
  ;; draw shell lines plain (the pre-change behaviour).
  (define shell-highlight?
    (make-parameter #t (lambda (v) (and v #t))))

  ;; Toggle for flagging a non-existent path argument.  When #t (default)
  ;; a path-shaped argument (one containing `/`) that does not exist on disk is
  ;; drawn red + underlined.  This is the ONE filesystem touch on the render hot
  ;; path, so it has its own switch: set to #f to keep highlighting entirely
  ;; filesystem-free.  A `~`-leading token is never stat'd regardless.
  (define shell-highlight-paths?
    (make-parameter #t (lambda (v) (and v #t))))

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
    (ident-hash-range s 0 (string-length s)))

  ;; The same djb2 hash over the half-open range [start,end) of s, without
  ;; allocating the substring.  The colourised display hashes an identifier's
  ;; WHOLE span this way even when it draws only a clipped part of it, so an atom
  ;; that straddles a draw window's edge keeps ONE hue across both halves.
  (define (ident-hash-range s start end)
    (let loop ([i start] [h 5381])
      (if (>= i end) h
          (loop (+ i 1)
                (bitwise-and (+ (* h 33) (char->integer (string-ref s i)))
                             #xFFFFFFFF)))))

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
  ;; Shell-line lexer
  ;; ======================================================================

  ;; A pure, side-effect-free lexer for a single shell command line, the sibling
  ;; of `tokenize` above.  It mirrors the same single left-to-right integer-offset
  ;; walk and allocates no substrings while lexing (the renderer clips and draws);
  ;; unlike `parser.ss tokenise` it performs NO glob/brace/tilde expansion and
  ;; touches NO filesystem, so it is cheap enough to run on every keystroke.  The
  ;; colouriser that consumes these spans lives in a later change; this is only
  ;; the span structure and the fixed role->colour table.

  ;; Scan a quoted run whose opening quote (character `q`) sits at index `open`.
  ;; Escape-aware, mirroring the string arm of `tokenize`: a backslash escapes the
  ;; next character, so an escaped quote stays inside the run.  Returns the index
  ;; one past the matching close quote, or `len` when the run is unterminated.
  (define (scan-quote text open len q)
    (let scan ([j (+ open 1)])
      (cond
        [(>= j len) len]
        [(char=? (string-ref text j) #\\) (scan (+ j 2))]   ; skip the escaped char
        [(char=? (string-ref text j) q) (+ j 1)]
        [else (scan (+ j 1))])))

  ;; Does a redirection operator begin at index `i`?  Recognises the single-char
  ;; leads `>` and `<` (which cover `>>`/`<<`/`>&` when scanned) and the two-char
  ;; IO forms `2>` and `&>` (a digit/ampersand that is a redir ONLY when the very
  ;; next character is `>`, so a bare `2` or `&` inside a word is not one).
  (define (redir-lead? text i len)
    (let ([ch (string-ref text i)])
      (or (char=? ch #\>)
          (char=? ch #\<)
          (and (or (char=? ch #\2) (char=? ch #\&))
               (< (+ i 1) len)
               (char=? (string-ref text (+ i 1)) #\>)))))

  ;; Consume the whole redirection operator beginning at `i` (a redir-lead?), and
  ;; return the index just past it.  Handles `>` `>>` `>&` `<` `<<` `2>` `&>`.
  (define (scan-redir text i len)
    (let ([ch (string-ref text i)])
      (cond
        [(or (char=? ch #\2) (char=? ch #\&)) (+ i 2)]        ; 2>  &>
        [(char=? ch #\>)
         (if (and (< (+ i 1) len)
                  (let ([n (string-ref text (+ i 1))])
                    (or (char=? n #\>) (char=? n #\&))))
             (+ i 2)                                          ; >>  >&
             (+ i 1))]                                        ; >
        [(char=? ch #\<)
         (if (and (< (+ i 1) len) (char=? (string-ref text (+ i 1)) #\<))
             (+ i 2)                                          ; <<
             (+ i 1))]                                        ; <
        [else (+ i 1)])))

  ;; Scan a bare word from `i` up to (but not including) the next unquoted
  ;; whitespace or the start of a redirection operator.  Purely positional.
  (define (scan-word text i len)
    (let scan ([j i])
      (if (or (>= j len)
              (char-whitespace? (string-ref text j))
              (let ([c (string-ref text j)]) (or (char=? c #\>) (char=? c #\<))))
          j
          (scan (+ j 1)))))

  ;; Purely SYNTACTIC path test over the word text[start,end): is it path-shaped?
  ;; True when it contains a `/` anywhere -- which already subsumes the `./`,
  ;; `../` and `~/` prefixes the roles list names, each of which carries a slash.
  ;; No filesystem is read; whether such a path EXISTS is the renderer's decision
  ;; in a later change, never this lexer's.
  (define (path-shaped? text start end)
    (let scan ([k start])
      (cond
        [(>= k end) #f]
        [(char=? (string-ref text k) #\/) #t]
        [else (scan (+ k 1))])))

  ;; shell-lex : string -> (list (role start end)), one span per character range
  ;; so the spans tile [0, len) exactly (every character is drawn once, as the
  ;; Scheme colouriser does for whitespace/other).  Roles, left to right:
  ;;   whitespace  -- a run of whitespace
  ;;   string      -- a single- or double-quoted, escape-aware run
  ;;   redir       -- a redirection operator (> >> < << 2> &> >&)
  ;;   head        -- the FIRST non-whitespace word (POSITIONAL only)
  ;;   flag        -- a subsequent word beginning `-`
  ;;   path-arg    -- a subsequent path-shaped word (contains `/`)
  ;;   arg         -- any other subsequent word
  ;; The head role marks only WHERE the head is: this lexer never decides colour,
  ;; never reads the classifier, a parameter, or the disk -- it is a pure function
  ;; of the string.  Head resolvability is settled by the renderer later and fed
  ;; to `shell-role-colour`.
  (define (shell-lex text)
    (let ([len (string-length text)])
      (let loop ([i 0] [head-seen? #f] [acc '()])
        (if (>= i len)
            (reverse acc)
            (let ([ch (string-ref text i)])
              (cond
                ;; Whitespace run.
                [(char-whitespace? ch)
                 (let ws ([j (+ i 1)])
                   (if (and (< j len) (char-whitespace? (string-ref text j)))
                       (ws (+ j 1))
                       (loop j head-seen? (cons (list 'whitespace i j) acc))))]
                ;; Quoted string (either quote character), escape-aware.
                [(or (char=? ch #\") (char=? ch #\'))
                 (let ([end (scan-quote text i len ch)])
                   (loop end head-seen? (cons (list 'string i end) acc)))]
                ;; Redirection operator.
                [(redir-lead? text i len)
                 (let ([end (scan-redir text i len)])
                   (loop end head-seen? (cons (list 'redir i end) acc)))]
                ;; A bare word: head (first only), flag, path-arg, or arg.
                [else
                 (let* ([end (scan-word text i len)]
                        [role (cond [(not head-seen?)          'head]
                                    [(char=? ch #\-)           'flag]
                                    [(path-shaped? text i end) 'path-arg]
                                    [else                      'arg])])
                   (loop end #t (cons (list role i end) acc)))]))))))

  ;; shell-role-colour : role head-resolvable? -> 256-colour index, or #f for "no
  ;; colour" (draw the span verbatim).  A pure table: it reads no parameter and no
  ;; filesystem.  Head resolvability is PASSED IN -- the classifier lookup lives in
  ;; the renderer, never here.  `path-arg` returns #f: a missing-path style (red +
  ;; underline) is a filesystem decision the renderer makes, not this map.  Indices
  ;; follow the codebase palette -- red 1 / green 2 / yellow 3 (the prompt and exit
  ;; segments), cyan 6 / magenta 5 (the standard ANSI base-16 indices).
  (define (shell-role-colour role head-resolvable?)
    (case role
      [(head)   (if head-resolvable? 2 1)]      ; green resolvable / red unresolved
      [(string) 3]                              ; yellow
      [(flag)   6]                              ; cyan
      [(redir)  5]                              ; magenta
      [else     #f]))                           ; arg / path-arg / whitespace: verbatim

  ;; ======================================================================
  ;; Shell-line colouriser (role colouring + unknown-head marking)
  ;; ======================================================================

  ;; 256-colour foreground SGR (xterm base-16 / 256 index), closed by the [39m
  ;; reset the Scheme windows already use.  Kept beside fg-colour (truecolour)
  ;; because the shell role map speaks in indices, not RGB triples.
  (define (fg-256 port idx)
    (display "\x1b;[38;5;" port)
    (display idx port)
    (display "m" port))

  ;; The head span (the first 'head span) of a shell-lex list, or #f when the line
  ;; has no word at all (empty or all whitespace).
  (define (find-head-span spans)
    (cond
      [(null? spans) #f]
      [(eq? (caar spans) 'head) (car spans)]
      [else (find-head-span (cdr spans))]))

  ;; Does the head look like a command name rather than a Scheme datum?  True when
  ;; its first character is NOT one of the classifier's Scheme-prefix leads, so
  ;; `(+ 1 2)`, `'x`, `#t`, `,@e`, `[a]` are left to the Scheme colouriser.  Uses
  ;; the classifier's own scheme-prefix-chars (imported), so the two never drift.
  (define (command-headish? text head-start)
    (and (< head-start (string-length text))
         (not (memv (string-ref text head-start) scheme-prefix-chars))))

  ;; Is the line shell-SHAPED -- worth highlighting even when the classifier sent
  ;; it to 'scheme because its head does not resolve?  True when there is command
  ;; structure past the head: any flag / redirection / path-arg span, OR at least
  ;; two word tokens (head + another).  A lone bare word is NOT shell-shaped, so a
  ;; single Scheme symbol typed at the prompt is never reddened.
  (define (shell-shaped? spans)
    (let loop ([s spans] [words 0] [structure? #f])
      (if (null? s)
          (or structure? (>= words 2))
          (case (caar s)
            [(flag redir path-arg) (loop (cdr s) words #t)]
            [(head arg)            (loop (cdr s) (+ words 1) structure?)]
            [else                  (loop (cdr s) words structure?)]))))

  ;; Decide whether to shell-highlight `text` (already lexed to `spans`, already
  ;; classified to `cls`), and with what head resolvability.  Returns
  ;; (values highlight? head-resolvable?).  Highlight when the classifier already
  ;; calls it shell/builtin (head resolves), OR it is 'scheme with a genuinely-
  ;; unknown, command-headish, shell-shaped head -- the unknown-head case the
  ;; classifier hides by sending every unresolved head to 'scheme.  head-resolvable?
  ;; is command-not-found-suppress? over the head token in every case, so the head
  ;; colour and the unknown-head marking are the one resolution predicate.
  (define (shell-highlight-decision text spans cls)
    (if (not (shell-highlight?))
        (values #f #f)
        (let ([head (find-head-span spans)])
          (if (not head)
              (values #f #f)
              (let* ([hstart (cadr head)]
                     [head-text (substring text hstart (caddr head))]
                     [resolvable? (command-not-found-suppress? head-text)])
                (cond
                  [(memq cls '(shell builtin)) (values #t resolvable?)]
                  [(and (not resolvable?)
                        (command-headish? text hstart)
                        (shell-shaped? spans))
                   (values #t #f)]
                  [else (values #f #f)]))))))

  ;; Does the path-shaped word text[start,end) name something that does NOT exist?
  ;; The one filesystem read on the render path, confined to path-arg spans.  A
  ;; `~`-leading word is treated as present (never stat'd) since it needs the home
  ;; expansion the lexer deliberately omits; any stat error is treated as present
  ;; too, so a fault never paints the line red.  No globbing, no expansion.
  (define (path-missing? text start end)
    (and (< start end)
         (not (char=? (string-ref text start) #\~))
         (guard (e (#t #f))
           (not (file-exists? (substring text start end))))))

  ;; Draw the half-open window [lo,hi) of a shell command line, colouring each
  ;; lexed span by role via shell-role-colour.  The head's colour is fixed by
  ;; `head-resolvable?` (passed in from shell-highlight-decision), never re-derived
  ;; here, so the two draw windows of the ghost renderer agree.  A path-shaped
  ;; argument that does not exist is drawn red + underlined when shell-highlight-
  ;; paths? is on.  With colour? #f every branch draws the span verbatim (zero
  ;; escape bytes), mirroring display-colourised/window's plain arm.
  (define (display-shell-colourised/window port text spans head-resolvable? colour? lo hi)
    (for-each
      (lambda (span)
        (let ([role (car span)] [start (cadr span)] [end (caddr span)])
          (unless (or (<= end lo) (>= start hi))
            (let ([str (substring text (max start lo) (min end hi))])
              (if (not colour?)
                  (display str port)
                  (cond
                    [(and (eq? role 'path-arg)
                          (shell-highlight-paths?)
                          (path-missing? text start end))
                     (display "\x1b;[38;5;1m\x1b;[4m" port)  ; red + underline
                     (display str port)
                     (display "\x1b;[24m\x1b;[39m" port)]
                    [(shell-role-colour role head-resolvable?)
                     => (lambda (idx)
                          (fg-256 port idx)
                          (display str port)
                          (display "\x1b;[39m" port))]
                    [else (display str port)]))))))
      spans))

  ;; ======================================================================
  ;; Colourised display
  ;; ======================================================================

  ;; display-colourised takes an optional trailing colour? (default #t).  When
  ;; colour? is #f every coloured branch falls through to its plain arm, so the
  ;; output carries zero escape bytes; the four-argument callers keep the prior
  ;; behaviour unchanged.  Both arities are the whole-text window of the core
  ;; below, so their bytes are exactly what they always were.
  (define display-colourised
    (case-lambda
      [(port text tokens cursor-pos)
       (display-colourised port text tokens cursor-pos #t)]
      [(port text tokens cursor-pos colour?)
       (display-colourised/window port text tokens cursor-pos colour?
                                  0 (string-length text))]))

  ;; display-colourised/window: colourise only the half-open window [lo,hi) of
  ;; text.  A caller can then draw the buffer in two halves and slip something
  ;; between them -- which is exactly what the ghost renderer does, splitting at
  ;; the cursor so the suggestion is drawn INSIDE paredit's auto-inserted closers
  ;; rather than after them.
  ;;
  ;; Every colour decision is still made from the WHOLE text and the token's
  ;; ORIGINAL start: the enclosing-paren bold indices, the rainbow depth, the
  ;; identifier hue (hashed over the token's whole span, not the clipped one) and
  ;; every syntax hue.  A windowed half therefore colours each character exactly
  ;; as the single full-width pass does; only the SPAN emitted is narrowed.  That
  ;; is why the window CLIPS an already-lexed token rather than re-tokenising the
  ;; fragment: a fragment would be lexed out of context -- its paren depth would
  ;; restart at zero, a string opened in the other half would be lost -- and a
  ;; token really can straddle the split, since } is not a token delimiter and so
  ;; an atom can span the cursor while the text after it is still all closers.
  ;;
  ;; With lo = 0 and hi = (string-length text) no token is skipped and no span is
  ;; clipped, so the full-width call emits byte-for-byte what it always emitted.
  (define (display-colourised/window port text tokens cursor-pos colour? lo hi)
    (let-values ([(open-idx close-idx) (find-enclosing-parens text cursor-pos)])
      (for-each
        (lambda (tok)
          (let* ([type (car tok)]
                 [start (cadr tok)]
                 [end (caddr tok)]
                 [depth (cadddr tok)])
            ;; A token lying wholly outside the window contributes nothing to it.
            (unless (or (<= end lo) (>= start hi))
              (let ([span (substring text (max start lo) (min end hi))])
                (case type
                  [(paren)
                   (if (and colour? (rainbow-parens?))
                       (let ([col (vector-ref paren-colours
                                    (modulo depth num-paren-colours))]
                             [bold? (or (eqv? start open-idx)
                                        (eqv? start close-idx))])
                         (when bold? (display "\x1b;[1m" port))
                         (fg-colour-l port col)
                         (display span port)
                         (display "\x1b;[0m" port))
                       (let ([bold? (and colour?
                                         (or (eqv? start open-idx)
                                             (eqv? start close-idx)))])
                         (when bold? (display "\x1b;[1m" port))
                         (display span port)
                         (when bold? (display "\x1b;[22m" port))))]
                  [(atom)
                   (if (and colour? (rainbow-identifiers?) (> depth 0))
                       (let ([col (ident-colour-from-hash
                                    (ident-hash-range text start end))])
                         (fg-colour-l port col)
                         (display span port)
                         (display "\x1b;[39m" port))
                       (display span port))]
                  [(number)
                   (if (and colour? (syntax-highlight?))
                       (begin (fg-colour-l port number-colour)
                              (display span port)
                              (display "\x1b;[39m" port))
                       (display span port))]
                  [(boolean)
                   (if (and colour? (syntax-highlight?))
                       (begin (fg-colour-l port boolean-colour)
                              (display span port)
                              (display "\x1b;[39m" port))
                       (display span port))]
                  [(string)
                   (if (and colour? (syntax-highlight?))
                       (begin (fg-colour-l port string-colour)
                              (display span port)
                              (display "\x1b;[39m" port))
                       (display span port))]
                  [(comment)
                   (if (and colour? (syntax-highlight?))
                       (begin (fg-colour-l port comment-colour)
                              (display span port)
                              (display "\x1b;[39m" port))
                       (display span port))]
                  [else
                   (display span port)])))))
        tokens)))

  ;; ======================================================================
  ;; render-line
  ;; ======================================================================

  ;; Count visual lines (screen rows) for text with prompt, accounting for
  ;; line wrapping at terminal width. Returns total extra rows beyond the first.
  (define (count-visual-lines prompt-width text term-cols)
    (if (<= term-cols 0)
        (count-newlines text)
        (let ([len (string-length text)])
          (let lp ([i 0] [col prompt-width] [extra 0])
            (cond
              [(>= i len) extra]
              [(char=? (string-ref text i) #\newline)
               ;; Newline: new physical line, column resets to 0 (no prompt on continuation)
               (lp (+ i 1) 0 (+ extra 1))]
              [else
               (let* ([cw (char-display-width (string-ref text i))]
                      [new-col (+ col cw)])
                 (if (> new-col term-cols)
                     ;; Wrap: extra visual line
                     (lp (+ i 1) cw (+ extra 1))
                     (lp (+ i 1) new-col extra)))])))))

  ;; Visual row AND column of the cursor within the text before it, in a single
  ;; wrap-aware walk.  The column is the display width consumed on the FINAL
  ;; visual row: it starts at prompt-width, resets to 0 at an explicit newline (a
  ;; continuation line carries no prompt) and to the wrapped character's own width
  ;; at a wrap boundary, so on a line that overruns term-cols it is the position
  ;; WITHIN the final wrapped row -- not the wrap-blind width-since-the-last-
  ;; newline that overshoots the terminal edge.  Returns a (row . col) pair;
  ;; cursor-visual-row is a thin wrapper that keeps the original single return.
  (define (cursor-visual-row+col prompt-width before-text term-cols)
    (if (<= term-cols 0)
        ;; Unknown width: no wrapping, so the column is the same wrap-blind value
        ;; the render paths emitted before -- prompt-width on row 0 (else 0) plus
        ;; the width after the last newline -- keeping this path byte-identical.
        (let ([row (count-newlines before-text)])
          (cons row
                (+ (if (= row 0) prompt-width 0)
                   (width-after-last-newline before-text))))
        (let ([len (string-length before-text)])
          (let lp ([i 0] [col prompt-width] [row 0])
            (cond
              [(>= i len) (cons row col)]
              [(char=? (string-ref before-text i) #\newline)
               (lp (+ i 1) 0 (+ row 1))]
              [else
               (let* ([cw (char-display-width (string-ref before-text i))]
                      [new-col (+ col cw)])
                 (if (> new-col term-cols)
                     (lp (+ i 1) cw (+ row 1))
                     (lp (+ i 1) new-col row)))])))))

  ;; Count visual row of cursor position within text (for cursor-up after render).
  (define (cursor-visual-row prompt-width before-text term-cols)
    (car (cursor-visual-row+col prompt-width before-text term-cols)))

  ;; The bundle of buffer + geometry values the three renderers each need up
  ;; front.  Computed once by render-prologue and read back through these
  ;; accessors, so render-line, render-line/suggestion and render-line/selection
  ;; share one prologue rather than three verbatim copies.  Immutable: every
  ;; field is set once at construction.
  (define-record-type render-geom
    (fields before after text cursor-pos prompt-width total-lines
            cursor-row cursor-col colour? ansi?))

  ;; Compute the buffer + geometry prologue shared VERBATIM by the three
  ;; renderers.  Splitting the gap buffer, measuring the prompt and computing the
  ;; wrap-aware cursor row/column is identical work for all three, so it is done
  ;; here ONCE; each renderer then adds only its own tail (the ghost suggestion
  ;; or the selection indicator).  Every step is a pure query -- no bytes are
  ;; emitted here -- so sharing this prologue leaves each renderer's escape
  ;; output byte-identical.
  (define (render-prologue port prompt gb term-cols)
    (let* ([before (gap-buffer-before-string gb)]
           [after  (gap-buffer-after-string gb)]
           [text (string-append before after)]
           [cursor-pos (gap-buffer-cursor-pos gb)]
           [prompt-width (ansi-display-width prompt)]
           [total-lines (count-visual-lines prompt-width text term-cols)]
           [cursor-row/col (cursor-visual-row+col prompt-width before term-cols)]
           ;; The emitted 1-based column is the wrap-aware visual column plus 1.
           ;; That column already carries the prompt width on row 0 and resets at
           ;; each wrap boundary, so a line wider than the terminal lands the
           ;; cursor within its final wrapped row rather than past the right edge.
           [cursor-col (+ (cdr cursor-row/col) 1)]
           ;; Decide once from the output target: colour on colour-ok?, and
           ;; cursor/movement on ansi-ok?.  A non-terminal target (a pipe or a
           ;; file) yields #f for both, so editing degrades to plain text with
           ;; no escape bytes.
           [colour? (colour-ok? port)]
           [ansi? (ansi-ok? port)])
      (make-render-geom before after text cursor-pos prompt-width total-lines
                        (car cursor-row/col) cursor-col colour? ansi?)))

  ;; render-line: redraw prompt + buffer.  prev-lines is the cursor row
  ;; (0-indexed from prompt line) left by the *previous* render call (0 on first call).
  ;; term-cols: terminal width (0 = unknown, fall back to newline counting).
  ;; Returns the cursor-row of this render for passing as prev-lines next time.
  (define render-line
    (case-lambda
      [(port prompt gb prev-lines)
       (render-line port prompt gb prev-lines 0)]
      [(port prompt gb prev-lines term-cols)
       (let* ([geom (render-prologue port prompt gb term-cols)]
              [text (render-geom-text geom)]
              [cursor-pos (render-geom-cursor-pos geom)]
              [total-lines (render-geom-total-lines geom)]
              [cursor-row (render-geom-cursor-row geom)]
              [cursor-col (render-geom-cursor-col geom)]
              [colour? (render-geom-colour? geom)]
              [ansi? (render-geom-ansi? geom)])
         ;; Move up from cursor to prompt line (prev-lines = previous cursor-row)
         (when (and ansi? (> prev-lines 0))
           (display "\x1b;[" port)
           (display prev-lines port)
           (display "A" port))
         ;; CR + clear to end of screen (wipes all old content below prompt line)
         (when ansi?
           (display "\r\x1b;[J" port))
         ;; Display prompt
         (display prompt port)
         ;; Colourise: a shell-shaped line by role (head, roles, unknown-head
         ;; marking), else a Scheme line by token, else plain.
         (let* ([spans (shell-lex text)]
                [cls (classify-input text)])
           (let-values ([(hl? head-ok?) (shell-highlight-decision text spans cls)])
             (cond
               [hl?
                (display-shell-colourised/window port text spans head-ok? colour?
                                                 0 (string-length text))]
               [(eq? cls 'scheme)
                (let ([tokens (tokenize text)])
                  (display-colourised port text tokens cursor-pos colour?))]
               [else (display text port)])))
         ;; Position cursor
         (when ansi?
           (let ([lines-after-cursor (- total-lines cursor-row)])
             (when (> lines-after-cursor 0)
               (display "\x1b;[" port)
               (display lines-after-cursor port)
               (display "A" port)))
           (display "\x1b;[" port)
           (display cursor-col port)
           (display "G" port))
         (flush-output-port port)
         cursor-row)]))

  ;; only-closing-delimiters?: #t iff every character of str is a closing
  ;; delimiter or quote; the empty string is vacuously true.  Shared by the ghost
  ;; display gate here and by the editor's history-ghost-suffix seam: an all-closer
  ;; (or empty) after-cursor string means the cursor sits at the end of the typed
  ;; region, so the ghost may still show through paredit's auto-inserted trailing
  ;; closers.  The set is the auto-pair closers ) ] " plus a harmless } .
  (define (only-closing-delimiters? str)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(>= i len) #t]
          [(memv (string-ref str i) '(#\) #\] #\} #\")) (loop (+ i 1))]
          [else #f]))))

  ;; The length of the longest common suffix of two strings (0 when they share
  ;; no trailing character).  Walks both backwards from their ends while the
  ;; characters agree, so it is bounded by the shorter string.
  (define (common-suffix-length a b)
    (let ([la (string-length a)]
          [lb (string-length b)])
      (let loop ([i (- la 1)] [j (- lb 1)] [n 0])
        (if (and (>= i 0) (>= j 0)
                 (char=? (string-ref a i) (string-ref b j)))
            (loop (- i 1) (- j 1) (+ n 1))
            n))))

  ;; ghost-visible-suffix: the part of a ghost suggestion that is actually DRAWN,
  ;; given the after-cursor text it will be drawn in front of.  The ghost is
  ;; displayed at the cursor -- inside paredit's auto-inserted closers, not past
  ;; them -- so any trailing run it shares with that after-cursor text is already
  ;; on the screen as real buffer characters and must be elided, or the line shows
  ;; those characters twice.
  ;;
  ;;   suggestion "+ 1 2)" + after ")"   ->  "+ 1 2"   the common case: the buffer's
  ;;                                                   own closer is not redrawn
  ;;   suggestion ")"      + after ")"   ->  ""        nothing left to draw
  ;;   suggestion "-world" + after ""    ->  "-world"  end-of-buffer: drawn whole
  ;;
  ;; Only the run the two GENUINELY share is elided.  For "lambda (x) x) 1)" in
  ;; front of "))" the tails "1)" and "))" differ, so exactly one closer is shared
  ;; and the ghost keeps the other -- an extra closer on screen, which is ACCEPTED:
  ;; it is the honest continuation of the matched history entry, and eliding
  ;; further would mean guessing which of the buffer's closers the ghost's own
  ;; closers correspond to.  Accepting the ghost is unaffected either way: that
  ;; path lays down the whole suggestion over the closers (cmd-accept-suggestion),
  ;; never the visible part.
  (define (ghost-visible-suffix suggestion after)
    (let ([n (common-suffix-length suggestion after)])
      (substring suggestion 0 (- (string-length suggestion) n))))

  ;; render-line/suggestion: like render-line, but draws the dim ghost AT THE
  ;; CURSOR.  The line is laid down in three pieces -- the buffer before the
  ;; cursor, then the ghost, then the buffer after it -- rather than as the whole
  ;; buffer followed by the ghost.  That layout is what lets a suggestion expand
  ;; INSIDE the parens the user already has: with paredit on, typing "(" leaves
  ;; the buffer "()" with the cursor between them, so a ghost drawn past the whole
  ;; buffer lands OUTSIDE the closer ("> ()+ 1 2)") instead of within it
  ;; ("> (+ 1 2)").
  ;;
  ;; The ghost shows only when the cursor sits at the end of the TYPED region --
  ;; the real end-of-buffer (after = "") or just before paredit's auto-inserted
  ;; trailing closers (after is all closers/quotes) -- and only the part of it that
  ;; is not already on the screen is drawn: ghost-visible-suffix elides the
  ;; trailing run the suggestion shares with the after-cursor text, so the closer
  ;; the buffer is already showing is not drawn a second time ("> (+ 1 2))").  A
  ;; ghost that elides away to nothing draws nothing.
  ;;
  ;; The buffer past the cursor goes through the SAME colouriser as the buffer
  ;; before it, so those closers keep their own paren colour and are never mistaken
  ;; for ghost text.  When the after-cursor text is empty that second piece emits
  ;; nothing and the ghost lands exactly where it always did, so the end-of-buffer
  ;; frame is byte-identical to the one this renderer drew before.
  (define render-line/suggestion
    (case-lambda
      [(port prompt gb prev-lines suggestion)
       (render-line/suggestion port prompt gb prev-lines suggestion 0)]
      [(port prompt gb prev-lines suggestion term-cols)
       (let* ([geom (render-prologue port prompt gb term-cols)]
              [before (render-geom-before geom)]
              [after (render-geom-after geom)]
              [text (render-geom-text geom)]
              ;; cursor-pos IS the split index into text: it is the length of
              ;; before, so the two draw windows are [0,cursor-pos) and
              ;; [cursor-pos,len).
              [cursor-pos (render-geom-cursor-pos geom)]
              [text-len (string-length text)]
              [prompt-width (render-geom-prompt-width geom)]
              [total-lines (render-geom-total-lines geom)]
              [cursor-row (render-geom-cursor-row geom)]
              [cursor-col (render-geom-cursor-col geom)]
              [colour? (render-geom-colour? geom)]
              [ansi? (render-geom-ansi? geom)]
              ;; The part of the suggestion actually drawn: the whole of it at
              ;; end-of-buffer, less whatever trailing closers the buffer is
              ;; already showing past the cursor.
              [ghost-visible (ghost-visible-suffix suggestion after)]
              ;; Is a ghost actually drawn?  Only when colour is live, the cursor
              ;; sits at the end of the typed region, and something survives the
              ;; elision.  This mirrors the draw gate below, so the row count and
              ;; the draw stay in lock-step.
              [ghost-shown? (and colour?
                                 (only-closing-delimiters? after)
                                 (> (string-length ghost-visible) 0))]
              ;; Total screen rows drawn.  Measured over the text that is really
              ;; drawn -- before ++ ghost ++ after -- through the SAME wrap-aware
              ;; helper the edit line uses, so the ghost's own wrapped and newlined
              ;; rows are counted and the climb-back below lands the cursor on the
              ;; user's typing row rather than stranding it on the ghost's bottom
              ;; row.  Counting the suggestion WHOLE (rather than its visible part)
              ;; would over-count the moment an elision happened.
              [drawn-lines (if ghost-shown?
                               (count-visual-lines
                                 prompt-width
                                 (string-append before ghost-visible after)
                                 term-cols)
                               total-lines)]
              ;; Tokenise the buffer ONCE; both windows read the same token list,
              ;; so each is coloured in the context of the whole line.  Lex it once
              ;; too and settle the shell-highlight decision here, so both draw
              ;; windows below agree on whether — and how — to colour the head.
              [cls (classify-input text)]
              [scheme? (eq? cls 'scheme)]
              [tokens (if scheme? (tokenize text) '())]
              [spans (shell-lex text)]
              [decision (call-with-values
                          (lambda () (shell-highlight-decision text spans cls))
                          cons)]
              [shell-hl? (car decision)]
              [head-ok? (cdr decision)])
         (when (and ansi? (> prev-lines 0))
           (display "\x1b;[" port)
           (display prev-lines port)
           (display "A" port))
         (when ansi?
           (display "\r\x1b;[J" port))
         (display prompt port)
         ;; The buffer up to the cursor.
         (cond
           [shell-hl?
            (display-shell-colourised/window port text spans head-ok? colour?
                                             0 cursor-pos)]
           [scheme?
            (display-colourised/window port text tokens cursor-pos colour?
                                       0 cursor-pos)]
           [else (display (substring text 0 cursor-pos) port)])
         ;; The dim grey ghost, AT the cursor.  Gated exactly as drawn-lines above
         ;; (ghost-shown?), so a plain sink still emits zero escapes and no ghost.
         (when ghost-shown?
           (display "\x1b;[38;5;240m" port)  ; dim grey
           (display ghost-visible port)
           (display "\x1b;[39m" port))
         ;; The rest of the buffer -- paredit's auto-inserted closers -- in its own
         ;; colour, never in the ghost's grey.  Empty at end-of-buffer.
         (cond
           [shell-hl?
            (display-shell-colourised/window port text spans head-ok? colour?
                                             cursor-pos text-len)]
           [scheme?
            (display-colourised/window port text tokens cursor-pos colour?
                                       cursor-pos text-len)]
           [else (display (substring text cursor-pos text-len) port)])
         ;; Position cursor: climb back over every drawn row (the ghost's rows
         ;; included) to the user's logical typing row, then to the column.  Both
         ;; are measured from `before` alone, which is still drawn first.
         (when ansi?
           (let ([lines-after-cursor (- drawn-lines cursor-row)])
             (when (> lines-after-cursor 0)
               (display "\x1b;[" port)
               (display lines-after-cursor port)
               (display "A" port)))
           (display "\x1b;[" port)
           (display cursor-col port)
           (display "G" port))
         (flush-output-port port)
         cursor-row)]))

  ;; render-line/selection: like render-line but reverse-video-highlights an
  ;; active selection span and, optionally, draws a compact mode indicator on its
  ;; own row below the edit line.  sel-range is a (start . end) index pair into
  ;; text = before ++ after (the same coordinate space as gap-buffer-cursor-pos)
  ;; or #f; indicator is a short status string (e.g. "-- VISUAL --") or #f.  The
  ;; two existing render-line / render-line/suggestion arities are untouched --
  ;; this is a parallel entry point.  Like render-line/suggestion it folds any
  ;; indicator row into the climb-back so the cursor lands on the logical typing
  ;; row, and it returns that same row (identical to render-line) so prev-lines
  ;; tracking is unchanged.  Off a terminal (ansi?/colour? #f) neither the
  ;; highlight nor the indicator is drawn and no escape bytes are emitted.
  (define render-line/selection
    (case-lambda
      [(port prompt gb prev-lines sel-range indicator)
       (render-line/selection port prompt gb prev-lines sel-range indicator 0)]
      [(port prompt gb prev-lines sel-range indicator term-cols)
       (let* ([geom (render-prologue port prompt gb term-cols)]
              [text (render-geom-text geom)]
              [cursor-pos (render-geom-cursor-pos geom)]
              [prompt-width (render-geom-prompt-width geom)]
              [total-lines (render-geom-total-lines geom)]
              [cursor-row (render-geom-cursor-row geom)]
              [cursor-col (render-geom-cursor-col geom)]
              [colour? (render-geom-colour? geom)]
              [ansi? (render-geom-ansi? geom)]
              ;; Is the mode indicator actually drawn?  Only on a terminal
              ;; (ansi?) with a non-empty string.  This mirrors the draw gate
              ;; below so the row count and the draw stay in lock-step.
              [indicator-shown? (and ansi?
                                     (string? indicator)
                                     (> (string-length indicator) 0))]
              ;; Total screen rows drawn.  When the indicator shows, its own
              ;; row (plus any wrap) counts too: measure the buffer text, a
              ;; separating newline, and the indicator through the SAME wrap-
              ;; aware helper the ghost uses, so the cursor climbs back to the
              ;; user's typing row rather than stranding on the indicator row.
              [drawn-lines (if indicator-shown?
                               (count-visual-lines prompt-width
                                                   (string-append text "\n" indicator)
                                                   term-cols)
                               total-lines)])
         (when (and ansi? (> prev-lines 0))
           (display "\x1b;[" port)
           (display prev-lines port)
           (display "A" port))
         (when ansi?
           (display "\r\x1b;[J" port))
         (display prompt port)
         ;; Draw the buffer text: reverse-video the selection span when one is
         ;; given, otherwise the usual colourised (Scheme) / plain path.
         (if (pair? sel-range)
             (display-with-selection port text sel-range colour?)
             (let* ([spans (shell-lex text)]
                    [cls (classify-input text)])
               (let-values ([(hl? head-ok?) (shell-highlight-decision text spans cls)])
                 (cond
                   [hl?
                    (display-shell-colourised/window port text spans head-ok? colour?
                                                     0 (string-length text))]
                   [(eq? cls 'scheme)
                    (let ([tokens (tokenize text)])
                      (display-colourised port text tokens cursor-pos colour?))]
                   [else (display text port)]))))
         ;; Drop to a new row and draw the compact mode indicator, dimmed.
         ;; Gated exactly as drawn-lines above, so a plain sink emits zero
         ;; escapes and no indicator.
         (when indicator-shown?
           (display "\n" port)
           (display "\x1b;[2m" port)    ; dim on
           (display indicator port)
           (display "\x1b;[22m" port))  ; dim off
         ;; Position cursor: climb back over every drawn row (the indicator's
         ;; row included) to the logical typing row, then to the column.
         (when ansi?
           (let ([lines-after-cursor (- drawn-lines cursor-row)])
             (when (> lines-after-cursor 0)
               (display "\x1b;[" port)
               (display lines-after-cursor port)
               (display "A" port)))
           (display "\x1b;[" port)
           (display cursor-col port)
           (display "G" port))
         (flush-output-port port)
         cursor-row)]))

  ;; ======================================================================
  ;; flash-matching-paren
  ;; ======================================================================

  ;; The flash is a pure cursor-save/reverse-video/cursor-restore effect, so the
  ;; whole thing (including the pause and the clearing re-render) only runs on a
  ;; terminal target; a non-terminal target has nothing to flash and stays silent.
  (define (flash-matching-paren port prompt gb match-idx prev-lines)
    (when (ansi-ok? port)
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
        (render-line port prompt gb prev-lines))))

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
              [overflow (> n max-items)]
              ;; Colour the selection only on a colour-capable target; the list
              ;; itself (indents, candidates, overflow line) always renders.
              [colour? (colour-ok? port)])
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
                (when (and colour? (= i selected-index))
                  (display "\x1b;[7m" port))
                (display "  " port)
                (display candidate port)
                (when (and colour? (= i selected-index))
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
              [overflow (> n max-items)]
              [colour? (colour-ok? port)])
         (display "\n" port)
         ;; Match count indicator
         (when colour? (display "\x1b;[38;5;240m" port))  ; dim grey
         (display "  " port)
         (if (>= selected-index 0)
             (begin (display (+ selected-index 1) port)
                    (display "/" port)
                    (display n port))
             (begin (display n port)
                    (display " match" port)
                    (when (not (= n 1)) (display "es" port))))
         (when colour? (display "\x1b;[39m" port))
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
                (when (and colour? (= i selected-index))
                  (display "\x1b;[7m" port))
                (display "  " port)
                (display-with-highlights port candidate positions colour?)
                ;; Show description in dim grey after candidate
                (when (and description (string? description))
                  (when (and colour? (not (= i selected-index)))
                    (display "\x1b;[38;5;240m" port))
                  (display "  " port)
                  (display description port)
                  (when (and colour? (not (= i selected-index)))
                    (display "\x1b;[39m" port)))
                (when (and colour? (= i selected-index))
                  (display "\x1b;[0m" port))
                (display "\n" port)
                (loop (+ i 1) (+ lines 1)))])))]))

  ;; Display a string with certain character positions highlighted (bold+underline).
  ;; The optional trailing colour? (default #t) suppresses the highlight escapes
  ;; when #f, so the string still renders as plain text.
  (define display-with-highlights
    (case-lambda
      [(port str positions)
       (display-with-highlights port str positions #t)]
      [(port str positions colour?)
       (let ([len (string-length str)]
             [pos-set (make-eq-hashtable)])
         ;; Build a set of positions for O(1) lookup
         (for-each (lambda (p) (hashtable-set! pos-set p #t)) positions)
         (let loop ([i 0] [in-highlight? #f])
           (when (< i len)
             (let ([want-hl? (hashtable-ref pos-set i #f)])
               (cond
                 [(and want-hl? (not in-highlight?))
                  (when colour? (display "\x1b;[1;4m" port))  ; bold + underline
                  (display (string-ref str i) port)
                  (loop (+ i 1) #t)]
                 [(and (not want-hl?) in-highlight?)
                  (when colour? (display "\x1b;[22;24m" port))  ; reset bold + underline
                  (display (string-ref str i) port)
                  (loop (+ i 1) #f)]
                 [else
                  (display (string-ref str i) port)
                  (loop (+ i 1) in-highlight?)]))))
         ;; Reset if we ended in highlight
         (when (and colour?
                    (let loop ([i 0] [last #f])
                      (cond [(>= i len) last]
                            [(hashtable-ref pos-set i #f) (loop (+ i 1) #t)]
                            [else (loop (+ i 1) #f)])))
           (display "\x1b;[22;24m" port)))]))

  ;; Display text with the half-open span [start,end) shown in reverse video.
  ;; sel-range is a (start . end) integer pair (indices into text) or #f.  The
  ;; span is clamped defensively — start is pinned into [0,len] and end into
  ;; [start,len] — so an out-of-range or inverted range can never index past the
  ;; string or invert; an empty span, a #f range, or colour? #f renders the text
  ;; verbatim with zero escape bytes.  colour? is an explicit trailing argument
  ;; (default #t), threaded by the caller rather than derived from the port, so
  ;; the highlight is assertable on a plain string port.  Each character of text
  ;; is displayed exactly once, in order.  The span is closed with the narrower
  ;; [27m reset, which clears only reverse video and leaves any other active
  ;; attributes intact -- unlike render-completion-menu and flash-matching-paren,
  ;; which close their reverse video with the full [0m reset.
  (define display-with-selection
    (case-lambda
      [(port text sel-range)
       (display-with-selection port text sel-range #t)]
      [(port text sel-range colour?)
       (let ([len (string-length text)])
         (if (and colour? (pair? sel-range))
             (let* ([s (max 0 (min (car sel-range) len))]
                    [e (max s (min (cdr sel-range) len))])
               (if (< s e)
                   ;; Walk char by char, toggling reverse video on the span edges.
                   (let loop ([i 0] [in-sel? #f])
                     (cond
                       [(>= i len)
                        ;; Span ran to the end of the string: close it off.
                        (when in-sel? (display "\x1b;[27m" port))]
                       [(and (= i s) (not in-sel?))
                        (display "\x1b;[7m" port)          ; reverse video on
                        (display (string-ref text i) port)
                        (loop (+ i 1) #t)]
                       [(and (= i e) in-sel?)
                        (display "\x1b;[27m" port)         ; reverse video off
                        (display (string-ref text i) port)
                        (loop (+ i 1) #f)]
                       [else
                        (display (string-ref text i) port)
                        (loop (+ i 1) in-sel?)]))
                   ;; Empty span after clamping: nothing to highlight.
                   (display text port)))
             ;; No colour, or no range: verbatim, zero escape bytes.
             (display text port)))]))

  ;; ======================================================================
  ;; render-completion-grid — fish/zsh-style multi-column completion
  ;; ======================================================================

  ;; Selection colours (muted indigo background, light text)
  (define sel-bg "\x1b;[48;2;56;62;87m")
  (define sel-fg "\x1b;[38;2;195;202;230m")

  ;; Truncate string to max display width, appending … if truncated.
  (define (truncate-display str max-w)
    (let ([len (string-length str)])
      (let lp ([i 0] [w 0])
        (cond
          [(>= i len) str]
          [else
           (let ([cw (char-display-width (string-ref str i))])
             (if (> (+ w cw) (- max-w 1))
                 (string-append (substring str 0 i) "\x2026;")
                 (lp (+ i 1) (+ w cw))))]))))

  ;; Pad string to exactly target display width with spaces.
  (define (pad-to-width str target-w)
    (let ([w (string-display-width str)])
      (if (>= w target-w)
          str
          (string-append str (make-string (- target-w w) #\space)))))

  ;; Sanitise one completion entry for DISPLAY: strip terminal escapes and stray
  ;; control bytes from the candidate name and, on a triple, its description,
  ;; keeping the entry's (name . positions) or (name positions description) shape.
  ;; This is the single choke-point at which menu text is sanitised -- the menu is
  ;; where a crafted name could repaint the screen -- while insertion upstream
  ;; keeps the original bytes, so a completed filename stays byte-exact.  The match
  ;; positions ride through unchanged: on the ordinary escape-free name the strip
  ;; is a no-op and the offsets stay valid; on a control-bearing name the offsets
  ;; were meaningless already and display-with-highlights is bounds-safe.
  (define (strip-entry-for-display e)
    (let ([name (strip-terminal-escapes (car e))])
      (if (and (pair? (cdr e)) (pair? (cddr e)) (list? (cadr e)))
          (list name (cadr e)
                (let ([d (caddr e)])
                  (if (and d (string? d)) (strip-terminal-escapes d) d)))
          (cons name (cdr e)))))

  ;; ======================================================================
  ;; === Grouped completion grid ===
  ;; ======================================================================

  ;; Screen-row -> header label (a string) or #f, laid out to match the row-plan.
  ;; The k-th header row in draw order shows the k-th string label of the group-
  ;; plan, so the labels ride the SAME order build-row-plan placed the header rows
  ;; in -- the renderer never re-derives the geometry, it reads the header rows
  ;; back off the shared plan (a col-0 cell of #f marks a header) and pairs each
  ;; with the next label.  A candidate row carries #f.
  (define (grid-row-labels plan group-plan)
    (let ([nrows (row-plan-rows plan)]
          [labels (filter string? (map car group-plan))]
          [v (make-vector (row-plan-rows plan) #f)])
      (let loop ([r 0] [ls labels])
        (cond
          [(fx>= r nrows) v]
          ;; A candidate row: col 0 is always filled, so row-plan-cell answers a
          ;; flat index (truthy, 0 included); a header row answers #f.
          [(row-plan-cell plan r 0) (loop (fx+ r 1) ls)]
          [(pair? ls) (vector-set! v r (car ls)) (loop (fx+ r 1) (cdr ls))]
          [else (loop (fx+ r 1) ls)]))))

  ;; Fish-style grid completion renderer, now group-aware.
  ;; entries: list of (candidate positions [description]); GROUP-PLAN (6-arg form)
  ;; is a list of (label . count) in draw order -- a string label draws a dim
  ;; header row, a #f label draws none.  The 5-arg form keeps every existing caller
  ;; and test byte-identical: it delegates with a single unlabelled group, so no
  ;; header is drawn and the layout is exactly as before.  index<->cell and the row
  ;; count come from the shared row-plan, so this renderer and its geometry mirrors
  ;; (grid-dimensions, drawn-rows, navigate) cannot drift once a header splits a row.
  ;; Returns (values menu-lines grid-cols grid-rows scroll-offset); grid-rows counts
  ;; header rows so the pager and the caller's geometry stay honest.
  (define render-completion-grid
    (case-lambda
      [(port entries selected-index term-cols max-visible)
       (render-completion-grid port entries selected-index term-cols max-visible #f #f)]
      [(port entries selected-index term-cols max-visible group-plan)
       (render-completion-grid port entries selected-index term-cols max-visible group-plan #f)]
      [(port entries selected-index term-cols max-visible group-plan types)
       (let* ([entries (map strip-entry-for-display entries)]
              ;; === LS_COLORS candidate colouring ===
              ;; Resolve the type/extension colour table ONCE per render (never per
              ;; cell) and turn the parallel type list into a vector for O(1) lookup
              ;; by flat index.  types is #f from the shorter clauses, in which case
              ;; every candidate resolves as the plain-file colour.  The renderer
              ;; never stats: the type is the one the completion source threaded.
              [ls-table (current-ls-colors)]
              [types-vec (and (list? types) (list->vector types))]
              [n (length entries)]
              ;; Extract candidate names and check for descriptions
              [names (map car entries)]
              [has-descs? (and (pair? entries)
                              (pair? (cdr (car entries)))
                              (pair? (cddr (car entries)))
                              (list? (cadar entries)))]
              [descs (if has-descs? (map caddr entries) '())]
              ;; Compute max candidate display width
              [max-name-w (fold-left
                            (lambda (mx s) (max mx (string-display-width s)))
                            0 names)]
              ;; Compute max description display width
              [max-desc-w (if has-descs?
                             (fold-left
                               (lambda (mx d)
                                 (if (and d (string? d))
                                     (max mx (string-display-width d))
                                     mx))
                               0 descs)
                             0)]
              ;; Column width: 2 indent + name + 2 gap (between cols)
              ;; With descriptions: single column, name + 2 + desc
              [cell-width (if has-descs?
                             term-cols  ; single column when descriptions present
                             (+ 2 (min max-name-w (quotient term-cols 2)) 2))]
              ;; Grid columns
              [grid-cols (if has-descs?
                            1
                            (max 1 (quotient term-cols cell-width)))]
              ;; The shared row-plan is the ONE geometry authority: header and
              ;; candidate rows laid out once, so index<->cell is a lookup here and
              ;; in every mirror.  A #f group-plan is a single unlabelled group --
              ;; no header, and the layout stays byte-identical to the old grid.
              [plan (build-row-plan (or group-plan (list (cons #f n))) grid-cols)]
              [grid-rows (row-plan-rows plan)]           ; header rows INCLUDED
              [row-labels (grid-row-labels plan (or group-plan (list (cons #f n))))]
              ;; Selected row: look it up in the plan, never quotient over a
              ;; header-free grid, so the highlight tracks across a header boundary.
              [sel-row (if (and (>= selected-index 0) (< selected-index n))
                           (let-values ([(r c) (row-plan-locate plan selected-index)])
                             r)
                           0)]
              ;; Scroll offset: keep selected row visible
              [scroll-off (cond
                            [(< sel-row max-visible)
                             0]
                            [else
                             (- sel-row (- max-visible 1))])]
              [vis-rows (min grid-rows max-visible)]
              [name-col-w (min max-name-w (- (if has-descs?
                                                 (- term-cols 6)
                                                 (quotient term-cols 2))
                                             0))]
              ;; Colour the selection, directory names, descriptions, header and
              ;; pager on a colour-capable target only; the grid layout -- headers
              ;; included -- always renders (structure, not decoration).
              [colour? (colour-ok? port)])

         ;; Output the grid
         (display "\n" port)
         (let row-loop ([row scroll-off] [lines 0])
           (cond
             [(or (>= row (+ scroll-off vis-rows)) (>= row grid-rows))
              ;; Pager indicator if scrolled
              (let ([total-lines
                     (if (> grid-rows vis-rows)
                         (begin
                           (when colour? (display "\x1b;[38;5;240m" port))
                           (display "  " port)
                           (display (+ sel-row 1) port)
                           (display "/" port)
                           (display grid-rows port)
                           (display " rows" port)
                           (when colour? (display "\x1b;[39m" port))
                           (display "\n" port)
                           (+ lines 1))
                         lines)])
                (values total-lines grid-cols grid-rows scroll-off))]
             [(vector-ref row-labels row)
              ;; A header row: a dim, left-aligned label on its own row (no
              ;; candidate indent, no blank separator), truncated to the terminal
              ;; width with the ellipsis.  The dim SGR is the only colour; the label
              ;; glyphs render regardless so the grouping survives NO_COLOR/dumb.
              ;; The label is re-stripped here -- the single display choke-point --
              ;; so a crafted label can never repaint the screen even off the sink.
              => (lambda (label)
                   (when colour? (display "\x1b;[2m" port))
                   (display (truncate-display (strip-terminal-escapes label) term-cols)
                            port)
                   (when colour? (display "\x1b;[22m" port))
                   (display "\n" port)
                   (row-loop (+ row 1) (+ lines 1)))]
             [else
              ;; A candidate row: walk its cells off the plan.  row-plan-cell answers
              ;; #f past the row's last cell, so a ragged final row stops early and a
              ;; full row stops at grid-cols -- the old (>= col grid-cols)/(>= idx n).
              (let col-loop ([col 0])
                (let ([idx (row-plan-cell plan row col)])
                  (if (not idx)
                      ;; End of row — newline and proceed to next row
                      (begin
                        (display "\n" port)
                        (row-loop (+ row 1) (+ lines 1)))
                      ;; Render one cell
                      (let* ([entry (list-ref entries idx)]
                             [candidate (car entry)]
                             [positions (if (and (pair? (cdr entry))
                                                (pair? (cddr entry))
                                                (list? (cadr entry)))
                                           (cadr entry)
                                           (cdr entry))]
                             [description (if (and (pair? (cdr entry))
                                                  (pair? (cddr entry))
                                                  (list? (cadr entry)))
                                             (caddr entry)
                                             #f)]
                             [selected? (= idx selected-index)]
                             ;; The candidate's colour: its threaded file type plus
                             ;; its own display name (for the *.ext match), resolved
                             ;; to a VALIDATED SGR string or #f.  A selected cell keeps
                             ;; the selection colour and takes none.  candidate-sgr
                             ;; allow-lists to [0-9;], so a crafted $LS_COLORS value
                             ;; resolves to #f and paints nothing.
                             [cand-type (and types-vec (< idx (vector-length types-vec))
                                             (vector-ref types-vec idx))]
                             [cand-sgr (and colour? (not selected?)
                                            (candidate-sgr ls-table candidate cand-type))]
                             [disp-name (truncate-display candidate name-col-w)]
                             [padded (if has-descs?
                                        (pad-to-width disp-name (+ name-col-w 2))
                                        (pad-to-width disp-name (- cell-width 2)))])
                        ;; Selected background
                        (when (and colour? selected?)
                          (display sel-bg port) (display sel-fg port))
                        ;; Indent
                        (display "  " port)
                        ;; Candidate colour from $LS_COLORS (non-selected cells only):
                        ;; wrap the name in the VALIDATED param string -- never a raw
                        ;; env byte -- so a hostile value cannot smuggle an escape.
                        (when cand-sgr
                          (display "\x1b;[" port) (display cand-sgr port) (display "m" port))
                        ;; Candidate with highlights
                        (display-with-highlights port padded positions colour?)
                        ;; Reset the candidate colour: a non-selected cell has no other
                        ;; active attribute, so [0m clears any bold the SGR set.
                        (when cand-sgr (display "\x1b;[0m" port))
                        ;; Description (single-column mode)
                        (when (and has-descs? description (string? description))
                          (when colour?
                            (if selected?
                                (display "\x1b;[38;2;140;145;170m" port)
                                (display "\x1b;[38;5;240m" port)))
                          (display (truncate-display description
                                     (max 10 (- term-cols name-col-w 6))) port)
                          (when colour?
                            (if selected?
                                (display sel-fg port)
                                (display "\x1b;[39m" port))))
                        ;; Reset selection
                        (when (and colour? selected?) (display "\x1b;[0m" port))
                        (col-loop (+ col 1))))))]))) ]))

  ;; ======================================================================
  ;; Reusable terminal-overlay helpers
  ;; ======================================================================

  ;; A minimal overlay vocabulary: draw a span of rows at an anchor relative to
  ;; the edit line, and clean-clear a tracked span before the next repaint.  Both
  ;; move with RELATIVE motion only (ESC[<n>A / ESC[<n>B, a carriage return, and
  ;; clear-to-end-of-screen) and never a cursor save/restore (ESC7/ESC8): an
  ;; absolute save cannot survive the screen scrolling when a span is painted near
  ;; the terminal bottom, so a later restore lands too low and the old paint
  ;; stacks.  Climbing back over a tracked row count relatively always lands true.
  ;; ansi? is an explicit argument (not derived from the port) so the escape
  ;; geometry is assertable on a plain string port and a non-terminal sink can
  ;; force the zero-escape path.  This is an overlay primitive, deliberately not a
  ;; window manager.

  ;; overlay-clear!: wipe a tracked N-row overlay span so nothing stacks on the
  ;; next repaint.  When ansi? is true and rows > 0, climb relatively over the
  ;; rows drawn, return to column 0, and clear to end of screen; otherwise emit
  ;; nothing, so a non-terminal sink (or an empty span) stays byte-for-byte empty.
  ;; Modelled on render-line's own move-up-then-wipe clear idiom.
  (define (overlay-clear! port rows ansi?)
    (when (and ansi? (> rows 0))
      (display "\x1b;[" port)
      (display rows port)
      (display "A" port)
      (display "\r\x1b;[J" port)))

  ;; overlay-draw: paint a span anchored to the edit line, then leave the cursor
  ;; back on the edit row.  place is 'down for a dropdown (below the edit line) or
  ;; 'up for a drop-up (above it, chosen when the room below is too small).
  ;; offset is the number of screen rows between the edit cursor and the anchor:
  ;; for a dropdown, the rows below the cursor down to the bottom of the (possibly
  ;; wrapped) edit line; for a drop-up, the rows to climb above the edit line to
  ;; open the span.  draw-thunk paints the span (in production a thunk that calls
  ;; render-completion-grid) and returns the row count it drew; overlay-draw
  ;; returns that same count.  The thunk opens one leading transition row before
  ;; its content, so the net descent below the edit cursor is offset + rows + 1,
  ;; and the return climb reverses exactly that — all relative, no save/restore.
  ;; On a non-terminal target (ansi? #f) the thunk still runs for its content and
  ;; return value, but overlay-draw emits no motion escapes of its own.
  (define (overlay-draw port place offset ansi? draw-thunk)
    ;; Signed displacement to the anchor, positive downward: a dropdown descends,
    ;; a drop-up climbs.
    (let ([anchor (if (eq? place 'up) (- offset) offset)])
      (when ansi?
        (cond
          [(> anchor 0)
           (display "\x1b;[" port) (display anchor port) (display "B" port)]
          [(< anchor 0)
           (display "\x1b;[" port) (display (- anchor) port) (display "A" port)])
        (display "\r" port))
      (let* ([rows (draw-thunk)]
             ;; Rows below the edit cursor once the thunk's own descent (its
             ;; content rows plus the single leading transition row) is added to
             ;; the anchor move.
             [net (+ anchor rows 1)])
        (when (and ansi? (not (= net 0)))
          (if (> net 0)
              (begin (display "\x1b;[" port) (display net port) (display "A" port))
              (begin (display "\x1b;[" port) (display (- net) port) (display "B" port))))
        rows)))

) ; end library
