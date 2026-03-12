;;; (hafod finder) -- Full-screen fuzzy finder (fzf-style TUI)
;;; Blocking sub-loop on alternate screen with buffered rendering,
;;; match highlighting, keyboard navigation, and SIGWINCH resize handling.
;;; Faithfully reproduces fzf's dark256 colour scheme and bottom-up layout.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod finder)
  (export run-finder fuzzy-select)
  (import (chezscheme)
          (only (hafod fuzzy) filter-search-pattern/positions)
          (only (hafod editor input-decode)
                read-key-event key-event? key-event-type key-event-value key-event-mods
                char-display-width string-display-width
                MOD_CTRL MOD_ALT)
          (only (hafod editor editor) with-raw-mode)
          (only (hafod editor render)
                tokenize
                paren-colours num-paren-colours
                string-colour comment-colour number-colour boolean-colour
                fg-colour fg-colour-l
                ident-colour-from-hash ident-hash)
          (only (hafod interactive) query-terminal-size)
          (only (hafod posix) SIGWINCH))

  ;; ======================================================================
  ;; fzf dark256 colour scheme (256-colour escapes)
  ;; ======================================================================
  ;; Source: github.com/junegunn/fzf src/tui/tui.go Dark256

  (define (fg256 n) (string-append "\x1b;[38;5;" (number->string n) "m"))
  (define (bg256 n) (string-append "\x1b;[48;5;" (number->string n) "m"))

  (define SGR-RESET    "\x1b;[0m")
  (define SGR-BOLD     "\x1b;[1m")
  (define SGR-DIM      "\x1b;[2m")

  ;; fzf dark256 palette
  (define COL-HL       (fg256 108))  ; match highlight (sage green)
  (define COL-FG+      (fg256 254))  ; selected line foreground (near-white)
  (define COL-BG+      (bg256 236))  ; selected line background (dark grey)
  (define COL-HL+      (fg256 151))  ; match highlight on selected (light green)
  (define COL-INFO     (fg256 144))  ; info/count (tan)
  (define COL-PROMPT   (fg256 110))  ; prompt (steel blue)
  (define COL-POINTER  (fg256 161))  ; pointer (hot pink)
  (define COL-SPINNER  (fg256 148))  ; spinner/separator fill

  ;; Pointer character: ▌ (U+258C LEFT HALF BLOCK) — same as fzf unicode default
  (define POINTER-CHAR "\x258c;")
  (define POINTER-WIDTH 1)  ; display width of ▌

  ;; Separator character: ─ (U+2500 BOX DRAWINGS LIGHT HORIZONTAL)
  (define SEPARATOR-CHAR "\x2500;")

  ;; ======================================================================
  ;; Finder state record
  ;; ======================================================================

  (define-record-type finder-state
    (fields
      items                    ;; vector of original items (strings)
      (mutable filtered)       ;; vector of (candidate . positions) pairs
      (mutable query)          ;; string: current search query
      (mutable cursor)         ;; integer: cursor position in query
      (mutable selected)       ;; integer: index of highlighted item in filtered list
      (mutable scroll-offset)  ;; integer: index of first visible item
      (mutable rows)           ;; integer: terminal rows
      (mutable cols)           ;; integer: terminal columns
      total-count              ;; integer: length of original items
      prompt                   ;; string: prompt text (e.g. "> ")
      display-items            ;; vector of flattened display strings
      colorize?))              ;; boolean: apply Scheme syntax colouring

  ;; ======================================================================
  ;; Multiline flattening
  ;; ======================================================================

  ;; Flatten a multiline string for single-line display.
  ;; Replaces newlines with ↵ and collapses continuation whitespace.
  (define (flatten-for-display str)
    (let ([len (string-length str)])
      (if (not (let lp ([i 0])
                 (cond [(fx>= i len) #f]
                       [(char=? (string-ref str i) #\newline) #t]
                       [else (lp (fx1+ i))])))
          str
          (let ([out (open-output-string)])
            (let lp ([i 0] [after-nl? #f])
              (cond
                [(fx>= i len) (get-output-string out)]
                [(char=? (string-ref str i) #\newline)
                 (display " \x21b5; " out)
                 (lp (fx1+ i) #t)]
                [(and after-nl? (char-whitespace? (string-ref str i)))
                 (lp (fx1+ i) after-nl?)]
                [else
                 (display (string-ref str i) out)
                 (lp (fx1+ i) #f)]))))))

  (define (build-display-items items)
    (let* ([len (vector-length items)]
           [disp (make-vector len)])
      (let lp ([i 0])
        (when (fx< i len)
          (vector-set! disp i (flatten-for-display (vector-ref items i)))
          (lp (fx1+ i))))
      disp))

  (define (display-version state candidate)
    (let ([disp (finder-state-display-items state)]
          [items (finder-state-items state)]
          [len (vector-length (finder-state-items state))])
      (let lp ([i 0])
        (cond
          [(fx>= i len) (flatten-for-display candidate)]
          [(eq? candidate (vector-ref items i)) (vector-ref disp i)]
          [else (lp (fx1+ i))]))))

  ;; ======================================================================
  ;; Alternate screen buffer
  ;; ======================================================================

  (define (with-alternate-screen thunk)
    (let ([out (console-output-port)])
      (dynamic-wind
        (lambda ()
          (display "\x1b;[?1049h" out)
          (display "\x1b;[?25l" out)
          (flush-output-port out))
        thunk
        (lambda ()
          (display "\x1b;[?25h" out)
          (display "\x1b;[?1049l" out)
          (flush-output-port out)))))

  ;; ======================================================================
  ;; Rendering — faithful fzf bottom-up layout
  ;; ======================================================================
  ;;
  ;; Bottom-up (fzf default --layout=default):
  ;;   Row N:       prompt + query
  ;;   Row N-1:     info line (count + separator)
  ;;   Row N-2:     best match (index 0) — nearest to prompt
  ;;   Row N-3:     next match (index 1)
  ;;   ...
  ;;   Row 1:       furthest match from prompt

  (define (truncate-to-width str max-width)
    (let ([len (string-length str)])
      (let lp ([i 0] [w 0])
        (cond
          [(fx>= i len) str]
          [else
           (let ([cw (char-display-width (string-ref str i))])
             (if (fx> (fx+ w cw) max-width)
                 (substring str 0 i)
                 (lp (fx1+ i) (fx+ w cw))))]))))

  (define (visible-rows state)
    (fxmax 1 (fx- (finder-state-rows state) 2)))

  (define (filtered-count state)
    (vector-length (finder-state-filtered state)))

  ;; Remap match positions from original string to flattened display string.
  (define (remap-positions original display-str positions)
    (if (eq? original display-str)
        positions
        (let* ([olen (string-length original)]
               [dlen (string-length display-str)]
               [map-vec (make-vector olen -1)])
          (let lp ([oi 0] [di 0] [after-nl? #f])
            (when (and (fx< oi olen) (fx< di dlen))
              (let ([och (string-ref original oi)])
                (cond
                  [(char=? och #\newline)
                   (lp (fx1+ oi) (fx+ di 3) #t)]
                  [(and after-nl? (char-whitespace? och))
                   (lp (fx1+ oi) di after-nl?)]
                  [else
                   (vector-set! map-vec oi di)
                   (lp (fx1+ oi) (fx1+ di) #f)]))))
          (filter (lambda (p) (fx>= p 0))
                  (map (lambda (p)
                         (if (fx< p olen) (vector-ref map-vec p) -1))
                       positions)))))

  ;; Display candidate text with fzf-style match highlighting.
  ;; on-selected? uses hl+ (151), otherwise hl (108).
  (define (display-candidate buf str positions on-selected?)
    (let ([len (string-length str)]
          [pos-set (make-eq-hashtable)]
          [hl-on  (string-append SGR-BOLD (if on-selected? COL-HL+ COL-HL))]
          [hl-off (if on-selected?
                      (string-append SGR-RESET COL-BG+ SGR-BOLD COL-FG+)
                      SGR-RESET)])
      (for-each (lambda (p) (hashtable-set! pos-set p #t)) positions)
      (let loop ([i 0] [in-hl? #f])
        (when (fx< i len)
          (let ([ch (string-ref str i)]
                [want? (hashtable-ref pos-set i #f)])
            (cond
              ;; ↵ symbol rendered in dim
              [(char=? ch #\x21b5)
               (when in-hl? (display hl-off buf))
               (display SGR-DIM buf)
               (display ch buf)
               (display SGR-RESET buf)
               (when on-selected?
                 (display (string-append COL-BG+ SGR-BOLD COL-FG+) buf))
               (loop (fx1+ i) #f)]
              [(and want? (not in-hl?))
               (display hl-on buf)
               (display ch buf)
               (loop (fx1+ i) #t)]
              [(and (not want?) in-hl?)
               (display hl-off buf)
               (display ch buf)
               (loop (fx1+ i) #f)]
              [else
               (display ch buf)
               (loop (fx1+ i) in-hl?)]))))
      (when (let loop ([i 0] [last #f])
              (cond [(fx>= i len) last]
                    [(hashtable-ref pos-set i #f) (loop (fx1+ i) #t)]
                    [else (loop (fx1+ i) #f)]))
        (display hl-off buf))))

  ;; Display candidate with Scheme syntax colouring + match highlighting.
  ;; Token-based foreground colours (rainbow parens, identifiers, strings, etc.)
  ;; with bold+underline overlay on matched positions.
  (define (display-candidate/syntax buf str positions on-selected?)
    (let* ([tokens (tokenize str)]
           [len (string-length str)]
           [pos-set (make-eq-hashtable)])
      (for-each (lambda (p) (hashtable-set! pos-set p #t)) positions)
      (for-each
        (lambda (tok)
          (let* ([type (car tok)]
                 [start (cadr tok)]
                 [end (caddr tok)]
                 [depth (cadddr tok)]
                 [span (substring str start end)])
            ;; Set syntax foreground for this token
            (case type
              [(paren)
               (fg-colour-l buf (vector-ref paren-colours
                                  (modulo depth num-paren-colours)))]
              [(atom)
               (fg-colour-l buf (ident-colour-from-hash (ident-hash span)))]
              [(number)  (fg-colour-l buf number-colour)]
              [(boolean) (fg-colour-l buf boolean-colour)]
              [(string)  (fg-colour-l buf string-colour)]
              [(comment) (fg-colour-l buf comment-colour)]
              [else (void)])  ; whitespace/other: default fg
            ;; Render characters with match position overlay
            (let char-loop ([i start] [in-hl? #f])
              (when (fx< i end)
                (let ([ch (string-ref str i)]
                      [want-hl? (hashtable-ref pos-set i #f)])
                  (cond
                    ;; ↵ symbol rendered dim
                    [(char=? ch #\x21b5)
                     (when in-hl? (display "\x1b;[22;24m" buf))
                     (display SGR-DIM buf)
                     (display ch buf)
                     (display SGR-RESET buf)
                     (when on-selected? (display COL-BG+ buf))
                     ;; Re-apply token colour for remaining chars
                     (case type
                       [(paren) (fg-colour-l buf (vector-ref paren-colours
                                                   (modulo depth num-paren-colours)))]
                       [(atom) (fg-colour-l buf (ident-colour-from-hash (ident-hash span)))]
                       [(number) (fg-colour-l buf number-colour)]
                       [(boolean) (fg-colour-l buf boolean-colour)]
                       [(string) (fg-colour-l buf string-colour)]
                       [(comment) (fg-colour-l buf comment-colour)]
                       [else (void)])
                     (char-loop (fx1+ i) #f)]
                    [(and want-hl? (not in-hl?))
                     (display "\x1b;[1;4m" buf)
                     (display ch buf)
                     (char-loop (fx1+ i) #t)]
                    [(and (not want-hl?) in-hl?)
                     (display "\x1b;[22;24m" buf)
                     (display ch buf)
                     (char-loop (fx1+ i) #f)]
                    [else
                     (display ch buf)
                     (char-loop (fx1+ i) in-hl?)]))))
            ;; Reset bold/underline and fg after each token
            (display "\x1b;[22;24;39m" buf)
            (when on-selected? (display COL-BG+ buf))))
        tokens)))

  (define (render-finder! state)
    (let* ([buf (open-output-string)]
           [rows (finder-state-rows state)]
           [cols (finder-state-cols state)]
           [filt (finder-state-filtered state)]
           [fcount (vector-length filt)]
           [sel (finder-state-selected state)]
           [scroll (finder-state-scroll-offset state)]
           [vis (visible-rows state)]
           [prompt (finder-state-prompt state)]
           [query (finder-state-query state)]
           [total (finder-state-total-count state)]
           [cand-bottom (fx- rows 2)]   ; row of best match
           [info-row (fx- rows 1)]      ; info/separator row
           [colorize? (finder-state-colorize? state)])

      ;; --- Candidate rows (bottom-up) ---
      ;; cand-bottom = index 0 (best match), going up for higher indices
      (let lp ([i 0])
        (when (fx< i vis)
          (let* ([screen-row (fx- cand-bottom i)]
                 [idx (fx+ scroll i)]
                 [selected? (fx= idx sel)])
            (display "\x1b;[" buf)
            (display screen-row buf)
            (display ";1H\x1b;[2K" buf)
            (if (fx< idx fcount)
                (let* ([entry (vector-ref filt idx)]
                       [original (car entry)]
                       [positions (cdr entry)]
                       [disp-str (display-version state original)]
                       [disp-positions (remap-positions original disp-str positions)]
                       [text-width (fx- cols 2)]  ; 2 cols for gutter (pointer area)
                       [truncated (truncate-to-width disp-str text-width)]
                       [tlen (string-length truncated)]
                       [vis-positions (filter (lambda (p) (fx< p tlen)) disp-positions)])
                  (if selected?
                      (begin
                        ;; Selected line: bg+ across entire line, pointer in gutter
                        (display COL-BG+ buf)
                        ;; Pad entire line with bg+
                        (let pad ([k 0])
                          (when (fx< k cols)
                            (display " " buf)
                            (pad (fx1+ k))))
                        ;; Reposition and draw content
                        (display "\x1b;[" buf)
                        (display screen-row buf)
                        (display ";1H" buf)
                        (display COL-BG+ buf)
                        ;; Gutter: pointer char
                        (display SGR-BOLD buf)
                        (display COL-POINTER buf)
                        (display COL-BG+ buf)
                        (display POINTER-CHAR buf)
                        ;; Space after pointer
                        (display " " buf)
                        ;; Candidate text in fg+ with hl+
                        (if colorize?
                            (begin
                              (display COL-BG+ buf)
                              (display-candidate/syntax buf truncated vis-positions #t))
                            (begin
                              (display SGR-BOLD buf)
                              (display COL-FG+ buf)
                              (display COL-BG+ buf)
                              (display-candidate buf truncated vis-positions #t)))
                        (display SGR-RESET buf))
                      (begin
                        ;; Normal line: 2 space gutter, default fg
                        (display "  " buf)
                        (if colorize?
                            (display-candidate/syntax buf truncated vis-positions #f)
                            (display-candidate buf truncated vis-positions #f))
                        (display SGR-RESET buf))))
                ;; Empty row (beyond results)
                (display SGR-RESET buf)))
          (lp (fx1+ i))))

      ;; --- Clear any remaining rows above candidates ---
      (let ([top-cand-row (fx- cand-bottom (fx1- (fxmin vis (fxmax fcount 1))))])
        (let lp ([r 1])
          (when (fx< r top-cand-row)
            (display "\x1b;[" buf)
            (display r buf)
            (display ";1H\x1b;[2K" buf)
            (lp (fx1+ r)))))

      ;; --- Info/separator line on row N-1 ---
      (display "\x1b;[" buf)
      (display info-row buf)
      (display ";1H\x1b;[2K" buf)
      ;; Match count
      (display "  " buf)
      (display COL-INFO buf)
      (display fcount buf)
      (display "/" buf)
      (display total buf)
      (display SGR-RESET buf)
      ;; Separator: ─ filling remaining width
      (let* ([count-str (string-append (number->string fcount) "/" (number->string total))]
             [used (fx+ 2 (string-length count-str) 1)]  ; "  " + count + " "
             [remaining (fxmax 0 (fx- cols used))])
        (display " " buf)
        (display COL-INFO buf)
        (let sep-lp ([k 0])
          (when (fx< k remaining)
            (display SEPARATOR-CHAR buf)
            (sep-lp (fx1+ k))))
        (display SGR-RESET buf))

      ;; --- Prompt + query on row N ---
      (display "\x1b;[" buf)
      (display rows buf)
      (display ";1H\x1b;[2K" buf)
      (display SGR-BOLD buf)
      (display COL-PROMPT buf)
      (display prompt buf)
      (display SGR-RESET buf)
      (display query buf)

      ;; --- Flush to terminal ---
      (let ([out (console-output-port)])
        (display (get-output-string buf) out)
        ;; Position cursor in query
        (let ([cursor-col (fx+ (string-display-width prompt)
                               (string-display-width
                                 (substring query 0 (finder-state-cursor state)))
                               1)])
          (display "\x1b;[" out)
          (display rows out)
          (display ";" out)
          (display cursor-col out)
          (display "H" out)
          (display "\x1b;[?25h" out))
        (flush-output-port out))))

  ;; ======================================================================
  ;; Query editing
  ;; ======================================================================

  (define (query-insert! state ch)
    (let* ([q (finder-state-query state)]
           [c (finder-state-cursor state)]
           [new-q (string-append (substring q 0 c)
                                 (string ch)
                                 (substring q c (string-length q)))])
      (finder-state-query-set! state new-q)
      (finder-state-cursor-set! state (fx1+ c))
      (refilter! state)))

  (define (query-delete-back! state)
    (let* ([q (finder-state-query state)]
           [c (finder-state-cursor state)])
      (when (fx> c 0)
        (let ([new-q (string-append (substring q 0 (fx1- c))
                                    (substring q c (string-length q)))])
          (finder-state-query-set! state new-q)
          (finder-state-cursor-set! state (fx1- c))
          (refilter! state)))))

  (define (query-delete-forward! state)
    (let* ([q (finder-state-query state)]
           [c (finder-state-cursor state)]
           [len (string-length q)])
      (when (fx< c len)
        (let ([new-q (string-append (substring q 0 c)
                                    (substring q (fx1+ c) len))])
          (finder-state-query-set! state new-q)
          (refilter! state)))))

  (define (query-clear! state)
    (let ([q (finder-state-query state)])
      (unless (string=? q "")
        (finder-state-query-set! state "")
        (finder-state-cursor-set! state 0)
        (refilter! state))))

  (define (query-delete-word-back! state)
    (let* ([q (finder-state-query state)]
           [c (finder-state-cursor state)])
      (when (fx> c 0)
        (let* ([i (let skip-ws ([j (fx1- c)])
                    (if (and (fx> j 0) (char-whitespace? (string-ref q j)))
                        (skip-ws (fx1- j))
                        j))]
               [i (let skip-word ([j i])
                    (if (and (fx> j 0) (not (char-whitespace? (string-ref q (fx1- j)))))
                        (skip-word (fx1- j))
                        j))])
          (let ([new-q (string-append (substring q 0 i)
                                      (substring q c (string-length q)))])
            (finder-state-query-set! state new-q)
            (finder-state-cursor-set! state i)
            (refilter! state))))))

  ;; ======================================================================
  ;; Re-filtering
  ;; ======================================================================

  (define (refilter! state)
    (let* ([q (finder-state-query state)]
           [items (finder-state-items state)]
           [candidates (vector->list items)]
           [results (filter-search-pattern/positions q candidates)]
           [new-filtered (list->vector results)])
      (finder-state-filtered-set! state new-filtered)
      (finder-state-selected-set! state 0)
      (finder-state-scroll-offset-set! state 0)))

  ;; ======================================================================
  ;; Navigation
  ;; ======================================================================
  ;; In bottom-up layout, visual UP = increase index, visual DOWN = decrease.
  ;; nav-up!/nav-down! are INDEX-based; the event loop swaps them for visual direction.

  (define (nav-prev! state)
    (let ([sel (finder-state-selected state)])
      (when (fx> sel 0)
        (finder-state-selected-set! state (fx1- sel))
        (adjust-scroll! state))))

  (define (nav-next! state)
    (let* ([sel (finder-state-selected state)]
           [max-idx (fx1- (filtered-count state))])
      (when (fx< sel max-idx)
        (finder-state-selected-set! state (fx1+ sel))
        (adjust-scroll! state))))

  (define (nav-page-prev! state)
    (let* ([sel (finder-state-selected state)]
           [page (fxmax 1 (fx- (visible-rows state) 1))]
           [new-sel (fxmax 0 (fx- sel page))])
      (finder-state-selected-set! state new-sel)
      (adjust-scroll! state)))

  (define (nav-page-next! state)
    (let* ([sel (finder-state-selected state)]
           [max-idx (fx1- (filtered-count state))]
           [page (fxmax 1 (fx- (visible-rows state) 1))]
           [new-sel (fxmin max-idx (fx+ sel page))])
      (when (fx>= max-idx 0)
        (finder-state-selected-set! state new-sel)
        (adjust-scroll! state))))

  (define (nav-home! state)
    (finder-state-selected-set! state 0)
    (adjust-scroll! state))

  (define (nav-end! state)
    (let ([max-idx (fx1- (filtered-count state))])
      (when (fx>= max-idx 0)
        (finder-state-selected-set! state max-idx)
        (adjust-scroll! state))))

  (define (adjust-scroll! state)
    (let* ([sel (finder-state-selected state)]
           [scroll (finder-state-scroll-offset state)]
           [vis (visible-rows state)])
      (cond
        [(fx< sel scroll)
         (finder-state-scroll-offset-set! state sel)]
        [(fx>= sel (fx+ scroll vis))
         (finder-state-scroll-offset-set! state (fx+ 1 (fx- sel vis)))])))

  ;; ======================================================================
  ;; Event loop
  ;; ======================================================================

  (define (make-flag) (cons #f '()))
  (define (flag-set! f) (set-car! f #t))
  (define (flag-clear! f) (set-car! f #f))
  (define (flag-ref f) (car f))

  (define (finder-loop state resize-flag)
    (when (flag-ref resize-flag)
      (flag-clear! resize-flag)
      (let-values ([(new-rows new-cols) (query-terminal-size)])
        (finder-state-rows-set! state new-rows)
        (finder-state-cols-set! state new-cols)
        (adjust-scroll! state)))

    (render-finder! state)

    (let ([ev (read-key-event (console-input-port))])
      (cond
        [(eof-object? ev) #f]
        [(key-event? ev)
         (let ([type (key-event-type ev)]
               [val (key-event-value ev)]
               [mods (key-event-mods ev)])
           (cond
             [(and (eq? type 'special) (eq? val 'escape)) #f]
             [(and (eq? type 'ctrl) (eqv? val #\c)) #f]
             [(and (eq? type 'ctrl) (eqv? val #\g)) #f]
             ;; Enter -> accept (returns original string with newlines intact)
             [(and (eq? type 'special) (eq? val 'return))
              (let ([fcount (filtered-count state)])
                (if (fx> fcount 0)
                    (car (vector-ref (finder-state-filtered state)
                                     (finder-state-selected state)))
                    #f))]
             ;; fzf bottom-up: UP arrow = visual up = next index (away from prompt)
             ;; DOWN arrow = visual down = prev index (towards prompt/best match)
             [(or (and (eq? type 'special) (eq? val 'up))
                  (and (eq? type 'ctrl) (eqv? val #\p))
                  (and (eq? type 'ctrl) (eqv? val #\k)))
              (nav-next! state)
              (finder-loop state resize-flag)]
             [(or (and (eq? type 'special) (eq? val 'down))
                  (and (eq? type 'ctrl) (eqv? val #\n))
                  (and (eq? type 'ctrl) (eqv? val #\j)))
              (nav-prev! state)
              (finder-loop state resize-flag)]
             [(and (eq? type 'special) (eq? val 'page-up))
              (nav-page-next! state)
              (finder-loop state resize-flag)]
             [(and (eq? type 'special) (eq? val 'page-down))
              (nav-page-prev! state)
              (finder-loop state resize-flag)]
             [(and (eq? type 'special) (eq? val 'home))
              (nav-home! state)
              (finder-loop state resize-flag)]
             [(and (eq? type 'special) (eq? val 'end))
              (nav-end! state)
              (finder-loop state resize-flag)]
             [(and (eq? type 'special) (eq? val 'backspace))
              (query-delete-back! state)
              (finder-loop state resize-flag)]
             [(and (eq? type 'special) (eq? val 'delete))
              (query-delete-forward! state)
              (finder-loop state resize-flag)]
             [(and (eq? type 'special) (eq? val 'left))
              (let ([c (finder-state-cursor state)])
                (when (fx> c 0)
                  (finder-state-cursor-set! state (fx1- c))))
              (finder-loop state resize-flag)]
             [(and (eq? type 'special) (eq? val 'right))
              (let ([c (finder-state-cursor state)]
                    [len (string-length (finder-state-query state))])
                (when (fx< c len)
                  (finder-state-cursor-set! state (fx1+ c))))
              (finder-loop state resize-flag)]
             [(and (eq? type 'ctrl) (eqv? val #\a))
              (finder-state-cursor-set! state 0)
              (finder-loop state resize-flag)]
             [(and (eq? type 'ctrl) (eqv? val #\e))
              (finder-state-cursor-set! state
                (string-length (finder-state-query state)))
              (finder-loop state resize-flag)]
             [(and (eq? type 'ctrl) (eqv? val #\u))
              (query-clear! state)
              (finder-loop state resize-flag)]
             [(and (eq? type 'ctrl) (eqv? val #\w))
              (query-delete-word-back! state)
              (finder-loop state resize-flag)]
             [(and (eq? type 'special) (eq? val 'tab))
              (finder-loop state resize-flag)]
             [(eq? type 'char)
              (query-insert! state val)
              (finder-loop state resize-flag)]
             [else
              (finder-loop state resize-flag)]))]
        [else
         (finder-loop state resize-flag)])))

  ;; ======================================================================
  ;; Public API
  ;; ======================================================================

  (define run-finder
    (case-lambda
      [(items prompt) (run-finder items prompt #f)]
      [(items prompt colorize?)
       (run-finder* items prompt colorize?)]))

  (define (run-finder* items prompt colorize?)
    (let-values ([(rows cols) (query-terminal-size)])
      (let* ([item-vec (list->vector items)]
             [disp-vec (build-display-items item-vec)]
             [init-filtered (list->vector (map (lambda (s) (cons s '())) items))]
             [state (make-finder-state item-vec init-filtered
                                       "" 0 0 0
                                       rows cols
                                       (length items) prompt
                                       disp-vec colorize?)]
             [resize-flag (make-flag)])
        (let ([saved-handler #f])
          (dynamic-wind
            (lambda ()
              (set! saved-handler
                (let ([old #f])
                  (register-signal-handler SIGWINCH
                    (lambda (sig) (flag-set! resize-flag)))
                  old)))
            (lambda ()
              (with-raw-mode 0
                (lambda ()
                  (with-alternate-screen
                    (lambda ()
                      (finder-loop state resize-flag))))))
            (lambda ()
              (register-signal-handler SIGWINCH (lambda (sig) (void)))))))))

  (define fuzzy-select
    (case-lambda
      [(items) (run-finder items "> ")]
      [(items prompt) (run-finder items prompt)]))

) ; end library
