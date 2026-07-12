;;; (hafod finder) -- Full-screen fuzzy finder (fzf-style TUI)
;;; Blocking sub-loop on alternate screen with buffered rendering,
;;; match highlighting, keyboard navigation, and SIGWINCH resize handling.
;;; Faithfully reproduces fzf's dark256 colour scheme and bottom-up layout.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod finder)
  (export run-finder fuzzy-select
          ;; Exposed for the PTY-free renderer tests (white-box): construct a
          ;; state, render to a string port, and drive the plain fallback.
          render-finder! make-finder-state run-finder/plain
          ;; Further white-box hooks for the incremental-narrowing and O(1)
          ;; display-lookup equivalence tests: drive one keystroke, read the
          ;; filtered vector back, and resolve a candidate's display string.
          query-insert! finder-state-filtered display-version)
  (import (chezscheme)
          (only (hafod fuzzy) filter-search-pattern/positions)
          (only (hafod srfi-13) string-prefix?)
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
          (only (hafod tty) refresh-terminal-size-cache! cached-terminal-size)
          (only (hafod posix) SIGWINCH)
          (only (hafod signal) with-signal-handler)
          (only (hafod terminal-caps) ansi-ok? colour-ok?)
          (only (hafod shell classifier) classify-input))

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

  ;; Emit an SGR/palette string only when colour output is permitted. When
  ;; colour? is #f every palette constant collapses to "" so the renderer emits
  ;; zero SGR bytes at source; the cursor/movement escapes are gated separately
  ;; (they only run once ansi-ok? has already admitted the full-screen TUI).
  (define (sgr colour? s) (if colour? s ""))

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
      colorize?                ;; #f or procedure: (string -> boolean) — true means syntax-colour
      show-numbers?            ;; boolean: show 1-based index numbers beside candidates
      colour?                  ;; boolean: colour-ok? of the output — gates every SGR/palette byte
      disp-map                 ;; eq-hashtable item->display-string (computed, not passed)
      idx-map)                 ;; eq-hashtable item->original 0-based index (computed, not passed)
    ;; The two trailing fields are computed once from items/display-items, so the
    ;; public constructor keeps exactly its 14 positional arguments (white-box
    ;; callers and test/test-finder.ss construct with those). disp-map backs the
    ;; O(1) display-version lookup; idx-map restores narrowing survivors to
    ;; original-corpus order so a narrowed re-score stays byte-identical to a full
    ;; one (fuzzy's stable score-tiebreak leaves ties in input order).
    (protocol
      (lambda (new)
        (lambda (items filtered query cursor selected scroll-offset rows cols
                 total-count prompt display-items colorize? show-numbers? colour?)
          (let ([dmap (make-eq-hashtable)]
                [imap (make-eq-hashtable)]
                [n (vector-length items)])
            (let lp ([i 0])
              (when (fx< i n)
                (hashtable-set! dmap (vector-ref items i) (vector-ref display-items i))
                (hashtable-set! imap (vector-ref items i) i)
                (lp (fx1+ i))))
            (new items filtered query cursor selected scroll-offset rows cols
                 total-count prompt display-items colorize? show-numbers? colour?
                 dmap imap))))))

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

  ;; Sentinel distinct from every stored display string, so an absent candidate
  ;; is told apart from a present one in a single hashtable probe.
  (define %display-absent (list 'absent))

  ;; Resolve a candidate's flattened display string in O(1) via the precomputed
  ;; disp-map (item->display-string), falling back to flatten-for-display for a
  ;; candidate not drawn from the original corpus.
  (define (display-version state candidate)
    (let ([disp (hashtable-ref (finder-state-disp-map state) candidate %display-absent)])
      (if (eq? disp %display-absent)
          (flatten-for-display candidate)
          disp)))

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

  ;; Emit a single character with highlight toggling.
  ;; Returns the new in-hl? state.
  ;; hl-on/hl-off: escape strings for entering/leaving highlight.
  ;; restore-after-newline: thunk called after ↵ to restore colours.
  (define (emit-char-with-highlight buf ch want-hl? in-hl?
                                    hl-on hl-off restore-after-newline colour?)
    (cond
      ;; ↵ symbol rendered in dim
      [(char=? ch #\x21b5)
       (when in-hl? (display hl-off buf))
       (display (sgr colour? SGR-DIM) buf)
       (display ch buf)
       (display (sgr colour? SGR-RESET) buf)
       (restore-after-newline)
       #f]
      [(and want-hl? (not in-hl?))
       (display hl-on buf)
       (display ch buf)
       #t]
      [(and (not want-hl?) in-hl?)
       (display hl-off buf)
       (display ch buf)
       #f]
      [else
       (display ch buf)
       in-hl?]))

  ;; Display candidate text with fzf-style match highlighting.
  ;; on-selected? uses hl+ (151), otherwise hl (108).
  (define (display-candidate buf str positions on-selected? colour?)
    (let ([len (string-length str)]
          [pos-set (make-eq-hashtable)]
          [hl-on  (sgr colour? (string-append SGR-BOLD (if on-selected? COL-HL+ COL-HL)))]
          [hl-off (sgr colour? (if on-selected?
                                   (string-append SGR-RESET COL-BG+ SGR-BOLD COL-FG+)
                                   SGR-RESET))]
          [restore (lambda ()
                     (when (and colour? on-selected?)
                       (display (string-append COL-BG+ SGR-BOLD COL-FG+) buf)))])
      (for-each (lambda (p) (hashtable-set! pos-set p #t)) positions)
      (let loop ([i 0] [in-hl? #f])
        (when (fx< i len)
          (let ([ch (string-ref str i)]
                [want? (hashtable-ref pos-set i #f)])
            (loop (fx1+ i)
                  (emit-char-with-highlight buf ch want? in-hl?
                                            hl-on hl-off restore colour?)))))
      (when (let loop ([i 0] [last #f])
              (cond [(fx>= i len) last]
                    [(hashtable-ref pos-set i #f) (loop (fx1+ i) #t)]
                    [else (loop (fx1+ i) #f)]))
        (display hl-off buf))))

  ;; Display candidate with Scheme syntax colouring + match highlighting.
  ;; Token-based foreground colours (rainbow parens, identifiers, strings, etc.)
  ;; with bold+underline overlay on matched positions.
  (define (display-candidate/syntax buf str positions on-selected? colour?)
    (let* ([tokens (tokenize str)]
           [len (string-length str)]
           [pos-set (make-eq-hashtable)]
           [hl-on  (sgr colour? "\x1b;[1;4m")]
           [hl-off (sgr colour? "\x1b;[22;24m")])
      (for-each (lambda (p) (hashtable-set! pos-set p #t)) positions)
      (for-each
        (lambda (tok)
          (let* ([type (car tok)]
                 [start (cadr tok)]
                 [end (caddr tok)]
                 [depth (cadddr tok)]
                 [span (substring str start end)]
                 [emit-tok-colour
                   (lambda ()
                     (when colour?
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
                         [else (void)])))]
                 [restore (lambda ()
                            (when (and colour? on-selected?) (display COL-BG+ buf))
                            (emit-tok-colour))])
            ;; Set syntax foreground for this token
            (emit-tok-colour)
            ;; Render characters with match position overlay
            (let char-loop ([i start] [in-hl? #f])
              (when (fx< i end)
                (let ([ch (string-ref str i)]
                      [want-hl? (hashtable-ref pos-set i #f)])
                  (char-loop (fx1+ i)
                             (emit-char-with-highlight buf ch want-hl? in-hl?
                                                      hl-on hl-off restore colour?)))))
            ;; Reset bold/underline and fg after each token
            (when colour? (display "\x1b;[22;24;39m" buf))
            (when (and colour? on-selected?) (display COL-BG+ buf))))
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
           [colorize? (finder-state-colorize? state)]
           [show-nums? (finder-state-show-numbers? state)]
           [colour? (finder-state-colour? state)]
           ;; Shadow the palette with colour?-gated copies: when colour output
           ;; is not permitted every SGR collapses to "" at source, while the
           ;; cursor/clear-line escapes below stay intact.
           [COL-BG+     (sgr colour? COL-BG+)]
           [COL-FG+     (sgr colour? COL-FG+)]
           [COL-POINTER (sgr colour? COL-POINTER)]
           [COL-INFO    (sgr colour? COL-INFO)]
           [COL-PROMPT  (sgr colour? COL-PROMPT)]
           [SGR-BOLD    (sgr colour? SGR-BOLD)]
           [SGR-DIM     (sgr colour? SGR-DIM)]
           [SGR-RESET   (sgr colour? SGR-RESET)])

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
                       ;; Number gutter: "  123 " when show-nums? is true
                       [num-str (if show-nums?
                                    (string-append
                                      (number->string (fx1+ idx)) " ")
                                    "")]
                       [num-width (string-length num-str)]
                       [text-width (fx- cols (fx+ 2 num-width))]  ; 2 cols for pointer gutter + number
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
                        ;; Line number (dim)
                        (when show-nums?
                          (display SGR-DIM buf)
                          (display num-str buf)
                          (display SGR-BOLD buf))
                        ;; Candidate text in fg+ with hl+
                        (if (and colorize? (colorize? original))
                            (begin
                              (display COL-BG+ buf)
                              (display-candidate/syntax buf truncated vis-positions #t colour?))
                            (begin
                              (display SGR-BOLD buf)
                              (display COL-FG+ buf)
                              (display COL-BG+ buf)
                              (display-candidate buf truncated vis-positions #t colour?)))
                        (display SGR-RESET buf))
                      (begin
                        ;; Normal line: 2 space gutter + optional number, default fg
                        (display "  " buf)
                        (when show-nums?
                          (display SGR-DIM buf)
                          (display num-str buf)
                          (display SGR-RESET buf))
                        (if (and colorize? (colorize? original))
                            (display-candidate/syntax buf truncated vis-positions #f colour?)
                            (display-candidate buf truncated vis-positions #f colour?))
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
      (refilter! state q)))

  (define (query-delete-back! state)
    (let* ([q (finder-state-query state)]
           [c (finder-state-cursor state)])
      (when (fx> c 0)
        (let ([new-q (string-append (substring q 0 (fx1- c))
                                    (substring q c (string-length q)))])
          (finder-state-query-set! state new-q)
          (finder-state-cursor-set! state (fx1- c))
          (refilter! state q)))))

  (define (query-delete-forward! state)
    (let* ([q (finder-state-query state)]
           [c (finder-state-cursor state)]
           [len (string-length q)])
      (when (fx< c len)
        (let ([new-q (string-append (substring q 0 c)
                                    (substring q (fx1+ c) len))])
          (finder-state-query-set! state new-q)
          (refilter! state q)))))

  (define (query-clear! state)
    (let ([q (finder-state-query state)])
      (unless (string=? q "")
        (finder-state-query-set! state "")
        (finder-state-cursor-set! state 0)
        (refilter! state q))))

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
            (refilter! state q))))))

  ;; ======================================================================
  ;; Re-filtering
  ;; ======================================================================

  ;; True iff string s contains character ch.
  (define (string-contains-char? s ch)
    (let ([n (string-length s)])
      (let lp ([i 0])
        (cond
          [(fx>= i n) #f]
          [(char=? (string-ref s i) ch) #t]
          [else (lp (fx1+ i))]))))

  ;; May the new query re-score only the OLD query's survivors rather than the
  ;; whole corpus?  Only when it strictly extends the old query at the end AND
  ;; introduces no operator that can make the match set non-monotonic:
  ;;   |  OR — a later alternative can match strings the old query rejected;
  ;;   !  negation — flips which strings qualify;
  ;;   $  suffix anchor — a trailing $ makes the token a suffix match, so typing
  ;;      past it (foo$ -> foo$x) reverts to a fuzzy token that can re-admit
  ;;      strings the suffix excluded.
  ;; A deletion or a mid-string insert fails string-prefix? and also falls back.
  ;; This is a conservative over-approximation: a literal |/!/$ merely forces a
  ;; full re-score, never a wrong result.
  (define (can-narrow? old new)
    (and (fx> (string-length new) (string-length old))
         (string-prefix? old new)
         (not (string-contains-char? new #\|))
         (not (string-contains-char? new #\!))
         (not (string-contains-char? new #\$))))

  ;; Re-score the candidates for the current query.  When can-narrow? holds we
  ;; feed only the previous survivors — but RESTORED TO ORIGINAL-CORPUS ORDER via
  ;; idx-map, because fuzzy's score-tiebreak leaves equal-score/equal-length ties
  ;; in input order and list-sort is stable; feeding the old-sorted order would
  ;; reorder ties versus a full re-score.  Otherwise we re-score the whole corpus.
  ;; Callers pass their pre-mutation query as old-query.
  (define (refilter! state old-query)
    (let* ([q (finder-state-query state)]
           [candidates
            (if (can-narrow? old-query q)
                (let ([imap (finder-state-idx-map state)])
                  (list-sort
                    (lambda (a b)
                      (fx< (hashtable-ref imap a 0) (hashtable-ref imap b 0)))
                    (map car (vector->list (finder-state-filtered state)))))
                (vector->list (finder-state-items state)))]
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
      ;; Refresh the leaf width cache on the same resize event, mirroring the
      ;; REPL's SIGWINCH handler.  The finder installs its OWN SIGWINCH handler
      ;; for the duration of the TUI, displacing the REPL handler that is
      ;; otherwise the sole refresher of this cache; without this the editor
      ;; that resumes after the finder would read a pre-resize width.  Source
      ;; the finder-state rows/cols from the just-refreshed cache so the two
      ;; never diverge (one live query, not two).
      (refresh-terminal-size-cache!)
      (let-values ([(new-rows new-cols) (cached-terminal-size)])
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
  ;; Plain-text fallback (non-ANSI output target)
  ;; ======================================================================

  ;; Trim leading/trailing whitespace so " 2 " (or a trailing CR from CRLF)
  ;; still parses as the integer 2.
  (define (strip-surrounding-whitespace s)
    (let ([len (string-length s)])
      (let ([start (let lp ([i 0])
                     (if (and (fx< i len) (char-whitespace? (string-ref s i)))
                         (lp (fx1+ i))
                         i))]
            [end (let lp ([i len])
                   (if (and (fx> i 0) (char-whitespace? (string-ref s (fx1- i))))
                       (lp (fx1- i))
                       i))])
        (if (fx<= end start) "" (substring s start end)))))

  ;; Plain-text selection used when the output target is not ANSI-capable
  ;; (a pipe, a file, a dumb terminal, or a CI log). The full-screen finder
  ;; needs cursor positioning and the alternate screen, so instead present a
  ;; numbered list and read one choice — emitting zero escape bytes. Returns
  ;; the chosen ORIGINAL item string, or #f on end-of-file, a blank line, or an
  ;; out-of-range / non-numeric choice (matching the TUI's accept-returns-the-
  ;; string / cancel-returns-#f contract).
  (define (run-finder/plain items prompt in-port out-port)
    (let* ([vec (list->vector items)]
           [n (vector-length vec)])
      (let lp ([i 0])
        (when (fx< i n)
          (display (fx1+ i) out-port)
          (display ". " out-port)
          (display (flatten-for-display (vector-ref vec i)) out-port)
          (newline out-port)
          (lp (fx1+ i))))
      (display prompt out-port)
      (flush-output-port out-port)
      (let ([line (get-line in-port)])
        (if (eof-object? line)
            #f
            (let ([choice (string->number (strip-surrounding-whitespace line))])
              (if (and choice (exact? choice) (integer? choice)
                       (>= choice 1) (<= choice n))
                  (vector-ref vec (- choice 1))
                  #f))))))

  ;; ======================================================================
  ;; Public API
  ;; ======================================================================

  (define run-finder
    (case-lambda
      [(items prompt) (run-finder items prompt #f)]
      [(items prompt colorize?)
       (run-finder* items prompt colorize? #f #f)]
      [(items prompt colorize? mode-map)
       (run-finder* items prompt colorize? mode-map #f)]
      [(items prompt colorize? mode-map show-numbers?)
       (run-finder* items prompt colorize? mode-map show-numbers?)]))

  (define (run-finder* items prompt colorize? mode-map show-numbers?)
    ;; Seed the leaf terminal-size cache at entry so the finder's first size
    ;; query and any immediate cache read start from the true width rather than
    ;; the stale 24x80 default.  The matching re-seed after finder-loop returns
    ;; (below) is what restores the width the editor resumes at.
    (refresh-terminal-size-cache!)
    ;; Gate the full-screen TUI on the ANSI-capability of the OUTPUT target,
    ;; using the explicit fd 1: the shared console port aliases to fd 2, so
    ;; gating on the port object would inspect the wrong stream and could leak
    ;; escapes into a piped stdout. When the output is not a capable terminal,
    ;; refuse raw-mode + the alternate screen and use a plain, zero-escape
    ;; numbered selection instead.
    (if (not (ansi-ok? 1))
        (run-finder/plain items prompt (console-input-port) (console-output-port))
        (let-values ([(rows cols) (query-terminal-size)])
          (let* ([colour? (colour-ok? 1)]
                 [item-vec (list->vector items)]
                 [disp-vec (build-display-items item-vec)]
                 [init-filtered (list->vector (map (lambda (s) (cons s '())) items))]
                 [color-fn (cond
                            ;; Suppress syntax colour when colour output is not
                            ;; permitted (NO_COLOR on a real TTY) as well as when
                            ;; the caller did not ask for it.
                            [(not (and colour? colorize?)) #f]
                            [mode-map
                             ;; Use stored mode: syntax-colour only if mode is 'scheme
                             (lambda (s) (eq? 'scheme (hashtable-ref mode-map s 'scheme)))]
                            [else
                             ;; Default: use classifier
                             (lambda (s) (eq? 'scheme (classify-input s)))])]
                 [state (make-finder-state item-vec init-filtered
                                           "" 0 0 0
                                           rows cols
                                           (length items) prompt
                                           disp-vec color-fn (and show-numbers? #t)
                                           colour?)]
                 [resize-flag (make-flag)])
            ;; Scope the resize handler to the finder: with-signal-handler
            ;; installs it for the duration of the TUI and restores whatever
            ;; SIGWINCH disposition was live beforehand (the REPL's resize
            ;; handler) on exit, rather than clobbering it with a no-op.
            (let ([result
                   (with-signal-handler SIGWINCH
                     (lambda (sig) (flag-set! resize-flag))
                     (lambda ()
                       (with-raw-mode 0
                         (lambda ()
                           (with-alternate-screen
                             (lambda ()
                               (finder-loop state resize-flag)))))))])
              ;; Re-seed the leaf width cache once the finder has torn down (raw
              ;; mode, alternate screen and the finder's own SIGWINCH handler
              ;; all restored).  Belt-and-braces alongside the in-loop refresh:
              ;; it guarantees the editor that resumes reads the true width even
              ;; when a resize arrived while the finder's handler was installed.
              (refresh-terminal-size-cache!)
              result)))))

  (define fuzzy-select
    (case-lambda
      [(items) (run-finder items "> ")]
      [(items prompt) (run-finder items prompt)]))

) ; end library
