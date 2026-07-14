;;; (test vterm) -- A PTY-free virtual-terminal screen model for asserting the
;;; editor's rendered output.  It folds a captured ANSI byte string into a
;;; fixed-width cell grid with a derived cursor and per-cell SGR attributes, so a
;;; test can read back the text, the cursor row/column, and the colour/attribute
;;; at any (row, column) without a real terminal.
;;;
;;; This library is deliberately NOT named test-*.ss.  The Makefile globs
;;; test/*.ss and runs as a script every file it is not told is a harness file,
;;; and Chez, handed a library form via --script, does not baulk at it: it simply
;;; defines the library and exits 0, printing nothing.  So a harness library that
;;; escaped the Makefile's TEST_HARNESS list would not fail loudly -- it would
;;; join the run as a phantom suite, silently passing without ever asserting
;;; anything, which is a good deal worse than an error.  The split is therefore
;;; enforced in both directions: make check-test-wiring holds every test/*.ss to
;;; being either a wired suite or a declared harness file, so no library can
;;; drift into the suite list, and no failing suite can be quietened by parking
;;; it in the exclusion list.  It is a source-only library (like test/runner.ss)
;;; that resolves under the suites' --libdirs .:src.
;;;
;;; Correctness is defined as AGREEMENT with (hafod editor render): the glyph
;;; advance reuses char-display-width (the renderer's wcwidth width oracle) and
;;; the wrap rule mirrors count-visual-lines, so the grid geometry matches the
;;; renderer rather than re-deriving it.
;;;
;;; Copyright (c) 2026, hafod contributors.

(library (test vterm)
  (export make-vterm vterm-feed! render->screen
          vterm-cursor vterm-cursor-row vterm-cursor-col
          vterm-cell vterm-row-text vterm-lines vterm-grid
          vterm-rows vterm-cols
          cell-glyph cell-fg cell-bg
          cell-bold? cell-reverse? cell-underline? cell-dim?
          assume-terminal-caps)
  (import (chezscheme)
          (only (hafod editor input-decode) char-display-width string-display-width)
          (only (hafod editor render) ansi-display-width count-visual-lines cursor-visual-row)
          (only (hafod terminal-caps) assume-terminal-caps))

  ;; ====================================================================
  ;; Cell -- one screen position: its glyph plus the pen (SGR attributes) that
  ;; were live when it was written.  fg/bg are 'default, an (r g b) list
  ;; (truecolour), or an integer (a 256-colour index).  Record style mirrors the
  ;; house convention (nongenerative define-record-type, as gap-buffer.ss uses).
  ;; ====================================================================
  (define-record-type cell
    (nongenerative)
    (fields (mutable glyph      cell-glyph      cell-glyph-set!)
            (mutable fg         cell-fg         cell-fg-set!)
            (mutable bg         cell-bg         cell-bg-set!)
            (mutable bold?      cell-bold?      cell-bold?-set!)
            (mutable reverse?   cell-reverse?   cell-reverse?-set!)
            (mutable underline? cell-underline? cell-underline?-set!)
            (mutable dim?       cell-dim?       cell-dim?-set!)))

  ;; A fresh blank cell: a space glyph, default colours, no attributes.
  (define (make-blank-cell)
    (make-cell #\space 'default 'default #f #f #f #f))

  ;; Reset a cell in place to blank (used by the erase operations).
  (define (blank-cell! c)
    (cell-glyph-set! c #\space)
    (cell-fg-set! c 'default)
    (cell-bg-set! c 'default)
    (cell-bold?-set! c #f)
    (cell-reverse?-set! c #f)
    (cell-underline?-set! c #f)
    (cell-dim?-set! c #f))

  ;; A private copy of a cell (used for DECSC's saved pen).
  (define (copy-cell c)
    (make-cell (cell-glyph c) (cell-fg c) (cell-bg c)
               (cell-bold? c) (cell-reverse? c) (cell-underline? c) (cell-dim? c)))

  ;; Copy just the pen attributes (not the glyph) from src into dst.
  (define (copy-pen-attrs! src dst)
    (cell-fg-set! dst (cell-fg src))
    (cell-bg-set! dst (cell-bg src))
    (cell-bold?-set! dst (cell-bold? src))
    (cell-reverse?-set! dst (cell-reverse? src))
    (cell-underline?-set! dst (cell-underline? src))
    (cell-dim?-set! dst (cell-dim? src)))

  ;; ====================================================================
  ;; Screen model -- a fixed-width grid of cells, a 0-based cursor, a live pen
  ;; used as the attribute template, and a saved cursor+pen for DECSC/DECRC.
  ;; The row store is a list of row vectors (row 0 first); each row is a vector
  ;; of `cols` distinct cells.  Rows grow on demand as the cursor descends.
  ;; ====================================================================
  (define-record-type vterm
    (nongenerative)
    (fields (immutable cols    vterm-cols)
            (mutable   store   vterm-store   vterm-store-set!)
            (mutable   cur-row vterm-cur-row vterm-cur-row-set!)
            (mutable   cur-col vterm-cur-col vterm-cur-col-set!)
            (mutable   pen     vterm-pen     vterm-pen-set!)
            (mutable   saved   vterm-saved   vterm-saved-set!))
    (protocol
      (lambda (new)
        (lambda (cols)
          (let ([w (max cols 0)])
            (new w (list (make-blank-row w)) 0 0 (make-blank-cell) #f))))))

  ;; Build a fresh blank row of `cols` distinct cells.
  (define (make-blank-row cols)
    (let ([v (make-vector cols #f)])
      (let fill ([i 0])
        (if (>= i cols)
            v
            (begin (vector-set! v i (make-blank-cell)) (fill (+ i 1)))))))

  ;; Build n fresh blank rows.
  (define (make-blank-rows n cols)
    (let loop ([k n] [acc '()])
      (if (<= k 0) acc (loop (- k 1) (cons (make-blank-row cols) acc)))))

  ;; Grow the store so that row r (0-based) exists; new rows are blank.
  (define (ensure-row! vt r)
    (let ([have (length (vterm-store vt))]
          [need (+ r 1)])
      (when (< have need)
        (vterm-store-set! vt
          (append (vterm-store vt) (make-blank-rows (- need have) (vterm-cols vt)))))))

  ;; The row vector at row r (assumes ensure-row! has already made it exist).
  (define (row-at vt r)
    (list-ref (vterm-store vt) r))

  ;; ====================================================================
  ;; Glyph placement -- write a glyph at the cursor with a copy of the pen, then
  ;; advance the column.  The wrap rule mirrors (hafod editor render)'s
  ;; count-visual-lines: a glyph whose width would carry the column past `cols`
  ;; first moves to a new row at column 0, so the grid geometry agrees with the
  ;; renderer.  A glyph that fills the last column exactly does not wrap -- the
  ;; column rests at `cols` (a pending wrap) and the next glyph wraps.  A
  ;; zero-width glyph does not advance and is dropped.
  ;;
  ;; Precondition for exact agreement with the oracle: the prompt fits the
  ;; terminal (prompt-width <= cols), which holds for every real render path.
  ;; This model wraps ALL glyphs uniformly, including the prompt; count-visual-
  ;; lines/cursor-visual-row instead treat a prompt wider than the terminal as
  ;; un-wrapped, so a sub-prompt-width terminal would make grid geometry and the
  ;; oracle disagree.  Keep prompt-width <= cols when cross-checking the two.
  ;; ====================================================================
  (define (put-glyph! vt ch)
    (let ([cols (vterm-cols vt)]
          [cw (char-display-width ch)])
      (when (> cw 0)
        (when (> (+ (vterm-cur-col vt) cw) cols)
          (vterm-cur-row-set! vt (+ (vterm-cur-row vt) 1))
          (vterm-cur-col-set! vt 0))
        (ensure-row! vt (vterm-cur-row vt))
        (let ([col (vterm-cur-col vt)])
          (when (< col cols)
            (let ([c (vector-ref (row-at vt (vterm-cur-row vt)) col)])
              (cell-glyph-set! c ch)
              (copy-pen-attrs! (vterm-pen vt) c))))
        (vterm-cur-col-set! vt (+ (vterm-cur-col vt) cw)))))

  ;; ====================================================================
  ;; SGR fold -- fold a list of integer parameters left-to-right into the pen.
  ;; Each code updates exactly one independent attribute, so 39m resets only the
  ;; foreground (the background survives), 0m resets everything, 22m clears bold
  ;; and dim.  38/48 consume their truecolour (2;r;g;b) or 256 (5;n) arguments.
  ;; ====================================================================
  (define (reset-pen! pen)
    (cell-fg-set! pen 'default)
    (cell-bg-set! pen 'default)
    (cell-bold?-set! pen #f)
    (cell-dim?-set! pen #f)
    (cell-reverse?-set! pen #f)
    (cell-underline?-set! pen #f))

  (define (set-pen-colour! pen which val)
    (if (eq? which 'fg)
        (cell-fg-set! pen val)
        (cell-bg-set! pen val)))

  ;; Consume the sub-parameters after a 38/48 code and set the pen colour.
  ;; 2 r g b -> (r g b) truecolour; 5 n -> integer 256-colour index.  Returns the
  ;; remaining parameters; a malformed run consumes nothing more.
  (define (take-colour! pen which rest)
    (cond
      [(and (pair? rest) (eqv? (car rest) 2)
            (pair? (cdr rest)) (pair? (cddr rest)) (pair? (cdddr rest)))
       (set-pen-colour! pen which (list (cadr rest) (caddr rest) (cadddr rest)))
       (cddddr rest)]
      [(and (pair? rest) (eqv? (car rest) 5) (pair? (cdr rest)))
       (set-pen-colour! pen which (cadr rest))
       (cddr rest)]
      [else rest]))

  (define (apply-sgr! pen params)
    (let loop ([p (if (null? params) '(0) params)])
      (unless (null? p)
        (case (car p)
          [(0)  (reset-pen! pen)                                  (loop (cdr p))]
          [(1)  (cell-bold?-set! pen #t)                          (loop (cdr p))]
          [(2)  (cell-dim?-set! pen #t)                           (loop (cdr p))]
          [(22) (cell-bold?-set! pen #f) (cell-dim?-set! pen #f)  (loop (cdr p))]
          [(4)  (cell-underline?-set! pen #t)                     (loop (cdr p))]
          [(24) (cell-underline?-set! pen #f)                     (loop (cdr p))]
          [(7)  (cell-reverse?-set! pen #t)                       (loop (cdr p))]
          [(27) (cell-reverse?-set! pen #f)                       (loop (cdr p))]
          [(39) (cell-fg-set! pen 'default)                       (loop (cdr p))]
          [(49) (cell-bg-set! pen 'default)                       (loop (cdr p))]
          [(38) (loop (take-colour! pen 'fg (cdr p)))]
          [(48) (loop (take-colour! pen 'bg (cdr p)))]
          [else (loop (cdr p))]))))

  ;; ====================================================================
  ;; Readers
  ;; ====================================================================

  ;; Current row count (grows as the cursor descends past written content).
  (define (vterm-rows vt) (length (vterm-store vt)))

  (define (vterm-cursor-row vt) (vterm-cur-row vt))
  (define (vterm-cursor-col vt) (vterm-cur-col vt))
  (define (vterm-cursor vt) (values (vterm-cur-row vt) (vterm-cur-col vt)))

  ;; The cell at (row, col), or #f if out of range.
  (define (vterm-cell vt row col)
    (and (>= row 0) (< row (vterm-rows vt))
         (>= col 0) (< col (vterm-cols vt))
         (vector-ref (row-at vt row) col)))

  ;; A row vector as a string with trailing blank cells trimmed.
  (define (row->string rv)
    (let ([n (vector-length rv)])
      (let find-last ([i 0] [last -1])
        (if (>= i n)
            (let build ([k 0] [acc '()])
              (if (> k last)
                  (list->string (reverse acc))
                  (build (+ k 1) (cons (cell-glyph (vector-ref rv k)) acc))))
            (find-last (+ i 1)
                       (if (char=? (cell-glyph (vector-ref rv i)) #\space) last i))))))

  ;; That row's glyphs as a string (trailing blanks trimmed); "" if out of range.
  (define (vterm-row-text vt row)
    (if (and (>= row 0) (< row (vterm-rows vt)))
        (row->string (row-at vt row))
        ""))

  ;; Every row as a string (trailing blanks trimmed per row).
  (define (vterm-lines vt)
    (map row->string (vterm-store vt)))

  ;; The grid as a list of rows, each a list of cells.
  (define (vterm-grid vt)
    (map vector->list (vterm-store vt)))

  ;; ====================================================================
  ;; Erase helpers
  ;; ====================================================================
  (define (blank-row! rv)
    (let ([n (vector-length rv)])
      (let loop ([i 0]) (when (< i n) (blank-cell! (vector-ref rv i)) (loop (+ i 1))))))

  (define (blank-row-from! rv col)
    (let ([n (vector-length rv)])
      (let loop ([i (max col 0)]) (when (< i n) (blank-cell! (vector-ref rv i)) (loop (+ i 1))))))

  ;; The first (min n length) elements of lst; never raises on a short list.
  (define (list-head-safe lst n)
    (if (or (<= n 0) (null? lst))
        '()
        (cons (car lst) (list-head-safe (cdr lst) (- n 1)))))

  ;; ESC[J with param 2 erases the whole grid (cursor kept); otherwise it erases
  ;; from the cursor to end-of-screen: the current row from the cursor column
  ;; onward, plus every row below.
  (define (erase-in-display! vt mode)
    (cond
      [(= mode 2)
       (for-each blank-row! (vterm-store vt))]
      [else
       (let ([r (vterm-cur-row vt)])
         (ensure-row! vt r)
         (blank-row-from! (row-at vt r) (vterm-cur-col vt))
         (vterm-store-set! vt (list-head-safe (vterm-store vt) (+ r 1))))]))

  ;; ====================================================================
  ;; DECSC / DECRC -- save and restore the cursor and pen (emitted by the
  ;; matching-paren flash).  Consumed here so the flash frame parses cleanly.
  ;; ====================================================================
  (define (save-cursor! vt)
    (vterm-saved-set! vt
      (list (vterm-cur-row vt) (vterm-cur-col vt) (copy-cell (vterm-pen vt)))))

  (define (restore-cursor! vt)
    (let ([s (vterm-saved vt)])
      (when (pair? s)
        (vterm-cur-row-set! vt (car s))
        (vterm-cur-col-set! vt (cadr s))
        (copy-pen-attrs! (caddr s) (vterm-pen vt))
        (ensure-row! vt (vterm-cur-row vt)))))

  ;; ====================================================================
  ;; CSI parameter parsing
  ;; ====================================================================
  (define (ascii-digit? c) (and (char<=? #\0 c) (char<=? c #\9)))

  ;; Parse a CSI parameter string ("38;2;255;0;0") into a list of integers.
  ;; Fields are separated by #\; ; each field's value is the integer of its digit
  ;; characters (any private/intermediate byte, e.g. #\? or a space, is ignored);
  ;; an empty field yields 0.  An empty parameter string yields '().
  (define (parse-params s)
    (let ([len (string-length s)])
      (if (= len 0)
          '()
          (let loop ([i 0] [cur 0] [acc '()])
            (cond
              [(>= i len) (reverse (cons cur acc))]
              [(char=? (string-ref s i) #\;) (loop (+ i 1) 0 (cons cur acc))]
              [(ascii-digit? (string-ref s i))
               (loop (+ i 1)
                     (+ (* cur 10)
                        (- (char->integer (string-ref s i)) (char->integer #\0)))
                     acc)]
              [else (loop (+ i 1) cur acc)])))))

  ;; The first parameter, or d if there are none.
  (define (param-default params d)
    (if (null? params) d (car params)))

  ;; The 0-based nth parameter, or d if absent.
  (define (param-nth params n d)
    (let loop ([p params] [k n])
      (cond [(null? p) d]
            [(= k 0) (car p)]
            [else (loop (cdr p) (- k 1))])))

  ;; ====================================================================
  ;; The escape parser -- a bounded GROUND/ESC/CSI/OSC state machine that walks
  ;; the captured string once and always advances, so a malformed, truncated, or
  ;; unknown sequence can never hang, throw, or corrupt the grid with payload.
  ;; ====================================================================

  ;; Dispatch a completed CSI (parameters between "[" and the final byte).
  (define (dispatch-csi! vt param-str final)
    (let ([params (parse-params param-str)])
      (case final
        [(#\m) (apply-sgr! (vterm-pen vt) params)]
        [(#\A) (vterm-cur-row-set! vt
                 (max 0 (- (vterm-cur-row vt) (param-default params 1))))]
        [(#\B) (vterm-cur-row-set! vt (+ (vterm-cur-row vt) (param-default params 1)))
               (ensure-row! vt (vterm-cur-row vt))]
        [(#\G) (vterm-cur-col-set! vt (max 0 (- (param-default params 1) 1)))]
        [(#\J) (erase-in-display! vt (param-default params 0))]
        [(#\H) (vterm-cur-row-set! vt (max 0 (- (param-default params 1) 1)))
               (vterm-cur-col-set! vt (max 0 (- (param-nth params 1 1) 1)))
               (ensure-row! vt (vterm-cur-row vt))]
        [else (void)])))                ; unknown final byte: a no-op skip

  ;; From index k (just after "["), collect parameter/private/intermediate bytes
  ;; up to the first final byte in #x40..#x7E, then dispatch.  Returns the next
  ;; index; a truncated CSI (no final byte before end-of-string) stops safely.
  (define (handle-csi vt str len k)
    (let scan ([j k])
      (cond
        [(>= j len) len]
        [(let ([c (char->integer (string-ref str j))])
           (and (>= c #x40) (<= c #x7E)))
         (dispatch-csi! vt (substring str k j) (string-ref str j))
         (+ j 1)]
        [else (scan (+ j 1))])))

  ;; From index k (just after "]"), skip an OSC string until BEL (#x07) or a
  ;; two-byte ST (ESC #\\), consuming the terminator; the payload never becomes
  ;; glyphs.  An unterminated OSC stops safely at end-of-string.
  (define (skip-osc str len k)
    (let scan ([j k])
      (cond
        [(>= j len) len]
        [(char=? (string-ref str j) #\x07) (+ j 1)]
        [(and (char=? (string-ref str j) #\x1b)
              (< (+ j 1) len)
              (char=? (string-ref str (+ j 1)) #\\))
         (+ j 2)]
        [else (scan (+ j 1))])))

  ;; str[j] is the byte after ESC.  Returns the next index to resume from.
  (define (handle-escape vt str len j)
    (if (>= j len)
        len                             ; a truncated ESC at end-of-string: stop
        (let ([c (string-ref str j)])
          (cond
            [(char=? c #\[) (handle-csi vt str len (+ j 1))]
            [(char=? c #\]) (skip-osc str len (+ j 1))]
            [(char=? c #\7) (save-cursor! vt) (+ j 1)]
            [(char=? c #\8) (restore-cursor! vt) (+ j 1)]
            [else (+ j 1)]))))          ; unknown two-byte ESC x: skip one byte

  ;; Fold a captured byte string into the grid.  ESC enters the escape handler;
  ;; #\return moves the column to 0; #\newline advances the row and resets the
  ;; column to 0; any other byte is a glyph.  Bounded by the string length.
  (define (vterm-feed! vt str)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (when (< i len)
          (let ([ch (string-ref str i)])
            (cond
              [(char=? ch #\x1b) (loop (handle-escape vt str len (+ i 1)))]
              [(char=? ch #\return) (vterm-cur-col-set! vt 0) (loop (+ i 1))]
              [(char=? ch #\newline)
               (vterm-cur-row-set! vt (+ (vterm-cur-row vt) 1))
               (vterm-cur-col-set! vt 0)
               (ensure-row! vt (vterm-cur-row vt))
               (loop (+ i 1))]
              [else (put-glyph! vt ch) (loop (+ i 1))]))))))

  ;; ====================================================================
  ;; render->screen -- force capabilities on, capture a render thunk's output to
  ;; a string port, and fold it into a fresh vterm.  PTY-free: the thunk emits
  ;; its full escape stream only because assume-terminal-caps is forced 'on.
  ;; ====================================================================
  (define (render->screen cols thunk)
    (let ([sp (open-output-string)])
      (parameterize ([assume-terminal-caps 'on])
        (thunk sp))
      (let ([vt (make-vterm cols)])
        (vterm-feed! vt (get-output-string sp))
        vt)))

  ) ; end library
