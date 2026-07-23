;;; (hafod editor ls-colors) -- Pure $LS_COLORS parser and type/extension -> SGR
;;; lookup, used to colour a completion candidate by its file type.
;;;
;;; The wire format (verified against GNU coreutils 9.11) is colon-separated
;;; key=value pairs whose value is a raw SGR parameter string -- semicolon-separated
;;; numbers, with no leading \e[ and no trailing m.  A 2-letter key names a type
;;; (di directory, ln symlink, ex executable, fi regular file); a *.suffix key names
;;; an extension rule.  Precedence follows ls: a type rule beats any extension rule,
;;; and among extension rules the LAST-listed match wins (list order, not the longest
;;; suffix).  When $LS_COLORS is unset a built-in default mirrors GNU dircolors.
;;;
;;; A $LS_COLORS value is untrusted -- ls does NOT sanitise it, so a value such as
;;; "01;34m\e[2J" would clear the screen once wrapped in an SGR sequence.  Every
;;; resolved value is therefore allow-listed to the SGR parameter class [0-9;] and
;;; dropped to #f on any other byte BEFORE it can reach \e[...m.  This module returns
;;; the raw parameter string only; the renderer wraps it, never this module, so there
;;; is no import of the renderer or editor here and thus no cycle -- the one-way rule
;;; is that render/editor import this leaf, never the reverse.
;;;
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor ls-colors)
  (export parse-ls-colors default-ls-colors current-ls-colors
          candidate-sgr valid-sgr?)
  (import (chezscheme))

  ;; ======================================================================
  ;; === SGR value allow-list (the hostile-input gate) ===
  ;; ======================================================================

  ;; #t only when every byte of S is a digit or a semicolon -- the SGR parameter
  ;; class.  A value carrying an escape, a letter, a bracket or any other byte is
  ;; refused here, before it can be wrapped in \e[...m, so a crafted $LS_COLORS
  ;; cannot smuggle a cursor-move, clear-screen or OSC sequence to the terminal.
  (define (valid-sgr? s)
    (let ([len (string-length s)])
      (let loop ([i 0])
        (or (= i len)
            (let ([c (string-ref s i)])
              (and (or (and (char<=? #\0 c) (char<=? c #\9))
                       (char=? c #\;))
                   (loop (+ i 1))))))))

  ;; ======================================================================
  ;; === Small self-contained string helpers ===
  ;; (terminal-caps deliberately pulls in next to nothing; mirror that -- no heavy
  ;; string library for a colon split, a "=" scan and a suffix test.)
  ;; ======================================================================

  ;; The 0-based index of the first CH in S, or #f when it does not occur.
  (define (index-of-char s ch)
    (let ([len (string-length s)])
      (let loop ([i 0])
        (cond
          [(= i len) #f]
          [(char=? (string-ref s i) ch) i]
          [else (loop (+ i 1))]))))

  ;; Split S into the substrings lying between each CH, in order.  Adjacent, leading
  ;; or trailing separators yield empty substrings (the parser skips those).  A
  ;; single left-to-right walk.
  (define (split-on s ch)
    (let ([len (string-length s)])
      (let loop ([i 0] [start 0] [acc '()])
        (cond
          [(= i len) (reverse (cons (substring s start len) acc))]
          [(char=? (string-ref s i) ch)
           (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
          [else (loop (+ i 1) start acc)]))))

  ;; #t when NAME ends with SUFFIX, folding case on both sides.  The suffix is stored
  ;; lower-cased at parse time; folding NAME here keeps the match symmetric so a
  ;; *.JPG rule colours photo.jpg and PHOTO.JPG alike.
  (define (suffix-ci? name suffix)
    (let ([nl (string-length name)]
          [sl (string-length suffix)])
      (and (>= nl sl)
           (let loop ([i (- nl sl)] [j 0])
             (or (= j sl)
                 (and (char=? (char-downcase (string-ref name i))
                              (char-downcase (string-ref suffix j)))
                      (loop (+ i 1) (+ j 1))))))))

  ;; ======================================================================
  ;; === LS_COLORS parser ===
  ;; ======================================================================

  ;; The 2-letter type keys this module honours.  The rarer keys (rs mh su sg ca ow
  ;; st tw or mi bd cd pi so do no) are deliberately ignored.
  (define (type-key? k)
    (or (string=? k "di") (string=? k "ln")
        (string=? k "ex") (string=? k "fi")))

  ;; An extension rule key is "*<suffix>" -- e.g. "*.tar", "*.tar.gz".
  (define (ext-key? k)
    (and (> (string-length k) 0)
         (char=? (string-ref k 0) #\*)))

  ;; The lower-cased suffix a *.ext key selects on, the leading "*" dropped so the
  ;; dot is retained (".tar", ".tar.gz").
  (define (ext-suffix k)
    (string-downcase (substring k 1 (string-length k))))

  ;; Split the raw value on ":" into key=value entries and sort them into a
  ;; (type-ht . ext-rules) table.  A type key sets the type table (a later duplicate
  ;; overwrites); a *.suffix key APPENDS to the ordered extension list so parse order
  ;; survives -- that ordering is what makes "last-listed wins" decidable at lookup.
  ;; Malformed entries (no "=", empty value) and unknown keys are skipped.  Pure: it
  ;; reads no environment.
  (define (parse-ls-colors raw)
    (let ([type-ht (make-eq-hashtable)])
      (let loop ([entries (split-on raw #\:)] [ext-acc '()])
        (if (null? entries)
            (cons type-ht (reverse ext-acc))
            (let* ([entry (car entries)]
                   [eq (index-of-char entry #\=)])
              (if (not eq)
                  (loop (cdr entries) ext-acc)
                  (let ([key (substring entry 0 eq)]
                        [val (substring entry (+ eq 1) (string-length entry))])
                    (cond
                      [(= (string-length val) 0)
                       (loop (cdr entries) ext-acc)]
                      [(type-key? key)
                       (hashtable-set! type-ht (string->symbol key) val)
                       (loop (cdr entries) ext-acc)]
                      [(ext-key? key)
                       (loop (cdr entries)
                             (cons (cons (ext-suffix key) val) ext-acc))]
                      [else
                       (loop (cdr entries) ext-acc)]))))))))

  ;; The built-in palette used when $LS_COLORS is unset, mirroring GNU dircolors:
  ;; directory bold blue, symlink bold cyan, executable bold green, a plain file
  ;; uncoloured.  No extension rules.
  (define (default-ls-colors)
    (let ([type-ht (make-eq-hashtable)])
      (hashtable-set! type-ht 'di "01;34")
      (hashtable-set! type-ht 'ln "01;36")
      (hashtable-set! type-ht 'ex "01;32")
      (hashtable-set! type-ht 'fi "0")
      (cons type-ht '())))

  ;; ======================================================================
  ;; === Type / extension -> validated colour lookup ===
  ;; ======================================================================

  ;; Walk the ordered extension rules keeping the LAST whose suffix matches NAME, or
  ;; #f when none matches.  Last-match-wins reproduces ls, which walks its extension
  ;; list and lets the final matching rule decide -- NOT the longest suffix.
  (define (ext-colour ext-rules name)
    (let loop ([rules ext-rules] [found #f])
      (if (null? rules)
          found
          (loop (cdr rules)
                (if (suffix-ci? name (caar rules)) (cdar rules) found)))))

  ;; Resolve TYPE first (dir->di, link->ln, exec->ex): a type rule beats any
  ;; extension rule.  Otherwise take the last matching extension rule, else the
  ;; plain-file colour.  The resolved value is passed through valid-sgr? -- a value
  ;; carrying any byte outside [0-9;] is dropped to #f so no colour reaches the pen.
  ;; Returns the raw SGR parameter string ONLY; the renderer wraps it in \e[...m,
  ;; never this procedure.  TYPE is one of dir link exec file #f.
  (define (candidate-sgr table name type)
    (let* ([type-ht (car table)]
           [ext-rules (cdr table)]
           [raw (cond
                  [(eq? type 'dir)  (hashtable-ref type-ht 'di #f)]
                  [(eq? type 'link) (hashtable-ref type-ht 'ln #f)]
                  [(eq? type 'exec) (hashtable-ref type-ht 'ex #f)]
                  [else
                   (or (ext-colour ext-rules name)
                       (hashtable-ref type-ht 'fi #f))])])
      (and raw (valid-sgr? raw) raw)))

  ;; ======================================================================
  ;; === Environment-backed table, memoised on the raw value ===
  ;; ======================================================================

  ;; The table for the live $LS_COLORS is computed once and memoised on the raw
  ;; value, so a render colours every cell from one parse rather than re-parsing per
  ;; candidate.  The cache is a (raw-key . table) pair whose key is the exact getenv
  ;; result -- INCLUDING the #f "unset" case.  Comparing the live getenv (its #f
  ;; absence included) against the cached key on every call means a toggle between a
  ;; set value and an unset environment always re-resolves, so no stale table leaks
  ;; across a change; when unset the built-in default is returned.
  (define ls-colors-cache #f)          ; #f => empty; otherwise (raw-key . table)

  (define (current-ls-colors)
    (let ([raw (getenv "LS_COLORS")])
      (if (and ls-colors-cache
               (equal? (car ls-colors-cache) raw))
          (cdr ls-colors-cache)
          (let ([table (if raw (parse-ls-colors raw) (default-ls-colors))])
            (set! ls-colors-cache (cons raw table))
            table))))

  ) ; end library
