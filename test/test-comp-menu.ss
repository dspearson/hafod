;;; test/test-comp-menu.ss -- The aligned-description proof for the completion
;;; menu, driven through the production grid renderer.  When candidates carry
;;; descriptions the grid pads every candidate to a common column so the
;;; descriptions line up beneath one another (the zsh/fish look), and it truncates
;;; an over-long description to the terminal width with an ellipsis.  Both
;;; behaviours are asserted here against render-completion-grid -- the renderer the
;;; live editor actually draws with -- rather than the exported-but-undrawn menu
;;; variant, so a green result reflects what the user sees.  Entirely PTY-free: the
;;; virtual-terminal harness folds the renderer's escape stream into a cell grid,
;;; so the suite opens no terminal and needs no platform gate.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor render) render-completion-grid)
        ;; The pure row-plan the grid draws with: the tests reckon the expected
        ;; header-inclusive rows and index<->cell with the SAME helpers so a header
        ;; miscount cannot hide behind the assertions' own arithmetic.
        (only (hafod editor completion-groups)
              build-row-plan row-plan-rows row-plan-locate row-plan-cell)
        ;; The live filename completer, driven end to end for the no-regression
        ;; smoke: its typed output must still render through the shared grid.
        (only (hafod editor editor) filename-completions)
        (only (hafod fileinfo) create-directory)
        (only (hafod process-state) with-cwd* pid)
        ;; Pin $LS_COLORS around every colour-target drive so a candidate's colour
        ;; is the palette under test, never the box's ambient value.
        (only (hafod environment) with-env* setenv)
        (chezscheme))

(test-begin "comp-menu")

;; ======================================================================
;; Helpers
;; ======================================================================

;; The 0-based index of the first occurrence of NEEDLE in HAY, or #f when absent.
;; A folded row is built from column 0, so an index into the row string is the
;; screen column of that glyph.
(define (substring-index hay needle)
  (let ([hl (string-length hay)]
        [nl (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nl) hl) #f]
        [(string=? (substring hay i (+ i nl)) needle) i]
        [else (loop (+ i 1))]))))

;; The screen column at which NEEDLE first appears anywhere in the folded grid,
;; or #f when no row carries it.  Descriptions are unique per row, so this reads
;; back the column each one starts at.
(define (needle-col scr needle)
  (let loop ([lines (vterm-lines scr)])
    (cond
      [(null? lines) #f]
      [(substring-index (car lines) needle) => (lambda (i) i)]
      [else (loop (cdr lines))])))

;; True when STR's last glyph is the U+2026 horizontal ellipsis truncate-display
;; appends when it drops the tail of an over-wide string.
(define (ends-with-ellipsis? str)
  (let ([n (string-length str)])
    (and (> n 0) (char=? (string-ref str (- n 1)) #\x2026))))

;; ======================================================================
;; $LS_COLORS pinning -- the CI-determinism rule.
;; render->screen forces the colour verdict on, so any grid drive that reads a
;; candidate's colour reads the AMBIENT $LS_COLORS -- set on a dev box, often
;; unset on CI -- the exact make-test-invisible env sensitivity this repo has
;; hit.  Every colour-target drive below is therefore wrapped so the palette is
;; the one under test, never the box's.  A #f pin UNSETS (the built-in default
;; palette resolves) via setenv -- NOT a with-env* '(("LS_COLORS" . #f)) delta,
;; which would route the unset through the C setenv, whose string argument cannot
;; take #f.  A string pin installs that exact override through with-env*.  The
;; ambient value is saved and restored either way.
;; ======================================================================
(define (with-ls-colors pin thunk)
  (if pin
      (with-env* (list (cons "LS_COLORS" pin)) thunk)
      (let ([saved (getenv "LS_COLORS")])
        (dynamic-wind
          (lambda () (setenv "LS_COLORS" #f))
          thunk
          (lambda () (setenv "LS_COLORS" saved))))))

;; Capture a render thunk's raw escape stream under the forced verdict, WITHOUT
;; folding it -- used to assert that a hostile $LS_COLORS payload never reaches
;; the byte stream at all.
(define (render->string cols thunk)
  (let ([sp (open-output-string)])
    (parameterize ([assume-terminal-caps 'on])
      (thunk sp))
    (get-output-string sp)))

;; Drive a SINGLE type-coloured candidate through the 7-arg grid, pinning
;; $LS_COLORS to PIN, and read the cell its first glyph lands on -- always
;; (row 1, column 2): the grid's leading newline puts the lone candidate row at 1
;; and the two-space indent puts the first glyph at column 2 (the same per-cell
;; read idiom as test-vterm-attrs / test-completion-overlay).  A lone unlabelled
;; group draws no header, so nothing shifts that cell, and driving one candidate
;; at a time sidesteps the column truncation the widest name would otherwise take.
(define (typed-first-cell cols name type pin)
  (let ([scr (with-ls-colors pin
               (lambda ()
                 (render->screen cols
                   (lambda (p)
                     (render-completion-grid p (list (cons name '())) -1 cols 10
                                             (list (cons #f 1)) (list type))))))])
    (vterm-cell scr 1 2)))

;; ======================================================================
;; (a) Aligned descriptions -- the alignment invariant on the live grid.
;; Three candidates of DIFFERING widths, each carrying a description, are rendered
;; on a wide terminal with no selection.  The grid pads every candidate to the
;; width of the widest before its description, so each description's first glyph
;; lands at one common column regardless of the candidate's own width.  A naive
;; renderer that placed a description right after its candidate would start the
;; three at three different columns, so the equal-column assertion fails on such a
;; tree -- it is non-vacuous.
;; ======================================================================

(let* ([entries '(("a"    () "alpha")
                  ("bbbb" () "beta")
                  ("cc"   () "gamma"))]
       [scr (with-ls-colors #f
              (lambda ()
                (render->screen 40
                  (lambda (p) (render-completion-grid p entries -1 40 10)))))]
       [col-alpha (needle-col scr "alpha")]
       [col-beta  (needle-col scr "beta")]
       [col-gamma (needle-col scr "gamma")])
  (test-assert "menu: every candidate's description is drawn into the grid"
    (and col-alpha col-beta col-gamma))
  (test-assert "menu: descriptions for mixed-width candidates share one start column"
    (and (= col-alpha col-beta) (= col-beta col-gamma))))

;; ======================================================================
;; (b) Over-long description truncation -- pins truncate-display.
;; On a narrow terminal a description wider than the remaining room is truncated
;; and closed with the U+2026 ellipsis.  The candidate carrying the long
;; description is the NARROWER of the two, so its own name is not truncated and
;; the ellipsis at the row's end is unambiguously the description's.  The row is
;; found by the description's leading word, so the assertion cannot pass on an
;; unrelated line.
;; ======================================================================

(let* ([long-desc "alpha beta gamma delta epsilon zeta eta theta"]
       [entries (list (list "aa"        '() long-desc)
                      (list "wwwwwwwww" '() "brief"))]
       [scr (with-ls-colors #f
              (lambda ()
                (render->screen 30
                  (lambda (p) (render-completion-grid p entries -1 30 10)))))]
       [desc-row (let loop ([lines (vterm-lines scr)])
                   (cond
                     [(null? lines) ""]
                     [(substring-index (car lines) "alpha") (car lines)]
                     [else (loop (cdr lines))]))])
  (test-assert "menu: the long description's row is present in the grid"
    (substring-index desc-row "alpha"))
  (test-assert "menu: an over-long description is truncated with the U+2026 ellipsis"
    (ends-with-ellipsis? desc-row)))

;; ======================================================================
;; Grouped-completion geometry -- the header-aware grid.
;; The renderer interleaves a dim category header before each labelled group and
;; maps the selected index to its screen cell through the SHARED row-plan, so the
;; four geometry sites (grid, grid-dimensions, drawn-rows, navigate) cannot drift.
;; These assertions drive the 6-arg render-completion-grid directly and reckon the
;; expected rows/cells with build-row-plan / row-plan-locate / row-plan-cell, so
;; each one fails on a header-blind renderer that quotients a flat index over the
;; column count (the pre-header behaviour) -- they are non-vacuous.
;; ======================================================================

;; The vterm row whose trimmed text is exactly TEXT (a header row is its label
;; alone from column 0), or #f.  row->string trims only TRAILING blanks, so an
;; exact match also proves the label carries no leading indent -- left-aligned.
(define (row-of-text scr text)
  (let loop ([lines (vterm-lines scr)] [r 0])
    (cond
      [(null? lines) #f]
      [(string=? (car lines) text) r]
      [else (loop (cdr lines) (+ r 1))])))

;; #t when vterm row R carries any cell with a non-default background.  The menu
;; paints a background on exactly one thing -- the selected cell -- so this reads
;; the selection back without coupling to the selection colour's RGB.
(define (row-selected? scr r)
  (let loop ([c 0])
    (cond
      [(>= c (vterm-cols scr)) #f]
      [(let ([cell (vterm-cell scr r c)])
         (and cell (not (eq? (cell-bg cell) 'default)))) #t]
      [else (loop (+ c 1))])))

;; The vterm row carrying the selection background, or #f.
(define (selected-row scr)
  (let loop ([r 0])
    (cond
      [(>= r (vterm-rows scr)) #f]
      [(row-selected? scr r) r]
      [else (loop (+ r 1))])))

;; The grid-cols / grid-rows the renderer computes for these inputs (grid-cols
;; follows the candidate widths, grid-rows counts header rows).  Read straight off
;; render-completion-grid's return so the assertions reckon with the geometry it
;; actually drew rather than a guess; colour does not affect these, so a plain
;; string port suffices.
(define (grid-geom cols entries maxv gp)
  (call-with-values
    (lambda () (render-completion-grid (open-output-string) entries -1 cols maxv gp))
    (lambda (ml gc gr so) (values gc gr))))

;; Draw a grouped grid onto a folded screen at COLS wide.  $LS_COLORS is pinned
;; unset so the header/selection reads are a deterministic palette: the candidates
;; here carry no threaded type, so this only neutralises the ambient value (dev
;; sets it, CI often does not), keeping cell-bg/cell-dim reads env-independent.
(define (render-grouped cols entries sel maxv gp)
  (with-ls-colors #f
    (lambda ()
      (render->screen cols
        (lambda (p) (render-completion-grid p entries sel cols maxv gp))))))

;; Two labelled groups: two directories then three files.  Candidate names are
;; short so both a group's candidates share one row on a wide terminal.
(define group-plan-2 (list (cons "directories" 2) (cons "files" 3)))
(define entries-2
  (list (cons "d1/" '()) (cons "d2/" '())
        (cons "f1" '()) (cons "f2" '()) (cons "f3" '())))

;; ----------------------------------------------------------------------
;; (c) Dim headers on their own row, left-aligned, with no blank separator.
;; ----------------------------------------------------------------------
(let* ([scr (render-grouped 40 entries-2 -1 10 group-plan-2)]
       [dir-row (row-of-text scr "directories")]
       [file-row (row-of-text scr "files")])
  (test-assert "menu: each group's category header is drawn on its own row, left-aligned at column 0"
    (and dir-row file-row))
  (test-assert "menu: the directories header reads back dim"
    (and dir-row (cell-dim? (vterm-cell scr dir-row 0))))
  (test-assert "menu: the files header reads back dim"
    (and file-row (cell-dim? (vterm-cell scr file-row 0))))
  (test-assert "menu: the files header sits below the directories header"
    (and dir-row file-row (> file-row dir-row)))
  ;; No blank line between a group's candidates and the next header: every row
  ;; from just past the directories header down to the files header is non-blank.
  (test-assert "menu: no blank row separates a group's candidates from the next header"
    (and dir-row file-row
         (let loop ([r (+ dir-row 1)])
           (cond
             [(>= r file-row) #t]
             [(string=? (vterm-row-text scr r) "") #f]
             [else (loop (+ r 1))])))))

;; ----------------------------------------------------------------------
;; (d) A single unlabelled group draws NO header (and no dim row at all).
;; ----------------------------------------------------------------------
(let* ([entries (list (cons "aa" '()) (cons "bb" '()) (cons "cc" '()))]
       [scr (render-grouped 40 entries -1 10 (list (cons #f 3)))]
       [any-dim?
        (let row ([r 0])
          (cond
            [(>= r (vterm-rows scr)) #f]
            [(let col ([c 0])
               (cond
                 [(>= c (vterm-cols scr)) #f]
                 [(let ([cell (vterm-cell scr r c)]) (and cell (cell-dim? cell))) #t]
                 [else (col (+ c 1))])) #t]
            [else (row (+ r 1))]))])
  (test-assert "menu: a single unlabelled group draws no dim header row"
    (not any-dim?)))

;; ----------------------------------------------------------------------
;; (d2) Structure, not decoration: with colour OFF the header label still renders
;; -- its glyphs are content -- and only the dim SGR drops.  A plain string port
;; is no terminal, so colour-ok? is #f: this is the NO_COLOR / TERM=dumb path.
;; ----------------------------------------------------------------------
(let ([sp (open-output-string)])
  (call-with-values
    (lambda () (render-completion-grid sp entries-2 -1 40 10 group-plan-2))
    (lambda ignored #f))
  (let ([out (get-output-string sp)])
    (test-assert "menu: a header label still renders with colour off (grouping is structure)"
      (and (substring-index out "directories") (substring-index out "files")))
    (test-assert "menu: the header's dim SGR is dropped with colour off (colour is decoration)"
      (not (substring-index out "\x1b;[2m")))))

;; ----------------------------------------------------------------------
;; (e) The selected index tracks across the header boundary.
;; Index 2 is the FIRST candidate of the SECOND group; its highlight must land on
;; the row just under the files header -- not on a header, and not on the flat
;; quotient cell a header-blind renderer would pick.
;; ----------------------------------------------------------------------
(let-values ([(gc gr) (grid-geom 40 entries-2 10 group-plan-2)])
  (let* ([plan (build-row-plan group-plan-2 gc)]
         [scr (render-grouped 40 entries-2 2 10 group-plan-2)]
         [file-row (row-of-text scr "files")]
         [dir-row (row-of-text scr "directories")]
         [sel-row (selected-row scr)])
    (let-values ([(srow scol) (row-plan-locate plan 2)])
      (test-assert "menu: the header pushes the selected row off the flat quotient cell"
        (not (= srow (quotient 2 gc))))
      (test-assert "menu: the selection lands on the row just under the files header"
        (and file-row sel-row (= sel-row (+ file-row 1))))
      (test-assert "menu: the selection never lands on a header row"
        (and dir-row file-row sel-row
             (not (= sel-row dir-row)) (not (= sel-row file-row))))
      (test-assert "menu: no header row is highlighted"
        (and dir-row file-row
             (not (row-selected? scr dir-row))
             (not (row-selected? scr file-row)))))))

;; ----------------------------------------------------------------------
;; (f) Arrow navigation crosses a header boundary and never parks on a header.
;; This pins the exact row-plan lookups cmd-completion-navigate's up/down perform:
;; from a group-1 candidate, the row directly below is a HEADER (row-plan-cell = #f)
;; and the row below that is group 2's candidate at the same column.  Down steps
;; over the header onto group 2; up steps back onto group 1.
;; ----------------------------------------------------------------------
(let-values ([(gc gr) (grid-geom 40 entries-2 10 group-plan-2)])
  (let ([plan (build-row-plan group-plan-2 gc)])
    (let-values ([(r0 c0) (row-plan-locate plan 0)])   ; a directories candidate
      (test-assert "nav: a header row sits directly below a group's candidate row"
        (not (row-plan-cell plan (+ r0 1) 0)))          ; #f == header row
      (test-assert "nav: down steps over the header onto the files group's same column"
        (eqv? (row-plan-cell plan (+ r0 2) c0) 2)))
    (let-values ([(r2 c2) (row-plan-locate plan 2)])   ; the first files candidate
      (test-assert "nav: a header row sits directly above the files candidate row"
        (not (row-plan-cell plan (- r2 1) 0)))
      (test-assert "nav: up steps back over the header onto the directories group"
        (eqv? (row-plan-cell plan (- r2 2) c2) 0)))))

;; ----------------------------------------------------------------------
;; (g) The pager's "N/M rows" M counts header rows.
;; With more rows than are visible the grid draws a pager whose denominator is the
;; header-inclusive row count -- row-plan-rows -- not the candidate-only count a
;; header-blind renderer would print.
;; ----------------------------------------------------------------------
(let-values ([(gc gr) (grid-geom 40 entries-2 2 group-plan-2)])
  (let* ([plan (build-row-plan group-plan-2 gc)]
         [scr (render-grouped 40 entries-2 -1 2 group-plan-2)]
         [want (string-append "/" (number->string (row-plan-rows plan)) " rows")])
    (test-assert "menu: the row-plan counts the two header rows into the total"
      (= gr (row-plan-rows plan)))
    (test-assert "menu: the pager denominator counts the header rows"
      (let loop ([lines (vterm-lines scr)])
        (cond
          [(null? lines) #f]
          [(substring-index (car lines) want) #t]
          [else (loop (cdr lines))])))))

;; ======================================================================
;; LS_COLORS candidate colouring -- the type-coloured grid.
;; The grid colours each candidate by its THREADED file type (never a stat, never
;; a trailing-slash guess) through the pure $LS_COLORS lookup, wrapping only the
;; validated [0-9;] SGR the lookup returns.  Every drive below PINS $LS_COLORS so
;; the colour under test is deterministic on the dev box and CI alike.
;; ======================================================================

;; ----------------------------------------------------------------------
;; (h) The built-in default palette (with $LS_COLORS pinned UNSET).
;; A directory reads bold blue (fg 4), an executable green (fg 2), a symlink cyan
;; (fg 6).  A plain file takes the "0" file colour and reads back the DEFAULT
;; foreground -- the non-vacuous anchor that keeps the three colour reads honest:
;; a header-blind or uncoloured renderer fails every one.
;; ----------------------------------------------------------------------
(let ([dir-cell  (typed-first-cell 40 "adir/" 'dir  #f)]
      [exe-cell  (typed-first-cell 40 "anexe" 'exec #f)]
      [link-cell (typed-first-cell 40 "alink" 'link #f)]
      [file-cell (typed-first-cell 40 "afile" 'file #f)])
  (test-assert "colour: an unset $LS_COLORS colours a directory bold blue"
    (and (cell-bold? dir-cell) (eqv? (cell-fg dir-cell) 4)))
  (test-assert "colour: an unset $LS_COLORS colours an executable green"
    (eqv? (cell-fg exe-cell) 2))
  (test-assert "colour: an unset $LS_COLORS colours a symlink cyan"
    (eqv? (cell-fg link-cell) 6))
  (test-equal "colour: a plain file takes no colour under the default palette (non-vacuous)"
    'default (cell-fg file-cell))
  (test-assert "colour: the plain-file cell is not bold"
    (not (cell-bold? file-cell))))

;; ----------------------------------------------------------------------
;; (i) An explicit $LS_COLORS override is honoured.  A di= type rule repaints the
;; directory away from the default blue, and a *.md= extension rule colours a
;; plain readme by its suffix.  Both pins are 256-colour so the read is an exact
;; index, and the directory index (99) differs from the default 4 -- proving the
;; override took, not the built-in.
;; ----------------------------------------------------------------------
(let ([dir-cell (typed-first-cell 40 "od/"       'dir  "di=38;5;99:*.md=38;5;213")]
      [md-cell  (typed-first-cell 40 "readme.md" 'file "di=38;5;99:*.md=38;5;213")])
  (test-assert "colour: a di= override repaints a directory away from the default blue"
    (and (eqv? (cell-fg dir-cell) 99) (not (eqv? (cell-fg dir-cell) 4))))
  (test-assert "colour: a *.md extension rule colours a plain file by its suffix"
    (eqv? (cell-fg md-cell) 213)))

;; ----------------------------------------------------------------------
;; (j) A hostile $LS_COLORS cannot repaint the screen.  ls does not sanitise the
;; value, so a di= carrying an ESC + clear-screen would wipe the terminal once
;; wrapped in an SGR -- but candidate-sgr allow-lists to [0-9;] and drops such a
;; value to #f, so the grid paints nothing and the payload never reaches the byte
;; stream.  The raw capture carries no clear-screen (indeed no ESC at all), and
;; the folded directory cell carries no colour.
;; ----------------------------------------------------------------------
(let* ([raw (with-ls-colors "di=01;34m\x1b;[2J"
              (lambda ()
                (render->string 40
                  (lambda (p)
                    (render-completion-grid p (list (cons "hostiledir/" '())) -1 40 10
                                            (list (cons #f 1)) '(dir))))))]
       [scr (let ([vt (make-vterm 40)]) (vterm-feed! vt raw) vt)]
       [cell (vterm-cell scr 1 2)])
  (test-assert "colour: a hostile clear-screen payload never reaches the byte stream"
    (not (substring-index raw (string #\x1b #\[ #\2 #\J))))
  (test-assert "colour: a hostile $LS_COLORS emits no ESC byte at all"
    (not (substring-index raw (string #\x1b))))
  (test-assert "colour: the hostile directory candidate is left uncoloured (rejected to #f)"
    (and (eq? (cell-fg cell) 'default) (not (cell-bold? cell)))))

;; ======================================================================
;; Degradation grid matrix -- narrow width and no-completer-regression.
;; The last of the header-aware grid proofs: at a too-narrow terminal the grid
;; falls to one column and an over-wide header truncates with the ellipsis, and the
;; live completers still render their typed output through the shared path.
;; ======================================================================

;; ----------------------------------------------------------------------
;; (k) Narrow width: the grid collapses to ONE column and an over-wide category
;; header truncates with the U+2026 ellipsis, while candidates keep one per row.
;; term-cols 20 with a header label far wider than the row: grid-cols comes back 1
;; (a width-blind renderer would keep several columns), the header row is left-
;; aligned at column 0 and ends in the ellipsis (truncate-display), the row-plan
;; holds exactly one row per candidate plus the two headers, and a short candidate
;; still lands at the two-space indent.  $LS_COLORS is pinned unset (render-grouped)
;; so the reads are palette-independent.
;; ----------------------------------------------------------------------
(define narrow-cols 20)
(define narrow-label "directories-in-a-very-long-category-name")
(define narrow-plan (list (cons narrow-label 2) (cons "files" 2)))
(define narrow-entries
  (list (cons "aa/" '()) (cons "subdir-2/" '())
        (cons "ff1" '()) (cons "ff2" '())))

(let-values ([(gc gr) (grid-geom narrow-cols narrow-entries 10 narrow-plan)])
  (let* ([scr (render-grouped narrow-cols narrow-entries -1 10 narrow-plan)]
         [plan (build-row-plan narrow-plan gc)]
         [header-line
          (let loop ([lines (vterm-lines scr)])
            (cond
              [(null? lines) ""]
              [(substring-index (car lines) "directories") (car lines)]
              [else (loop (cdr lines))]))])
    (test-equal "menu: a too-narrow terminal collapses the grid to a single column"
      1 gc)
    (test-equal "menu: the over-wide category header is left-aligned at column 0"
      0 (substring-index header-line "directories"))
    (test-assert "menu: the over-wide category header truncates with the U+2026 ellipsis"
      (ends-with-ellipsis? header-line))
    (test-equal "menu: with one column the row-plan holds one row per candidate plus the two headers"
      (+ 2 (length narrow-entries)) (row-plan-rows plan))
    (test-equal "menu: a short candidate still renders on its own row at the two-space indent"
      2 (needle-col scr "aa/"))))

;; ----------------------------------------------------------------------
;; (l) No completer regressed: the live filename-completions still yields TYPED
;; entries, and the grid renders them.  A throwaway directory mixing a sub-directory
;; and a plain file is completed from inside it; the sub-directory comes back typed
;; 'dir (shown with a trailing slash), the file typed 'file.  Splitting those triples
;; into (name . positions) pairs and a parallel type list -- exactly as the sink does
;; -- and feeding them through the 7-arg grid returns four bounded values with no
;; raise.  The all-directories slice, drawn as a single unlabelled group, draws no
;; header, mirroring (d) on real completer output.  The registered completers' own
;; return values live in test-completers / test-comp-dispatch; here we confirm the
;; grid draws their output.
;; ----------------------------------------------------------------------
(define (temp-dir-name tag)
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/hafod-comp-menu-" tag "-"
                 (number->string (pid)) "-"
                 (number->string (random 1000000))))

(define (entry-named cands nm)
  (let loop ([es cands])
    (cond
      [(null? es) #f]
      [(string=? (car (car es)) nm) (car es)]
      [else (loop (cdr es))])))

;; #t when ANY cell of the folded screen reads dim (a drawn category header).
(define (any-dim-cell? scr)
  (let row ([r 0])
    (cond
      [(>= r (vterm-rows scr)) #f]
      [(let col ([c 0])
         (cond
           [(>= c (vterm-cols scr)) #f]
           [(let ([cell (vterm-cell scr r c)]) (and cell (cell-dim? cell))) #t]
           [else (col (+ c 1))])) #t]
      [else (row (+ r 1))])))

(let ([root (temp-dir-name "reg")])
  (create-directory root)
  (create-directory (string-append root "/zsub"))
  (call-with-output-file (string-append root "/zfile")
    (lambda (p) (put-string p "x")))
  (dynamic-wind
    void
    (lambda ()
      (let* ([cands (with-cwd* root (lambda () (filename-completions "z")))]
             [dir-entry (entry-named cands "zsub/")]
             [file-entry (entry-named cands "zfile")])
        (test-assert "menu: filename-completions still returns both temp-dir entries"
          (and dir-entry file-entry))
        (test-assert "menu: the sub-directory candidate is typed 'dir (shown with a trailing slash)"
          (and dir-entry (eq? (caddr dir-entry) 'dir)))
        (test-assert "menu: the plain-file candidate is typed 'file"
          (and file-entry (eq? (caddr file-entry) 'file)))
        ;; Feed the completer output through the grid as the sink does: a
        ;; (name . positions) pair list and a parallel type list.
        (let ([pairs (map (lambda (e) (cons (car e) (cadr e))) cands)]
              [types (map caddr cands)])
          (let-values ([(ml gc gr so)
                        (render-completion-grid (open-output-string) pairs -1 40 10 #f types)])
            (test-assert "menu: the grid renders the live completer output without error"
              (and (>= gc 1) (>= gr 1) (>= ml 0))))
          ;; The all-directories slice as one unlabelled group draws no header.
          (let* ([dir-pairs (map (lambda (e) (cons (car e) (cadr e)))
                                 (filter (lambda (e) (eq? (caddr e) 'dir)) cands))]
                 [scr (render-grouped 40 dir-pairs -1 10 (list (cons #f (length dir-pairs))))])
            (test-assert "menu: an all-directories completion (single unlabelled group) draws no header"
              (not (any-dim-cell? scr)))))))
    (lambda ()
      (delete-file (string-append root "/zfile"))
      (delete-directory (string-append root "/zsub"))
      (delete-directory root))))

(test-end)
