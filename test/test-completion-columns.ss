;;; test/test-completion-columns.ss -- The multi-column completion grid guard.
;;; An ordinary Tab completion -- a filename or a command name, the common case --
;;; carries no per-candidate descriptions, and its menu must tile several short
;;; candidates across each row in the fish/zsh style rather than stack them one to
;;; a line.  The reconciliation sink returns the descriptions of a no-description
;;; completion as a length-n list of #f (its parallel-list contract), NOT the empty
;;; list; a layout that asks merely "are there any description entries?" with a
;;; bare null test misreads that all-#f sentinel as "has descriptions" and collapses
;;; the grid to a single column.
;;;
;;; The honest question is narrower still: descriptions force the single (aligned)
;;; column only when they VARY per item.  A uniform descriptions list carries no
;;; per-candidate information and must still tile -- whether it is the all-#f
;;; sentinel, the cd completer's per-directory "" (blank strings, which are truthy
;;; in Scheme and so fool a mere non-#f test), or a category tag like "branch" /
;;; "host" repeated for every candidate.  Only genuinely per-item descriptions --
;;; kill's process lines, git's per-subcommand summaries -- force single-column.
;;;
;;; This suite drives the LIVE completion path -- the sink apply-completions! (which
;;; runs the reconciliation and sets the menu state) and the column predictor
;;; completion-grid-dimensions the drawn menu lays out with -- and, end to end, a
;;; real filename-completion Tab through the shipped renderer folded onto a virtual
;;; screen.  It deliberately does NOT hand render-completion-grid pre-built pairs
;;; the way the geometry suites do: that path bypasses the sink, which is exactly
;;; where the all-#f sentinel is minted, so those suites never exercised it.
;;; Entirely PTY-free: synthetic gap buffers, a string-fed reader, and the folded
;;; virtual terminal, so it opens no terminal and needs no platform gate.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor gap-buffer)
              make-gap-buffer gap-buffer-insert-string!)
        (only (hafod editor editor)
              apply-completions! completion-active? completion-normalise
              completion-grid-dimensions completion-picker? read-expression)
        (only (hafod shell completers) cd-completer)
        (only (hafod environment) setenv getenv)
        (only (hafod fileinfo) create-directory)
        (only (hafod process-state) with-cwd* pid)
        (except (chezscheme) getenv))

(test-begin "completion-columns")

;; A gap buffer holding TEXT with the cursor at the end, ready to complete the
;; trailing word.  apply-completions! reads the buffer string and cursor itself.
(define (buffer-with text)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert-string! gb text)
    gb))

;; Six short candidates carrying NO descriptions -- the shape a filename or command
;; completion produces.  Bare (name . positions) pairs with distinct first letters,
;; so the reconciliation keeps them in one unlabelled group in input order and the
;; longest common prefix is empty.
(define short-candidates
  (list (cons "aa" '()) (cons "bb" '()) (cons "cc" '())
        (cons "dd" '()) (cons "ee" '()) (cons "ff" '())))

;; ======================================================================
;; (a) The sink's no-description sentinel -- the root-cause characterisation.
;; completion-normalise fills the descriptions with a length-n list of #f (never
;; '()) when the completer supplies none.  This is the precise shape a bare null
;; test misreads; pinning it here documents why the column layout must ask for a
;; REAL description rather than merely a non-empty list.
;; ======================================================================
(let-values ([(names positions ndescs types plan)
              (completion-normalise short-candidates #f (lambda (nm) #f))])
  (test-equal "normalise: one description slot per candidate"
    (length short-candidates) (length ndescs))
  (test-assert "normalise: a no-description completion fills the slots with #f, not '()"
    (and (pair? ndescs) (for-all not ndescs)))
  (test-equal "normalise: the candidate names survive in input order"
    '("aa" "bb" "cc" "dd" "ee" "ff") names))

;; ======================================================================
;; (b) THE REGRESSION: short descriptionless candidates lay out multi-column.
;; Drive the live sink (apply-completions!, the picker off -- the shipped default),
;; then read back the geometry the drawn menu uses at a fixed 80 columns.  With no
;; real description the grid must tile several candidates per row, so grid-cols is
;; at least two.  On the pre-fix tree the all-#f description list forced a single
;; column and grid-cols came back 1, so this assertion fails there: non-vacuous.
;; ======================================================================
(let ([gb (buffer-with "ls xx")])
  (parameterize ([completion-picker? #f])
    (apply-completions! gb "ls xx" 5 3 short-candidates))
  (test-assert "menu: the inline grid is populated"
    (completion-active?))
  (let-values ([(grid-cols grid-rows) (completion-grid-dimensions 80)])
    (test-assert "menu: short descriptionless candidates tile several to a row (grid-cols >= 2)"
      (>= grid-cols 2))))

;; ======================================================================
;; (c) The description layout is NOT regressed: VARYING descriptions stay
;; single-column.  The same sink handed a parallel list of per-item description
;; strings -- each candidate's differs from the next -- must keep one candidate per
;; row so the descriptions can align beneath one another, so grid-cols is 1.  This
;; is the genuinely per-item case (kill's process lines, git-subcommand summaries);
;; a fix that forced multi-column unconditionally would fail here, so it guards the
;; other direction.
;; ======================================================================
(let ([gb (buffer-with "cd xx")]
      [desc-pairs (list (cons "aa" '()) (cons "bb" '()) (cons "cc" '()))]
      [descs (list "the first thing" "the second thing" "the third thing")])
  (parameterize ([completion-picker? #f])
    (apply-completions! gb "cd xx" 5 3 desc-pairs descs))
  (let-values ([(grid-cols grid-rows) (completion-grid-dimensions 80)])
    (test-equal "menu: a completion carrying varying per-item descriptions stays single-column"
      1 grid-cols)))

;; ======================================================================
;; (e) THE AUDIT'S EXACT REPRO -- the cd completer's uniform BLANK descriptions.
;; A `cd <tab>` on a directory of several subdirectories offers each as a candidate
;; carrying the SAME empty-string description (a cwd listing's desc is ""), and ""
;; is truthy in Scheme; a layout that forces a single column on any non-#f
;; description therefore collapses the cd menu to one directory per row.  Blank
;; descriptions carry no per-item information, so the menu must tile them.  This
;; drives the LIVE cd-completer with $CDPATH neutralised (so every candidate is a
;; cwd "" -- exactly the un-CDPATH'd cd the user hit), splits its (name positions
;; desc) triples into the pairs + descs the editor's dispatch feeds the sink, and
;; reads the drawn geometry back at a fixed 80 columns: grid-cols must be at least
;; two.  On the pre-fix tree the uniform "" forced a single column and grid-cols
;; came back 1, so this fails there: non-vacuous.
;; ======================================================================
(define cd-root
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/hafod-cd-cols-" (number->string (pid))
                 "-" (number->string (random 1000000))))
(define cd-subdirs '("sable" "sabre" "sachet" "saddle" "safari" "saffron"))

(create-directory cd-root)
(for-each (lambda (d) (create-directory (string-append cd-root "/" d))) cd-subdirs)

(let ([saved-cdpath (getenv "CDPATH")])
  (dynamic-wind
    (lambda () (setenv "CDPATH" #f))   ; drop $CDPATH: every candidate is a cwd ""
    (lambda ()
      (with-cwd* cd-root
        (lambda ()
          (let* ([triples (cd-completer "s" (list (cons 'command "cd")))]
                 ;; Split the completer's triples into pairs + descs exactly as the
                 ;; editor's completion dispatch does before the sink.
                 [pairs (map (lambda (e) (cons (car e) (cadr e))) triples)]
                 [descs (map (lambda (e) (caddr e)) triples)]
                 [gb (buffer-with "cd s")])
            (test-assert "cd: the live completer offered several subdirectories"
              (>= (length triples) 2))
            (test-assert "cd: every subdirectory candidate carries the same blank description"
              (for-all (lambda (d) (equal? d "")) descs))
            (parameterize ([completion-picker? #f])
              (apply-completions! gb "cd s" 4 3 pairs descs))
            (let-values ([(grid-cols grid-rows) (completion-grid-dimensions 80)])
              (test-assert "cd: blank-description directories tile several to a row (grid-cols >= 2)"
                (>= grid-cols 2)))))))
    (lambda ()
      (setenv "CDPATH" saved-cdpath)
      (for-each (lambda (d) (delete-directory (string-append cd-root "/" d))) cd-subdirs)
      (delete-directory cd-root))))

;; ======================================================================
;; (f) UNIFORM CATEGORY LABELS -- git-branch / ssh-host style.  The git-branch, ssh,
;; git-remote and git-modified completers tag EVERY candidate with the same category
;; word ("branch", "host", "remote", "modified") -- a label that names the kind of
;; thing completed, not anything about the individual item.  A uniform tag carries
;; no per-candidate information, so its menu must tile like a bare filename list; a
;; layout that forced single-column on any non-#f description would stack these one
;; to a row.  Feeding the sink the short candidates with a descs list that is the
;; same "branch" for every one, grid-cols must be at least two.  On the pre-fix tree
;; the repeated tag forced a single column, so this fails there: non-vacuous.
;; ======================================================================
(let ([gb (buffer-with "git checkout ")]
      [uniform-descs (map (lambda (c) "branch") short-candidates)])
  (parameterize ([completion-picker? #f])
    (apply-completions! gb "git checkout " 13 13 short-candidates uniform-descs))
  (let-values ([(grid-cols grid-rows) (completion-grid-dimensions 80)])
    (test-assert "menu: candidates under one repeated category label tile several to a row (grid-cols >= 2)"
      (>= grid-cols 2))))

;; ======================================================================
;; (d) End to end through the drawn menu: a real filename Tab draws several
;; candidates per row.  A throwaway directory of eight short files is completed in
;; string context and the shipped renderer draws the menu; folded onto a virtual
;; screen the whole candidate list must land on FEWER rows than there are
;; candidates -- the drawn proof of the tiled layout, not merely the predictor's.
;; The fold is taken wide so the drawn row cannot wrap whatever width the editor
;; queried.  Single-column (the pre-fix behaviour) draws one candidate per row --
;; eight rows -- so the row-count assertion fails on that tree.
;; ======================================================================
(define file-names '("aa" "ab" "ac" "ad" "ae" "af" "ag" "ah"))
(define tmp-root
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/hafod-cols-" (number->string (pid))
                 "-" (number->string (random 1000000))))

(create-directory tmp-root)
(for-each (lambda (n)
            (call-with-output-file (string-append tmp-root "/" n)
              (lambda (p) (put-string p "x"))))
          file-names)

(dynamic-wind
  void
  (lambda ()
    (with-cwd* tmp-root
      (lambda ()
        (let* ([scr (render->screen 1000
                      (lambda (p)
                        (read-expression "> "
                          (open-input-string (string-append "\"a" (string #\tab)))
                          p)))]
               ;; Non-empty folded rows less the single prompt row: the rows the
               ;; menu itself drew.  A tiled menu packs the eight files onto one
               ;; row; a single-column menu would need eight.
               [nonempty
                (let loop ([lines (vterm-lines scr)] [n 0])
                  (cond
                    [(null? lines) n]
                    [(string=? (car lines) "") (loop (cdr lines) n)]
                    [else (loop (cdr lines) (+ n 1))]))]
               [menu-rows (- nonempty 1)])
          (test-assert "menu: the drawn menu packs the files onto fewer rows than candidates"
            (< menu-rows (length file-names)))))))
  (lambda ()
    (for-each (lambda (n) (delete-file (string-append tmp-root "/" n))) file-names)
    (delete-directory tmp-root)))

(test-end)
