;;; test/test-comp-dispatch.ss -- The end-to-end proof that the editor's Tab
;;; dispatch routes a ~-token to the login completer and a dash-token on a
;;; completer-less command to the option-flag completer, and that every candidate
;;; is control-stripped before it reaches the menu.  Each case drives the real
;;; read-expression over a scripted key string under a forced capability verdict,
;;; captures the emitted escape stream to a plain string port, and asserts on the
;;; captured bytes -- exactly as the completion-overlay cycle proof does, so the
;;; drive is deterministic and returns at once (end-of-input submits).  Entirely
;;; PTY-free: string ports throughout, no terminal, no platform gate.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor editor) read-expression completion-normalise)
        (only (hafod terminal-caps) assume-terminal-caps)
        (only (hafod shell completers) command-flag-source register-completer!)
        (only (hafod posix) posix-getpwuid posix-getuid passwd-info-name)
        (only (hafod process-state) pid)
        (only (hafod fileinfo) create-directory)
        (only (hafod syntax) run)
        (chezscheme))

(test-begin "comp-dispatch")

;; ======================================================================
;; Helpers
;; ======================================================================

;; Drive read-expression over KEYS with capabilities forced on, so it emits its
;; full escape stream to a string port with no real terminal; end-of-input
;; submits, so the call returns at once.  Returns the captured bytes.
(define (drive-capture keys)
  (let ([sp (open-output-string)])
    (parameterize ([assume-terminal-caps 'on])
      (read-expression "> " (open-input-string keys) sp))
    (get-output-string sp)))

;; Like drive-capture, but also hands back read-expression's own result -- the
;; submitted line, which is the edit buffer at end-of-input -- so a test can
;; assert on the bytes INSERTED into the buffer as well as those emitted to the
;; screen.  Returns (values captured result).
(define (drive-capture+result keys)
  (let ([sp (open-output-string)])
    (parameterize ([assume-terminal-caps 'on])
      (let ([result (read-expression "> " (open-input-string keys) sp)])
        (values (get-output-string sp) result)))))

;; Naive substring search (no srfi-13 dependency in this suite).
(define (contains-substring? hay needle)
  (let ([hl (string-length hay)]
        [nl (string-length needle)])
    (let outer ([i 0])
      (cond
        [(> (+ i nl) hl) #f]
        [(string=? (substring hay i (+ i nl)) needle) #t]
        [else (outer (+ i 1))]))))

;; Fold the captured escape stream into the virtual terminal and report whether
;; any VISIBLE cell holds a byte a control-strip must have removed: a BEL (#x07),
;; a DEL (#x7f), or a C1 introducer (#x80..#x9f).  The fold is essential: the
;; editor's own screen setup legitimately emits an OSC sequence whose terminator
;; is a BEL, and a raw whole-stream scan would flag that terminator.  The vterm
;; consumes every CSI and OSC sequence (the OSC's BEL terminator among them), so a
;; BEL/DEL/C1 that survives as a glyph cell can only be a leaked candidate byte --
;; each such byte has display width one, so a leak does land as a cell.
(define (menu-has-spoof-glyph? captured cols)
  (let ([vt (make-vterm cols)])
    (vterm-feed! vt captured)
    (let rloop ([r 0])
      (cond
        [(>= r (vterm-rows vt)) #f]
        [(let cloop ([c 0])
           (cond
             [(>= c (vterm-cols vt)) #f]
             [(let ([cell (vterm-cell vt r c)])
                (and cell
                     (let ([code (char->integer (cell-glyph cell))])
                       (or (= code #x07) (= code #x7f)
                           (and (>= code #x80) (<= code #x9f))))))
              #t]
             [else (cloop (+ c 1))]))
         #t]
        [else (rloop (+ r 1))]))))

(define TAB (string #\tab))

;; ======================================================================
;; (a) --help option flags reach the menu.
;; `demo` has no registered completer, so a dash-prefixed argument token falls to
;; the option-flag completer.  Its producer is rebound to a fixed fixture so the
;; drive spawns nothing and is deterministic.  Before this dispatch existed a
;; dash token yielded filenames, so a pre-feature tree carries neither the flag
;; nor its description -- the assertions are non-vacuous.
;; ======================================================================

(let ([captured
       (parameterize ([command-flag-source
                       (lambda (cmd) '(("--foo" . "do foo") ("--bar" . "do bar")))])
         (drive-capture (string-append "demo --" TAB)))])
  (test-assert "dispatch: a --flag on a completer-less command offers its help flags"
    (contains-substring? captured "foo"))
  (test-assert "dispatch: the flag's help description reaches the rendered menu"
    (contains-substring? captured "do foo")))

;; ======================================================================
;; (b) A ~-token completes login names.
;; The current login is enumerated and typed back one character short, so the
;; full login only appears in the capture when the ~ arm intercepts and completes
;; it -- either as the sole match inserted into the line or among the menu
;; candidates.  A pre-feature tree lacks the arm and hands ~name to the filename
;; path, which prepends $HOME and never yields the login, so it fails this.
;; ======================================================================

(let* ([me (guard (e [#t #f]) (passwd-info-name (posix-getpwuid (posix-getuid))))])
  (test-assert "dispatch: the current login resolves for the ~user drive"
    (and (string? me) (> (string-length me) 0)))
  (when (and (string? me) (> (string-length me) 0))
    ;; One character short of the full login, so a bare echo of the typed prefix
    ;; cannot satisfy the assertion; only real completion produces the whole login.
    (let* ([typed (substring me 0 (max 0 (- (string-length me) 1)))]
           [captured (drive-capture (string-append "~" typed TAB))])
      (test-assert "dispatch: a ~-token completes the current login into the capture"
        (contains-substring? captured me)))))

;; ======================================================================
;; (c) The render strip sanitises an un-sanitising completer.
;; A throwaway command is registered to a completer whose two candidates embed a
;; raw BEL in their names and a raw DEL in their descriptions and that does NOT
;; strip its own output -- the same un-sanitised class as the shipped
;; git-branches/ssh-hosts completers.  Two candidates make the menu render rather
;; than auto-inserting a lone match.  The BEL sits AFTER the point the two names
;; diverge, so their longest common prefix -- the text actually inserted -- is
;; control-free, while each full name (shown only in the menu) still carries the
;; byte.  The rendered menu must therefore carry no BEL, DEL or C1 byte, proving
;; the strip happens at the single render choke-point regardless of the completer,
;; without disturbing the byte-exact insertion checked in (d).
;; ======================================================================

(register-completer! "spoofcmd"
  (lambda (prefix ctx)
    (list (list (string #\a #\a #\; #\b #\x07 #\b) '()
                (string #\d #\e #\s #\c #\x7f #\1))
          (list (string #\a #\a #\; #\c #\x07 #\c) '()
                (string #\d #\e #\s #\c #\x7f #\2)))))

(let ([captured (drive-capture (string-append "spoofcmd x" TAB))])
  (test-assert "dispatch: the menu did render for the control-bearing completer"
    (contains-substring? captured "aa;"))
  (test-assert "dispatch: the render strip removes raw control bytes from the menu"
    (not (menu-has-spoof-glyph? captured 80))))

;; ======================================================================
;; (d) Insertion is byte-exact; the strip is display-only.
;; A completer returns two candidates that DIVERGE after an embedded TAB, so
;; their longest common prefix already carries the raw byte.  A first TAB inserts
;; that prefix and opens the menu; a second cycles onto the first full candidate,
;; inserting it whole from completion-candidates.  read-expression returns the
;; submitted buffer, which must carry the candidate's ORIGINAL bytes -- a filename
;; with a control byte names the real file, not a stripped near-miss -- while the
;; rendered menu shows the same name with the byte removed.  The menu's stripped
;; fragment "QWen" cannot come from the byte-exact line (where a TAB sits between
;; Q and W), so it witnesses the display strip on its own; a whole-screen glyph
;; scan cannot be used here because the legitimate control byte now on the edit
;; line would trip it -- which is exactly the point.
;; ======================================================================

(register-completer! "byteexact"
  (lambda (prefix ctx)
    (list (list (string #\Q #\x09 #\W #\e #\n #\d) '() #f)
          (list (string #\Q #\x09 #\W #\x #\i #\t) '() #f))))

(let-values ([(captured result)
              (drive-capture+result (string-append "byteexact Q" TAB TAB))])
  (test-assert "dispatch: a control-bearing candidate inserts its ORIGINAL bytes"
    (and (string? result)
         (contains-substring? result (string #\Q #\x09 #\W #\e #\n #\d))))
  (test-assert "dispatch: the same candidate is shown in the menu with the byte stripped"
    (contains-substring? captured "QWen")))

;; ======================================================================
;; (e) A dir-only command never leaks a file on an empty completer result.
;; cd / pushd / rmdir complete directories only, so when the directory completer
;; finds no match the editor must show nothing rather than fall through to the
;; filename list.  The fixture holds a file whose name the typed prefix
;; fuzzy-matches but no matching directory: a pre-guard tree falls back and
;; inserts the file (the submitted line grows), while the guarded editor leaves
;; the typed text byte-for-byte -- so the assertion is non-vacuous.  Absolute
;; paths in the key string keep the drive independent of the process cwd.
;; ======================================================================

(define (dispatch-temp-root tag)
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/hafod-comp-dispatch-" tag "-"
                 (number->string (pid)) "-"
                 (number->string (random 1000000))))

(let ([root (dispatch-temp-root "dironly")])
  (create-directory root)
  (dynamic-wind
    void
    (lambda ()
      (run (sh "-c" ,(string-append "echo hi > " root "/widget")))
      (let-values ([(captured result)
                    (drive-capture+result
                      (string-append "cd " root "/widge" TAB))])
        (test-assert "dispatch: cd with only a file match inserts nothing"
          (and (string? result)
               (string=? result (string-append "cd " root "/widge"))))
        (test-assert "dispatch: the unmatched filename never reaches the menu"
          (not (contains-substring? captured "widget")))))
    (lambda () (run (rm "-rf" ,root)))))

;; A real directory still completes through the same guarded path, so the guard
;; suppresses only the empty-result file fallback and not directory completion.
(let ([root (dispatch-temp-root "dirmatch")])
  (create-directory root)
  (create-directory (string-append root "/subdir"))
  (dynamic-wind
    void
    (lambda ()
      (let-values ([(captured result)
                    (drive-capture+result
                      (string-append "cd " root "/sub" TAB))])
        (test-assert "dispatch: cd still completes a real directory (trailing slash)"
          (and (string? result)
               (contains-substring? result "subdir/")))))
    (lambda () (run (rm "-rf" ,root)))))

;; ======================================================================
;; (f) The sink normalises the flat-vs-grouped completer shape ONCE.
;; completion-normalise is the single point every menu path funnels through: it
;; infers an untagged candidate's group in a fixed order, threads each candidate's
;; file type, flattens an explicit grouping in its given order with every label
;; escape-stripped, and keeps the no-description path a length-n list of #f so the
;; live menu's column count cannot silently flip.  These are direct calls -- no
;; terminal, no drive.  A pre-normalise tree lacks the export, so the whole suite
;; fails to load -- the additions are non-vacuous, and the reorder assertions below
;; would also fail any flat pass-through.
;; ======================================================================

;; A fake PATH set: only these bare names read as commands.
(define (stub-command? nm) (and (member nm '("grepcmd" "gzipcmd")) #t))

;; Flat mixed input, deliberately given in a NON-grouped order (command, file,
;; directory): the sink must regroup it into the fixed directories < files <
;; commands order, threading each candidate's colour type in lockstep.
(let-values ([(names positions descs types plan)
              (completion-normalise
                (list (cons "grepcmd" '(0))          ; command -- a bare pair
                      (list "notes.txt" '(0) 'file)  ; file -- a typed triple
                      (list "src/" '(0) 'dir))       ; directory -- a typed triple
                #f stub-command?)])
  (test-equal "sink: flat candidates regroup into directories < files < commands"
    '("src/" "notes.txt" "grepcmd") names)
  (test-equal "sink: the group-plan headers follow that fixed order"
    '(("directories" . 1) ("files" . 1) ("commands" . 1)) plan)
  (test-equal "sink: each candidate's colour type rides through in group order"
    '(dir file file) types))

;; An explicit grouping flattens in the completer's GIVEN order, each group keeping
;; its label and count.
(let-values ([(names positions descs types plan)
              (completion-normalise
                (list (cons "git branches" (list (cons "main" '(0)) (cons "dev" '(0))))
                      (cons "tags" (list (cons "v1" '(0)))))
                #f stub-command?)])
  (test-equal "sink: an explicit grouping flattens in the completer's given order"
    '("main" "dev" "v1") names)
  (test-equal "sink: each labelled group keeps its label and count, in order"
    '(("git branches" . 2) ("tags" . 1)) plan))

;; A lone inferred group draws no header (a #f label) and preserves input order.
(let-values ([(names positions descs types plan)
              (completion-normalise
                (list (cons "gzipcmd" '(0)) (cons "grepcmd" '(0)))
                #f stub-command?)])
  (test-equal "sink: a lone inferred group takes a #f label (no header)"
    '((#f . 2)) plan)
  (test-equal "sink: a one-group flat result preserves input order byte-for-byte"
    '("gzipcmd" "grepcmd") names))

;; A group label carrying a control byte is escape-stripped in the group-plan
;; before it can reach the menu.
(let-values ([(names positions descs types plan)
              (completion-normalise
                (list (cons (string #\r #\e #\m #\x07 #\o #\t #\e)
                            (list (cons "origin" '(0)))))
                #f stub-command?)])
  (test-equal "sink: a group label carrying a control byte is escape-stripped"
    '(("remote" . 1)) plan))

;; The no-description path returns descs as a length-n list of #f, NEVER '() -- one
;; slot per candidate is the sink's parallel-list contract.  The list being all #f
;; is precisely what marks a completion as carrying no REAL description, so the menu
;; tiles its candidates multi-column (see (g)); only a real description string forces
;; the aligned single-column layout.
(let-values ([(names positions descs types plan)
              (completion-normalise
                (list (list "a.txt" '(0) 'file) (list "b.txt" '(0) 'file))
                #f stub-command?)])
  (test-equal "sink: the no-description path returns a length-n list of #f (not '())"
    '(#f #f) descs))

;; With descriptions present, they permute into the group sort in lockstep with the
;; candidates (here a directory is pulled ahead of a file).
(let-values ([(names positions descs types plan)
              (completion-normalise
                (list (list "zzz.txt" '(0) 'file) (list "adir/" '(0) 'dir))
                (list "file-desc" "dir-desc")
                stub-command?)])
  (test-equal "sink: candidates sort directories before files"
    '("adir/" "zzz.txt") names)
  (test-equal "sink: descriptions permute in lockstep with the group sort"
    '("dir-desc" "file-desc") descs))

;; ======================================================================
;; (g) The live apply-completions! -> render-with-menu -> grid path tiles a
;; no-description completion across the row.  Every other suite drives
;; render-completion-grid directly, so this seam is otherwise unpinned.  Two plain
;; files (both file type -> one header-free files group, no descriptions) are
;; completed through the real read-expression, and with no real description the
;; folded menu packs BOTH candidates onto one row -- the fish/zsh-style multi-column
;; layout.  The sink's all-#f description list is NOT a description signal: only a
;; real description string collapses the grid to a single aligned column.
;;
;; The two candidates share the prefix "alp" but diverge after it, and the grid
;; always ellipsis-truncates its longest name, so the count -- not the full name --
;; is the reliable signal: a menu row is one holding "alp" but not the "xyzzy" of
;; the edit line, and the tiled layout yields exactly ONE such row.  The drive runs
;; with the temp dir as the working directory so the candidates are short relative
;; names the grid never has to truncate near the marker; the directory is restored
;; immediately afterwards.
;; ======================================================================

(let ([root (dispatch-temp-root "cols")]
      [saved (current-directory)])
  (create-directory root)
  (dynamic-wind
    void
    (lambda ()
      (run (sh "-c" ,(string-append "touch " root "/alpha " root "/alpine")))
      (let* ([captured
              (dynamic-wind
                (lambda () (current-directory root))
                (lambda () (drive-capture (string-append "xyzzy alp" TAB)))
                (lambda () (current-directory saved)))]
             [vt (make-vterm 80)]
             [_ (vterm-feed! vt captured)]
             [menu-row?
              (lambda (r)
                (let ([t (vterm-row-text vt r)])
                  (and (contains-substring? t "alp")
                       (not (contains-substring? t "xyzzy")))))]
             [menu-rows
              (let loop ([r 0] [acc '()])
                (if (>= r (vterm-rows vt))
                    (reverse acc)
                    (loop (+ r 1) (if (menu-row? r) (cons r acc) acc))))])
        (test-equal "sink: a no-description completion tiles multi-column (both files on one row)"
          1 (length menu-rows))))
    (lambda () (run (rm "-rf" ,root)))))

(test-end)
