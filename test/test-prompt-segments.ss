#!chezscheme
;;; test-prompt-segments.ss -- The pluggable prompt segment model.  A segment is a
;;; bare (placement . thunk) pair held in the ordered prompt-segments parameter,
;;; with register (append -> draw order) and clear.  The placement-aware renderers
;;; space-join the left segments into the info line, break a line segment onto its
;;; own row, and feed the right segments to the right prompt; a per-draw context
;;; carries the live exit / duration / cwd / colour depth / glyph tier / width to
;;; every thunk.  The exit-coloured input glyph (❯ green after a success, red after
;;; a failure, ASCII '>' on a weak glyph terminal) is the first line segment.  All
;;; PTY-free: the model proofs render to a string port and the glyph/two-line proofs
;;; fold the output into the (test vterm) cell grid.
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (except (chezscheme) getenv)
        (test runner)
        (test vterm)
        (only (hafod interactive)
              prompt-segments register-prompt-segment! clear-prompt-segments!
              make-current-prompt-ctx render-prompt-segments
              render-right-prompt-segments prompt-char-segment
              prompt-ctx-exit prompt-ctx-duration prompt-ctx-width
              terminal-width
              last-status last-duration
              prompt-customised? enable-informative-prompt! repl-prompt-hook
              should-install-default-prompt? informative-prompt?
              interactive-enhancements?)
        (only (hafod config) set-prompt!)
        (only (hafod environment) with-env* setenv)
        (only (hafod terminal-caps) glyph-tier glyph-tier-override colour-override))

(test-begin "prompt-segments")

;; Substring search (the runner carries no string-contains).
(define (string-contains? haystack needle)
  (let ([hlen (string-length haystack)] [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

;; True when STR carries no ESC (#x1b) byte -- the invariant a plain, uncoloured
;; segment must satisfy under a mono colour verdict.
(define (no-esc? str)
  (let loop ([i 0])
    (cond
      [(>= i (string-length str)) #t]
      [(= (char->integer (string-ref str i)) #x1b) #f]
      [else (loop (+ i 1))])))

;; Render the left/line segments to a string with a fresh per-draw context.
(define (left-render)
  (let ([p (open-output-string)])
    (render-prompt-segments p (make-current-prompt-ctx))
    (get-output-string p)))

;; Render the right segments to a string with a fresh context.
(define (right-render)
  (let ([p (open-output-string)])
    (render-right-prompt-segments p (make-current-prompt-ctx))
    (get-output-string p)))

;; === Segment model: an ordered (placement . thunk) list, register / clear ===

;; Append order is draw order: two left thunks space-join into "A B".
(clear-prompt-segments!)
(register-prompt-segment! 'left (lambda (c) "A"))
(register-prompt-segment! 'left (lambda (c) "B"))
(test-assert "model: left segments space-join in registration order"
  (string-contains? (left-render) "A B"))

;; A left thunk returning #f is skipped, leaving no stray separator: "A C".
(clear-prompt-segments!)
(register-prompt-segment! 'left (lambda (c) "A"))
(register-prompt-segment! 'left (lambda (c) #f))
(register-prompt-segment! 'left (lambda (c) "C"))
(test-equal "model: a #f left segment is skipped with no stray separator"
  "A C" (left-render))

;; A left thunk returning "" is likewise dropped -- no double space.
(clear-prompt-segments!)
(register-prompt-segment! 'left (lambda (c) "A"))
(register-prompt-segment! 'left (lambda (c) ""))
(register-prompt-segment! 'left (lambda (c) "C"))
(test-equal "model: an empty left segment leaves no double separator"
  "A C" (left-render))

;; clear empties the list: nothing renders.
(clear-prompt-segments!)
(register-prompt-segment! 'left (lambda (c) "GONE"))
(clear-prompt-segments!)
(test-equal "model: clear-prompt-segments! empties the list" "" (left-render))

;; Placement routing: a right segment appears only in the right render, a left
;; segment only in the left; neither leaks into the other.
(clear-prompt-segments!)
(register-prompt-segment! 'left  (lambda (c) "LEFTSEG"))
(register-prompt-segment! 'right (lambda (c) "RIGHTSEG"))
(test-assert "model: a right segment is absent from the left render"
  (not (string-contains? (left-render) "RIGHTSEG")))
(test-assert "model: a right segment shows in the right render"
  (string-contains? (right-render) "RIGHTSEG"))
(test-assert "model: a left segment shows in the left render"
  (string-contains? (left-render) "LEFTSEG"))
(test-assert "model: a left segment is absent from the right render"
  (not (string-contains? (right-render) "LEFTSEG")))

;; A line segment renders on its own row: its text follows a newline (the two-line
;; break) and the info line precedes that break.
(clear-prompt-segments!)
(register-prompt-segment! 'left (lambda (c) "INFO"))
(register-prompt-segment! 'line (lambda (c) "LINESEG"))
(let ([s (left-render)])
  (test-assert "model: a line segment renders after a newline"
    (string-contains? s "\nLINESEG"))
  (test-assert "model: the info line precedes the line break"
    (string-contains? s "INFO\n")))

;; A line-only prompt (no left/info segments) must NOT emit a leading blank line:
;; the first line segment starts on the current row.  On the pre-fix tree
;; render-prompt-segments wrote a newline before EVERY line segment unconditionally,
;; so a line-only prompt rendered "\nGLYPH" -- a blank first row above the input
;; glyph -- and the equality below (which expects no leading newline) failed:
;; non-vacuous.
(clear-prompt-segments!)
(register-prompt-segment! 'line (lambda (c) "GLYPH"))
(test-equal "layout: a line-only prompt has no leading blank line"
  "GLYPH" (left-render))

;; Two line-only segments join with a single separating newline and still no leading
;; blank line.
(clear-prompt-segments!)
(register-prompt-segment! 'line (lambda (c) "A"))
(register-prompt-segment! 'line (lambda (c) "B"))
(test-equal "layout: line-only segments join with no leading blank line"
  "A\nB" (left-render))

;; With left/info content present the line segment still breaks onto its own row --
;; the newline before it is suppressed only when there is nothing above it.
(clear-prompt-segments!)
(register-prompt-segment! 'left (lambda (c) "INFO"))
(register-prompt-segment! 'line (lambda (c) "GLYPH"))
(test-equal "layout: with info content the line segment breaks onto its own row"
  "INFO\nGLYPH" (left-render))

;; A right thunk returning #f is dropped from the right render too.
(clear-prompt-segments!)
(register-prompt-segment! 'right (lambda (c) "R1"))
(register-prompt-segment! 'right (lambda (c) #f))
(register-prompt-segment! 'right (lambda (c) "R2"))
(test-equal "model: a #f right segment leaves no stray separator"
  "R1 R2" (right-render))

;; register validates placement and thunk: a bogus placement or a non-procedure is
;; rejected rather than silently corrupting the draw list.
(test-error "model: register rejects an unknown placement"
  (register-prompt-segment! 'middle (lambda (c) "x")))
(test-error "model: register rejects a non-procedure thunk"
  (register-prompt-segment! 'left "not-a-thunk"))

;; The parameter validates its whole value: a non-(placement . thunk) element is
;; rejected at the boundary.
(test-error "model: prompt-segments rejects a malformed element"
  (prompt-segments (list (cons 'left "not-a-thunk"))))

;; make-current-prompt-ctx captures the live draw state: a thunk reading the ctx
;; sees the live last-status and last-duration for that draw.
(clear-prompt-segments!)
(register-prompt-segment! 'left
  (lambda (c) (number->string (prompt-ctx-exit c))))
(parameterize ([last-status 7])
  (test-equal "ctx: the context carries the live exit into the thunk"
    "7" (left-render)))
(clear-prompt-segments!)
(register-prompt-segment! 'left
  (lambda (c) (number->string (prompt-ctx-duration c))))
(parameterize ([last-duration 1234])
  (test-equal "ctx: the context carries the live duration into the thunk"
    "1234" (left-render)))

;; === A left segment is budgeted against what the ROW has left ===
;;
;; The width the context carries is a BUDGET, and the info line is the left
;; segments space-joined onto one row.  A segment that budgets against the whole
;; terminal while sharing that row with others is not budgeted at all: the group
;; of language versions took a fixed share of eighty columns however much the
;; forty-column path and the whole branch name before it had already spent, so the
;; row ran to some ninety-five columns, wrapped onto a second one, and took the
;; right prompt's single-row cursor positioning with it -- the very failure
;; budgeting the group was written to prevent.
;;
;; The first left segment still sees the whole row; the next sees what is left of
;; it, the joining space included.  A width-blind context hands the second segment
;; eighty and the row overflows: the last two rows are non-vacuous.
(clear-prompt-segments!)
(let ([seen #f])
  (register-prompt-segment! 'left (lambda (c) (set! seen (prompt-ctx-width c)) "X"))
  (parameterize ([terminal-width 80]) (left-render))
  (test-equal "budget: the first left segment is handed the whole row" 80 seen))

(clear-prompt-segments!)
(let ([seen #f])
  (register-prompt-segment! 'left (lambda (c) (make-string 70 #\-)))
  (register-prompt-segment! 'left (lambda (c) (set! seen (prompt-ctx-width c)) ""))
  (parameterize ([terminal-width 80]) (left-render))
  (test-equal "budget: a later left segment is handed the columns the row has left"
    9 seen))

(clear-prompt-segments!)
(register-prompt-segment! 'left (lambda (c) (make-string 70 #\-)))
(register-prompt-segment! 'left
  (lambda (c) (if (>= (prompt-ctx-width c) 15) (make-string 15 #\=) "")))
(parameterize ([terminal-width 80])
  (test-assert "budget: a self-trimming left segment keeps the info line on one row"
    (<= (string-length (left-render)) 80)))

;; The right placement is positioned from the far edge and the line placement puts
;; each segment on its own row, so neither narrows: both still see the whole width.
(clear-prompt-segments!)
(let ([seen #f])
  (register-prompt-segment! 'right (lambda (c) (make-string 30 #\-)))
  (register-prompt-segment! 'right (lambda (c) (set! seen (prompt-ctx-width c)) "R"))
  (parameterize ([terminal-width 80]) (right-render))
  (test-equal "budget: a right segment is not narrowed by the one before it"
    80 seen))

;; === Segment isolation: a raising segment thunk must not break the draw ===
;;
;; Segments are a public extension point, so a thunk may be arbitrary user code.  A
;; raise inside one is dropped like a #f result -- the segment is skipped and the
;; surrounding segments still render -- rather than propagating out of the draw and
;; disrupting the REPL.  On the pre-fix tree the raise escaped segment-outputs, so
;; left-render itself raised and the equality below was never reached: non-vacuous.
(clear-prompt-segments!)
(register-prompt-segment! 'left (lambda (c) "BEFORE"))
(register-prompt-segment! 'left (lambda (c) (error 'seg "boom")))
(register-prompt-segment! 'left (lambda (c) "AFTER"))
(test-equal "isolation: a raising left segment is skipped, the neighbours render"
  "BEFORE AFTER" (left-render))

;; A raising right segment is isolated the same way -- both placements route through
;; segment-outputs -- so the other right segment still renders.
(clear-prompt-segments!)
(register-prompt-segment! 'right (lambda (c) (error 'seg "boom")))
(register-prompt-segment! 'right (lambda (c) "RSAFE"))
(test-equal "isolation: a raising right segment is skipped, the neighbour renders"
  "RSAFE" (right-render))

;; === The exit-coloured input glyph + the two-line layout ===
;;
;; render->screen forces the terminal-capability verdict on, so the segment's
;; gated colour reaches the cell grid; the prompt char is the first glyph on the
;; row below the info line, so it is located at (row 1, column 0) -- a position
;; that holds whether the glyph is ❯ (emoji tier) or '>' (ascii tier).

;; Render the left + line segments into a fresh COLS-wide screen with a live ctx.
(define (screen-of cols)
  (render->screen cols
    (lambda (p) (render-prompt-segments p (make-current-prompt-ctx)))))

;; The default info + input-glyph layout: an info left segment and the ❯ as the
;; single line segment.
(clear-prompt-segments!)
(register-prompt-segment! 'left (lambda (c) "info"))
(register-prompt-segment! 'line prompt-char-segment)

;; After a successful last command the prompt char is the green 256-index (2).
(parameterize ([last-status 0])
  (let ([scr (screen-of 80)])
    (test-equal "char: green (256 index 2) after exit 0"
      2 (cell-fg (vterm-cell scr 1 0)))))

;; After a non-zero exit it is the red 256-index (1) -- so a fixed-colour glyph
;; fails one of these two cases.
(parameterize ([last-status 1])
  (let ([scr (screen-of 80)])
    (test-equal "char: red (256 index 1) after a non-zero exit"
      1 (cell-fg (vterm-cell scr 1 0)))))

;; Under the ascii glyph tier the prompt char degrades to '>' (never a Nerd Font):
;; a tier-blind glyph fails this case.
(parameterize ([last-status 0] [glyph-tier-override 'ascii])
  (let ([scr (screen-of 80)])
    (test-equal "char: degrades to ASCII '>' under the ascii glyph tier"
      #\> (cell-glyph (vterm-cell scr 1 0)))))

;; Under the emoji tier (the default) the glyph is the heavy right angle ❯
;; (U+276F).  Guarded on the live tier so it asserts ❯ only where emoji is the
;; verdict, never on a terminal that legitimately falls to ascii.
(when (eq? (glyph-tier) 'emoji)
  (parameterize ([last-status 0])
    (let ([scr (screen-of 80)])
      (test-equal "char: the heavy right angle glyph under the emoji tier"
        #\x276f (cell-glyph (vterm-cell scr 1 0))))))

;; Under a mono / false colour verdict the segment emits the glyph plain -- no ESC
;; byte.  Driven by a direct string render (not render->screen, which forces caps
;; on): colour-override 'never makes the injectable verdict false.
(parameterize ([last-status 1] [colour-override 'never])
  (test-assert "char: no SGR (no ESC) under a mono colour verdict"
    (no-esc? (prompt-char-segment (make-current-prompt-ctx)))))

;; The two-line layout: the info segment on grid row 0, the prompt char on row 1
;; (the line segment's newline advances the vterm row).  A single-line
;; implementation would put the char on row 0 and leave no row 1 -> fails both the
;; row-0 equality and the row-count assertion.
(parameterize ([last-status 0])
  (let ([scr (screen-of 80)])
    (test-equal "layout: the info segment occupies row 0"
      "info" (vterm-row-text scr 0))
    (test-assert "layout: a second row exists for the prompt char"
      (> (vterm-rows scr) 1))
    (test-assert "layout: the prompt char sits on row 1"
      (let ([c (vterm-cell scr 1 0)])
        (and c (not (char=? (cell-glyph c) #\space)))))))

;; A right segment shares the info row (row 0) while the line segment's char is on
;; row 1 -- the placement model routes right + left to the info row and line
;; below.  (The right prompt's right-EDGE positioning is display-right-prompt's
;; own job; here the two renderers are composed to prove the row routing.)
(clear-prompt-segments!)
(register-prompt-segment! 'right (lambda (c) "RSEG"))
(register-prompt-segment! 'left  (lambda (c) "info"))
(register-prompt-segment! 'line  prompt-char-segment)
(parameterize ([last-status 0])
  (let ([scr (render->screen 80
               (lambda (p)
                 (render-right-prompt-segments p (make-current-prompt-ctx))
                 (render-prompt-segments p (make-current-prompt-ctx))))])
    (test-assert "layout: a right segment shares row 0 with the info line"
      (and (string-contains? (vterm-row-text scr 0) "RSEG")
           (string-contains? (vterm-row-text scr 0) "info")))
    (test-assert "layout: the prompt char is on row 1, not the info row"
      (and (> (vterm-rows scr) 1)
           (let ([c (vterm-cell scr 1 0)])
             (and c (not (char=? (cell-glyph c) #\space))))))))

;; === The pristine-prompt guard ===
;;
;; prompt-customised? records whether the user has taken over the prompt -- via
;; set-prompt! or a direct hook rebind.  A pristine start is #f (the converters
;; running on their construction defaults do not self-trip; the guard is armed only
;; afterwards).  The preset installs its own hooks with the guard DISARMED, so
;; composing the default prompt must not itself look like a user customisation --
;; while a genuine user customisation must.

;; A pristine start: nothing has taken over the prompt.
(prompt-customised? #f)
(test-assert "guard: a pristine start is not customised"
  (not (prompt-customised?)))

;; A user calling the preset directly in init.ss (guard armed) DOES mark the prompt
;; customised: its leading clear-prompt-segments! and the segment registrations trip
;; the armed guard, so the REPL-entry default-on will not re-invoke the preset with
;; default options and override the user's explicit ones.  (Default-on installs the
;; preset with the guard disarmed at ITS call site, so its own composition stays
;; unmarked -- the survival chain proven at the end of this suite.)
(prompt-customised? #f)
(enable-informative-prompt!)
(test-assert "guard: a direct preset call (armed) marks the prompt customised"
  (prompt-customised?))

;; A user's set-prompt! trips the armed converter (through repl-prompt-string).
(prompt-customised? #f)
(set-prompt! "x> ")
(test-assert "guard: set-prompt! marks the prompt customised"
  (prompt-customised?))

;; A direct repl-prompt-hook rebind trips it too.
(prompt-customised? #f)
(repl-prompt-hook (lambda () (void)))
(test-assert "guard: a direct repl-prompt-hook rebind marks customised"
  (prompt-customised?))

;; === The default-on decision ===
;;
;; should-install-default-prompt? is the REPL-entry decision minus the real-terminal
;; gate (the REPL adds use-editor? at the install site, so -c/-s/pipe/non-tty install
;; nothing).  It is #t only for a pristine, enhanced, enabled session with
;; HAFOD_PROMPT unset or non-falsy; a customised prompt, the opt-out parameter, the
;; master gate off, and a falsy HAFOD_PROMPT each independently force it #f.

;; Baseline: HAFOD_PROMPT unset, a pristine + enhanced + enabled state -> install.
(setenv "HAFOD_PROMPT" #f)
(prompt-customised? #f)
(test-assert "default-on: pristine + enhanced + enabled installs by default"
  (should-install-default-prompt?))

;; A customised prompt suppresses it (the survival guarantee at the decision level).
(setenv "HAFOD_PROMPT" #f)
(test-assert "default-on: a customised prompt suppresses the install"
  (parameterize ([prompt-customised? #t])
    (not (should-install-default-prompt?))))

;; The opt-out parameter suppresses it without the user setting their own prompt.
(prompt-customised? #f)
(test-assert "default-on: (informative-prompt? #f) suppresses the install"
  (parameterize ([informative-prompt? #f])
    (not (should-install-default-prompt?))))

;; The master gate off suppresses it (a plain shell never gets the prompt).
(prompt-customised? #f)
(test-assert "default-on: (interactive-enhancements? #f) suppresses the install"
  (parameterize ([interactive-enhancements? #f])
    (not (should-install-default-prompt?))))

;; A falsy HAFOD_PROMPT suppresses it even when pristine and enabled -- the whole
;; accepted spelling set, case-insensitively, and its complement.  The gate reads
;; the same shared predicate as the version group's own opt-out, so the two agree
;; by construction rather than by two copies of this list happening to match; the
;; sweep is what keeps that true if the set is ever widened.
(for-each
  (lambda (v)
    (prompt-customised? #f)
    (test-assert (string-append "default-on: HAFOD_PROMPT=\"" v "\" suppresses the install")
      (with-env* (list (cons "HAFOD_PROMPT" v))
        (lambda () (not (should-install-default-prompt?))))))
  '("0" "false" "no" "NO" "False" ""))
(for-each
  (lambda (v)
    (prompt-customised? #f)
    (test-assert (string-append "default-on: HAFOD_PROMPT=\"" v "\" leaves the install on")
      (with-env* (list (cons "HAFOD_PROMPT" v))
        (lambda () (should-install-default-prompt?)))))
  '("1" "yes" "true"))

;; Survival: a user's own prompt hook is never clobbered.  Setting a sentinel hook
;; trips the pristine guard, so the decision is #f; a guarded install therefore
;; leaves the sentinel in place.  A flag-ignoring install would replace the hook and
;; fail the eq? check -- so this is non-vacuous.
(let ([sentinel (lambda () (void))])
  (setenv "HAFOD_PROMPT" #f)
  (prompt-customised? #f)
  (repl-prompt-hook sentinel)
  (test-assert "default-on: a customised prompt keeps the decision #f"
    (not (should-install-default-prompt?)))
  (when (should-install-default-prompt?)
    (enable-informative-prompt!))
  (test-assert "default-on: the user's prompt hook survives the decision"
    (eq? sentinel (repl-prompt-hook))))

;; === Default-on must not clobber an init.ss segment / preset composition ===
;;
;; The public knobs an init.ss composes a prompt from -- register-prompt-segment!,
;; clear-prompt-segments!, the prompt-segments setter and the enable-informative-
;; prompt! preset -- must mark the prompt customised while the guard is armed, so
;; the REPL-entry default-on skips rather than wiping the user's composition.  The
;; preset installed by default-on itself runs with the guard disarmed at its call
;; site, so its own composition stays unmarked (the pristine leg proven above).

;; A segment registered in init.ss (guard armed) marks the prompt customised, so
;; should-install-default-prompt? is #f and the segment survives the default-on
;; decision.  On the pre-fix tree register did not trip the guard, so the decision
;; stayed #t and default-on's leading clear-prompt-segments! wiped the segment --
;; each assertion below then goes red, so this oracle is non-vacuous.
(let ([sentinel (lambda (ctx) "MYSEG")])
  (setenv "HAFOD_PROMPT" #f)
  (clear-prompt-segments!)
  (prompt-customised? #f)                 ; reset AFTER the clear (which itself trips armed)
  (register-prompt-segment! 'right sentinel)
  (test-assert "default-on: register-prompt-segment! (armed) marks the prompt customised"
    (prompt-customised?))
  (test-assert "default-on: a user-registered segment keeps the decision #f"
    (not (should-install-default-prompt?)))
  (when (should-install-default-prompt?)
    (enable-informative-prompt!))
  (test-assert "default-on: the user's registered segment survives the decision"
    (find (lambda (s) (eq? (cdr s) sentinel)) (prompt-segments))))

;; Configuring the preset in init.ss with a non-default option (guard armed) marks
;; the prompt customised, so default-on does NOT re-invoke the preset with default
;; options and override the user's opt-out.  On the pre-fix tree the preset's
;; clear/register did not trip the guard, so the decision stayed #t and default-on
;; re-enabled the git/timing segments the user had switched off -- non-vacuous.
(setenv "HAFOD_PROMPT" #f)
(prompt-customised? #f)
(enable-informative-prompt! 'git #f 'timing #f)
(test-assert "default-on: enable-informative-prompt! (armed) marks the prompt customised"
  (prompt-customised?))
(test-assert "default-on: a user preset call keeps the decision #f"
  (not (should-install-default-prompt?)))

(test-end)
