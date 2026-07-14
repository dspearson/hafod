;;; test/test-terminal-resize.ss -- One kernel query per terminal resize.
;;; A resize used to ask the kernel for the window size TWICE: once to republish
;;; the width parameter and once to refresh the width cache -- two ioctls, two
;;; foreign-alloc/free round trips, one answer's worth of information.  Both
;;; consumers are now fed from a single query.
;;;
;;; No assertion about the ANSWER can tell those two trees apart: both report the
;;; same width.  The only proof is to COUNT the queries, so every window-size
;;; ioctl is routed through one hook (winsize-ioctl) and this suite stubs it -- to
;;; count the calls, and to answer with a size of its choosing.  The kernel
;;; therefore never runs: the suite opens no terminal, needs no pseudo-terminal
;;; and no platform gate, and is deterministic wherever it is run.
;;;
;;; The counting cuts BOTH ways, and this suite holds both lines.  Collapsing a
;;; query away is right only where the answer is already in hand: a resize, and
;;; the finder's entry, hold one and must ask exactly ONCE.  A caller that holds
;;; none must still ask -- so the public width query, which any script may call
;;; outside the REPL where nothing ever refreshes a cache, must ask EVERY time.
;;; Fewer queries is not a uniform good; a query collapsed on the wrong side of
;;; that line is a reader frozen at an answer that has since stopped being true.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test poll)
        (only (hafod interactive)
              terminal-width query-terminal-width
              refresh-terminal-width! install-resize-handler!)
        (only (hafod tty)
              winsize-ioctl cached-terminal-size set-terminal-size-cache! tty?)
        (only (hafod finder) run-finder)
        (only (hafod terminal-caps) assume-terminal-caps)
        (only (hafod signal) set-signal-handler!)
        (only (hafod posix) SIGWINCH posix-kill posix-getpid)
        (chezscheme))

(test-begin "terminal-resize")

;; The kernel's stand-in.  Returning rc 0 means "this fd answered", which stops
;; the window-size query's fd probe at the FIRST fd it tries -- so one query is
;; exactly one call here, whatever the harness's terminals happen to be.  42x137
;; is a size no live query in this process could produce (off a non-terminal the
;; answer is the 24x80 fallback), so a width of 137 can only have come from here.
(define queries 0)

(define (counting-winsize-ioctl fd p)
  (set! queries (+ queries 1))
  (values 0 42 137))

;; The same stand-in, but for a window whose size CHANGES under the caller's feet
;; -- which is the only thing that can tell a live reader from a cached one.  A
;; reader asked once, against a kernel that always answers the same, looks identical
;; either way; both report 137.  So the width this one reports is a cell the case
;; below rewrites mid-run, exactly as a user dragging a window edge would: a reader
;; that asks every time follows it, and a reader answering from an answer it took
;; once is frozen at the old width and gives itself away.
(define reported-cols 100)

(define (widening-winsize-ioctl fd p)
  (set! queries (+ queries 1))
  (values 0 42 reported-cols))

;; RESET THE COUNTER AT THE START OF EVERY CASE.  The counter is shared, so a case
;; that fails to reset it sees the previous case's queries added to its own and
;; reads a count higher than the one it asserts -- failing for a reason that has
;; nothing to do with its subject.  The temptation is then to relax the assertion
;; to "at least one", which would gut it: EXACTLY ONE query per resize is the
;; whole point of this suite.  Reset the counter; never weaken the count.
(define (reset-queries!) (set! queries 0))

;; A delivered signal's handler runs at the next safe point, not at the instruction
;; after the kill, so wait for it on the WALL CLOCK rather than spinning a fixed
;; number of attempts -- a spin bounds the tries, not the time they are given, and
;; can exhaust them before the handler has been scheduled at all.
(define (resize-published-the-width?)
  (poll-until 5000
              (lambda () (sleep (make-time 'time-duration 10000000 0)))
              (lambda () (= (terminal-width) 137))))

;; ---------------------------------------------------------------------------
;; The public width query answers the CURRENT terminal, on every call
;; ---------------------------------------------------------------------------
;;
;; THIS CASE MUST RUN FIRST -- its first call is a COLD one, taken before anything
;; in this process has filled any cache, and that is half its subject.
;;
;; query-terminal-width is umbrella-public API, and a script is entitled to call it
;; far from any REPL.  `hafod -s`, `hafod -c` and a shebang script all load their
;; source and exit without ever entering interactive-repl -- so in those processes
;; NOTHING installs the SIGWINCH handler and nothing takes an editor entry, and those
;; are the only things that ever refresh the width cache.  A cache-backed width query
;; therefore has no refresher AT ALL on that path.  It would answer the first width
;; it ever saw, for the life of the process, and ask the kernel nothing after that: a
;; script sizing its output to the window -- a rule, a table, a progress bar -- would
;; be frozen at whatever width the terminal happened to have when it started, and
;; would never notice the user resizing it.
;;
;; Only a CHANGING answer can prove the query is live.  An assertion on a single call
;; passes just as happily against a cache that has been seeded once, which is exactly
;; how a stale reader hides.  So the stub widens the window from 100 to 200 columns
;; between calls -- a resize, in other words -- and the query has to follow it, asking
;; the kernel every time rather than reporting the width it first saw.
;;
;; Deliberately NOT routed through install-resize-handler!.  That is the REPL's
;; refresher, and installing it here would hand the case the very thing the script
;; path does not have -- so the case would go green on a tree where the script path
;; is broken.  The whole point is to read the way a script reads: cold, and alone.
(reset-queries!)
(set! reported-cols 100)
(parameterize ([winsize-ioctl widening-winsize-ioctl])
  (let* ([cold (query-terminal-width)]
         [asked-cold queries])
    ;; The window is widened under the caller's feet, as a real resize does.
    (set! reported-cols 200)
    (let* ([widened (query-terminal-width)]
           [asked-widened queries]
           [again (query-terminal-width)]
           [asked-again queries])
      (test-equal "a cold width query answers with the live width, not the default"
                  100 cold)
      (test-equal "a cold width query asks the kernel exactly once"
                  1 asked-cold)
      (test-equal "the width query answers the resized terminal, not the width it first saw"
                  200 widened)
      (test-equal "the width query asks the kernel again rather than reusing its first answer"
                  2 asked-widened)
      (test-equal "every later width query still answers the current width"
                  200 again)
      (test-equal "the width query asks the kernel on every call"
                  3 asked-again))))

;; ---------------------------------------------------------------------------
;; A resize asks the kernel exactly once
;; ---------------------------------------------------------------------------
;;
;; The load-bearing assertion, and the reason the ioctl is routed through a hook at
;; all.  install-resize-handler! is the product's OWN installer -- the very one the
;; REPL calls -- so this drives the real handler rather than a copy of it that could
;; drift away from it.  The prior disposition it hands back is restored on the way
;; out, so the suite leaves no live resize handler behind.
;;
;; The handler republishes the width LAST, so a width of 137 is its completion
;; signal.  A handler that never fires times the poll out and fails loudly rather
;; than quietly counting zero.
(test-assert "a terminal resize asks the kernel for the window size exactly once"
  (let ([prior #f])
    ;; Plant a width and a cached size the kernel cannot produce, so nothing this
    ;; case observes could have been left behind by an earlier one.
    (terminal-width 71)
    (set-terminal-size-cache! 999 998)
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([winsize-ioctl counting-winsize-ioctl])
          (reset-queries!)
          (set! prior (install-resize-handler!))
          (posix-kill (posix-getpid) SIGWINCH)
          (and (resize-published-the-width?)  ; the handler ran to completion
               (= (terminal-width) 137)       ; and republished the live width
               (= queries 1))))               ; THE PROOF: one query, not two
      (lambda ()
        ;; Restore exactly what was there before; when there was truly nothing, fall
        ;; back to a benign no-op, as the scoped signal helper itself does.
        (set-signal-handler! SIGWINCH (or prior (lambda (sig) (void))))))))

;; ---------------------------------------------------------------------------
;; A resize republishes the width it has just fetched
;; ---------------------------------------------------------------------------
;;
;; The ordering guard.  The resize step refreshes the cache and THEN reads it; a
;; step that read the width FIRST would republish whatever the cache held BEFORE
;; the query -- the previous size -- and would then be wrong on every render until
;; the next resize, silently.  Plant a width and a cache the kernel cannot produce
;; and the inversion is caught: it would republish 998.
;;
;; Planting the width first matters: the case above has already left it at 137, so
;; without a fresh distinct value this one could pass while doing nothing at all.
;;
;; The count is deliberately NOT asserted here -- ordering is this case's subject,
;; counting is the case above's -- so that a two-query tree reddens exactly one case
;; and the failure points straight at it.
(reset-queries!)
(terminal-width 71)
(set-terminal-size-cache! 999 998)
(parameterize ([winsize-ioctl counting-winsize-ioctl])
  (refresh-terminal-width!))

(test-equal "a resize republishes the width it has just fetched"
            137 (terminal-width))
(test-assert "a resize does not republish the width the cache held beforehand"
  (not (= (terminal-width) 998)))
(let-values ([(rows cols) (cached-terminal-size)])
  (test-equal "a resize stores the fetched rows in the cache" 42 rows)
  (test-equal "a resize stores the fetched columns in the cache" 137 cols))

;; ---------------------------------------------------------------------------
;; The finder asks the kernel for its size once at entry
;; ---------------------------------------------------------------------------
;;
;; The finder's entry asked TWICE: once to refresh the width cache, and then again,
;; live, to size its own state -- two ioctls, two foreign-alloc/free round trips, for
;; one window.  Its in-loop resize path already takes both from a single answer, and
;; says so; its entry did not.  Beyond the wasted round trip the two answers could
;; genuinely DISAGREE, because real work happens between them: the capability probe
;; runs an isatty and reads the environment, and a resize landing in that gap leaves
;; the cache holding one size while the finder's own rows and cols are built from
;; another.  The finder then renders against a width the cache does not share, and
;; the editor that resumes afterwards inherits the disagreement.
;;
;; Driving the real run-finder without a pseudo-terminal, as test-finder.ss does:
;;
;;   * assume-terminal-caps 'on forces the capability verdict and steers into the
;;     full-screen branch.  Left to the ambient fd 1 this would be a hostage to how
;;     the suite was invoked, and the plain branch sizes nothing, so it would prove
;;     nothing.
;;
;;   * The suite's stdin is /dev/null, so (with-raw-mode 0) inside the finder raises
;;     on tcgetattr.  That raise is this case's stopping point, and a load-bearing
;;     one: it lands AFTER the entry has sized itself and BEFORE the finder can block
;;     on a keystroke, so the count left behind is exactly the entry's count.
;;
;;   * Refuse loudly rather than enter raw mode on somebody's terminal.
(test-assert "the finder asks the kernel for the window size once at entry"
  (and
    (not (tty? 0))
    (let ([entered #f])
      (reset-queries!)
      (parameterize ([winsize-ioctl counting-winsize-ioctl]
                     [console-input-port (open-input-string "")]
                     [console-output-port (open-output-string)]
                     [assume-terminal-caps 'on])
        (guard (e [#t (set! entered #t)])   ; the tcgetattr raise
          (run-finder '("alpha" "beta") "> ")))
      (and entered          ; control: the full-screen entry really was reached
           (= queries 1)))))  ; THE PROOF: one query at entry, not two

;; The control for the case above, and the other half of the same claim: move ONE
;; variable -- the capability verdict -- and the branch moves with it.  Forced OFF,
;; the same call takes the plain numbered-selection branch, which sizes nothing to
;; the terminal at all: it prints a list, reads a line, and never consults a width.
;; A window-size query on that path is one whose answer nobody reads, so the entry
;; must not issue one -- a piped run or a CI log should not pay an ioctl to be told
;; a width it will never look at.  Being the plain branch it also neither raises nor
;; blocks: it reads the empty console input port, hits EOF and hands back #f.
(test-assert "the finder asks the kernel nothing on the branch that reads no size"
  (and
    (not (tty? 0))
    (let ([raised #f]
          [result 'unset])
      (reset-queries!)
      (parameterize ([winsize-ioctl counting-winsize-ioctl]
                     [console-input-port (open-input-string "")]
                     [console-output-port (open-output-string)]
                     [assume-terminal-caps 'off])
        (guard (e [#t (set! raised #t)])
          (set! result (run-finder '("alpha" "beta") "> "))))
      (and (not raised)       ; control: the plain branch neither raises
           (eq? result #f)    ; nor blocks -- it hit EOF and returned
           (= queries 0)))))  ; THE PROOF: no query on a path that reads none

(test-end)
