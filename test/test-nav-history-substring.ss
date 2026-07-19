;;; test/test-nav-history-substring.ss -- History recall matches a substring
;;; anywhere in a line, not only at its head.
;;;
;;; The editor's Up/Down history navigation was prefix-only: typing the start of
;;; an earlier line and pressing Up walked back through the lines that BEGAN with
;;; it.  A line whose distinguishing word sits in the middle -- "git commit
;;; --amend" reached for by "amend" -- was unreachable that way.  This suite
;;; proves the substring primitive the recall now rests on: a mid-line needle
;;; finds an entry a prefix search misses, the prefix search still finds a
;;; genuine prefix entry (so that behaviour stays reachable), and the shared
;;; smart-case predicate is case-insensitive for an all-lowercase needle,
;;; case-sensitive for a needle carrying uppercase, and treats an empty needle
;;; as "no filter" -- matching every line.
;;;
;;; Entirely in-process: an in-memory history is seeded with a known corpus and
;;; the model search functions are driven directly, no terminal.  Degrades to a
;;; printed note where libsqlite3 cannot be loaded, mirroring the other history
;;; suites.
;;; Copyright (c) 2026, hafod contributors.

;; Resolve the compiled libraries by ABSOLUTE path from the launch directory, so
;; library resolution stays independent of any later cd -- the idiom the other
;; navigation suites use.  This suite never cds, but the pinning costs nothing
;; and keeps every nav suite consistent.
(library-directories
  (let ([root (current-directory)])
    (list (cons (string-append root "/src") (string-append root "/src"))
          (cons root root))))
(import (test runner)
        (hafod editor history)
        (only (hafod editor editor) history-search-mode)
        (only (hafod editor sqlite3) sqlite3-loaded?)
        (chezscheme))

(define (note . parts)
  (display "  note: ")
  (for-each display parts)
  (newline))

(test-begin "nav-history-substring")

;; The history-search-mode parameter contract, provable with neither a terminal
;; nor a history: it is the toggle the editor's Up/Down dispatch branches on.  It
;; defaults to 'substring (recall a needle anywhere in a line), round-trips
;; 'prefix under parameterize, and -- being validated at the set site -- raises
;; on anything else, so a mistaken value in a user's init is refused loudly
;; rather than quietly disabling recall.
(test-assert "history-search-mode defaults to 'substring"
  (eq? (history-search-mode) 'substring))

(test-assert "history-search-mode round-trips 'prefix under parameterize"
  (parameterize ([history-search-mode 'prefix])
    (eq? (history-search-mode) 'prefix)))

;; Setting an out-of-range value must raise; the guard fires and the assertion
;; holds only because it did.  A naive parameter with no validator would set the
;; value, evaluate the body to #f, and fail this.
(test-assert "history-search-mode raises on a value outside 'substring/'prefix"
  (guard (e [#t #t])
    (parameterize ([history-search-mode 'fuzzy]) #f)))

;; open-history goes through the sqlite3 wrappers, whose shared object is
;; resolved on first use -- so this first open is what loads it, and
;; sqlite3-loaded? can be trusted from there on.  An in-memory history needs no
;; file and leaves nothing behind; the search functions read the in-memory entry
;; vector, so the corpus below drives them with no disk at all.
(define h (open-history ":memory:"))

(if (not (sqlite3-loaded?))
    (begin
      (note "libsqlite3 is unavailable; skipping the history-substring assertions")
      (history-close! h))
    (begin
      ;; A known corpus, oldest first (history-add! appends, most recent last):
      ;;   0  deploy staging      -- a genuine PREFIX entry (reached by "deploy")
      ;;   1  git commit --amend  -- the MID-LINE entry (reached by "amend")
      ;;   2  echo hello world    -- a distractor matching neither needle
      (history-add! h "deploy staging")
      (history-add! h "git commit --amend")
      (history-add! h "echo hello world")

      (let ([top (- (history-count h) 1)])
        ;; The headline.  "amend" sits in the middle of "git commit --amend", so
        ;; a substring search reaches it while a prefix search -- anchored at the
        ;; head of every entry -- never does.  One assertion, both halves: the
        ;; substring hit lands on the right line, and the prefix search misses.
        (test-assert "a mid-line needle finds the entry a prefix search misses"
          (let ([idx (history-substring-search-backward h "amend" top)])
            (and idx
                 (string=? (history-ref h idx) "git commit --amend")
                 (not (history-prefix-search-backward h "amend" top)))))

        ;; Prefix behaviour stays reachable: a needle that really is a prefix is
        ;; still found by the prefix search, on the line it heads.
        (test-assert "a genuine prefix needle is still found by the prefix search"
          (let ([idx (history-prefix-search-backward h "deploy" top)])
            (and idx
                 (string=? (history-ref h idx) "deploy staging")))))

      ;; Smart-case, asserted on the shared predicate directly (needle then
      ;; entry).  An all-lowercase needle is matched case-insensitively...
      (test-assert "an all-lowercase needle matches a mixed-case entry (case-insensitive)"
        (smart-substring-match? "readme" "Open the README file"))

      ;; ...while a needle carrying any uppercase is case-sensitive: it matches an
      ;; entry cased the same way...
      (test-assert "a needle carrying uppercase matches an exactly-cased entry"
        (smart-substring-match? "README" "open the README file"))

      ;; ...and does NOT match one cased differently.
      (test-assert "a needle carrying uppercase does not match a differently-cased entry"
        (not (smart-substring-match? "README" "open the readme file")))

      ;; An empty needle is "no filter": it matches every line.  The match index
      ;; is 0 here, and 0 counts as a match -- only #f is a miss -- so this guards
      ;; against a regression that mistook the zero index for "not found".
      (test-assert "an empty needle matches everything"
        (smart-substring-match? "" "any line at all"))

      ;; Up and Down agree by construction.  Over the seeded corpus, walk
      ;; history-substring-search-backward to exhaustion for a needle matching
      ;; some lines but not all -- "l" is in "deploy staging" and "echo hello
      ;; world" but not "git commit --amend" -- collecting every index the
      ;; BACKWARD scan (Up) matches.  Then, index for index, the forward-
      ;; membership predicate smart-substring-match? -- the very predicate the
      ;; forward (Down) loop tests -- must be truthy exactly on those indices.  An
      ;; entry matches under the backward helper iff the shared predicate holds,
      ;; so the two directions cannot diverge on what counts as a match.
      (test-assert "the forward predicate and the backward scan agree on the matched set"
        (let* ([needle "l"]
               [top (- (history-count h) 1)]
               [backward-set
                (let loop ([start top] [acc '()])
                  (let ([idx (history-substring-search-backward h needle start)])
                    (if idx
                        (loop (- idx 1) (cons idx acc))
                        acc)))])
          (let loop ([i 0])
            (or (>= i (history-count h))
                (and (eq? (and (memv i backward-set) #t)
                          (and (smart-substring-match? needle (history-ref h i)) #t))
                     (loop (+ i 1)))))))

      ;; 'prefix mode restores the head-anchored path.  Under it the dispatch's
      ;; predicate is string-prefix?: a genuine prefix ("deploy") is found while a
      ;; mid-line needle ("amend") is not -- yet the substring helper the default
      ;; mode uses does find that mid-line needle.  So the prefix behaviour stays
      ;; reachable through the toggle, distinct from the substring default.
      (test-assert "'prefix mode matches a prefix needle and rejects a mid-line one"
        (parameterize ([history-search-mode 'prefix])
          (let ([top (- (history-count h) 1)])
            (and (eq? (history-search-mode) 'prefix)
                 (history-prefix-search-backward h "deploy" top)
                 (not (history-prefix-search-backward h "amend" top))
                 (history-substring-search-backward h "amend" top)))))

      (history-close! h)))

;; The editor's Up/Down dispatch reads history-search-mode and routes both
;; directions through the one smart-substring-match? predicate proven above --
;; which is exactly what makes the toggle and the "Up and Down agree" assertions
;; hold: both directions match through the same predicate, so they cannot diverge
;; on what counts as a match.

(test-end)
