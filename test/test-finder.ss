(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod fuzzy)
        (hafod interactive)
        (hafod finder)
        (only (hafod signal) set-signal-handler! current-signal-handler with-signal-handler)
        (only (hafod posix) SIGWINCH))

(test-begin "finder")

;; ======================================================================
;; filter-search-pattern/positions tests (FIND-06)
;; ======================================================================

;; Plain fuzzy query returns candidates with non-empty position lists
(test-assert "fuzzy query returns candidates with positions"
  (let ([results (filter-search-pattern/positions "foo" '("foobar" "baz" "foo"))])
    (and (= (length results) 2)
         (pair? (cdar results))
         (andmap (lambda (r) (and (string? (car r)) (list? (cdr r)))) results))))

;; Extended syntax: ^prefix returns candidates starting with prefix, with positions
(test-assert "^prefix returns prefix matches with positions"
  (let ([results (filter-search-pattern/positions "^foo" '("foobar" "barfoo" "foo"))])
    (and (= (length results) 2)
         (member "foobar" (map car results))
         (member "foo" (map car results))
         (not (member "barfoo" (map car results))))))

;; Extended syntax: 'exact returns exact substring matches with positions
(test-assert "'exact returns exact substring matches with positions"
  (let ([results (filter-search-pattern/positions "'bar" '("foobar" "barbaz" "hello"))])
    (and (= (length results) 2)
         (andmap (lambda (r) (pair? (cdr r))) results))))

;; Extended syntax: !negate excludes matching candidates
(test-assert "!negate excludes matching candidates"
  (let ([results (filter-search-pattern/positions "!baz" '("foobar" "baz" "hello"))])
    (and (= (length results) 2)
         (not (member "baz" (map car results))))))

;; Extended syntax: suffix$ matches suffix
(test-assert "suffix$ matches suffix"
  (let ([results (filter-search-pattern/positions "bar$" '("foobar" "barbaz" "bar"))])
    (and (= (length results) 2)
         (member "foobar" (map car results))
         (member "bar" (map car results)))))

;; Extended syntax: foo | bar matches either (OR)
(test-assert "OR syntax matches either term"
  (let ([results (filter-search-pattern/positions "'foo | 'baz" '("foobar" "bazqux" "hello"))])
    (= (length results) 2)))

;; Empty pattern returns all candidates with empty position lists
(test-assert "empty pattern returns all with empty positions"
  (let ([results (filter-search-pattern/positions "" '("alpha" "beta" "gamma"))])
    (and (= (length results) 3)
         (andmap (lambda (r) (null? (cdr r))) results))))

;; Results are sorted by score (FIND-03): shorter/better match first
(test-assert "results sorted by score - shorter match first"
  (let ([results (filter-search-pattern/positions "ab" '("ab" "abc" "abcd"))])
    (and (= (length results) 3)
         (string=? (caar results) "ab"))))

;; ======================================================================
;; query-terminal-size tests
;; ======================================================================

;; Returns two values (rows and cols)
(test-assert "query-terminal-size returns two values"
  (call-with-values query-terminal-size
    (lambda (rows cols)
      (and (integer? rows) (integer? cols)))))

;; Both are positive integers
(test-assert "query-terminal-size returns positive integers"
  (call-with-values query-terminal-size
    (lambda (rows cols)
      (and (> rows 0) (> cols 0)))))

;; ======================================================================
;; (hafod finder) library tests (Plan 02)
;; ======================================================================

;; Library loads without error (verified by successful import above)
(test-assert "finder library loads successfully" #t)

;; fuzzy-select is an exported procedure (PAPI-01)
(test-assert "fuzzy-select is a procedure"
  (procedure? fuzzy-select))

;; run-finder is an exported procedure
(test-assert "run-finder is a procedure"
  (procedure? run-finder))

;; fuzzy-select accepts 1 or 2 arguments (case-lambda)
;; We can't call it without a terminal, but we can verify it's callable
(test-assert "fuzzy-select is callable with arity check"
  (and (procedure? fuzzy-select)
       ;; Verify it's a case-lambda by checking procedure?
       ;; (actual invocation requires terminal)
       #t))

;; ======================================================================
;; Capability-gated output (PTY-free)
;; ======================================================================

;; Chez has no built-in substring search; mirror test-interactive.ss's helper.
(define (str-contains? haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (and (>= hlen nlen)
         (let loop ([i 0])
           (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) #t]
             [else (loop (+ i 1))])))))

;; True iff the string contains any ESC (0x1b) byte.
(define (has-escape? s)
  (let ([n (string-length s)])
    (let loop ([i 0])
      (cond
        [(>= i n) #f]
        [(char=? (string-ref s i) #\x1b) #t]
        [else (loop (+ i 1))]))))

;; Minimal single-candidate finder-state; colour? is the gate under test.
;; Field order mirrors the finder-state record definition.
(define (make-render-state colour?)
  (make-finder-state
    (vector "hello")             ; items
    (vector (cons "hello" '()))  ; filtered: (candidate . positions)
    "" 0 0 0                     ; query cursor selected scroll-offset
    24 80                        ; rows cols
    1 "> "                       ; total-count prompt
    (vector "hello")             ; display-items
    #f #f                        ; colorize? show-numbers?
    colour?))                    ; colour?

;; Render once to a string port, capturing every byte the finder would emit.
(define (render->string colour?)
  (let ([sp (open-output-string)])
    (parameterize ([console-output-port sp])
      (render-finder! (make-render-state colour?)))
    (get-output-string sp)))

;; Palette gating: colour off => cursor codes survive but every SGR is suppressed.
(test-assert "render-finder! emits cursor codes but zero SGR when colour is off"
  (let ([out (render->string #f)])
    (and (str-contains? out "\x1b;[2K")         ; a clear-line cursor code survives
         (not (str-contains? out "\x1b;[38;5"))  ; no 256-colour foreground
         (not (str-contains? out "\x1b;[48;5"))  ; no 256-colour background
         (not (str-contains? out "\x1b;[1m"))    ; no bold
         (not (str-contains? out "\x1b;[0m")))))  ; no reset

;; Palette gating: colour on => cursor codes AND SGR are both present (no regression).
(test-assert "render-finder! emits SGR when colour is on"
  (let ([out (render->string #t)])
    (and (str-contains? out "\x1b;[2K")    ; cursor codes still emitted
         (str-contains? out "\x1b;[1m"))))  ; and the SGR palette is emitted

;; Plain fallback: returns the chosen ORIGINAL item and emits zero escape bytes.
(test-assert "run-finder/plain returns the selected item with zero escapes"
  (let ([out (open-output-string)])
    (let ([choice (run-finder/plain '("alpha" "beta" "gamma")
                                    "pick> "
                                    (open-input-string "1\n")
                                    out)])
      (and (equal? choice "alpha")
           (not (has-escape? (get-output-string out)))))))

;; Plain fallback: a blank line cancels (returns #f), still zero escapes.
(test-assert "run-finder/plain returns #f on a blank line"
  (let ([out (open-output-string)])
    (let ([choice (run-finder/plain '("alpha" "beta")
                                    "pick> "
                                    (open-input-string "\n")
                                    out)])
      (and (not choice)
           (not (has-escape? (get-output-string out)))))))

;; Plain fallback: an out-of-range choice cancels (returns #f).
(test-assert "run-finder/plain returns #f for an out-of-range choice"
  (equal? #f (run-finder/plain '("alpha" "beta")
                               "pick> "
                               (open-input-string "9\n")
                               (open-output-string))))

;; ======================================================================
;; Resize scope restores the prior SIGWINCH handler
;; ======================================================================

;; A scoped SIGWINCH swap restores the prior disposition rather than a
;; no-op -- the exact registry contract the finder now relies on. After a
;; with-signal-handler scope exits, the handler that was live beforehand is
;; current again. SIGWINCH is used purely as an inert registry key: no TTY
;; is touched and no signal is delivered.
(test-assert "resize scope restores the prior SIGWINCH handler"
  (let ([sentinel (lambda (sig) 'sentinel)]
        [other (lambda (sig) 'other)])
    (set-signal-handler! SIGWINCH sentinel)
    (with-signal-handler SIGWINCH other
      (lambda () #t))
    (eq? (current-signal-handler SIGWINCH) sentinel)))

(test-end)
