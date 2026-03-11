;;; (hafod shell parser) -- Shell command parser producing EPF datums
;;; Translates shell syntax (e.g., "ls -la | grep foo > out.txt")
;;; into EPF quoted datums for evaluation via the existing run macro.
;;; Supports: pipes, redirections, &&, ||, ;, &, quoting, variables,
;;; glob expansion, and brace expansion.

(library (hafod shell parser)
  (export parse-shell-command)

  (import (except (chezscheme) getenv)
          (only (hafod glob) glob)
          (only (hafod environment) getenv))

  ;; ======================================================================
  ;; Token types
  ;; ======================================================================
  ;; Tokens are (type . value) pairs:
  ;;   (word . "string")
  ;;   (pipe . #f)            — |
  ;;   (and-if . #f)          — &&
  ;;   (or-if . #f)           — ||
  ;;   (semi . #f)            — ;
  ;;   (background . #f)      — &
  ;;   (redirect-out . #f)    — >
  ;;   (redirect-append . #f) — >>
  ;;   (redirect-in . #f)     — <

  (define (make-token type value) (cons type value))
  (define (token-type tok) (car tok))
  (define (token-value tok) (cdr tok))

  ;; ======================================================================
  ;; Character predicates
  ;; ======================================================================

  (define (var-start-char? c)
    (or (char-alphabetic? c) (char=? c #\_)))

  (define (var-char? c)
    (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))

  (define (glob-meta? c)
    (or (char=? c #\*) (char=? c #\?) (char=? c #\[)))

  ;; ======================================================================
  ;; Environment variable expansion
  ;; ======================================================================

  ;; Read a variable name starting at pos (after the $).
  ;; Returns (name . new-pos).
  (define (read-var-name str pos len)
    (cond
      ;; ${VAR} form
      [(and (< pos len) (char=? (string-ref str pos) #\{))
       (let loop ([i (+ pos 1)] [chars '()])
         (cond
           [(>= i len)
            ;; Unterminated ${...} -- treat as literal
            (cons (string-append "{" (list->string (reverse chars))) i)]
           [(char=? (string-ref str i) #\})
            (cons (list->string (reverse chars)) (+ i 1))]
           [else
            (loop (+ i 1) (cons (string-ref str i) chars))]))]
      ;; $VAR form -- read identifier chars
      [(and (< pos len) (var-start-char? (string-ref str pos)))
       (let loop ([i pos] [chars '()])
         (if (and (< i len) (var-char? (string-ref str i)))
             (loop (+ i 1) (cons (string-ref str i) chars))
             (cons (list->string (reverse chars)) i)))]
      ;; Bare $ at end or before non-var char -- literal $
      [else (cons "$" pos)]))

  (define (expand-var name)
    (or (getenv name) ""))

  ;; ======================================================================
  ;; Brace expansion
  ;; ======================================================================

  ;; Find the matching } for a { at position start, respecting nesting.
  ;; Returns index of } or #f if unmatched.
  (define (find-matching-close str start len)
    (let loop ([i start] [depth 1])
      (cond
        [(>= i len) #f]
        [(and (char=? (string-ref str i) #\\) (< (+ i 1) len))
         (loop (+ i 2) depth)]
        [(char=? (string-ref str i) #\{)
         (loop (+ i 1) (+ depth 1))]
        [(char=? (string-ref str i) #\})
         (if (= depth 1) i (loop (+ i 1) (- depth 1)))]
        [else (loop (+ i 1) depth)])))

  ;; Split brace content on top-level commas, respecting nested braces.
  ;; Returns list of strings.
  (define (parse-brace-content str)
    (let ([len (string-length str)])
      (let loop ([i 0] [depth 0] [start 0] [items '()])
        (cond
          [(>= i len)
           (reverse (cons (substring str start len) items))]
          [(and (char=? (string-ref str i) #\\) (< (+ i 1) len))
           (loop (+ i 2) depth start items)]
          [(char=? (string-ref str i) #\{)
           (loop (+ i 1) (+ depth 1) start items)]
          [(char=? (string-ref str i) #\})
           (loop (+ i 1) (- depth 1) start items)]
          [(and (char=? (string-ref str i) #\,) (= depth 0))
           (loop (+ i 1) depth (+ i 1)
                 (cons (substring str start i) items))]
          [else (loop (+ i 1) depth start items)]))))

  ;; Try to parse "start..end" as a numeric range.
  ;; Returns list of number strings, or #f.
  (define (parse-range str)
    (let ([len (string-length str)])
      ;; Find ".." (not preceded by another dot)
      (let scan ([i 0])
        (cond
          [(>= (+ i 1) len) #f]
          [(and (char=? (string-ref str i) #\.)
                (char=? (string-ref str (+ i 1)) #\.))
           (let ([s1 (substring str 0 i)]
                 [s2 (substring str (+ i 2) len)])
             (let ([n1 (string->number s1)]
                   [n2 (string->number s2)])
               (and n1 n2 (exact? n1) (exact? n2)
                    (let ([step (if (<= n1 n2) 1 -1)])
                      (let loop ([n n1] [acc '()])
                        (if (if (> step 0) (> n n2) (< n n2))
                            (reverse acc)
                            (loop (+ n step)
                                  (cons (number->string n) acc))))))))]
          [else (scan (+ i 1))]))))

  ;; Expand brace expressions in a word.
  ;; Returns a list of expanded strings.
  (define (brace-expand str)
    (let ([len (string-length str)])
      ;; Find first unescaped { outside quotes
      (let find-open ([i 0])
        (cond
          [(>= i len) (list str)]
          [(and (char=? (string-ref str i) #\\) (< (+ i 1) len))
           (find-open (+ i 2))]
          [(char=? (string-ref str i) #\{)
           (let ([close (find-matching-close str (+ i 1) len)])
             (if (not close)
                 (list str)  ; unmatched brace — no expansion
                 (let* ([prefix (substring str 0 i)]
                        [content (substring str (+ i 1) close)]
                        [suffix (substring str (+ close 1) len)])
                   ;; Try range first
                   (let ([range (parse-range content)])
                     (if range
                         ;; Numeric range: expand and recurse for cross-product
                         (apply append
                           (map (lambda (item)
                                  (brace-expand (string-append prefix item suffix)))
                                range))
                         ;; Comma-separated items
                         (let ([items (parse-brace-content content)])
                           (if (>= (length items) 2)
                               ;; 2+ items: expand and recurse
                               (apply append
                                 (map (lambda (item)
                                        (brace-expand (string-append prefix item suffix)))
                                      items))
                               ;; Single item: no expansion (bash convention)
                               (list str))))))))]
          [else (find-open (+ i 1))]))))

  ;; ======================================================================
  ;; Tokeniser
  ;; ======================================================================

  ;; Tokenise a shell command string into a list of tokens.
  ;; Handles quoting, escaping, variable expansion, glob and brace detection.
  (define (tokenise str)
    (let* ([len (string-length str)]
           [tokens '()]        ; accumulated tokens (reversed)
           [word-chars '()]    ; current word chars (reversed)
           [has-glob? #f]      ; whether current word has unquoted glob meta
           [has-brace? #f]     ; whether current word has unquoted brace
           [word-started? #f]) ; whether we've started a word (for empty expansions)

      ;; Flush current word as a token (with brace/glob expansion)
      (define (flush-word!)
        (unless (and (null? word-chars) (not word-started?))
          (let* ([word (list->string (reverse word-chars))]
                 ;; Brace expansion first (if any unquoted brace seen)
                 [words (if has-brace?
                            (brace-expand word)
                            (list word))]
                 ;; Then glob expansion for each word
                 [toks (apply append
                         (map (lambda (w)
                                (if has-glob?
                                    (let ([expanded (glob w)])
                                      (if (null? expanded)
                                          (list (make-token 'word w))
                                          (map (lambda (f) (make-token 'word f)) expanded)))
                                    (list (make-token 'word w))))
                              words))])
            (set! tokens (append (reverse toks) tokens))
            (set! word-chars '())
            (set! has-glob? #f)
            (set! has-brace? #f)
            (set! word-started? #f))))

      (define (add-char! c)
        (set! word-chars (cons c word-chars)))

      (define (add-string! s)
        (string-for-each (lambda (c) (add-char! c)) s))

      ;; Main loop
      (let loop ([i 0])
        (if (>= i len)
            (begin
              (flush-word!)
              (reverse tokens))
            (let ([c (string-ref str i)])
              (cond
                ;; Whitespace in normal mode: flush word
                [(char-whitespace? c)
                 (flush-word!)
                 (loop (+ i 1))]

                ;; Pipe or ||
                [(char=? c #\|)
                 (flush-word!)
                 (if (and (< (+ i 1) len) (char=? (string-ref str (+ i 1)) #\|))
                     (begin
                       (set! tokens (cons (make-token 'or-if #f) tokens))
                       (loop (+ i 2)))
                     (begin
                       (set! tokens (cons (make-token 'pipe #f) tokens))
                       (loop (+ i 1))))]

                ;; && or background &
                [(char=? c #\&)
                 (flush-word!)
                 (if (and (< (+ i 1) len) (char=? (string-ref str (+ i 1)) #\&))
                     (begin
                       (set! tokens (cons (make-token 'and-if #f) tokens))
                       (loop (+ i 2)))
                     (begin
                       (set! tokens (cons (make-token 'background #f) tokens))
                       (loop (+ i 1))))]

                ;; Semicolon
                [(char=? c #\;)
                 (flush-word!)
                 (set! tokens (cons (make-token 'semi #f) tokens))
                 (loop (+ i 1))]

                ;; Redirect: > or >>
                [(char=? c #\>)
                 (flush-word!)
                 (if (and (< (+ i 1) len) (char=? (string-ref str (+ i 1)) #\>))
                     (begin
                       (set! tokens (cons (make-token 'redirect-append #f) tokens))
                       (loop (+ i 2)))
                     (begin
                       (set! tokens (cons (make-token 'redirect-out #f) tokens))
                       (loop (+ i 1))))]

                ;; Redirect: <
                [(char=? c #\<)
                 (flush-word!)
                 (set! tokens (cons (make-token 'redirect-in #f) tokens))
                 (loop (+ i 1))]

                ;; Backslash escape
                [(char=? c #\\)
                 (if (< (+ i 1) len)
                     (begin
                       (add-char! (string-ref str (+ i 1)))
                       (loop (+ i 2)))
                     ;; Trailing backslash -- ignore
                     (loop (+ i 1)))]

                ;; Double quote
                [(char=? c #\")
                 (set! word-started? #t)
                 (let dq-loop ([j (+ i 1)])
                   (if (>= j len)
                       ;; Unterminated double quote -- consume rest
                       (loop j)
                       (let ([dc (string-ref str j)])
                         (cond
                           [(char=? dc #\")
                            (loop (+ j 1))]
                           [(char=? dc #\\)
                            ;; In double quotes, backslash escapes " $ \ newline
                            (if (< (+ j 1) len)
                                (let ([nc (string-ref str (+ j 1))])
                                  (if (memv nc '(#\" #\$ #\\ #\newline))
                                      (begin (add-char! nc) (dq-loop (+ j 2)))
                                      (begin (add-char! #\\) (add-char! nc) (dq-loop (+ j 2)))))
                                (begin (add-char! #\\) (dq-loop (+ j 1))))]
                           [(char=? dc #\$)
                            ;; Variable expansion inside double quotes
                            (let* ([result (read-var-name str (+ j 1) len)]
                                   [name (car result)]
                                   [new-pos (cdr result)])
                              (if (string=? name "$")
                                  ;; Bare $ -- literal
                                  (begin (add-char! #\$) (dq-loop (+ j 1)))
                                  (begin (add-string! (expand-var name))
                                         (dq-loop new-pos))))]
                           [else
                            (add-char! dc)
                            (dq-loop (+ j 1))]))))]

                ;; Single quote
                [(char=? c #\')
                 (set! word-started? #t)
                 (let sq-loop ([j (+ i 1)])
                   (if (>= j len)
                       ;; Unterminated single quote -- consume rest
                       (loop j)
                       (let ([sc (string-ref str j)])
                         (if (char=? sc #\')
                             (loop (+ j 1))
                             (begin (add-char! sc) (sq-loop (+ j 1)))))))]

                ;; Dollar sign -- variable expansion
                [(char=? c #\$)
                 (let* ([result (read-var-name str (+ i 1) len)]
                        [name (car result)]
                        [new-pos (cdr result)])
                   (if (string=? name "$")
                       ;; Bare $ -- literal
                       (begin (add-char! #\$) (loop (+ i 1)))
                       (begin
                         (set! word-started? #t)
                         (add-string! (expand-var name))
                         (loop new-pos))))]

                ;; Brace metacharacter (outside quotes)
                [(char=? c #\{)
                 (set! has-brace? #t)
                 (add-char! c)
                 (loop (+ i 1))]

                ;; Glob metacharacters
                [(glob-meta? c)
                 (set! has-glob? #t)
                 (add-char! c)
                 (loop (+ i 1))]

                ;; Normal character (including } which is harmless without matching {)
                [else
                 (add-char! c)
                 (loop (+ i 1))]))))))

  ;; ======================================================================
  ;; Form builder: tokens -> Scheme datum
  ;; ======================================================================

  ;; Split a list on elements satisfying pred, returning list of sublists.
  (define (split-on pred lst)
    (let loop ([rest lst] [current '()] [result '()])
      (cond
        [(null? rest)
         (reverse (cons (reverse current) result))]
        [(pred (car rest))
         (loop (cdr rest) '() (cons (reverse current) result))]
        [else
         (loop (cdr rest) (cons (car rest) current) result)])))

  ;; Check if a token is a redirection type.
  (define (redirect-token? tok)
    (memq (token-type tok) '(redirect-out redirect-append redirect-in)))

  ;; Map redirect token type to EPF symbol.
  (define (redirect-sym type)
    (case type
      [(redirect-out) '>]
      [(redirect-append) '>>]
      [(redirect-in) '<]
      [else (error 'redirect-sym "unexpected redirect type" type)]))

  ;; Parse a single pipeline stage: extract command and redirections.
  ;; Returns (command-form . redirect-forms).
  (define (parse-stage tokens)
    (let loop ([rest tokens] [cmd-words '()] [redirects '()])
      (cond
        [(null? rest)
         (let* ([words (reverse cmd-words)]
                [cmd-form (if (null? words)
                              (error 'parse-shell-command "empty command")
                              (cons (string->symbol (token-value (car words)))
                                    (map token-value (cdr words))))]
                [redir-forms (reverse redirects)])
           (cons cmd-form redir-forms))]
        [(redirect-token? (car rest))
         ;; Next token must be a word (the filename)
         (if (and (pair? (cdr rest))
                  (eq? (token-type (cadr rest)) 'word))
             (let ([redir-form (list (redirect-sym (token-type (car rest)))
                                     (token-value (cadr rest)))])
               (loop (cddr rest) cmd-words (cons redir-form redirects)))
             (error 'parse-shell-command
                    "redirect without filename"))]
        [(eq? (token-type (car rest)) 'word)
         (loop (cdr rest) (cons (car rest) cmd-words) redirects)]
        [else
         (error 'parse-shell-command "unexpected token" (car rest))])))

  ;; Build (run ...) form for a pipeline (pipe-separated commands).
  (define (build-pipeline tokens)
    (let* ([stages (split-on (lambda (t) (eq? (token-type t) 'pipe)) tokens)]
           [stages (filter (lambda (s) (not (null? s))) stages)])
      (if (null? stages)
          '(run)
          (let ([parsed (map parse-stage stages)])
            (cond
              ;; Single command
              [(= (length parsed) 1)
               (let* ([stage (car parsed)]
                      [cmd (car stage)]
                      [redirects (cdr stage)])
                 (cons 'run (cons cmd redirects)))]
              ;; Pipeline
              [else
               (let* ([last-stage (list-ref parsed (- (length parsed) 1))]
                      [last-redirects (cdr last-stage)]
                      [pipe-forms (map car parsed)]
                      [pipe-form (cons 'pipe pipe-forms)])
                 (cons 'run (cons pipe-form last-redirects)))])))))

  ;; Split token list on && and || operators, preserving operator type.
  ;; Returns list of (tokens . operator) where operator is
  ;; 'and-if, 'or-if, or #f (last segment).
  (define (split-and-or tokens)
    (let loop ([rest tokens] [current '()] [result '()])
      (cond
        [(null? rest)
         (reverse (cons (cons (reverse current) #f) result))]
        [(memq (token-type (car rest)) '(and-if or-if))
         (loop (cdr rest) '()
               (cons (cons (reverse current) (token-type (car rest))) result))]
        [else
         (loop (cdr rest) (cons (car rest) current) result)])))

  ;; Build form for an and-or list (handles && and || chaining).
  ;; Emits let/if chains that propagate wait status properly.
  (define (build-and-or tokens)
    (let* ([segments (split-and-or tokens)]
           [segments (filter (lambda (s) (not (null? (car s)))) segments)])
      (if (null? segments)
          '(run)
          ;; Build left-to-right chain
          (let loop ([segs segments] [form (build-pipeline (caar segments))])
            (let ([op (cdar segs)]
                  [rest (cdr segs)])
              (if (or (not op) (null? rest))
                  form
                  (let ([next (build-pipeline (caar rest))])
                    (loop rest
                          (case op
                            [(and-if) `(let ([_s ,form]) (if (zero? _s) ,next _s))]
                            [(or-if) `(let ([_s ,form]) (if (zero? _s) _s ,next))]
                            [else form])))))))))

  ;; Split token list on ; and & separators, preserving separator type.
  ;; Returns list of (tokens . separator) where separator is
  ;; 'semi, 'background, or #f (last segment).
  (define (split-list tokens)
    (let loop ([rest tokens] [current '()] [result '()])
      (cond
        [(null? rest)
         (reverse (cons (cons (reverse current) #f) result))]
        [(memq (token-type (car rest)) '(semi background))
         (loop (cdr rest) '()
               (cons (cons (reverse current) (token-type (car rest))) result))]
        [else
         (loop (cdr rest) (cons (car rest) current) result)])))

  ;; Reconstruct the command string for a segment of tokens (for job display).
  (define (tokens->string tokens)
    (let loop ([rest tokens] [acc '()])
      (if (null? rest)
          (apply string-append (reverse acc))
          (let ([tok (car rest)])
            (loop (cdr rest)
                  (cons (case (token-type tok)
                          [(word) (if (null? acc)
                                      (token-value tok)
                                      (string-append " " (token-value tok)))]
                          [(pipe) " | "]
                          [(and-if) " && "]
                          [(or-if) " || "]
                          [else ""])
                        acc))))))

  ;; Build the final form from a complete token list.
  ;; Handles ; sequencing and & background at the top level.
  (define (build-form tokens)
    (let* ([segments (split-list tokens)]
           [segments (filter (lambda (s) (not (null? (car s)))) segments)])
      (if (null? segments)
          '(run)
          (let ([forms
                 (map (lambda (seg)
                        (let ([toks (car seg)]
                              [sep (cdr seg)])
                          (let ([form (build-and-or toks)])
                            (if (eq? sep 'background)
                                (let ([cmd-str (tokens->string toks)])
                                  `(job-bg! ,cmd-str
                                     (fork (lambda ()
                                             (set-process-group 0 0)
                                             ,form))))
                                form))))
                      segments)])
            (if (= (length forms) 1)
                (car forms)
                (cons 'begin forms))))))

  ;; ======================================================================
  ;; Main entry point
  ;; ======================================================================

  (define (parse-shell-command str)
    (let ([tokens (tokenise str)])
      (if (null? tokens)
          '(run)  ; empty command
          (build-form tokens))))

) ; end library
