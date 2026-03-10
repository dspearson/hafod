;;; (hafod shell parser) -- Shell command parser producing EPF datums
;;; Translates shell syntax (e.g., "ls -la | grep foo > out.txt")
;;; into EPF quoted datums (e.g., '(run (pipe (ls "-la") (grep "foo")) (> "out.txt")))
;;; for evaluation via the existing run macro.

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
  ;;   (pipe . #f)
  ;;   (redirect-out . #f)
  ;;   (redirect-append . #f)
  ;;   (redirect-in . #f)

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
  ;; Tokeniser
  ;; ======================================================================

  ;; Tokenise a shell command string into a list of tokens.
  ;; Handles quoting, escaping, variable expansion, and glob detection.
  (define (tokenise str)
    (let* ([len (string-length str)]
           [tokens '()]        ; accumulated tokens (reversed)
           [word-chars '()]    ; current word chars (reversed)
           [has-glob? #f]      ; whether current word has unquoted glob meta
           [word-started? #f]) ; whether we've started a word (for empty expansions)

      ;; Flush current word as a token (possibly glob-expanding)
      (define (flush-word!)
        (unless (and (null? word-chars) (not word-started?))
          (let* ([word (list->string (reverse word-chars))]
                 [toks (if has-glob?
                           (let ([expanded (glob word)])
                             (if (null? expanded)
                                 (list (make-token 'word word))
                                 (map (lambda (f) (make-token 'word f)) expanded)))
                           (list (make-token 'word word)))])
            (set! tokens (append (reverse toks) tokens))
            (set! word-chars '())
            (set! has-glob? #f)
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

                ;; Pipe
                [(char=? c #\|)
                 (flush-word!)
                 (set! tokens (cons (make-token 'pipe #f) tokens))
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

                ;; Glob metacharacters
                [(glob-meta? c)
                 (set! has-glob? #t)
                 (add-char! c)
                 (loop (+ i 1))]

                ;; Normal character
                [else
                 (add-char! c)
                 (loop (+ i 1))]))))))

  ;; ======================================================================
  ;; Form builder: tokens -> EPF datum
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

  ;; Build the final EPF datum from a token list.
  (define (build-form tokens)
    ;; Split on pipe tokens into pipeline stages
    (let* ([stages (split-on (lambda (t) (eq? (token-type t) 'pipe)) tokens)]
           ;; Remove empty stages (from leading/trailing pipes)
           [stages (filter (lambda (s) (not (null? s))) stages)]
           ;; Parse each stage
           [parsed (map parse-stage stages)])
      (cond
        ;; Single command
        [(= (length parsed) 1)
         (let* ([stage (car parsed)]
                [cmd (car stage)]
                [redirects (cdr stage)])
           (cons 'run (cons cmd redirects)))]
        ;; Pipeline
        [else
         ;; Collect redirections from the last stage only;
         ;; earlier stages have their redirections dropped for v1.2
         ;; (per-stage redirects would need epf nesting)
         (let* ([last-stage (list-ref parsed (- (length parsed) 1))]
                [last-redirects (cdr last-stage)]
                [pipe-forms (map car parsed)]
                [pipe-form (cons 'pipe pipe-forms)])
           (cons 'run (cons pipe-form last-redirects)))])))

  ;; ======================================================================
  ;; Main entry point
  ;; ======================================================================

  (define (parse-shell-command str)
    (let ([tokens (tokenise str)])
      (if (null? tokens)
          '(run)  ; empty command
          (build-form tokens))))

) ; end library
