;;; (hafod shell classifier) -- Input classification for shell mode
;;; Routes user input to Scheme eval, shell parser, or builtin execution.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod shell classifier)
  (export classify-input rebuild-path-cache! path-cache scheme-prefix-chars
          path-cache-keys command-not-found-suppress? command-not-found-suggestions
          alias-set! alias-remove! alias-ref alias-names alias-expand-line)
  (import (chezscheme)
          (only (hafod process) exec-path-list)
          (only (hafod fuzzy) fuzzy-filter))

  ;; Characters that unambiguously start Scheme expressions
  (define scheme-prefix-chars '(#\( #\' #\` #\# #\, #\[))

  ;; Common Scheme keywords -- if the first token is one of these,
  ;; treat as Scheme even if a same-named executable exists in PATH.
  (define scheme-keywords
    (let ([ht (make-hashtable string-hash string=?)])
      (for-each (lambda (k) (hashtable-set! ht k #t))
                '("define" "import" "set!" "begin" "library" "when" "unless"
                  "cond" "case" "let" "let*" "letrec" "letrec*" "lambda" "if"
                  "and" "or" "do" "syntax-case" "syntax-rules" "quote"
                  "quasiquote" "define-syntax" "define-record-type" "values"
                  "call/cc" "call-with-values" "guard" "parameterize"
                  "fluid-let" "load" "include" "with-exception-handler"
                  "trace-define" "trace-lambda"))
      ht))

  ;; Builtin shell commands -- hardcoded to avoid a circular dependency
  ;; with (hafod shell builtins).  alias/unalias and the z/zi jump commands are
  ;; listed here too so a line that manages aliases or jumps classifies as a
  ;; builtin -- dispatched through run-builtin! rather than evaluated as Scheme,
  ;; and never mistaken for an unknown command or an auto-cd target.  The twin
  ;; name list in the builtins module carries their actual dispatch and must stay
  ;; in step with this one.
  (define builtin-names-set
    (let ([ht (make-hashtable string-hash string=?)])
      (for-each (lambda (n) (hashtable-set! ht n #t))
                '("cd" "pushd" "popd" "export" "jobs" "fg" "bg" "alias" "unalias" "z" "zi"))
      ht))

  ;; PATH cache: command-name -> #t for O(1) lookup
  (define path-cache-ht (make-hashtable string-hash string=?))

  (define (path-cache) path-cache-ht)

  ;; Memoised list of the PATH cache keys, held in a mutable box (#f = stale).
  ;; The suggestion path materialises the key set once and reuses it, rather
  ;; than allocating a fresh list on every line; rebuild-path-cache! clears the
  ;; box so a PATH change refreshes the key-list together with the command cache.
  (define path-cache-keys-box (vector #f))

  (define (path-cache-keys)
    (or (vector-ref path-cache-keys-box 0)
        (let ([keys (vector->list (hashtable-keys path-cache-ht))])
          (vector-set! path-cache-keys-box 0 keys)
          keys)))

  (define (rebuild-path-cache!)
    (hashtable-clear! path-cache-ht)
    (for-each
      (lambda (dir)
        (when (file-directory? dir)
          (for-each
            (lambda (name)
              (hashtable-set! path-cache-ht name #t))
            (directory-list dir))))
      (exec-path-list))
    ;; Invalidate the memoised key-list so the next path-cache-keys rebuilds it.
    (vector-set! path-cache-keys-box 0 #f))

  ;; Alias table: command-head name -> expansion string.  Same shape as
  ;; path-cache-ht (a string-keyed hashtable), but the value is the expansion
  ;; text rather than a bare presence flag, because an alias rewrites the head
  ;; rather than merely confirming it exists.  Read by classify-input (through
  ;; alias-expand-line) and by the alias/unalias builtins.
  (define alias-table-ht (make-hashtable string-hash string=?))

  (define (alias-set! name expansion) (hashtable-set! alias-table-ht name expansion))
  (define (alias-remove! name) (hashtable-delete! alias-table-ht name))
  (define (alias-ref name) (hashtable-ref alias-table-ht name #f))
  (define (alias-names) (vector->list (hashtable-keys alias-table-ht)))

  ;; Extract the first whitespace-delimited token from a string,
  ;; starting at position i. Returns "" if no token found.
  (define (extract-first-token str i)
    (let ([len (string-length str)])
      ;; skip leading whitespace
      (let skip ([j i])
        (cond
          [(>= j len) ""]
          [(char-whitespace? (string-ref str j)) (skip (+ j 1))]
          [else
           ;; collect chars until whitespace or end
           (let collect ([k j] [acc '()])
             (cond
               [(>= k len)
                (list->string (reverse acc))]
               [(char-whitespace? (string-ref str k))
                (list->string (reverse acc))]
               [else
                (collect (+ k 1) (cons (string-ref str k) acc))]))]))))

  ;; Check if string looks like a self-evaluating literal:
  ;; numbers, strings (starts with "), #t, #f, #\...
  (define (self-evaluating-literal? tok)
    (and (> (string-length tok) 0)
         (or (string->number tok)
             (char=? (string-ref tok 0) #\")
             (and (> (string-length tok) 1)
                  (char=? (string-ref tok 0) #\#)
                  (memv (string-ref tok 1) '(#\t #\f #\\))))))

  ;; A head is expandable only when it is NOT a builtin and NOT a Scheme keyword
  ;; (both of which win outright) AND it is present in the alias table.  Sharing
  ;; this predicate keeps classify-input and alias-expand-line in lock-step on
  ;; precedence, and enforces the rule that a builtin cannot be shadowed by an
  ;; alias.  Returns the expansion string, or #f when the head is not aliasable.
  (define (aliasable-expansion head)
    (and (not (hashtable-ref builtin-names-set head #f))
         (not (hashtable-ref scheme-keywords head #f))
         (hashtable-ref alias-table-ht head #f)))

  ;; Split a line into (values head rest): the first whitespace-delimited token
  ;; and the remainder of the line from the first character after that token,
  ;; with any leading whitespace already skipped.  The same scan shape as
  ;; extract-first-token, but the tail is kept rather than discarded so the
  ;; remaining words can be re-attached verbatim.  A blank or whitespace-only
  ;; line yields ("" "").
  (define (split-head+rest str)
    (let ([len (string-length str)])
      (let skip ([j 0])
        (cond
          [(>= j len) (values "" "")]
          [(char-whitespace? (string-ref str j)) (skip (+ j 1))]
          [else
           (let collect ([k j])
             (cond
               [(>= k len) (values (substring str j k) "")]
               [(char-whitespace? (string-ref str k))
                (values (substring str j k) (substring str k len))]
               [else (collect (+ k 1))]))]))))

  ;; Expand an aliased command head, recursively, appending the remaining words
  ;; verbatim.  Only the first token is looked up; the rest of the line keeps its
  ;; original spacing and quoting, so `ll 'a b'` becomes `ls -la 'a b'` and still
  ;; parses correctly downstream -- no positional substitution is performed.  The
  ;; recursion is bounded twice over: the in-progress `seen` head-name set stops
  ;; the instant a head recurs, so a self-referential alias (`ls` -> `ls --color`)
  ;; or a mutual pair settles to a fixed point rather than looping; a fixed depth
  ;; cap is a defensive backstop.  This mirrors bash: an alias is not re-expanded
  ;; while it is already being expanded.
  (define (alias-expand-line str)
    (let loop ([line str] [seen '()] [depth 0])
      (let-values ([(head rest) (split-head+rest line)])
        (let ([exp (and (> (string-length head) 0)
                        (not (member head seen))
                        (< depth 50)
                        (aliasable-expansion head))])
          (if exp
              (loop (string-append exp rest) (cons head seen) (+ depth 1))
              line)))))

  ;; Command-not-found gate: return #t when the first token should NOT trigger a
  ;; "command not found" suggestion -- because it is empty, a builtin, a Scheme
  ;; keyword, a self-evaluating literal, a defined alias, an already-known PATH
  ;; command, or a bound top-level identifier (a procedure or variable such as
  ;; car, list, or a user define).  Only a genuinely-unknown token returns #f --
  ;; the sole case that should yield a suggestion.  The alias clause makes an
  ;; aliased head a known head, which is the resolution seam later highlighting
  ;; reads so an aliased head is never marked unknown.  Keep the keyword check:
  ;; top-level-bound? reports #f for macros/keywords, which are bound as syntax
  ;; rather than as top-level variables.
  (define (command-not-found-suppress? cmd)
    (and (or (= (string-length cmd) 0)
             (hashtable-ref builtin-names-set cmd #f)
             (hashtable-ref scheme-keywords cmd #f)
             (self-evaluating-literal? cmd)
             (hashtable-ref alias-table-ht cmd #f)
             (hashtable-ref path-cache-ht cmd #f)
             (top-level-bound? (string->symbol cmd)))
         #t))

  ;; At most three fuzzy suggestions for an unknown command, drawn from the
  ;; memoised PATH key-list.  Callers invoke this lazily -- only after
  ;; command-not-found-suppress? has declined to suppress -- so no PATH scan runs
  ;; on a suppressed line.
  (define (command-not-found-suggestions cmd)
    (let ([filtered (fuzzy-filter cmd (path-cache-keys))])
      (if (> (length filtered) 3)
          (list (car filtered) (cadr filtered) (caddr filtered))
          filtered)))

  ;; Classify a line whose aliases have already been expanded: returns 'scheme,
  ;; 'builtin, or 'shell from the command head alone.  No alias arm lives here --
  ;; the head is substituted upstream by classify-input, so an aliased head has
  ;; already become its expansion's head by the time this cond runs.
  (define (classify-input-raw str)
    (let ([len (string-length str)])
      ;; Skip leading whitespace to find first non-ws char
      (let skip ([i 0])
        (cond
          [(>= i len) 'scheme]  ;; empty or whitespace-only
          [(char-whitespace? (string-ref str i)) (skip (+ i 1))]
          [else
           (let ([c (string-ref str i)])
             (cond
               ;; Scheme prefix character
               [(memv c scheme-prefix-chars) 'scheme]
               [else
                (let ([tok (extract-first-token str i)])
                  (cond
                    ;; Builtin command
                    [(hashtable-ref builtin-names-set tok #f) 'builtin]
                    ;; Scheme keyword
                    [(hashtable-ref scheme-keywords tok #f) 'scheme]
                    ;; Self-evaluating literal
                    [(self-evaluating-literal? tok) 'scheme]
                    ;; Found in PATH
                    [(hashtable-ref path-cache-ht tok #f) 'shell]
                    ;; Default: Scheme
                    [else 'scheme]))]))]))))

  ;; Main classifier: returns 'scheme, 'builtin, or 'shell.  Aliases are expanded
  ;; first, so an aliased head classifies as its expanded head's class through the
  ;; same command-head cond -- `ll foo` with `ll` -> `ls -la` becomes `ls -la foo`
  ;; and resolves as 'shell; `up` with `up` -> `cd ..` becomes a builtin.  The
  ;; expansion is pure string/hashtable work with no filesystem access, so this is
  ;; safe to call on every render.
  (define (classify-input str)
    (classify-input-raw (alias-expand-line str)))
)
