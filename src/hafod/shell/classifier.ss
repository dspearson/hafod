;;; (hafod shell classifier) -- Input classification for shell mode
;;; Routes user input to Scheme eval, shell parser, or builtin execution.
;;; Phase 36 Plan 01
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod shell classifier)
  (export classify-input rebuild-path-cache! path-cache scheme-prefix-chars)
  (import (chezscheme)
          (only (hafod process) exec-path-list))

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

  ;; Builtin shell commands -- hardcoded to avoid circular dependency
  ;; with (hafod shell builtins).
  (define builtin-names-set
    (let ([ht (make-hashtable string-hash string=?)])
      (for-each (lambda (n) (hashtable-set! ht n #t))
                '("cd" "pushd" "popd" "export" "jobs" "fg" "bg"))
      ht))

  ;; PATH cache: command-name -> #t for O(1) lookup
  (define path-cache-ht (make-hashtable string-hash string=?))

  (define (path-cache) path-cache-ht)

  (define (rebuild-path-cache!)
    (hashtable-clear! path-cache-ht)
    (for-each
      (lambda (dir)
        (when (file-directory? dir)
          (for-each
            (lambda (name)
              (hashtable-set! path-cache-ht name #t))
            (directory-list dir))))
      (exec-path-list)))

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

  ;; Main classifier: returns 'scheme, 'builtin, or 'shell
  (define (classify-input str)
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
)
