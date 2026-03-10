;;; (hafod shell builtins) -- Shell builtin commands
;;; cd, pushd, popd, export -- must execute in-process.
;;; Phase 36 Plan 01
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod shell builtins)
  (export builtin? run-builtin! builtin-names dir-stack)
  (import (except (chezscheme) getenv)
          (only (hafod process-state) chdir cwd)
          (only (hafod environment) getenv setenv)
          (only (hafod user-group) home-directory))

  ;; Directory stack for pushd/popd
  (define dir-stack-list '())

  (define (dir-stack) dir-stack-list)

  (define builtin-name-list '("cd" "pushd" "popd" "export"))

  (define (builtin-names) builtin-name-list)

  (define (builtin? name)
    (and (member name builtin-name-list) #t))

  ;; Parse arguments from the input string after the first token.
  ;; Returns a list of argument strings. Respects double and single quotes.
  (define (parse-args str)
    (let ([len (string-length str)])
      ;; Skip the first token (command name)
      (let skip-cmd ([i 0])
        (cond
          [(>= i len) '()]
          [(char-whitespace? (string-ref str i)) (parse-rest str i len)]
          [else (skip-cmd (+ i 1))]))))

  (define (parse-rest str i len)
    ;; Skip whitespace, then collect tokens
    (let loop ([j i] [args '()])
      (cond
        [(>= j len) (reverse args)]
        [(char-whitespace? (string-ref str j)) (loop (+ j 1) args)]
        [else
         (let-values ([(tok next) (scan-token str j len)])
           (loop next (cons tok args)))])))

  (define (scan-token str i len)
    ;; Read one token, handling quotes
    (let ([c (string-ref str i)])
      (cond
        [(or (char=? c #\") (char=? c #\'))
         (read-quoted str (+ i 1) len c '())]
        [else
         (read-unquoted str i len '())])))

  (define (read-quoted str i len q acc)
    (cond
      [(>= i len) (values (list->string (reverse acc)) i)]
      [(char=? (string-ref str i) q)
       (values (list->string (reverse acc)) (+ i 1))]
      [else
       (read-quoted str (+ i 1) len q (cons (string-ref str i) acc))]))

  (define (read-unquoted str i len acc)
    (cond
      [(>= i len) (values (list->string (reverse acc)) i)]
      [(char-whitespace? (string-ref str i))
       (values (list->string (reverse acc)) i)]
      [else
       (read-unquoted str (+ i 1) len (cons (string-ref str i) acc))]))

  ;; --- cd ---
  (define (builtin-cd args)
    (let* ([old (cwd)]
           [target (cond
                     [(null? args) (home-directory)]
                     [(string=? (car args) "-")
                      (or (getenv "OLDPWD")
                          (begin
                            (display "cd: OLDPWD not set\n" (console-error-port))
                            #f))]
                     [else (car args)])])
      (when target
        (guard (e [#t (display
                        (format "cd: ~a: ~a\n"
                                target
                                (if (condition? e)
                                    (condition-message e)
                                    e))
                        (console-error-port))])
          (chdir target)
          (setenv "OLDPWD" old)
          (setenv "PWD" (cwd))))))

  ;; --- pushd ---
  (define (builtin-pushd args)
    (if (null? args)
        (display "pushd: no directory specified\n" (console-error-port))
        (let ([old (cwd)])
          (guard (e [#t (display
                          (format "pushd: ~a: ~a\n"
                                  (car args)
                                  (if (condition? e)
                                      (condition-message e)
                                      e))
                          (console-error-port))])
            (chdir (car args))
            (set! dir-stack-list (cons old dir-stack-list))
            (setenv "OLDPWD" old)
            (setenv "PWD" (cwd))))))

  ;; --- popd ---
  (define (builtin-popd args)
    (if (null? dir-stack-list)
        (display "popd: directory stack empty\n" (console-error-port))
        (let ([target (car dir-stack-list)]
              [old (cwd)])
          (set! dir-stack-list (cdr dir-stack-list))
          (chdir target)
          (setenv "OLDPWD" old)
          (setenv "PWD" (cwd)))))

  ;; --- export ---
  (define (builtin-export args)
    (when (pair? args)
      (let ([arg (car args)])
        (let ([eqpos (let loop ([i 0])
                       (cond
                         [(>= i (string-length arg)) #f]
                         [(char=? (string-ref arg i) #\=) i]
                         [else (loop (+ i 1))]))])
          (if eqpos
              (let ([var (substring arg 0 eqpos)]
                    [val (substring arg (+ eqpos 1) (string-length arg))])
                (setenv var val))
              ;; No =, just mark for export (setenv with current value)
              (let ([cur (getenv arg)])
                (when cur (setenv arg cur))))))))

  ;; --- Dispatcher ---
  (define (run-builtin! str)
    (let ([args (parse-args str)])
      ;; Extract command name (first token)
      (let ([cmd (let loop ([i 0] [len (string-length str)])
                   (cond
                     [(>= i len) (substring str 0 len)]
                     [(char-whitespace? (string-ref str i)) (substring str 0 i)]
                     [else (loop (+ i 1) len)]))])
        (cond
          [(string=? cmd "cd") (builtin-cd args)]
          [(string=? cmd "pushd") (builtin-pushd args)]
          [(string=? cmd "popd") (builtin-popd args)]
          [(string=? cmd "export") (builtin-export args)]
          [else (display (format "~a: not a builtin\n" cmd) (console-error-port))]))))
)
