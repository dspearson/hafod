;;; (hafod shell builtins) -- Shell builtin commands
;;; cd, pushd, popd, export -- must execute in-process.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod shell builtins)
  (export builtin? run-builtin! builtin-names dir-stack)
  (import (except (chezscheme) getenv)
          (only (hafod process-state) chdir cwd)
          (only (hafod environment) getenv setenv)
          (only (hafod user-group) home-directory)
          (only (hafod process) init-exec-path-list)
          (only (hafod shell parser) parse-command-words)
          (only (hafod shell classifier) rebuild-path-cache!)
          (only (hafod shell jobs) list-jobs job-fg! job-bg-resume!))

  ;; Directory stack for pushd/popd
  (define dir-stack-list '())

  (define (dir-stack) dir-stack-list)

  (define builtin-name-list '("cd" "pushd" "popd" "export" "jobs" "fg" "bg"))

  (define (builtin-names) builtin-name-list)

  (define (builtin? name)
    (and (member name builtin-name-list) #t))

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
          (guard (e [#t (display
                          (format "popd: ~a: ~a\n"
                                  target
                                  (if (condition? e)
                                      (condition-message e)
                                      e))
                          (console-error-port))])
            (chdir target)
            (set! dir-stack-list (cdr dir-stack-list))
            (setenv "OLDPWD" old)
            (setenv "PWD" (cwd))))))

  ;; --- export ---
  ;; Apply every VAR=value pair, not just the first: split each argument on its
  ;; first `=` and set the variable. A bare NAME with no `=` marks an
  ;; already-set variable exported by re-setting its current value (a not-yet-set
  ;; bare name is left alone), so `export A=1 B=2 C` sets A and B and marks C.
  ;;
  ;; When an assignment changes PATH, re-read $PATH into the exec-path search
  ;; list and repopulate the classifier's command cache once, so a command that
  ;; has just become reachable classifies as a shell command within the session.
  ;; The rebuild fires only for a PATH assignment, not on every export.
  (define (builtin-export args)
    (let ([path-changed? #f])
      (for-each
        (lambda (arg)
          (let ([eqpos (let loop ([i 0])
                         (cond
                           [(>= i (string-length arg)) #f]
                           [(char=? (string-ref arg i) #\=) i]
                           [else (loop (+ i 1))]))])
            (if eqpos
                (let ([var (substring arg 0 eqpos)])
                  (setenv var (substring arg (+ eqpos 1) (string-length arg)))
                  (when (string=? var "PATH")
                    (set! path-changed? #t)))
                (let ([cur (getenv arg)])
                  (when cur (setenv arg cur))))))
        args)
      (when path-changed?
        (init-exec-path-list)
        (rebuild-path-cache!))))

  ;; --- Dispatcher ---
  ;; Split and expand the whole line once through the shared parser tokeniser,
  ;; so builtin arguments undergo the same $/~/quote/escape/glob expansion as an
  ;; external command's. Because the tokeniser skips a leading empty word,
  ;; leading whitespace before the command name is tolerated ("   cd /tmp").
  (define (run-builtin! str)
    (let* ([words (parse-command-words str)]
           [cmd (if (null? words) "" (car words))]
           [args (if (null? words) '() (cdr words))])
      (cond
          [(string=? cmd "cd") (builtin-cd args)]
          [(string=? cmd "pushd") (builtin-pushd args)]
          [(string=? cmd "popd") (builtin-popd args)]
          [(string=? cmd "export") (builtin-export args)]
          [(string=? cmd "jobs") (list-jobs)]
          [(string=? cmd "fg") (job-fg! (if (null? args) "" (car args)))]
          [(string=? cmd "bg") (job-bg-resume! (if (null? args) "" (car args)))]
          [else (display (format "~a: not a builtin\n" cmd) (console-error-port))])))
)
