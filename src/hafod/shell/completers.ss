;;; (hafod shell completers) -- Programmable command-specific completions
;;; Provides a registry mapping command names to completer procedures,
;;; plus built-in completers for git, ssh, kill, and make.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod shell completers)
  (export register-completer! lookup-completer completer-names
          git-completer ssh-completer kill-completer make-completer)
  (import (chezscheme)
          (hafod fuzzy))

  ;; Registry: command-name → completer procedure
  ;; A completer is (lambda (prefix context) -> list of (name . description))
  ;; where context is an alist with keys like 'args (previous args on line).
  (define *completers* (make-hashtable string-hash string=?))

  (define (register-completer! cmd proc)
    (hashtable-set! *completers* cmd proc))

  (define (lookup-completer cmd)
    (hashtable-ref *completers* cmd #f))

  (define (completer-names)
    (vector->list (hashtable-keys *completers*)))

  ;; ======================================================================
  ;; Helper: run a command and collect output lines
  ;; ======================================================================

  (define (command-output cmd . args)
    (guard (e [#t '()])
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports
                      (apply string-append cmd
                             (map (lambda (a) (string-append " " a)) args))
                      (buffer-mode block)
                      (make-transcoder (utf-8-codec)))])
        (close-port to-stdin)
        (let loop ([acc '()])
          (let ([line (get-line from-stdout)])
            (if (eof-object? line)
                (begin
                  (close-port from-stdout)
                  (close-port from-stderr)
                  (reverse acc))
                (loop (cons line acc))))))))

  ;; ======================================================================
  ;; Git completer
  ;; ======================================================================

  (define (git-subcommands)
    '("add" "bisect" "branch" "checkout" "cherry-pick" "clone" "commit"
      "diff" "fetch" "grep" "init" "log" "merge" "mv" "pull" "push"
      "rebase" "remote" "reset" "restore" "revert" "rm" "show" "stash"
      "status" "switch" "tag" "worktree"))

  (define (git-subcommand-descriptions)
    '(("add" . "stage files")
      ("bisect" . "binary search for bugs")
      ("branch" . "list/create/delete branches")
      ("checkout" . "switch branches or restore files")
      ("cherry-pick" . "apply commit changes")
      ("clone" . "clone a repository")
      ("commit" . "record changes")
      ("diff" . "show changes")
      ("fetch" . "download remote refs")
      ("grep" . "search tracked files")
      ("init" . "create repository")
      ("log" . "show commit history")
      ("merge" . "join branches")
      ("mv" . "move/rename files")
      ("pull" . "fetch and merge")
      ("push" . "update remote refs")
      ("rebase" . "reapply commits")
      ("remote" . "manage remotes")
      ("reset" . "reset HEAD")
      ("restore" . "restore working tree files")
      ("revert" . "revert commits")
      ("rm" . "remove files")
      ("show" . "show objects")
      ("stash" . "stash changes")
      ("status" . "show working tree status")
      ("switch" . "switch branches")
      ("tag" . "manage tags")
      ("worktree" . "manage worktrees")))

  (define (git-branches)
    (let ([lines (command-output "git" "branch" "--list" "--format=%(refname:short)")])
      (map (lambda (line)
             ;; strip leading whitespace/asterisk
             (let loop ([i 0])
               (if (and (< i (string-length line))
                        (or (char-whitespace? (string-ref line i))
                            (char=? (string-ref line i) #\*)))
                   (loop (+ i 1))
                   (substring line i (string-length line)))))
           lines)))

  (define (git-modified-files)
    (let ([lines (command-output "git" "status" "--porcelain")])
      (filter (lambda (s) (> (string-length s) 0))
              (map (lambda (line)
                     (if (> (string-length line) 3)
                         ;; skip 2-char status + space
                         (let ([f (substring line 3 (string-length line))])
                           ;; strip trailing whitespace
                           (let loop ([i (- (string-length f) 1)])
                             (if (and (>= i 0) (char-whitespace? (string-ref f i)))
                                 (loop (- i 1))
                                 (substring f 0 (+ i 1)))))
                         ""))
                   lines))))

  (define (git-completer prefix context)
    (let ([args (cdr (assq 'args context))])
      (cond
        ;; No subcommand yet: complete subcommands
        [(null? args)
         (let* ([descs (git-subcommand-descriptions)]
                [matches (fuzzy-filter/positions prefix (git-subcommands))])
           (map (lambda (m)
                  (let ([name (car m)]
                        [positions (cdr m)])
                    (let ([desc (assoc name descs)])
                      (list name positions (if desc (cdr desc) #f)))))
                matches))]
        ;; After subcommand: context-specific
        [else
         (let ([sub (car args)])
           (cond
             ;; Branch-taking subcommands
             [(member sub '("checkout" "switch" "merge" "rebase" "branch" "diff" "log"))
              (let ([branches (git-branches)])
                (map (lambda (m) (list (car m) (cdr m) "branch"))
                     (fuzzy-filter/positions prefix branches)))]
             ;; File-taking subcommands
             [(member sub '("add" "restore" "rm" "mv" "diff" "reset"))
              (let ([files (git-modified-files)])
                (map (lambda (m) (list (car m) (cdr m) "modified"))
                     (fuzzy-filter/positions prefix files)))]
             [else '()]))])))

  ;; ======================================================================
  ;; SSH completer — hostnames from known_hosts and config
  ;; ======================================================================

  (define (ssh-hosts)
    (let ([hosts '()]
          [home (or (getenv "HOME") "")])
      ;; Parse known_hosts
      (guard (e [#t #f])
        (let ([p (open-input-file (string-append home "/.ssh/known_hosts"))])
          (let loop ()
            (let ([line (get-line p)])
              (unless (eof-object? line)
                (when (and (> (string-length line) 0)
                           (not (char=? (string-ref line 0) #\#)))
                  ;; First field before space is hostname (possibly hashed)
                  (let ([end (let scan ([i 0])
                               (cond
                                 [(>= i (string-length line)) i]
                                 [(or (char=? (string-ref line i) #\space)
                                      (char=? (string-ref line i) #\,)) i]
                                 [else (scan (+ i 1))]))])
                    (let ([host (substring line 0 end)])
                      ;; Skip hashed entries (start with |)
                      (unless (and (> (string-length host) 0)
                                   (char=? (string-ref host 0) #\|))
                        (set! hosts (cons host hosts))))))
                (loop))))
          (close-port p)))
      ;; Parse ssh config Host entries
      (guard (e [#t #f])
        (let ([p (open-input-file (string-append home "/.ssh/config"))])
          (let loop ()
            (let ([line (get-line p)])
              (unless (eof-object? line)
                ;; Match lines starting with "Host " (case-insensitive)
                (let ([trimmed (let skip ([i 0])
                                 (if (and (< i (string-length line))
                                          (char-whitespace? (string-ref line i)))
                                     (skip (+ i 1))
                                     (substring line i (string-length line))))])
                  (when (and (>= (string-length trimmed) 5)
                             (let ([pfx (substring trimmed 0 5)])
                               (or (string=? pfx "Host ")
                                   (string=? pfx "host "))))
                    ;; Extract hostname (skip "Host ")
                    (let ([host (let skip ([i 5])
                                  (if (and (< i (string-length trimmed))
                                           (char-whitespace? (string-ref trimmed i)))
                                      (skip (+ i 1))
                                      (let end ([j i])
                                        (if (or (>= j (string-length trimmed))
                                                (char-whitespace? (string-ref trimmed j)))
                                            (substring trimmed i j)
                                            (end (+ j 1))))))])
                      ;; Skip wildcards
                      (unless (let loop ([i 0])
                                (cond
                                  [(>= i (string-length host)) #f]
                                  [(char=? (string-ref host i) #\*) #t]
                                  [(char=? (string-ref host i) #\?) #t]
                                  [else (loop (+ i 1))]))
                        (set! hosts (cons host hosts))))))
                (loop))))
          (close-port p)))
      ;; Deduplicate
      (let ([ht (make-hashtable string-hash string=?)])
        (filter (lambda (h)
                  (if (hashtable-ref ht h #f)
                      #f
                      (begin (hashtable-set! ht h #t) #t)))
                (reverse hosts)))))

  (define (ssh-completer prefix context)
    (let ([hosts (ssh-hosts)])
      (map (lambda (m) (list (car m) (cdr m) "host"))
           (fuzzy-filter/positions prefix hosts))))

  ;; ======================================================================
  ;; Kill completer — PIDs from process table
  ;; ======================================================================

  (define (kill-completer prefix context)
    (let* ([lines (command-output "ps" "-eo" "pid=" "-eo" "comm=")]
           [entries
            (filter (lambda (e) (> (string-length (car e)) 0))
              (map (lambda (line)
                     (let* ([trimmed (let skip ([i 0])
                                       (if (and (< i (string-length line))
                                                (char-whitespace? (string-ref line i)))
                                           (skip (+ i 1))
                                           (substring line i (string-length line))))]
                            [space (let scan ([i 0])
                                     (cond
                                       [(>= i (string-length trimmed)) #f]
                                       [(char-whitespace? (string-ref trimmed i)) i]
                                       [else (scan (+ i 1))]))])
                       (if space
                           (cons (substring trimmed 0 space)
                                 (let skip ([i (+ space 1)])
                                   (if (and (< i (string-length trimmed))
                                            (char-whitespace? (string-ref trimmed i)))
                                       (skip (+ i 1))
                                       (substring trimmed i (string-length trimmed)))))
                           (cons trimmed ""))))
                   lines))]
           [pids (map car entries)]
           [desc-ht (let ([ht (make-hashtable string-hash string=?)])
                      (for-each (lambda (e) (hashtable-set! ht (car e) (cdr e))) entries)
                      ht)])
      (map (lambda (m)
             (list (car m) (cdr m)
                   (hashtable-ref desc-ht (car m) #f)))
           (fuzzy-filter/positions prefix pids))))

  ;; ======================================================================
  ;; Make completer — targets from Makefile
  ;; ======================================================================

  (define (make-targets)
    (guard (e [#t '()])
      (let ([p (open-input-file "Makefile")])
        (let loop ([acc '()])
          (let ([line (get-line p)])
            (if (eof-object? line)
                (begin (close-port p) (reverse acc))
                (if (and (> (string-length line) 0)
                         (not (char=? (string-ref line 0) #\tab))
                         (not (char=? (string-ref line 0) #\#))
                         (not (char=? (string-ref line 0) #\.)))
                    ;; Find colon
                    (let ([colon (let scan ([i 0])
                                   (cond
                                     [(>= i (string-length line)) #f]
                                     [(char=? (string-ref line i) #\:) i]
                                     [else (scan (+ i 1))]))])
                      (if (and colon (> colon 0)
                               ;; Not a variable assignment (no = before :)
                               (not (let scan ([i 0])
                                      (cond
                                        [(>= i colon) #f]
                                        [(char=? (string-ref line i) #\=) #t]
                                        [else (scan (+ i 1))]))))
                          (let ([target (substring line 0 colon)])
                            ;; Skip pattern rules (contain %)
                            (if (let scan ([i 0])
                                  (cond
                                    [(>= i (string-length target)) #f]
                                    [(char=? (string-ref target i) #\%) #t]
                                    [else (scan (+ i 1))]))
                                (loop acc)
                                (loop (cons target acc))))
                          (loop acc)))
                    (loop acc))))))))

  (define (make-completer prefix context)
    (let ([targets (make-targets)])
      (map (lambda (m) (list (car m) (cdr m) "target"))
           (fuzzy-filter/positions prefix targets))))

  ;; ======================================================================
  ;; Register built-in completers
  ;; ======================================================================

  (register-completer! "git" git-completer)
  (register-completer! "ssh" ssh-completer)
  (register-completer! "scp" ssh-completer)
  (register-completer! "kill" kill-completer)
  (register-completer! "killall" kill-completer)
  (register-completer! "make" make-completer)
  (register-completer! "gmake" make-completer)

) ; end library
