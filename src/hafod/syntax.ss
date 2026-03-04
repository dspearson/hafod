#!chezscheme
;;; (hafod syntax) -- EPF Process Notation for hafod
;;; Note: #!chezscheme required for || symbol
;;; The core user-facing feature: scsh-style process forms translated to
;;; runtime calls via syntax-case macros.
;;; Ported from scsh/scheme/syntax.scm and scsh/scheme/syntax-helpers.scm
;;; Rewritten from explicit-renaming macros to R6RS syntax-case.
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod syntax)
  (export
    ;; Core EPF macro
    exec-epf

    ;; Process form wrappers
    & run

    ;; Sequencing operators
    || :or: &&

    ;; Runtime helpers (used by macro expansions and useful standalone)
    open-string-source with-stdio-ports* stdports->stdio
    <<-port-holder <<-port-holder-set!

    ;; Collector macros
    run/port run/string run/strings run/sexp run/sexps
    run/port+proc run/collecting run/file)

  (import (hafod internal base)
          (hafod posix) (hafod compat) (hafod procobj)
          (hafod fd-ports) (hafod process) (hafod collect) (hafod port-collect)
          (only (hafod environment) getenv))

  ;; File option aliases (read-only, create+trunc, write+append+create)
  ;; are imported from (hafod fd-ports)

  ;; ======================================================================
  ;; <<-port-holder: prevents GC from closing the temp port before exec
  ;; Uses a vector box because R6RS forbids set! on exported variables.
  ;; ======================================================================

  (define <<-port-holder (vector #f))

  (define (<<-port-holder-set! port)
    (vector-set! <<-port-holder 0 port))

  ;; ======================================================================
  ;; open-string-source: create temp file containing displayed obj
  ;; Returns an input port for reading the content back.
  ;; ======================================================================

  (define (open-string-source obj)
    (receive (path fd) (posix-mkstemp (string-append
                                        (or (getenv "TMPDIR") "/tmp")
                                        "/hafod-heredoc-XXXXXX"))
      (let ([outp (fdes->outport fd)])
        (display obj outp)
        (close outp)
        ;; Reopen for reading, then unlink (fd keeps data alive)
        (let ([inp (open-file path open/read)])
          (posix-unlink path)
          inp))))

  ;; with-stdio-ports* is imported from (hafod fd-ports)

  ;; ======================================================================
  ;; stdports->stdio: move current Scheme ports to fds 0/1/2
  ;; ======================================================================

  (define (stdports->stdio)
    ;; Only move if the port is tracked in the fd-port table (is an fdport).
    (when (fdport? (current-input-port))
      (move->fdes (current-input-port) 0))
    (when (fdport? (current-output-port))
      (move->fdes (current-output-port) 1))
    (when (fdport? (current-error-port))
      (move->fdes (current-error-port) 2)))

  ;; ======================================================================
  ;; The exec-epf macro -- core EPF translator
  ;; ======================================================================
  ;;
  ;; Translates scsh Extended Process Forms into runtime Scheme code.
  ;; Ported from scsh's syntax-helpers.scm (explicit-renaming macros)
  ;; to R6RS syntax-case for Chez Scheme.
  ;;
  ;; EPF Grammar:
  ;;
  ;;   epf      ::= (pf redir ...)
  ;;   pf       ::= (begin body ...)           ; Scheme code in subprocess
  ;;              | (pipe pf1 ... pfn)          ; simple pipeline (alias: |)
  ;;              | (pipe+ conns pf1 ... pfn)   ; complex pipeline (alias: |+)
  ;;              | (epf pf redir ...)          ; nested EPF
  ;;              | (prog arg ...)              ; external program (default)
  ;;   redir    ::= (< [fdes] fname)           ; input from file
  ;;              | (> [fdes] fname)            ; output to file (create/trunc)
  ;;              | (>> [fdes] fname)           ; output to file (append)
  ;;              | (<< [fdes] object)          ; here-string (display obj)
  ;;              | (= fdes1 fdes2)             ; dup fd2 onto fd1
  ;;              | (- fdes)                    ; close fd
  ;;              | stdports                    ; sync fds 0,1,2 from ports
  ;;
  ;; Expansion Strategy:
  ;;   - External programs are implicitly quasiquoted:
  ;;     (exec-epf (echo ,var)) => (apply exec-path `(echo ,var))
  ;;   - Pipelines: all-but-last stages forked via fork/pipe, last runs
  ;;     in current process. E.g. (exec-epf (pipe (ls) (grep "foo")))
  ;;     expands to:
  ;;       (begin (fork/pipe (lambda () (apply exec-path '(ls))))
  ;;              (apply exec-path '(grep "foo")))
  ;;   - Redirections expand to shell-open/dup->fdes/close/move->fdes
  ;;     calls executed before the process form
  ;;   - (begin ...) forms are wrapped with with-stdio-ports* to sync
  ;;     Scheme ports with Unix fds after redirections
  ;;
  ;; Key Design Note (syntax hygiene):
  ;;   All transcribe-* helpers are defined inside the transformer lambda
  ;;   so they exist at expansion time (phase 1). datum->syntax uses the
  ;;   macro keyword as context to inject identifiers into the caller's
  ;;   scope. Runtime procedures (shell-open, fork/pipe, exec-path, etc.)
  ;;   are referenced as template identifiers via #'... syntax, resolved
  ;;   in the importing module's environment.
  ;;
  ;; IMPORTANT: datum->syntax requires an identifier as first arg, not
  ;; a full syntax form. We extract the keyword from the macro use.
  ;;
  ;; IMPORTANT: When building datum forms for datum->syntax, we must use
  ;; (list 'quasiquote datum) instead of `(quasiquote ,datum) because
  ;; the Scheme reader's quasiquote expansion happens at read time, not
  ;; at the time datum->syntax runs.
  ;;
  ;; Correspondence to scsh (syntax-helpers.scm):
  ;;   parse-redir-spec            <= parse-spec (inline lambda)
  ;;   transcribe-redirection      <= transcribe-redirection
  ;;   transcribe-process-form     <= transcribe-process-form
  ;;   make-begin-syntax           <= blockify (simplified, no deblocking)
  ;;   transcribe-simple-pipeline  <= transcribe-simple-pipeline
  ;;   transcribe-complex-pipeline <= transcribe-complex-pipeline
  ;;   transcribe-extended-process-form <= transcribe-extended-process-form

  (define-syntax exec-epf
    (lambda (stx)

      ;; Extract the keyword identifier for datum->syntax context.
      (define ctx (syntax-case stx () [(kw . rest) #'kw]))

      ;; ---- parse-redir-spec ---------------------------------------------------
      ;; Parse a ([fdes] arg) redirection spec into (values fdes-int arg-stx).
      ;;
      ;; Input:   args-stxs    -- list of 1 or 2 syntax objects from the redir form
      ;;          default-fdes -- integer fd to use when fdes is omitted
      ;;                          (0 for input redirs: <, <<; 1 for output: >, >>)
      ;;
      ;; Output:  (values fdes arg-stx) where fdes is a Scheme integer and
      ;;          arg-stx is a syntax object (preserved for variable references).
      ;;
      ;; Used by: transcribe-redirection for all file-based redirections.
      ;; Corresponds to: parse-spec lambda in scsh's transcribe-redirection
      ;; -----------------------------------------------------------------
      (define (parse-redir-spec args-stxs default-fdes)
        ;; Like scsh's parse-spec, wrap the argument in quasiquote so that
        ;; unquoted variables (,var) are evaluated at runtime.  Literal
        ;; strings pass through unchanged: `"foo" => "foo".
        ;; Uses with-syntax to preserve the original lexical context of
        ;; user-supplied syntax objects (critical for variable resolution).
        (define (backq stx)
          (with-syntax ([x stx])
            #'(quasiquote x)))
        (let ([n (length args-stxs)])
          (cond
            [(= n 1)
             (values default-fdes (backq (car args-stxs)))]
            [(= n 2)
             (values (syntax->datum (car args-stxs))
                     (backq (cadr args-stxs)))]
            [else
             (syntax-violation 'exec-epf "Bad redirection argument count" stx)])))

      ;; Helper: generate (shell-open fname mode fd) for <, >, >> redirections.
      ;; Deduplicates the identical let-values/with-syntax/shell-open pattern
      ;; used by all three file-open redirection operators.
      (define (transcribe-file-open args-stxs default-fdes mode-id)
        (let-values ([(fdes fname-stx) (parse-redir-spec args-stxs default-fdes)])
          (with-syntax ([fname fname-stx]
                        [fd (datum->syntax ctx fdes)]
                        [mode mode-id])
            #'(shell-open fname mode fd))))

      ;; ---- transcribe-redirection ---------------------------------------------
      ;; Translate a single EPF redirection form into a runtime expression.
      ;;
      ;; Input:   redir-stx -- syntax object for one redirection, one of:
      ;;            (< [fdes] fname), (> [fdes] fname), (>> [fdes] fname),
      ;;            (<< [fdes] object), (= fdes1 fdes2), (- fdes), or stdports
      ;;
      ;; Output:  A syntax object representing the runtime call:
      ;;   (< fname)      => (shell-open fname read-only 0)
      ;;   (< fd fname)   => (shell-open fname read-only fd)
      ;;   (> fname)      => (shell-open fname create+trunc 1)
      ;;   (>> fname)     => (shell-open fname write+append+create 1)
      ;;   (<< obj)       => (let ([port (open-string-source obj)])
      ;;                       (<<-port-holder-set! port)
      ;;                       (move->fdes port 0))
      ;;   (= fd1 fd2)    => (dup->fdes fd2 fd1)
      ;;   (- fd)         => (close fd)
      ;;   stdports       => (stdports->stdio)
      ;;
      ;; Filenames and expressions are kept as syntax objects to preserve
      ;; variable references under quasiquoting. Only fd numbers (always
      ;; literal integers) are extracted as datums via syntax->datum.
      ;;
      ;; Corresponds to: transcribe-redirection in scsh's syntax-helpers.scm
      ;; -----------------------------------------------------------------
      (define (transcribe-redirection redir-stx)
        (let ([redir (syntax->datum redir-stx)])
          (cond
            ;; Bare symbol: stdports
            [(eq? redir 'stdports)
             (datum->syntax ctx '(stdports->stdio))]

            [(not (pair? redir))
             (syntax-violation 'exec-epf "Unknown redirection" stx redir-stx)]

            [else
             (let* ([op (car redir)]
                    [args-stxs (cdr (syntax->list redir-stx))])
               (cond
                 [(eq? op '<)
                  (transcribe-file-open args-stxs 0 #'read-only)]

                 [(eq? op '>)
                  (transcribe-file-open args-stxs 1 #'create+trunc)]

                 [(eq? op '>>)
                  (transcribe-file-open args-stxs 1 #'write+append+create)]

                 [(eq? op '<<)
                  (let-values ([(fdes exp-stx) (parse-redir-spec args-stxs 0)])
                    (with-syntax ([exp exp-stx]
                                  [fd (datum->syntax ctx fdes)])
                      #'(let ([port (open-string-source exp)])
                          (<<-port-holder-set! port)
                          (move->fdes port fd))))]

                 [(eq? op '=)
                  (unless (= (length args-stxs) 2)
                    (syntax-violation 'exec-epf "= requires exactly 2 arguments" stx redir-stx))
                  (let ([fd1 (syntax->datum (car args-stxs))]
                        [fd2 (syntax->datum (cadr args-stxs))])
                    (with-syntax ([src (datum->syntax ctx fd2)]
                                  [tgt (datum->syntax ctx fd1)])
                      #'(dup->fdes src tgt)))]

                 [(eq? op '-)
                  (unless (= (length args-stxs) 1)
                    (syntax-violation 'exec-epf "- requires exactly 1 argument" stx redir-stx))
                  (let ([fd (syntax->datum (car args-stxs))])
                    (with-syntax ([fd-val (datum->syntax ctx fd)])
                      #'(close fd-val)))]

                 [else
                  (syntax-violation 'exec-epf "Unknown redirection operator" stx redir-stx)]))])))

      ;; ---- transcribe-process-form --------------------------------------------
      ;; Translate a single process form into runtime code for the current process.
      ;;
      ;; Input:   pf-stx -- syntax object for a process form (must be a list)
      ;;
      ;; Output:  A syntax object representing the expansion:
      ;;   (begin body ...)    => (with-stdio-ports* (lambda () body ...))
      ;;   (pipe pf1 ... pfn)  => delegated to transcribe-simple-pipeline
      ;;   (pipe+ conns pf...) => delegated to transcribe-complex-pipeline
      ;;   (epf pf redir ...)  => delegated to transcribe-extended-process-form
      ;;   (prog arg ...)      => (apply exec-path `(prog arg ...))
      ;;
      ;; The external program case uses implicit quasiquoting: the entire
      ;; argument list is wrapped in quasiquote so that (echo ,var) becomes
      ;; (apply exec-path `(echo ,var)) at runtime. Unquoted variables are
      ;; evaluated; everything else becomes a literal string/symbol.
      ;;
      ;; Note: (string->symbol "|") is needed because | cannot appear as a
      ;; literal symbol in Chez Scheme source code.
      ;;
      ;; Corresponds to: transcribe-process-form in scsh's syntax-helpers.scm
      ;; -----------------------------------------------------------------
      (define (transcribe-process-form pf-stx)
        (let ([pf (syntax->datum pf-stx)])
          (unless (and (list? pf) (pair? pf))
            (syntax-violation 'exec-epf "Illegal process form" stx pf-stx))
          (let ([head (car pf)])
            (cond
              [(eq? head 'begin)
               ;; (begin . body) => (with-stdio-ports* (lambda () . body))
               ;; Keep body as syntax to preserve variable references
               (let ([body (cdr (syntax->list pf-stx))])
                 (with-syntax ([(b ...) body])
                   #'(with-stdio-ports* (lambda () b ...))))]

              [(memq head (list 'pipe (string->symbol "|")))
               ;; Simple pipeline
               (transcribe-simple-pipeline (cdr (syntax->list pf-stx)))]

              [(memq head (list 'pipe+ (string->symbol "|+")))
               ;; Complex pipeline with explicit fd connections
               (let ([rest (cdr (syntax->list pf-stx))])
                 (transcribe-complex-pipeline (car rest) (cdr rest)))]

              [(eq? head 'epf)
               ;; Nested extended process form
               (transcribe-extended-process-form (cdr (syntax->list pf-stx)))]

              [else
               ;; External program: (prog arg ...) => (apply exec-path `(prog arg ...))
               ;; Implicit quasiquoting: the entire form is wrapped in quasiquote
               ;; so (echo ,var) becomes (apply exec-path `(echo ,var)) at runtime.
               ;; Uses with-syntax to preserve user lexical context for ,var references.
               (with-syntax ([pf pf-stx])
                 #'(apply exec-path (quasiquote pf)))]))))

      ;; -- Helper to combine a list of syntax forms into a begin block
      (define (make-begin-syntax forms)
        (if (= (length forms) 1)
            (car forms)
            (with-syntax ([(f ...) forms])
              #'(begin f ...))))

      ;; ---- build-pipeline ----------------------------------------------------
      ;; Shared pipeline assembly: transcribe each process form, fork all-but-last
      ;; stages using the make-forker-datum callback, run last in current process.
      ;; Used by both transcribe-simple-pipeline and transcribe-complex-pipeline.
      ;; The only difference between simple and complex is the forker datum shape
      ;; (fork/pipe vs fork/pipe+ with a connection list).
      ;; -----------------------------------------------------------------
      (define (build-pipeline pf-stxs make-forker-stx)
        (let* ([codes (map transcribe-process-form pf-stxs)]
               [rev (reverse codes)]
               [last-code (car rev)]
               [first-codes (reverse (cdr rev))])
          (if (null? first-codes)
              last-code
              (let ([forkers (map make-forker-stx first-codes)])
                (make-begin-syntax (append forkers (list last-code)))))))

      ;; ---- transcribe-simple-pipeline -----------------------------------------
      ;; Translate (pipe pf1 pf2 ... pfn) into a begin-block that forks
      ;; all-but-last stages and runs the last stage in the current process.
      ;;
      ;; Input:   pf-stxs -- list of process form syntax objects (must be non-empty)
      ;;
      ;; Output:  Syntax for a begin-block. For example, (pipe (ls) (sort) (head)):
      ;;   (begin
      ;;     (fork/pipe (lambda () (apply exec-path '(ls))))
      ;;     (fork/pipe (lambda () (apply exec-path '(sort))))
      ;;     (apply exec-path '(head)))
      ;;
      ;; The last stage runs in the current process so that exec replaces it.
      ;; Each fork/pipe call creates a pipe, forks, and connects stdout of the
      ;; child to stdin of the next stage.
      ;;
      ;; Corresponds to: transcribe-simple-pipeline in scsh's syntax-helpers.scm
      ;; -----------------------------------------------------------------
      (define (transcribe-simple-pipeline pf-stxs)
        (when (null? pf-stxs)
          (syntax-violation 'exec-epf "Empty pipeline" stx))
        (build-pipeline pf-stxs
          (lambda (code)
            (with-syntax ([body code])
              #'(fork/pipe (lambda () body))))))

      ;; ---- transcribe-complex-pipeline ----------------------------------------
      ;; Translate (pipe+ conns pf1 pf2 ... pfn) into a begin-block using
      ;; fork/pipe+ with explicit file descriptor connection specifications.
      ;;
      ;; Input:   conns-stx -- syntax for the connection list, e.g. ((1 2 0) (3 0))
      ;;                       Each element is (from-fd ... to-fd): the last number
      ;;                       is the fd in the next stage, preceding numbers are
      ;;                       fds in the forked stage that connect to it.
      ;;          pf-stxs   -- list of process form syntax objects (must be non-empty)
      ;;
      ;; Output:  Same structure as simple pipeline but using fork/pipe+ with the
      ;;          connection list. The conns are implicitly backquoted:
      ;;   (pipe+ ((1 2 0)) (writer) (reader)) =>
      ;;   (begin
      ;;     (fork/pipe+ `((1 2 0)) (lambda () (apply exec-path '(writer))))
      ;;     (apply exec-path '(reader)))
      ;;
      ;; Corresponds to: transcribe-complex-pipeline in scsh's syntax-helpers.scm
      ;; -----------------------------------------------------------------
      (define (transcribe-complex-pipeline conns-stx pf-stxs)
        (when (null? pf-stxs)
          (syntax-violation 'exec-epf "Empty pipeline" stx))
        (build-pipeline pf-stxs
          (lambda (code)
            (with-syntax ([body code]
                          [conns conns-stx])
              #'(fork/pipe+ (quasiquote conns) (lambda () body))))))

      ;; ---- transcribe-extended-process-form -----------------------------------
      ;; Top-level entry point: translate an EPF (process form + redirections)
      ;; into a begin-block where redirections execute first, then the process.
      ;;
      ;; Input:   forms -- list of syntax objects: first is the process form,
      ;;                   rest are redirection forms
      ;;
      ;; Output:  (begin redir1-code ... redirN-code pf-code)
      ;;          or just pf-code if there are no redirections.
      ;;
      ;; This is the function called from the main dispatch at the bottom of
      ;; exec-epf. It delegates to transcribe-redirection for each redir and
      ;; transcribe-process-form for the process form.
      ;;
      ;; Corresponds to: transcribe-extended-process-form in syntax-helpers.scm
      ;; -----------------------------------------------------------------
      (define (transcribe-extended-process-form forms)
        (let* ([pf-stx (car forms)]
               [redir-stxs (cdr forms)]
               [redir-codes (map transcribe-redirection redir-stxs)]
               [pf-code (transcribe-process-form pf-stx)])
          (if (null? redir-codes)
              pf-code
              (with-syntax ([(r ...) redir-codes]
                            [pf pf-code])
                #'(begin r ... pf)))))

      ;; -- Main dispatch
      (syntax-case stx ()
        [(_ pf redir ...)
         (transcribe-extended-process-form (syntax->list #'(pf redir ...)))])))

  ;; ======================================================================
  ;; Wrapper macros
  ;; ======================================================================

  ;; (& . epf) => (fork (lambda () (exec-epf . epf)))
  (define-syntax &
    (syntax-rules ()
      [(_ . epf) (fork (lambda () (exec-epf . epf)))]))

  ;; Helper: check if a datum is a special EPF head keyword.
  ;; meta define makes it available at both run time and expand time.
  (meta define (epf-special-head? d)
    (memq d `(begin pipe epf pipe+ ,(string->symbol "|") ,(string->symbol "|+"))))

  ;; Helper: check if a datum is a pipe head keyword.
  (meta define (pipe-head? d)
    (memq d `(pipe ,(string->symbol "|"))))

  ;; Helper: check if all pipeline stages are simple programs.
  (meta define (all-simple-stages? stages-stx)
    (let ([stages (syntax->list stages-stx)])
      (and stages
           (pair? stages)
           (for-all (lambda (s)
                      (let ([d (syntax->datum s)])
                        (and (pair? d)
                             (symbol? (car d))
                             (not (epf-special-head? (car d))))))
                    stages))))

  ;; (run . epf) — uses posix_spawn for simple program forms and pipelines,
  ;; falls back to fork+exec otherwise.
  (define-syntax run
    (lambda (stx)
      (syntax-case stx ()
        [(_ (prog arg ...))
         (and (identifier? #'prog)
              (not (epf-special-head? (syntax->datum #'prog))))
         ;; Simple program — posix_spawn fast path
         (with-syntax ([pf #'(prog arg ...)])
           #'(let ([cmd (quasiquote pf)])
               (wait (spawn-program (car cmd) (cdr cmd)))))]
        [(_ (head stage ...))
         (and (identifier? #'head)
              (pipe-head? (syntax->datum #'head))
              (all-simple-stages? #'(stage ...)))
         ;; Simple pipeline — posix_spawn all stages
         (with-syntax ([(s ...) #'(stage ...)])
           #'(let ([procs (spawn-pipeline (list (quasiquote s) ...))])
               (wait (car (reverse procs)))))]
        [(_ . epf)
         #'(wait (& . epf))])))

  ;; (|| pf ...) — short-circuit OR: run until one succeeds (exit 0)
  (define-syntax ||
    (syntax-rules ()
      [(_ pf ...) (or (zero? (run pf)) ...)]))

  ;; (:or: pf ...) — alias for || (safe for readers that choke on ||)
  (define-syntax :or:
    (syntax-rules ()
      [(_ pf ...) (or (zero? (run pf)) ...)]))

  ;; (&& pf ...) — short-circuit AND: run until one fails (exit non-0)
  (define-syntax &&
    (syntax-rules ()
      [(_ pf ...) (and (zero? (run pf)) ...)]))

  ;; ======================================================================
  ;; Collector macros
  ;; ======================================================================

  ;; (run/port . epf) — posix_spawn fast path for simple programs and pipelines
  (define-syntax run/port
    (lambda (stx)
      (syntax-case stx ()
        [(_ (prog arg ...))
         (and (identifier? #'prog)
              (not (epf-special-head? (syntax->datum #'prog))))
         (with-syntax ([pf #'(prog arg ...)])
           #'(let ([cmd (quasiquote pf)])
               (let-values ([(port _proc) (run-spawn/port (car cmd) (cdr cmd))])
                 port)))]
        [(_ (head stage ...))
         (and (identifier? #'head)
              (pipe-head? (syntax->datum #'head))
              (all-simple-stages? #'(stage ...)))
         (with-syntax ([(s ...) #'(stage ...)])
           #'(let-values ([(port _procs) (spawn-pipeline/port (list (quasiquote s) ...))])
               port))]
        [(_ . epf)
         #'(run/port* (lambda () (exec-epf . epf)))])))

  ;; (run/string . epf) — posix_spawn fast path for simple programs and pipelines
  (define-syntax run/string
    (lambda (stx)
      (syntax-case stx ()
        [(_ (prog arg ...))
         (and (identifier? #'prog)
              (not (epf-special-head? (syntax->datum #'prog))))
         #'(close-after (run/port (prog arg ...)) port->string)]
        [(_ (head stage ...))
         (and (identifier? #'head)
              (pipe-head? (syntax->datum #'head))
              (all-simple-stages? #'(stage ...)))
         #'(close-after (run/port (head stage ...)) port->string)]
        [(_ . epf)
         #'(run/string* (lambda () (exec-epf . epf)))])))

  ;; (run/strings . epf) — posix_spawn fast path for simple programs and pipelines
  (define-syntax run/strings
    (lambda (stx)
      (syntax-case stx ()
        [(_ (prog arg ...))
         (and (identifier? #'prog)
              (not (epf-special-head? (syntax->datum #'prog))))
         #'(close-after (run/port (prog arg ...)) port->string-list)]
        [(_ (head stage ...))
         (and (identifier? #'head)
              (pipe-head? (syntax->datum #'head))
              (all-simple-stages? #'(stage ...)))
         #'(close-after (run/port (head stage ...)) port->string-list)]
        [(_ . epf)
         #'(run/strings* (lambda () (exec-epf . epf)))])))

  ;; (run/sexp . epf) — posix_spawn fast path for simple programs and pipelines
  (define-syntax run/sexp
    (lambda (stx)
      (syntax-case stx ()
        [(_ (prog arg ...))
         (and (identifier? #'prog)
              (not (epf-special-head? (syntax->datum #'prog))))
         #'(close-after (run/port (prog arg ...)) read)]
        [(_ (head stage ...))
         (and (identifier? #'head)
              (pipe-head? (syntax->datum #'head))
              (all-simple-stages? #'(stage ...)))
         #'(close-after (run/port (head stage ...)) read)]
        [(_ . epf)
         #'(run/sexp* (lambda () (exec-epf . epf)))])))

  ;; (run/sexps . epf) — posix_spawn fast path for simple programs and pipelines
  (define-syntax run/sexps
    (lambda (stx)
      (syntax-case stx ()
        [(_ (prog arg ...))
         (and (identifier? #'prog)
              (not (epf-special-head? (syntax->datum #'prog))))
         #'(close-after (run/port (prog arg ...)) port->sexp-list)]
        [(_ (head stage ...))
         (and (identifier? #'head)
              (pipe-head? (syntax->datum #'head))
              (all-simple-stages? #'(stage ...)))
         #'(close-after (run/port (head stage ...)) port->sexp-list)]
        [(_ . epf)
         #'(run/sexps* (lambda () (exec-epf . epf)))])))

  ;; (run/port+proc . epf) — posix_spawn fast path for simple programs and pipelines
  (define-syntax run/port+proc
    (lambda (stx)
      (syntax-case stx ()
        [(_ (prog arg ...))
         (and (identifier? #'prog)
              (not (epf-special-head? (syntax->datum #'prog))))
         (with-syntax ([pf #'(prog arg ...)])
           #'(let ([cmd (quasiquote pf)])
               (run-spawn/port (car cmd) (cdr cmd))))]
        [(_ (head stage ...))
         (and (identifier? #'head)
              (pipe-head? (syntax->datum #'head))
              (all-simple-stages? #'(stage ...)))
         (with-syntax ([(s ...) #'(stage ...)])
           #'(let-values ([(port procs) (spawn-pipeline/port (list (quasiquote s) ...))])
               (values port (car (reverse procs)))))]
        [(_ . epf)
         #'(run/port+proc* (lambda () (exec-epf . epf)))])))

  ;; (run/file . epf) — posix_spawn fast path for simple programs
  (define-syntax run/file
    (lambda (stx)
      (syntax-case stx ()
        [(_ (prog arg ...))
         (and (identifier? #'prog)
              (not (epf-special-head? (syntax->datum #'prog))))
         (with-syntax ([pf #'(prog arg ...)])
           #'(let ([cmd (quasiquote pf)])
               (run-spawn/file (car cmd) (cdr cmd))))]
        [(_ . epf)
         #'(run/file* (lambda () (exec-epf . epf)))])))

  ;; (run/collecting (fd ...) . epf) => (run/collecting* '(fd ...) (lambda () (exec-epf . epf)))
  ;; The fd list is implicitly quoted (backquoted in scsh convention).
  (define-syntax run/collecting
    (syntax-rules ()
      [(_ (fd ...) . epf)
       (run/collecting* '(fd ...) (lambda () (exec-epf . epf)))]))

) ;; end library
