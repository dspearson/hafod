;;; (hafod port-collect) -- Port collector utilities
;;; port->string, port->string-list, port->sexp-list, port->list, port-fold, reduce-port
;;; Ported from scsh process-high-level.scm port collector functions.

(library (hafod port-collect)
  (export port->string port->string-list port->sexp-list port->list
          port-fold reduce-port
          make-char-port-filter make-string-port-filter
          ;; scsh-compatible string port aliases
          make-string-input-port make-string-output-port
          string-output-port-output call-with-string-output-port)

  (import (hafod internal base)
          (hafod rdelim) (hafod compat))

  ;; port->string: read all characters from port until EOF, return as string.
  ;; Uses Chez Scheme's get-string-all for efficiency.
  (define (port->string port)
    (let ([s (get-string-all port)])
      (if (eof-object? s) "" s)))

  ;; port->string-list: read lines until EOF, return as list of strings.
  ;; Uses (hafod rdelim) read-line which strips the newline delimiter.
  (define (port->string-list port)
    (let loop ([lines '()])
      (let ([line (read-line port)])
        (if (eof-object? line)
            (reverse lines)
            (loop (cons line lines))))))

  ;; port->sexp-list: read S-expressions until EOF, return as list.
  (define (port->sexp-list port)
    (let loop ([sexps '()])
      (let ([s (read port)])
        (if (eof-object? s)
            (reverse sexps)
            (loop (cons s sexps))))))

  ;; port->list: general reader->list. Repeatedly apply reader to port
  ;; until EOF. Return list of items read.
  (define (port->list reader port)
    (let loop ([items '()])
      (let ([v (reader port)])
        (if (eof-object? v)
            (reverse items)
            (loop (cons v items))))))

  ;; port-fold: fold over port values.
  ;; Repeatedly read from PORT with READER. Each time a value V is read,
  ;; compute new seeds with (apply OP V SEEDS). On EOF, return (apply values SEEDS).
  (define (port-fold port reader op . seeds)
    (let loop ([seeds seeds])
      (let ([v (reader port)])
        (if (eof-object? v)
            (apply values seeds)
            (receive new-seeds (apply op v seeds)
              (loop new-seeds))))))

  ;; reduce-port: alias for port-fold (scsh compatibility)
  (define reduce-port port-fold)

  ;; ======================================================================
  ;; Port filters
  ;; ======================================================================

  ;; make-char-port-filter: returns a procedure that reads chars from
  ;; current-input-port, applies proc to each char (proc should write
  ;; to current-output-port), until EOF.
  (define (make-char-port-filter proc)
    (lambda ()
      (let loop ()
        (let ([c (read-char)])
          (unless (eof-object? c)
            (proc c)
            (loop))))))

  ;; make-string-port-filter: returns a procedure that reads lines from
  ;; current-input-port, applies proc to each line. If proc returns
  ;; a non-#f value, writes it to current-output-port followed by newline.
  (define (make-string-port-filter proc)
    (lambda ()
      (let loop ()
        (let ([line (read-line (current-input-port))])
          (unless (eof-object? line)
            (let ([result (proc line)])
              (when result
                (display result)
                (newline)))
            (loop))))))

  ;; ======================================================================
  ;; String port aliases (scsh-compatible names)
  ;; ======================================================================

  (define make-string-input-port open-input-string)

  ;; Chez's open-string-output-port returns (values port extractor).
  ;; scsh's make-string-output-port returns just a port, with a separate
  ;; string-output-port-output to extract. Bridge with an eq? hashtable.
  (define *string-port-extractors* (make-eq-hashtable))

  (define (make-string-output-port)
    (let-values ([(port extractor) (open-string-output-port)])
      (hashtable-set! *string-port-extractors* port extractor)
      port))

  (define (string-output-port-output port)
    (let ([extractor (hashtable-ref *string-port-extractors* port #f)])
      (if extractor
          (extractor)
          (error 'string-output-port-output
                 "not a string output port created by make-string-output-port"
                 port))))

) ;; end library
