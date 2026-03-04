;;; (hafod glob) -- Glob pattern expansion
;;; Provides glob and glob-quote.
;;; Ported from scsh/scheme/glob.scm
;;; Uses fnmatch(3) for pattern matching instead of scsh's regex engine.

(library (hafod glob)
  (export glob glob-quote maybe-directory-files)

  (import (hafod internal base)
          (hafod posix) (hafod fname) (hafod compat))

  ;; ======================================================================
  ;; Glob-quote: escape glob metacharacters
  ;; ======================================================================

  (define (glob-quote string)
    (let lp ([i (- (string-length string) 1)]
             [result '()])
      (if (< i 0)
          (list->string result)
          (lp (- i 1)
              (let* ([c (string-ref string i)]
                     [result (cons c result)])
                (if (memv c '(#\[ #\] #\* #\? #\{ #\} #\\))
                    (cons #\\ result)
                    result))))))

  ;; ======================================================================
  ;; Brace expansion
  ;; ======================================================================

  ;; Append a suffix to all strings in a list.
  (define (append-suffix strs suffix)
    (map (lambda (s) (string-append s suffix)) strs))

  ;; Cross-product of prefixes and suffixes.
  (define (cross-append prefixes suffixes)
    (apply append (map (lambda (sfx) (append-suffix prefixes sfx)) suffixes)))

  ;; Parse a glob pattern into brace-free patterns.
  ;; Returns (values patterns end-index).
  (define (parse-glob-braces pattern start comma-terminates?)
    (let ([pattern-len (string-length pattern)])
      (let ([finish (lambda (prefixes pat)
                      (append-suffix prefixes (list->string (reverse pat))))])
        (let lp ([i start]
                 [prefixes '("")]
                 [pat '()])
          (if (= i pattern-len)
              (values (finish prefixes pat) i)
              (let ([c (string-ref pattern i)])
                (case c
                  [(#\{)
                   (let ([prefixes (append-suffix prefixes
                                                   (list->string (reverse pat)))])
                     (receive (pats i) (parse-comma-sequence pattern (+ i 1))
                       (lp i (cross-append prefixes pats) '())))]

                  [(#\\)
                   (let ([next-i (+ i 1)])
                     (if (= next-i pattern-len)
                         (error 'glob "Dangling escape char in glob pattern" pattern)
                         (if (memv (string-ref pattern next-i) '(#\{ #\, #\} #\\))
                             (lp (+ next-i 1) prefixes
                                 (cons (string-ref pattern next-i) pat))
                             (lp (+ i 1) prefixes
                                 (cons (string-ref pattern i) pat)))))]

                  [(#\,)
                   (if comma-terminates?
                       (values (finish prefixes pat) i)
                       (lp (+ i 1) prefixes (cons c pat)))]

                  [(#\})
                   (values (finish prefixes pat) i)]

                  [else
                   (lp (+ i 1) prefixes (cons c pat))])))))))

  ;; Parse the inside of a {...} brace list.
  (define (parse-comma-sequence pattern start)
    (let ([pattern-len (string-length pattern)])
      (let lp ([i start]
               [patterns '()])
        (if (= i pattern-len)
            (error 'glob "Unterminated brace in glob pattern" pattern)
            (receive (pats i) (parse-glob-braces pattern i #t)
              (let ([patterns (append patterns pats)])
                (if (= i pattern-len)
                    (error 'glob "Unterminated brace in glob pattern" pattern)
                    (let ([c (string-ref pattern i)])
                      (case c
                        [(#\}) (values patterns (+ i 1))]
                        [(#\,) (lp (+ i 1) patterns)]
                        [else (error 'glob "Internal error in glob brace parser" pattern i)])))))))))

  ;; Remove braces from a pattern, expanding into multiple patterns.
  (define (glob-remove-braces pattern)
    (receive (pats i) (parse-glob-braces pattern 0 #f)
      (if (= i (string-length pattern))
          pats
          (error 'glob "Unmatched close brace in glob pattern" pattern i))))

  ;; ======================================================================
  ;; Pattern matching helpers
  ;; ======================================================================

  ;; Is a pattern constant (no wildcards)?
  (define (constant-glob? pattern)
    (let ([patlen (string-length pattern)])
      (let lp ([i 0])
        (or (= i patlen)
            (let ([next-i (+ i 1)])
              (case (string-ref pattern i)
                [(#\\) (if (= next-i patlen)
                           (error 'glob "Ill-formed glob pattern" pattern)
                           (lp (+ next-i 1)))]
                [(#\* #\? #\[) #f]
                [else (lp next-i)]))))))

  ;; Remove backslash escapes from a constant glob pattern.
  (define (glob-unquote string)
    (let ([len (string-length string)])
      (let lp ([i 0] [result '()])
        (if (= i len)
            (list->string (reverse result))
            (let ([c (string-ref string i)])
              (if (char=? c #\\)
                  (let ([next-i (+ i 1)])
                    (if (= next-i len)
                        (error 'glob "Dangling escape in glob pattern" string)
                        (lp (+ i 2) (cons (string-ref string next-i) result))))
                  (lp (+ i 1) (cons c result))))))))

  ;; List directory entries, optionally including dot-files.
  ;; Silently returns '() on errors (permission denied, not a directory, etc).
  (define (maybe-directory-files path dotfiles?)
    (guard (e [#t '()])
      (let* ([dir (if (string=? path "") "." path)]
             [dirp (posix-opendir dir)])
        (let loop ([acc '()])
          (let ([name (posix-readdir dirp)])
            (if name
                (loop (if (or (string=? name ".") (string=? name ".."))
                          acc
                          (cons name acc)))
                (begin
                  (posix-closedir dirp)
                  (if dotfiles?
                      acc
                      (filter (lambda (f) (not (char=? (string-ref f 0) #\.)))
                              acc)))))))))

  ;; Check if path is a directory (silently returns #f on error).
  (define (maybe-isdir? path)
    (guard (e [#t #f])
      (eq? 'directory (stat-info-type (posix-stat path)))))

  ;; ======================================================================
  ;; Core glob matching
  ;; ======================================================================

  ;; Match a sub-pattern against directory entries.
  ;; Returns (values winners sure?) where sure? means entries exist for certain.
  (define (glob-subpat dir pat)
    (cond
      [(string=? pat "")
       (values '() #t)]

      [(constant-glob? pat)
       ;; Constant pattern -- don't scan directory, just return the pattern.
       ;; The caller will verify existence later.
       (values (list (glob-unquote pat)) #f)]

      [else
       (let* ([dots? (char=? #\. (string-ref pat 0))]
              [candidates (maybe-directory-files dir dots?)]
              ;; Use fnmatch with FNM_PERIOD to respect dot-file convention
              [flags (if dots? 0 FNM_PERIOD)])
         (values (filter (lambda (f) (zero? (posix-fnmatch pat f flags)))
                         candidates)
                 #t))]))

  ;; Recursively match patterns against directory tree.
  (define (really-glob root-file patterns directories-only?)
    (let recur ([file root-file]
                [pats patterns]
                [sure? #f])
      (if (pair? pats)
          (let ([pat (car pats)]
                [pats (cdr pats)]
                [dir (file-name-as-directory file)])
            (receive (winners sure?) (glob-subpat dir pat)
              (apply append
                     (map (lambda (f)
                            (recur (string-append dir f) pats sure?))
                          winners))))
          ;; All patterns consumed
          (if directories-only?
              (if (maybe-isdir? file)
                  (list (file-name-as-directory file))
                  '())
              (if (or sure? (zero? (posix-access file F_OK)))
                  (list file)
                  '())))))

  ;; Process a single brace-free glob pattern.
  (define (glob-one-pattern pattern)
    (let ([plen (string-length pattern)])
      (if (zero? plen)
          '()
          (let ([directories-only? (char=? #\/ (string-ref pattern (- plen 1)))]
                [patterns (split-file-name pattern)])
            (if (equal? "" (car patterns))
                (really-glob "" (cdr patterns) directories-only?)
                (really-glob "." patterns directories-only?))))))

  ;; ======================================================================
  ;; Public API
  ;; ======================================================================

  ;; Check if a pattern contains braces (needs Scheme-level expansion).
  (define (has-braces? pat)
    (let ([len (string-length pat)])
      (let loop ([i 0])
        (and (< i len)
             (let ([c (string-ref pat i)])
               (if (char=? c #\\)
                   (loop (+ i 2))  ;; skip escaped chars
                   (or (char=? c #\{) (loop (+ i 1)))))))))

  ;; (glob pattern ...) => list of matching file paths
  ;; Expands brace patterns, then matches against filesystem.
  ;; Never returns "." or "..". Only returns dot-files if pattern starts with ".".
  ;; Uses C glob(3) fast path for brace-free patterns.
  (define (glob . pattern-list)
    (apply append
           (map (lambda (pat)
                  (if (has-braces? pat)
                      ;; Brace expansion needed — use Scheme implementation
                      (apply append
                             (map glob-one-pattern (glob-remove-braces pat)))
                      ;; No braces — use C glob(3) directly, filter . and ..
                      (filter (lambda (p)
                                (let ([base (file-name-nondirectory p)])
                                  (not (or (string=? base ".")
                                           (string=? base "..")))))
                              (posix-glob-fast pat))))
                pattern-list)))

) ;; end library
