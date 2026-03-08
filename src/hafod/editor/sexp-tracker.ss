;;; (hafod editor sexp-tracker) -- Paren depth tracking with lexer awareness
;;; A mini-lexer that correctly handles strings, line comments, block comments,
;;; and character literals when computing parenthesis/bracket nesting depth.
;;; Also provides structural navigation (forward/backward sexp) and
;;; enclosing paren detection for paredit-style editing.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor sexp-tracker)
  (export sexp-depth find-matching-paren find-matching-close
          find-enclosing-parens lexer-state-at scan-lexer
          forward-sexp-end backward-sexp-start
          forward-down-list)
  (import (chezscheme))

  ;; ======================================================================
  ;; Lexer scanner — shared core for all sexp-aware operations
  ;; States: normal, in-string, in-string-escape, in-line-comment,
  ;;         in-block-comment, after-hash, after-hash-backslash
  ;; ======================================================================

  ;; Scan from 0 to target, returning (values state paren-stack bc-depth).
  ;; paren-stack is a list of open-paren indices (most recent first).
  (define (scan-lexer str target)
    (let ([len (fxmin target (string-length str))])
      (let loop ([i 0] [state 'normal] [bc-depth 0] [stack '()])
        (if (fx>= i len)
            (values state stack bc-depth)
            (let ([ch (string-ref str i)])
              (case state
                [(normal)
                 (cond
                   [(or (char=? ch #\() (char=? ch #\[))
                    (loop (fx+ i 1) 'normal 0 (cons i stack))]
                   [(or (char=? ch #\)) (char=? ch #\]))
                    (loop (fx+ i 1) 'normal 0 (if (pair? stack) (cdr stack) stack))]
                   [(char=? ch #\") (loop (fx+ i 1) 'in-string 0 stack)]
                   [(char=? ch #\;) (loop (fx+ i 1) 'in-line-comment 0 stack)]
                   [(char=? ch #\#) (loop (fx+ i 1) 'after-hash 0 stack)]
                   [else (loop (fx+ i 1) 'normal 0 stack)])]
                [(in-string)
                 (case ch
                   [(#\\) (loop (fx+ i 1) 'in-string-escape 0 stack)]
                   [(#\") (loop (fx+ i 1) 'normal 0 stack)]
                   [else (loop (fx+ i 1) 'in-string 0 stack)])]
                [(in-string-escape)
                 (loop (fx+ i 1) 'in-string 0 stack)]
                [(in-line-comment)
                 (if (char=? ch #\newline)
                     (loop (fx+ i 1) 'normal 0 stack)
                     (loop (fx+ i 1) 'in-line-comment 0 stack))]
                [(after-hash)
                 (case ch
                   [(#\|) (loop (fx+ i 1) 'in-block-comment 1 stack)]
                   [(#\\) (loop (fx+ i 1) 'after-hash-backslash 0 stack)]
                   [else (loop (fx+ i 1) 'normal 0 stack)])]
                [(after-hash-backslash)
                 (loop (fx+ i 1) 'normal 0 stack)]
                [(in-block-comment)
                 (cond
                   [(and (char=? ch #\#)
                         (fx< (fx+ i 1) (string-length str))
                         (char=? (string-ref str (fx+ i 1)) #\|))
                    (loop (fx+ i 2) 'in-block-comment (fx+ bc-depth 1) stack)]
                   [(and (char=? ch #\|)
                         (fx< (fx+ i 1) (string-length str))
                         (char=? (string-ref str (fx+ i 1)) #\#))
                    (if (fx= bc-depth 1)
                        (loop (fx+ i 2) 'normal 0 stack)
                        (loop (fx+ i 2) 'in-block-comment (fx- bc-depth 1) stack))]
                   [else (loop (fx+ i 1) 'in-block-comment bc-depth stack)])]
                [else (loop (fx+ i 1) 'normal 0 stack)]))))))

  ;; Return the lexer state at position pos in str.
  (define (lexer-state-at str pos)
    (let-values ([(state stack bc-depth) (scan-lexer str pos)])
      state))

  ;; ======================================================================
  ;; sexp-depth (existing)
  ;; ======================================================================

  (define (sexp-depth str)
    (let ([len (string-length str)])
      (let loop ([i 0] [depth 0] [state 'normal] [bc-depth 0])
        (if (fx>= i len)
            depth
            (let ([ch (string-ref str i)])
              (case state
                [(normal)
                 (case ch
                   [(#\( #\[) (loop (fx+ i 1) (fx+ depth 1) 'normal 0)]
                   [(#\) #\]) (loop (fx+ i 1) (fx- depth 1) 'normal 0)]
                   [(#\") (loop (fx+ i 1) depth 'in-string 0)]
                   [(#\;) (loop (fx+ i 1) depth 'in-line-comment 0)]
                   [(#\#) (loop (fx+ i 1) depth 'after-hash 0)]
                   [else (loop (fx+ i 1) depth 'normal 0)])]
                [(in-string)
                 (case ch
                   [(#\\) (loop (fx+ i 1) depth 'in-string-escape 0)]
                   [(#\") (loop (fx+ i 1) depth 'normal 0)]
                   [else (loop (fx+ i 1) depth 'in-string 0)])]
                [(in-string-escape) (loop (fx+ i 1) depth 'in-string 0)]
                [(in-line-comment)
                 (if (char=? ch #\newline)
                     (loop (fx+ i 1) depth 'normal 0)
                     (loop (fx+ i 1) depth 'in-line-comment 0))]
                [(after-hash)
                 (case ch
                   [(#\|) (loop (fx+ i 1) depth 'in-block-comment 1)]
                   [(#\\) (loop (fx+ i 1) depth 'after-hash-backslash 0)]
                   [else (loop (fx+ i 1) depth 'normal 0)])]
                [(after-hash-backslash) (loop (fx+ i 1) depth 'normal 0)]
                [(in-block-comment)
                 (cond
                   [(and (char=? ch #\#)
                         (fx< (fx+ i 1) len)
                         (char=? (string-ref str (fx+ i 1)) #\|))
                    (loop (fx+ i 2) depth 'in-block-comment (fx+ bc-depth 1))]
                   [(and (char=? ch #\|)
                         (fx< (fx+ i 1) len)
                         (char=? (string-ref str (fx+ i 1)) #\#))
                    (if (fx= bc-depth 1)
                        (loop (fx+ i 2) depth 'normal 0)
                        (loop (fx+ i 2) depth 'in-block-comment (fx- bc-depth 1)))]
                   [else (loop (fx+ i 1) depth 'in-block-comment bc-depth)])]
                [else (loop (fx+ i 1) depth 'normal 0)]))))))

  ;; ======================================================================
  ;; find-matching-paren (existing) — find opener for a closer
  ;; ======================================================================

  (define (find-matching-paren str close-idx)
    (let* ([len (string-length str)]
           [close-ch (string-ref str close-idx)]
           [open-ch (case close-ch
                      [(#\)) #\(]
                      [(#\]) #\[]
                      [else #f])])
      (if (not open-ch)
          #f
          (let loop ([i 0] [state 'normal] [bc-depth 0] [stack '()])
            (cond
              [(fx> i close-idx) #f]
              [(fx= i close-idx)
               (if (and (eq? state 'normal) (pair? stack))
                   (car stack)
                   #f)]
              [else
               (let ([ch (string-ref str i)])
                 (case state
                   [(normal)
                    (cond
                      [(or (char=? ch #\() (char=? ch #\[))
                       (loop (fx+ i 1) 'normal 0 (cons i stack))]
                      [(or (char=? ch #\)) (char=? ch #\]))
                       (loop (fx+ i 1) 'normal 0
                             (if (pair? stack) (cdr stack) stack))]
                      [(char=? ch #\")
                       (loop (fx+ i 1) 'in-string 0 stack)]
                      [(char=? ch #\;)
                       (loop (fx+ i 1) 'in-line-comment 0 stack)]
                      [(char=? ch #\#)
                       (loop (fx+ i 1) 'after-hash 0 stack)]
                      [else
                       (loop (fx+ i 1) 'normal 0 stack)])]
                   [(in-string)
                    (case ch
                      [(#\\) (loop (fx+ i 1) 'in-string-escape 0 stack)]
                      [(#\") (loop (fx+ i 1) 'normal 0 stack)]
                      [else (loop (fx+ i 1) 'in-string 0 stack)])]
                   [(in-string-escape)
                    (loop (fx+ i 1) 'in-string 0 stack)]
                   [(in-line-comment)
                    (if (char=? ch #\newline)
                        (loop (fx+ i 1) 'normal 0 stack)
                        (loop (fx+ i 1) 'in-line-comment 0 stack))]
                   [(after-hash)
                    (case ch
                      [(#\|) (loop (fx+ i 1) 'in-block-comment 1 stack)]
                      [(#\\) (loop (fx+ i 1) 'after-hash-backslash 0 stack)]
                      [else (loop (fx+ i 1) 'normal 0 stack)])]
                   [(after-hash-backslash)
                    (loop (fx+ i 1) 'normal 0 stack)]
                   [(in-block-comment)
                    (cond
                      [(and (char=? ch #\#)
                            (fx< (fx+ i 1) len)
                            (char=? (string-ref str (fx+ i 1)) #\|))
                       (loop (fx+ i 2) 'in-block-comment (fx+ bc-depth 1) stack)]
                      [(and (char=? ch #\|)
                            (fx< (fx+ i 1) len)
                            (char=? (string-ref str (fx+ i 1)) #\#))
                       (if (fx= bc-depth 1)
                           (loop (fx+ i 2) 'normal 0 stack)
                           (loop (fx+ i 2) 'in-block-comment (fx- bc-depth 1) stack))]
                      [else (loop (fx+ i 1) 'in-block-comment bc-depth stack)])]
                   [else (loop (fx+ i 1) 'normal 0 stack)]))])))))

  ;; ======================================================================
  ;; find-matching-close — find closer for an opener
  ;; ======================================================================

  (define (find-matching-close str open-idx)
    (let ([len (string-length str)])
      (let loop ([i (fx+ open-idx 1)] [depth 1] [state 'normal] [bc-depth 0])
        (cond
          [(fx>= i len) #f]
          [else
           (let ([ch (string-ref str i)])
             (case state
               [(normal)
                (cond
                  [(or (char=? ch #\() (char=? ch #\[))
                   (loop (fx+ i 1) (fx+ depth 1) 'normal 0)]
                  [(or (char=? ch #\)) (char=? ch #\]))
                   (if (fx= depth 1) i
                       (loop (fx+ i 1) (fx- depth 1) 'normal 0))]
                  [(char=? ch #\") (loop (fx+ i 1) depth 'in-string 0)]
                  [(char=? ch #\;) (loop (fx+ i 1) depth 'in-line-comment 0)]
                  [(char=? ch #\#) (loop (fx+ i 1) depth 'after-hash 0)]
                  [else (loop (fx+ i 1) depth 'normal 0)])]
               [(in-string)
                (case ch
                  [(#\\) (loop (fx+ i 1) depth 'in-string-escape 0)]
                  [(#\") (loop (fx+ i 1) depth 'normal 0)]
                  [else (loop (fx+ i 1) depth 'in-string 0)])]
               [(in-string-escape) (loop (fx+ i 1) depth 'in-string 0)]
               [(in-line-comment)
                (if (char=? ch #\newline)
                    (loop (fx+ i 1) depth 'normal 0)
                    (loop (fx+ i 1) depth 'in-line-comment 0))]
               [(after-hash)
                (case ch
                  [(#\|) (loop (fx+ i 1) depth 'in-block-comment 1)]
                  [(#\\) (loop (fx+ i 1) depth 'after-hash-backslash 0)]
                  [else (loop (fx+ i 1) depth 'normal 0)])]
               [(after-hash-backslash) (loop (fx+ i 1) depth 'normal 0)]
               [(in-block-comment)
                (cond
                  [(and (char=? ch #\#)
                        (fx< (fx+ i 1) len)
                        (char=? (string-ref str (fx+ i 1)) #\|))
                   (loop (fx+ i 2) depth 'in-block-comment (fx+ bc-depth 1))]
                  [(and (char=? ch #\|)
                        (fx< (fx+ i 1) len)
                        (char=? (string-ref str (fx+ i 1)) #\#))
                   (if (fx= bc-depth 1)
                       (loop (fx+ i 2) depth 'normal 0)
                       (loop (fx+ i 2) depth 'in-block-comment (fx- bc-depth 1)))]
                  [else (loop (fx+ i 1) depth 'in-block-comment bc-depth)])]
               [else (loop (fx+ i 1) depth 'normal 0)]))]))))

  ;; ======================================================================
  ;; find-enclosing-parens — innermost ( ) or [ ] around cursor position
  ;; Returns (values open-idx close-idx) or (values #f #f).
  ;; ======================================================================

  (define (find-enclosing-parens str pos)
    (let-values ([(state stack bc-depth) (scan-lexer str pos)])
      (if (and (eq? state 'normal) (pair? stack))
          (let* ([open-idx (car stack)]
                 [close-idx (find-matching-close str open-idx)])
            (values open-idx (or close-idx #f)))
          (values #f #f))))

  ;; ======================================================================
  ;; Sexp navigation
  ;; ======================================================================

  (define (sexp-delimiter? ch)
    (or (char-whitespace? ch)
        (memv ch '(#\( #\) #\[ #\] #\" #\;))))

  ;; forward-sexp-end: from pos, skip whitespace, return position AFTER next sexp.
  ;; Returns #f if no sexp found.
  (define (forward-sexp-end str pos)
    (let* ([len (string-length str)]
           [start (let skip ([i pos])
                    (if (and (fx< i len) (char-whitespace? (string-ref str i)))
                        (skip (fx+ i 1)) i))])
      (cond
        [(fx>= start len) #f]
        [else
         (let ([ch (string-ref str start)]
               [state (lexer-state-at str start)])
           (cond
             ;; Inside string/comment — don't navigate structurally
             [(not (eq? state 'normal)) #f]
             ;; Open paren/bracket: find matching close
             [(or (char=? ch #\() (char=? ch #\[))
              (let ([close (find-matching-close str start)])
                (and close (fx+ close 1)))]
             ;; String literal: skip to matching close quote
             [(char=? ch #\")
              (let skip-str ([i (fx+ start 1)])
                (cond
                  [(fx>= i len) #f]
                  [(char=? (string-ref str i) #\\) (skip-str (fx+ i 2))]
                  [(char=? (string-ref str i) #\") (fx+ i 1)]
                  [else (skip-str (fx+ i 1))]))]
             ;; Hash prefix: #t, #f, #\x, #(...), etc.
             [(char=? ch #\#)
              (cond
                ;; #( or #vu8( — vector literal
                [(and (fx< (fx+ start 1) len)
                      (char=? (string-ref str (fx+ start 1)) #\())
                 (let ([close (find-matching-close str (fx+ start 1))])
                   (and close (fx+ close 1)))]
                ;; #\ — character literal
                [(and (fx< (fx+ start 1) len)
                      (char=? (string-ref str (fx+ start 1)) #\\))
                 (let skip-atom ([i (fx+ start 2)])
                   (cond
                     [(fx>= i len) i]
                     [(sexp-delimiter? (string-ref str i)) i]
                     [else (skip-atom (fx+ i 1))]))]
                ;; Other # atoms (#t, #f, etc.)
                [else
                 (let skip-atom ([i start])
                   (cond
                     [(fx>= i len) i]
                     [(sexp-delimiter? (string-ref str i)) i]
                     [else (skip-atom (fx+ i 1))]))])]
             ;; Close paren — can't go forward over this
             [(or (char=? ch #\)) (char=? ch #\])) #f]
             ;; Atom (symbol, number, etc.): skip to next delimiter
             [else
              (let skip-atom ([i start])
                (cond
                  [(fx>= i len) i]
                  [(sexp-delimiter? (string-ref str i)) i]
                  [else (skip-atom (fx+ i 1))]))]))])))

  ;; backward-sexp-start: from pos, skip whitespace backward, return position
  ;; of previous sexp start. Returns #f if no sexp found.
  (define (backward-sexp-start str pos)
    (let ([start (let skip ([i (fx- pos 1)])
                   (if (and (fx>= i 0) (char-whitespace? (string-ref str i)))
                       (skip (fx- i 1)) i))])
      (cond
        [(fx< start 0) #f]
        [else
         (let ([ch (string-ref str start)])
           (cond
             ;; Close paren/bracket: find matching opener
             [(or (char=? ch #\)) (char=? ch #\]))
              (find-matching-paren str start)]
             ;; Close quote: find matching open quote via forward scan
             [(char=? ch #\")
              (let-values ([(state stack bc-depth) (scan-lexer str start)])
                (if (eq? state 'in-string)
                    ;; This " closes a string — find where it opened
                    (let scan ([j 0] [st 'normal] [open-pos #f])
                      (cond
                        [(fx>= j start) open-pos]
                        [else
                         (let ([c (string-ref str j)])
                           (case st
                             [(normal)
                              (cond
                                [(char=? c #\") (scan (fx+ j 1) 'in-string j)]
                                [(char=? c #\;) (scan (fx+ j 1) 'in-line-comment open-pos)]
                                [(char=? c #\#) (scan (fx+ j 1) 'after-hash open-pos)]
                                [else (scan (fx+ j 1) 'normal open-pos)])]
                             [(in-string)
                              (cond
                                [(char=? c #\\) (scan (fx+ j 1) 'in-string-escape open-pos)]
                                [(char=? c #\") (scan (fx+ j 1) 'normal #f)]
                                [else (scan (fx+ j 1) 'in-string open-pos)])]
                             [(in-string-escape) (scan (fx+ j 1) 'in-string open-pos)]
                             [(in-line-comment)
                              (if (char=? c #\newline)
                                  (scan (fx+ j 1) 'normal open-pos)
                                  (scan (fx+ j 1) 'in-line-comment open-pos))]
                             [(after-hash)
                              (if (char=? c #\\)
                                  (scan (fx+ j 1) 'after-hash-backslash open-pos)
                                  (scan (fx+ j 1) 'normal open-pos))]
                             [(after-hash-backslash) (scan (fx+ j 1) 'normal open-pos)]
                             [else (scan (fx+ j 1) st open-pos)]))]))
                    ;; Not in string — this " might be start of next string
                    start))]
             ;; Open paren — can't go backward over this
             [(or (char=? ch #\() (char=? ch #\[)) #f]
             ;; Atom: skip backward over non-delimiters
             [else
              (let skip-back ([i start])
                (cond
                  [(fx< i 0) 0]
                  [(sexp-delimiter? (string-ref str i)) (fx+ i 1)]
                  [else (skip-back (fx- i 1))]))]))])))

  ;; ======================================================================
  ;; forward-down-list — find next open paren/bracket in normal state
  ;; Returns position AFTER the opener (inside the list), or #f.
  ;; ======================================================================

  (define (forward-down-list str pos)
    (let ([len (string-length str)])
      (let loop ([i pos])
        (cond
          [(fx>= i len) #f]
          [(and (let ([ch (string-ref str i)])
                  (or (char=? ch #\() (char=? ch #\[)))
                (eq? (lexer-state-at str i) 'normal))
           (fx+ i 1)]
          [else (loop (fx+ i 1))]))))

) ; end library
