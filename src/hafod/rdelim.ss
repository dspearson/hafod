;;; (hafod rdelim) -- Delimited readers
;;; Ported from scsh/scheme/rdelim.scm
;;; Complete rewrite using Chez standard port operations (get-char, lookahead-char, unget-char)
;;; instead of Scheme48 port-buffer internals.
;;; Copyright (c) 1992 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod rdelim)
  (export read-line read-delimited read-delimited! %read-delimited!
          read-paragraph skip-char-set)
  (import (hafod internal base)
          (hafod compat)
          (hafod internal strings))

  ;;; ========== %read-delimited! (core primitive) ==========
  ;;; Returns two values: (values TERMINATOR NUM-READ)
  ;;; TERMINATOR is:
  ;;;   - a character => read terminated by this delimiter char
  ;;;   - eof-object  => read terminated by EOF
  ;;;   - #f          => buffer filled without terminating
  ;;; NUM-READ is the number of chars written into buf.
  ;;;
  ;;; If GOBBLE? is true, a terminator character is consumed from the port.
  ;;; Otherwise, it is left for a following read.

  (define (%read-delimited! delims buf gobble? . args)
    (let-optionals* args ((port  (current-input-port))
                          (start 0)
                          (end   (string-length buf)))
      (let ((cset (x->char-set delims)))
        (let lp ((i start))
          (cond
           ;; Buffer full -- peek to check for delimiter or EOF
           ((>= i end)
            (let ((c (lookahead-char port)))
              (cond
               ((eof-object? c)
                (values c (- i start)))
               ((char-set-contains? cset c)
                (when gobble? (get-char port))
                (values c (- i start)))
               (else
                (values #f (- i start))))))
           ;; Buffer has room -- read next char
           (else
            (let ((c (get-char port)))
              (cond
               ((eof-object? c)
                (values c (- i start)))
               ((char-set-contains? cset c)
                (unless gobble? (unget-char port c))
                (values c (- i start)))
               (else
                (string-set! buf i c)
                (lp (+ i 1)))))))))))

  ;;; ========== read-delimited! ==========
  ;;; Returns:
  ;;;   - EOF if at end of file and a non-zero read was requested
  ;;;   - Integer j if that many chars read into buf
  ;;;   - #f if buffer was filled without finding a delimiter
  ;;; DELIM-ACTION: peek, trim (default), concat, split

  (define (read-delimited! delims buf . args)
    (let-optionals* args ((port         (current-input-port))
                          (delim-action 'trim)
                          (start        0)
                          (end          (string-length buf)))
      (receive (terminator num-read)
               (%read-delimited! delims buf
                                 (not (eq? delim-action 'peek))
                                 port
                                 start
                                 (if (eq? delim-action 'concat)
                                     (- end 1)  ; Room for terminator
                                     end))
        (if terminator    ; Check for buffer overflow
            (let ((retval (if (and (zero? num-read)
                                   (eof-object? terminator))
                              terminator
                              num-read)))
              (case delim-action
                ((peek trim) retval)
                ((split) (values retval terminator))
                ((concat) (cond ((char? terminator)
                                 (string-set! buf (+ start num-read) terminator)
                                 (+ num-read 1))
                                (else retval)))))

            ;; Buffer overflow
            (case delim-action
              ((peek trim) #f)
              ((split) (values #f #f))
              ((concat) (let ((last (get-char port)))
                          (if (char? last)
                              (string-set! buf (+ start num-read) last))
                          (and (or (eof-object? last)
                                   (char-set-contains? (x->char-set delims) last))
                               (+ num-read 1)))))))))

  ;;; ========== read-delimited ==========
  ;;; Unbounded version: allocates buffers dynamically, doubling each time.
  ;;; Returns a string (or EOF). DELIM-ACTION: peek, trim (default), concat, split.

  (define (read-delimited delims . args)
    (let-optionals* args ((port         (current-input-port))
                          (delim-action 'trim))
      (let ((substr (lambda (s end)
                      (if (= end (string-length s)) s
                          (substring s 0 end))))
            (delims (x->char-set delims))
            (gobble? (not (eq? delim-action 'peek))))
        ;; BUFLEN is total amount of buffer space allocated to date.
        (let lp ((strs '()) (buflen 80) (buf (make-string 80)))
          (receive (terminator num-read)
                   (%read-delimited! delims buf gobble? port)
            (if terminator
                ;; We are done.
                (let ((retval (if (and (zero? num-read)
                                       (eof-object? terminator)
                                       (null? strs))
                                  terminator

                                  (let ((s (substr buf num-read)))
                                    (cond ((and (eq? delim-action 'concat)
                                                (char? terminator))
                                           (apply string-append
                                                  (reverse (cons (string terminator)
                                                                 (cons s strs)))))
                                          ((null? strs) s)
                                          (else (apply string-append
                                                       (reverse (cons s strs)))))))))
                  (if (eq? delim-action 'split)
                      (values retval terminator)
                      retval))

                ;; Not done -- loop with bigger buffer.
                (lp (cons buf strs)
                    (+ buflen buflen)
                    (make-string buflen))))))))

  ;;; ========== read-line ==========
  ;;; Simple wrapper using newline as delimiter.
  (define (read-line . rest)
    (apply read-delimited char-set:newline rest))

  ;;; ========== skip-char-set ==========
  ;;; Skip characters in the given set, return count of chars skipped.

  (define (skip-char-set skip-chars . maybe-port)
    (let ((port (:optional maybe-port (current-input-port)))
          (cset (x->char-set skip-chars)))
      (let lp ((count 0))
        (let ((c (lookahead-char port)))
          (cond
           ((eof-object? c) count)
           ((char-set-contains? cset c)
            (get-char port)
            (lp (+ count 1)))
           (else count))))))

  ;;; ========== read-paragraph ==========
  ;;; Read a paragraph: skip blank lines, then read non-blank lines until
  ;;; a blank line or EOF. handle-delim: peek, trim (default), concat, split.

  (define (whitespace-line? line)
    (string-every (lambda (c) (char-set-contains? char-set:whitespace c)) line))

  (define (read-paragraph . args)
    (let-optionals* args ((port         (current-input-port))
                          (handle-delim 'trim))
      ;; First, skip all blank lines.
      (let lp ()
        (let ((line (read-line port 'concat)))
          (cond ((eof-object? line)
                 (if (eq? handle-delim 'split) (values line line) line))

                ((whitespace-line? line) (lp))

                ;; Then, read in non-blank lines.
                (else
                 (let lp2 ((lines (list line)))
                   (let ((line (read-line port 'concat)))
                     (if (and (string? line)
                              (not (whitespace-line? line)))

                         (lp2 (cons line lines))

                         ;; Return the paragraph
                         (let ((->str (lambda (lns) (apply string-append (reverse lns)))))
                           (case handle-delim
                             ((trim) (->str lines))
                             ((concat)
                              (->str (if (eof-object? line) lines (cons line lines))))
                             ((split)
                              (values (->str lines) line))
                             (else (error 'read-paragraph
                                          "Illegal handle-delim parameter"
                                          handle-delim)))))))))))))

  ) ; end library
