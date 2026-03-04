;;; (hafod internal re-records) -- RE ADT record types for hafod
;;; Extracted from (hafod re-adt) -- all record types, smart constructors,
;;; predicates, accessors, singletons, and transformation utilities.
;;; Copyright (c) 2026, hafod contributors.
;;; Original scsh code: Copyright (c) 1997, 1998 Olin Shivers.

(library (hafod internal re-records)
  (export
    ;; re-string
    really-make-re-string make-re-string re-string? re-string:chars
    ;; re-char-set
    really-make-re-char-set make-re-char-set re-char-set? re-char-set:cset
    ;; Anchor singletons
    re-bos re-bos? re-eos re-eos? re-bol re-bol? re-eol re-eol?
    ;; re-dsm
    really-make-re-dsm make-re-dsm make-re-dsm/tsm
    re-dsm? re-dsm:body re-dsm:pre-dsm re-dsm:tsm re-dsm:post-dsm
    re-dsm open-dsm
    ;; re-seq
    really-make-re-seq make-re-seq make-re-seq/tsm
    re-seq? re-seq:elts re-seq:tsm
    re-seq
    ;; re-choice
    really-make-re-choice make-re-choice make-re-choice/tsm
    re-choice? re-choice:elts re-choice:tsm
    re-choice
    ;; re-repeat
    really-make-re-repeat make-re-repeat make-re-repeat/tsm
    re-repeat? re-repeat:from re-repeat:to re-repeat:body re-repeat:tsm
    re-repeat
    ;; re-submatch
    really-make-re-submatch make-re-submatch make-re-submatch/tsm
    re-submatch? re-submatch:body re-submatch:pre-dsm re-submatch:tsm
    re-submatch:post-dsm
    re-submatch
    ;; Named singletons
    re-trivial re-trivial? re-empty re-empty? re-any re-any? re-nonl
    ;; Generic
    regexp? re-tsm
    ;; Transformations
    flush-submatches uncase uncase-char-set uncase-string
    ;; Predicates
    re-char-class? static-char-class?)
  (import (hafod internal base)
          (hafod compat)
          (hafod internal char-sets))


  ;; ======================================================================
  ;; re-string
  ;; ======================================================================

  (define-record-type re-string-type
    (fields chars (mutable posix))
    (nongenerative hafod-re-string))

  (define (really-make-re-string chars posix)
    (make-re-string-type chars posix))

  (define-scsh-accessors
    (re-string?      re-string-type?)
    (re-string:chars re-string-type-chars))

  ;; Internal mutable posix field (hand-written)
  (define (re-string:posix re)
    (re-string-type-posix re))

  (define (set-re-string:posix re v)
    (re-string-type-posix-set! re v))


  ;; ======================================================================
  ;; re-char-set
  ;; ======================================================================

  (define-record-type re-char-set-type
    (fields (mutable cset) (mutable posix))
    (nongenerative hafod-re-char-set))

  (define (really-make-re-char-set cset posix)
    (make-re-char-set-type cset posix))

  (define-scsh-accessors
    (re-char-set?     re-char-set-type?)
    (re-char-set:cset re-char-set-type-cset))

  ;; Internal mutable fields (hand-written)
  (define (set-re-char-set:cset re v)
    (re-char-set-type-cset-set! re v))

  (define (re-char-set:posix re)
    (re-char-set-type-posix re))

  (define (set-re-char-set:posix re v)
    (re-char-set-type-posix-set! re v))

  (define (make-re-char-set cset)
    (really-make-re-char-set cset #f))


  ;; ======================================================================
  ;; Anchor records: re-bos, re-eos, re-bol, re-eol
  ;; ======================================================================

  (define-record-type re-bos-type
    (nongenerative hafod-re-bos))

  (define-record-type re-eos-type
    (nongenerative hafod-re-eos))

  (define-record-type re-bol-type
    (nongenerative hafod-re-bol))

  (define-record-type re-eol-type
    (nongenerative hafod-re-eol))

  (define-scsh-accessors
    (re-bos? re-bos-type?)
    (re-eos? re-eos-type?)
    (re-bol? re-bol-type?)
    (re-eol? re-eol-type?))

  ;; Singleton instances
  (define re-bos (make-re-bos-type))
  (define re-eos (make-re-eos-type))
  (define re-bol (make-re-bol-type))
  (define re-eol (make-re-eol-type))


  ;; ======================================================================
  ;; re-dsm (Deleted Sub-Matches)
  ;; ======================================================================

  (define-record-type re-dsm-type
    (fields body pre-dsm tsm (mutable posix))
    (nongenerative hafod-re-dsm))

  (define (really-make-re-dsm body pre-dsm tsm posix)
    (make-re-dsm-type body pre-dsm tsm posix))

  (define-scsh-accessors
    (re-dsm?        re-dsm-type?)
    (re-dsm:body    re-dsm-type-body)
    (re-dsm:pre-dsm re-dsm-type-pre-dsm)
    (re-dsm:tsm     re-dsm-type-tsm))

  ;; Internal mutable posix field (hand-written)
  (define (re-dsm:posix re) (re-dsm-type-posix re))
  (define (set-re-dsm:posix re v) (re-dsm-type-posix-set! re v))

  ;; Virtual field: post-dsm = tsm - pre-dsm - body-tsm
  (define (re-dsm:post-dsm re)
    (- (re-dsm:tsm re)
       (+ (re-dsm:pre-dsm re)
          (re-tsm (re-dsm:body re)))))

  (define (make-re-dsm/tsm body pre-dsm tsm)
    (really-make-re-dsm body pre-dsm tsm #f))

  ;; Raw constructor: make-re-dsm body pre-dsm post-dsm
  (define (make-re-dsm body pre-dsm post-dsm)
    (make-re-dsm/tsm body pre-dsm (+ post-dsm pre-dsm (re-tsm body))))

  ;; Take a regexp RE and return (values body total-pre-dsm).
  ;; Recursively unwraps nested DSMs.
  (define (open-dsm re)
    (let lp ((re re) (pre-dsm 0))
      (if (re-dsm? re)
          (lp (re-dsm:body re) (+ pre-dsm (re-dsm:pre-dsm re)))
          (values re pre-dsm))))

  ;; Smart DSM constructor: absorbs inner DSMs, drops trivial ones.
  (define (re-dsm body pre-dsm post-dsm)
    (let ((tsm (+ pre-dsm (re-tsm body) post-dsm)))
      (receive (body1 pre-dsm1) (open-dsm body)
        (let ((pre-dsm (+ pre-dsm pre-dsm1)))
          (if (= tsm (re-tsm body1))
              body1                            ; Trivial DSM
              (make-re-dsm/tsm body1 pre-dsm tsm))))))


  ;; ======================================================================
  ;; re-seq
  ;; ======================================================================

  (define-record-type re-seq-type
    (fields elts tsm (mutable posix))
    (nongenerative hafod-re-seq))

  (define (really-make-re-seq elts tsm posix)
    (make-re-seq-type elts tsm posix))

  (define-scsh-accessors
    (re-seq?     re-seq-type?)
    (re-seq:elts re-seq-type-elts)
    (re-seq:tsm  re-seq-type-tsm))

  ;; Internal mutable posix field (hand-written)
  (define (re-seq:posix re) (re-seq-type-posix re))
  (define (set-re-seq:posix re v) (re-seq-type-posix-set! re v))

  (define (make-re-seq/tsm elts tsm)
    (really-make-re-seq elts tsm #f))

  (define (make-re-seq res)
    (make-re-seq/tsm res
                     (fold-left (lambda (sm-count re)
                                  (+ (re-tsm re) sm-count))
                                0 res)))

  ;; Smart seq constructor: flattens, drops trivials, simplifies.
  (define (re-seq res)
    (let ((res (let recur ((res res))
                 (if (pair? res)
                     (let* ((re (car res))
                            (tail (recur (cdr res))))
                       (cond ((re-seq? re)
                              (append (recur (re-seq:elts re)) tail))
                             ((re-trivial? re) tail)
                             (else (cons re tail))))
                     '()))))
      (if (pair? res)
          (if (pair? (cdr res))
              (make-re-seq res)           ; General case
              (car res))                  ; Singleton
          re-trivial)))                   ; Empty => ""


  ;; ======================================================================
  ;; re-choice
  ;; ======================================================================

  (define-record-type re-choice-type
    (fields elts tsm (mutable posix))
    (nongenerative hafod-re-choice))

  (define (really-make-re-choice elts tsm posix)
    (make-re-choice-type elts tsm posix))

  (define-scsh-accessors
    (re-choice?     re-choice-type?)
    (re-choice:elts re-choice-type-elts)
    (re-choice:tsm  re-choice-type-tsm))

  ;; Internal mutable posix field (hand-written)
  (define (re-choice:posix re) (re-choice-type-posix re))
  (define (set-re-choice:posix re v) (re-choice-type-posix-set! re v))

  (define (make-re-choice/tsm elts tsm)
    (really-make-re-choice elts tsm #f))

  (define (make-re-choice res)
    (if (for-all re-char-set? res)
        (make-re-char-set (apply char-set-union (map re-char-set:cset res)))
        (make-re-choice/tsm res
                            (fold-left (lambda (sm-count re)
                                         (+ (re-tsm re) sm-count))
                                       0 res))))

  ;; Smart choice constructor: flattens, drops empties, merges char-classes.
  (define (re-choice res)
    (let ((res (let recur ((res res))
                 (if (pair? res)
                     (let* ((re (car res))
                            (tail (recur (cdr res))))
                       (cond ((re-choice? re)
                              (append (recur (re-choice:elts re)) tail))
                             ((re-empty? re) tail)
                             (else (cons re tail))))
                     '()))))
      ;; If all elts are char-class REs, fold them together.
      (if (for-all static-char-class? res)
          (if (null? res)
              re-empty
              (let ((cset (apply char-set-union
                                 (map (lambda (elt)
                                        (if (re-char-set? elt)
                                            (re-char-set:cset elt)
                                            (string->char-set (re-string:chars elt))))
                                      res))))
                (if (= 1 (char-set-size cset))
                    (make-re-string (apply string (char-set->list cset)))
                    (make-re-char-set cset))))
          (if (pair? res)
              (if (pair? (cdr res))
                  (make-re-choice res)      ; General case
                  (car res))                ; Singleton
              re-empty))))                  ; Empty choice


  ;; ======================================================================
  ;; re-repeat
  ;; ======================================================================

  (define-record-type re-repeat-type
    (fields from to body tsm (mutable posix))
    (nongenerative hafod-re-repeat))

  (define (really-make-re-repeat from to body tsm posix)
    (make-re-repeat-type from to body tsm posix))

  (define-scsh-accessors
    (re-repeat?      re-repeat-type?)
    (re-repeat:from  re-repeat-type-from)
    (re-repeat:to    re-repeat-type-to)
    (re-repeat:body  re-repeat-type-body)
    (re-repeat:tsm   re-repeat-type-tsm))

  ;; Internal mutable posix field (hand-written)
  (define (re-repeat:posix re) (re-repeat-type-posix re))
  (define (set-re-repeat:posix re v) (re-repeat-type-posix-set! re v))

  (define (make-re-repeat/tsm from to body tsm)
    (really-make-re-repeat from to body tsm #f))

  (define (make-re-repeat from to body)
    (make-re-repeat/tsm (check-arg (lambda (from)
                                     (or (not (integer? from))
                                         (>= from 0)))
                                   from
                                   make-re-repeat)
                        (check-arg (lambda (to)
                                     (or (not (integer? to))
                                         (and (integer? to) (>= to 0))))
                                   to
                                   make-re-repeat)
                        body
                        (re-tsm body)))

  ;; Smart repeat constructor: reduces trivial cases, commutes with DSM.
  (define (re-repeat from to body)
    (receive (re pre-dsm) (reduce-repeat from to body 0)
      (re-dsm re pre-dsm (- (re-tsm body) (+ pre-dsm (re-tsm re))))))

  ;; Helper that does all the reduction work.
  (define (reduce-repeat from to body pre-dsm)
    (receive (from to body1 pre-dsm)
             ;; Collapse nested repeats and DSMs:
             (let iter ((from from) (to to) (body body) (dsm0 pre-dsm))
               (receive (body body-dsm0) (open-dsm body)
                 (let ((dsm0 (+ dsm0 body-dsm0)))
                   (if (and (integer? from)
                            (or (not to) (integer? to))
                            (re-repeat? body))
                       (let ((bfrom (re-repeat:from body))
                             (bto (re-repeat:to body))
                             (bbody (re-repeat:body body)))
                         (if (or (not (integer? bfrom))
                                 (and bto (not (integer? bto))))
                             (values from to body dsm0)
                             (iter (* from bfrom)
                                   (and to bto (* to bto))
                                   bbody
                                   dsm0)))
                       (values from to body dsm0)))))
      (cond
       ;; re{1,1} => re
       ((and (eqv? from 1) (eqv? to 1))
        (values body1 pre-dsm))

       ;; re{0,0} => ""
       ((and (eqv? from 0) (eqv? to 0))
        (values re-trivial (+ (re-tsm body1) pre-dsm)))

       ;; re{m,n} => re-empty when m>n
       ((and (integer? from) (integer? to) (> from to))
        (values re-empty (+ (re-tsm body1) pre-dsm)))

       ;; Reduce the body = re-empty case.
       ((and (re-empty? body1) (integer? from))
        (values (if (> from 0) re-empty re-trivial)
                pre-dsm))

       ;; If body1 is eos, bos, or "", and m<=n, reduce to simply body1.
       ((and (integer? from)
             (or (and (integer? to) (<= from to)) (not to))
             (or (re-eos? body1)
                 (re-bos? body1)
                 (and (re-string? body1)
                      (string=? "" (re-string:chars body1)))))
        (values body1 pre-dsm))

       ;; General case
       (else (values (make-re-repeat from to body1)
                     pre-dsm)))))


  ;; ======================================================================
  ;; re-submatch
  ;; ======================================================================

  (define-record-type re-submatch-type
    (fields body pre-dsm tsm (mutable posix))
    (nongenerative hafod-re-submatch))

  (define (really-make-re-submatch body pre-dsm tsm posix)
    (make-re-submatch-type body pre-dsm tsm posix))

  (define-scsh-accessors
    (re-submatch?       re-submatch-type?)
    (re-submatch:body   re-submatch-type-body)
    (re-submatch:pre-dsm re-submatch-type-pre-dsm)
    (re-submatch:tsm    re-submatch-type-tsm))

  ;; Internal mutable posix field (hand-written)
  (define (re-submatch:posix re) (re-submatch-type-posix re))
  (define (set-re-submatch:posix re v) (re-submatch-type-posix-set! re v))

  ;; Virtual field: post-dsm
  (define (re-submatch:post-dsm re)
    (- (re-submatch:tsm re)
       (+ 1
          (re-submatch:pre-dsm re)
          (re-tsm (re-submatch:body re)))))

  (define (make-re-submatch/tsm body pre-dsm tsm)
    (really-make-re-submatch body pre-dsm tsm #f))

  (define (make-re-submatch body . maybe-pre+post-dsm)
    (let-optionals maybe-pre+post-dsm ((pre-dsm 0) (post-dsm 0))
      (make-re-submatch/tsm body pre-dsm (+ pre-dsm 1 (re-tsm body) post-dsm))))

  ;; Smart submatch constructor: unpack DSMs, handle re-empty body.
  (define (re-submatch body . maybe-pre+post-dsm)
    (let-optionals maybe-pre+post-dsm ((pre-dsm 0) (post-dsm 0))
      (let ((tsm (+ 1 pre-dsm (re-tsm body) post-dsm)))
        (receive (body1 pre-dsm1) (open-dsm body)
          (if (re-empty? body1)
              (re-dsm re-empty tsm 0)
              (make-re-submatch/tsm body1 (+ pre-dsm pre-dsm1) tsm))))))


  ;; ======================================================================
  ;; Singletons
  ;; ======================================================================

  ;; Internal empty-string RE (not exported; used to define re-trivial)
  (define re-empty-string (really-make-re-string "" #f))

  ;; re-trivial: matches the empty string. Defined as a DSM wrapping
  ;; an empty-string re-string with pre-dsm=1, tsm=0.
  ;; This is a special sentinel; re-trivial? checks by identity (eq?).
  (define re-trivial (really-make-re-dsm re-empty-string 1 0 #f))

  (define (re-trivial? re) (eq? re re-trivial))

  ;; make-re-string: smart constructor; returns re-trivial for "".
  (define (make-re-string chars)
    (if (string=? "" chars)
        re-trivial
        (really-make-re-string chars #f)))

  ;; re-empty: never matches (empty char-set)
  (define re-empty (make-re-char-set char-set:empty))

  (define (re-empty? re)
    (and (re-char-set? re)
         (let ((cs (re-char-set:cset re)))
           (and (char-set? cs)
                (char-set-empty? cs)))))

  ;; re-any: matches any character (full char-set)
  (define re-any (make-re-char-set char-set:full))

  (define (re-any? re)
    (and (re-char-set? re)
         (let ((cs (re-char-set:cset re)))
           (and (char-set? cs)
                (char-set-full? cs)))))

  ;; re-nonl: matches any character except newline
  (define re-nonl
    (make-re-char-set (char-set-complement (char-set #\newline))))


  ;; ======================================================================
  ;; Generic predicates and accessors
  ;; ======================================================================

  (define (regexp? x)
    (or (re-seq? x) (re-choice? x) (re-repeat? x)
        (re-char-set? x) (re-string? x)
        (re-bos? x) (re-eos? x)
        (re-bol? x) (re-eol? x)
        (re-submatch? x) (re-dsm? x)))

  ;; Return total number of submatches bound in RE.
  (define (re-tsm re)
    (cond
     ((re-seq? re)      (re-seq:tsm re))
     ((re-choice? re)   (re-choice:tsm re))
     ((re-repeat? re)   (re-repeat:tsm re))
     ((re-dsm? re)      (re-dsm:tsm re))
     ((re-submatch? re) (re-submatch:tsm re))
     ((or (re-char-set? re) (re-string? re)
          (re-bos? re) (re-eos? re)
          (re-bol? re) (re-eol? re))
      0)
     (else (error 're-tsm "not a regexp" re))))


  ;; ======================================================================
  ;; Char-class predicates
  ;; ======================================================================

  (define (re-char-class? re)
    (or (re-char-set? re)
        (and (re-string? re)
             (= 1 (string-length (re-string:chars re))))))

  (define (static-char-class? re)
    (or (and (re-char-set? re)
             (char-set? (re-char-set:cset re)))
        (and (re-string? re)
             (= 1 (string-length (re-string:chars re))))))


  ;; ======================================================================
  ;; flush-submatches
  ;; ======================================================================

  (define (flush-submatches re)
    (cond
     ((zero? (re-tsm re)) re)

     ((re-seq? re)
      (re-seq (map flush-submatches (re-seq:elts re))))

     ((re-choice? re)
      (re-choice (map flush-submatches (re-choice:elts re))))

     ((re-repeat? re)
      (re-repeat (re-repeat:from re)
                 (re-repeat:to re)
                 (flush-submatches (re-repeat:body re))))

     ((re-submatch? re) (flush-submatches (re-submatch:body re)))
     ((re-dsm? re) (flush-submatches (re-dsm:body re)))

     (else re)))


  ;; ======================================================================
  ;; map/changed helper
  ;; ======================================================================

  (define (map/changed f elts)
    (let recur ((elts elts))
      (if (pair? elts)
          (let ((elt (car elts)))
            (receive (new-elts elts-changed?) (recur (cdr elts))
              (receive (new-elt elt-changed?) (f elt)
                (if (or elts-changed? elt-changed?)
                    (values (cons new-elt new-elts) #t)
                    (values elts #f)))))
          (values '() #f))))


  ;; ======================================================================
  ;; uncase: make RE case-insensitive
  ;; ======================================================================

  (define (uncase re)
    (receive (new-re changed?)
        (let recur ((re re))
          (cond
           ((re-seq? re)
            (let ((elts (re-seq:elts re)))
              (receive (new-elts elts-changed?)
                  (map/changed recur elts)
                (if elts-changed?
                    (values (make-re-seq/tsm new-elts (re-seq:tsm re)) #t)
                    (values re #f)))))

           ((re-choice? re)
            (let ((elts (re-choice:elts re)))
              (receive (new-elts elts-changed?)
                  (map/changed recur elts)
                (if elts-changed?
                    (values (re-choice new-elts) #t)
                    (values re #f)))))

           ((re-char-set? re)
            (let* ((cs (re-char-set:cset re))
                   (new-cs-re (uncase-char-set cs)))
              (if (char-set= cs (re-char-set:cset new-cs-re))
                  (values re #f)
                  (values new-cs-re #t))))

           ((re-repeat? re)
            (receive (new-body body-changed?) (recur (re-repeat:body re))
              (if body-changed?
                  (values (re-repeat (re-repeat:from re)
                                     (re-repeat:to re)
                                     new-body)
                          #t)
                  (values re #f))))

           ((re-submatch? re)
            (receive (new-body body-changed?) (recur (re-submatch:body re))
              (if body-changed?
                  (values (make-re-submatch/tsm new-body
                                                (re-submatch:pre-dsm re)
                                                (re-submatch:tsm re))
                          #t)
                  (values re #f))))

           ((re-string? re)
            (let ((cf-re (uncase-string (re-string:chars re))))
              (if (re-string? cf-re)
                  (values re #f)
                  (values cf-re #t))))

           (else (values re #f))))
      new-re))


  ;; ======================================================================
  ;; uncase-char-set
  ;; ======================================================================

  (define (uncase-char-set cs)
    (make-re-char-set
     (char-set-fold (lambda (c new-cset)
                      (char-set-adjoin! new-cset
                                        (char-downcase c)
                                        (char-upcase c)))
                    (char-set-copy char-set:empty)
                    cs)))


  ;; ======================================================================
  ;; uncase-string
  ;; ======================================================================

  (define (uncase-string s)
    ;; Build list of chars and doubleton char-sets.
    (let* ((seq (let loop ((i (- (string-length s) 1)) (acc '()))
                  (if (< i 0) acc
                      (let ((c (string-ref s i)))
                        (loop (- i 1)
                              (cons (cond
                                     ((char-lower-case? c)
                                      (char-set c (char-upcase c)))
                                     ((char-upper-case? c)
                                      (char-set c (char-downcase c)))
                                     (else c))
                                    acc))))))

           ;; Coalesce adjacent chars together into a string.
           (fixup (lambda (chars seq)
                    (if (pair? chars)
                        (cons (make-re-string (list->string (reverse chars)))
                              seq)
                        seq)))

           (new-seq (let recur ((seq seq) (chars '()))
                      (if (pair? seq)
                          (let ((elt (car seq))
                                (seq (cdr seq)))
                            (if (char? elt)
                                (recur seq (cons elt chars))
                                (fixup chars (cons (make-re-char-set elt)
                                                   (recur seq '())))))
                          (fixup chars '())))))

      (if (= 1 (length new-seq))
          (car new-seq)
          (make-re-seq new-seq))))


  ) ; end library
