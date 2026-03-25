#!chezscheme
;;; (hafod srfi-1) -- SRFI-1 List Library
;;; Provides full SRFI-1 compatibility for scsh parity.
;;; Delegates to Chez Scheme builtins where possible.
;;; Reference: https://srfi.schemers.org/srfi-1/srfi-1.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-1)
  (export
    ;; Constructors
    xcons cons* make-list list-tabulate list-copy circular-list iota
    ;; Predicates
    proper-list? circular-list? dotted-list? not-pair? null-list? list=
    ;; Selectors
    first second third fourth fifth sixth seventh eighth ninth tenth
    car+cdr take drop take-right drop-right take! drop-right! split-at split-at!
    last last-pair
    ;; Miscellaneous
    length+ zip unzip1 unzip2 unzip3 unzip4 unzip5
    count append! concatenate concatenate! reverse! append-reverse
    append-reverse!
    ;; Fold, unfold & map
    fold fold-right unfold unfold-right
    pair-fold pair-fold-right
    reduce reduce-right
    append-map append-map!
    map! pair-for-each filter-map map-in-order
    ;; Filtering & partitioning
    filter partition remove
    filter! partition! remove!
    ;; Searching
    find any every list-index
    take-while drop-while take-while!
    span span! break break!
    ;; Deleting
    delete delete! delete-duplicates delete-duplicates!
    ;; Association lists
    assoc alist-cons alist-copy alist-delete alist-delete!
    ;; Set operations on lists
    lset<= lset=
    lset-union lset-union!
    lset-intersection lset-intersection!
    lset-difference lset-difference!
    lset-xor lset-xor!
    lset-diff+intersection lset-diff+intersection!
    lset-adjoin)

  (import
    (except (chezscheme)
      ;; We redefine these with SRFI-1 semantics
      fold-right   ; need multi-list support
      iota         ; SRFI-1 version supports (iota count start step)
      break        ; Chez's break is unrelated (debugger)
      assoc        ; SRFI-1 version takes optional = arg
      ;; delete — not in Chez, defined below
      exists       ; replaced by any
      for-all      ; replaced by every
      remove!      ; SRFI-1 version
      ))

  ;; =========================================================
  ;; Constructors
  ;; =========================================================

  (define (xcons d a) (cons a d))

  ;; cons* — already imported from Chez

  ;; make-list — already imported from Chez

  (define (list-tabulate n init-proc)
    (let loop ([i (- n 1)] [acc '()])
      (if (< i 0) acc
          (loop (- i 1) (cons (init-proc i) acc)))))

  ;; list-copy — already imported from Chez

  (define (circular-list val . vals)
    (let ([lst (cons val vals)])
      (set-cdr! (last-pair lst) lst)
      lst))

  ;; iota — Chez only has (iota count); SRFI-1 needs (iota count [start step])
  ;; Shadow with a version that handles all three forms.
  (define iota
    (case-lambda
      [(count) (iota count 0 1)]
      [(count start) (iota count start 1)]
      [(count start step)
       (let loop ([i 0] [acc '()])
         (if (>= i count) (reverse acc)
             (loop (+ i 1) (cons (+ start (* i step)) acc))))]))

  ;; =========================================================
  ;; Predicates
  ;; =========================================================

  (define (proper-list? x)
    (let loop ([fast x] [slow x])
      (cond
        [(null? fast) #t]
        [(not (pair? fast)) #f]
        [(null? (cdr fast)) #t]
        [(not (pair? (cdr fast))) #f]
        [(eq? (cdr fast) slow) #f]  ; circular
        [else (loop (cddr fast) (cdr slow))])))

  (define (circular-list? x)
    (let loop ([fast x] [slow x])
      (cond
        [(null? fast) #f]
        [(not (pair? fast)) #f]
        [(null? (cdr fast)) #f]
        [(not (pair? (cdr fast))) #f]
        [(eq? (cddr fast) (cdr slow)) #t]
        [else (loop (cddr fast) (cdr slow))])))

  (define (dotted-list? x)
    (let loop ([fast x] [slow x])
      (cond
        [(null? fast) #f]
        [(not (pair? fast)) #t]
        [(null? (cdr fast)) #f]
        [(not (pair? (cdr fast))) #t]
        [(eq? (cdr fast) slow) #f]
        [else (loop (cddr fast) (cdr slow))])))

  (define (not-pair? x) (not (pair? x)))

  (define (null-list? lst)
    (cond
      [(null? lst) #t]
      [(pair? lst) #f]
      [else (error 'null-list? "not a proper list" lst)]))

  (define (list= elt= . lists)
    (or (null? lists)
        (let loop ([lists lists])
          (or (null? (cdr lists))
              (let inner ([a (car lists)] [b (cadr lists)])
                (cond
                  [(and (null? a) (null? b))
                   (loop (cdr lists))]
                  [(or (null? a) (null? b)) #f]
                  [(elt= (car a) (car b))
                   (inner (cdr a) (cdr b))]
                  [else #f]))))))

  ;; =========================================================
  ;; Selectors
  ;; =========================================================

  (define (first  x) (car x))
  (define (second x) (cadr x))
  (define (third  x) (caddr x))
  (define (fourth x) (cadddr x))
  (define (fifth  x) (car (cddddr x)))
  (define (sixth  x) (cadr (cddddr x)))
  (define (seventh x) (caddr (cddddr x)))
  (define (eighth x) (cadddr (cddddr x)))
  (define (ninth  x) (car (cddddr (cddddr x))))
  (define (tenth  x) (cadr (cddddr (cddddr x))))

  (define (car+cdr pair) (values (car pair) (cdr pair)))

  (define (take lst k)
    (let loop ([lst lst] [k k] [acc '()])
      (if (zero? k) (reverse acc)
          (loop (cdr lst) (- k 1) (cons (car lst) acc)))))

  (define (drop lst k)
    (if (zero? k) lst
        (drop (cdr lst) (- k 1))))

  (define (take-right lst k)
    (let ([len (length lst)])
      (drop lst (- len k))))

  (define (drop-right lst k)
    (let ([len (length lst)])
      (take lst (- len k))))

  (define (drop-right! lst k)
    (let ([len (length lst)])
      (if (<= len k) '()
          (begin
            (set-cdr! (drop lst (- len k 1)) '())
            lst))))

  (define (take! lst k)
    (if (zero? k) '()
        (begin
          (set-cdr! (drop lst (- k 1)) '())
          lst)))

  (define (split-at lst k)
    (values (take lst k) (drop lst k)))

  (define (split-at! lst k)
    (if (zero? k)
        (values '() lst)
        (let ([tail (drop lst (- k 1))])
          (let ([rest (cdr tail)])
            (set-cdr! tail '())
            (values lst rest)))))

  (define (last lst)
    (car (last-pair lst)))

  ;; last-pair — already imported from Chez

  ;; =========================================================
  ;; Miscellaneous: length+, zip, unzip, count
  ;; =========================================================

  (define (length+ lst)
    (let loop ([fast lst] [slow lst] [len 0])
      (cond
        [(null? fast) len]
        [(not (pair? fast)) #f]
        [(null? (cdr fast)) (+ len 1)]
        [(not (pair? (cdr fast))) #f]
        [(and (> len 0) (eq? (cdr fast) slow)) #f]  ; circular
        [else (loop (cddr fast) (cdr slow) (+ len 2))])))

  (define (zip . lists)
    (apply map list lists))

  (define (unzip1 lst) (map car lst))
  (define (unzip2 lst)
    (values (map car lst) (map cadr lst)))
  (define (unzip3 lst)
    (values (map car lst) (map cadr lst) (map caddr lst)))
  (define (unzip4 lst)
    (values (map car lst) (map cadr lst) (map caddr lst) (map cadddr lst)))
  (define (unzip5 lst)
    (values (map car lst) (map cadr lst) (map caddr lst)
            (map cadddr lst) (map (lambda (x) (car (cddddr x))) lst)))

  (define count
    (case-lambda
      [(pred lst)
       (let loop ([lst lst] [n 0])
         (if (null? lst) n
             (loop (cdr lst) (if (pred (car lst)) (+ n 1) n))))]
      [(pred lst . lists)
       (let loop ([lsts (cons lst lists)] [n 0])
         (if (any null? lsts) n
             (loop (map cdr lsts)
                   (if (apply pred (map car lsts)) (+ n 1) n))))]))

  ;; concatenate
  (define (concatenate lists) (apply append lists))
  (define (concatenate! lists) (apply append! lists))

  ;; append-reverse
  (define (append-reverse rev-head tail)
    (let loop ([rev-head rev-head] [tail tail])
      (if (null? rev-head) tail
          (loop (cdr rev-head) (cons (car rev-head) tail)))))

  (define (append-reverse! rev-head tail)
    (let loop ([rev-head rev-head] [tail tail])
      (if (null? rev-head) tail
          (let ([next (cdr rev-head)])
            (set-cdr! rev-head tail)
            (loop next rev-head)))))

  ;; =========================================================
  ;; Fold, unfold & map
  ;; =========================================================

  ;; SRFI-1 fold: (fold kons knil lis1 ...)
  ;; kons takes (element ... accumulator) — element FIRST
  ;; NB: Chez fold-left takes (accumulator element ...) — REVERSED
  (define fold
    (case-lambda
      [(kons knil lst)
       (let loop ([lst lst] [acc knil])
         (if (null? lst) acc
             (loop (cdr lst) (kons (car lst) acc))))]
      [(kons knil lst . lists)
       (let loop ([lsts (cons lst lists)] [acc knil])
         (if (any null? lsts) acc
             (loop (map cdr lsts)
                   (apply kons (append (map car lsts) (list acc))))))]))

  ;; SRFI-1 fold-right: (fold-right kons knil lis1 ...)
  ;; kons takes (element ... accumulator) — same as Chez fold-right for single list
  ;; Chez fold-right already correct for single list, but we need multi-list support
  (define fold-right
    (case-lambda
      [(kons knil lst)
       (let loop ([lst lst])
         (if (null? lst) knil
             (kons (car lst) (loop (cdr lst)))))]
      [(kons knil lst . lists)
       (let loop ([lsts (cons lst lists)])
         (if (any null? lsts) knil
             (apply kons (append (map car lsts)
                                 (list (loop (map cdr lsts)))))))]))

  (define (unfold p f g seed . maybe-tail-gen)
    (let ([tail-gen (if (null? maybe-tail-gen) (lambda (x) '()) (car maybe-tail-gen))])
      (let loop ([seed seed])
        (if (p seed)
            (tail-gen seed)
            (cons (f seed) (loop (g seed)))))))

  (define (unfold-right p f g seed . maybe-tail)
    (let ([tail (if (null? maybe-tail) '() (car maybe-tail))])
      (let loop ([seed seed] [acc tail])
        (if (p seed) acc
            (loop (g seed) (cons (f seed) acc))))))

  (define pair-fold
    (case-lambda
      [(kons knil lst)
       (let loop ([lst lst] [acc knil])
         (if (null? lst) acc
             (let ([tail (cdr lst)])
               (loop tail (kons lst acc)))))]
      [(kons knil lst . lists)
       (let loop ([lsts (cons lst lists)] [acc knil])
         (if (any null? lsts) acc
             (let ([tails (map cdr lsts)])
               (loop tails (apply kons (append lsts (list acc)))))))]))

  (define pair-fold-right
    (case-lambda
      [(kons knil lst)
       (let loop ([lst lst])
         (if (null? lst) knil
             (kons lst (loop (cdr lst)))))]
      [(kons knil lst . lists)
       (let loop ([lsts (cons lst lists)])
         (if (any null? lsts) knil
             (apply kons (append lsts (list (loop (map cdr lsts)))))))]))

  (define reduce
    (case-lambda
      [(f ridentity lst)
       (if (null? lst) ridentity
           (fold f (car lst) (cdr lst)))]))

  (define reduce-right
    (case-lambda
      [(f ridentity lst)
       (if (null? lst) ridentity
           (fold-right f (last lst) (drop-right lst 1)))]))

  ;; map — use Chez's built-in map (handles multiple lists)

  ;; for-each — use Chez's built-in for-each

  (define append-map
    (case-lambda
      [(f lst) (concatenate (map f lst))]
      [(f lst . lists) (concatenate (apply map f lst lists))]))

  (define append-map!
    (case-lambda
      [(f lst) (concatenate! (map f lst))]
      [(f lst . lists) (concatenate! (apply map f lst lists))]))

  (define (map! f lst)
    (let loop ([lst lst])
      (unless (null? lst)
        (set-car! lst (f (car lst)))
        (loop (cdr lst))))
    lst)

  (define pair-for-each
    (case-lambda
      [(f lst)
       (let loop ([lst lst])
         (unless (null? lst)
           (let ([tail (cdr lst)])
             (f lst)
             (loop tail))))]
      [(f lst . lists)
       (let loop ([lsts (cons lst lists)])
         (unless (any null? lsts)
           (let ([tails (map cdr lsts)])
             (apply f lsts)
             (loop tails))))]))

  (define filter-map
    (case-lambda
      [(f lst)
       (let loop ([lst lst] [acc '()])
         (if (null? lst) (reverse acc)
             (let ([v (f (car lst))])
               (loop (cdr lst) (if v (cons v acc) acc)))))]
      [(f lst . lists)
       (let loop ([lsts (cons lst lists)] [acc '()])
         (if (any null? lsts) (reverse acc)
             (let ([v (apply f (map car lsts))])
               (loop (map cdr lsts) (if v (cons v acc) acc)))))]))

  (define map-in-order
    (case-lambda
      [(f lst)
       (let loop ([lst lst] [acc '()])
         (if (null? lst) (reverse acc)
             (loop (cdr lst) (cons (f (car lst)) acc))))]
      [(f lst . lists)
       (let loop ([lsts (cons lst lists)] [acc '()])
         (if (any null? lsts) (reverse acc)
             (loop (map cdr lsts)
                   (cons (apply f (map car lsts)) acc))))]))

  ;; =========================================================
  ;; Filtering & partitioning
  ;; =========================================================

  ;; filter, partition, remove — already imported from Chez

  (define (filter! pred lst)
    (filter pred lst))  ; non-destructive fallback

  (define (partition! pred lst)
    (partition pred lst))

  (define (remove! pred lst)
    (remove pred lst))

  ;; =========================================================
  ;; Searching
  ;; =========================================================

  ;; find — already imported from Chez

  ;; any/every — SRFI-1 names for Chez's exists/for-all
  ;; But SRFI-1's any returns the value from pred, not just #t/#f
  (define any
    (case-lambda
      [(pred lst)
       (let loop ([lst lst])
         (cond
           [(null? lst) #f]
           [(null? (cdr lst)) (pred (car lst))]  ; tail position
           [(pred (car lst))]
           [else (loop (cdr lst))]))]
      [(pred lst . lists)
       (let loop ([lsts (cons lst lists)])
         (cond
           [(any null? lsts) #f]   ; uses single-list any recursively
           [else
            (let ([heads (map car lsts)]
                  [tails (map cdr lsts)])
              (if (any null? tails)
                  (apply pred heads)  ; tail position for last
                  (or (apply pred heads)
                      (loop tails))))]))]))

  (define every
    (case-lambda
      [(pred lst)
       (or (null? lst)
           (let loop ([lst lst])
             (cond
               [(null? (cdr lst)) (pred (car lst))]  ; tail position
               [(pred (car lst)) (loop (cdr lst))]
               [else #f])))]
      [(pred lst . lists)
       (let ([lsts (cons lst lists)])
         (or (any null? lsts)
             (let loop ([lsts lsts])
               (let ([heads (map car lsts)]
                     [tails (map cdr lsts)])
                 (if (any null? tails)
                     (apply pred heads)
                     (if (apply pred heads)
                         (loop tails)
                         #f))))))]))

  (define list-index
    (case-lambda
      [(pred lst)
       (let loop ([lst lst] [i 0])
         (cond
           [(null? lst) #f]
           [(pred (car lst)) i]
           [else (loop (cdr lst) (+ i 1))]))]
      [(pred lst . lists)
       (let loop ([lsts (cons lst lists)] [i 0])
         (cond
           [(any null? lsts) #f]
           [(apply pred (map car lsts)) i]
           [else (loop (map cdr lsts) (+ i 1))]))]))

  (define (take-while pred lst)
    (let loop ([lst lst] [acc '()])
      (if (or (null? lst) (not (pred (car lst))))
          (reverse acc)
          (loop (cdr lst) (cons (car lst) acc)))))

  (define (take-while! pred lst)
    (take-while pred lst))

  (define (drop-while pred lst)
    (let loop ([lst lst])
      (if (or (null? lst) (not (pred (car lst))))
          lst
          (loop (cdr lst)))))

  (define (span pred lst)
    (let loop ([lst lst] [acc '()])
      (if (or (null? lst) (not (pred (car lst))))
          (values (reverse acc) lst)
          (loop (cdr lst) (cons (car lst) acc)))))

  (define (span! pred lst)
    (span pred lst))

  (define (break pred lst)
    (span (lambda (x) (not (pred x))) lst))

  (define (break! pred lst)
    (break pred lst))

  ;; =========================================================
  ;; Deleting
  ;; =========================================================

  (define delete
    (case-lambda
      [(x lst) (delete x lst equal?)]
      [(x lst =) (filter (lambda (y) (not (= x y))) lst)]))

  (define delete!
    (case-lambda
      [(x lst) (delete x lst equal?)]
      [(x lst =) (delete x lst =)]))

  (define delete-duplicates
    (case-lambda
      [(lst) (delete-duplicates lst equal?)]
      [(lst =)
       (let loop ([lst lst] [acc '()])
         (if (null? lst) (reverse acc)
             (let ([x (car lst)])
               (if (any (lambda (y) (= x y)) acc)
                   (loop (cdr lst) acc)
                   (loop (cdr lst) (cons x acc))))))]))

  (define delete-duplicates!
    (case-lambda
      [(lst) (delete-duplicates lst equal?)]
      [(lst =) (delete-duplicates lst =)]))

  ;; =========================================================
  ;; Association lists
  ;; =========================================================

  (define assoc
    (case-lambda
      [(key alist) (assoc key alist equal?)]
      [(key alist =)
       (find (lambda (entry) (= key (car entry))) alist)]))

  (define (alist-cons key datum alist)
    (cons (cons key datum) alist))

  (define (alist-copy alist)
    (map (lambda (entry) (cons (car entry) (cdr entry))) alist))

  (define alist-delete
    (case-lambda
      [(key alist) (alist-delete key alist equal?)]
      [(key alist =)
       (filter (lambda (entry) (not (= key (car entry)))) alist)]))

  (define alist-delete!
    (case-lambda
      [(key alist) (alist-delete key alist equal?)]
      [(key alist =) (alist-delete key alist =)]))

  ;; =========================================================
  ;; Set operations on lists
  ;; =========================================================

  (define (lset<= = . lists)
    (or (null? lists)
        (null? (cdr lists))
        (let loop ([lists lists])
          (or (null? (cdr lists))
              (and (every (lambda (elt)
                            (any (lambda (x) (= elt x)) (cadr lists)))
                          (car lists))
                   (loop (cdr lists)))))))

  (define (lset= = . lists)
    (or (null? lists)
        (null? (cdr lists))
        (let loop ([lists lists])
          (or (null? (cdr lists))
              (and (lset<= = (car lists) (cadr lists))
                   (lset<= = (cadr lists) (car lists))
                   (loop (cdr lists)))))))

  (define (lset-adjoin = lst . elts)
    (fold (lambda (elt lst)
            (if (any (lambda (x) (= x elt)) lst)
                lst
                (cons elt lst)))
          lst elts))

  (define lset-union
    (case-lambda
      [(=) '()]
      [(= lst) lst]
      [(= lst . lists)
       (fold (lambda (lst2 result)
               (fold (lambda (elt result)
                       (if (any (lambda (x) (= x elt)) result)
                           result
                           (cons elt result)))
                     result lst2))
             lst lists)]))

  (define lset-union!
    (case-lambda
      [(=) '()]
      [(= lst) lst]
      [(= lst . lists) (apply lset-union = lst lists)]))

  (define (lset-intersection = lst . lists)
    (filter (lambda (elt)
              (every (lambda (lst2)
                       (any (lambda (x) (= elt x)) lst2))
                     lists))
            lst))

  (define (lset-intersection! = lst . lists)
    (apply lset-intersection = lst lists))

  (define (lset-difference = lst . lists)
    (filter (lambda (elt)
              (not (any (lambda (lst2)
                          (any (lambda (x) (= elt x)) lst2))
                        lists)))
            lst))

  (define (lset-difference! = lst . lists)
    (apply lset-difference = lst lists))

  (define (lset-xor = . lists)
    (reduce (lambda (lst2 result)
              (let ([remove-from-result
                     (filter (lambda (x)
                               (any (lambda (y) (= x y)) lst2))
                             result)])
                (append (lset-difference = result lst2)
                        (lset-difference = lst2 result))))
            '() lists))

  (define (lset-xor! = . lists)
    (apply lset-xor = lists))

  (define (lset-diff+intersection = lst . lists)
    (values (apply lset-difference = lst lists)
            (apply lset-intersection = lst lists)))

  (define (lset-diff+intersection! = lst . lists)
    (apply lset-diff+intersection = lst lists))

)
