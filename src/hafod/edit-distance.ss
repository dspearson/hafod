;;; (hafod edit-distance) -- Bounded transposition-aware string distance
;;; Optimal-string-alignment distance (Levenshtein plus adjacent transposition),
;;; with a max-distance ceiling that short-circuits the fill once a whole row has
;;; drifted past the cap. Both inputs are folded to lower case up front, so the
;;; comparison is case-insensitive -- correction measures a typed word against a
;;; fixed set of command names. Imports (chezscheme) only: a pure leaf, cycle-free
;;; by construction, and deliberately NOT a member of the (hafod) umbrella.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod edit-distance)
  (export damerau-levenshtein)
  (import (chezscheme))

  ;; === Bounded transposition-aware string distance ===

  ;; Optimal-string-alignment distance between A and B, capped at MAX-DISTANCE.
  ;;
  ;; Returns the exact edit count when it is <= MAX-DISTANCE, otherwise the
  ;; sentinel (+ MAX-DISTANCE 1) meaning "at least that far" -- i.e. the result
  ;; is always (min actual (+ max-distance 1)). The four edit operations each
  ;; cost 1: insert, delete, substitute, and the transposition of two ADJACENT
  ;; characters (so "gti" is distance 1 from "git", which plain Levenshtein
  ;; scores 2). This is the optimal-string-alignment recurrence, not the full
  ;; unrestricted Damerau-Levenshtein -- the latter needs a whole-alphabet last
  ;; occurrence map and buys nothing for short command names.
  ;;
  ;; Two clauses: the 2-argument form measures the true distance (its ceiling is
  ;; the larger length, which the distance can never exceed, so the cap never
  ;; fires); the 3-argument form takes an explicit ceiling and stops early.
  (define damerau-levenshtein
    (case-lambda
      [(a b)
       ;; The distance between two strings is never more than the longer length
       ;; (align the common positions, then insert or delete the tail), so this
       ;; ceiling leaves the exact count untouched -- effectively unbounded.
       (damerau-levenshtein a b (fxmax (string-length a) (string-length b)))]
      [(a b max-distance)
       ;; Fold case once, up front: correction is case-insensitive and the
       ;; command names are the fixed side, so "GIT" and "git" measure as equal.
       (let* ([a  (string-downcase a)]
              [b  (string-downcase b)]
              [la (string-length a)]
              [lb (string-length b)]
              [ceiling (fx+ max-distance 1)])
         (cond
           ;; Empty-string edges: turning "" into an n-character string (or the
           ;; reverse) costs n inserts (or n deletes), still capped at the ceiling.
           [(fx= la 0) (fxmin lb ceiling)]
           [(fx= lb 0) (fxmin la ceiling)]
           [else
            ;; Three rolling rows -- d[i-2], d[i-1] and the row being filled.
            ;; Two would suffice for plain Levenshtein, but the transposition
            ;; term reads d[i-2][j-2], so the row two back must survive as well.
            ;; The three fixed buffers are rotated (never reallocated) per row.
            (let ([prev2 (make-fxvector (fx1+ lb) 0)]   ; d[i-2]
                  [prev1 (make-fxvector (fx1+ lb) 0)]   ; d[i-1]
                  [cur   (make-fxvector (fx1+ lb) 0)])  ; d[i], filled below
              ;; Row 0: the distance from "" to the j-character prefix of b is j.
              (let fill0 ([j 0])
                (when (fx<= j lb)
                  (fxvector-set! prev1 j j)
                  (fill0 (fx1+ j))))
              (let row ([i 1])
                (cond
                  ;; Every row filled -- the answer sits in the last completed
                  ;; row (now prev1) at column lb, capped at the ceiling.
                  [(fx> i la) (fxmin (fxvector-ref prev1 lb) ceiling)]
                  [else
                   ;; Column 0: deleting the whole i-character prefix of a.
                   (fxvector-set! cur 0 i)
                   (let ([ai   (string-ref a (fx1- i))]
                         [ai-1 (if (fx>= i 2) (string-ref a (fx- i 2)) #\nul)])
                     (let col ([j 1] [row-min i])
                       (cond
                         [(fx> j lb)
                          ;; The row minimum never decreases as i grows, so once
                          ;; a whole row has passed the cap the final cell must
                          ;; too -- stop now and hand back the sentinel rather
                          ;; than finish an N*M fill for a hopeless pair.
                          (if (fx> row-min max-distance)
                              ceiling
                              ;; Rotate: d[i-2] <- d[i-1], d[i-1] <- d[i], and
                              ;; recycle the retired d[i-2] buffer as the next row.
                              (let ([recycled prev2])
                                (set! prev2 prev1)
                                (set! prev1 cur)
                                (set! cur recycled)
                                (row (fx1+ i))))]
                         [else
                          (let* ([bj   (string-ref b (fx1- j))]
                                 [cost (if (char=? ai bj) 0 1)]
                                 [del  (fx1+ (fxvector-ref prev1 j))]
                                 [ins  (fx1+ (fxvector-ref cur (fx1- j)))]
                                 [sub  (fx+ (fxvector-ref prev1 (fx1- j)) cost)]
                                 [best (fxmin del ins sub)]
                                 ;; Adjacent transposition: a[i-1] a[i-2] lines up
                                 ;; with b[j-2] b[j-1], so swapping the two costs
                                 ;; d[i-2][j-2] + 1.
                                 [best (if (and (fx>= i 2) (fx>= j 2)
                                                (char=? ai (string-ref b (fx- j 2)))
                                                (char=? ai-1 bj))
                                           (fxmin best
                                                  (fx1+ (fxvector-ref prev2 (fx- j 2))))
                                           best)])
                            (fxvector-set! cur j best)
                            (col (fx1+ j) (fxmin row-min best)))])))])))]))]))

  ) ; end library
