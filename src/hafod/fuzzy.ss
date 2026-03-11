;;; (hafod fuzzy) -- FZF-style fuzzy matching engine
;;; Faithful port of junegunn/fzf's scoring and matching algorithms:
;;;   - V2 (Smith-Waterman DP) for optimal match quality
;;;   - V1 (greedy forward+backward) as fast fallback for large inputs
;;;   - calculateScore with proper consecutive bonus propagation
;;;   - Extended search syntax with OR groups (|), negation, exact/prefix/suffix
;;;   - Smart case sensitivity
;;; Copyright (c) 2026, hafod contributors.

(library (hafod fuzzy)
  (export fuzzy-match fuzzy-score fuzzy-filter
          fuzzy-filter/positions
          ;; Extended search syntax
          parse-search-pattern match-search-pattern filter-search-pattern
          ;; Search term record
          search-term? search-term-type search-term-negated?
          search-term-pattern search-term-case-sensitive?)
  (import (chezscheme))

  ;; ======================================================================
  ;; Scoring constants (identical to fzf defaults)
  ;; ======================================================================

  (define score-match         16)
  (define score-gap-start     -3)
  (define score-gap-extension -1)

  ;; Bonus values — derived exactly as in fzf's algo.go
  (define bonus-boundary            (fx/ score-match 2))           ; 8
  (define bonus-non-word            (fx/ score-match 2))           ; 8
  (define bonus-camel123            (fx+ bonus-boundary score-gap-extension))  ; 7
  (define bonus-consecutive         (fx- 0 (fx+ score-gap-start score-gap-extension)))  ; 4
  (define bonus-first-char-mult     2)
  (define bonus-boundary-white      (fx+ bonus-boundary 2))       ; 10
  (define bonus-boundary-delimiter  (fx+ bonus-boundary 1))       ; 9

  ;; ======================================================================
  ;; Character classification (matches fzf's charClass enum)
  ;; ======================================================================

  (define delimiter-chars "/,:;|")

  ;; Returns one of: white, non-word, delimiter, lower, upper, letter, number
  (define (char-class ch)
    (cond
      [(char-lower-case? ch) 'lower]
      [(char-upper-case? ch) 'upper]
      [(char-numeric? ch)    'number]
      [(char-whitespace? ch) 'white]
      [(let lp ([i (fx1- (string-length delimiter-chars))])
         (and (fx>= i 0)
              (or (char=? ch (string-ref delimiter-chars i))
                  (lp (fx1- i))))) 'delimiter]
      [(char-alphabetic? ch) 'letter]  ; non-ASCII letters
      [else                  'non-word]))

  ;; Bonus matrix: bonus-for(prevClass, class) — matches fzf's bonusFor exactly.
  (define (bonus-for prev-class class)
    (cond
      ;; Transition into a word character (lower/upper/letter/number)
      [(memq class '(lower upper number letter))
       (case prev-class
         [(white)     bonus-boundary-white]
         [(delimiter) bonus-boundary-delimiter]
         [(non-word)  bonus-boundary]
         [else
          (cond
            ;; camelCase: lower -> upper
            [(and (eq? prev-class 'lower) (eq? class 'upper)) bonus-camel123]
            ;; letter123: non-number -> number
            [(and (not (eq? prev-class 'number)) (eq? class 'number)) bonus-camel123]
            [else 0])])]
      ;; Non-word / delimiter characters
      [(memq class '(non-word delimiter)) bonus-non-word]
      [(eq? class 'white)                 bonus-boundary-white]
      [else 0]))

  ;; Precomputed bonus at position idx in text.
  ;; Position 0 is treated as if preceded by whitespace (fzf: initialCharClass = charWhite).
  (define (bonus-at text idx)
    (if (fx= idx 0)
        bonus-boundary-white
        (bonus-for (char-class (string-ref text (fx1- idx)))
                   (char-class (string-ref text idx)))))

  ;; ======================================================================
  ;; Unicode normalisation: map composed Latin characters to ASCII base
  ;; ======================================================================

  ;; fxvector covering U+00C0..U+017F.  Each slot holds the ASCII codepoint
  ;; of the base letter (e.g. é→e) or 0 if no mapping applies.
  (define *latin-normalise*
    (let ([v (make-fxvector 192 0)])  ; 0x17F - 0xC0 + 1 = 192
      (for-each
        (lambda (pair)
          (let ([from (car pair)] [to (cdr pair)])
            (fxvector-set! v (fx- from #xC0) (char->integer to))))
        '(;; Latin-1 Supplement (U+00C0..U+00FF)
          (#xC0 . #\a) (#xC1 . #\a) (#xC2 . #\a) (#xC3 . #\a) (#xC4 . #\a) (#xC5 . #\a)
          (#xC7 . #\c)
          (#xC8 . #\e) (#xC9 . #\e) (#xCA . #\e) (#xCB . #\e)
          (#xCC . #\i) (#xCD . #\i) (#xCE . #\i) (#xCF . #\i)
          (#xD0 . #\d) (#xD1 . #\n)
          (#xD2 . #\o) (#xD3 . #\o) (#xD4 . #\o) (#xD5 . #\o) (#xD6 . #\o) (#xD8 . #\o)
          (#xD9 . #\u) (#xDA . #\u) (#xDB . #\u) (#xDC . #\u)
          (#xDD . #\y) (#xDE . #\t)
          (#xDF . #\s)  ; ß → s
          (#xE0 . #\a) (#xE1 . #\a) (#xE2 . #\a) (#xE3 . #\a) (#xE4 . #\a) (#xE5 . #\a)
          (#xE7 . #\c)
          (#xE8 . #\e) (#xE9 . #\e) (#xEA . #\e) (#xEB . #\e)
          (#xEC . #\i) (#xED . #\i) (#xEE . #\i) (#xEF . #\i)
          (#xF0 . #\d) (#xF1 . #\n)
          (#xF2 . #\o) (#xF3 . #\o) (#xF4 . #\o) (#xF5 . #\o) (#xF6 . #\o) (#xF8 . #\o)
          (#xF9 . #\u) (#xFA . #\u) (#xFB . #\u) (#xFC . #\u)
          (#xFD . #\y) (#xFE . #\t) (#xFF . #\y)
          ;; Latin Extended-A (U+0100..U+017F)
          (#x100 . #\a) (#x101 . #\a) (#x102 . #\a) (#x103 . #\a) (#x104 . #\a) (#x105 . #\a)
          (#x106 . #\c) (#x107 . #\c) (#x108 . #\c) (#x109 . #\c) (#x10A . #\c) (#x10B . #\c)
          (#x10C . #\c) (#x10D . #\c)
          (#x10E . #\d) (#x10F . #\d) (#x110 . #\d) (#x111 . #\d)
          (#x112 . #\e) (#x113 . #\e) (#x114 . #\e) (#x115 . #\e) (#x116 . #\e) (#x117 . #\e)
          (#x118 . #\e) (#x119 . #\e) (#x11A . #\e) (#x11B . #\e)
          (#x11C . #\g) (#x11D . #\g) (#x11E . #\g) (#x11F . #\g) (#x120 . #\g) (#x121 . #\g)
          (#x122 . #\g) (#x123 . #\g)
          (#x124 . #\h) (#x125 . #\h) (#x126 . #\h) (#x127 . #\h)
          (#x128 . #\i) (#x129 . #\i) (#x12A . #\i) (#x12B . #\i) (#x12C . #\i) (#x12D . #\i)
          (#x12E . #\i) (#x12F . #\i) (#x130 . #\i) (#x131 . #\i)
          (#x134 . #\j) (#x135 . #\j)
          (#x136 . #\k) (#x137 . #\k)
          (#x139 . #\l) (#x13A . #\l) (#x13B . #\l) (#x13C . #\l) (#x13D . #\l) (#x13E . #\l)
          (#x141 . #\l) (#x142 . #\l)
          (#x143 . #\n) (#x144 . #\n) (#x145 . #\n) (#x146 . #\n) (#x147 . #\n) (#x148 . #\n)
          (#x14C . #\o) (#x14D . #\o) (#x14E . #\o) (#x14F . #\o) (#x150 . #\o) (#x151 . #\o)
          (#x154 . #\r) (#x155 . #\r) (#x156 . #\r) (#x157 . #\r) (#x158 . #\r) (#x159 . #\r)
          (#x15A . #\s) (#x15B . #\s) (#x15C . #\s) (#x15D . #\s) (#x15E . #\s) (#x15F . #\s)
          (#x160 . #\s) (#x161 . #\s)
          (#x162 . #\t) (#x163 . #\t) (#x164 . #\t) (#x165 . #\t) (#x166 . #\t) (#x167 . #\t)
          (#x168 . #\u) (#x169 . #\u) (#x16A . #\u) (#x16B . #\u) (#x16C . #\u) (#x16D . #\u)
          (#x16E . #\u) (#x16F . #\u) (#x170 . #\u) (#x171 . #\u) (#x172 . #\u) (#x173 . #\u)
          (#x174 . #\w) (#x175 . #\w)
          (#x176 . #\y) (#x177 . #\y) (#x178 . #\y)
          (#x179 . #\z) (#x17A . #\z) (#x17B . #\z) (#x17C . #\z) (#x17D . #\z) (#x17E . #\z)))
      v))

  ;; Normalise a character: strip diacritics from Latin letters, then optionally downcase.
  ;; Returns the normalised char.
  (define (char-normalise ch)
    (let ([cp (char->integer ch)])
      (if (and (fx>= cp #xC0) (fx<= cp #x17F))
          (let ([mapped (fxvector-ref *latin-normalise* (fx- cp #xC0))])
            (if (fx> mapped 0) (integer->char mapped) ch))
          ch)))

  ;; ======================================================================
  ;; Smart case sensitivity
  ;; ======================================================================

  (define (all-lower? s)
    (let ([len (string-length s)])
      (let lp ([i 0])
        (or (fx>= i len)
            (and (not (char-upper-case? (string-ref s i)))
                 (lp (fx1+ i)))))))

  (define-syntax char=?/ci
    (syntax-rules ()
      [(_ a b cs?)
       (if cs? (char=? a b)
           (char=? (char-downcase (char-normalise a))
                   (char-downcase (char-normalise b))))]))

  (define (char-downcase-if ch cs?)
    (if cs? ch (char-downcase (char-normalise ch))))

  ;; ======================================================================
  ;; calculateScore — faithful port of fzf's calculateScore function.
  ;; Given text, pattern, and a match window [sidx, eidx), computes the
  ;; score and match positions using proper consecutive bonus propagation.
  ;; Returns (score . positions).
  ;; ======================================================================

  (define (calculate-score text pattern sidx eidx case-sensitive?)
    (let ([plen (string-length pattern)])
      (let lp ([idx sidx]
               [pidx 0]
               [score 0]
               [in-gap? #f]
               [consecutive 0]
               [first-bonus 0]
               [positions '()])
        (cond
          [(fx>= idx eidx) (cons score (reverse positions))]
          [else
           (let* ([ch (char-downcase-if (string-ref text idx) case-sensitive?)]
                  [pch (string-ref pattern pidx)]
                  [prev-class (if (fx= idx 0) 'white
                                  (char-class (string-ref text (fx1- idx))))]
                  [class (char-class (string-ref text idx))])
             (cond
               [(char=? ch pch)
                ;; Match
                (let* ([bonus (bonus-for prev-class class)]
                       [new-first-bonus
                        (cond
                          [(fx= consecutive 0) bonus]
                          ;; Break consecutive chunk if this boundary is stronger
                          [(and (fx>= bonus bonus-boundary) (fx> bonus first-bonus))
                           bonus]
                          [else first-bonus])]
                       [effective-bonus
                        (if (fx= consecutive 0)
                            bonus
                            (fxmax bonus (fxmax bonus-consecutive new-first-bonus)))]
                       [char-score
                        (fx+ score-match
                             (if (fx= pidx 0)
                                 (fx* effective-bonus bonus-first-char-mult)
                                 effective-bonus))])
                  (lp (fx1+ idx)
                      (fx1+ pidx)
                      (fx+ score char-score)
                      #f
                      (fx1+ consecutive)
                      new-first-bonus
                      (cons idx positions)))]
               [else
                ;; Gap
                (let ([penalty (if in-gap? score-gap-extension score-gap-start)])
                  (lp (fx1+ idx)
                      pidx
                      (fx+ score penalty)
                      #t
                      0
                      0
                      positions))]))]))))

  ;; ======================================================================
  ;; V1 algorithm: greedy forward + backward scan, then calculateScore.
  ;; Fast O(n) per candidate. Used as fallback for large inputs.
  ;; ======================================================================

  (define (fuzzy-match-v1 pattern text case-sensitive?)
    (let ([plen (string-length pattern)]
          [tlen (string-length text)])
      ;; Phase 1: Forward scan — find first occurrence of all pattern chars
      (let fwd ([ti 0] [pi 0] [sidx -1])
        (cond
          [(fx= pi plen)
           ;; eidx = ti (one past last matched char)
           (let ([eidx ti])
             ;; Phase 2: Backward scan — shrink window from the right
             (let bwd ([ti (fx1- eidx)] [pi (fx1- plen)])
               (cond
                 [(fx< pi 0)
                  ;; [ti+1, eidx) is the tightest window
                  (calculate-score text pattern (fx1+ ti) eidx case-sensitive?)]
                 [(char=?/ci (string-ref text ti) (string-ref pattern pi) case-sensitive?)
                  (bwd (fx1- ti) (fx1- pi))]
                 [else
                  (bwd (fx1- ti) pi)])))]
          [(fx>= ti tlen) #f]
          [(char=?/ci (string-ref text ti) (string-ref pattern pi) case-sensitive?)
           (fwd (fx1+ ti) (fx1+ pi) (if (fx= pi 0) ti sidx))]
          [else
           (fwd (fx1+ ti) pi sidx)]))))

  ;; ======================================================================
  ;; V2 algorithm: Smith-Waterman DP for optimal scoring alignment.
  ;; O(nm) where n=text length, m=pattern length.
  ;; Faithful port of fzf's FuzzyMatchV2.
  ;; ======================================================================

  (define (fuzzy-match-v2 pattern text case-sensitive?)
    (let* ([M (string-length pattern)]
           [N (string-length text)])

      ;; Phase 1: Quick check — can all pattern chars be found?
      ;; Also record first occurrence of each pattern char (F vector)
      ;; and the last matched text index.
      (let* ([F (make-fxvector M 0)]  ; first occurrence of pattern[i]
             [B (make-fxvector N 0)]   ; bonus at each text position
             [T (make-string N)])      ; normalised text (lowered if case-insensitive)

        ;; Build T, B, F, verify all pattern chars present.
        ;; F[i] = first sequential occurrence of pattern[i].
        ;; last-idx = last text position matching the current pattern char
        ;; (extends past F to catch later occurrences for DP bounds).
        (let build ([ti 0] [pi 0] [last-idx 0] [prev-class 'white]
                    [pchar (string-ref pattern 0)])
          (cond
            [(fx>= ti N)
             (if (fx< pi M)
                 #f  ; not all pattern chars found
                 (if (fx= M 1)
                     (fuzzy-v2-single-char T B N pattern case-sensitive?)
                     (fuzzy-v2-dp T B F N M last-idx pattern text case-sensitive?)))]
            [else
             (let* ([raw-ch (string-ref text ti)]
                    [class (char-class raw-ch)]
                    [ch (char-downcase-if raw-ch case-sensitive?)]
                    [bonus (bonus-for prev-class class)])
               (string-set! T ti ch)
               (fxvector-set! B ti bonus)
               (if (char=? ch pchar)
                   (let* ([new-pi (if (fx< pi M)
                                      (begin (fxvector-set! F pi ti) (fx1+ pi))
                                      pi)]
                          [new-pchar (string-ref pattern (fxmin new-pi (fx1- M)))])
                     (build (fx1+ ti) new-pi ti class new-pchar))
                   (build (fx1+ ti) pi last-idx class pchar)))])))))


  ;; V2 single-char: find best-scoring position of the single pattern char.
  (define (fuzzy-v2-single-char T B N pattern case-sensitive?)
    (let ([pch (string-ref pattern 0)])
      (let lp ([ti 0] [best-score 0] [best-pos -1])
        (cond
          [(fx>= ti N)
           (if (fx< best-pos 0)
               #f
               (cons best-score (list best-pos)))]
          [(char=? (string-ref T ti) pch)
           (let ([s (fx+ score-match (fx* (fxvector-ref B ti) bonus-first-char-mult))])
             (if (fx> s best-score)
                 (if (fx>= (fxvector-ref B ti) bonus-boundary)
                     ;; Early exit: boundary match for first char is optimal
                     (cons s (list ti))
                     (lp (fx1+ ti) s ti))
                 (lp (fx1+ ti) best-score best-pos)))]
          [else (lp (fx1+ ti) best-score best-pos)]))))

  ;; V2 DP core.
  ;; H[i][j] = best score for matching pattern[0..i] against text[0..j]
  ;; C[i][j] = length of consecutive match ending at (i,j)
  ;; We only need the sub-matrix from F[0]..lastIdx columns.
  (define (fuzzy-v2-dp T B F N M last-idx pattern text case-sensitive?)
    (let* ([f0 (fxvector-ref F 0)]
           [width (fx+ (fx- last-idx f0) 1)]
           ;; H and C matrices: M rows x width columns, stored as fxvectors
           [H (make-fxvector (fx* M width) 0)]
           [C (make-fxvector (fx* M width) 0)])

      ;; Fill row 0 (first pattern character)
      (let fill-row0 ([col f0] [prev-h 0] [in-gap? #f])
        (when (fx<= col last-idx)
          (let ([j (fx- col f0)])  ; column index in matrix
            (if (char=? (string-ref T col) (string-ref pattern 0))
                (let ([s (fx+ score-match
                              (fx* (fxvector-ref B col) bonus-first-char-mult))])
                  (fxvector-set! H j s)
                  (fxvector-set! C j 1)
                  (fill-row0 (fx1+ col) s #f))
                (let ([s (fxmax 0 (fx+ prev-h (if in-gap?
                                                   score-gap-extension
                                                   score-gap-start)))])
                  (fxvector-set! H j s)
                  (fxvector-set! C j 0)
                  (fill-row0 (fx1+ col) s #t))))))

      ;; Fill rows 1..M-1
      (let row-loop ([pi 1])
        (when (fx< pi M)
          (let* ([row-off (fx* pi width)]
                 [prev-row-off (fx* (fx1- pi) width)]
                 [fi (fxvector-ref F pi)]
                 [pch (string-ref pattern pi)])
            (let col-loop ([col fi] [in-gap? #f])
              (when (fx<= col last-idx)
                (let* ([j (fx- col f0)]
                       [j-1 (fx1- j)]
                       ;; s1: diagonal score (match)
                       ;; s2: left score (gap)
                       [s2 (if (fx> j 0)
                               (let ([h-left (fxvector-ref H (fx+ row-off j-1))])
                                 (fx+ h-left (if in-gap?
                                                  score-gap-extension
                                                  score-gap-start)))
                               -10000)]
                       [s1+consec
                        (if (char=? (string-ref T col) pch)
                            (let* ([h-diag (if (and (fx> j 0) (fx> pi 0))
                                               (fxvector-ref H (fx+ prev-row-off j-1))
                                               0)]
                                   [s1-base (fx+ h-diag score-match)]
                                   [b (fxvector-ref B col)]
                                   [c-diag (if (and (fx> j 0) (fx> pi 0))
                                               (fxvector-ref C (fx+ prev-row-off j-1))
                                               0)]
                                   [consec (fx1+ c-diag)])
                              ;; Consecutive bonus propagation
                              (if (fx> consec 1)
                                  (let ([fb (fxvector-ref B (fx+ 1 (fx- col consec)))])
                                    (if (and (fx>= b bonus-boundary) (fx> b fb))
                                        ;; Break: restart consecutive run
                                        (let ([eff-b b])
                                          (if (fx< (fx+ s1-base eff-b) s2)
                                              (cons (fx+ s1-base (fxvector-ref B col))
                                                    0)
                                              (cons (fx+ s1-base eff-b) 1)))
                                        ;; Continue run
                                        (let ([eff-b (fxmax b (fxmax bonus-consecutive fb))])
                                          (if (fx< (fx+ s1-base eff-b) s2)
                                              (cons (fx+ s1-base (fxvector-ref B col))
                                                    0)
                                              (cons (fx+ s1-base eff-b) consec)))))
                                  ;; First in potential run
                                  (let ([eff-b b])
                                    (if (fx< (fx+ s1-base eff-b) s2)
                                        (cons (fx+ s1-base (fxvector-ref B col)) 0)
                                        (cons (fx+ s1-base eff-b) consec)))))
                            (cons -10000 0))]
                       [s1 (car s1+consec)]
                       [consec (cdr s1+consec)]
                       [new-in-gap? (fx< s1 s2)]
                       [score (fxmax (fxmax s1 s2) 0)])
                  (fxvector-set! H (fx+ row-off j) score)
                  (fxvector-set! C (fx+ row-off j)
                    (if new-in-gap? 0 consec))
                  (col-loop (fx1+ col) new-in-gap?)))))
          (row-loop (fx1+ pi))))

      ;; Find maximum score in last row
      (let* ([last-row-off (fx* (fx1- M) width)]
             [fi-last (fxvector-ref F (fx1- M))])
        (let find-max ([col (fx- fi-last f0)] [best-score 0] [best-col (fx- fi-last f0)])
          (cond
            [(fx> col (fx- last-idx f0))
             (if (fx= best-score 0)
                 #f  ; shouldn't happen if Phase 1 passed
                 ;; Phase 4: Backtrace to find positions
                 (fuzzy-v2-backtrace H C F f0 width M best-col))]
            [else
             (let ([s (fxvector-ref H (fx+ last-row-off col))])
               (if (fx> s best-score)
                   (find-max (fx1+ col) s col)
                   (find-max (fx1+ col) best-score best-col)))])))))

  ;; V2 backtrace: walk backward through H to recover matched positions.
  (define (fuzzy-v2-backtrace H C F f0 width M best-col)
    (let lp ([i (fx1- M)] [j best-col] [prefer-match? #t] [positions '()])
      (cond
        [(fx< i 0)
         ;; Score is H[M-1][best-col]
         (let ([score (fxvector-ref H (fx+ (fx* (fx1- M) width) best-col))])
           (cons score positions))]
        [else
         (let* ([I (fx* i width)]
                [s (fxvector-ref H (fx+ I j))]
                ;; Diagonal score (from previous row, previous column)
                [s1 (if (and (fx> i 0) (fx>= (fx1- j) (fx- (fxvector-ref F i) f0)))
                        (fxvector-ref H (fx+ (fx* (fx1- i) width) (fx1- j)))
                        0)]
                ;; Left score (same row, previous column)
                [s2 (if (fx> j (fx- (fxvector-ref F i) f0))
                        (fxvector-ref H (fx+ I (fx1- j)))
                        0)])
           (cond
             [(and (fx> s s1) (or (fx> s s2)
                                   (and (fx= s s2) prefer-match?)))
              ;; This cell was a match — record position
              (let ([new-prefer
                     (or (fx> (fxvector-ref C (fx+ I j)) 1)
                         (and (fx< (fx+ I width j 1) (fx* M width))
                              (fx> (fxvector-ref C (fx+ I width j 1)) 0)))])
                (lp (fx1- i) (fx1- j) new-prefer (cons (fx+ j f0) positions)))]
             [else
              ;; Gap — move left
              (lp i (fx1- j) prefer-match? positions)]))])))

  ;; ======================================================================
  ;; Public API: fuzzy-match
  ;; Returns (score . positions) or #f.
  ;; Uses V2 for short inputs, falls back to V1 for large N*M.
  ;; ======================================================================

  (define (fuzzy-match pattern text)
    (let* ([plen (string-length pattern)]
           [tlen (string-length text)]
           [case-sensitive? (not (all-lower? pattern))])
      (cond
        [(fx= plen 0) (cons 0 '())]
        [(fx> plen tlen) #f]
        ;; Fall back to V1 for very large inputs or very long patterns
        [(or (fx> (fx* plen tlen) 65536) (fx> plen 1000))
         (fuzzy-match-v1 pattern text case-sensitive?)]
        [else
         (fuzzy-match-v2 pattern text case-sensitive?)])))

  ;; ======================================================================
  ;; Convenience: score only
  ;; ======================================================================

  (define (fuzzy-score pattern text)
    (let ([result (fuzzy-match pattern text)])
      (if result (car result) #f)))

  ;; ======================================================================
  ;; Filter and sort candidates by fuzzy score (descending).
  ;; ======================================================================

  ;; Tiebreak comparator: higher score first, then shorter candidate, then
  ;; earlier first-match position (approximated by candidate order for filter).
  (define (score-tiebreak a b)
    (let ([sa (car a)] [sb (car b)])
      (cond
        [(fx> sa sb) #t]
        [(fx< sa sb) #f]
        ;; Equal scores: prefer shorter candidate
        [else
         (let ([la (string-length (cadr a))]
               [lb (string-length (cadr b))])
           (cond
             [(fx< la lb) #t]
             [(fx> la lb) #f]
             ;; Equal length: preserve original order (stable)
             [else #f]))])))

  (define (fuzzy-filter pattern candidates)
    (if (string=? pattern "")
        candidates
        (let* ([scored (filter-map
                         (lambda (c)
                           (let ([result (fuzzy-match pattern c)])
                             (and result (list (car result) c))))
                         candidates)]
               [sorted (list-sort score-tiebreak scored)])
          (map cadr sorted))))

  ;; Like fuzzy-filter but returns (candidate . positions) pairs.
  (define (fuzzy-filter/positions pattern candidates)
    (if (string=? pattern "")
        (map (lambda (c) (cons c '())) candidates)
        (let* ([scored (filter-map
                         (lambda (c)
                           (let ([result (fuzzy-match pattern c)])
                             (and result
                                  (list (car result) c (cdr result)))))
                         candidates)]
               [sorted (list-sort score-tiebreak scored)])
          (map (lambda (entry) (cons (cadr entry) (caddr entry))) sorted))))

  (define (filter-map f lst)
    (let lp ([l lst] [acc '()])
      (cond
        [(null? l) (reverse acc)]
        [else
         (let ([v (f (car l))])
           (lp (cdr l) (if v (cons v acc) acc)))])))

  ;; ======================================================================
  ;; Extended search syntax (fzf-style)
  ;; ======================================================================
  ;; Structure: list of term-sets. Term-sets are ANDed. Within a term-set,
  ;; terms are ORed (separated by | in input).
  ;;
  ;; Token syntax:
  ;;   foo     — fuzzy match
  ;;   'foo    — exact substring
  ;;   ^foo    — prefix
  ;;   foo$    — suffix
  ;;   ^foo$   — equal (prefix + suffix)
  ;;   !foo    — negated exact (fzf: ! flips to exact)
  ;;   !'foo   — negated fuzzy (fzf: !' flips back to fuzzy)
  ;;   !^foo   — negated prefix
  ;;   !foo$   — negated suffix
  ;;   |       — OR (between adjacent terms in the same group)
  ;;
  ;; Each term: #(type negated? pattern case-sensitive?)
  ;; type: fuzzy | exact | prefix | suffix | equal

  (define-record-type search-term
    (fields type negated? pattern case-sensitive?))

  (define (parse-token-str tok)
    (let* ([len (string-length tok)]
           [negated? (and (fx> len 0) (char=? (string-ref tok 0) #\!))]
           [rest (if negated? (substring tok 1 len) tok)]
           [rlen (string-length rest)]
           [cs? #f]  ; will be determined from smart-case
           ;; In fzf: ! makes it exact, !' makes it fuzzy
           [base-type (if negated? 'exact 'fuzzy)])
      (cond
        [(fx= rlen 0) #f]
        [else
         (let-values ([(type text)
                       (let* ([has-suffix? (and (not (string=? rest "$"))
                                                (fx> rlen 1)
                                                (char=? (string-ref rest (fx1- rlen)) #\$))]
                              [r1 (if has-suffix?
                                      (substring rest 0 (fx1- rlen))
                                      rest)]
                              [r1len (string-length r1)])
                         (cond
                           ;; 'exact' (with matching quotes) -> exact-boundary
                           [(and (fx> r1len 2)
                                 (char=? (string-ref r1 0) #\')
                                 (char=? (string-ref r1 (fx1- r1len)) #\'))
                            (values 'exact (substring r1 1 (fx1- r1len)))]
                           ;; 'exact
                           [(char=? (string-ref r1 0) #\')
                            (values (if (and (not negated?) (eq? base-type 'fuzzy))
                                        'exact  ; ' flips fuzzy -> exact
                                        'fuzzy) ; !' flips exact -> fuzzy
                                    (substring r1 1 r1len))]
                           ;; ^prefix
                           [(char=? (string-ref r1 0) #\^)
                            (values (if has-suffix? 'equal 'prefix)
                                    (substring r1 1 r1len))]
                           ;; suffix$ (already stripped $)
                           [has-suffix?
                            (values 'suffix r1)]
                           ;; default
                           [else
                            (values base-type r1)]))])
           (if (fx= (string-length text) 0)
               #f
               (let* ([lower-text (string-downcase text)]
                      [case-sensitive? (not (string=? text lower-text))]
                      [norm-text (if case-sensitive? text lower-text)])
                 (make-search-term type negated? norm-text case-sensitive?))))])))

  ;; Parse a full pattern string into a list of term-sets.
  ;; Each term-set is a list of search-term records (ORed).
  ;; Term-sets are ANDed.
  (define (parse-search-pattern str)
    (let* ([tokens (string-split str)]
           [sets '()]
           [current-set '()]
           [after-bar? #f])
      (let lp ([toks tokens] [sets '()] [current '()] [after-bar? #f])
        (cond
          [(null? toks)
           (let ([final-sets (if (null? current) sets (cons (reverse current) sets))])
             (reverse final-sets))]
          [(string=? (car toks) "|")
           (lp (cdr toks) sets current #t)]
          [else
           (let ([term (parse-token-str (car toks))])
             (cond
               [(not term)
                (lp (cdr toks) sets current #f)]
               [after-bar?
                ;; OR: add to current term-set
                (lp (cdr toks) sets (cons term current) #f)]
               [(null? current)
                ;; Start new term-set
                (lp (cdr toks) sets (list term) #f)]
               [else
                ;; AND: close current set, start new one
                (lp (cdr toks)
                    (cons (reverse current) sets)
                    (list term)
                    #f)]))]))))

  ;; Split string by whitespace. Handles escaped spaces (\ ).
  ;; Returns list of non-empty token strings.
  (define (string-split s)
    (let ([len (string-length s)])
      ;; First replace escaped spaces with a sentinel, split, then restore.
      ;; Simpler: accumulate chars into current token.
      (let lp ([i 0] [cur '()] [acc '()])
        (cond
          [(fx>= i len)
           (reverse (if (null? cur) acc
                        (cons (list->string (reverse cur)) acc)))]
          ;; Escaped space: \ followed by space
          [(and (char=? (string-ref s i) #\\)
                (fx< (fx1+ i) len)
                (char=? (string-ref s (fx1+ i)) #\space))
           (lp (fx+ i 2) (cons #\space cur) acc)]
          ;; Whitespace separator
          [(char-whitespace? (string-ref s i))
           (lp (fx1+ i) '()
               (if (null? cur) acc
                   (cons (list->string (reverse cur)) acc)))]
          ;; Regular character
          [else
           (lp (fx1+ i) (cons (string-ref s i) cur) acc)]))))

  ;; ======================================================================
  ;; Term matching — each term type has its own matcher
  ;; ======================================================================

  ;; Exact match: find best-scoring occurrence of needle in haystack.
  ;; Returns (score . positions) or #f.
  (define (exact-match-scored haystack needle cs?)
    (let ([hlen (string-length haystack)]
          [nlen (string-length needle)])
      (if (fx> nlen hlen) #f
          (let lp ([i 0] [best-score -1] [best-pos -1])
            (cond
              [(fx> (fx+ i nlen) hlen)
               (if (fx< best-pos 0) #f
                   (let ([positions (let build ([k 0] [acc '()])
                                     (if (fx= k nlen) (reverse acc)
                                         (build (fx1+ k) (cons (fx+ best-pos k) acc))))])
                     (calculate-score haystack needle best-pos (fx+ best-pos nlen) cs?)))]
              [(let inner ([j 0])
                 (or (fx= j nlen)
                     (and (char=?/ci (string-ref haystack (fx+ i j))
                                     (string-ref needle j)
                                     cs?)
                          (inner (fx1+ j)))))
               ;; Found a match at position i — check bonus
               (let ([bonus (bonus-at haystack i)])
                 (cond
                   [(fx>= bonus bonus-boundary)
                    ;; Boundary match — this is optimal, stop
                    (calculate-score haystack needle i (fx+ i nlen) cs?)]
                   [(fx> bonus best-score)
                    (lp (fx1+ i) bonus i)]
                   [else (lp (fx1+ i) best-score best-pos)]))]
              [else (lp (fx1+ i) best-score best-pos)])))))

  ;; Prefix match with scoring.
  (define (prefix-match-scored haystack needle cs?)
    (let ([hlen (string-length haystack)]
          [nlen (string-length needle)])
      ;; Skip leading whitespace (fzf behaviour) unless pattern starts with space
      (let ([trimmed (if (and (fx> nlen 0) (not (char-whitespace? (string-ref needle 0))))
                         (let lp ([i 0])
                           (if (or (fx>= i hlen) (not (char-whitespace? (string-ref haystack i))))
                               i (lp (fx1+ i))))
                         0)])
        (and (fx<= nlen (fx- hlen trimmed))
             (let check ([j 0])
               (cond
                 [(fx= j nlen)
                  (calculate-score haystack needle trimmed (fx+ trimmed nlen) cs?)]
                 [(char=?/ci (string-ref haystack (fx+ trimmed j))
                             (string-ref needle j) cs?)
                  (check (fx1+ j))]
                 [else #f]))))))

  ;; Suffix match with scoring.
  (define (suffix-match-scored haystack needle cs?)
    (let* ([hlen (string-length haystack)]
           [nlen (string-length needle)]
           ;; Trim trailing whitespace (fzf behaviour)
           [trimmed-end (if (and (fx> nlen 0)
                                  (not (char-whitespace? (string-ref needle (fx1- nlen)))))
                            (let lp ([i (fx1- hlen)])
                              (if (or (fx< i 0) (not (char-whitespace? (string-ref haystack i))))
                                  (fx1+ i) (lp (fx1- i))))
                            hlen)]
           [start (fx- trimmed-end nlen)])
      (and (fx>= start 0)
           (let check ([j 0])
             (cond
               [(fx= j nlen)
                (calculate-score haystack needle start trimmed-end cs?)]
               [(char=?/ci (string-ref haystack (fx+ start j))
                           (string-ref needle j) cs?)
                (check (fx1+ j))]
               [else #f])))))

  ;; Equal match (^...$): entire string must match.
  (define (equal-match-scored haystack needle cs?)
    (let ([hlen (string-length haystack)]
          [nlen (string-length needle)])
      ;; Trim leading and trailing whitespace
      (let* ([trimmed-start (let lp ([i 0])
                              (if (or (fx>= i hlen) (not (char-whitespace? (string-ref haystack i))))
                                  i (lp (fx1+ i))))]
             [trimmed-end (let lp ([i (fx1- hlen)])
                            (if (or (fx< i 0) (not (char-whitespace? (string-ref haystack i))))
                                (fx1+ i) (lp (fx1- i))))]
             [trimmed-len (fx- trimmed-end trimmed-start)])
        (and (fx= nlen trimmed-len)
             (let check ([j 0])
               (cond
                 [(fx= j nlen)
                  (calculate-score haystack needle trimmed-start trimmed-end cs?)]
                 [(char=?/ci (string-ref haystack (fx+ trimmed-start j))
                             (string-ref needle j) cs?)
                  (check (fx1+ j))]
                 [else #f]))))))

  ;; Run a single term against text. Returns (score . positions) or #f.
  (define (term-match term text)
    (let ([type (search-term-type term)]
          [pat (search-term-pattern term)]
          [cs? (search-term-case-sensitive? term)])
      (case type
        [(fuzzy)  (let* ([p (if cs? pat pat)]
                         [result (fuzzy-match-internal pat text cs?)])
                    result)]
        [(exact)  (exact-match-scored text pat cs?)]
        [(prefix) (prefix-match-scored text pat cs?)]
        [(suffix) (suffix-match-scored text pat cs?)]
        [(equal)  (equal-match-scored text pat cs?)]
        [else #f])))

  ;; Internal fuzzy match with explicit case-sensitivity.
  (define (fuzzy-match-internal pattern text case-sensitive?)
    (let ([plen (string-length pattern)]
          [tlen (string-length text)])
      (cond
        [(fx= plen 0) (cons 0 '())]
        [(fx> plen tlen) #f]
        [(or (fx> (fx* plen tlen) 65536) (fx> plen 1000))
         (fuzzy-match-v1 pattern text case-sensitive?)]
        [else
         (fuzzy-match-v2 pattern text case-sensitive?)])))

  ;; Test whether a term-set (OR group) matches text.
  ;; Returns (score . positions) for the first matching non-inverted term,
  ;; or (0 . ()) for a matching inverted term, or #f if no match.
  (define (term-set-match term-set text)
    (let lp ([terms term-set])
      (cond
        [(null? terms) #f]
        [else
         (let* ([term (car terms)]
                [result (term-match term text)])
           (cond
             [(and result (not (search-term-negated? term)))
              result]  ; positive match found
             [(and (not result) (search-term-negated? term))
              (cons 0 '())]  ; negated term: no match means success
             [else (lp (cdr terms))]))])))

  ;; Match all term-sets (AND semantics). Returns total score and merged positions, or #f.
  (define (match-search-pattern term-sets text)
    (let lp ([sets term-sets] [total-score 0] [all-positions '()])
      (cond
        [(null? sets) (cons total-score all-positions)]
        [else
         (let ([result (term-set-match (car sets) text)])
           (if result
               (lp (cdr sets)
                   (fx+ total-score (car result))
                   (append all-positions (cdr result)))
               #f))])))

  ;; Filter candidates using extended search syntax.
  ;; Returns candidates sorted by score (descending).
  (define (filter-search-pattern pattern-str candidates)
    (let ([term-sets (parse-search-pattern pattern-str)])
      (if (null? term-sets)
          candidates
          (let* ([scored (filter-map
                           (lambda (c)
                             (let ([result (match-search-pattern term-sets c)])
                               (and result (list (car result) c))))
                           candidates)]
                 [sorted (list-sort score-tiebreak scored)])
            (map cadr sorted)))))

) ; end library
