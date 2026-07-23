(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod edit-distance)
        (hafod fuzzy))

(test-begin "edit-distance")

;; ======================================================================
;; Exact small-edit distances -- one edit of each kind scores 1.
;; ======================================================================

(test-equal "identical strings measure zero"
  0 (damerau-levenshtein "git" "git"))

(test-equal "an adjacent transposition costs one (gti -> git)"
  1 (damerau-levenshtein "gti" "git"))

(test-equal "an adjacent transposition costs one (pyhton -> python)"
  1 (damerau-levenshtein "pyhton" "python"))

(test-equal "a single insertion costs one (gt -> git)"
  1 (damerau-levenshtein "gt" "git"))

(test-equal "a single substitution costs one (got -> git)"
  1 (damerau-levenshtein "got" "git"))

;; Empty-string edges: the distance is the other string's length.
(test-equal "an empty source measures the target length"
  3 (damerau-levenshtein "" "git"))

(test-equal "an empty target measures the source length"
  3 (damerau-levenshtein "git" ""))

(test-equal "two empty strings measure zero"
  0 (damerau-levenshtein "" ""))

;; Case is folded on both sides before measuring -- correction is
;; case-insensitive and the command names are the fixed side.
(test-equal "the comparison folds case (GIT -> git)"
  0 (damerau-levenshtein "GIT" "git"))

;; ======================================================================
;; The bounded form short-circuits: a far pair stops at the ceiling
;; sentinel (max + 1) without a full fill; a near pair still reports true.
;; ======================================================================

(test-equal "a far pair short-circuits to the ceiling sentinel"
  3 (damerau-levenshtein "abcdef" "uvwxyz" 2))

(test-equal "a near pair returns its true distance under the cap"
  1 (damerau-levenshtein "gti" "git" 2))

;; ======================================================================
;; Non-vacuity: the shipped subsequence fuzzy engine cannot see these
;; commonest typos -- it returns #f -- yet the transposition-aware
;; distance scores each at one. This is the row a tree that tried to
;; reuse fuzzy for correction would fail on.
;; ======================================================================

(test-assert "the subsequence fuzzy engine misses the gti -> git transposition"
  (not (fuzzy-match "gti" "git")))

(test-assert "the subsequence fuzzy engine misses the pyhton -> python transposition"
  (not (fuzzy-match "pyhton" "python")))

(test-assert "the transposition-aware distance catches gti -> git within one"
  (<= (damerau-levenshtein "gti" "git") 1))

(test-assert "the transposition-aware distance catches pyhton -> python within one"
  (<= (damerau-levenshtein "pyhton" "python") 1))

(test-end)
