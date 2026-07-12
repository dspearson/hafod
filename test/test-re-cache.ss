;;; test/test-re-cache.ss -- The bounded compiled-regexp cache at the
;;; string->regexp boundary. Proves two properties a naive, cache-free engine
;;; fails: (1) the SAME (pattern . flags) compiled twice yields the SAME record
;;; (compile-once identity -- a fresh tree builds a distinct record each time);
;;; and (2) a record obtained BEFORE cap-exceeding churn -- and therefore
;;; evicted from the cache -- still searches correctly, proving eviction drops
;;; only the cache's strong reference and never frees the compiled regex out
;;; from under a live holder (the guardian frees each compiled regex exactly
;;; once, when its record is fully unreachable -- an evicted-but-retained record
;;; is still reachable, so it is never freed). Entirely PTY-free and
;;; platform-independent: it touches only the in-process regex engine, opens no
;;; terminal, and needs no platform gate, so it runs identically everywhere.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod re) string->regexp regexp-search?))

(test-begin "re-cache")

;; (1) Cache identity: compiling the same pattern with the same (default) flags
;; twice returns the SAME cached record. A cache-free engine recompiles, so the
;; two records would not be eq?.
(let ((a (string->regexp "cache-identity-probe"))
      (b (string->regexp "cache-identity-probe")))
  (test-assert "same (pattern . flags) is compiled once (eq? cached record)"
    (eq? a b)))

;; The cache keys on the pattern -- it does not collapse distinct patterns onto
;; one entry -- so two different patterns are two different records.
(test-assert "a distinct pattern is a distinct record"
  (not (eq? (string->regexp "cache-identity-probe")
            (string->regexp "cache-other-probe"))))

;; A cached record must still be a working matcher, not an opaque placeholder.
(test-assert "a cached compiled regexp matches correctly"
  (regexp-search? (string->regexp "ab+c") "xx abbbc yy"))

;; (2) Post-eviction liveness. Retain a record, then churn far more than the
;; cap's worth of DISTINCT patterns so the retained pattern is evicted, then
;; prove the retained record still searches correctly. The churn count (2048) is
;; comfortably above the internal cap (a few hundred), so eviction is certain.
(let ((retained (string->regexp "liveness-probe")))
  (do ((i 0 (+ i 1)))
      ((= i 2048))
    (string->regexp (string-append "churn-" (number->string i))))
  ;; Eviction actually happened: a fresh compile of the churned-out pattern is a
  ;; NEW record, not the retained one. (This re-populates the cache, but every
  ;; assertion below is against the ORIGINAL retained record.)
  (test-assert "cap-exceeding churn evicted the earliest pattern"
    (not (eq? retained (string->regexp "liveness-probe"))))
  ;; The retained (now-evicted) record's compiled regex was never freed: it
  ;; still matches and still rejects correctly -- no use-after-free versus the
  ;; guardian. A regfree-on-eviction bug would corrupt or free this regex and
  ;; these searches would crash or misbehave.
  (test-assert "an evicted-but-retained compiled regexp still matches"
    (regexp-search? retained "xx liveness-probe yy"))
  (test-assert "an evicted-but-retained compiled regexp still rejects"
    (not (regexp-search? retained "no probe here"))))

(test-end)
