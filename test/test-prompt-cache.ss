#!chezscheme
;;; test-prompt-cache.ss -- The per-cwd git segment cache.  Keyed on the working
;;; directory plus the mtimes of .git/HEAD and .git/index, it serves an unchanged
;;; repository from a couple of stats instead of re-spawning `git status`.  Three
;;; facts are pinned PTY-free through the git-probe-count seam: an unchanged repo
;;; is a cache hit (probe-count flat, which also proves an empty/timed-out result
;;; is cached against its stamp rather than re-spawned every prompt), an index
;;; mtime bump invalidates (probe-count increments), and a ".git-is-a-FILE"
;;; worktree resolves its real git dir to a stable stamp while a plain directory
;;; yields #f.  No git binary is required: the count, not the rendered content,
;;; is the oracle.
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
;; The cache builds throwaway .git trees and tears them down with the run EPF, so
;; the full chezscheme surface is imported -- except getenv, taken from (hafod
;; environment) below for its presence-aware semantics, mirroring the sibling
;; prompt suites.
(import (except (chezscheme) getenv)
        (test runner)
        (only (hafod interactive)
              cached-git-segment git-probe-count resolve-git-dir git-marker-stamp)
        (only (hafod environment) getenv setenv)
        (only (hafod fileinfo) create-directory set-file-times)
        (only (hafod syntax) run)
        (only (hafod process-state) pid))

(test-begin "prompt-cache")

;; A unique temp path under $TMPDIR (or /tmp), keyed by pid and a random suffix so
;; concurrent runs never collide.  A path under the system temp dir is outside any
;; git working tree, so resolve-git-dir's walk finds only the .git tree the test
;; plants -- the same isolation the sibling git-prompt suite relies on.
(define (temp-dir-name tag)
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/hafod-prompt-cache-" tag "-"
                 (number->string (pid)) "-"
                 (number->string (random 1000000))))

;; Write CONTENT to PATH (the file must not already exist -- every path here is
;; fresh under a unique temp dir).
(define (write-file path content)
  (call-with-output-file path
    (lambda (p) (put-string p content))))

;; === mtime hit / miss + timeout-sentinel caching ===
;; A hand-made .git/ (HEAD + index) is not a real repository, so the rendered
;; segment is empty -- but the git-probe-count seam counts REAL recomputes, so the
;; hit/miss behaviour is provable without a git binary.  The first draw probes
;; once; a second at the same HEAD/index mtime is a hit (count flat), which is
;; also the proof that the empty result was cached against its stamp rather than
;; re-spawned every prompt; bumping the index mtime invalidates so the next draw
;; re-probes.
(let ([dir (temp-dir-name "repo")]
      [old-pwd (getenv "PWD")])
  (create-directory dir)
  (create-directory (string-append dir "/.git"))
  (write-file (string-append dir "/.git/HEAD") "ref: refs/heads/master\n")
  (write-file (string-append dir "/.git/index") "fake-index\n")
  (dynamic-wind
    void
    (lambda ()
      (setenv "PWD" dir)
      (git-probe-count 0)
      (cached-git-segment)
      (test-equal "cache: the first draw probes once"
        1 (git-probe-count))
      (cached-git-segment)
      (test-equal "cache: an unchanged repo is a hit (no re-probe, sentinel cached)"
        1 (git-probe-count))
      ;; A distinct index mtime (epoch 2001) differs from the just-created "now",
      ;; so the stamp changes and the cache misses.
      (set-file-times (string-append dir "/.git/index") 1000000000 1000000000)
      (cached-git-segment)
      (test-equal "cache: an index mtime bump invalidates (re-probe)"
        2 (git-probe-count)))
    (lambda ()
      (setenv "PWD" old-pwd)
      (run (rm "-rf" ,dir)))))

;; === worktree / submodule .git-as-a-FILE resolution ===
;; A linked worktree has a REGULAR-FILE .git holding "gitdir: <path>"; resolving
;; it must follow the pointer to <path> and stamp <path>/HEAD + <path>/index, not
;; stat the missing <dir>/.git/HEAD and thrash on a #f stamp.
(let ([wt (temp-dir-name "wt")]
      [gd (temp-dir-name "gitdir")])
  (create-directory wt)
  (create-directory gd)
  (write-file (string-append gd "/HEAD") "ref: refs/heads/master\n")
  (write-file (string-append gd "/index") "idx\n")
  (write-file (string-append wt "/.git") (string-append "gitdir: " gd "\n"))
  (dynamic-wind
    void
    (lambda ()
      (test-equal "worktree: resolve-git-dir follows the gitdir pointer"
        gd (resolve-git-dir wt))
      (test-assert "worktree: a .git-file resolves to a non-#f two-mtime stamp"
        (let ([stamp (git-marker-stamp wt)])
          (and (list? stamp) (= (length stamp) 2)))))
    (lambda ()
      (run (rm "-rf" ,wt))
      (run (rm "-rf" ,gd)))))

;; === a plain directory has no marker ===
;; No .git anywhere up the tree, so the stamp is #f -- the cache key that lets a
;; non-repository directory cache its empty segment without a spawn storm.
(let ([plain (temp-dir-name "plain")])
  (create-directory plain)
  (dynamic-wind
    void
    (lambda ()
      (test-assert "plain: a directory with no .git yields a #f marker stamp"
        (eq? #f (git-marker-stamp plain))))
    (lambda ()
      (run (rm "-rf" ,plain)))))

(test-end)
