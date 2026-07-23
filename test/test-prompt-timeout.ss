#!chezscheme
;;; test-prompt-timeout.ss -- The poll(2) readiness primitive underpinning the
;;; prompt's bounded segment collector. Two facts are pinned here: the
;;; `struct pollfd` layout is exactly 8 bytes on the build host, and
;;; fd-wait-readable reports 'ready on a readable descriptor, 'timeout on an
;;; idle one within its deadline, and stays bounded (a blocking implementation
;;; would never return). The primitive lets a slow child probe be bounded and
;;; reaped instead of hanging the prompt on the single OS thread.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (chezscheme)
        (only (hafod internal platform-ftypes) pollfd-t)
        (only (hafod posix) posix-pipe posix-close posix-write fd-wait-readable
              posix-waitpid wait/poll posix-open O_RDONLY)
        (only (hafod internal posix-misc) fcntl-fault)
        (only (hafod interactive) with-spawn-timeout with-spawn-timeout/status
              spawn-grace-probe-count))

(test-begin "prompt-timeout")

;; === struct pollfd layout ===
;; poll(2) reads an array of `struct pollfd { int fd; short events; short
;; revents; }`. Because the layout is a define-ftype, Chez computes every field
;; offset, so the single ABI fact left to pin is the total size: 4 + 2 + 2 = 8
;; bytes, identical on every supported host. An undersized buffer handed to
;; poll would let the kernel write revents past the block, so this probe gates
;; the primitive before it is ever trusted with a live descriptor.
(test-equal "pollfd-t is 8 bytes" 8 (ftype-sizeof pollfd-t))

;; === fd-wait-readable readiness ===
;; A byte already sitting in the pipe means poll reports the read end readable
;; at once. 'ready is the only acceptable verdict here.
(test-equal "readable pipe polls 'ready"
  'ready
  (let ([p (posix-pipe)])
    (posix-write (cdr p) (bytevector 65))
    (let ([verdict (fd-wait-readable (car p) 100)])
      (posix-close (car p))
      (posix-close (cdr p))
      verdict)))

;; An idle pipe must yield 'timeout, and must do so within a small multiple of
;; the 50 ms deadline. The wall-clock bound is the load-bearing half: a
;; blocking-read implementation would sit on the empty pipe forever and never
;; reach the assertion, so pinning the elapsed time under ~1 s is the proof
;; that the block is genuinely bounded.
(test-assert "idle pipe polls 'timeout within its deadline"
  (let ([p (posix-pipe)])
    (let ([t0 (real-time)])
      (let ([verdict (fd-wait-readable (car p) 50)])
        (let ([elapsed (- (real-time) t0)])
          (posix-close (car p))
          (posix-close (cdr p))
          (and (eq? verdict 'timeout) (< elapsed 1000)))))))

;; A non-positive deadline must return promptly rather than blocking forever
;; (poll treats a negative timeout as "wait indefinitely", so the primitive
;; clamps it). On an idle pipe that means a bounded 'timeout, never a hang.
(test-assert "negative deadline returns promptly, never blocks"
  (let ([p (posix-pipe)])
    (let ([t0 (real-time)])
      (let ([verdict (fd-wait-readable (car p) -1)])
        (let ([elapsed (- (real-time) t0)])
          (posix-close (car p))
          (posix-close (cdr p))
          (and (eq? verdict 'timeout) (< elapsed 1000)))))))

;; A descriptor carrying a poll error condition -- here a CLOSED fd, which polls
;; POLLNVAL -- must not be reported 'ready.  A blind r>0 -> 'ready makes the bounded
;; collector's guarded read raise, collapse to a re-poll, and busy-spin the same
;; error to the deadline.  fd-wait-readable now inspects revents and reports a bail
;; verdict ('timeout) for an error/invalid fd carrying neither readable data nor a
;; hangup, so the collector stops and reaps at once.  On the pre-fix tree this
;; returned 'ready -- non-vacuous.
(test-equal "an invalid (closed) fd polls as a bail verdict, not 'ready"
  'timeout
  (let ([p (posix-pipe)])
    (posix-close (car p))
    (posix-close (cdr p))
    (fd-wait-readable (car p) 50)))

;; === with-spawn-timeout -- bounded collect + kill/reap ===
;; The bounded child collector built on fd-wait-readable: it spawns a child with
;; its stdout piped back, polls that descriptor against a deadline, and on a
;; timeout kills (SIGTERM then SIGKILL) and reaps the child exactly once. The
;; two load-bearing guarantees are the wall-clock bound (a slow child cannot
;; hang the prompt) and the reap (a killed child leaves no zombie).

;; Reap and count every already-finished child of this process. A non-blocking
;; waitpid (wait/poll is WNOHANG) returns a positive pid for each finished child
;; in turn, 0 while a child is still running, and raises when none remain at all
;; -- swallowed here as "none left". Zero leftover after a call proves the
;; collector reaped its own child rather than leaking a zombie.
(define (drain-finished-children)
  (let loop ([n 0])
    (let ([r (guard (e [#t 'none])
               (let-values ([(w s) (posix-waitpid -1 wait/poll)]) w))])
      (cond
        [(eq? r 'none) n]
        [(and (integer? r) (> r 0)) (loop (+ n 1))]
        [else n]))))

;; A child that outlives its deadline is bounded and reaped. sleep 5 against a
;; 100 ms deadline must return timed-out within a small multiple of the deadline
;; -- a blocking read would sit for the full five seconds and blow the wall-clock
;; assertion -- and afterward no unreaped child remains. sleep is spawned
;; DIRECTLY (not through sh -c) so the killed pid is the reaped pid, with no
;; orphaned grandchild to muddy the zombie count. The leading drain clears any
;; child the runner scaffolding left behind before the bound is measured.
(test-assert "with-spawn-timeout bounds a slow child and reaps it (no zombie)"
  (begin
    (drain-finished-children)
    (let ([t0 (real-time)])
      (let-values ([(out timed-out?) (with-spawn-timeout "sleep" '("sleep" "5") 100)])
        (let ([elapsed (- (real-time) t0)])
          (and timed-out?
               (< elapsed 1000)
               (= (drain-finished-children) 0)))))))

;; A fast child's whole stdout is captured and decoded once, not timed out. echo
;; writes "hi" and exits well inside the one-second deadline, so the collector
;; reads to EOF and reports timed-out? #f with the output in hand.
(test-assert "with-spawn-timeout captures a fast child's output whole"
  (let-values ([(out timed-out?) (with-spawn-timeout "echo" '("echo" "hi") 1000)])
    (and (not timed-out?)
         (>= (string-length out) 2)
         (string=? (substring out 0 2) "hi"))))

;; A child that exits immediately with no output takes the not-timed-out path.
(test-assert "with-spawn-timeout completes a quick child within its deadline"
  (let-values ([(out timed-out?) (with-spawn-timeout "true" '("true") 1000)])
    (not timed-out?)))

;; An absent program is a quiet empty result -- the whole spawn is guarded, so a
;; missing binary yields ("" . #f) exactly as the fail-quiet git path does,
;; never a raised condition scrolling above the prompt.
(test-assert "with-spawn-timeout on an absent program is a quiet empty result"
  (let-values ([(out timed-out?)
                (with-spawn-timeout "hafod-no-such-program-xyzzy"
                                    '("hafod-no-such-program-xyzzy") 1000)])
    (and (string=? out "") (not timed-out?))))

;; === with-spawn-timeout closes the read fd even when the non-blocking setup
;; raises ===
;; Once the child and its pipe exist, the read fd must be closed exactly once on
;; EVERY exit -- including the path where setting it non-blocking (posix-fcntl)
;; raises before the read loop.  fcntl-fault forces that raise; a lowest-free-fd
;; canary then proves the read fd is not leaked across a run of forced-fault
;; collections.  On the pre-fix tree the coarse outer guard returned ("" . #f)
;; without closing the read fd, so the canary would climb -- non-vacuous.

;; A lowest-free-fd probe: opening a stable read-only target returns the lowest free
;; descriptor and closing it hands that slot straight back, so two probes with
;; nothing leaked between them return the SAME number (the ffi-leak suite's canary).
(define (probe-fd)
  (let ([fd (posix-open "/dev/null" O_RDONLY 0)])
    (posix-close fd)
    fd))

(parameterize ([fcntl-fault (lambda () (error 'inject "forced fcntl fault"))])
  ;; Under a forced fcntl fault the collection is a quiet empty result and the child
  ;; is reaped -- "true" exits at once, so the kill/reap tail is cheap.
  (test-assert "with-spawn-timeout under a forced fcntl fault is a quiet empty result"
    (let-values ([(out timed-out?) (with-spawn-timeout "true" '("true") 1000)])
      (string=? out "")))
  (drain-finished-children)
  ;; Drive 40 forced-fault collections; each spawns a child and holds a read fd
  ;; until finish-spawn closes it on the unwind.  A leaked read fd would occupy a
  ;; low slot and bump the probe higher; an equal before/after number proves the
  ;; read fd is closed on the fault path.
  (let ([before (probe-fd)])
    (do ([i 0 (+ i 1)]) ((= i 40))
      (let-values ([(out timed-out?) (with-spawn-timeout "true" '("true") 1000)]) #f)
      (drain-finished-children))
    (test-equal "with-spawn-timeout closes the read fd on the fcntl-fault unwind (fd canary)"
      before (probe-fd))))

;; === the SIGTERM->SIGKILL grace loop sleeps between reap probes (no busy-spin) ===
;; A child that IGNORES SIGTERM forces the grace window to fully elapse -- exactly
;; when a tight waitpid(WNOHANG) spin would manifest.  The grace loop increments
;; spawn-grace-probe-count once per probe; sleeping ~2 ms between probes bounds the
;; count to a handful across the ~20 ms window, whereas a spin runs thousands.
;;
;; Such a child needs a shell trap plus a long-lived, output-free command.
;; with-spawn-timeout spawns its child with a scrubbed PATH, so the shell cannot
;; resolve an external `sleep` -- exec an ABSOLUTE sleep instead (SIG_IGN survives
;; the exec, so the sleep keeps ignoring SIGTERM and stays our direct child, reaped
;; without an orphan).  The leg self-skips where no POSIX sh or absolute sleep is
;; present, both being available on every supported platform.
(define sh-present?
  (guard (e [#t #f])
    (let-values ([(out t?) (with-spawn-timeout "sh" '("sh" "-c" "echo ok") 1000)])
      (and (not t?) (>= (string-length out) 2) (string=? (substring out 0 2) "ok")))))

(define sleep-bin
  (cond [(file-exists? "/bin/sleep") "/bin/sleep"]
        [(file-exists? "/usr/bin/sleep") "/usr/bin/sleep"]
        [else #f]))

(when (and sh-present? sleep-bin)
  (drain-finished-children)
  (spawn-grace-probe-count 0)
  (let-values ([(out timed-out?)
                (with-spawn-timeout "sh"
                  (list "sh" "-c" (string-append "trap '' TERM; exec " sleep-bin " 1"))
                  30)])
    (test-assert "with-spawn-timeout bounds a SIGTERM-ignoring child"
      timed-out?)
    (test-assert "the SIGTERM grace loop sleeps between reap probes (bounded count)"
      (< (spawn-grace-probe-count) 100)))
  (drain-finished-children))

;; === with-spawn-timeout stderr disposition and child environment ===
;; The two dispositions the per-language version probe adds, each pinned against
;; its opposite so neither leg is vacuous.  Both drive `sh` with a FIXED literal
;; argv -- the sentinel value travels through the env-list, never spliced into the
;; command string -- and self-skip where no POSIX sh is present.

;; Naive substring search: #t iff NEEDLE occurs anywhere in HAYSTACK.  Enough to
;; assert a version-shaped token did (or did not) reach the captured stream.
(define (string-contains? haystack needle)
  (let ([hn (string-length haystack)] [nn (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nn) hn) #f]
        [(string=? (substring haystack i (+ i nn)) needle) #t]
        [else (loop (+ i 1))]))))

(when sh-present?
  ;; Stderr-merge: a child that writes its banner ONLY to stderr (the shape of
  ;; `java -version`) is captured under 'merge -- fd 2 is folded into the same pipe
  ;; as stdout -- and dropped under 'discard, where stderr goes to /dev/null.  The
  ;; discard leg is the shipped behaviour, so only the merge leg sees the token:
  ;; non-vacuous.
  (let ([argv (list "sh" "-c" "echo 7.8.9 1>&2")])
    (let-values ([(merged merge-t?) (with-spawn-timeout "sh" argv 1000 'merge #f)])
      (test-assert "with-spawn-timeout 'merge captures a stderr-only child's banner"
        (and (not merge-t?) (string-contains? merged "7.8.9"))))
    (let-values ([(discarded discard-t?) (with-spawn-timeout "sh" argv 1000 'discard #f)])
      (test-assert "with-spawn-timeout 'discard drops a stderr-only child's banner"
        (and (not discard-t?) (not (string-contains? discarded "7.8.9"))))))

  ;; Child environment: a non-#f env-list is delivered to the child -- a sentinel
  ;; variable read inside the child prints its value -- whereas env-list #f leaves
  ;; the child with an empty environment, so the sentinel is unset and the output is
  ;; empty.  printf %s keeps the capture exact, and the discard mode shows env
  ;; delivery is orthogonal to the stderr disposition.
  (let ([argv (list "sh" "-c" "printf %s \"$HAFOD_PROBE_SENTINEL\"")]
        [sentinel "env-sentinel-9times7"])
    (let-values ([(seen seen-t?)
                  (with-spawn-timeout "sh" argv 1000 'discard
                    (list (string-append "HAFOD_PROBE_SENTINEL=" sentinel)))])
      (test-assert "with-spawn-timeout hands the env-list to the child"
        (and (not seen-t?) (string=? seen sentinel))))
    (let-values ([(none none-t?) (with-spawn-timeout "sh" argv 1000 'discard #f)])
      (test-assert "with-spawn-timeout with env-list #f gives the child an empty environment"
        (and (not none-t?) (string=? none ""))))))

;; === EOF on the pipe does not mean the child has exited ===
;;
;; The deadline bounds the READ.  When the read ends at EOF rather than at the
;; deadline the collector takes its clean-finish path, and a plain blocking
;; waitpid there is unbounded: a wrapper that redirects its own stdout and stderr
;; away and then works on (`exec >/dev/null 2>&1; ...`) closes every pipe write
;; end immediately and keeps running, so the collector sees EOF at once and then
;; sits for as long as the child lives -- with the REPL frozen behind it.  Before
;; the version group the only callee was git, which never behaves this way; now it
;; is any wrapper shim on the user's PATH.
;;
;; The child below sleeps five seconds after closing its output, against a 200 ms
;; deadline the read never reaches (EOF arrives first).  An unbounded reap sits
;; the full five seconds, so the wall-clock bound is strongly non-vacuous; the
;; drain afterwards proves the bounded path still leaves no zombie.  Self-skips
;; where no POSIX sh or absolute sleep is present.
(when (and sh-present? sleep-bin)
  (drain-finished-children)
  (let ([t0 (real-time)])
    (let-values ([(out timed-out?)
                  (with-spawn-timeout "sh"
                    (list "sh" "-c"
                          (string-append "exec >/dev/null 2>&1; exec " sleep-bin " 5"))
                    200)])
      (let ([elapsed (- (real-time) t0)])
        (test-assert "with-spawn-timeout bounds the reap of a child that closes stdout and lives on"
          (< elapsed 1500))
        (test-equal "the bounded reap leaves no zombie behind"
          0 (drain-finished-children))))))

;; === A byte ceiling belongs to the CALLER, and a capped read is not a timeout ===
;;
;; The collector has two callers wanting opposite things.  A --version banner is
;; two lines, so the version probe wants a ceiling on a shim that dumps a log; a
;; `git status --porcelain=v2` is some 150 bytes per changed or untracked entry,
;; so a few hundred of them clear 64 KB in an ordinary dirty tree and a ceiling
;; charged to THAT caller truncates a legitimate answer.  Worse, a ceiling
;; reported as timed-out makes the git segment throw away the complete branch
;; header it is already holding at the front of the capture, and the empty result
;; is then cached.
;;
;; The child below prints 200 000 bytes and exits cleanly, comfortably inside its
;; deadline, so the only thing that can cut it short is a ceiling.  Three facts:
;; a two-value caller gets the whole stream and no timeout; the five-argument
;; /status form is likewise unbounded and reports the clean exit; and the
;; six-argument form caps the read but reports it as a NON-timeout with an unknown
;; exit status (the child was killed, so there is none) -- which is what the
;; version probe already rejects on, and what leaves a prefix-reading caller its
;; prefix.  Self-skips where no POSIX sh is present.
(when sh-present?
  (drain-finished-children)
  (let* ([pad     (make-string 999 #\x)]
         [total   200000]
         [big-argv (list "sh" "-c"
                         (string-append "i=0; while [ $i -lt 200 ]; do printf '%s\\n' "
                                        pad "; i=$((i+1)); done"))])
    (let-values ([(out timed-out?) (with-spawn-timeout "sh" big-argv 5000)])
      (test-assert "with-spawn-timeout reads a large stream whole and reports no timeout"
        (and (not timed-out?) (= (string-length out) total))))
    (let-values ([(out timed-out? status)
                  (with-spawn-timeout/status "sh" big-argv 5000 'discard #f)])
      (test-assert "with-spawn-timeout/status without a ceiling is bounded only by the deadline"
        (and (not timed-out?) (= (string-length out) total) (eqv? status 0))))
    (let-values ([(out timed-out? status)
                  (with-spawn-timeout/status "sh" big-argv 5000 'discard #f 65536)])
      (test-assert "a caller's byte ceiling caps the read"
        (and (>= (string-length out) 65536) (< (string-length out) total)))
      (test-assert "a capped read is NOT reported as a timeout"
        (not timed-out?))
      (test-assert "a capped read reports an unknown exit status (the child was killed)"
        (not status)))
    (test-equal "the capped child leaves no zombie behind"
      0 (drain-finished-children))))

(test-end)
