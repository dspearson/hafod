;;; (hafod terminal-caps) -- Live, per-port terminal capability predicates.
;;; Provides ansi-ok? and colour-ok?: pure predicates of a target output port
;;; or file descriptor deciding whether it is safe to emit ANSI cursor/movement
;;; escapes (ansi-ok?) and SGR colour (colour-ok?).  Each is evaluated live at
;;; every call -- there is no startup cache -- so a piped stream is gated
;;; independently of a terminal one and a mid-session redirect is respected.
;;; Gating governs EMISSION, not measurement.
;;;
;;;   ansi-ok?   = the target is a terminal (isatty) AND TERM is set AND TERM is
;;;                not "dumb".
;;;   colour-ok? = ansi-ok? AND NO_COLOR is unset.  NO_COLOR is honoured by
;;;                PRESENCE: any definition -- including the empty string --
;;;                disables colour (the no-color.org convention).
;;;
;;; The fd resolver is deliberately defensive.  A plain string port, a custom
;;; port, or anything that is not a non-negative descriptor or a live fd-backed
;;; port is treated as "not a terminal" and yields #f rather than raising, so
;;; the predicates are total over whatever argument a caller hands them.  This
;;; keeps the error arm of tty? and the error arm of the underlying fd lookup
;;; unreachable.
;;;
;;; Copyright (c) 2026, hafod contributors.

(library (hafod terminal-caps)
  (export ansi-ok? colour-ok?)

  ;; Exclude Chez's getenv so the environment one -- which is presence-aware and
  ;; tracks the Scheme-side environment -- is used for the TERM/NO_COLOR reads.
  (import (except (chezscheme) getenv)
          (only (hafod tty) tty?)
          (only (hafod environment) getenv)
          (only (hafod fd-ports) fdport?))

  ;; Resolve a target to something tty? can inspect WITHOUT raising:
  ;;   - a non-negative descriptor resolves to itself;
  ;;   - a live fd-backed port resolves to itself, so tty? inspects it via the
  ;;     no-side-effect fd lookup (it does not disturb the revealed count -- the
  ;;     right choice for a predicate called at every emission);
  ;;   - everything else (a string port, a custom port, a negative descriptor)
  ;;     resolves to #f, i.e. "not a terminal".
  (define (target->fd x)
    (cond
      [(and (integer? x) (>= x 0)) x]
      [(fdport? x) x]
      [else #f]))

  ;; ansi-ok?: may the target receive cursor/movement/alt-screen escapes?
  (define (ansi-ok? target)
    (let ([resolved (target->fd target)])
      (and resolved
           (tty? resolved)
           (let ([term (getenv "TERM")])
             (and term (not (string=? term "dumb")))))))

  ;; colour-ok?: may the target receive SGR colour?  Requires ansi-ok? and an
  ;; absent NO_COLOR (presence, not value: "" still disables colour).
  (define (colour-ok? target)
    (and (ansi-ok? target)
         (not (getenv "NO_COLOR"))))

  ) ; end library
