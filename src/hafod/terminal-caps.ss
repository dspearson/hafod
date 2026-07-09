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
  (export assume-terminal-caps ansi-ok? colour-ok? colour-override)

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

  ;; assume-terminal-caps: a default-off override for the capability verdict.
  ;; When set it short-circuits BOTH ansi-ok? and colour-ok? to a forced answer,
  ;; bypassing the live fd/tty/TERM/NO_COLOR probe entirely.  Three values:
  ;;   #f    -- probe live (the default): both predicates behave exactly as
  ;;            before, so an unset parameter is a pure no-op (byte-identical).
  ;;   'on   -- force capabilities ON: both return #t for ANY target (even a
  ;;            plain string port), independent of the ambient environment.
  ;;   'off  -- force capabilities OFF: both return #f for ANY target.
  ;; It is a dynamically-scoped Chez parameter, so a parameterize extent leaves
  ;; no residue once it unwinds.  colour-ok?'s 'on arm returns #t directly rather
  ;; than falling through to the NO_COLOR read, so a forced verdict is wholly
  ;; self-contained and never consults TERM or NO_COLOR.  The same three values
  ;; are what a later colour-control launcher flag reuses to force colour on/off.
  (define assume-terminal-caps (make-parameter #f))

  ;; colour-override: the colour-SPECIFIC override a launcher --color flag sets.
  ;; Distinct from assume-terminal-caps, which forces BOTH ansi and colour: this
  ;; moves ONLY the colour verdict, so a forced-on colour still emits SGR without
  ;; any cursor/alt-screen escapes (ansi-ok? is never consulted from here).  Three
  ;; values, validated on every set:
  ;;   #f       -- auto (the default): fall through to the NO_COLOR/CLICOLOR_FORCE
  ;;               precedence and then ansi-ok?, i.e. behave as before.
  ;;   'always  -- force colour ON for ANY target (even a plain string port),
  ;;               beating NO_COLOR and the live probe.
  ;;   'never   -- suppress colour for ANY target, beating CLICOLOR_FORCE and a tty.
  ;; It is a dynamically-scoped Chez parameter, so a parameterize extent leaves no
  ;; residue once it unwinds.  The validator rejects anything but 'always/'never/#f
  ;; with a controlled error rather than silently accepting a bogus verdict.
  (define colour-override
    (make-parameter #f
      (lambda (v)
        (case v
          [(always never #f) v]
          [else (error 'colour-override
                       "expected 'always, 'never, or #f" v)]))))

  ;; ansi-ok?: may the target receive cursor/movement/alt-screen escapes?
  ;; A forced assume-terminal-caps verdict ('on/'off) bypasses the live probe;
  ;; the default #f runs the unchanged fd/tty/TERM check in the else arm.
  (define (ansi-ok? target)
    (case (assume-terminal-caps)
      [(on) #t]
      [(off) #f]
      [else
       (let ([resolved (target->fd target)])
         (and resolved
              (tty? resolved)
              (let ([term (getenv "TERM")])
                (and term (not (string=? term "dumb"))))))]))

  ;; clicolor-force?: does CLICOLOR_FORCE demand colour?  It forces when the
  ;; variable is SET and its value is not "0" (the de-facto convention:
  ;; CLICOLOR_FORCE=1 forces, =0 does not).  This is deliberately ASYMMETRIC with
  ;; the NO_COLOR read in colour-ok?, which is presence-based -- NO_COLOR="" still
  ;; disables colour by mere presence, whereas CLICOLOR_FORCE="" (being non-"0")
  ;; forces it.  getenv here is the presence-aware (hafod environment) one, so it
  ;; returns the variable's string value or #f when it is unset.
  (define (clicolor-force?)
    (let ([v (getenv "CLICOLOR_FORCE")])
      (and v (not (string=? v "0")))))

  ;; colour-ok?: may the target receive SGR colour?  A forced assume-terminal-caps
  ;; verdict ('on/'off) still wins FIRST (the coarse capability seam; tests only).
  ;; Then the LOCKED colour precedence applies -- colour-only, so ansi-ok? is never
  ;; consulted except as the final auto fallthrough:
  ;;   (1) colour-override 'always -> #t / 'never -> #f  (an explicit --color wins).
  ;;   (2) NO_COLOR present         -> #f  (presence; BEATS CLICOLOR_FORCE -- the
  ;;                                        accessibility-first rule).
  ;;   (3) CLICOLOR_FORCE (non-"0")  -> #t  (force colour, even on a non-tty).
  ;;   (4) else -> (ansi-ok? target).  Reaching this arm proves NO_COLOR is absent,
  ;;       so with colour-override #f and CLICOLOR_FORCE unset it is BYTE-IDENTICAL
  ;;       to the historical (and (ansi-ok? target) (not (getenv "NO_COLOR"))) --
  ;;       the automatic path does not change.
  (define (colour-ok? target)
    (case (assume-terminal-caps)
      [(on) #t]
      [(off) #f]
      [else
       (case (colour-override)
         [(always) #t]
         [(never)  #f]
         [else
          (cond
            [(getenv "NO_COLOR") #f]      ; (2) presence beats CLICOLOR_FORCE
            [(clicolor-force?)   #t]      ; (3) CLICOLOR_FORCE forces (value-checked)
            [else (ansi-ok? target)])])])); (4) byte-identical auto path

  ) ; end library
