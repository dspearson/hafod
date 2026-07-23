#!chezscheme
;;; test-prompt-version.ss -- The per-language version-tool registry.  A tool is a
;;; prompt-tool record (name, a (files . exts) marker pair, command, args, a parse
;;; procedure, and emoji / ascii / an optional nerd glyph); the ordered prompt-tools
;;; parameter holds the set, register-prompt-tool! appends to it -- so registration
;;; order is draw order -- and clear-prompt-tools! empties it.  These proofs pin the
;;; record shape, the append order, the optional-nerd default and the parse-shape
;;; rejection PTY-free: pure record and parameter assertions, no terminal and no
;;; spawned tool.  They also pin the one-tool renderer: the Nerd-Font opt-in, the
;;; emoji/ascii/nerd glyph choice, the "via <glyph> vX" layout, the colour gating
;;; on the fd-1 verdict and the control-byte sanitisation -- all forced, no spawn.
;;; The second half drives the live engine -- marker detection, the PATH gate,
;;; the per-cwd cache and the bounded probe -- against fake tools wrapping real
;;; host binaries in throwaway fixture directories, so no real toolchain is
;;; required and nothing needs a terminal.
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
;; The detection/probe battery builds throwaway marker-fixture directories and
;; tears them down with the run EPF, so the full chezscheme surface is imported;
;; the fixture helpers (create-directory / set-file-times, run, pid) and the
;; classifier path-cache come from the hafod libraries, mirroring the sibling
;; prompt-cache suite.
(import (chezscheme)
        (test runner)
        (only (hafod interactive)
              prompt-tools register-prompt-tool! unregister-prompt-tool!
              clear-prompt-tools!
              make-prompt-tool prompt-tool?
              prompt-tool-name prompt-tool-markers prompt-tool-command
              prompt-tool-args prompt-tool-parse prompt-tool-emoji
              prompt-tool-ascii prompt-tool-nerd
              parse-version/common
              parse-python parse-node parse-rust parse-go parse-bun
              parse-ruby parse-java parse-deno parse-php parse-zig
              nerd-glyphs? tool-glyph render-one-tool prompt-colour-ok?
              detect-tools-in tool-on-path?
              ;; The candidate-list seams: the single normaliser of a command
              ;; value, the first-candidate-on-PATH resolver, the argv builder
              ;; that substitutes the winner for a LIST command only -- and the
              ;; single probe itself, called directly so the "no candidate
              ;; resolves" early return is reached rather than masked by the
              ;; group's own gate.
              tool-command-candidates resolve-tool-command tool-probe-argv
              probe-tool-version
              cached-version-segment version-probe-count version-cache-ttl-ms
              version-detect-count version-cache-cap prompt-spawn-timeout-ms
              prompt-versions? version-group-enabled?
              ;; ansi-visible-length measures rendered COLUMNS with the SGR
              ;; stripped -- the same oracle the width budget itself uses.
              ansi-visible-length)
        ;; setenv drives the toolchain-environment half of the cache stamp; it is
        ;; not a (chezscheme) name, so it needs no exclusion above.
        ;; envp-rebuild-observer fires once per REAL rebuild of the shared child
        ;; environment, so a test can prove the probe goes through that shared
        ;; path rather than composing an environment of its own.
        (only (hafod environment) setenv envp-rebuild-observer)
        (only (hafod fileinfo) create-directory set-file-times)
        (only (hafod shell classifier) path-cache)
        (only (hafod syntax) run)
        (only (hafod process-state) pid)
        ;; posix-waitpid / wait/poll drain finished children, so the timeout leg
        ;; can assert the killed probe left no zombie (copied from the sibling
        ;; timeout suite's drain-finished-children).
        (only (hafod posix) posix-waitpid wait/poll))

(test-begin "prompt-version")

;; A trivial parse procedure for the descriptor proofs -- the identity, so the
;; value an accessor returns is obviously the one handed in.
(define (parse-id out) out)

;; === The descriptor round-trips its fields ===
;;
;; make-prompt-tool stores each field and its accessor returns it -- the shape the
;; detector, probe and renderer consume downstream.  A record whose accessors were
;; wired to the wrong fields fails one of these equalities: non-vacuous.
(let ([t (make-prompt-tool "ruby" '(("Gemfile" "Gemfile.lock") . (".rb"))
                           "ruby" '("ruby" "--version") parse-id
                           "\x1f48e;" "rb" #f)])
  (test-assert "shape: make-prompt-tool builds a prompt-tool" (prompt-tool? t))
  (test-equal "shape: the name accessor" "ruby" (prompt-tool-name t))
  (test-equal "shape: the markers accessor keeps the (files . exts) pair"
    '(("Gemfile" "Gemfile.lock") . (".rb")) (prompt-tool-markers t))
  (test-equal "shape: the command accessor" "ruby" (prompt-tool-command t))
  (test-equal "shape: the args accessor" '("ruby" "--version") (prompt-tool-args t))
  (test-assert "shape: the parse accessor returns the procedure handed in"
    (eq? parse-id (prompt-tool-parse t)))
  (test-equal "shape: the emoji accessor" "\x1f48e;" (prompt-tool-emoji t))
  (test-equal "shape: the ascii accessor" "rb" (prompt-tool-ascii t)))

;; === Registration order is draw order ===
;;
;; From a known (empty) registry, two register-prompt-tool! calls append in call
;; order, so the registry reads back last-registered-last.  A registry that
;; PREPENDED would reverse the two names and fail the ordering below: non-vacuous.
;; Each proof isolates the registry with parameterize so the shipped built-in
;; tool set never perturbs it.
(parameterize ([prompt-tools '()])
  (register-prompt-tool! "alpha" '(("alpha.marker") . ()) "alpha"
                         '("alpha" "--version") parse-id "A" "a")
  (register-prompt-tool! "beta" '(("beta.marker") . ()) "beta"
                         '("beta" "--version") parse-id "B" "b")
  (test-equal "order: register-prompt-tool! appends, so registration order is draw order"
    '("alpha" "beta") (map prompt-tool-name (prompt-tools))))

;; === The optional Nerd-Font glyph defaults to #f ===
;;
;; The seven-argument form supplies no nerd glyph, so prompt-tool-nerd is #f; the
;; eight-argument form carries the glyph handed to it.  A register! that dropped
;; the eighth argument, or left the seven-arg nerd unset, fails one leg: non-vacuous.
(parameterize ([prompt-tools '()])
  (register-prompt-tool! "seven" '(("seven.marker") . ()) "seven"
                         '("seven" "--version") parse-id "S" "s")
  (register-prompt-tool! "eight" '(("eight.marker") . ()) "eight"
                         '("eight" "--version") parse-id "E" "e" "\xe0a0;")
  (let ([tools (prompt-tools)])
    (test-equal "nerd: the seven-argument form defaults the Nerd-Font glyph to #f"
      #f (prompt-tool-nerd (car tools)))
    (test-equal "nerd: the eight-argument form carries the supplied Nerd-Font glyph"
      "\xe0a0;" (prompt-tool-nerd (cadr tools)))))

;; === A non-procedure parse is rejected at registration ===
;;
;; parse must be a procedure (probe output -> version string or #f).  Handing a
;; string where the procedure belongs raises at registration rather than
;; corrupting the registry: an unvalidated register! would accept it silently and
;; only fail at the first probe, so the raise below is non-vacuous.  The
;; parameterize keeps the failed registration from leaking into a later proof.
(test-error "reject: register-prompt-tool! rejects a non-procedure parse"
  (parameterize ([prompt-tools '()])
    (register-prompt-tool! "bad" '(("bad.marker") . ()) "bad"
                           '("bad" "--version") "not-a-procedure" "X" "x")))

;; The parameter validates its whole value too: a non-record element is rejected at
;; the parameter boundary, not merely inside register-prompt-tool!.
(test-error "reject: prompt-tools rejects a non-record element"
  (parameterize ([prompt-tools (list "not-a-tool")]) #t))

;; === Every descriptor field is checked, not only parse ===
;;
;; Each field below is dereferenced structurally downstream -- the detector reads
;; markers as a pair of lists, the PATH gate hashes command, the renderer takes
;; the length of a glyph -- and a bad shape there raises inside a prompt draw,
;; where the segment machinery's blanket guard swallows it and the entire version
;; group disappears without a word.  A register! that validated only parse accepts
;; every row below, so each is non-vacuous.  The bare-list markers row is the
;; commonest slip of the three.
(test-error "reject: markers must be a (filenames . extensions) pair, not a bare list"
  (parameterize ([prompt-tools '()])
    (register-prompt-tool! "flat" '("flat.marker") "flat"
                           '("flat" "--version") parse-id "F" "f")))
;; The command field takes a binary NAME or a non-empty list of candidate names,
;; so "malformed" is the honest description of what is refused: an integer is
;; neither.  The three rows below sweep the whole rule, and the empty list is the
;; one that matters -- it satisfies list?, so a boundary spelled merely "a string
;; or a list" would accept a descriptor the gate can never resolve.
(test-error "reject: register-prompt-tool! rejects a malformed command"
  (parameterize ([prompt-tools '()])
    (register-prompt-tool! "nc" '(("nc.marker") . ()) 42
                           '("nc" "--version") parse-id "N" "n")))
(test-error "reject: register-prompt-tool! rejects an empty candidate list"
  (parameterize ([prompt-tools '()])
    (register-prompt-tool! "ec" '(("ec.marker") . ()) '()
                           '("ec" "--version") parse-id "E" "e")))
(test-error "reject: register-prompt-tool! rejects a candidate list carrying a non-string"
  (parameterize ([prompt-tools '()])
    (register-prompt-tool! "mc" '(("mc.marker") . ()) '("mc" 42)
                           '("mc" "--version") parse-id "M" "m")))

;; The other half of the same rule: a non-empty list of strings IS accepted, and
;; reaches the registry as the list it was handed.  A boundary that still demanded
;; a string would raise here instead: non-vacuous.
(parameterize ([prompt-tools '()])
  (register-prompt-tool! "alt" '(("alt.marker") . ()) '("alt" "alt3")
                         '("alt" "--version") parse-id "A" "a")
  (test-equal "accept: register-prompt-tool! accepts a candidate-list command"
    '("alt" "alt3") (prompt-tool-command (car (prompt-tools)))))
(test-error "reject: register-prompt-tool! rejects a non-string glyph"
  (parameterize ([prompt-tools '()])
    (register-prompt-tool! "ng" '(("ng.marker") . ()) "ng"
                           '("ng" "--version") parse-id 'not-a-string "n")))
(test-error "reject: register-prompt-tool! rejects an empty argv"
  (parameterize ([prompt-tools '()])
    (register-prompt-tool! "na" '(("na.marker") . ()) "na"
                           '() parse-id "N" "n")))

;; === clear-prompt-tools! empties the registry ===
;;
;; A no-op clear would leave the registered tool in place and fail the equality:
;; non-vacuous.
(parameterize ([prompt-tools '()])
  (register-prompt-tool! "gone" '(("gone.marker") . ()) "gone"
                         '("gone" "--version") parse-id "G" "g")
  (clear-prompt-tools!)
  (test-equal "clear: clear-prompt-tools! empties the registry" '() (prompt-tools)))

;; === A tool is removed by NAME, the rest keeping their order ===
;;
;; "Drop the java segment, keep the other nine" is the obvious edit a user makes
;; to the shipped set, and it is the reason the registry is worth exporting at
;; all.  Removing the MIDDLE of three proves the survivors keep registry (= draw)
;; order rather than being rebuilt in some other one, and removing a name that is
;; not there leaves the registry alone rather than raising.
(parameterize ([prompt-tools '()])
  (for-each
    (lambda (n)
      (register-prompt-tool! n (cons (list (string-append n ".marker")) '()) n
                             (list n "--version") parse-id "G" "g"))
    '("one" "two" "three"))
  (unregister-prompt-tool! "two")
  (test-equal "unregister: the named tool is dropped and the order survives"
    '("one" "three") (map prompt-tool-name (prompt-tools)))
  (unregister-prompt-tool! "hafod-no-such-tool")
  (test-equal "unregister: an unknown name leaves the registry alone"
    '("one" "three") (map prompt-tool-name (prompt-tools))))

(test-error "unregister: a non-string name is rejected"
  (parameterize ([prompt-tools '()]) (unregister-prompt-tool! 'two)))

;; === The shared version parser takes the first dotted-decimal token with a minor ===
;;
;; parse-version/common walks --version output a LINE at a time and returns the
;; first run of at least two dot-separated components ([0-9]+(.[0-9]+)+),
;; stepping over any leading label word, a quote or a leading `v`.  Each row
;; below is a documented --version line; a naive "second whitespace token" or
;; "first line only" scan fails the quoted-string and the multi-line rows, so
;; these are non-vacuous.  The empty string and a version-free line yield #f --
;; a mis-parse is fail-quiet (no segment), never a crash.
(test-equal "parse: a labelled version -- Python 3.13.14"
  "3.13.14" (parse-version/common "Python 3.13.14\n"))
(test-equal "parse: a leading v is stripped -- v20.11.1"
  "20.11.1" (parse-version/common "v20.11.1\n"))
(test-equal "parse: a trailing build tail is dropped -- rustc 1.77.0 (...)"
  "1.77.0" (parse-version/common "rustc 1.77.0 (aedd173a2 2024-03-17)"))
(test-equal "parse: a quoted version -- openjdk version \"21.0.2\""
  "21.0.2" (parse-version/common "openjdk version \"21.0.2\" 2024-01-16"))
(test-equal "parse: a prefixed token -- go version go1.22.1"
  "1.22.1" (parse-version/common "go version go1.22.1 linux/amd64"))
(test-equal "parse: a bare version -- 0.11.0"
  "0.11.0" (parse-version/common "0.11.0\n"))
(test-equal "parse: a version-free warning line is stepped over"
  "1.22.1"
  (parse-version/common "warning: something is off\ngo version go1.22.1 linux/amd64\n"))
(test-equal "parse: the empty string yields #f" #f (parse-version/common ""))
(test-equal "parse: a line with no version yields #f"
  #f (parse-version/common "not a version"))

;; A toolchain's own PREAMBLE must not win the parse.  The probe merges the
;; child's stderr into its stdout, and a piped child's stderr is unbuffered
;; while its stdout is block buffered, so the diagnostics a tool prints before
;; its banner normally reach the pipe FIRST.  These are the two commonest, and
;; neither is hostile: the JVM's own notice when JAVA_TOOL_OPTIONS is set (a
;; default in many container images) and a PHP module startup warning.  A scan
;; that took the first digit run anywhere reports "2048" and "0" as the
;; version, so both rows are non-vacuous; requiring a minor component steps
;; over each bare integer and the banner on the next line wins.
(test-equal "parse: a JVM heap-option notice is not the version"
  "21.0.2"
  (parse-version/common
    "Picked up JAVA_TOOL_OPTIONS: -Xmx2048m\nopenjdk version \"21.0.2\" 2024-01-16\n"))
(test-equal "parse: a startup warning's line number is not the version"
  "8.3.3"
  (parse-version/common
    "PHP Warning:  Module 'xdebug' already loaded in Unknown on line 0\nPHP 8.3.3 (cli)\n"))
(test-equal "parse: a bare integer is never a version (a minor is required)"
  #f (parse-version/common "512\n"))

;; === Each built-in tool's parse rule extracts its documented version ===
;;
;; The per-tool parse procedures delegate to the shared helper; each is proved
;; against the tool's documented --version output (canned strings, NO spawn).
;; The java row (its banner prints to stderr and quotes the version) and the go
;; row (a go-prefixed token) are the ones a wrong-token scan fails, so the set is
;; non-vacuous.  A garbage line yields #f (fail-quiet).
(test-equal "parse-python: Python 3.13.14 -> 3.13.14"
  "3.13.14" (parse-python "Python 3.13.14\n"))
(test-equal "parse-node: v20.11.1 -> 20.11.1"
  "20.11.1" (parse-node "v20.11.1\n"))
(test-equal "parse-rust: rustc 1.77.0 (...) -> 1.77.0"
  "1.77.0" (parse-rust "rustc 1.77.0 (aedd173a2 2024-03-17)"))
(test-equal "parse-go: go version go1.22.1 ... -> 1.22.1"
  "1.22.1" (parse-go "go version go1.22.1 linux/amd64"))
(test-equal "parse-bun: 1.0.30 -> 1.0.30"
  "1.0.30" (parse-bun "1.0.30\n"))
(test-equal "parse-ruby: ruby 3.3.0 (...) [x86_64-linux] -> 3.3.0"
  "3.3.0" (parse-ruby "ruby 3.3.0 (2023-12-25 revision 5124f9ac75) [x86_64-linux]"))
(test-equal "parse-java: openjdk version \"21.0.2\" -> 21.0.2"
  "21.0.2" (parse-java "openjdk version \"21.0.2\" 2024-01-16"))
(test-equal "parse-deno: deno 1.41.0 (release, ...) -> 1.41.0"
  "1.41.0"
  (parse-deno "deno 1.41.0 (release, x86_64-unknown-linux-gnu)\nv8 12.1.285.27\ntypescript 5.3.3"))
(test-equal "parse-php: PHP 8.3.3 (cli) ... -> 8.3.3"
  "8.3.3" (parse-php "PHP 8.3.3 (cli) (built: Feb 15 2024 00:00:00) (NTS)"))
(test-equal "parse-zig: 0.11.0 -> 0.11.0"
  "0.11.0" (parse-zig "0.11.0\n"))
(test-equal "parse-go: a garbage line yields #f" #f (parse-go "not go output"))
(test-equal "parse-java: a garbage line yields #f" #f (parse-java "no version here"))

;; === The built-in tool set seeds the registry in the locked order ===
;;
;; The prompt-tools default is the shipped ten built-ins, in the fixed
;; registration (= draw) order.  This pins the count, the exact order and that
;; every descriptor carries a non-empty emoji, a non-empty ascii and a procedure
;; parse -- the fields the detector, probe and renderer consume downstream.  A
;; set that dropped a tool, reordered them or shipped a blank glyph fails a row:
;; non-vacuous.  Read outside any parameterize so it sees the true default.
(let ([tools (prompt-tools)])
  (test-equal "builtins: the default registry holds exactly ten tools"
    10 (length tools))
  (test-equal "builtins: the locked registration order"
    '("python" "node" "rust" "go" "bun" "ruby" "java" "deno" "php" "zig")
    (map prompt-tool-name tools))
  (for-each
    (lambda (t)
      (let ([name (prompt-tool-name t)])
        (test-assert (string-append "builtins: " name " carries a non-empty emoji")
          (and (string? (prompt-tool-emoji t))
               (positive? (string-length (prompt-tool-emoji t)))))
        (test-assert (string-append "builtins: " name " carries a non-empty ascii")
          (and (string? (prompt-tool-ascii t))
               (positive? (string-length (prompt-tool-ascii t)))))
        (test-assert (string-append "builtins: " name " carries a procedure parse")
          (procedure? (prompt-tool-parse t)))
        (test-assert (string-append "builtins: " name "'s nerd glyph is a string or #f")
          (let ([n (prompt-tool-nerd t)])
            (or (not n) (and (string? n) (positive? (string-length n))))))))
    tools)
  ;; The Nerd-Font opt-in must not be inert.  With every shipped descriptor at #f
  ;; -- as they all were -- (nerd-glyphs? #t) changed nothing whatever out of the
  ;; box, and the branch could only ever be exercised by a fabricated test tool.
  ;; The languages with a long-established devicon carry one; the rest fall back.
  (test-assert "builtins: the shipped set actually exercises the Nerd-Font opt-in"
    (exists (lambda (t) (and (prompt-tool-nerd t) #t)) tools))
  (test-assert "builtins: a tool WITH a nerd glyph shows it under the opt-in"
    (let ([t (find (lambda (t) (prompt-tool-nerd t)) tools)])
      (parameterize ([nerd-glyphs? #t])
        (string=? (tool-glyph t 'emoji) (prompt-tool-nerd t))))))

;; === The Nerd-Font opt-in, glyph choice and the one-tool renderer ===
;;
;; render-one-tool formats a detected tool as a starship-style "via <glyph> vX"
;; segment, choosing the glyph by the tier and the Nerd-Font opt-in and colouring
;; only under a non-mono depth AND a true fd-1 verdict.  A fake tool with three
;; DISTINCT glyphs -- an emoji, an ascii mnemonic and a nerd glyph -- lets each
;; row assert exactly which glyph the renderer chose.  The tier and colour depth
;; are passed as the per-draw context (make-current-prompt-ctx) would supply
;; them, so the proof is PTY-free and spawns nothing.

;; Substring search and an ESC-free predicate (the runner carries neither).
(define (string-contains? haystack needle)
  (let ([hlen (string-length haystack)] [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

(define (no-esc? str)
  (let loop ([i 0])
    (cond
      [(>= i (string-length str)) #t]
      [(= (char->integer (string-ref str i)) #x1b) #f]
      [else (loop (+ i 1))])))

;; A fake tool with three distinct glyphs, a nerd glyph present.  The mnemonics
;; are unlike any substring of the "via ... v" frame, so a body assertion is
;; unambiguous about which glyph was chosen.
(define fake-tool
  (make-prompt-tool "fake" '(("fake.marker") . ()) "fake"
                    '("fake" "--version") parse-id "EMO" "ASC" "NRD"))

;; emoji default: with no opt-in and the emoji tier the emoji glyph is used, and
;; a tier-blind renderer that emitted the ascii mnemonic would fail this row.
(test-assert "render: the emoji glyph is the default"
  (string-contains? (render-one-tool fake-tool 'emoji 'mono "1.2.3")
                    "via EMO v1.2.3"))

;; ascii fallback: on the ascii tier the mnemonic is swapped in, the "via ... v"
;; frame unchanged -- so the fallback fires on a poor glyph terminal.
(test-assert "render: the ascii mnemonic is the fallback on a poor glyph tier"
  (string-contains? (render-one-tool fake-tool 'ascii 'mono "1.2.3")
                    "via ASC v1.2.3"))

;; nerd opt-in ON: only under the opt-in does the nerd glyph appear.
(test-assert "render: the nerd glyph appears under the opt-in"
  (parameterize ([nerd-glyphs? #t])
    (string-contains? (render-one-tool fake-tool 'emoji 'mono "1.2.3")
                      "via NRD v1.2.3")))

;; nerd opt-in OFF is the default -- a Nerd Font is never assumed -- so a default
;; draw NEVER shows the nerd glyph.  A renderer defaulting to nerd fails here.
(test-equal "render: the Nerd-Font opt-in defaults off" #f (nerd-glyphs?))
(test-assert "render: a default draw never shows the nerd glyph"
  (not (string-contains? (render-one-tool fake-tool 'emoji 'mono "1.2.3") "NRD")))

;; colour ON: a non-mono depth AND a true fd-1 verdict wrap the body in an SGR
;; (an ESC controlsequence introducer).  A fixed-plain renderer fails this row.
(test-assert "render: colour under a non-mono depth and a true fd-1 verdict"
  (parameterize ([prompt-colour-ok? (lambda () #t)])
    (string-contains? (render-one-tool fake-tool 'emoji '256 "1.2.3") "\x1b;[")))

;; MONO: a mono depth yields the plain body with no SGR even when fd 1 would
;; permit colour -- so a mono terminal gets no stray escape.  A renderer that
;; gated on the verdict alone (ignoring the depth) would emit an ESC and fail.
(test-assert "render: a mono depth is plain even when fd 1 permits colour"
  (parameterize ([prompt-colour-ok? (lambda () #t)])
    (no-esc? (render-one-tool fake-tool 'emoji 'mono "1.2.3"))))

;; sanitisation: a control byte in the process-controlled version string is
;; stripped before display, so an ESC never reaches the output.  An unsanitised
;; renderer leaks the ESC and fails.
(test-assert "render: an embedded ESC in the version is stripped"
  (no-esc? (render-one-tool fake-tool 'emoji 'mono "1.2\x1b;3")))

;; === tool-glyph: the full glyph-choice truth table ===
;;
;; tool-glyph picks the glyph directly, so the rows below pin each branch: the
;; ascii tier wins outright, then the nerd glyph under the opt-in when the
;; descriptor carries one, else the emoji default.  A second fake WITHOUT a nerd
;; glyph proves the opt-in falls back to the tier default when the descriptor
;; carries none (never assumed).
(define fake-no-nerd
  (make-prompt-tool "fnn" '(("fnn.marker") . ()) "fnn"
                    '("fnn" "--version") parse-id "EMO" "ASC" #f))

(test-equal "glyph: opt-in off, emoji tier -> the emoji"
  "EMO" (tool-glyph fake-tool 'emoji))
(test-equal "glyph: opt-in off, ascii tier -> the ascii mnemonic"
  "ASC" (tool-glyph fake-tool 'ascii))
(test-equal "glyph: opt-in on with a nerd glyph -> the nerd glyph (emoji tier)"
  "NRD" (parameterize ([nerd-glyphs? #t]) (tool-glyph fake-tool 'emoji)))
;; The ascii tier OUTRANKS the opt-in.  That tier is returned only for TERM=linux,
;; TERM=dumb, HAFOD_ASCII or an explicit override -- terminals known not to render
;; non-ASCII -- and a nerd glyph is a private-use-area code point, so honouring
;; the opt-in there paints mojibake on exactly the terminals the fallback exists
;; to protect.  A picker that let nerd win regardless of tier fails this row.
(test-equal "glyph: the ascii tier outranks the nerd opt-in (no PUA on a weak terminal)"
  "ASC" (parameterize ([nerd-glyphs? #t]) (tool-glyph fake-tool 'ascii)))
(test-equal "glyph: opt-in on but NO nerd glyph -> the emoji default (never assumed)"
  "EMO" (parameterize ([nerd-glyphs? #t]) (tool-glyph fake-no-nerd 'emoji)))
(test-equal "glyph: opt-in on but NO nerd glyph, ascii tier -> the ascii mnemonic"
  "ASC" (parameterize ([nerd-glyphs? #t]) (tool-glyph fake-no-nerd 'ascii)))

;; === render-one-tool: the layout is identical across tiers ===
;;
;; Only the glyph swaps between the emoji and ascii tiers -- the "via <glyph> vX"
;; frame is unchanged.  Pinning both full bodies proves a renderer that reshaped
;; the frame per tier would fail.
(test-equal "render: the emoji body is exactly the framed emoji glyph"
  "via EMO v1.2.3" (render-one-tool fake-tool 'emoji 'mono "1.2.3"))
(test-equal "render: the ascii body is the SAME frame with only the glyph swapped"
  "via ASC v1.2.3" (render-one-tool fake-tool 'ascii 'mono "1.2.3"))

;; === render-one-tool: colour needs the fd-1 verdict, not the depth alone ===
;;
;; A non-mono depth with a FALSE fd-1 verdict is still plain -- colour needs BOTH
;; a non-mono depth AND the injectable verdict (prompt-colour-ok?), the same
;; discipline as the git and exit segments.  So the string capture port the
;; prompt hook rebinds -- a non-colour sink -- gets no stray SGR.  A renderer
;; that coloured on the depth alone would emit an ESC here and fail.
(test-assert "render: a non-mono depth with a false fd-1 verdict stays plain"
  (parameterize ([prompt-colour-ok? (lambda () #f)])
    (no-esc? (render-one-tool fake-tool 'emoji '256 "1.2.3"))))

;; === render-one-tool: both the version AND the glyph are sanitised ===
;;
;; A carriage return in the version is stripped (not only ESC), and a control
;; byte in the repo-controlled glyph descriptor is stripped too -- so neither a
;; process-printed nor a marker-supplied control byte reaches the terminal.
(test-assert "render: an embedded CR in the version is stripped"
  (not (string-contains? (render-one-tool fake-tool 'emoji 'mono "1.2\x0d;3") "\x0d;")))
(test-assert "render: a control byte in the glyph descriptor is stripped"
  (let ([evil (make-prompt-tool "evil" '(("evil.marker") . ()) "evil"
                                '("evil" "--version") parse-id "X\x1b;Y" "x" #f)])
    (no-esc? (render-one-tool evil 'emoji 'mono "1.2.3"))))

;; ======================================================================
;; The detection / PATH-gate / cache / probe engine
;; ======================================================================
;;
;; From here the proofs drive the live engine against FAKE tools -- real host
;; binaries (echo / true / sleep / sh) wrapped in a prompt-tool descriptor -- and
;; marker-fixture temp directories, with version-probe-count (REAL spawns) as the
;; oracle, so NO real toolchain is required.  The idiom (unique temp dir, run EPF
;; teardown, the setenv-free direct-cwd form) mirrors the sibling prompt-cache
;; suite.

;; A unique temp path under $TMPDIR (or /tmp), keyed by pid and a random suffix so
;; concurrent runs never collide.  Outside any project tree, so a directory-list
;; of it finds only the markers the test plants.
(define (temp-dir-name tag)
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/hafod-prompt-version-" tag "-"
                 (number->string (pid)) "-"
                 (number->string (random 1000000))))

;; Write CONTENT to PATH (each path is fresh under a unique temp dir).
(define (write-file path content)
  (call-with-output-file path (lambda (p) (put-string p content))))

;; === Marker detection is one current-directory scan, no walk-up ===
;;
;; detect-tools-in reads the cwd ONCE and returns the registered tools whose
;; markers -- a filename OR an extension -- are present, in registry (= draw)
;; order, consulting the CURRENT directory only.  A naive impl that walked up the
;; tree, or matched on a substring, fails these rows.  parse is irrelevant to
;; detection (no spawn here).
(define det-file-tool
  (make-prompt-tool "ftool" '(("ftool.marker") . ()) "ftool"
                    (list "ftool" "--version") parse-version/common "F" "f" #f))
(define det-ext-tool
  (make-prompt-tool "xtool" '(() . (".xyz")) "xtool"
                    (list "xtool" "--version") parse-version/common "X" "x" #f))

;; A filename marker present -> the filename tool is detected.
(let ([dir (temp-dir-name "det-file")])
  (create-directory dir)
  (write-file (string-append dir "/ftool.marker") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list det-file-tool det-ext-tool)])
        (test-equal "detect: a filename marker detects its tool (current dir only)"
          '("ftool") (map prompt-tool-name (detect-tools-in dir)))))
    (lambda () (run (rm "-rf" ,dir)))))

;; A file carrying a marker EXTENSION present -> the extension tool is detected.
(let ([dir (temp-dir-name "det-ext")])
  (create-directory dir)
  (write-file (string-append dir "/main.xyz") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list det-file-tool det-ext-tool)])
        (test-equal "detect: a file with a marker extension detects its tool"
          '("xtool") (map prompt-tool-name (detect-tools-in dir)))))
    (lambda () (run (rm "-rf" ,dir)))))

;; An unrelated dir detects nothing; both markers present detect both in registry
;; order (so draw order is preserved).
(let ([dir (temp-dir-name "det-none")])
  (create-directory dir)
  (write-file (string-append dir "/unrelated.txt") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list det-file-tool det-ext-tool)])
        (test-equal "detect: an unrelated dir detects nothing"
          '() (detect-tools-in dir))
        (write-file (string-append dir "/ftool.marker") "x")
        (write-file (string-append dir "/main.xyz") "x")
        (test-equal "detect: both markers present detect both, in registry order"
          '("ftool" "xtool") (map prompt-tool-name (detect-tools-in dir)))))
    (lambda () (run (rm "-rf" ,dir)))))

;; An absent directory is a guarded empty result, never a raise.
(test-equal "detect: an absent directory detects nothing (guarded)"
  '() (parameterize ([prompt-tools (list det-file-tool det-ext-tool)])
        (detect-tools-in (temp-dir-name "det-absent-never-created"))))

;; NO walk-up: a marker in the PARENT is not seen from a marker-free child.
(let ([parent (temp-dir-name "det-parent")])
  (create-directory parent)
  (write-file (string-append parent "/ftool.marker") "x")
  (let ([child (string-append parent "/child")])
    (create-directory child)
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([prompt-tools (list det-file-tool det-ext-tool)])
          (test-equal "detect: a parent-only marker is NOT seen from the child (no walk-up)"
            '() (detect-tools-in child))))
      (lambda () (run (rm "-rf" ,parent))))))

;; === The PATH gate reads the classifier path-cache, no spawn ===
;;
;; tool-on-path? is #t exactly when the tool's command is a key in the path-cache
;; -- an O(1) read, no spawn.  Seeding the cache for one command and leaving
;; another unseeded proves the gate without a real binary.
(let ()
  (hashtable-set! (path-cache) "on-path-cmd" #t)
  (let ([on  (make-prompt-tool "on" '(("on.marker") . ()) "on-path-cmd"
                               (list "on-path-cmd") parse-version/common "O" "o" #f)]
        [off (make-prompt-tool "off" '(("off.marker") . ()) "hafod-no-such-tool-xyzzy"
                               (list "hafod-no-such-tool-xyzzy")
                               parse-version/common "F" "f" #f)])
    (test-assert "gate: a command in the path-cache is on PATH"
      (tool-on-path? on))
    (test-assert "gate: a command absent from the path-cache is NOT on PATH"
      (not (tool-on-path? off)))))

;; === The per-cwd cache: detect -> PATH-gate -> probe -> render, cached ===
;;
;; cached-version-segment drives the whole engine.  These oracles use a fake tool
;; run through a real `echo`, printing a version-shaped token its parse extracts,
;; with version-probe-count (REAL spawns) as the oracle -- so no real toolchain is
;; needed.  Each resets the count first.  The PATH gate reads the classifier
;; path-cache, which is DECOUPLED from the OS PATH the spawn itself resolves
;; against, so the gate is seeded explicitly.

;; echo resolved to an absolute path where present, else the bare name (PATH-
;; resolved, exactly as the sibling timeout suite relies on for echo/true).
(define echo-bin
  (cond [(file-exists? "/bin/echo") "/bin/echo"]
        [(file-exists? "/usr/bin/echo") "/usr/bin/echo"]
        [else "echo"]))

;; A fake tool: MARKER present in the cwd gates it, echo-bin is its (on-PATH)
;; command, and TOKEN is echoed so parse-version/common extracts the version.
(define (echo-tool name marker token)
  (make-prompt-tool name (cons (list marker) '()) echo-bin
                    (list echo-bin token) parse-version/common "T" "t" #f))

;; Seed the PATH gate for the echo command (once; harmless to leave seeded).
(hashtable-set! (path-cache) echo-bin #t)

;; A marked dir renders a segment; an EMPTY dir spawns NOTHING.  A detection-blind
;; engine would probe the on-PATH echo tool in the empty dir and fail the second.
(let ([dir (temp-dir-name "ver-mark")])
  (create-directory dir)
  (write-file (string-append dir "/fake.marker") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list (echo-tool "fake" "fake.marker" "Fake 9.9.9"))])
        (version-probe-count 0)
        (let ([seg (cached-version-segment dir 'emoji 'mono)])
          (test-assert "cache: a marked dir with an on-PATH tool renders its version"
            (string-contains? seg "9.9.9"))
          (test-equal "cache: the marked dir spawned exactly once"
            1 (version-probe-count)))))
    (lambda () (run (rm "-rf" ,dir)))))

(let ([dir (temp-dir-name "ver-empty")])
  (create-directory dir)
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list (echo-tool "fake" "fake.marker" "Fake 9.9.9"))])
        (version-probe-count 0)
        (let ([seg (cached-version-segment dir 'emoji 'mono)])
          (test-equal "cache: an empty dir detects nothing and renders \"\"" "" seg)
          (test-equal "cache: an empty dir spawns NOTHING (detection-gated)"
            0 (version-probe-count)))))
    (lambda () (run (rm "-rf" ,dir)))))

;; cache hit / miss: a repeat draw at the same marker mtime is a hit (probe-count
;; flat), which also proves the rendered group was cached against its stamp; a
;; set-file-times bump invalidates so the next draw re-probes.  A cwd-only key
;; that never invalidated would fail the mtime-bump row.
(let ([dir (temp-dir-name "ver-hit")])
  (create-directory dir)
  (write-file (string-append dir "/fake.marker") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list (echo-tool "fake" "fake.marker" "Fake 9.9.9"))])
        (version-probe-count 0)
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "cache: the first draw probes once" 1 (version-probe-count))
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "cache: an unchanged marker mtime is a hit (no re-probe)"
          1 (version-probe-count))
        (set-file-times (string-append dir "/fake.marker") 1000000000 1000000000)
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "cache: a marker mtime bump invalidates (re-probe)"
          2 (version-probe-count))))
    (lambda () (run (rm "-rf" ,dir)))))

;; === One malformed tool costs its own segment, not the whole group ===
;;
;; register-prompt-tool! now rejects a malformed descriptor, but prompt-tools is a
;; plain parameter that a direct set can fill with anything the record constructor
;; accepts -- so the engine must survive one anyway.  A descriptor whose markers
;; are a bare string (the detector calls car on it) sits FIRST, ahead of a working
;; tool.  Without the per-tool guard the raise escapes detection, escapes the
;; cached segment, and the prompt loses the working tool along with the broken
;; one, so this row is non-vacuous.
(let ([dir (temp-dir-name "ver-broken")])
  (create-directory dir)
  (write-file (string-append dir "/fake.marker") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools
                      (list (make-prompt-tool "broken" "not-a-marker-pair" "broken"
                                              (list "broken" "--version")
                                              parse-version/common "B" "b" #f)
                            (echo-tool "fake" "fake.marker" "Fake 9.9.9"))])
        (test-assert "isolate: a malformed tool does not delete the working ones"
          (string-contains? (cached-version-segment dir 'emoji 'mono) "9.9.9"))))
    (lambda () (run (rm "-rf" ,dir)))))

;; === The probe spawns through the project's ONE child-environment path ===
;;
;; (hafod environment) already owns how hafod composes a child environment: a
;; dirty-flag-invalidated KEY=VALUE list that every spawn site in (hafod process)
;; shares, so an unchanged-environment spawn loop rebuilds once rather than once
;; per spawn.  A probe that re-derived the same list by hand allocated a string
;; per variable per probe and, worse, left a second implementation for a future
;; change to miss.
;;
;; envp-rebuild-observer fires only when that shared list is really rebuilt, so
;; it is the oracle: dirty the environment, run a probe, and the observer must
;; have fired.  A probe composing its own environment never touches the shared
;; list and leaves the count at zero -- non-vacuous.  The second leg pins the
;; point of sharing it: a second probe with the environment untouched rebuilds
;; nothing.
(let ([dir (temp-dir-name "ver-envpath")]
      [rebuilds 0])
  (create-directory dir)
  (write-file (string-append dir "/fake.marker") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list (echo-tool "fake" "fake.marker" "Fake 9.9.9"))]
                     [envp-rebuild-observer (lambda () (set! rebuilds (+ rebuilds 1)))])
        (setenv "HAFOD_ENVPATH_SENTINEL" "1")     ; dirties the shared list
        (cached-version-segment dir 'emoji 'mono)
        (test-assert "envpath: the probe builds its child environment through the shared path"
          (>= rebuilds 1))
        (let ([after-first rebuilds])
          (set-file-times (string-append dir "/fake.marker") 1000000000 1000000000)
          (cached-version-segment dir 'emoji 'mono)
          (test-equal "envpath: a second probe with the environment untouched rebuilds nothing"
            after-first rebuilds))
        (setenv "HAFOD_ENVPATH_SENTINEL" #f)))
    (lambda () (run (rm "-rf" ,dir)))))

;; === A repeat draw in an unchanged directory does no filesystem scan ===
;;
;; Detection is a full directory-list plus a filename table plus an extension
;; table -- work whose cost is unbounded in the size of the directory -- and a
;; prompt is drawn after every command.  Running it unconditionally meant that
;; simply sitting in a downloads directory or a node_modules paid for the whole
;; listing on every draw, cache hit or miss; the cache saved only the spawns.  A
;; scan-per-draw engine reports three after three draws, so the second row is
;; non-vacuous.  The directory's mtime is then moved explicitly (rather than by
;; adding a file and hoping the second ticks over) so the re-scan row is
;; deterministic.
(let ([dir (temp-dir-name "ver-scan")])
  (create-directory dir)
  (write-file (string-append dir "/fake.marker") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list (echo-tool "fake" "fake.marker" "Fake 9.9.9"))])
        (version-detect-count 0)
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "scan: the first draw scans the directory once"
          1 (version-detect-count))
        (cached-version-segment dir 'emoji 'mono)
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "scan: repeat draws in an unchanged directory never scan again"
          1 (version-detect-count))
        (set-file-times dir 1000000000 1000000000)
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "scan: a changed directory mtime scans again"
          2 (version-detect-count))
        ;; A registry change re-detects too, whatever the directory's mtime: the
        ;; reused set is a function of the registry as much as of the entries.
        (parameterize ([prompt-tools (list (echo-tool "other" "fake.marker" "Fake 8.8.8"))])
          (cached-version-segment dir 'emoji 'mono)
          (test-equal "scan: a registry change re-scans on an unchanged directory"
            3 (version-detect-count)))))
    (lambda () (run (rm "-rf" ,dir)))))

;; === The detection-reuse window is NOT the probe cache's TTL ===
;;
;; The reuse condition was "the entry is fresh AND the directory mtime is
;; unchanged", where freshness meant version-cache-ttl-ms -- thirty seconds by
;; default.  safe-mtime yields whole epoch SECONDS, so a marker created in the
;; same wall-clock second as the entry's own stat leaves the directory mtime
;; equal, and equal it then stays for ever, because nothing else touches the
;; directory.  The stale detection was therefore served for the whole TTL rather
;; than for the sub-second stat gap the reuse was meant to cover: `cargo init` in
;; the directory one is standing in left the group blank for half a minute, and a
;; marker DELETION kept it probing a tool that had gone.
;;
;; The real-scan counter is the oracle.  Two draws in quick succession scan once
;; (the window is open); a draw a second and a bit later scans again, although
;; neither the directory nor the registry has changed and the TTL has nowhere near
;; expired -- the row a TTL-tied reuse fails.  The draw after THAT reuses again,
;; which is the other half of the contract: the window is refreshed by the scan,
;; so the directory is not re-listed on every draw for ever after.  The mtime is
;; pinned to a fixed past value first so the directory cannot tick underneath the
;; proof, and the TTL is set explicitly so the two windows are visibly different
;; numbers.
(let ([dir (temp-dir-name "ver-reuse-window")])
  (create-directory dir)
  (write-file (string-append dir "/fake.marker") "x")
  (set-file-times dir 1000000000 1000000000)
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list (echo-tool "fake" "fake.marker" "Fake 9.9.9"))]
                     [version-cache-ttl-ms 30000])
        (version-detect-count 0)
        (cached-version-segment dir 'emoji 'mono 200)
        (cached-version-segment dir 'emoji 'mono 200)
        (test-equal "reuse: two draws inside the window scan the directory once"
          1 (version-detect-count))
        (sleep (make-time 'time-duration 200000000 1))   ; 1.2 s: past the window
        (cached-version-segment dir 'emoji 'mono 200)
        (test-equal "reuse: a draw past the window re-scans, TTL or no TTL"
          2 (version-detect-count))
        (cached-version-segment dir 'emoji 'mono 200)
        (test-equal "reuse: the re-scan refreshes the window (no scan per draw)"
          2 (version-detect-count))))
    (lambda () (run (rm "-rf" ,dir)))))

;; === Registering a tool takes effect in the directory you are standing in ===
;;
;; Registering at the REPL is a thing users do, and the natural expectation is
;; that the new segment appears at the next prompt.  A cache keyed only on marker
;; mtimes could not see it: a tool detected purely by EXTENSION contributes no
;; marker file to the stamp, so registering one while sitting in a matching
;; directory left the stamp identical, the lookup a hit, and the new tool
;; invisible until the directory changed.  The registry write path therefore
;; drops the cached groups.
;;
;; The fixture is exactly that shape: an already-cached directory holding only a
;; ".xyz" file, into which an extension-detected tool is registered.  A registry
;; write that did not invalidate re-serves the cached empty group and never
;; probes, failing the second row: non-vacuous.
(let ([dir (temp-dir-name "ver-register")])
  (create-directory dir)
  (write-file (string-append dir "/main.xyz") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools '()])
        (version-probe-count 0)
        (test-equal "register: the empty registry caches an empty group here"
          "" (cached-version-segment dir 'emoji 'mono))
        (register-prompt-tool! "xecho" '(() . (".xyz")) echo-bin
                               (list echo-bin "Ext 5.5.5") parse-version/common
                               "X" "x")
        (test-assert "register: a tool registered at the REPL shows up in this very directory"
          (string-contains? (cached-version-segment dir 'emoji 'mono) "5.5.5"))
        (test-equal "register: and it really did probe (not a stale render)"
          1 (version-probe-count))
        ;; The named removal takes the segment away again, just as promptly.
        (unregister-prompt-tool! "xecho")
        (test-equal "register: unregistering takes the segment away again"
          "" (cached-version-segment dir 'emoji 'mono))))
    (lambda () (run (rm "-rf" ,dir)))))

;; === A toolchain SWITCH invalidates, even though it touches no marker ===
;;
;; The event this segment exists to report -- `nvm use 20`, `pyenv shell 3.12`,
;; `rustup default nightly` -- changes the environment and nothing else: no
;; marker file moves, so a marker-mtime-only stamp keeps serving the pre-switch
;; version for the rest of the session.  Moving a watched variable with the
;; markers untouched is exactly that shape, and a marker-only stamp fails the
;; third row (a hit, no re-probe): non-vacuous.  The second row is the control --
;; an unchanged environment must still be a hit, or the stamp would merely have
;; broken caching altogether.  RUSTUP_TOOLCHAIN is restored afterwards.
(let ([dir (temp-dir-name "ver-env")])
  (create-directory dir)
  (write-file (string-append dir "/fake.marker") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list (echo-tool "fake" "fake.marker" "Fake 9.9.9"))])
        (setenv "RUSTUP_TOOLCHAIN" #f)
        (version-probe-count 0)
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "env: the first draw probes once" 1 (version-probe-count))
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "env: an unchanged environment is still a hit"
          1 (version-probe-count))
        (setenv "RUSTUP_TOOLCHAIN" "hafod-test-nightly")
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "env: a toolchain switch invalidates though no marker moved"
          2 (version-probe-count))
        (setenv "RUSTUP_TOOLCHAIN" #f)
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "env: switching back invalidates again"
          3 (version-probe-count))))
    (lambda () (run (rm "-rf" ,dir)))))

;; === The group is trimmed to the columns it is given ===
;;
;; Each rendered tool costs "via " + glyph + " v" + version -- thirteen to
;; sixteen columns with a double-width emoji -- and the info line is never
;; truncated, so an unbudgeted group of four wraps an 80-column terminal onto a
;; second row and breaks the right prompt's single-row column positioning along
;; with it.  The width argument is the columns THIS GROUP may occupy, which is
;; what the per-draw context carries and what the segment renderer narrows to the
;; free part of the row.
;;
;; Three fake tools render "via T vN.N.N", twelve columns each, so wide asks for
;; thirty-eight and narrow leaves room for one.  A width-blind group emits all
;; three at both widths, failing the narrow rows: non-vacuous.
(let ([dir (temp-dir-name "ver-width")])
  (create-directory dir)
  (for-each
    (lambda (n) (write-file (string-append dir "/w" n ".marker") "x"))
    '("1" "2" "3"))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list (echo-tool "w1" "w1.marker" "Aaa 1.1.1")
                                         (echo-tool "w2" "w2.marker" "Bbb 2.2.2")
                                         (echo-tool "w3" "w3.marker" "Ccc 3.3.3"))])
        (let ([wide (cached-version-segment dir 'emoji 'mono 200)])
          (test-assert "width: a wide budget carries every detected tool"
            (and (string-contains? wide "1.1.1")
                 (string-contains? wide "2.2.2")
                 (string-contains? wide "3.3.3"))))
        (let ([narrow (cached-version-segment dir 'emoji 'mono 20)])
          (test-assert "width: a narrow budget keeps the first tool"
            (string-contains? narrow "1.1.1"))
          (test-assert "width: a narrow budget drops the tools that do not fit"
            (not (string-contains? narrow "3.3.3")))
          (test-assert "width: the group stays inside the columns it was given"
            (<= (ansi-visible-length narrow) 20)))
        ;; No columns left is a real answer, not a bad argument: the group is
        ;; dropped rather than falling back to some share of the terminal.
        (test-equal "width: a spent budget renders nothing"
          "" (cached-version-segment dir 'emoji 'mono 0))))
    (lambda () (run (rm "-rf" ,dir)))))

;; === Only a NEW directory can overflow the table ===
;;
;; The overflow check ran before every insert, including one that merely
;; overwrote a key already present and so could not grow the table.  Once a
;; session had visited the cap's worth of directories, every marker touch in an
;; already-cached one wiped all of them -- and refilling this cache is a whole
;; tool group per directory, not one spawn.
;;
;; With the cap at two, the third draw below re-renders a directory the table
;; already holds.  A check-before-every-insert engine clears the table there and
;; then has to re-probe the other directory, costing a fourth spawn; the fixed
;; rule leaves both entries in place and the fourth draw is a hit.  Two dirs,
;; four draws, three spawns: non-vacuous.
(let ([first  (temp-dir-name "ver-cap-a")]
      [second (temp-dir-name "ver-cap-b")])
  (for-each
    (lambda (d)
      (create-directory d)
      (write-file (string-append d "/fake.marker") "x"))
    (list first second))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list (echo-tool "fake" "fake.marker" "Fake 9.9.9"))]
                     [version-cache-cap 2])
        (version-probe-count 0)
        ;; The first of these is itself a new key into an over-cap table, so it
        ;; clears and the pair below are the only two entries either way.
        (cached-version-segment first 'emoji 'mono)
        (cached-version-segment second 'emoji 'mono)
        (test-equal "cap: the two directories probe once each" 2 (version-probe-count))
        ;; Re-render the FIRST: an update, not an insert, so nothing is evicted.
        (set-file-times (string-append first "/fake.marker") 1000000000 1000000000)
        (cached-version-segment first 'emoji 'mono)
        (test-equal "cap: re-rendering a cached directory costs one probe"
          3 (version-probe-count))
        (cached-version-segment second 'emoji 'mono)
        (test-equal "cap: and it did not evict the other directory"
          3 (version-probe-count))))
    (lambda ()
      (run (rm "-rf" ,first))
      (run (rm "-rf" ,second)))))

;; === A cached group ages out, so an in-place upgrade self-heals ===
;;
;; `apt upgrade python3` / `rustup update` move neither a marker nor a watched
;; variable, so nothing the stamp can see changes -- the age limit is the only
;; thing that eventually re-probes.  Forcing the limit to zero proves an expired
;; entry re-probes with an unchanged stamp (a cache with no age limit stays at
;; one: non-vacuous); the generous-limit control beside it proves the entry is
;; still served while it is fresh.
;; Each leg gets its OWN directory: the two share a cache key otherwise, and the
;; entry the first leg leaves behind is fresh enough to serve the second before
;; it has probed at all.
(let ([expired (temp-dir-name "ver-ttl-expired")]
      [fresh   (temp-dir-name "ver-ttl-fresh")])
  (for-each
    (lambda (d)
      (create-directory d)
      (write-file (string-append d "/fake.marker") "x"))
    (list expired fresh))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list (echo-tool "fake" "fake.marker" "Fake 9.9.9"))])
        (parameterize ([version-cache-ttl-ms 0])
          (version-probe-count 0)
          (cached-version-segment expired 'emoji 'mono)
          (cached-version-segment expired 'emoji 'mono)
          (test-equal "age: an expired entry re-probes on an unchanged stamp"
            2 (version-probe-count)))
        (parameterize ([version-cache-ttl-ms 600000])
          (version-probe-count 0)
          (cached-version-segment fresh 'emoji 'mono)
          (cached-version-segment fresh 'emoji 'mono)
          (test-equal "age: a fresh entry is served without re-probing"
            1 (version-probe-count)))))
    (lambda ()
      (run (rm "-rf" ,expired))
      (run (rm "-rf" ,fresh)))))

;; at-most-once: two markers for two on-PATH tools probe twice on the first draw
;; and not again on the second -- each detected on-PATH tool probed at most once
;; per cwd.  A gate-after-spawn engine would keep re-probing on the second draw.
(let ([dir (temp-dir-name "ver-two")])
  (create-directory dir)
  (write-file (string-append dir "/a.marker") "x")
  (write-file (string-append dir "/b.marker") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools (list (echo-tool "a" "a.marker" "A 1.1.1")
                                         (echo-tool "b" "b.marker" "B 2.2.2"))])
        (version-probe-count 0)
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "cache: two detected on-PATH tools probe twice on the first draw"
          2 (version-probe-count))
        (cached-version-segment dir 'emoji 'mono)
        (test-equal "cache: the second draw re-probes neither (at most once per cwd)"
          2 (version-probe-count))))
    (lambda () (run (rm "-rf" ,dir)))))

;; uninstalled -> no spawn: a detected tool whose command is ABSENT from the
;; path-cache is declined by the PATH gate BEFORE any spawn -- so a marker-present-
;; but-uninstalled tool contributes no segment and probe-count stays 0.
(let ([dir (temp-dir-name "ver-uninst")])
  (create-directory dir)
  (write-file (string-append dir "/u.marker") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools
                      (list (make-prompt-tool "u" '(("u.marker") . ())
                                              "hafod-no-such-tool-xyzzy"
                                              (list "hafod-no-such-tool-xyzzy")
                                              parse-version/common "U" "u" #f))])
        (version-probe-count 0)
        (let ([seg (cached-version-segment dir 'emoji 'mono)])
          (test-equal "cache: a marker-present-but-uninstalled tool renders \"\"" "" seg)
          (test-equal "cache: an uninstalled tool is NOT spawned (PATH-gated)"
            0 (version-probe-count)))))
    (lambda () (run (rm "-rf" ,dir)))))

;; === A mis-behaved binary is bounded, killed and reaped; the empty group cached ===
;;
;; A tool whose probe never terminates (`sleep`) must be bounded by the spawn
;; timeout, killed and reaped (no zombie), its empty result cached so a second
;; draw does NOT re-spawn.  prompt-spawn-timeout-ms is set small; the wall-clock
;; bound is the load-bearing half -- an unbounded read would sit for the full
;; sleep.  Self-skips where no absolute `sleep` is present (mirroring the sibling
;; timeout suite).

;; Reap and count every already-finished child of this process (WNOHANG); zero
;; leftover proves the timed-out probe reaped its own child rather than leaking a
;; zombie.  Copied verbatim from the sibling timeout suite.
(define (drain-finished-children)
  (let loop ([n 0])
    (let ([r (guard (e [#t 'none])
               (let-values ([(w s) (posix-waitpid -1 wait/poll)]) w))])
      (cond
        [(eq? r 'none) n]
        [(and (integer? r) (> r 0)) (loop (+ n 1))]
        [else n]))))

(define sleep-bin
  (cond [(file-exists? "/bin/sleep") "/bin/sleep"]
        [(file-exists? "/usr/bin/sleep") "/usr/bin/sleep"]
        [else #f]))

(when sleep-bin
  (hashtable-set! (path-cache) sleep-bin #t)
  (let ([dir (temp-dir-name "ver-slow")])
    (create-directory dir)
    (write-file (string-append dir "/slow.marker") "x")
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([prompt-tools
                        (list (make-prompt-tool "slow" '(("slow.marker") . ())
                                                sleep-bin (list sleep-bin "5")
                                                parse-version/common "S" "s" #f))]
                       [prompt-spawn-timeout-ms 100])
          (version-probe-count 0)
          (drain-finished-children)
          (let* ([t0  (real-time)]
                 [seg (cached-version-segment dir 'emoji 'mono)]
                 [elapsed (- (real-time) t0)])
            (test-equal "timeout: a hung probe yields an empty segment" "" seg)
            (test-assert "timeout: the probe is bounded near the deadline (not the full sleep)"
              (< elapsed 2000))
            (test-equal "timeout: the killed child is reaped (no zombie)"
              0 (drain-finished-children))
            (test-equal "timeout: the hung probe spawned once" 1 (version-probe-count))
            ;; a second draw at the same marker mtime is a cached "" -> NO re-spawn
            (cached-version-segment dir 'emoji 'mono)
            (test-equal "timeout: the empty result is cached (no re-spawn storm)"
              1 (version-probe-count)))))
      (lambda () (run (rm "-rf" ,dir))))))

;; === The group holds ONE wall-clock budget and a fan-out cap ===
;;
;; The probes run SERIALLY inside the prompt draw, so a per-probe deadline alone
;; multiplies by the number of detected tools.  Six directories' worth of hung
;; tools, each given the full per-probe deadline, is what a polyglot directory
;; costs on every cache miss.  Both oracles below fail on a per-probe-only bound:
;; it spawns all six (the cap says at most four) and pays six deadlines plus six
;; kill graces (the budget says one budget's worth).  The probe count is the
;; deterministic half; the elapsed bound sits well under the six-deadline cost so
;; a loaded machine does not make it flaky.  Self-skips where no absolute `sleep`
;; is present.
(when sleep-bin
  (let ([dir (temp-dir-name "ver-budget")]
        [ns  '(1 2 3 4 5 6)])
    (create-directory dir)
    (for-each
      (lambda (n)
        (write-file (string-append dir "/slow" (number->string n) ".marker") "x"))
      ns)
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([prompt-tools
                        (map (lambda (n)
                               (let ([m (string-append "slow" (number->string n)
                                                       ".marker")])
                                 (make-prompt-tool m (cons (list m) '())
                                                   sleep-bin (list sleep-bin "5")
                                                   parse-version/common "S" "s" #f)))
                             ns)]
                       [prompt-spawn-timeout-ms 100])
          (version-probe-count 0)
          (drain-finished-children)
          (let* ([t0      (real-time)]
                 [seg     (cached-version-segment dir 'emoji 'mono)]
                 [elapsed (- (real-time) t0)])
            (test-equal "budget: six hung tools still render an empty group" "" seg)
            (test-assert "budget: the group probes at most four tools, not all six"
              (<= (version-probe-count) 4))
            (test-assert "budget: the whole group is bounded by ONE wall-clock budget"
              (< elapsed 600))
            (test-equal "budget: every killed child is reaped (no zombie)"
              0 (drain-finished-children)))))
      (lambda () (run (rm "-rf" ,dir))))))

;; === The PATH gate runs BEFORE the fan-out cap ===
;;
;; The gate is an O(1) path-cache read with no spawn and no stat, so declining an
;; uninstalled tool is free -- but trimming the DETECTED list to four ahead of the
;; gate spent a probe slot on each of those free declines, and an installed tool
;; sitting behind four uninstalled ones was never reached at all.  Five markers,
;; four commands absent from the path-cache and the fifth present: a cap-then-gate
;; group renders nothing and probes nothing, so both rows below fail on it.
(let ([dir (temp-dir-name "ver-gate-order")])
  (create-directory dir)
  (for-each (lambda (n) (write-file (string-append dir "/g" n ".marker") "x"))
            '("1" "2" "3" "4" "5"))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools
                      (append
                        (map (lambda (n)
                               (let ([m   (string-append "g" n ".marker")]
                                     [cmd (string-append "hafod-absent-tool-" n)])
                                 (make-prompt-tool m (cons (list m) '()) cmd
                                                   (list cmd) parse-version/common
                                                   "A" "a" #f)))
                             '("1" "2" "3" "4"))
                        (list (echo-tool "g5" "g5.marker" "Ghost 5.5.5")))])
        (version-probe-count 0)
        (let ([seg (cached-version-segment dir 'emoji 'mono 200)])
          (test-assert "gate order: an installed tool behind four uninstalled ones renders"
            (string-contains? seg "5.5.5"))
          (test-equal "gate order: a free decline costs no probe slot"
            1 (version-probe-count)))))
    (lambda () (run (rm "-rf" ,dir)))))

;; === A half-shaped descriptor costs its own segment and nothing else ===
;;
;; register-prompt-tool! validates the marker shape, but prompt-tools is a plain
;; parameter a user (or a parameterize) may set directly, so a malformed
;; descriptor still reaches the engine.  The detector's match uses `exists`, which
;; short-circuits on the first hit: a marker list whose FIRST entry was a valid,
;; present filename was detected whatever followed it, and the marker STAMP then
;; walked the rest and appended a non-string to the cwd.  That raise left the
;; stamp, left the cache and left the segment thunk, to be swallowed by the draw's
;; blanket guard -- so the whole group, including the working tool registered
;; beside the broken one, went blank with no diagnostic.  Both rows fail on a tree
;; that guards only the detector and the render.
(let ([dir (temp-dir-name "ver-halfbad")])
  (create-directory dir)
  (write-file (string-append dir "/bad.marker") "x")
  (write-file (string-append dir "/good.marker") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools
                      (list (make-prompt-tool "halfbad"
                                              (cons (list "bad.marker" 42) '())
                                              echo-bin (list echo-bin "Bad 1.2.3")
                                              parse-version/common "B" "b" #f)
                            (echo-tool "good" "good.marker" "Good 4.5.6"))])
        (test-assert "half-shaped: the marker stamp does not raise out of the draw"
          (string? (cached-version-segment dir 'emoji 'mono 200)))
        (test-assert "half-shaped: the working tool beside it still renders"
          (string-contains? (cached-version-segment dir 'emoji 'mono 200) "4.5.6"))
        (test-assert "half-shaped: the broken descriptor renders nothing of its own"
          (not (string-contains? (cached-version-segment dir 'emoji 'mono 200)
                                 "1.2.3")))))
    (lambda () (run (rm "-rf" ,dir)))))

;; === A stderr-only banner is captured end-to-end (the merge disposition) ===
;;
;; A tool that writes its version ONLY to stderr (the shape of `java -version`) is
;; captured because the probe merges the child's stderr into its stdout, so the
;; version reaches the parse and the rendered segment.  Under the shipped
;; /dev/null discard it would be lost -- so this proves the merge reaches the
;; parse through the REAL probe path, not merely the with-spawn-timeout unit.
;; Self-skips where no POSIX sh is present.
(define sh-bin
  (cond [(file-exists? "/bin/sh") "/bin/sh"]
        [(file-exists? "/usr/bin/sh") "/usr/bin/sh"]
        [else #f]))

(when sh-bin
  (hashtable-set! (path-cache) sh-bin #t)
  (let ([dir (temp-dir-name "ver-stderr")])
    (create-directory dir)
    (write-file (string-append dir "/err.marker") "x")
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([prompt-tools
                        (list (make-prompt-tool "err" '(("err.marker") . ())
                                                sh-bin
                                                (list sh-bin "-c" "echo 9.9.9 1>&2")
                                                parse-version/common "E" "e" #f))])
          (version-probe-count 0)
          (let ([seg (cached-version-segment dir 'emoji 'mono)])
            (test-assert "stderr: a stderr-only banner reaches the segment (merge disposition)"
              (string-contains? seg "9.9.9"))
            (test-equal "stderr: the stderr probe spawned once" 1 (version-probe-count)))))
      (lambda () (run (rm "-rf" ,dir))))))

;; === The group has its own opt-out, separate from the whole prompt ===
;;
;; Merely changing into a directory runs every detected toolchain's version
;; command there, and several marker filenames -- .python-version, .ruby-version,
;; .nvmrc, .java-version, .php-version -- are exactly the files a version-manager
;; shim reads to decide WHICH toolchain to run.  So cloning an untrusted
;; repository and cd-ing in makes the user's own shim resolve a repository-chosen
;; version.  That is a defensible default, but the only escape hatch was
;; HAFOD_PROMPT=0, which turns off the entire informative prompt.
;;
;; Both legs of the narrow opt-out are pinned here, and the environment leg
;; follows HAFOD_PROMPT exactly: unset or non-falsy leaves the group on, so the
;; default needs no environment at all.
(test-assert "optout: the group is on by default"
  (begin (setenv "HAFOD_PROMPT_VERSIONS" #f) (version-group-enabled?)))
(test-assert "optout: (prompt-versions? #f) turns the group off"
  (not (parameterize ([prompt-versions? #f]) (version-group-enabled?))))
(for-each
  (lambda (v)
    (setenv "HAFOD_PROMPT_VERSIONS" v)
    (test-assert (string-append "optout: HAFOD_PROMPT_VERSIONS=\"" v "\" turns the group off")
      (not (version-group-enabled?))))
  '("0" "false" "no" "NO" "False" ""))
(for-each
  (lambda (v)
    (setenv "HAFOD_PROMPT_VERSIONS" v)
    (test-assert (string-append "optout: HAFOD_PROMPT_VERSIONS=\"" v "\" leaves the group on")
      (version-group-enabled?)))
  '("1" "yes" "true"))
(setenv "HAFOD_PROMPT_VERSIONS" #f)

;; === A runaway child is capped, in bytes read and in version length ===
;;
;; The read loop appends 4 KB chunks until EOF or the deadline, so a child that
;; writes fast can push megabytes into the accumulator -- which the finish then
;; materialises through utf8->string at four bytes per character, inside a prompt
;; draw.  Before the version group the only callee was `git status`; now it is any
;; interpreter or wrapper shim on the user's PATH.  The deadline here is set high
;; on purpose, so the byte ceiling is the only thing that can stop the read: an
;; uncapped loop swallows the whole dump and then reports the banner printed after
;; it, so the row is non-vacuous.
(when sh-bin
  (let ([dir (temp-dir-name "ver-flood")])
    (create-directory dir)
    (write-file (string-append dir "/flood.marker") "x")
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([prompt-tools
                        (list (make-prompt-tool "flood" '(("flood.marker") . ())
                                                sh-bin
                                                (list sh-bin "-c"
                                                      "head -c 400000 /dev/zero; echo 9.9.9")
                                                parse-version/common "F" "f" #f))]
                       [prompt-spawn-timeout-ms 4000])
          (version-probe-count 0)
          (test-equal "cap: a child that floods the pipe is cut short, not parsed"
            "" (cached-version-segment dir 'emoji 'mono))
          (test-equal "cap: the flooding probe still spawned once"
            1 (version-probe-count))))
      (lambda () (run (rm "-rf" ,dir))))))

;; The extracted version is interpolated into the prompt verbatim, so its length
;; is bounded too: a 40-character digit run out of a log is not a version.  An
;; unbounded probe renders it on the info line, so this row is non-vacuous, and
;; the short control beside it proves the bound did not simply reject everything.
(let ([dir (temp-dir-name "ver-long")])
  (create-directory dir)
  (write-file (string-append dir "/long.marker") "x")
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([prompt-tools
                      (list (echo-tool "long" "long.marker"
                                       "1.222222222.333333333.444444444.55555"))])
        (test-equal "cap: an absurdly long version token is refused" ""
          (cached-version-segment dir 'emoji 'mono))))
    (lambda () (run (rm "-rf" ,dir)))))

;; === A non-zero exit is not a banner, however version-shaped its output ===
;;
;; A version-manager shim that cannot satisfy the directory's request prints a
;; fully version-shaped diagnostic and exits non-zero -- pyenv's "version
;; `3.12.1' is not installed", rustup's "error: toolchain '1.75.0' is not
;; installed".  Because the probe merges stderr into stdout, that text reaches
;; the parser and parses perfectly, so the prompt would report a toolchain that
;; is neither installed nor in use.  The exit status is the only thing that
;; tells the two apart: a probe that ignored it renders "v3.12.1" here, so this
;; row is non-vacuous.  The clean-exit control beside it proves the check did
;; not simply switch the segment off.
(when sh-bin
  (let ([dir (temp-dir-name "ver-exit")])
    (create-directory dir)
    (write-file (string-append dir "/fail.marker") "x")
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([prompt-tools
                        (list (make-prompt-tool "fail" '(("fail.marker") . ())
                                                sh-bin
                                                (list sh-bin "-c"
                                                      "echo \"shim: version 3.12.1 is not installed\"; exit 1")
                                                parse-version/common "X" "x" #f))])
          (version-probe-count 0)
          (let ([seg (cached-version-segment dir 'emoji 'mono)])
            (test-equal "exit: a non-zero exit yields no version, however version-shaped"
              "" seg)
            (test-equal "exit: the failing probe still spawned once" 1 (version-probe-count)))))
      (lambda () (run (rm "-rf" ,dir))))))

(when sh-bin
  (let ([dir (temp-dir-name "ver-exit-ok")])
    (create-directory dir)
    (write-file (string-append dir "/ok.marker") "x")
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([prompt-tools
                        (list (make-prompt-tool "ok" '(("ok.marker") . ())
                                                sh-bin
                                                (list sh-bin "-c"
                                                      "echo \"Tool 4.5.6\"; exit 0")
                                                parse-version/common "O" "o" #f))])
          (version-probe-count 0)
          (test-assert "exit: the SAME banner on a clean exit is still reported"
            (string-contains? (cached-version-segment dir 'emoji 'mono) "4.5.6"))))
      (lambda () (run (rm "-rf" ,dir))))))

;; ======================================================================
;; The alternate-command candidate list
;; ======================================================================
;;
;; A descriptor's command is EITHER a binary name (the shape every row above uses)
;; or a non-empty list of candidate names tried in order, the first present on PATH
;; gating the tool and becoming argv[0] of the fixed literal argv.  Every row below
;; is non-vacuous against a tree that took only a name: there the gate handed the
;; command straight to a string-keyed hashtable, so a list command RAISED inside
;; the group's per-tool guard and the tool was declined outright -- and the pure
;; rows would not find the procedures at all.

;; === The normaliser takes the raw command VALUE ===
;;
;; It is handed values directly, with no descriptor in sight, and that signature is
;; the point: it lets the registration boundary ask this rather than restate the
;; shape rule, so what the boundary accepts and what the gate can resolve cannot
;; drift apart.
(test-equal "candidates: a bare name yields the one-element candidate list"
  '("node") (tool-command-candidates "node"))
(test-equal "candidates: a list of names yields itself"
  '("python" "python3") (tool-command-candidates '("python" "python3")))
(test-equal "candidates: a value that is neither a string nor a list yields none"
  '() (tool-command-candidates 42))
;; The empty list is the row that keeps the boundary and the gate agreeing: it
;; satisfies list?, so it slips straight past a rule spelled merely "a string or a
;; list", yet it offers the gate nothing whatever to resolve.
(test-equal "candidates: the empty list yields no candidates"
  '() (tool-command-candidates '()))
(test-equal "candidates: a list carrying a non-string yields no candidates"
  '() (tool-command-candidates '("ok" 42)))

;; === The first candidate PRESENT on PATH wins, in order ===
;;
;; Only the SECOND and THIRD of three candidates are seeded, so a resolver that
;; took the first name regardless, or the last name it found, reports the wrong
;; binary here.  The names appear nowhere else in this suite, so no earlier seeding
;; can satisfy them.
(define alt-first  "hafod-alt-first-qzx")
(define alt-second "hafod-alt-second-qzx")
(define alt-third  "hafod-alt-third-qzx")
(hashtable-set! (path-cache) alt-second #t)
(hashtable-set! (path-cache) alt-third #t)

(define alt-list-tool
  (make-prompt-tool "altlist" '(("altlist.marker") . ())
                    (list alt-first alt-second alt-third)
                    (list alt-first "--version") parse-version/common "A" "a" #f))
(define alt-none-tool
  (make-prompt-tool "altnone" '(("altnone.marker") . ())
                    '("hafod-alt-absent-one-qzx" "hafod-alt-absent-two-qzx")
                    '("hafod-alt-absent-one-qzx" "--version")
                    parse-version/common "N" "n" #f))
(define alt-bare-tool
  (make-prompt-tool "altbare" '(("altbare.marker") . ())
                    alt-second (list alt-second "--version")
                    parse-version/common "B" "b" #f))

(test-equal "resolve: the first candidate present on PATH wins, earlier misses skipped"
  alt-second (resolve-tool-command alt-list-tool))
(test-equal "resolve: a candidate list with nothing on PATH resolves to #f"
  #f (resolve-tool-command alt-none-tool))
(test-equal "resolve: a bare-name command resolves to itself when it is on PATH"
  alt-second (resolve-tool-command alt-bare-tool))
(test-assert "gate: a tool with any candidate on PATH passes the gate"
  (tool-on-path? alt-list-tool))
(test-assert "gate: a tool with no candidate on PATH is declined"
  (not (tool-on-path? alt-none-tool)))

;; === argv[0] moves for a candidate list, and ONLY for one ===
;;
;; For a list the winner is substituted and the descriptor's remaining fixed
;; literals are kept, because the descriptor cannot know which of its names will
;; win.
(test-equal "argv: a list command execs the winner as argv[0], keeping the rest"
  (list alt-second "--version") (tool-probe-argv alt-list-tool alt-second))

;; The bare-name branch is the one an UNCONDITIONAL rewrite breaks -- and it breaks
;; nothing else in this file, so this is the only row that catches it.  A
;; single-name descriptor may set argv[0] to something other than the program
;; deliberately: that is the multi-call-binary idiom, a busybox-style command
;; invoked under the applet name it should dispatch on, the same thing a shell's
;; exec-with-a-chosen-name does.  The descriptor below is built in exactly that
;; shape -- one name as its command, a DIFFERENT one as its argv[0] -- and is
;; handed its own command as the winner, so a rewrite would be silent and visible
;; only here.
(define multicall-tool
  (make-prompt-tool "multicall" '(("multicall.marker") . ())
                    "hafod-multicall-qzx"
                    '("hafod-applet-name-qzx" "--version")
                    parse-version/common "M" "m" #f))
(test-equal "argv: a bare-name command keeps its argv verbatim, argv[0] included"
  '("hafod-applet-name-qzx" "--version")
  (tool-probe-argv multicall-tool "hafod-multicall-qzx"))
(test-assert "argv: the bare-name argv is the descriptor's own list, unchanged"
  (equal? (prompt-tool-args multicall-tool)
          (tool-probe-argv multicall-tool "hafod-multicall-qzx")))

;; === The decline happens BEFORE the counter, proven directly ===
;;
;; The group already declines an unresolvable tool at its own gate, so a build that
;; counted the spawn before resolving would pass every other row in this file.
;; Calling the probe DIRECTLY is the only way to reach the early return, and it is
;; what makes the counter mean REAL spawns exactly rather than nearly.
(version-probe-count 0)
(test-equal "probe: a tool with no candidate on PATH yields #f, called directly"
  #f (probe-tool-version alt-none-tool 200))
(test-equal "probe: and it charges no spawn to the counter"
  0 (version-probe-count))

;; === The shipped python descriptor offers the version-suffixed interpreter ===
;;
;; Read outside any parameterize, exactly as the built-in registry pins above are,
;; so it sees the true default.
(define python-builtin
  (find (lambda (t) (string=? (prompt-tool-name t) "python")) (prompt-tools)))

(test-equal "python: the shipped command is the candidate pair, unsuffixed first"
  '("python" "python3") (prompt-tool-command python-builtin))
(test-equal "python: its argv is the shipped literal, unchanged"
  '("python" "--version") (prompt-tool-args python-builtin))
(test-assert "python: the other nine built-ins keep a bare-name command"
  (for-all (lambda (t)
             (or (string=? (prompt-tool-name t) "python")
                 (string? (prompt-tool-command t))))
           (prompt-tools)))

;; THE HEADLINE, and the whole reason for the change: a box that follows PEP 394
;; and ships only the version-suffixed interpreter, with no unsuffixed name on PATH
;; at all.  Only the suffixed name is seeded -- it is used as a command nowhere
;; else in this suite, and a seed left in place is harmless -- so the descriptor
;; must fall through to it, and the probe must exec THAT while keeping the shipped
;; version flag.  Before the change the segment simply did not render there.
(hashtable-set! (path-cache) "python3" #t)
(test-equal "python: with only python3 on PATH the descriptor resolves to it"
  "python3" (resolve-tool-command python-builtin))
(test-assert "python: so the tool passes the gate on a version-suffixed-only box"
  (tool-on-path? python-builtin))
(test-equal "python: and the probe execs python3, keeping the version flag"
  '("python3" "--version") (tool-probe-argv python-builtin "python3"))

;; === The resolved candidate is what actually RUNS ===
;;
;; Two fixture scripts print DISTINCT version banners.  The descriptor lists them
;; in order and its own argv[0] is the FIRST, but only the SECOND is on PATH -- so
;; the rendered segment says unambiguously which one the child was.  A build that
;; gated on the list but spawned the descriptor's first name would run the wrong
;; script, or fail to spawn at all.  Self-skips where no POSIX sh is present.
(when sh-bin
  (let ([dir (temp-dir-name "alt-resolved")])
    (create-directory dir)
    (let ([one (string-append dir "/alt-one")]
          [two (string-append dir "/alt-two")])
      (write-file one (string-append "#!" sh-bin "\necho 'Alt 1.1.1'\n"))
      (write-file two (string-append "#!" sh-bin "\necho 'Alt 2.2.2'\n"))
      (run (chmod "+x" ,one))
      (run (chmod "+x" ,two))
      (write-file (string-append dir "/alt.marker") "x")
      (hashtable-set! (path-cache) two #t)
      (dynamic-wind
        void
        (lambda ()
          (parameterize ([prompt-tools
                          (list (make-prompt-tool "alt" '(("alt.marker") . ())
                                                  (list one two)
                                                  (list one "--version")
                                                  parse-version/common "A" "a" #f))])
            (version-probe-count 0)
            (let ([seg (cached-version-segment dir 'emoji 'mono 200)])
              (test-assert "alt: the RESOLVED candidate is the binary that ran"
                (string-contains? seg "2.2.2"))
              (test-assert "alt: the unresolved first candidate did NOT run"
                (not (string-contains? seg "1.1.1")))
              (test-equal "alt: the resolved probe spawned exactly once"
                1 (version-probe-count)))))
        (lambda () (run (rm "-rf" ,dir)))))))

;; === The winner is the child's own argv[0], reported by the child ===
;;
;; A bespoke parse for the row below: it yields a version only when the child's
;; report names the shell, so a report of anything else renders nothing.
(define (parse-argv0-report out)
  (if (and sh-bin (string-contains? out sh-bin)) "7.7.7" #f))

;; The descriptor lists an absent placeholder FIRST and the shell second, and its
;; own argv[0] is that placeholder.  The child is the shell with -c and no trailing
;; operand, so its zeroth parameter IS its argv[0], and it echoes it back.  A build
;; that resolved the program but left the descriptor's first candidate as argv[0]
;; makes the child report the placeholder, the parse yields #f and the segment is
;; empty -- so this row is the argv[0] oracle.  The shell is already seeded in the
;; path-cache further up.
(when sh-bin
  (let ([dir (temp-dir-name "alt-argv0")])
    (create-directory dir)
    (write-file (string-append dir "/argv0.marker") "x")
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([prompt-tools
                        (list (make-prompt-tool "argv0" '(("argv0.marker") . ())
                                                (list "hafod-alt-absent-argv0-qzx" sh-bin)
                                                (list "hafod-alt-absent-argv0-qzx"
                                                      "-c" "echo \"$0\"")
                                                parse-argv0-report "Z" "z" #f))])
          (version-probe-count 0)
          (test-assert "alt: the resolved winner is the child's own argv[0]"
            (string-contains? (cached-version-segment dir 'emoji 'mono 200) "7.7.7"))
          (test-equal "alt: the argv[0] probe spawned exactly once"
            1 (version-probe-count))))
      (lambda () (run (rm "-rf" ,dir))))))

;; === Nothing on PATH declines with ZERO spawns; a later appearance is picked up ===
;;
;; One directory, one descriptor, three sequential steps and no teardown between
;; them.  Step one is the spawn-avoidance property the engine ships: no candidate
;; resolves, so nothing is spawned and the group is empty.  Step three is the
;; cache-stamp oracle -- the environment did not move and no marker was touched, so
;; a stamp that ignored WHICH binary a tool resolved to would keep serving the
;; cached empty string for the whole cache lifetime and the version would never
;; appear.
(when sh-bin
  (let ([dir (temp-dir-name "alt-appears")])
    (create-directory dir)
    (let ([script (string-append dir "/alt-late")])
      (write-file script (string-append "#!" sh-bin "\necho 'Late 3.3.3'\n"))
      (run (chmod "+x" ,script))
      (write-file (string-append dir "/late.marker") "x")
      (dynamic-wind
        void
        (lambda ()
          (parameterize ([prompt-tools
                          (list (make-prompt-tool "late" '(("late.marker") . ())
                                                  (list "hafod-alt-absent-late-qzx" script)
                                                  (list "hafod-alt-absent-late-qzx"
                                                        "--version")
                                                  parse-version/common "L" "l" #f))])
            (version-probe-count 0)
            (test-equal "alt: a candidate list with nothing on PATH renders nothing"
              "" (cached-version-segment dir 'emoji 'mono 200))
            (test-equal "alt: and it spawns NOTHING, declining before the probe"
              0 (version-probe-count))
            ;; The binary becomes visible mid-session: no environment change, no
            ;; marker touched, only the path-cache moved.
            (hashtable-set! (path-cache) script #t)
            (version-probe-count 0)
            (test-assert "alt: a candidate appearing on PATH is picked up on the next draw"
              (string-contains? (cached-version-segment dir 'emoji 'mono 200) "3.3.3"))
            (test-equal "alt: the newly resolved probe spawned exactly once"
              1 (version-probe-count))))
        (lambda () (run (rm "-rf" ,dir)))))))

;; === A bare-name command is untouched by any of this ===
;;
;; The single-name path is the one the other nine built-ins take, so it is pinned
;; on both sides: an unseeded name still declines with zero spawns, and a seeded
;; one still probes exactly once per cwd and is served from the cache on a repeat
;; draw at the same width.
(when sh-bin
  (let ([dir (temp-dir-name "alt-bare-off")])
    (create-directory dir)
    (write-file (string-append dir "/bareoff.marker") "x")
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([prompt-tools
                        (list (make-prompt-tool "bareoff" '(("bareoff.marker") . ())
                                                "hafod-alt-absent-bare-qzx"
                                                '("hafod-alt-absent-bare-qzx" "--version")
                                                parse-version/common "S" "s" #f))])
          (version-probe-count 0)
          (test-equal "alt: a bare-name command absent from PATH still renders nothing"
            "" (cached-version-segment dir 'emoji 'mono 200))
          (test-equal "alt: and it still spawns nothing"
            0 (version-probe-count))))
      (lambda () (run (rm "-rf" ,dir))))))

(when sh-bin
  (let ([dir (temp-dir-name "alt-bare-on")])
    (create-directory dir)
    (write-file (string-append dir "/fake.marker") "x")
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([prompt-tools
                        (list (echo-tool "fake" "fake.marker" "Fake 8.8.8"))])
          (version-probe-count 0)
          (test-assert "alt: a bare-name command on PATH still renders its version"
            (string-contains? (cached-version-segment dir 'emoji 'mono 200) "8.8.8"))
          (test-equal "alt: the bare-name probe spawned exactly once"
            1 (version-probe-count))
          (cached-version-segment dir 'emoji 'mono 200)
          (test-equal "alt: a repeat draw at the same width is served from the cache"
            1 (version-probe-count))))
      (lambda () (run (rm "-rf" ,dir))))))

(test-end)
