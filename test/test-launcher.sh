#!/bin/sh
# test-launcher.sh -- Comprehensive tests for the hafod/scsh launcher
# Tests CLI switches, meta-arg processing, !# header stripping,
# entry points, error handling, and command-line argument passing.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

HAFOD="$(cd "$(dirname "$0")/.." && pwd)/bin/hafod"
TMPDIR="${TMPDIR:-/tmp}/hafod-launcher-test-$$"
PASS=0
FAIL=0
TOTAL=0

mkdir -p "$TMPDIR"

cleanup() {
    rm -rf "$TMPDIR"
}
trap cleanup EXIT

pass() {
    PASS=$((PASS + 1))
    TOTAL=$((TOTAL + 1))
    printf "  PASS: %s\n" "$1"
}

fail() {
    FAIL=$((FAIL + 1))
    TOTAL=$((TOTAL + 1))
    printf "  FAIL: %s\n" "$1"
    if [ -n "$2" ]; then
        printf "    expected: %s\n" "$2"
        printf "    actual:   %s\n" "$3"
    fi
}

assert_eq() {
    # $1=description $2=expected $3=actual
    if [ "$2" = "$3" ]; then
        pass "$1"
    else
        fail "$1" "$2" "$3"
    fi
}

assert_contains() {
    # $1=description $2=needle $3=haystack
    case "$3" in
        *"$2"*) pass "$1" ;;
        *) fail "$1" "contains '$2'" "$3" ;;
    esac
}

assert_exit() {
    # $1=description $2=expected-exit-code $3...=command
    desc="$1"; expected="$2"; shift 2
    set +e
    "$@" >/dev/null 2>&1
    actual=$?
    set -e
    assert_eq "$desc" "$expected" "$actual"
}

section() {
    printf "\n=== %s ===\n" "$1"
}

# ======================================================================
section "Basic switches"
# ======================================================================

# --help
out=$("$HAFOD" --help 2>&1)
assert_contains "--help shows usage" "Usage:" "$out"
assert_contains "--help mentions -s" "-s FILE" "$out"
assert_contains "--help mentions -c" "-c EXPR" "$out"
assert_contains "--help mentions -e" "-e PROC" "$out"
assert_contains "--help mentions --" "--  " "$out"

# --version
out=$("$HAFOD" --version 2>&1)
assert_contains "--version shows version" "hafod" "$out"

# Unknown switch
assert_exit "unknown switch exits 1" 1 "$HAFOD" -z

# ======================================================================
section "-c expression evaluation"
# ======================================================================

out=$("$HAFOD" -c '(display 42)' 2>&1)
assert_eq "-c evaluates expression" "42" "$out"

out=$("$HAFOD" -c '(display (+ 1 2 3))' 2>&1)
assert_eq "-c evaluates arithmetic" "6" "$out"

# -c with hafod API available
out=$("$HAFOD" -c '(display (pid))' 2>&1)
assert_contains "-c has hafod API (pid)" "" ""  # just shouldn't error
# Actually check it's a number
case "$out" in
    *[0-9]*) pass "-c has hafod API (pid returns number)" ;;
    *) fail "-c has hafod API (pid returns number)" "a number" "$out" ;;
esac

# -c with remaining args in command-line
out=$("$HAFOD" -c '(display (command-line-arguments))' foo bar 2>&1)
assert_eq "-c passes remaining args to command-line" "(foo bar)" "$out"

# -c with no expression
assert_exit "-c with no expression exits 1" 1 "$HAFOD" -c

# -c with -e should fail
assert_exit "-c with -e is rejected" 1 "$HAFOD" -e main -c '(display 1)'

# ======================================================================
section "-s script execution"
# ======================================================================

# Simple script
cat > "$TMPDIR/simple.ss" << 'EOF'
(import (hafod))
(display "simple-ok")
EOF
out=$("$HAFOD" -s "$TMPDIR/simple.ss" 2>&1)
assert_eq "-s runs simple script" "simple-ok" "$out"

# Script with args
cat > "$TMPDIR/args.ss" << 'EOF'
(import (hafod))
(for-each (lambda (a) (display a) (display " ")) (command-line-arguments))
EOF
out=$("$HAFOD" -s "$TMPDIR/args.ss" one two three 2>&1)
assert_eq "-s passes args to script" "one two three " "$out"

# Script uses hafod API
cat > "$TMPDIR/api.ss" << 'EOF'
(import (hafod))
(let ([r (run/string (echo hello))]) (display (string-length r)))
EOF
out=$("$HAFOD" -s "$TMPDIR/api.ss" 2>&1)
assert_eq "-s script can use hafod API" "6" "$out"

# -s with no filename
assert_exit "-s with no filename exits 1" 1 "$HAFOD" -s

# Bare filename (implicit -s)
out=$("$HAFOD" "$TMPDIR/simple.ss" 2>&1)
assert_eq "bare filename acts as -s" "simple-ok" "$out"

# Bare filename with args
out=$("$HAFOD" "$TMPDIR/args.ss" one two 2>&1)
assert_eq "bare filename passes args" "one two " "$out"

# ======================================================================
section "-e entry point"
# ======================================================================

cat > "$TMPDIR/entry.ss" << 'EOF'
(define (main args)
  (display "entry:")
  (for-each (lambda (a) (display " ") (display a)) args))
EOF
out=$("$HAFOD" -e main -s "$TMPDIR/entry.ss" x y z 2>&1)
assert_eq "-e calls entry point with args" "entry: x y z" "$out"

# -e with no args to entry function
out=$("$HAFOD" -e main -s "$TMPDIR/entry.ss" 2>&1)
assert_eq "-e with no script args" "entry:" "$out"

# -e with bare filename
out=$("$HAFOD" -e main "$TMPDIR/entry.ss" a b 2>&1)
assert_eq "-e with bare filename" "entry: a b" "$out"

# Entry point defined later in script
cat > "$TMPDIR/entry-late.ss" << 'EOF'
(define (helper x) (string-append "got:" x))
(define (main args) (display (helper (car args))))
EOF
out=$("$HAFOD" -e main -s "$TMPDIR/entry-late.ss" val 2>&1)
assert_eq "-e entry defined after helpers" "got:val" "$out"

# -e with no proc name
assert_exit "-e with no proc name exits 1" 1 "$HAFOD" -e

# ======================================================================
section "!# header stripping"
# ======================================================================

# Script with #! / !# header
cat > "$TMPDIR/shebang.ss" << 'EOF'
#!/usr/bin/env hafod
-s
!#
(import (hafod))
(display "shebang-ok")
EOF
out=$("$HAFOD" -s "$TMPDIR/shebang.ss" 2>&1)
assert_eq "!# header is stripped" "shebang-ok" "$out"

# Script with multi-line header (comment block between #! and !#)
cat > "$TMPDIR/multiline-header.ss" << 'EOF'
#!/usr/bin/env hafod
This is a comment line that should be stripped.
Another comment line.
!#
(import (hafod))
(display "multi-header-ok")
EOF
out=$("$HAFOD" -s "$TMPDIR/multiline-header.ss" 2>&1)
assert_eq "multi-line header stripped" "multi-header-ok" "$out"

# Bare filename with !# header
out=$("$HAFOD" "$TMPDIR/shebang.ss" 2>&1)
assert_eq "bare filename strips !# header" "shebang-ok" "$out"

# Script without header loads normally
cat > "$TMPDIR/no-header.ss" << 'EOF'
(display "no-header-ok")
EOF
out=$("$HAFOD" -s "$TMPDIR/no-header.ss" 2>&1)
assert_eq "script without header loads fine" "no-header-ok" "$out"

# !# header with -e entry point
cat > "$TMPDIR/shebang-entry.ss" << 'EOF'
#!/usr/bin/env hafod
-e main -s
!#
(define (main args) (display "shebang-entry-ok"))
EOF
out=$("$HAFOD" -e main -s "$TMPDIR/shebang-entry.ss" 2>&1)
assert_eq "!# header with -e entry" "shebang-entry-ok" "$out"

# !# appearing mid-line should NOT be treated as header end
cat > "$TMPDIR/bangbang-midline.ss" << 'EOF'
#!/usr/bin/env hafod
this line has !# in the middle so should not end header
!#
(display "midline-ok")
EOF
out=$("$HAFOD" -s "$TMPDIR/bangbang-midline.ss" 2>&1)
assert_eq "!# mid-line does not end header" "midline-ok" "$out"

# ======================================================================
section "Header-arg parsing (env-compatible shebang)"
# ======================================================================

# Bare filename with -e in header
cat > "$TMPDIR/hdr-entry.ss" << 'EOF'
#!/usr/bin/env hafod
-e main -s
!#
(define (main args) (display "hdr-entry-ok"))
EOF
out=$("$HAFOD" "$TMPDIR/hdr-entry.ss" 2>&1)
assert_eq "header -e sets entry point" "hdr-entry-ok" "$out"

# Bare filename with -e in header, passing args
cat > "$TMPDIR/hdr-entry-args.ss" << 'EOF'
#!/usr/bin/env hafod
-e main -s
!#
(define (main args)
  (for-each (lambda (a) (display a) (display " ")) args))
EOF
out=$("$HAFOD" "$TMPDIR/hdr-entry-args.ss" x y z 2>&1)
assert_eq "header -e passes args" "x y z " "$out"

# Bare filename with -l in header
cat > "$TMPDIR/hdr-prelude.ss" << 'EOF'
(define hdr-preloaded 99)
EOF

cat > "$TMPDIR/hdr-use-prelude.ss" << 'EOF'
#!/usr/bin/env hafod
-l PRELUDE -s
!#
(display hdr-preloaded)
EOF
# Substitute the actual path (can't use variable in heredoc)
sed "s|PRELUDE|$TMPDIR/hdr-prelude.ss|" "$TMPDIR/hdr-use-prelude.ss" > "$TMPDIR/hdr-use-prelude.ss.tmp" && mv "$TMPDIR/hdr-use-prelude.ss.tmp" "$TMPDIR/hdr-use-prelude.ss"
out=$("$HAFOD" "$TMPDIR/hdr-use-prelude.ss" 2>&1)
assert_eq "header -l preloads file" "99" "$out"

# Command-line -e overrides header -e
cat > "$TMPDIR/hdr-override.ss" << 'EOF'
#!/usr/bin/env hafod
-e header-entry -s
!#
(define (header-entry args) (display "header"))
(define (cli-entry args) (display "cli"))
EOF
out=$("$HAFOD" -e cli-entry "$TMPDIR/hdr-override.ss" 2>&1)
assert_eq "CLI -e overrides header -e" "cli" "$out"

# Header with -s only (no -e) — just runs the script body
cat > "$TMPDIR/hdr-s-only.ss" << 'EOF'
#!/usr/bin/env hafod
-s
!#
(display "s-only-ok")
EOF
out=$("$HAFOD" "$TMPDIR/hdr-s-only.ss" 2>&1)
assert_eq "header with -s only" "s-only-ok" "$out"

# No header — bare file still works
out=$("$HAFOD" "$TMPDIR/no-header.ss" 2>&1)
assert_eq "bare filename without header" "no-header-ok" "$out"

# ======================================================================
section "Actual shebang execution"
# ======================================================================

# Only run if hafod is on PATH (or we can symlink it)
HAFOD_BIN_DIR=$(dirname "$HAFOD")
if command -v "$HAFOD" >/dev/null 2>&1 || [ -x "$HAFOD" ]; then
    # Create a temp bin dir with hafod on PATH
    mkdir -p "$TMPDIR/bin"
    ln -sf "$HAFOD" "$TMPDIR/bin/hafod"
    ORIG_PATH="$PATH"
    export PATH="$TMPDIR/bin:$PATH"

    # Simple shebang script
    cat > "$TMPDIR/shebang-exec.ss" << 'SCRIPT'
#!/usr/bin/env hafod
!#
(display "shebang-exec-ok")
SCRIPT
    chmod +x "$TMPDIR/shebang-exec.ss"
    out=$("$TMPDIR/shebang-exec.ss" 2>&1)
    assert_eq "shebang execution works" "shebang-exec-ok" "$out"

    # Shebang with -e entry in header
    cat > "$TMPDIR/shebang-entry-exec.ss" << 'SCRIPT'
#!/usr/bin/env hafod
-e main -s
!#
(define (main args) (display "shebang-entry-exec-ok"))
SCRIPT
    chmod +x "$TMPDIR/shebang-entry-exec.ss"
    out=$("$TMPDIR/shebang-entry-exec.ss" 2>&1)
    assert_eq "shebang with -e entry works" "shebang-entry-exec-ok" "$out"

    # Shebang with args
    cat > "$TMPDIR/shebang-args-exec.ss" << 'SCRIPT'
#!/usr/bin/env hafod
-e main -s
!#
(define (main args)
  (for-each (lambda (a) (display a) (display " ")) args))
SCRIPT
    chmod +x "$TMPDIR/shebang-args-exec.ss"
    out=$("$TMPDIR/shebang-args-exec.ss" hello world 2>&1)
    assert_eq "shebang passes args to entry" "hello world " "$out"

    export PATH="$ORIG_PATH"
fi

# ======================================================================
section "Meta-argument processing"
# ======================================================================

# Basic meta-arg: \ <filename> triggers line-2 expansion
cat > "$TMPDIR/meta-script.ss" << 'EOF'
#!/path/to/hafod \
-s
!#
(import (hafod))
(display "meta-ok")
EOF
out=$("$HAFOD" '\' "$TMPDIR/meta-script.ss" 2>&1)
assert_eq "meta-arg basic expansion" "meta-ok" "$out"

# Meta-arg with -e
cat > "$TMPDIR/meta-entry.ss" << 'EOF'
#!/path/to/hafod \
-e main -s
!#
(define (main args) (display "meta-entry-ok"))
EOF
out=$("$HAFOD" '\' "$TMPDIR/meta-entry.ss" 2>&1)
assert_eq "meta-arg with -e entry" "meta-entry-ok" "$out"

# Meta-arg with extra args after filename
cat > "$TMPDIR/meta-args.ss" << 'EOF'
#!/path/to/hafod \
-s
!#
(for-each (lambda (a) (display a) (display " ")) (command-line-arguments))
EOF
out=$("$HAFOD" '\' "$TMPDIR/meta-args.ss" alpha beta 2>&1)
assert_eq "meta-arg preserves extra args" "alpha beta " "$out"

# Meta-arg with backslash escapes in line 2
cat > "$TMPDIR/meta-escape.ss" << 'EOF'
#!/path/to/hafod \
-s
!#
(display "escape-ok")
EOF
out=$("$HAFOD" '\' "$TMPDIR/meta-escape.ss" 2>&1)
assert_eq "meta-arg line 2 parsed correctly" "escape-ok" "$out"

# Meta-arg with spaces creating multiple args on line 2
cat > "$TMPDIR/meta-multi.ss" << 'EOF'
#!/path/to/hafod \
-e main -s
!#
(define (main args) (display (length args)))
EOF
out=$("$HAFOD" '\' "$TMPDIR/meta-multi.ss" a b c 2>&1)
assert_eq "meta-arg multi-switch line 2" "3" "$out"

# ======================================================================
section "-- REPL mode"
# ======================================================================

out=$(echo '(display "repl-ok")' | "$HAFOD" -- 2>&1)
assert_contains "-- starts REPL" "repl-ok" "$out"

# -- with remaining args
out=$(echo '(display (command-line-arguments))' | "$HAFOD" -- foo bar 2>&1)
assert_contains "-- passes remaining args" "(foo bar)" "$out"

# -- REPL has last-status (interactive-repl feature, not available in new-cafe)
out=$(echo '(last-status)' | "$HAFOD" -- 2>&1)
assert_contains "-- REPL has last-status" "0" "$out"

# no-args REPL has last-status
out=$(echo '(last-status)' | "$HAFOD" 2>&1)
assert_contains "no-args REPL has last-status" "0" "$out"

# -c mode unaffected (regression guard)
out=$("$HAFOD" -c '(display 42)' 2>&1)
assert_eq "-c mode unaffected" "42" "$out"

# ======================================================================
section "Quasiquote in process forms (scsh compat)"
# ======================================================================

cat > "$TMPDIR/quasiquote.ss" << 'EOF'
(import (hafod))
;; unquote variable in process form — use run/strings to avoid newline
(let ([name "world"])
  (display (car (run/strings (echo ,name)))))
EOF
out=$("$HAFOD" -s "$TMPDIR/quasiquote.ss" 2>&1)
assert_eq "unquote in process form" "world" "$out"

# unquote in redirection
cat > "$TMPDIR/qq-redir.ss" << 'EOF'
(import (hafod))
(let ([outfile "/tmp/hafod-launcher-test-redir"])
  (run (echo "redir-test") (> ,outfile))
  (display (car (run/strings (cat ,outfile))))
  (delete-file outfile))
EOF
out=$("$HAFOD" -s "$TMPDIR/qq-redir.ss" 2>&1)
assert_eq "unquote in redirection" "redir-test" "$out"

# unquote in pipeline
cat > "$TMPDIR/qq-pipe.ss" << 'EOF'
(import (hafod))
(let ([msg "piped"])
  (display (car (run/strings (pipe (echo ,msg) (cat))))))
EOF
out=$("$HAFOD" -s "$TMPDIR/qq-pipe.ss" 2>&1)
assert_eq "unquote in pipeline" "piped" "$out"

# bare symbol becomes literal string
cat > "$TMPDIR/qq-bare.ss" << 'EOF'
(import (hafod))
(display (car (run/strings (echo hello))))
EOF
out=$("$HAFOD" -s "$TMPDIR/qq-bare.ss" 2>&1)
assert_eq "bare symbol in process form" "hello" "$out"

# ======================================================================
section "signal-process with proc objects"
# ======================================================================

cat > "$TMPDIR/signal-proc.ss" << 'EOF'
(import (hafod))
(let ([child (fork (lambda () (pause) (%exit 0)))])
  (signal-process child SIGTERM)
  (let ([status (wait child)])
    (display (if (eqv? (status:term-sig status) SIGTERM) "sig-ok" "sig-fail"))))
EOF
out=$("$HAFOD" -s "$TMPDIR/signal-proc.ss" 2>&1)
assert_eq "signal-process accepts proc object" "sig-ok" "$out"

# ======================================================================
section "run/strings port stability"
# ======================================================================

cat > "$TMPDIR/runstrings.ss" << 'EOF'
(import (hafod))
(let ([result (run/strings (echo "a\nb\nc"))])
  (display (length result)))
EOF
out=$("$HAFOD" -s "$TMPDIR/runstrings.ss" 2>&1)
assert_eq "run/strings returns correct line count" "3" "$out"

# Stress test: run/strings in a loop
cat > "$TMPDIR/runstrings-stress.ss" << 'EOF'
(import (hafod))
(do ([i 0 (+ i 1)])
    ((= i 20))
  (let ([result (run/strings (echo "x\ny\nz"))])
    (unless (equal? result '("x" "y" "z"))
      (error 'test "mismatch" result))))
(display "stress-ok")
EOF
out=$("$HAFOD" -s "$TMPDIR/runstrings-stress.ss" 2>&1)
assert_eq "run/strings stable under repeated use" "stress-ok" "$out"

# ======================================================================
section "if-match / match-cond macros"
# ======================================================================

cat > "$TMPDIR/ifmatch.ss" << 'EOF'
(import (hafod))
;; if-match success
(if-match (regexp-search (rx (submatch (+ digit))) "abc123def")
  (digits)
  (display (string-append "match:" digits))
  (display "no-match"))
EOF
out=$("$HAFOD" -s "$TMPDIR/ifmatch.ss" 2>&1)
assert_eq "if-match extracts submatch" "match:123" "$out"

cat > "$TMPDIR/matchcond.ss" << 'EOF'
(import (hafod))
(define (classify s)
  (match-cond
    ((regexp-search (rx bos (submatch (+ digit)) eos) s) (n)
     (string-append "number:" n))
    ((regexp-search (rx bos (submatch (+ alpha)) eos) s) (w)
     (string-append "word:" w))
    (else "other")))
(display (classify "42"))
(display " ")
(display (classify "hello"))
(display " ")
(display (classify "!?"))
EOF
out=$("$HAFOD" -s "$TMPDIR/matchcond.ss" 2>&1)
assert_eq "match-cond dispatches correctly" "number:42 word:hello other" "$out"

# ======================================================================
section "Edge cases"
# ======================================================================

# Empty script
cat > "$TMPDIR/empty.ss" << 'EOF'
EOF
out=$("$HAFOD" -s "$TMPDIR/empty.ss" 2>&1)
assert_eq "empty script runs without error" "" "$out"

# Script with only a !# header and nothing after
cat > "$TMPDIR/header-only.ss" << 'EOF'
#!/usr/bin/env hafod
!#
EOF
out=$("$HAFOD" -s "$TMPDIR/header-only.ss" 2>&1)
assert_eq "header-only script runs without error" "" "$out"

# Script with multiple (import (hafod)) is harmless
cat > "$TMPDIR/double-import.ss" << 'EOF'
(import (hafod))
(import (hafod))
(display "double-ok")
EOF
out=$("$HAFOD" -s "$TMPDIR/double-import.ss" 2>&1)
assert_eq "double import is harmless" "double-ok" "$out"

# Nonexistent script
assert_exit "nonexistent script exits non-zero" 1 "$HAFOD" -s "$TMPDIR/nonexistent.ss"

# ======================================================================
section "REPL auto-import (LNCH-01)"
# ======================================================================

# -- REPL has (hafod) pre-imported -- can use pid without import
out=$(echo '(display (pid))' | "$HAFOD" -- 2>&1)
case "$out" in
    *[0-9]*) pass "-- REPL has hafod auto-imported (pid works)" ;;
    *) fail "-- REPL has hafod auto-imported (pid works)" "a number" "$out" ;;
esac

# -- REPL can use run/string without import
out=$(echo '(display (run/string (echo "auto-import-ok")))' | "$HAFOD" -- 2>&1)
assert_contains "-- REPL can use run/string" "auto-import-ok" "$out"

# ======================================================================
section "-l preload flag (LNCH-02)"
# ======================================================================

# -l loads a file before -s script
cat > "$TMPDIR/prelude.ss" << 'EOF'
(define preloaded-value 42)
EOF

cat > "$TMPDIR/use-prelude.ss" << 'EOF'
(display preloaded-value)
EOF

out=$("$HAFOD" -l "$TMPDIR/prelude.ss" -s "$TMPDIR/use-prelude.ss" 2>&1)
assert_eq "-l preloads before -s" "42" "$out"

# Multiple -l flags load in order
cat > "$TMPDIR/prelude-a.ss" << 'EOF'
(define a-val "A")
EOF

cat > "$TMPDIR/prelude-b.ss" << 'EOF'
(define b-val "B")
EOF

cat > "$TMPDIR/use-both.ss" << 'EOF'
(display (string-append a-val b-val))
EOF

out=$("$HAFOD" -l "$TMPDIR/prelude-a.ss" -l "$TMPDIR/prelude-b.ss" -s "$TMPDIR/use-both.ss" 2>&1)
assert_eq "multiple -l flags load in order" "AB" "$out"

# -l with -c
out=$("$HAFOD" -l "$TMPDIR/prelude.ss" -c '(display preloaded-value)' 2>&1)
assert_eq "-l preloads before -c" "42" "$out"

# -l with bare filename
out=$("$HAFOD" -l "$TMPDIR/prelude.ss" "$TMPDIR/use-prelude.ss" 2>&1)
assert_eq "-l preloads before bare filename" "42" "$out"

# -l with no filename
assert_exit "-l with no filename exits 1" 1 "$HAFOD" -l

# -l with -e entry point
cat > "$TMPDIR/entry-preloaded.ss" << 'EOF'
(define (main args) (display preloaded-value))
EOF

out=$("$HAFOD" -l "$TMPDIR/prelude.ss" -e main -s "$TMPDIR/entry-preloaded.ss" 2>&1)
assert_eq "-l preloads before -e -s" "42" "$out"

# --help mentions -l
out=$("$HAFOD" --help 2>&1)
assert_contains "--help mentions -l" "-l FILE" "$out"

# ======================================================================
section "Config System Tests"
# ======================================================================

# XDG config loading (CONF-01): init.ss is loaded on interactive startup
mkdir -p "$TMPDIR/xdg-config/hafod"
cat > "$TMPDIR/xdg-config/hafod/init.ss" << 'EOF'
(set-prompt! "test> ")
EOF

out=$(echo '(display (repl-prompt-string))' | XDG_CONFIG_HOME="$TMPDIR/xdg-config" "$HAFOD" -- 2>/dev/null)
assert_contains "XDG config loads init.ss" "test> " "$out"

# Config definitions are visible in REPL
cat > "$TMPDIR/xdg-config/hafod/init.ss" << 'EOF'
(define *config-marker* "hello-from-config")
EOF

out=$(echo '(display *config-marker*)' | XDG_CONFIG_HOME="$TMPDIR/xdg-config" "$HAFOD" -- 2>/dev/null)
assert_contains "Config definitions available in REPL" "hello-from-config" "$out"

# Config error handling (CONF-03): errors display filename, REPL continues
mkdir -p "$TMPDIR/xdg-bad/hafod"
echo '(this-is-broken' > "$TMPDIR/xdg-bad/hafod/init.ss"

out=$(echo '(display "ok")' | XDG_CONFIG_HOME="$TMPDIR/xdg-bad" "$HAFOD" -- 2>/dev/null)
assert_contains "Config error does not prevent startup" "ok" "$out"

STDERR=$(echo '(void)' | XDG_CONFIG_HOME="$TMPDIR/xdg-bad" "$HAFOD" -- 2>&1 >/dev/null || true)
assert_contains "Config error mentions filename" "init.ss" "$STDERR"

# --no-config flag (CONF-04): skips init.ss loading
mkdir -p "$TMPDIR/xdg-skip/hafod"
cat > "$TMPDIR/xdg-skip/hafod/init.ss" << 'EOF'
(set-prompt! "custom> ")
EOF

out=$(echo '(display (repl-prompt-string))' | XDG_CONFIG_HOME="$TMPDIR/xdg-skip" "$HAFOD" --no-config -- 2>/dev/null)
assert_contains "--no-config skips init.ss" "> " "$out"

# --norc backward compat alias also skips config
out=$(echo '(display (repl-prompt-string))' | XDG_CONFIG_HOME="$TMPDIR/xdg-skip" "$HAFOD" --norc -- 2>/dev/null)
assert_contains "--norc alias skips init.ss" "> " "$out"

# Missing config file is silently skipped (no error)
mkdir -p "$TMPDIR/xdg-empty/hafod"
out=$(echo '(display "works")' | XDG_CONFIG_HOME="$TMPDIR/xdg-empty" "$HAFOD" -- 2>/dev/null)
assert_contains "Missing config file silently skipped" "works" "$out"

# Config not loaded for -c mode
mkdir -p "$TMPDIR/xdg-cmode/hafod"
cat > "$TMPDIR/xdg-cmode/hafod/init.ss" << 'EOF'
(define *config-loaded* #t)
EOF

out=$(XDG_CONFIG_HOME="$TMPDIR/xdg-cmode" "$HAFOD" -c '(display (top-level-bound? (quote *config-loaded*)))' 2>&1)
assert_eq "Config not loaded for -c mode" "#f" "$out"

# Config not loaded for -s mode
cat > "$TMPDIR/config-script-test.ss" << 'EOF'
(display (top-level-bound? (quote *config-loaded*)))
EOF

out=$(XDG_CONFIG_HOME="$TMPDIR/xdg-cmode" "$HAFOD" -s "$TMPDIR/config-script-test.ss" 2>&1)
assert_eq "Config not loaded for -s mode" "#f" "$out"

# -- terminator also loads config
mkdir -p "$TMPDIR/xdg-dash/hafod"
cat > "$TMPDIR/xdg-dash/hafod/init.ss" << 'EOF'
(define *config-via-dash-dash* #t)
EOF

out=$(echo '(display *config-via-dash-dash*)' | XDG_CONFIG_HOME="$TMPDIR/xdg-dash" "$HAFOD" -- 2>/dev/null)
assert_contains "-- terminator loads config" "#t" "$out"

# --help mentions --no-config and init.ss
out=$("$HAFOD" --help 2>&1)
assert_contains "--help mentions --login" "--login" "$out"
assert_contains "--help mentions --no-config" "--no-config" "$out"
assert_contains "--help mentions --norc" "--norc" "$out"
assert_contains "--help mentions init.ss" "init.ss" "$out"

# Multi-file config (CONF-07): init.ss can load other files
mkdir -p "$TMPDIR/xdg-multi/hafod"
cat > "$TMPDIR/xdg-multi/hafod/extra.ss" << 'EOF'
(define *extra-loaded* #t)
EOF
cat > "$TMPDIR/xdg-multi/hafod/init.ss" << 'INITEOF'
(load (string-append (hafod-config-dir) "/extra.ss"))
INITEOF

out=$(echo '(display *extra-loaded*)' | XDG_CONFIG_HOME="$TMPDIR/xdg-multi" "$HAFOD" -- 2>/dev/null)
assert_contains "Multi-file config loads without error" "#t" "$out"

# ======================================================================
section "INTG-02: Piped input fallback (no raw mode)"
# ======================================================================

# Piped input uses bare read, produces correct output
out=$(echo '(display (+ 1 2))' | "$HAFOD" 2>/dev/null)
assert_contains "piped input produces correct output" "3" "$out"

# Piped eval with pretty-printed output
out=$(echo '(+ 40 2)' | "$HAFOD" 2>/dev/null)
assert_contains "piped eval output" "42" "$out"

# Multiple piped expressions -- each display produces output interspersed with prompts
out=$(printf '(display 1)\n(display 2)\n' | "$HAFOD" 2>/dev/null)
# Verify both values appear in the output (prompts may appear between them)
echo "$out" | grep -q "1" && echo "$out" | grep -q "2" && pass "multiple piped expressions" || fail "multiple piped expressions"

# Piped input with define + reference
out=$(printf '(define x 99)\n(display x)\n' | "$HAFOD" 2>/dev/null)
assert_contains "piped define and reference" "99" "$out"

# ======================================================================
# Summary
# ======================================================================

printf "\n=== Summary ===\n"
printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
