#!/bin/sh
# test-load.sh -- Assert that (hafod) and (hafod posix) import cleanly under
# BOTH petite (interpreter-only, no compiler -- it must load the pre-built
# fasls) and full scheme, via the source/fasl library path (--libdirs src),
# NOT the bundled bin/hafod.so whole-program image.
#
# Why petite earns its place: the existing test/test-*.ss suites all run under
# full scheme, which COMPILES imported libraries on the fly, so they can quietly
# paper over a dependency that needs a compiler at load time or a broken
# inter-library load order. Petite cannot compile -- it loads src/hafod/**/*.so
# or fails loudly -- so a petite import of the two libraries is the sharper
# regression detector. Exercising the per-library source/fasl path (rather than
# the pre-linked image) is what surfaces load-order issues the image hides.
#
# Each cell writes a two-line probe and runs it with --script, NOT a bare -q
# REPL. Under --script a failed import aborts the whole script BEFORE the LOAD-OK
# sentinel is displayed, so the sentinel is a reliable discriminator. A bare REPL
# would evaluate the next form after the exception and still print LOAD-OK,
# making the check silently vacuous -- hence the always-on refute_loads teeth,
# which prove the discriminator actually bites by rejecting a bogus sub-library.
#
# The test mutates no tracked file: probes live under a private per-PID TMPDIR
# removed by the exit trap. A bare host without petite degrades to a SKIP (not a
# FAIL); under `nix develop` both engines are present so all six cells run.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC="$ROOT/src"
TMPDIR="${TMPDIR:-/tmp}/hafod-load-test-$$"
PASS=0
FAIL=0
TOTAL=0

mkdir -p "$TMPDIR"

# The load test mutates nothing in the tree, so cleanup only removes TMPDIR.
cleanup() {
    rm -rf "$TMPDIR"
}
trap cleanup EXIT INT TERM HUP

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

section() {
    printf "\n=== %s ===\n" "$1"
}

# Resolve the two interpreters. Honour a SCHEME= override (the make recipe passes
# SCHEME='$(SCHEME)', so the build's chosen Chez and a `make test-load SCHEME=...`
# override both flow through here), then DERIVE petite next to the resolved
# scheme so the pair is guaranteed to be the SAME Chez even if a different-version
# petite shadows it on PATH. Fall back to a bare petite on PATH, and SKIP (not
# FAIL) an engine that cannot be found so a bare host outside the dev shell
# degrades gracefully.
SCHEME="${SCHEME:-$(command -v scheme 2>/dev/null || command -v chez-scheme 2>/dev/null || true)}"
SCHEME_PATH="$(command -v "$SCHEME" 2>/dev/null || true)"
PETITE=""
if [ -n "$SCHEME_PATH" ]; then
    candidate="$(dirname "$SCHEME_PATH")/petite"
    if [ -x "$candidate" ]; then
        PETITE="$candidate"
    fi
fi
if [ -z "$PETITE" ]; then
    PETITE="$(command -v petite 2>/dev/null || true)"
fi

# assert_loads DESC ENGINE LIBSPEC -- passes iff the library imports cleanly:
# stdout is exactly the LOAD-OK sentinel AND stderr is empty. -q suppresses the
# banner so stdout is precisely the sentinel; --script makes a failed import
# abort before the sentinel; --libdirs "$SRC" is the source/fasl path (never
# bin/hafod.so); the per-call </dev/null is belt-and-braces hang-proofing.
assert_loads() {
    printf '(import %s)(display "LOAD-OK")\n' "$3" > "$TMPDIR/probe.ss"
    set +e
    out=$("$2" -q --libdirs "$SRC" --script "$TMPDIR/probe.ss" </dev/null 2>"$TMPDIR/err")
    code=$?
    set -e
    if [ "$out" = "LOAD-OK" ] && [ ! -s "$TMPDIR/err" ]; then
        pass "$1"
    else
        fail "$1" "LOAD-OK, empty stderr, exit 0" "out=[$out] code=$code err=[$(cat "$TMPDIR/err")]"
    fi
}

# refute_loads DESC ENGINE LIBSPEC -- the always-on non-vacuity teeth. Passes
# iff a bogus sub-library is REJECTED (no LOAD-OK, non-empty stderr), proving the
# assert_loads sentinel actually discriminates. MUST use --script for the same
# reason: a bare REPL would print LOAD-OK past the failed import, making the
# whole test vacuous.
refute_loads() {
    printf '(import %s)(display "LOAD-OK")\n' "$3" > "$TMPDIR/probe.ss"
    set +e
    out=$("$2" -q --libdirs "$SRC" --script "$TMPDIR/probe.ss" </dev/null 2>"$TMPDIR/err")
    set -e
    if [ "$out" != "LOAD-OK" ] && [ -s "$TMPDIR/err" ]; then
        pass "$1"
    else
        fail "$1" "rejected (no LOAD-OK, non-empty stderr)" "out=[$out]"
    fi
}

# ======================================================================
section "petite load matrix"
# ======================================================================

# petite cannot compile: a clean load here proves the built fasl set is
# self-consistent and complete without any on-the-fly compilation.
if [ -z "$PETITE" ]; then
    printf "  SKIP: petite not found -- petite load cells skipped\n"
else
    assert_loads "petite loads (hafod)"       "$PETITE" "(hafod)"
    assert_loads "petite loads (hafod posix)" "$PETITE" "(hafod posix)"
fi

# ======================================================================
section "scheme load matrix"
# ======================================================================

# Full scheme validates the per-library source path (distinct from the
# pre-linked bin/hafod.so image) and is the fallback validator on a host
# without petite.
if [ -z "$SCHEME" ]; then
    printf "  SKIP: scheme not found -- scheme load cells skipped\n"
else
    assert_loads "scheme loads (hafod)"       "$SCHEME" "(hafod)"
    assert_loads "scheme loads (hafod posix)" "$SCHEME" "(hafod posix)"
fi

# ======================================================================
section "non-vacuity (teeth)"
# ======================================================================

# A bogus sub-library must be rejected under BOTH engines; otherwise the LOAD-OK
# sentinel above would prove nothing.
if [ -z "$PETITE" ]; then
    printf "  SKIP: petite not found -- petite teeth skipped\n"
else
    refute_loads "petite rejects a missing library (teeth)" "$PETITE" "(hafod does-not-exist)"
fi
if [ -z "$SCHEME" ]; then
    printf "  SKIP: scheme not found -- scheme teeth skipped\n"
else
    refute_loads "scheme rejects a missing library (teeth)" "$SCHEME" "(hafod does-not-exist)"
fi

# ======================================================================
# Summary
# ======================================================================

printf "\n=== Summary ===\n"
printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
# A zero-assertion run means neither engine was found: treat an all-skipped
# self-test as a failure rather than a vacuous green -- a regression-catcher
# that asserts nothing is worthless.
if [ "$TOTAL" -eq 0 ]; then
    printf "FAIL: no Scheme engine found -- the load self-test asserted nothing\n" >&2
    exit 1
fi
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
