#!/bin/sh
# test-version-guard.sh -- Tests for the bin/hafod startup version guard.
#
# Verifies that a Chez mismatch (or an interpreter that cannot compile the
# source path) produces a friendly, actionable two-line remediation and a
# NON-ZERO exit -- rather than a raw "incompatible fasl-object" crash or the
# silent exit-0 a petite source-path failure would otherwise give.
#
# Single-Chez safe: the mismatch is induced by rewriting the RECORDED build
# version (not the running interpreter), so it works even where only one Chez
# is installed.  Every file the test mutates is stashed up front and restored
# by the EXIT trap, so an early failure cannot leak a bad recorded version or a
# missing bin/hafod.so into the working tree.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
HAFOD="$ROOT/bin/hafod"
CHEZVER_SS="$ROOT/src/hafod/internal/chez-version.ss"
HAFOD_SO="$ROOT/bin/hafod.so"
TMPDIR="${TMPDIR:-/tmp}/hafod-version-guard-test-$$"
PASS=0
FAIL=0
TOTAL=0

mkdir -p "$TMPDIR"

# Stash the files the cases mutate so the trap can always put them back.
# Preserve timestamps (-p): Chez decides whether to recompile a library from
# source by comparing the .ss mtime against its .so/.wpo, so a restored
# chez-version.ss MUST keep its original mtime or the matched bin/hafod.so is
# treated as stale and the happy-path case fails with a compilation-instance
# mismatch.
[ -f "$CHEZVER_SS" ] && cp -p "$CHEZVER_SS" "$TMPDIR/chez-version.ss.orig"
[ -f "$HAFOD_SO" ] && cp -p "$HAFOD_SO" "$TMPDIR/hafod.so.orig"

cleanup() {
    # Restore the recorded version module, mtime included (-p).
    if [ -f "$TMPDIR/chez-version.ss.orig" ]; then
        cp -p "$TMPDIR/chez-version.ss.orig" "$CHEZVER_SS"
    fi
    # Restore bin/hafod.so (a case moves it aside to exercise the source path).
    if [ -f "$TMPDIR/hafod.so.orig" ]; then
        cp -p "$TMPDIR/hafod.so.orig" "$HAFOD_SO"
    fi
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

refute_contains() {
    # $1=description $2=needle $3=haystack -- passes only if needle is ABSENT.
    case "$3" in
        *"$2"*) fail "$1" "must NOT contain '$2'" "$3" ;;
        *) pass "$1" ;;
    esac
}

assert_nonzero() {
    # $1=description $2=exit-code
    if [ "$2" -ne 0 ]; then
        pass "$1"
    else
        fail "$1" "non-zero exit" "$2"
    fi
}

section() {
    printf "\n=== %s ===\n" "$1"
}

# ======================================================================
section "G1: recorded-vs-running version mismatch (single-Chez safe)"
# ======================================================================

# Rewrite the recorded triple to an impossible value.  The running Chez is
# unchanged (matched), so the wrapper sees recorded != running and must emit
# the friendly remediation instead of loading bin/hafod.so.
sed "s/build-chez-version-number '([0-9 ]*)/build-chez-version-number '(0 0 0)/" \
    "$CHEZVER_SS" > "$CHEZVER_SS.tmp"
mv "$CHEZVER_SS.tmp" "$CHEZVER_SS"

set +e
err=$("$HAFOD" --version </dev/null 2>&1 >/dev/null)
code=$?
set -e

assert_contains "G1 names the required version" "0.0.0 required" "$err"
assert_contains "G1 suggests nix develop" "nix develop" "$err"
assert_contains "G1 suggests a rebuild" "compile-wpo" "$err"
refute_contains "G1 is not a raw fasl-object error" "incompatible fasl-object" "$err"
assert_nonzero "G1 exits non-zero" "$code"

# Restore the recorded version before the next case (the trap also restores it),
# mtime included so the matched bin/hafod.so is not seen as stale.
cp -p "$TMPDIR/chez-version.ss.orig" "$CHEZVER_SS"

# ======================================================================
section "G2: source path under an interpreter that cannot compile FFI"
# ======================================================================

# Move bin/hafod.so aside so the wrapper takes the source path, and force a
# compiler-less interpreter (petite).  This is the case whose raw failure
# exits 0 -- the guard's value here is the NON-ZERO exit, so assert both the
# message and the code.
#
# Single-Chez-safe: G1 (version mismatch) runs before G2 in the wrapper, so if
# the recorded build version differs from the petite under test, G1 fires first
# and G2 is never reached.  That is the project's canonical setup (flake Chez
# vs a system petite of another version).  So pin the recorded version to
# petite's own (scheme-version-number) for this case, which makes G1 pass and
# lets control reach G2.  The EXIT trap restores chez-version.ss, and the
# happy-path case below restores it explicitly before it runs.
PETITE="$(command -v petite 2>/dev/null || true)"
ptrip=""
if [ -n "$PETITE" ]; then
    ptrip=$(printf '%s' '(call-with-values (lambda () (scheme-version-number)) (lambda (a b c)(display a)(display " ")(display b)(display " ")(display c)))' \
        | "$PETITE" -q 2>/dev/null)
fi
case "$ptrip" in
    [0-9]*\ [0-9]*\ [0-9]*) ptrip_ok=1 ;;
    *) ptrip_ok=0 ;;
esac
if [ -n "$PETITE" ] && [ -f "$HAFOD_SO" ] && [ "$ptrip_ok" -eq 1 ]; then
    # Pin the recorded version to petite's version so G1 is satisfied and G2 runs.
    sed "s/build-chez-version-number '([0-9 ]*)/build-chez-version-number '($ptrip)/" \
        "$CHEZVER_SS" > "$CHEZVER_SS.tmp"
    mv "$CHEZVER_SS.tmp" "$CHEZVER_SS"

    mv "$HAFOD_SO" "$TMPDIR/hafod.so.stash"

    set +e
    err=$(HAFOD_SCHEME="$PETITE" "$HAFOD" --version </dev/null 2>&1 >/dev/null)
    code=$?
    set -e

    # Put the .so back immediately (the trap also restores it on early exit).
    mv "$TMPDIR/hafod.so.stash" "$HAFOD_SO"
    # Restore the recorded version before the next case (the trap also restores
    # it), mtime included so the matched bin/hafod.so is not seen as stale.
    cp -p "$TMPDIR/chez-version.ss.orig" "$CHEZVER_SS"

    assert_contains "G2 reports the source path cannot compile" "cannot compile the source path" "$err"
    assert_contains "G2 suggests a rebuild" "compile-wpo" "$err"
    assert_nonzero "G2 exits non-zero (not a silent exit 0)" "$code"
else
    printf "  SKIP: petite not found, version unreadable, or bin/hafod.so absent -- G2 case skipped\n"
fi

# ======================================================================
section "Happy path: matched version, present .so -- guard is transparent"
# ======================================================================

# With the real recorded version and the matched Chez + present .so, the guard
# must not fire: --version prints the version and exits 0.
set +e
out=$("$HAFOD" --version </dev/null 2>&1)
code=$?
set -e

assert_contains "happy path prints the hafod version" "hafod" "$out"
refute_contains "happy path emits no remediation" "nix develop" "$out"
assert_eq "happy path exits 0" "0" "$code"

# ======================================================================
# Summary
# ======================================================================

printf "\n=== Summary ===\n"
printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
