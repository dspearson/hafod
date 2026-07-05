#!/bin/sh
# test-c-probe.sh -- Standalone proof that the independent C-probe cross-check
# has teeth: it passes on a matched tree and fails -- naming the offending
# constant -- when a single value is corrupted, all without ever writing the
# tracked constants file.
#
# The cross-check (`make check-c-probe`) compiles a separately authored C probe
# that computes each ABI fact directly, then a comparator asserts the committed
# platform-constants.ss agrees with it, naming any constant that disagrees. A
# gate that cannot be shown to fail on a wrong value gives false confidence, so
# this harness is the discriminating evidence that the gate actually bites.
#
# Three behaviours are checked:
#   1. a matched tree passes the cross-check (exit 0);
#   2. corrupting one constant in a TEMPORARY copy -- pointed at through the
#      overridable PC_SS make variable -- makes the gate exit non-zero and name
#      the corrupted constant;
#   3. the tracked platform-constants.ss is never written: its modification time
#      is unchanged after the run (a cp -p stash plus a trap restore stand as
#      defence in depth, but the mtime assertion is the positive proof).
#
# The corruption only ever touches a copy under a private temporary directory;
# the tracked file is read (cp -p) but is never the target of a redirect or sed.
# Like the sibling standalone proofs it shells `make` internally, so it is
# deliberately NOT folded into the `test:` aggregate.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PC_SS="$ROOT/src/hafod/internal/platform-constants.ss"
TMPDIR="${TMPDIR:-/tmp}/hafod-c-probe-test-$$"
PASS=0
FAIL=0
TOTAL=0

mkdir -p "$TMPDIR"

# Portable modification time in seconds: GNU stat on Linux, BSD stat on macOS.
mtime() {
    if [ "$(uname -s)" = "Darwin" ]; then
        stat -f %m "$1"
    else
        stat -c %Y "$1"
    fi
}

# Stash the tracked constants file so the trap can always put it back, even
# though the cross-check is pointed at a temporary copy and so never writes it.
# Preserve the timestamp (-p): bin/hafod.so depends on this file, so a restored
# copy with a bumped mtime would mark the launcher stale and force a needless
# rebuild. The trap restores on every exit path -- success, failure or signal --
# so an early abort can never leak a mutated constants file.
cp -p "$PC_SS" "$TMPDIR/platform-constants.ss.orig"
PC_ORIG_MTIME=$(mtime "$PC_SS")

cleanup() {
    if [ -f "$TMPDIR/platform-constants.ss.orig" ]; then
        cp -p "$TMPDIR/platform-constants.ss.orig" "$PC_SS"
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
section "cross-check on a matched tree"
# ======================================================================

# Happy path: on a tree that matches this toolchain and ABI the probe agrees
# with every committed constant, so the gate exits 0. Every make call goes
# through the flake-pinned Chez (nix develop -c); the system Chez is not what we
# are testing here.
set +e
out=$(nix develop -c make check-c-probe </dev/null 2>&1)
code=$?
set -e
assert_eq "a matched tree passes the cross-check" "0" "$code"

# ======================================================================
section "cross-check teeth"
# ======================================================================

# Corrupt ONE value in a TEMPORARY copy and point the gate at it through the
# overridable PC_SS variable. The probe still computes the true offset (48), so
# the rewritten 999 must surface as a named mismatch and a non-zero exit. The
# tracked file is only ever read here (cp -p); both the sed input and the
# redirect destination live under the private temporary directory.
cp -p "$PC_SS" "$TMPDIR/pc.ss"
sed 's/(define STAT-ST-SIZE 48)/(define STAT-ST-SIZE 999)/' "$TMPDIR/pc.ss" > "$TMPDIR/pc.bad.ss"

set +e
out=$(nix develop -c make check-c-probe PC_SS="$TMPDIR/pc.bad.ss" </dev/null 2>&1)
code=$?
set -e
assert_nonzero "a corrupted constant fails the cross-check" "$code"
assert_contains "the failure names the corrupted constant" "STAT-ST-SIZE" "$out"

# ======================================================================
section "tracked constants file untouched"
# ======================================================================

# Positive proof that the gate and the corruption operated only on temporary
# files: the tracked constants file's modification time is exactly what it was
# before the run, so bin/hafod.so's freshness graph was never perturbed.
assert_eq "the tracked constants file mtime is unchanged" \
    "$PC_ORIG_MTIME" "$(mtime "$PC_SS")"

# ======================================================================
# Summary
# ======================================================================

printf "\n=== Summary ===\n"
printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
