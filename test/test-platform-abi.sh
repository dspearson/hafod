#!/bin/sh
# test-platform-abi.sh -- Tests for the platform ABI fingerprint, the
# Rosetta/cross-build refusal helpers, and the capability-probed native link
# library selection.
#
# Drives tools/platform-fingerprint.sh and tools/refuse-cross-build.sh through
# their single-host branch matrix: the fingerprint is deterministic and changes
# on a simulated platform change; the refusal proceeds for every native target
# (Linux-x86_64, FreeBSD-amd64, macOS-Intel, macOS-arm, Linux-aarch64) and
# refuses a Rosetta-translated or cross/emulated build. Each branch is reached by
# injecting UNAME_S/UNAME_M/CC plus the test-only PROC_TRANSLATED/CC_DUMPMACHINE
# overrides, so the macOS/FreeBSD/Rosetta paths are exercised on a Linux host.
#
# It checks the native link library selection too: print-native-libs lists the
# host's capability-probed set (only the base libraries on a modern glibc, with
# no -ldl or -liconv); an unrecognised OS aborts the native link with a clear
# error and a non-zero exit without reaching the linker; that same error stays
# lazy so a non-link target such as clean still runs; and the present-vs-absent
# symbol link probe that drives the selection is shown to work both ways.
#
# It also exercises the check-platform drift gate end to end: it builds the tree
# so the gate has a matched target, asserts a clean tree reports no drift, then
# mutates the tracked platform-constants.ss and asserts the gate detects the
# drift and exits non-zero -- restoring the file (mtime preserved) on every exit
# path via a trap.
#
# This suite is run as a STANDALONE target, not folded into the `test:`
# aggregate: the check-platform cases drive a real `make` build and mutate the
# tracked constants (as the sibling standalone tests do), and a test that shells
# `make` must never be invoked from inside `make test`.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FINGERPRINT="$ROOT/tools/platform-fingerprint.sh"
REFUSE="$ROOT/tools/refuse-cross-build.sh"
PC_SS="$ROOT/src/hafod/internal/platform-constants.ss"
TMPDIR="${TMPDIR:-/tmp}/hafod-platform-abi-test-$$"
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

# Build with the flake-pinned Chez (10.3.0) so the working tree matches this
# toolchain before the gate inspects it; a different Chez is not what we test.
build() {
    nix develop -c make compile </dev/null
}

# Stash the tracked constants file the drift case mutates so the trap can always
# put it back. Preserve the timestamp (-p): bin/hafod.so depends on this file,
# so a restored copy with a bumped mtime would mark the launcher stale and force
# a needless rebuild. The trap restores on every exit path -- success, failure
# or signal -- so an early abort never leaks a mutated constants file.
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
section "platform fingerprint"
# ======================================================================

# One non-empty line on a default run.
fp1=$(sh "$FINGERPRINT" </dev/null)
if [ -n "$fp1" ]; then
    pass "fingerprint emits a non-empty line"
else
    fail "fingerprint emits a non-empty line" "non-empty" "empty"
fi
lines=$(sh "$FINGERPRINT" </dev/null | wc -l | tr -d '[:space:]')
assert_eq "fingerprint emits exactly one line" "1" "$lines"

# Two native runs must agree (deterministic for a fixed ABI).
fp2=$(sh "$FINGERPRINT" </dev/null)
assert_eq "fingerprint is deterministic across two native runs" "$fp1" "$fp2"

# A simulated platform change must drift the fingerprint.
fpd=$(UNAME_S=Darwin UNAME_M=arm64 sh "$FINGERPRINT" </dev/null)
if [ "$fpd" != "$fp1" ]; then
    pass "fingerprint changes on a simulated platform change"
else
    fail "fingerprint changes on a simulated platform change" "differs from native" "$fpd"
fi

# ======================================================================
section "cross-build refusal"
# ======================================================================

# Rosetta 2 (explicit proc_translated=1) must refuse with a clear message.
set +e
err=$(UNAME_S=Darwin PROC_TRANSLATED=1 sh "$REFUSE" </dev/null 2>&1 >/dev/null)
code=$?
set -e
assert_nonzero "Rosetta build is refused" "$code"
assert_contains "Rosetta refusal names Rosetta" "Rosetta" "$err"

# A cross toolchain (host arm64, target x86_64) must refuse and name the target.
set +e
err=$(UNAME_M=arm64 CC_DUMPMACHINE=x86_64-linux-gnu sh "$REFUSE" </dev/null 2>&1 >/dev/null)
code=$?
set -e
assert_nonzero "cross toolchain is refused" "$code"
assert_contains "cross refusal names the toolchain target" "toolchain target" "$err"

# Native Linux-x86_64 must proceed silently.
set +e
err=$(UNAME_S=Linux UNAME_M=x86_64 CC_DUMPMACHINE=x86_64-linux-gnu sh "$REFUSE" </dev/null 2>&1 >/dev/null)
code=$?
set -e
assert_eq "native Linux-x86_64 proceeds" "0" "$code"
refute_contains "native Linux-x86_64 prints no refusal" "refuse" "$err"

# FreeBSD-amd64: amd64 normalises to x86_64, so a native build must proceed.
set +e
UNAME_S=FreeBSD UNAME_M=amd64 CC_DUMPMACHINE=x86_64-unknown-freebsd14 sh "$REFUSE" </dev/null >/dev/null 2>&1
code=$?
set -e
assert_eq "FreeBSD-amd64 proceeds (amd64 normalised to x86_64)" "0" "$code"

# macOS-Intel: an empty proc_translated must proceed (the key is absent there).
set +e
UNAME_S=Darwin PROC_TRANSLATED= UNAME_M=x86_64 CC_DUMPMACHINE=x86_64-apple-darwin22 sh "$REFUSE" </dev/null >/dev/null 2>&1
code=$?
set -e
assert_eq "macOS-Intel proceeds (empty proc_translated)" "0" "$code"

# macOS-arm64: arm64 normalises to aarch64, so a native build must proceed.
set +e
UNAME_S=Darwin UNAME_M=arm64 CC_DUMPMACHINE=aarch64-apple-darwin22 PROC_TRANSLATED= sh "$REFUSE" </dev/null >/dev/null 2>&1
code=$?
set -e
assert_eq "macOS-arm64 proceeds (arm64 normalised to aarch64)" "0" "$code"

# Linux-aarch64: host and target agree, so a native build must proceed.
set +e
UNAME_S=Linux UNAME_M=aarch64 CC_DUMPMACHINE=aarch64-linux-gnu sh "$REFUSE" </dev/null >/dev/null 2>&1
code=$?
set -e
assert_eq "Linux-aarch64 proceeds" "0" "$code"

# ======================================================================
section "linker selection"
# ======================================================================

# Selection on this host: the capability probes fold dlopen and iconv_open into
# libc on a modern glibc, so print-native-libs lists only the base set -- no
# -ldl, no -liconv. Run inside the dev shell so the probes see the same -I/-L
# (CFLAGS/LDFLAGS) the real build links against.
libs=$(nix develop -c make print-native-libs </dev/null)
assert_contains "print-native-libs lists -lm"       "-lm"       "$libs"
assert_contains "print-native-libs lists -llz4"     "-llz4"     "$libs"
assert_contains "print-native-libs lists -lz"       "-lz"       "$libs"
assert_contains "print-native-libs lists -lncurses" "-lncurses" "$libs"
assert_contains "print-native-libs lists -lpthread" "-lpthread" "$libs"
refute_contains "this host needs no -ldl (dlopen is in libc)"        "-ldl"    "$libs"
refute_contains "this host needs no -liconv (iconv_open is in libc)" "-liconv" "$libs"

# An unrecognised OS aborts the native link with a clear error and a non-zero
# exit, without ever reaching the linker. A dry run (-n) is enough: the recursive
# NATIVE_LIBS expands its $(error) as soon as the link recipe is read.
set +e
err=$(make -n native UNAME_S=Plan9 2>&1)
code=$?
set -e
assert_nonzero "an unknown OS fails the native link" "$code"
assert_contains "the unknown-OS error names the OS" "Unsupported OS 'Plan9'" "$err"

# The error stays lazy: a non-link target such as clean must still run on that
# same unknown OS, because the recursive '=' keeps NATIVE_LIBS (and its $(error))
# from expanding for a target that never references it. A simply-expanded ':='
# here would abort every target at parse time. A dry run keeps the test from
# actually deleting the build.
set +e
make -n clean UNAME_S=Plan9 >/dev/null 2>&1
code=$?
set -e
assert_eq "an unknown OS does not break a non-link target (clean)" "0" "$code"

# Probe mechanism, independent of which libraries this host happens to need: a
# program that names a symbol present in libc (printf) links with no extra
# library, so the probe adds nothing; a program that names an absent symbol
# fails to link, which is exactly what makes the probe append a fallback flag.
set +e
printf 'char printf();int main(){return(int)(long)printf;}' | cc -x c - -o /dev/null 2>/dev/null
present=$?
printf 'char nonesuch_xyzzy();int main(){return(int)(long)nonesuch_xyzzy;}' | cc -x c - -o /dev/null 2>/dev/null
absent=$?
set -e
assert_eq "a present symbol links with no fallback library" "0" "$present"
assert_nonzero "an absent symbol fails to link (drives the fallback)" "$absent"

# ======================================================================
section "check-platform drift gate"
# ======================================================================

# Clean case: bring the tree in line with this toolchain, then the gate must
# report no drift and exit 0.
build

set +e
out=$(nix develop -c make check-platform </dev/null 2>&1)
code=$?
set -e
assert_eq "check-platform exits 0 on a matched tree" "0" "$code"
assert_contains "check-platform reports no drift on a matched tree" "no drift" "$out"

# Teeth: a mutated tracked constants file must be detected. Append a recognisable
# line, run the gate (which regenerates a fresh copy and diffs it against the
# working-tree file), and assert it fails loudly and shows the offending line.
printf '%s\n' ';; drift-canary-9f3c2a does-not-belong' >> "$PC_SS"

set +e
out=$(nix develop -c make check-platform </dev/null 2>&1)
code=$?
set -e
assert_nonzero "check-platform exits non-zero on drift" "$code"
assert_contains "check-platform names the drift" "DRIFT" "$out"
assert_contains "check-platform shows the offending line in the diff" "drift-canary-9f3c2a" "$out"

# Restore immediately (the trap also restores on any exit path), mtime preserved
# via cp -p so the launcher's freshness graph is not perturbed.
cp -p "$TMPDIR/platform-constants.ss.orig" "$PC_SS"
assert_eq "the tracked constants file is restored with its original mtime" \
    "$PC_ORIG_MTIME" "$(mtime "$PC_SS")"

# ======================================================================
# Summary
# ======================================================================

printf "\n=== Summary ===\n"
printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
