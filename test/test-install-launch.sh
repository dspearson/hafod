#!/bin/sh
# test-install-launch.sh -- Prove an INSTALLED hafod launches correctly.
#
# The dev-tree happy path is exercised by test-version-guard.sh, but the layout
# end users actually run is the one `make install` produces: hafod.so at the
# install root, the libraries copied under src/, and the generated
# chez-version.ss reinstated so the launcher's version guard can read the
# recorded build-Chez triple. Reinstating that source with a fresh mtime once
# made every matched-Chez launch of an installed copy abort with "different
# compilation instance of (hafod internal chez-version)": the launcher runs
# with `--libdirs $HAFOD_ROOT/src`, so Chez saw the source as newer than its
# compiled object and recompiled it into an instance that differs from the one
# baked into the whole-program hafod.so. This test installs into a throwaway
# prefix and asserts the installed wrapper starts cleanly (and that a Chez
# mismatch still yields the friendly remediation), so that regression cannot
# return unnoticed.
#
# Heavy by design: it runs a full `make install`, so -- like test-ffi-no-helper
# and test-hafod-so-fresh -- it is a STANDALONE target, never folded into the
# `test:` aggregate.
#
# The install prefix is a real, resolvable directory (NOT DESTDIR staging): the
# installed wrapper hardcodes its final HAFOD_ROOT, so it must be invoked from
# the path it was installed to. The prefix is removed on every exit path
# (success, failure, or signal) so no throwaway tree is ever left behind.
#
# Single-Chez safe: the mismatch case rewrites the RECORDED build version in the
# installed chez-version.ss to an impossible triple, so the guard fires no
# matter which single Chez is present -- it never needs a second interpreter.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

PASS=0
FAIL=0
TOTAL=0

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

# A throwaway, resolvable install prefix. mktemp -d gives an absolute path that
# already exists, so the wrapper's hardcoded HAFOD_ROOT points somewhere real.
PREFIX="$(mktemp -d "${TMPDIR:-/tmp}/hafod-install-launch-test-XXXXXX")"
trap 'rm -rf "$PREFIX"' EXIT INT TERM HUP

HAFOD="$PREFIX/bin/hafod"
INSTALLED_SS="$PREFIX/lib/hafod/src/hafod/internal/chez-version.ss"

section "Install into a throwaway prefix"

# Build and install with the flake-pinned Chez (10.3.0); a different Chez would
# produce an incompatible launcher and is not what this test is about. Empty
# DESTDIR: the wrapper bakes in the final paths, so it must run from PREFIX.
make install PREFIX="$PREFIX" </dev/null

if [ -x "$HAFOD" ]; then
    pass "installed launcher exists and is executable"
else
    fail "installed launcher exists and is executable" "present" "absent"
    printf "\n=== Summary ===\n"
    printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
    exit 1
fi

section "Installed happy path: matched Chez starts cleanly"

# The default interpreter on PATH here is the flake Chez the libraries were
# built with, so the guard must stay transparent: --version prints the version
# and exits 0, with NO recompile-induced compilation-instance crash and NO raw
# fasl-object error.
set +e
out=$("$HAFOD" --version </dev/null 2>&1)
code=$?
set -e

assert_contains "installed --version prints the hafod version" "hafod" "$out"
assert_eq "installed --version exits 0" "0" "$code"
refute_contains "installed launch is not a compilation-instance crash" \
    "different compilation instance" "$out"
refute_contains "installed launch is not a raw fasl-object error" \
    "incompatible fasl-object" "$out"
refute_contains "installed happy path emits no remediation" "nix develop" "$out"

section "Installed mismatch: friendly remediation, non-zero exit"

# Single-Chez safe: rewrite the recorded triple in the INSTALLED source to an
# impossible value so the guard sees recorded != running whatever Chez runs.
# G1 fires before the dispatch loads hafod.so, so the rewritten mtime never
# reaches the recompile path -- the wrapper exits with the friendly message.
sed "s/build-chez-version-number '([0-9 ]*)/build-chez-version-number '(0 0 0)/" \
    "$INSTALLED_SS" > "$INSTALLED_SS.tmp"
mv "$INSTALLED_SS.tmp" "$INSTALLED_SS"

set +e
err=$("$HAFOD" --version </dev/null 2>&1 >/dev/null)
code=$?
set -e

assert_contains "installed mismatch names the required version" "0.0.0 required" "$err"
assert_contains "installed mismatch suggests nix develop" "nix develop" "$err"
refute_contains "installed mismatch is not a raw fasl-object error" \
    "incompatible fasl-object" "$err"
assert_nonzero "installed mismatch exits non-zero" "$code"

printf "\n=== Summary ===\n"
printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
if [ "$FAIL" -gt 0 ]; then exit 1; fi
