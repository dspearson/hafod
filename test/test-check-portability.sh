#!/bin/sh
# test-check-portability.sh -- Standalone proof that the structural portability
# audit has teeth: it passes on the tracked tree and fails -- naming the
# offender -- when a bypass of the platform hub or a resurrected FFI shim is
# introduced, all without ever mutating the tracked source.
#
# `make check-portability` (tools/check-portability.sh) asserts the aggregate
# structural invariants the numeric-constant gates do not cover: exactly one
# reader of (machine-type) (the internal platform hub), no runtime FFI-helper
# loader, and no removed C variadic shim symbol. A gate that cannot be shown to
# fail on a real violation gives false confidence, so this harness is the
# discriminating evidence that it bites.
#
# The violations are only ever written into a private temporary fixture tree
# handed to the audit via its overridable scan-root argument; the tracked
# sources are never the target of a redirect. Like the sibling standalone
# proofs it is deliberately NOT folded into the `test:` aggregate.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
AUDIT="$ROOT/tools/check-portability.sh"
TMPDIR="${TMPDIR:-/tmp}/hafod-check-portability-test-$$"
PASS=0
FAIL=0
TOTAL=0

mkdir -p "$TMPDIR/hafod"
cleanup() { rm -rf "$TMPDIR"; }
trap cleanup EXIT INT TERM HUP

pass() { PASS=$((PASS + 1)); TOTAL=$((TOTAL + 1)); printf "  PASS: %s\n" "$1"; }
fail() {
    FAIL=$((FAIL + 1)); TOTAL=$((TOTAL + 1)); printf "  FAIL: %s\n" "$1"
    [ -n "$2" ] && printf "    detail: %s\n" "$2"
}

# 1. The tracked tree passes cleanly (exit 0).
if (cd "$ROOT" && sh "$AUDIT" >/dev/null 2>&1); then
    pass "tracked tree satisfies the structural portability invariants"
else
    fail "tracked tree unexpectedly failed the audit"
fi

# 2. A fixture that reads (machine-type) outside the hub fails, naming it.
printf '(library (bad) (export x) (import (chezscheme))\n  (define x (case (machine-type) [(a6le) 1] [else 2])))\n' \
    > "$TMPDIR/hafod/bad-mt.ss"
OUT=$(sh "$AUDIT" "$TMPDIR" 2>&1) && RC=0 || RC=$?
if [ "$RC" -ne 0 ] && printf '%s' "$OUT" | grep -q 'machine-type'; then
    pass "a (machine-type) read outside the hub is rejected and named"
else
    fail "audit did not reject a bypassing (machine-type) read" "rc=$RC"
fi
rm -f "$TMPDIR/hafod/bad-mt.ss"

# 3. A fixture that reintroduces the runtime FFI-helper loader fails, naming it.
printf '(library (bad2) (export y) (import (chezscheme))\n  (define y (load-ffi-helpers)))\n' \
    > "$TMPDIR/hafod/bad-helper.ss"
OUT=$(sh "$AUDIT" "$TMPDIR" 2>&1) && RC=0 || RC=$?
if [ "$RC" -ne 0 ] && printf '%s' "$OUT" | grep -q 'FFI-helper loader'; then
    pass "a resurrected runtime FFI-helper loader is rejected and named"
else
    fail "audit did not reject a resurrected FFI-helper loader" "rc=$RC"
fi

printf "\n%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
[ "$FAIL" -eq 0 ]
