#!/bin/sh
# test-hafod-so-fresh.sh -- Prove bin/hafod.so is never stale.
#
# bin/hafod.so is a real Make target whose prerequisites are the library
# sources. This test proves the freshness wiring works end to end: after a
# source changes, plain `make compile` rebuilds the launcher (its mtime
# advances), and with nothing changed a second `make compile` leaves it alone.
#
# It deliberately touches a real source file mid-run, so it is invoked ONLY as a
# standalone target -- never folded into the `test:` aggregate, which it would
# otherwise disturb. The touched source's modification time is restored on every
# exit path (success, failure, or signal) so a working tree is never left dirty
# and a re-run is deterministic.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

# A stable, tracked library source the launcher depends on. Avoid the generated
# version.ss/chez-version.ss: a build's gen step could rewrite those and confuse
# the timing. internal/base.ss is a foundational, hand-maintained library.
SRC="src/hafod/internal/base.ss"
SO="bin/hafod.so"

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

section() {
    printf "\n=== %s ===\n" "$1"
}

# Portable modification time in seconds: GNU stat on Linux, BSD stat on macOS.
mtime() {
    if [ "$(uname -s)" = "Darwin" ]; then
        stat -f %m "$1"
    else
        stat -c %Y "$1"
    fi
}

# Build only with the flake-pinned Chez; a different Chez would produce
# an incompatible launcher and is not what this test is about.
build() {
    nix develop -c make compile </dev/null
}

# Restore the touched source's modification time on any exit path so the tree is
# never left dirty. SRC_REF is a copy stamped with the original mtime; touch -r
# copies that timestamp back onto the real source.
SRC_REF="$(mktemp)"
touch -r "$SRC" "$SRC_REF"
trap 'touch -r "$SRC_REF" "$SRC"; rm -f "$SRC_REF"' EXIT INT TERM HUP

section "bin/hafod.so freshness"

# 1. Ensure a current launcher exists.
build

# 2. It must exist after a build.
if [ -f "$SO" ]; then
    pass "$SO exists after make compile"
else
    fail "$SO exists after make compile" "present" "absent"
    printf "\n=== Summary ===\n"
    printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
    exit 1
fi

# 3. Record the launcher's modification time.
before="$(mtime "$SO")"

# 4. Touch a source. Sleep first so the filesystem's mtime resolution can
#    register an advance even on coarse-grained (1s) timestamps.
sleep 1
touch "$SRC"

# 5. Rebuild: the launcher must rebuild because a prerequisite changed.
build

# 6. The launcher's mtime must have strictly advanced.
after="$(mtime "$SO")"
if [ "$after" -gt "$before" ]; then
    pass "$SO rebuilt after touching $SRC (mtime advanced)"
else
    fail "$SO rebuilt after touching $SRC" "mtime > $before" "$after"
fi

# 7. No-op property: with nothing changed, a further build leaves it untouched.
sleep 1
build
again="$(mtime "$SO")"
assert_eq "$SO is a no-op when nothing changed (mtime unchanged)" "$after" "$again"

printf "\n=== Summary ===\n"
printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
if [ "$FAIL" -gt 0 ]; then exit 1; fi
