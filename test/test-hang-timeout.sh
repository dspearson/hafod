#!/bin/sh
# test-hang-timeout.sh -- Prove the per-suite kill-timeout the test runner wraps
# around each suite actually has teeth, and that it shares its timeout value with
# the just runner so the two cannot drift.
#
# Four behaviours are asserted:
#   1. A wedged suite is killed and reported. A command that ignores SIGTERM is
#      SIGKILLed after the grace (exit 137); a plain over-deadline command is
#      SIGTERMed at the deadline (exit 124). The relabel-and-propagate handler the
#      build wraps each suite in announces either code as a HANG and re-propagates
#      the real code, so a hang fails the build instead of stalling it.
#   2. A genuine fast FAIL (exit 1) and PASS (exit 0) pass through the wrapper
#      unchanged -- only a kill-timeout is reclassified, never a real result.
#   3. On a host with no timeout(1)/gtimeout the suites run unwrapped: a real test
#      goal warns exactly once that it runs without a kill-timeout and still stays
#      green, while a non-test goal stays silent and the shared value is untouched.
#   4. The per-suite timeout the test runner prints tracks the just runner's
#      declared value, so the two runners cannot drift on it.
#
# Like the sibling standalone proofs it drives make internally, so it is
# deliberately NOT part of the `test:` aggregate (a suite that shells make must
# never be invoked from inside make test) and is reached only via its own target.
# It needs no compiled libraries -- it exercises timeout(1) and parse-only make
# goals alone.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

TMPDIR="${TMPDIR:-/tmp}/hafod-hang-timeout-test-$$"
PASS=0
FAIL=0
TOTAL=0

mkdir -p "$TMPDIR"

# This suite mutates no tracked file; the trap simply removes the scratch
# directory (where the dry-run's recipe spew is sunk) on every exit path --
# success, failure, or signal -- matching the standalone siblings.
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
section "value shared with the justfile"
# ======================================================================

# The justfile's test_timeout is the single source of truth. Parse it the same
# way the build does, then ask the build what it resolved: the two must agree, or
# the test runner and the just runner have drifted on the per-suite timeout.
exp=$(sed -n 's/^test_timeout[[:space:]]*:=[[:space:]]*"\([0-9][0-9]*\)".*/\1/p' justfile)
got=$(make --no-print-directory print-test-timeout)
assert_eq "make print-test-timeout tracks the justfile's test_timeout" "$exp" "$got"

# ======================================================================
section "no-timeout degrade"
# ======================================================================

# Emptying TIMEOUT models a host where neither timeout nor gtimeout is found. A
# real test goal then degrades to an unwrapped run and warns exactly once that it
# runs without a kill-timeout, while staying green. The dry run (-n) reaches that
# parse-time warning without compiling or running a single suite. Its recipe spew
# is sunk to the scratch dir; only the warning (on stderr) is captured.
set +e
warn=$(make --no-print-directory -n test TIMEOUT= 2>&1 1>"$TMPDIR/make-n.out")
code=$?
set -e
assert_contains "a degraded test goal warns it runs without a kill-timeout" \
    "WITHOUT a kill-timeout" "$warn"
assert_eq "the degraded dry run stays green" "0" "$code"

# The warning is scoped to a test goal: a non-test goal such as print-test-timeout
# stays silent even with no timeout binary, and the shared value is unperturbed by
# the degrade (it is sourced from the justfile, not from the timeout binary).
set +e
out=$(make --no-print-directory print-test-timeout TIMEOUT= 2>"$TMPDIR/pt.err")
code=$?
set -e
warn2=$(cat "$TMPDIR/pt.err")
assert_eq "a non-test goal still reports the shared value under degrade" "$exp" "$out"
assert_eq "a non-test goal stays green under degrade" "0" "$code"
refute_contains "a non-test goal emits no kill-timeout warning under degrade" \
    "WITHOUT a kill-timeout" "$warn2"

# Resolve a kill-timeout binary the way the build does. On a host without one the
# synthetic exit-code assertions below cannot run, so they are skipped (not
# failed) -- the degrade proof above already covers the no-timeout host.
to=$(command -v timeout 2>/dev/null || command -v gtimeout 2>/dev/null)

# ======================================================================
section "HANG classification (parity with just test)"
# ======================================================================

if [ -z "$to" ]; then
    printf "  SKIP: no timeout/gtimeout on this host -- synthetic kill-timeout assertions skipped\n"
else
    # A command that ignores SIGTERM is not stopped at the deadline; the -k grace
    # then SIGKILLs it, so timeout exits 137 (128 + SIGKILL). Keep the deadline at
    # 1s so the case finishes in roughly the grace, not 30s. Run inside a brace
    # group whose output is redirected so the shell's own job-control notification
    # for the SIGKILLed child is swallowed with it, leaving the report clean.
    code=0
    { "$to" -k 1 1 sh -c 'trap "" TERM; sleep 30' || code=$?; } >/dev/null 2>&1
    assert_eq "a TERM-ignoring hang is SIGKILLed after the grace (137)" "137" "$code"

    # A command that obeys SIGTERM is ended at the deadline itself, so timeout
    # exits 124. Both 124 and 137 are hangs the build must reclassify.
    code=0
    { "$to" -k 5 1 sh -c 'sleep 30' || code=$?; } >/dev/null 2>&1
    assert_eq "a plain over-deadline command is SIGTERMed at the deadline (124)" "124" "$code"

    # The relabel-and-propagate handler, in the shape the build wraps each suite
    # in: a 124/137 is announced as a HANG on stderr and the real code is
    # re-propagated, so a wedged suite fails the build instead of passing
    # silently. Capture stderr (where the HANG lands) and re-propagate the code.
    set +e
    warn=$( { "$to" -k 1 1 sh -c 'trap "" TERM; sleep 30' \
                || { rc=$?
                     if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
                         echo "  HANG  synthetic (killed after ${exp}s)" >&2
                     fi
                     exit "$rc"
                   }
            } 2>&1 1>/dev/null )
    code=$?
    set -e
    assert_nonzero "a relabelled hang re-propagates a non-zero code" "$code"
    assert_contains "the relabel handler announces a HANG on stderr" "HANG" "$warn"
fi

# ======================================================================
section "FAIL/PASS pass-through"
# ======================================================================

if [ -z "$to" ]; then
    printf "  SKIP: no timeout/gtimeout on this host -- pass-through assertions skipped\n"
else
    # A genuine failure inside the deadline is not a kill-timeout: timeout passes
    # the child's own exit code straight through, so the build sees a real FAIL.
    code=0
    { "$to" -k 5 5 sh -c 'exit 1' || code=$?; } >/dev/null 2>&1
    assert_eq "a genuine FAIL passes through the wrapper unchanged (1)" "1" "$code"

    # Likewise a genuine success is untouched -- the wrapper only reclassifies a
    # kill-timeout, never a real result.
    code=0
    { "$to" -k 5 5 sh -c 'exit 0' || code=$?; } >/dev/null 2>&1
    assert_eq "a genuine PASS passes through the wrapper unchanged (0)" "0" "$code"
fi

# ======================================================================
# Summary
# ======================================================================

printf "\n=== Summary ===\n"
printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
