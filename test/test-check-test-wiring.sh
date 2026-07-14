#!/bin/sh
# test-check-test-wiring.sh -- Standalone proof that the test-wiring audit has
# teeth: it passes on the tracked tree and fails -- naming the offender -- on a
# suite nothing runs, a suite silenced by parking it in the exclusion list, a
# library wired in as a phantom pass, and a justfile that globs a second suite
# set of its own. None of it ever mutates the tracked tree.
#
# `make check-test-wiring` (tools/check-test-wiring.sh) is the gate that keeps
# the suite glob honest. A structural gate that nobody has ever seen fail is
# indistinguishable from a comment -- and a comment is precisely what the build
# had before it: the Makefile said the harness list must stay exhaustive, and
# nothing checked that it was. So every direction the audit claims is driven
# here against a fixture that violates it, and every case asserts BOTH the exit
# status and that the message names the offender. An audit that fails for the
# wrong reason is not evidence.
#
# Case 5 is not a violation but a guard against one. Almost every suite in the
# tree opens with (library-directories ...), and an audit that mistook that form
# for a library head would condemn nearly all of them as phantoms and take the
# whole build down. It is asserted to be ACCEPTED, and the assertion is a live
# one: loosen the audit's pattern from ^\(library[[:space:]] to ^\(library and
# this is the case that fails.
#
# Every violation is written only into a private fixture tree under TMPDIR and
# handed to the audit through its overridable scan root and list overrides; the
# tracked test/, Makefile and justfile are never the target of a redirect. Like
# the sibling standalone proofs it is deliberately NOT folded into the `test:`
# aggregate -- though the audit it exercises very much is.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
AUDIT="$ROOT/tools/check-test-wiring.sh"
TMPDIR="${TMPDIR:-/tmp}/hafod-check-test-wiring-test-$$"
PASS=0
FAIL=0
TOTAL=0

mkdir -p "$TMPDIR"
cleanup() { rm -rf "$TMPDIR"; }
trap cleanup EXIT INT TERM HUP

pass() { PASS=$((PASS + 1)); TOTAL=$((TOTAL + 1)); printf "  PASS: %s\n" "$1"; }
fail() {
    FAIL=$((FAIL + 1)); TOTAL=$((TOTAL + 1)); printf "  FAIL: %s\n" "$1"
    [ -n "${2:-}" ] && printf "    detail: %s\n" "$2"
    return 0
}

# A fresh fixture directory per case, so no case can be satisfied -- or broken --
# by a file another one left behind.
mkcase() { mkdir -p "$TMPDIR/$1"; printf '%s\n' "$TMPDIR/$1"; }

# A justfile that does the right thing: it asks make for the suite set. Cases
# 2-5 point the audit at this one so that direction 4 stays quiet and each case
# fails for its own reason, not for a justfile it never touched.
GOOD_JUST="$TMPDIR/justfile-good"
cat > "$GOOD_JUST" <<'JUST'
test:
    for name in $(make print-test-targets); do
      scheme --libdirs .:src --script "test/$name.ss"
    done
JUST

# ---------------------------------------------------------------------------
# 1. The tracked tree passes -- and the audit takes the suite set from make.
#
# The second half is not decoration. The whole point of the gate is that there is
# exactly ONE answer to "which suites does the build run", and it is make's. An
# audit that quietly re-globbed the tree would agree with itself for ever while
# the build ran something else -- the very drift that produced the defect. So:
# handed its lists, it must not shell make at all (proving no recursion, which is
# what lets it live inside the test: aggregate); and denied both its lists and
# make, it must REFUSE rather than fall back on a glob of its own.
#
# The first assertion is stronger than it looks, because this harness is itself
# run BY make: MAKELEVEL is in the environment, so the audit's fallback shells a
# SUB-make, which turns on -w of its own accord and prefixes its stdout with
# "make[1]: Entering directory ...". An audit that read that back as a suite list
# would see real suites as unwired and the word "Entering" as a harness file. The
# inherited environment is left exactly as make hands it over, so this case stays
# a live guard on that -- do not be tempted to unset MAKELEVEL to tidy it up.
#
# The list fetches BELOW are this harness's own, and must be clean for the same
# reason -- hence --no-print-directory on each.
# ---------------------------------------------------------------------------
if (cd "$ROOT" && sh "$AUDIT" >/dev/null 2>&1); then
    if (cd "$ROOT" && WIRING_TARGETS="$(make --no-print-directory print-test-targets)" \
        WIRING_HARNESS="$(make --no-print-directory print-test-harness)" MAKE=false \
        sh "$AUDIT" >/dev/null 2>&1); then
        OUT=$(cd "$ROOT" && MAKE=false sh "$AUDIT" 2>&1) && RC=0 || RC=$?
        if [ "$RC" -ne 0 ] && printf '%s' "$OUT" | grep -q 'will not guess'; then
            pass "the tracked tree is soundly wired, and the audit sources the suite set from make (no sub-make when handed it; refuses to guess without it)"
        else
            fail "audit did not refuse when denied both its lists and make -- it may be re-deriving the suite glob itself" "rc=$RC"
        fi
    else
        fail "audit shelled make even when handed its lists (MAKE=false broke it) -- it would recurse inside the test: aggregate"
    fi
else
    fail "the tracked tree unexpectedly failed the wiring audit"
fi

# ---------------------------------------------------------------------------
# 2. An unwired suite. The original defect: a file nothing runs, and nothing
#    saying so. It must be named -- and the correctly-wired file beside it must
#    not be.
# ---------------------------------------------------------------------------
C=$(mkcase unwired)
printf '(display "wired suite\\n")\n' > "$C/test-wired.ss"
printf '(display "nobody runs me\\n")\n' > "$C/orphan.ss"
OUT=$(WIRING_TARGETS="test-wired" WIRING_HARNESS="" JUSTFILE="$GOOD_JUST" \
    sh "$AUDIT" "$C" 2>&1) && RC=0 || RC=$?
if [ "$RC" -ne 0 ] &&
    printf '%s' "$OUT" | grep -q 'orphan.ss is wired into nothing' &&
    ! printf '%s' "$OUT" | grep -q 'test-wired.ss'; then
    pass "a suite wired into nothing is rejected and named (and the wired suite beside it is not)"
else
    fail "audit did not reject a suite that nothing runs" "rc=$RC"
fi

# ---------------------------------------------------------------------------
# 3. A silenced suite. The exclusion list is the obvious place to quiet an
#    inconvenient failure: park the suite's name there and it stops running, and
#    stops failing with it. A name in that list must have a real library behind
#    it.
# ---------------------------------------------------------------------------
C=$(mkcase silenced)
printf '(display "a failing suite somebody wants quiet\\n")\n' > "$C/awkward.ss"
OUT=$(WIRING_TARGETS="" WIRING_HARNESS="$C/awkward.ss" JUSTFILE="$GOOD_JUST" \
    sh "$AUDIT" "$C" 2>&1) && RC=0 || RC=$?
if [ "$RC" -ne 0 ] &&
    printf '%s' "$OUT" | grep -q 'awkward.ss' &&
    printf '%s' "$OUT" | grep -q 'first form is not (library'; then
    pass "a suite parked in the harness exclusion list to quiet it is rejected and named"
else
    fail "audit did not reject a plain suite hidden in the harness list" "rc=$RC"
fi

# ---------------------------------------------------------------------------
# 4. A phantom suite -- the direction that matters most, and the one no other
#    check in the build can make.
#
# Chez does not choke on a (library ...) form handed to --script: it defines the
# library and exits 0, having printed nothing. So a library that reaches the
# suite list is not a build error, it is a suite that asserts nothing and always
# passes. Nothing else in the build can tell that apart from a real green.
#
# Two fixtures, because the audit has to see through a shebang to catch this. A
# library whose file opens `#!chezscheme` presents an interpreter directive as
# its first line; an audit that read only that line would call it a suite and let
# it straight through. Both must be named.
# ---------------------------------------------------------------------------
C=$(mkcase phantom)
cat > "$C/test-phantom.ss" <<'SCM'
(library (fixture phantom) (export) (import (chezscheme)))
SCM
cat > "$C/test-phantom-shebang.ss" <<'SCM'
#!chezscheme
(library (fixture phantom-shebang) (export) (import (chezscheme)))
SCM
OUT=$(WIRING_TARGETS="test-phantom test-phantom-shebang" WIRING_HARNESS="" \
    JUSTFILE="$GOOD_JUST" sh "$AUDIT" "$C" 2>&1) && RC=0 || RC=$?
if [ "$RC" -ne 0 ] &&
    printf '%s' "$OUT" | grep -q 'test-phantom.ss is run as a suite but is a (library' &&
    printf '%s' "$OUT" | grep -q 'test-phantom-shebang.ss is run as a suite but is a (library'; then
    pass "a library wired in as a suite is rejected and named -- plain, and behind a shebang (it would otherwise be defined, exit 0 and pass in silence)"
else
    fail "audit did not reject a library wired in as a suite" "rc=$RC"
fi

# ---------------------------------------------------------------------------
# 5. The false-positive guard. Almost every real suite opens with
#
#        (library-directories '(("src" . "src") ("." . ".")))
#
#    which starts with the letters `library` and is not a library at all. An
#    audit matching a bare ^\(library would call nearly every suite in the tree a
#    phantom and fail the build outright. These files are correctly wired and
#    must be ACCEPTED -- with and without a shebang ahead of them.
# ---------------------------------------------------------------------------
C=$(mkcase libdirs)
cat > "$C/test-libdirs.ss" <<'SCM'
(library-directories '(("src" . "src") ("." . ".")))
(import (chezscheme))
(display "a perfectly ordinary suite\n")
SCM
cat > "$C/test-libdirs-shebang.ss" <<'SCM'
#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (chezscheme))
(display "a perfectly ordinary suite, shebanged\n")
SCM
OUT=$(WIRING_TARGETS="test-libdirs test-libdirs-shebang" WIRING_HARNESS="" \
    JUSTFILE="$GOOD_JUST" sh "$AUDIT" "$C" 2>&1) && RC=0 || RC=$?
if [ "$RC" -eq 0 ]; then
    pass "a suite opening (library-directories ...) is accepted, shebang or not -- the audit reads a library FORM, not the prefix of the word"
else
    fail "audit wrongly condemned an ordinary (library-directories ...) suite as a library -- this pattern would fail nearly every suite in the tree" "rc=$RC: $OUT"
fi

# ---------------------------------------------------------------------------
# 6. Justfile drift. The second front end must take its suite set from make. Let
#    it glob for suites itself and the two can disagree about what "the tests"
#    are -- and the one that finds fewer just runs fewer, silently. This fixture
#    is the regression in its natural shape: the old globbing recipe, back again,
#    with make nowhere in it. Both halves of the direction must fire.
# ---------------------------------------------------------------------------
BAD_JUST="$TMPDIR/justfile-bad"
cat > "$BAD_JUST" <<'JUST'
test:
    for f in test/test-*.ss; do
      scheme --libdirs .:src --script "$f"
    done
JUST
# An empty fixture tree, so the other three directions have nothing to say and
# this case can only fail for its own reason.
C=$(mkcase justdrift)
OUT=$(WIRING_TARGETS="" WIRING_HARNESS="" JUSTFILE="$BAD_JUST" \
    sh "$AUDIT" "$C" 2>&1) && RC=0 || RC=$?
if [ "$RC" -ne 0 ] &&
    printf '%s' "$OUT" | grep -q 'globs for suites on its own account' &&
    printf '%s' "$OUT" | grep -q 'test/test-\*\.ss' &&
    printf '%s' "$OUT" | grep -q 'no longer takes its suite list from make'; then
    pass "a justfile that globs a second suite set of its own is rejected, naming the offending line"
else
    fail "audit did not reject a justfile globbing for suites on its own account" "rc=$RC"
fi

# ---------------------------------------------------------------------------
# 7. The phantom direction, against the formattings a first-line match missed.
#
# A library need not open `(library (name) ...)` all on one line. It may leave
# the `(library` keyword alone on its opening line, indent the form, or sit
# behind a `#| ... |#` block-comment header -- every one legal, and every one
# read by the Scheme reader as the very same (library ...) form. The retired
# line match saw none of them: it anchored `^\(library` on the first surviving
# line and so read a wrapped, indented or comment-preceded library as an
# ordinary suite -- waving a silent phantom pass straight into the run. Reading
# the head DATUM catches all three. Each must be rejected and named.
# ---------------------------------------------------------------------------
C=$(mkcase phantom-formattings)
cat > "$C/test-eol.ss" <<'SCM'
(library
  (fixture eol) (export) (import (chezscheme)))
SCM
cat > "$C/test-indent.ss" <<'SCM'
  (library (fixture indent) (export) (import (chezscheme)))
SCM
cat > "$C/test-blockcomment.ss" <<'SCM'
#| a header of the kind Chez itself writes |#
(library (fixture blockcomment) (export) (import (chezscheme)))
SCM
OUT=$(WIRING_TARGETS="test-eol test-indent test-blockcomment" WIRING_HARNESS="" \
    JUSTFILE="$GOOD_JUST" sh "$AUDIT" "$C" 2>&1) && RC=0 || RC=$?
if [ "$RC" -ne 0 ] &&
    printf '%s' "$OUT" | grep -q 'test-eol.ss is run as a suite but is a (library' &&
    printf '%s' "$OUT" | grep -q 'test-indent.ss is run as a suite but is a (library' &&
    printf '%s' "$OUT" | grep -q 'test-blockcomment.ss is run as a suite but is a (library'; then
    pass "a library wired in as a suite is caught however it is formatted -- keyword alone on the opening line, indented, or behind a block comment (a first-line match missed all three)"
else
    fail "audit did not reject the eol/indented/block-comment library formattings wired as suites" "rc=$RC: $OUT"
fi

# ---------------------------------------------------------------------------
# 8. A suite whose basename collides with a real Makefile target. Suites are
#    discovered by a static-pattern rule over a bare stem, so test/clean.ss
#    becomes the target `clean` -- and the build already has a `clean` recipe
#    that wins the override, runs in the suite's place, and exits 0 (a `clean`
#    recipe would wipe build artefacts mid-test into the bargain). Directions
#    1-4 see nothing amiss: the name is wired, the file exists, it is no library.
#    Only this direction, asking make which names are already targets, catches
#    it. `clean` must be rejected and named; the non-colliding suite beside it
#    must not be. The reserved-name set is read from the real Makefile, so this
#    fixture ships none of its own.
# ---------------------------------------------------------------------------
C=$(mkcase target-clash)
printf '(display "a would-be clean suite\\n")\n' > "$C/clean.ss"
printf '(display "an innocent suite\\n")\n'      > "$C/scsh-noclash.ss"
OUT=$(WIRING_TARGETS="clean scsh-noclash" WIRING_HARNESS="" JUSTFILE="$GOOD_JUST" \
    sh "$AUDIT" "$C" 2>&1) && RC=0 || RC=$?
if [ "$RC" -ne 0 ] &&
    printf '%s' "$OUT" | grep -q 'clean is wired as a suite' &&
    printf '%s' "$OUT" | grep -q 'also a real Makefile target' &&
    ! printf '%s' "$OUT" | grep -q 'scsh-noclash'; then
    pass "a suite whose basename collides with a real Makefile target (clean) is rejected and named -- and the non-colliding suite beside it is not"
else
    fail "audit did not reject a suite colliding with an explicit Makefile target" "rc=$RC: $OUT"
fi

printf "\n%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
[ "$FAIL" -eq 0 ]
