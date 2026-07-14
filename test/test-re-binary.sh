#!/bin/sh
# test-re-binary.sh -- Regex smoke test through the shipped merged binary.
#
# The regex --script suite runs in the process's default "C" collation, so it
# cannot observe a fault that appears only once the shipped launcher adopts the
# user's UTF-8 locale at start-up. This guard drives the real bin/hafod launcher
# (the -c / --program path) and asserts the character-range results a UTF-8
# collation must NOT change, catching any --script/--program divergence.
#
# The motivating case: (rx ascii) compiles to the byte range [\x01-\x7f]. Read
# through libc regexec under a UTF-8 collation, that endpoint pair straddles
# where letters sort, so it became a collation range that dropped plain ASCII
# letters -- (rx ascii) matched "a" under --script (C collation) but not through
# the launcher in a UTF-8 locale. The engine now pins LC_COLLATE to "C", so a
# character range compares by byte value whatever the user's collation is.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

HAFOD="$(cd "$(dirname "$0")/.." && pwd)/bin/hafod"
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

# assert_search DESC EXPECTED EXPR [ENV=VAL ...]
# Evaluate (display EXPR) through the shipped launcher under any given locale
# environment and compare stdout. The launcher itself does the automation; the
# caller never runs a raw Scheme command.
assert_search() {
    desc="$1"; expected="$2"; expr="$3"; shift 3
    actual=$(env "$@" "$HAFOD" -c "(display $expr)" 2>&1 | tail -1)
    if [ "$actual" = "$expected" ]; then
        pass "$desc"
    else
        fail "$desc" "$expected" "$actual"
    fi
}

section() { printf "\n=== %s ===\n" "$1"; }

# ======================================================================
section "ascii char-set through the merged binary (divergence guard)"
# ======================================================================

# THE regression. Under the inherited locale (a UTF-8 locale on a typical desktop
# and CI leg) this returned #f before the fix while --script said #t.
assert_search "ascii matches \"a\" under the inherited locale" \
    "#t" '(regexp-search? (rx ascii) "a")'

# Deterministic leg: force a UTF-8 locale whose collation actually reorders
# (C.UTF-8 sorts by codepoint and would NOT reproduce the fault), so the guard
# keeps its teeth even where the inherited locale is C. Skipped -- never failed
# -- when no such locale is installed.
utf8_locale=""
for cand in en_GB.UTF-8 en_US.UTF-8 en_GB.utf8 en_US.utf8; do
    if locale -a 2>/dev/null | grep -Fxq "$cand"; then
        utf8_locale="$cand"
        break
    fi
done
if [ -n "$utf8_locale" ]; then
    assert_search "ascii matches \"a\" under $utf8_locale collation" \
        "#t" '(regexp-search? (rx ascii) "a")' "LC_ALL=$utf8_locale"
else
    printf "  SKIP: no reordering UTF-8 collation locale installed to force the fault\n"
fi

# The byte-collation baseline must stay #t too.
assert_search "ascii matches \"a\" under LC_ALL=C" \
    "#t" '(regexp-search? (rx ascii) "a")' "LC_ALL=C"

# ======================================================================
section "ordinary ranges and classes are unaffected (behaviour preserved)"
# ======================================================================

# A literal collation range still bounds correctly (m is inside a..z, a digit is
# not), a common digit range matches, the LC_CTYPE-driven named classes and
# case-folding (REG_ICASE) still work, and the char-set edge cases hold.
assert_search "literal range [a-z] matches m"          "#t" '(regexp-search? "[a-z]" "m")'
assert_search "literal range [a-z] rejects a digit"    "#f" '(regexp-search? "[a-z]" "5")'
assert_search "literal range [0-9] matches 5"          "#t" '(regexp-search? "[0-9]" "5")'
assert_search "named class digit matches 123"          "#t" '(regexp-search? (rx (+ digit)) "123")'
assert_search "named class alpha matches abc"          "#t" '(regexp-search? (rx (+ alpha)) "abc")'
assert_search "case-insensitive match folds ABC/abc"   "#t" '(regexp-search? (rx (w/nocase "ABC")) "abc")'
assert_search "caret-dash char-set matches a dash"     "#t" '(regexp-search? (rx ("^-")) "-")'
assert_search "empty-alternation star matches empty"   "#t" '(regexp-search? (rx (* (or))) "")'

# ======================================================================
section "line anchors match at an interior boundary (through the merged binary)"
# ======================================================================

# bol/eol are LINE anchors: over a subject with an embedded newline they must
# match at the interior line boundary, not only at the string ends. A -c string
# passes backslash-n through the shell and hafod's reader turns it into a real
# newline, so the subject is genuinely multi-line. Read through the shipped
# launcher this catches a compiler that degrades bol/eol to the string anchors
# bos/eos: that degradation prints #f, since "abc\ndef" neither starts nor ends
# with "def". This is the anchor defect seen through the shipped path -- #f today,
# #t once bol/eol are line-aware -- across the same three locales the ascii guard
# uses.
assert_search "bol matches an interior line boundary under the inherited locale" \
    "#t" '(regexp-search? (rx (: bol "def")) "abc\ndef")'
if [ -n "$utf8_locale" ]; then
    assert_search "bol matches an interior line boundary under $utf8_locale collation" \
        "#t" '(regexp-search? (rx (: bol "def")) "abc\ndef")' "LC_ALL=$utf8_locale"
else
    printf "  SKIP: no reordering UTF-8 collation locale installed for the anchor leg\n"
fi
assert_search "bol matches an interior line boundary under LC_ALL=C" \
    "#t" '(regexp-search? (rx (: bol "def")) "abc\ndef")' "LC_ALL=C"

# A line-aware "any" (".") must keep matching a whole UTF-8 line, so the rewrite
# that keeps "." matching a newline stays multibyte-correct: a byte-range rewrite
# of "any" would drop the two-byte "é" under a UTF-8 LC_CTYPE and diverge here,
# while still passing the "C"-collation in-process suite. This is a differential
# guard -- #t both before and after the anchor fix -- not a match that flips once
# bol/eol are line-aware.
assert_search "line-aware any spans a utf-8 line under the inherited locale" \
    "#t" '(regexp-search? (rx (: bol (+ any))) "x\ncafé")'
if [ -n "$utf8_locale" ]; then
    assert_search "line-aware any spans a utf-8 line under $utf8_locale collation" \
        "#t" '(regexp-search? (rx (: bol (+ any))) "x\ncafé")' "LC_ALL=$utf8_locale"
else
    printf "  SKIP: no reordering UTF-8 collation locale installed for the multibyte anchor leg\n"
fi

# ======================================================================
# Summary
# ======================================================================

printf "\n=== Summary ===\n"
printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
