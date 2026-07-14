#!/bin/sh
# test-check-resource-guards.sh -- Standalone proof that the resource-guard audit
# has teeth AND is not a blunt instrument: it passes on the tracked tree, rejects
# (and names) a bare foreign-memory free inside a re-raising guard handler, and
# accepts the two safe shapes it must never flag -- a foreign-free in a
# dynamic-wind after-thunk and a latched release the guard handler merely calls --
# plus a bare free carrying the co-located allow marker.
#
# `make check-resource-guards` (tools/check-resource-guards.sh) asserts that no
# (guard ...) handler in internal/posix-*.ss frees foreign memory bare -- the
# inline-free-then-raise-into-the-enclosing-handler shape that frees an opaque
# heap block twice and aborts the allocator. A gate that cannot be shown to bite
# on a real violation, and to stay silent on the safe shapes, gives false
# confidence; this harness is the discriminating evidence for both directions.
#
# The fixtures are only ever written into a private temporary tree handed to the
# audit via its overridable scan-root argument; the tracked sources are never the
# target of a redirect. Like the sibling standalone proofs it is deliberately NOT
# folded into the `test:` aggregate.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
AUDIT="$ROOT/tools/check-resource-guards.sh"
TMPDIR="${TMPDIR:-/tmp}/hafod-check-resource-guards-test-$$"
TREE="$TMPDIR/tree"
PASS=0
FAIL=0
TOTAL=0

mkdir -p "$TREE"
cleanup() { rm -rf "$TMPDIR"; }
trap cleanup EXIT INT TERM HUP

pass() { PASS=$((PASS + 1)); TOTAL=$((TOTAL + 1)); printf "  PASS: %s\n" "$1"; }
fail() {
    FAIL=$((FAIL + 1)); TOTAL=$((TOTAL + 1)); printf "  FAIL: %s\n" "$1"
    [ -n "$2" ] && printf "    detail: %s\n" "$2"
}

# 1. The tracked tree passes cleanly (exit 0).
if (cd "$ROOT" && sh "$AUDIT" >/dev/null 2>&1); then
    pass "tracked tree satisfies the resource-guard invariant"
else
    fail "tracked tree unexpectedly failed the audit"
fi

# 2. A bare foreign-memory free inside a re-raising guard handler is rejected and
#    the offending file is named.
cat > "$TREE/posix-bad.ss" <<'EOF'
(library (posix-bad) (export f) (import (chezscheme))
  ;; The banned shape: an inline free in the guard handler, which then re-raises
  ;; into an enclosing handler that would free the same block again.
  (define (f)
    (let ([p (foreign-alloc 8)])
      (guard (e [#t
                 (c-spawn-fa-destroy p)
                 (foreign-free p)
                 (raise e)])
        (do-something p)))))
EOF
OUT=$(sh "$AUDIT" "$TREE" 2>&1) && RC=0 || RC=$?
if [ "$RC" -ne 0 ] && printf '%s' "$OUT" | grep -q 'posix-bad.ss'; then
    pass "a bare foreign-memory free in a re-raising guard handler is rejected and named"
else
    fail "audit did not reject a bare free in a guard handler" "rc=$RC"
fi
rm -f "$TREE/posix-bad.ss"

# 3. The two safe shapes are accepted (no false positive): a foreign-free in a
#    dynamic-wind after-thunk (single owner), and a latched release the guard
#    handler merely calls -- never a bare free.
cat > "$TREE/posix-safe.ss" <<'EOF'
(library (posix-safe) (export g h) (import (chezscheme))
  ;; Safe shape 1: the free lives in a dynamic-wind after-thunk -- one owner.
  (define (g)
    (let ([buf (foreign-alloc 16)])
      (dynamic-wind
        (lambda () #f)
        (lambda () (use buf))
        (lambda () (c-globfree buf) (foreign-free buf)))))
  ;; Safe shape 2: an idempotent latched release the handler calls. The frees
  ;; live in the release! closure, which sits BEFORE the guard; the handler only
  ;; calls (release!). This is the shipped reference shape.
  (define (h)
    (let ([p (foreign-alloc 8)] [released? #f])
      (define (release!)
        (unless released?
          (set! released? #t)
          (free-c-argv p 1)
          (foreign-free p)))
      (guard (e [#t (release!) (raise e)])
        (work p)
        (release!)))))
EOF
if sh "$AUDIT" "$TREE" >/dev/null 2>&1; then
    pass "a dynamic-wind after-thunk free plus a latched handler release are accepted"
else
    fail "audit false-positived on the known-safe shapes"
fi
rm -f "$TREE/posix-safe.ss"

# 4. A bare free in a guard handler carrying the co-located allow marker is
#    accepted -- the escape hatch for a future verified-safe site.
cat > "$TREE/posix-allow.ss" <<'EOF'
(library (posix-allow) (export k) (import (chezscheme))
  (define (k)
    (let ([p (foreign-alloc 8)])
      (guard (e [#t
                 (foreign-free p) ;; resource-guard-allow: verified single owner
                 (raise e)])
        (work p)))))
EOF
if sh "$AUDIT" "$TREE" >/dev/null 2>&1; then
    pass "a bare free carrying the resource-guard-allow marker is accepted"
else
    fail "audit rejected an allow-marked free"
fi
rm -f "$TREE/posix-allow.ss"

# 5. A square-bracket guard operator -- [guard ...], which Chez treats as
#    identical to (guard ...) -- whose handler frees foreign memory bare and then
#    re-raises must be flagged exactly as the parenthesised form is.
cat > "$TREE/posix-bracket.ss" <<'EOF'
(library (posix-bracket) (export f) (import (chezscheme))
  (define (f)
    (let ([p (foreign-alloc 8)])
      [guard (e [#t
                 (foreign-free p)
                 (raise e)])
        (do-something p)])))
EOF
OUT=$(sh "$AUDIT" "$TREE" 2>&1) && RC=0 || RC=$?
if [ "$RC" -ne 0 ] && printf '%s' "$OUT" | grep -q 'posix-bracket.ss'; then
    pass "a bare free in a re-raising square-bracket guard handler is rejected and named"
else
    fail "audit did not reject a bare free in a square-bracket guard handler" "rc=$RC"
fi
rm -f "$TREE/posix-bracket.ss"

# 6. The allow marker is honoured only in a line comment. When the marker text
#    sits inside a STRING literal on the same line as a bare free, it must NOT
#    exempt that free -- the bare free is still flagged.
cat > "$TREE/posix-string-marker.ss" <<'EOF'
(library (posix-string-marker) (export m) (import (chezscheme))
  (define (m)
    (let ([p (foreign-alloc 8)])
      (guard (e [#t
                 (log-line "resource-guard-allow") (foreign-free p)
                 (raise e)])
        (work p)))))
EOF
OUT=$(sh "$AUDIT" "$TREE" 2>&1) && RC=0 || RC=$?
if [ "$RC" -ne 0 ] && printf '%s' "$OUT" | grep -q 'posix-string-marker.ss'; then
    pass "a marker inside a string does not exempt a bare free on the same line"
else
    fail "audit was fooled by a string-embedded allow marker" "rc=$RC"
fi
rm -f "$TREE/posix-string-marker.ss"

printf "\n%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
[ "$FAIL" -eq 0 ]
