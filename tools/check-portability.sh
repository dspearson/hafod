#!/bin/sh
# check-portability.sh -- structural portability audit.
#
# Confirms the aggregate structural invariants that the numeric-constant gates
# (check-platform, check-c-probe) do NOT cover: that every platform decision
# still funnels through the one internal platform hub, and that the removed
# runtime FFI-helper/variadic-shim machinery has not crept back in. These are
# grep invariants over the tracked source, so a regression fails the build near
# its cause rather than on a foreign runner.
#
# Scans SRC_ROOT (default src/hafod) so a teeth test can point it at a fixture.
# Exits 0 when every invariant holds, 1 (naming the offender) otherwise.

set -eu

SRC_ROOT="${1:-src/hafod}"
status=0

# Real grep, bypassing any interactive shell alias (matches the project's gate
# convention). -R recurses; --include limits to Scheme source.
scan() {
	grep -RnE --include='*.ss' "$1" "$SRC_ROOT" 2>/dev/null || true
}

fail() {
	printf 'check-portability: %s\n' "$1" >&2
	status=1
}

# 1. Axis-B single dispatch: (machine-type) is read in exactly ONE place -- the
#    internal platform hub -- plus the (chezscheme) re-export in internal/base.ss
#    that the hub itself reads. Any other reader has bypassed the hub and would
#    reintroduce a scattered, silently-Linux-defaulting platform decision.
mt=$(scan 'machine-type' | grep -vE 'internal/platform\.ss|internal/base\.ss' || true)
if [ -n "$mt" ]; then
	fail "(machine-type) is read outside the platform hub -- route it through (hafod internal platform):"
	printf '%s\n' "$mt" >&2
fi

# 2. The runtime FFI-helper loader was removed (it was a startup .so-load failure
#    mode). It must not return: no load-ffi-helpers / ffi-helper-extension.
helpers=$(scan 'load-ffi-helpers|ffi-helper-extension' || true)
if [ -n "$helpers" ]; then
	fail "the removed runtime FFI-helper loader has reappeared:"
	printf '%s\n' "$helpers" >&2
fi

# 3. The C variadic shims were removed in favour of the native (__varargs_after n)
#    convention. The shim symbol names must not return anywhere in the tree. The
#    audit script itself names them (as grep patterns), so exclude it from its
#    own scan.
shims=$(grep -RnE 'hafod_open3|hafod_fcntl_void|hafod-ffi-helpers' src tools 2>/dev/null | grep -v 'tools/check-portability\.sh' || true)
if [ -n "$shims" ]; then
	fail "a removed C variadic shim symbol has reappeared:"
	printf '%s\n' "$shims" >&2
fi

if [ "$status" -eq 0 ]; then
	printf 'check-portability: structural portability invariants hold (single platform hub; no runtime FFI-helper loader; no C variadic shim)\n'
fi
exit "$status"
