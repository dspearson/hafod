# Requires GNU Make (gmake on FreeBSD).
# Chez Scheme binary: "scheme" on most systems, "chez-scheme" on FreeBSD pkg.
SCHEME ?= $(shell command -v scheme >/dev/null 2>&1 && echo scheme || echo chez-scheme)
LIBDIRS = --libdirs src
TESTDIRS = --libdirs .:src

# Library sources for the launcher's freshness graph. Enumerated at parse time,
# so it lists the .ss SOURCES (which exist on a clean tree) rather than the
# generated .so/.wpo (which do not exist until the libraries are built). Touching
# any hand-maintained source marks bin/hafod.so out of date, so the launcher is
# never stale.
#
# The generated sources (version.ss, chez-version.ss) are NOT captured by this
# wildcard on a clean tree / after `make clean`, because they do not exist when
# the Makefile is parsed. They are added to the launcher's prerequisites by
# literal name below (GENERATED_SRC) so a version or build-Chez bump that
# rewrites only a generated source still forces a launcher rebuild.
HAFOD_SRC := $(shell find src/hafod -name '*.ss')

PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
LIBDIR ?= $(PREFIX)/lib/hafod

# Chez Scheme library directory (for native build).
# Auto-detected: resolve the scheme binary, then probe for libkernel.a
# in both standard layouts (source/Nix: $prefix/lib/csv$v/$m, FreeBSD pkg: alongside binary).
# Override with: make CHEZ_LIBDIR=/path/to/...
CHEZ_LIBDIR ?= $(shell d=$$(perl -MCwd=realpath -e 'print realpath(shift)' $$(which $(SCHEME)) | xargs dirname); \
	if [ -f "$$d/libkernel.a" ]; then echo "$$d"; \
	else \
	  v=$$(echo '(let ([v (scheme-version)]) (let f ([i (- (string-length v) 1)]) (if (char=? (string-ref v i) (integer->char 32)) (display (substring v (+ i 1) (string-length v))) (f (- i 1)))))' | $(SCHEME) -q); \
	  m=$$(echo '(display (machine-type))' | $(SCHEME) -q); \
	  echo "$$d/../lib/csv$$v/$$m"; \
	fi)

# Auto-discover Scheme test files
TEST_SCRIPTS := $(wildcard test/test-*.ss)
TEST_TARGETS := $(patsubst test/test-%.ss,test-%,$(TEST_SCRIPTS))

# Platform detection
UNAME_S := $(shell uname -s)

.PHONY: all compile compile-libs compile-wpo native standalone test clean install uninstall test-launcher test-version-guard test-load test-ffi-no-helper test-hafod-so-fresh test-install-launch test-platform-abi test-hang-timeout print-test-timeout verify-umbrella platform-constants check-platform check-c-probe test-c-probe check-portability test-check-portability print-native-libs version-source chez-version-source $(TEST_TARGETS)

all: native

# Regenerate platform-specific struct offsets and constants.
# Automatically runs on the first build or when the ABI fingerprint changes.
PLATFORM_STAMP = src/hafod/internal/.platform-stamp
PC_SS = src/hafod/internal/platform-constants.ss
UNAME_M := $(shell uname -m)
# Human-readable platform label for build messages only; the stamp now records
# the ABI fingerprint from tools/platform-fingerprint.sh, not this tag.
PLATFORM_TAG := $(UNAME_S)-$(UNAME_M)

# Per-suite kill-timeout for the test target. The justfile's test_timeout is the
# single source of truth, so this runner and `just test` cannot drift on the
# value; fall back to 60 if the justfile is absent (a stripped release tarball)
# or its line is reformatted past the parse below.
TEST_TIMEOUT := $(shell sed -n 's/^test_timeout[[:space:]]*:=[[:space:]]*"\([0-9][0-9]*\)".*/\1/p' justfile 2>/dev/null)
TEST_TIMEOUT := $(or $(TEST_TIMEOUT),60)
# Grace between the SIGTERM and the follow-up SIGKILL, matching the reference
# runner's -k 5.
TIMEOUT_KILL ?= 5

# Resolve a kill-timeout command: GNU coreutils timeout, else Homebrew's
# gtimeout; empty when neither is present. CI always has timeout (nixpkgs
# coreutils on both jobs), so the empty case is a bare host outside the dev
# shell -- it degrades to an unwrapped run rather than failing for a missing
# binary; do not drop the wrapper "because macOS".
TIMEOUT := $(shell command -v timeout 2>/dev/null || command -v gtimeout 2>/dev/null)
ifeq ($(TIMEOUT),)
TIMEOUT_PREFIX =
# Warn once, and only when a goal that actually runs under $(TIMEOUT_PREFIX) was
# requested. Gating on $(MAKECMDGOALS) keeps `make clean` / `make compile` silent
# on a host without a timeout binary, mirroring the lazy NATIVE_LIBS/$(error)
# convention that expands only when a link target is built. The wrapped goals are
# enumerated rather than matched with test-%, because the standalone .sh proofs
# (test-ffi-no-helper, test-hafod-so-fresh, test-install-launch, test-platform-abi,
# test-hang-timeout) do not use the wrapper and must not trigger this warning.
ifneq ($(filter test $(TEST_TARGETS) test-launcher test-version-guard test-load verify-umbrella,$(MAKECMDGOALS)),)
$(warning No 'timeout' or 'gtimeout' found; test suites run WITHOUT a kill-timeout.)
endif
else
TIMEOUT_PREFIX = $(TIMEOUT) -k $(TIMEOUT_KILL) $(TEST_TIMEOUT)
endif

# Report a kill-timeout (124 at the deadline, 137 after the SIGKILL grace) as a
# HANG, then re-propagate the suite's real exit code so make still fails. Keep
# this recursive ('=', never ':='), like NATIVE_LIBS, so $@ and $$? expand per
# recipe rather than once at parse time.
HANG_CHECK = rc=$$?; if [ $$rc -eq 124 ] || [ $$rc -eq 137 ]; then \
               echo "  HANG  $@ (killed after $(TEST_TIMEOUT)s)" >&2; fi; exit $$rc

platform-constants: tools/gen-platform-constants.c
	sh tools/refuse-cross-build.sh
	$(CC) $(CFLAGS) -o tools/gen-platform-constants tools/gen-platform-constants.c
	tools/gen-platform-constants > $(PC_SS).tmp || { rm -f $(PC_SS).tmp; exit 1; }; \
	if cmp -s $(PC_SS).tmp $(PC_SS); then \
	  rm -f $(PC_SS).tmp; \
	else \
	  mv $(PC_SS).tmp $(PC_SS) || { rm -f $(PC_SS).tmp; exit 1; }; \
	fi
	sh tools/platform-fingerprint.sh > $(PLATFORM_STAMP)
	@echo "platform-constants: $(PLATFORM_TAG)"

# Regenerate if the stamp is missing or no longer matches the ABI fingerprint
$(PLATFORM_STAMP):
	@$(MAKE) platform-constants

# Authoritative drift gate: prove the committed platform-constants.ss still
# matches what this toolchain and ABI would generate. Refuses a Rosetta/cross
# build first, recompiles the generator, regenerates into a temporary file and
# diffs it against the working-tree constants -- read-only, so it never touches
# the tracked file or the stamp. Exits non-zero (showing the diff) on drift. The
# fast .platform-stamp fingerprint is the heuristic; this is the slow oracle.
check-platform: tools/gen-platform-constants.c
	sh tools/refuse-cross-build.sh
	$(CC) $(CFLAGS) -o tools/gen-platform-constants tools/gen-platform-constants.c
	@tmp=$$(mktemp); \
	tools/gen-platform-constants > "$$tmp"; \
	if diff -u $(PC_SS) "$$tmp"; then \
		echo "check-platform: $(PC_SS) matches this toolchain/ABI -- no drift"; \
		rm -f "$$tmp"; \
	else \
		echo "check-platform: DRIFT -- $(PC_SS) does not match this toolchain/ABI (diff above)." >&2; \
		echo "check-platform: run 'make platform-constants' (or rebuild) to regenerate." >&2; \
		rm -f "$$tmp"; exit 1; \
	fi

# Independent cross-check: a separately-authored C probe computes each ABI fact
# directly and a comparator asserts the committed constants agree, naming any
# mismatch. Complements the drift gate, which shares the generator on both sides.
check-c-probe: tools/probe-platform-constants.c
	sh tools/refuse-cross-build.sh
	$(CC) $(CFLAGS) $(LDFLAGS) -o tools/probe-platform-constants tools/probe-platform-constants.c
	@tmp=$$(mktemp); \
	tools/probe-platform-constants > "$$tmp" || { rm -f "$$tmp"; exit 1; }; \
	if $(SCHEME) --script tools/compare-platform-constants.ss $(PC_SS) "$$tmp"; then \
		rm -f "$$tmp"; \
	else \
		rm -f "$$tmp"; exit 1; \
	fi

# Structural portability audit: the aggregate grep invariants the numeric-constant
# gates do not cover -- one platform hub, no runtime FFI-helper loader, no C
# variadic shim. Read-only; no compile. Fails naming the offender.
check-portability:
	sh tools/check-portability.sh

# Regenerate the version library from git tags (source of truth), falling back
# to the VERSION file. Runs every build; gen-version.sh only rewrites the file
# when the version actually changes, so incremental builds aren't disturbed.
VERSION_SS = src/hafod/internal/version.ss
version-source:
	@sh tools/gen-version.sh

# Record the build Chez version into a generated library. Runs every build;
# gen-chez-version.sh only rewrites the file when the build Chez changes, so
# incremental builds aren't disturbed. SCHEME is passed in because the script
# invokes that interpreter to read its (scheme-version-number) triple.
CHEZVERSION_SS = src/hafod/internal/chez-version.ss
chez-version-source:
	@SCHEME='$(SCHEME)' sh tools/gen-chez-version.sh

# Generated library sources, by literal name. The launcher depends on these
# explicitly (not via the parse-time HAFOD_SRC wildcard, which misses them on a
# clean tree where they do not yet exist) so a version bump (new git tag) or a
# build-Chez change -- each of which rewrites only a generated source -- forces
# the whole-program launcher to rebuild instead of shipping a stale inlined
# version. The gen steps are content-guarded (cmp before replace), so these
# mtimes only advance on a real change and incremental builds stay quiet.
GENERATED_SRC = $(VERSION_SS) $(CHEZVERSION_SS)

# Man page: substitute the version (from git tags) and the HEAD commit date into
# the .TH line of the template. Falls back to the VERSION file / a static year
# when git is unavailable (release tarballs, nix sources without .git).
doc/hafod.1: doc/hafod.1.in VERSION
	@v=$$(sh tools/gen-version.sh --print); \
	d=$$(git log -1 --format=%cd --date=format:'%B %Y' 2>/dev/null); \
	[ -n "$$d" ] || d="2026"; \
	sed -e "s/@VERSION@/$$v/g" -e "s/@DATE@/$$d/g" doc/hafod.1.in > doc/hafod.1; \
	echo "gen-man: hafod $$v ($$d)"

# Build the hafod libraries (src/hafod/**/*.so + *.wpo). Regenerate the
# platform constants first if the recorded ABI fingerprint has drifted, so a
# stale stamp can never leave wrong struct offsets compiled into the libraries.
# This is the body that used to live in `compile`; it is factored out so it can
# be both a normal prerequisite of `compile` and the order-only prerequisite of
# the bin/hafod.so launcher target.
compile-libs: $(PLATFORM_STAMP) version-source chez-version-source
	@fp=$$(sh tools/platform-fingerprint.sh); \
	if [ -f "$(PLATFORM_STAMP)" ] && [ "$$(cat $(PLATFORM_STAMP))" != "$$fp" ]; then \
		$(MAKE) platform-constants; \
	fi
	$(SCHEME) $(LIBDIRS) --compile-imported-libraries --script compile-all.ss

# The launcher (bin/hafod.so) is a real file target so it is never stale: it
# rebuilds whenever a library source (hand-maintained or generated) or
# bin/hafod.sps changes, and is a no-op otherwise. GENERATED_SRC lists the
# generated sources by literal name -- they are absent from the parse-time
# HAFOD_SRC wildcard on a clean tree, so without naming them a version/Chez bump
# that rewrites only a generated source would leave the launcher carrying a
# stale inlined version. The library build is an ORDER-ONLY prerequisite
# (| compile-libs): it guarantees the library .so/.wpo exist first without
# forcing a launcher rebuild on every make (a normal prerequisite on a phony
# target would, because a phony is always considered newer).
bin/hafod.so: $(HAFOD_SRC) $(GENERATED_SRC) bin/hafod.sps | compile-libs
	$(SCHEME) $(LIBDIRS) --script tools/compile-launcher.ss

# Plain `make compile` now also keeps the launcher fresh: its work is entirely
# in its prerequisites. The generated-source steps are listed here too (they are
# idempotent -- the gen scripts only rewrite on a real change) so the build's
# inputs stay visible on `compile`; compile-libs then builds the libraries and
# bin/hafod.so builds the launcher.
compile: version-source chez-version-source compile-libs bin/hafod.so

# Thin alias retained for the build front-end and CI (which invoke
# `make compile-wpo`) and for native/standalone/test-launcher, which depend on it.
compile-wpo: compile

# Native binary: links against Chez's libkernel.a for direct startup
# without going through a shell wrapper.  Requires a C compiler and
# the Chez Scheme development files (scheme.h, libkernel.a, petite.boot).
#
# Link libraries are chosen by capability rather than a hardcoded per-OS table.
# BASE_LIBS is the set every supported host needs; the two probes below ask the
# real linker whether this host's libc already provides dlopen / iconv_open and
# add -ldl / -liconv only when it does not (so a glibc >= 2.34, which folds the
# old libdl into libc, links with neither, while an older glibc or a host that
# keeps iconv in a separate library still gets the flag it needs). An
# unrecognised OS is a hard error, not a silent fallthrough.
BASE_LIBS = -lm -llz4 -lz -lncurses -lpthread

# AC_SEARCH_LIBS-style probes, run in two steps so an unrelated failure can never
# false-add a library. First link a tiny program that names the symbol with no
# extra library; if that bare link succeeds the symbol is already in libc and no
# flag is needed. Only when the bare link fails do we try a second link WITH the
# fallback library, adopting the flag solely when that second link succeeds -- so
# a broken probe (absent $(CC), bad CFLAGS, an unwritable /dev/null) adds nothing
# rather than wrongly forcing -ldl/-liconv onto a glibc host that already has the
# symbol and would then fail to link. The `char fn();` prototype needs no header
# (no '#' inside a $(shell)); $(CFLAGS)/$(LDFLAGS) put the build's -I/-L in scope
# so the probe links against the very libraries the real build uses.
NEED_LDL    = $(shell p='char dlopen();int main(){return (int)(long)dlopen;}'; \
  printf '%s' "$$p" | $(CC) $(CFLAGS) -x c - $(LDFLAGS)       -o /dev/null 2>/dev/null && exit 0; \
  printf '%s' "$$p" | $(CC) $(CFLAGS) -x c - $(LDFLAGS) -ldl  -o /dev/null 2>/dev/null && echo -ldl)
NEED_LICONV = $(shell p='char iconv_open();int main(){return (int)(long)iconv_open;}'; \
  printf '%s' "$$p" | $(CC) $(CFLAGS) -x c - $(LDFLAGS)          -o /dev/null 2>/dev/null && exit 0; \
  printf '%s' "$$p" | $(CC) $(CFLAGS) -x c - $(LDFLAGS) -liconv  -o /dev/null 2>/dev/null && echo -liconv)

# Keep NATIVE_LIBS recursive ('=', never ':='): the probes above and the
# $(error) below must expand only when a link target is actually built. A
# simply-expanded ':=' would run them at parse time and abort even `make clean`
# or `make test` on an unrecognised OS.
ifeq ($(UNAME_S),Linux)
NATIVE_LIBS = $(BASE_LIBS) $(NEED_LDL) $(NEED_LICONV)
else ifeq ($(UNAME_S),Darwin)
NATIVE_LIBS = $(BASE_LIBS) $(NEED_LDL) $(NEED_LICONV)
else ifeq ($(UNAME_S),FreeBSD)
NATIVE_LIBS = $(BASE_LIBS) $(NEED_LDL) $(NEED_LICONV)
else
NATIVE_LIBS = $(error Unsupported OS '$(UNAME_S)': native link libraries are not configured (known: Linux, Darwin, FreeBSD))
endif

# Debug hook for the linker-selection test: print the computed link libraries
# without linking anything.
print-native-libs:
	@echo $(NATIVE_LIBS)

native: compile-wpo
	$(CC) -O2 -o bin/hafod-native tools/hafod.c \
		$(CHEZ_LIBDIR)/libkernel.a -I$(CHEZ_LIBDIR) -L$(CHEZ_LIBDIR) \
		-DLIBDIR=\"$(LIBDIR)\" \
		$(LDFLAGS) $(NATIVE_LIBS)

# Self-contained binary with boot file, libraries, and program embedded.
# No external files needed at runtime (only system shared libs).
# Uses make-boot-file to bake all hafod libraries into the Chez heap,
# then compiles the launcher program separately.
standalone: compile-wpo
	$(SCHEME) $(LIBDIRS) --script tools/build-standalone.ss

# Static pattern rule: test-X runs test/test-X.ss for all discovered test files.
# We use a static pattern rule (not an implicit pattern rule) because GNU Make
# skips implicit rule search for targets declared .PHONY. Static pattern rules
# are explicit rules and work correctly with .PHONY.
#
# stdin is redirected from /dev/null so a test never blocks on the terminal.
# interactive-repl picks the line editor vs. bare read via (tty? 0), so
# test-interactive would otherwise hang waiting for keystrokes when `make test`
# inherits a real terminal (e.g. in an interactive shell). Matches `just test`.
$(TEST_TARGETS): test-%: compile
	@$(TIMEOUT_PREFIX) $(SCHEME) $(TESTDIRS) --script test/test-$*.ss </dev/null \
	  || { $(HANG_CHECK); }

# Special case: test-launcher uses bash, not Scheme.
# This explicit rule overrides the static pattern rule above.
# Depends on compile-wpo (not just compile): the launcher exercises the
# bin/hafod wrapper, which loads bin/hafod.so -- built by compile-wpo, NOT by
# compile (that only builds src/*.so). Without this, the test runs against a
# stale/version-mismatched bin/hafod.so and fails with an incompatible
# fasl-object / "cannot compile foreign-procedure" error.
test-launcher: compile-wpo
	@$(TIMEOUT_PREFIX) sh test/test-launcher.sh </dev/null || { $(HANG_CHECK); }

# Startup version guard: a Chez mismatch (or a compiler-less interpreter on the
# source path) must yield a friendly remediation and a non-zero exit, not a raw
# fasl-object crash or a silent exit 0. Depends on compile-wpo so the happy-path
# case has a matched bin/hafod.so. The test stashes and restores chez-version.ss
# and bin/hafod.so via an EXIT trap (timestamps preserved), so it does not
# perturb the tree -- which is why it is safe inside the `test:` aggregate.
test-version-guard: compile-wpo
	@$(TIMEOUT_PREFIX) sh test/test-version-guard.sh </dev/null || { $(HANG_CHECK); }

# Load self-test: assert (hafod) and (hafod posix) import cleanly under BOTH
# petite (no compiler -- it loads the built fasls) and full scheme, via the
# source/fasl path (--libdirs src), not the bundled bin/hafod.so image. Depends
# on compile (which builds src/hafod/*.so via compile-libs), NOT compile-wpo:
# petite cannot compile, so it needs those fasls first, and a broken
# inter-library load order or a missing dependency then makes the import fail.
# SCHEME is passed through so the script derives a matching petite and honours a
# 'make test-load SCHEME=...' override.
test-load: compile
	@SCHEME='$(SCHEME)' $(TIMEOUT_PREFIX) sh test/test-load.sh </dev/null || { $(HANG_CHECK); }

# Standalone proof that the variadic syscalls work with no helper library
# present: open/fcntl/ioctl call libc directly via Chez's native variadic
# convention. The harness stashes the helper shared object, runs the full
# suite with it absent, and restores it on exit. Deliberately NOT part of the
# `test:` aggregate -- its recipe runs `make test` internally, so folding it
# into `test:` would recurse a fresh top-level `make test` indefinitely.
test-ffi-no-helper:
	sh test/test-ffi-no-helper.sh </dev/null

# Standalone proof that bin/hafod.so is never stale: build, touch a source,
# rebuild, and assert the launcher's mtime advanced (then that a no-change
# rebuild is a no-op). The harness runs `make compile` itself and touches a
# real source mid-run, restoring its timestamp on exit -- so, like
# test-ffi-no-helper, it is deliberately NOT part of the `test:` aggregate.
test-hafod-so-fresh:
	sh test/test-hafod-so-fresh.sh </dev/null

# Standalone proof that an INSTALLED hafod launches cleanly: install into a
# throwaway prefix, then run the installed wrapper on the matched Chez (must
# start with no compilation-instance / fasl crash) and under a recorded-version
# mismatch (must give the friendly remediation and a non-zero exit). The recipe
# runs a full `make install` into a temp tree it cleans up itself, so -- like
# test-ffi-no-helper and test-hafod-so-fresh -- it is deliberately NOT part of
# the `test:` aggregate. Depends on compile-wpo so the library .so/.wpo and
# bin/hafod.so the install copies already exist.
test-install-launch: compile-wpo
	sh test/test-install-launch.sh </dev/null

# Standalone proof of the platform fingerprint, the cross-build refusal, and the
# check-platform drift gate. The drift case runs `make` internally and mutates
# the tracked platform-constants.ss (restoring it, mtime preserved, on an exit
# trap), so -- like test-ffi-no-helper, test-hafod-so-fresh and
# test-install-launch -- it is deliberately NOT part of the `test:` aggregate.
test-platform-abi:
	sh test/test-platform-abi.sh </dev/null

# Standalone proof that the independent C-probe cross-check has teeth: it passes
# on a matched tree and fails, naming the constant, when one value is corrupted
# in a TEMPORARY copy (the tracked constants file is never written). The harness
# runs `make` internally, so -- like test-platform-abi and test-ffi-no-helper --
# it is deliberately NOT part of the `test:` aggregate.
test-c-probe:
	sh test/test-c-probe.sh </dev/null

# Standalone proof that the structural portability audit has teeth. Shells the
# audit against a private fixture tree, so it is kept out of the test: aggregate.
test-check-portability:
	sh test/test-check-portability.sh </dev/null

# Standalone proof of the per-suite kill-timeout: a wedged suite is killed and
# reported as a HANG, a fast FAIL or PASS exit passes through unchanged, and a
# host without timeout(1) degrades to an unwrapped run plus one warning. Like the
# four targets above it shells `make` internally, so it is deliberately NOT part
# of the `test:` aggregate.
test-hang-timeout:
	sh test/test-hang-timeout.sh </dev/null

# Echo the per-suite timeout (sourced from the justfile) so the shared value is
# checkable without parsing this Makefile.
print-test-timeout:
	@echo $(TEST_TIMEOUT)

# Umbrella verification: confirm all 791+ symbols are accessible via (import (hafod))
verify-umbrella: compile
	@$(TIMEOUT_PREFIX) $(SCHEME) $(LIBDIRS) --script tools/verify-umbrella.ss </dev/null \
	  || { $(HANG_CHECK); }

test: compile $(TEST_TARGETS) test-launcher test-version-guard test-load verify-umbrella
	@printf 'test platform: '; uname -srm

clean:
	find src -name '*.so' -delete 2>/dev/null || true
	find src -name '*.wpo' -delete 2>/dev/null || true
	find test -name '*.so' -delete 2>/dev/null || true
	rm -f tools/wpo-boot.so tools/wpo-boot.wpo tools/hafod.boot
	rm -f tools/petite-vfasl.boot tools/hafod-vfasl.boot
	rm -f tools/boot_data.c tools/hafod_boot_data.c tools/prog_data.c
	rm -f tools/hafod-lib-merged.wpo
	rm -rf lib/
	rm -f bin/hafod.so bin/hafod.wpo bin/hafod-native bin/hafod-standalone
	rm -f tools/gen-platform-constants
	rm -f tools/probe-platform-constants
	rm -f src/hafod/internal/.platform-stamp
	rm -f $(VERSION_SS)
	rm -f $(CHEZVERSION_SS)
	rm -f doc/hafod.1

install: doc/hafod.1
	install -d $(DESTDIR)$(BINDIR)
	rm -rf $(DESTDIR)$(LIBDIR)/src
	install -d $(DESTDIR)$(LIBDIR)/src
	cp -r src/hafod src/hafod.so $(DESTDIR)$(LIBDIR)/src/
	find $(DESTDIR)$(LIBDIR)/src -name '*.ss' -delete 2>/dev/null || true
	find $(DESTDIR)$(LIBDIR)/src -name '*.wpo' -delete 2>/dev/null || true
	# Reinstate the recorded build-Chez version: it is a generated .ss, so the
	# purge above deletes it, but the launcher's pre-flight version guard reads
	# it to detect a runtime Chez that differs from the one the libraries were
	# built with. Without it $expected is empty and the guard silently never
	# fires -- re-exposing the raw "incompatible fasl-object" crash on every
	# installed copy. Restored after the purge so the installed wrapper behaves
	# exactly like the dev tree.
	install -d $(DESTDIR)$(LIBDIR)/src/hafod/internal
	install -m 644 $(CHEZVERSION_SS) $(DESTDIR)$(LIBDIR)/src/hafod/internal/chez-version.ss
	# Stamp the reinstated source no newer than its already-installed compiled
	# object. The launcher runs with `--libdirs $HAFOD_ROOT/src`, so at runtime
	# Chez compares chez-version.ss against chez-version.so and recompiles the
	# source if it is the newer of the two. A fresh recompile yields a DIFFERENT
	# compilation instance of (hafod internal chez-version) than the one baked
	# into the whole-program hafod.so, so the matched-Chez launch then aborts
	# with "different compilation instance". `install` always lands the .ss with
	# a current mtime, so without this touch every installed copy would crash on
	# startup. Keeping the source older lets Chez load the WPO-baked instance
	# (no recompile) while the guard can still read the recorded triple from the
	# .ss text. If the compiled object is absent the install is unusable -- the
	# guard's source would always be recompiled into a mismatching instance -- so
	# fail loudly rather than ship a launcher that crashes on first run.
	@so="$(DESTDIR)$(LIBDIR)/src/hafod/internal/chez-version.so"; \
	ss="$(DESTDIR)$(LIBDIR)/src/hafod/internal/chez-version.ss"; \
	if [ ! -f "$$so" ]; then \
		echo "install: error: $$so is missing; the launcher would recompile chez-version.ss into a mismatching compilation instance and crash on startup. Run 'make compile-wpo' before 'make install'." >&2; \
		exit 1; \
	fi; \
	touch -r "$$so" "$$ss"
	install -m 644 bin/hafod.sps $(DESTDIR)$(LIBDIR)/hafod.sps
	install -m 644 bin/hafod.so $(DESTDIR)$(LIBDIR)/hafod.so
	# Copy boot files (not symlink) so the install is self-contained: the
	# native binary's libkernel.a is version-locked to these boot files at
	# build time, and copying avoids depending on the build-time Chez prefix
	# (e.g. a transient /nix/store path) at runtime. rm -f first in case a
	# previous install left a read-only symlink into a read-only store.
	rm -f $(DESTDIR)$(LIBDIR)/petite.boot $(DESTDIR)$(LIBDIR)/scheme.boot
	install -m 644 $(CHEZ_LIBDIR)/petite.boot $(DESTDIR)$(LIBDIR)/petite.boot
	install -m 644 $(CHEZ_LIBDIR)/scheme.boot $(DESTDIR)$(LIBDIR)/scheme.boot
	@if [ -f bin/hafod-native ]; then \
		install -m 755 bin/hafod-native $(DESTDIR)$(BINDIR)/hafod; \
	elif [ -f bin/hafod-standalone ]; then \
		install -m 755 bin/hafod-standalone $(DESTDIR)$(BINDIR)/hafod; \
	else \
		sed -e 's|HAFOD_ROOT="$$(cd "$$BINDIR/.." \&\& pwd)"|HAFOD_ROOT="$(LIBDIR)"|' \
		    -e 's|"$$BINDIR/hafod.so"|"$(LIBDIR)/hafod.so"|' \
		    -e 's|"$$BINDIR/hafod.sps"|"$(LIBDIR)/hafod.sps"|' \
			bin/hafod > $(DESTDIR)$(BINDIR)/hafod; \
		chmod +x $(DESTDIR)$(BINDIR)/hafod; \
	fi
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 644 doc/hafod.1 $(DESTDIR)$(PREFIX)/share/man/man1/hafod.1
	@if command -v scsh >/dev/null 2>&1; then \
		echo "Note: scsh already installed at $$(command -v scsh), skipping symlink"; \
	else \
		ln -sf hafod $(DESTDIR)$(BINDIR)/scsh; \
		echo "Created scsh -> hafod symlink in $(DESTDIR)$(BINDIR)"; \
	fi

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/hafod
	@if [ -L $(DESTDIR)$(BINDIR)/scsh ] && [ "$$(readlink $(DESTDIR)$(BINDIR)/scsh)" = "hafod" ]; then \
		rm -f $(DESTDIR)$(BINDIR)/scsh; \
	fi
	rm -f $(DESTDIR)$(PREFIX)/share/man/man1/hafod.1
	rm -rf $(DESTDIR)$(LIBDIR)
