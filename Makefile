# Requires GNU Make (gmake on FreeBSD).
# Chez Scheme binary: "scheme" on most systems, "chez-scheme" on FreeBSD pkg.
SCHEME ?= $(shell command -v scheme >/dev/null 2>&1 && echo scheme || echo chez-scheme)
LIBDIRS = --libdirs src
TESTDIRS = --libdirs .:src

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

.PHONY: all compile compile-wpo native standalone test clean install uninstall test-launcher verify-umbrella platform-constants ffi-helpers $(TEST_TARGETS)

all: native

# Regenerate platform-specific struct offsets and constants.
# Automatically runs on first build or when architecture changes.
PLATFORM_STAMP = src/hafod/internal/.platform-stamp
UNAME_M := $(shell uname -m)
PLATFORM_TAG := $(UNAME_S)-$(UNAME_M)

platform-constants: tools/gen-platform-constants.c
	$(CC) -o tools/gen-platform-constants tools/gen-platform-constants.c
	tools/gen-platform-constants > src/hafod/internal/platform-constants.ss
	echo "$(PLATFORM_TAG)" > $(PLATFORM_STAMP)

# Regenerate if stamp is missing or doesn't match current platform
$(PLATFORM_STAMP):
	@$(MAKE) platform-constants

FFI_HELPERS_SRC = tools/hafod-ffi-helpers.c
ifeq ($(UNAME_S),Darwin)
FFI_HELPERS_OUT = src/hafod-ffi-helpers.dylib
FFI_HELPERS_FLAGS = -dynamiclib
else
FFI_HELPERS_OUT = src/hafod-ffi-helpers.so
FFI_HELPERS_FLAGS = -shared -fPIC
endif

ffi-helpers: $(FFI_HELPERS_OUT)
$(FFI_HELPERS_OUT): $(FFI_HELPERS_SRC)
	$(CC) $(CFLAGS) $(FFI_HELPERS_FLAGS) -o $@ $< $(LDFLAGS)

compile: ffi-helpers $(PLATFORM_STAMP)
	@if [ -f "$(PLATFORM_STAMP)" ] && [ "$$(cat $(PLATFORM_STAMP))" != "$(PLATFORM_TAG)" ]; then \
		$(MAKE) platform-constants; \
	fi
	$(SCHEME) $(LIBDIRS) --compile-imported-libraries --script compile-all.ss

compile-wpo: compile
	$(SCHEME) $(LIBDIRS) --script tools/compile-launcher.ss

# Native binary: links against Chez's libkernel.a for direct startup
# without going through a shell wrapper.  Requires a C compiler and
# the Chez Scheme development files (scheme.h, libkernel.a, petite.boot).
# Platform-specific linker flags:
#   Linux:  -ldl -lpthread
#   macOS:  -liconv (iconv is separate from libSystem)
#   Both:   -lm -llz4 -lz -lncurses
ifeq ($(UNAME_S),Darwin)
NATIVE_LIBS = -lm -llz4 -lz -lncurses -lpthread -liconv
else ifeq ($(UNAME_S),FreeBSD)
NATIVE_LIBS = -lm -llz4 -lz -lncurses -lpthread
else
NATIVE_LIBS = -ldl -lm -llz4 -lz -lncurses -lpthread
endif

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
$(TEST_TARGETS): test-%: compile
	$(SCHEME) $(TESTDIRS) --script test/test-$*.ss

# Special case: test-launcher uses bash, not Scheme.
# This explicit rule overrides the static pattern rule above.
test-launcher: compile
	sh test/test-launcher.sh

# Umbrella verification: confirm all 791+ symbols are accessible via (import (hafod))
verify-umbrella: compile
	$(SCHEME) $(LIBDIRS) --script tools/verify-umbrella.ss

test: compile $(TEST_TARGETS) test-launcher verify-umbrella

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
	rm -f src/hafod/internal/.platform-stamp
	rm -f src/hafod-ffi-helpers.so src/hafod-ffi-helpers.dylib

install:
	install -d $(DESTDIR)$(BINDIR)
	rm -rf $(DESTDIR)$(LIBDIR)/src
	install -d $(DESTDIR)$(LIBDIR)/src
	cp -r src/hafod src/hafod.so $(DESTDIR)$(LIBDIR)/src/
	find $(DESTDIR)$(LIBDIR)/src -name '*.ss' -delete 2>/dev/null || true
	find $(DESTDIR)$(LIBDIR)/src -name '*.wpo' -delete 2>/dev/null || true
	@for f in src/hafod-ffi-helpers.so src/hafod-ffi-helpers.dylib; do \
		[ -f "$$f" ] && install -m 644 "$$f" $(DESTDIR)$(LIBDIR)/src/ || true; \
	done
	install -m 644 bin/hafod.sps $(DESTDIR)$(LIBDIR)/hafod.sps
	install -m 644 bin/hafod.so $(DESTDIR)$(LIBDIR)/hafod.so
	ln -sf $(CHEZ_LIBDIR)/petite.boot $(DESTDIR)$(LIBDIR)/petite.boot
	ln -sf $(CHEZ_LIBDIR)/scheme.boot $(DESTDIR)$(LIBDIR)/scheme.boot
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
