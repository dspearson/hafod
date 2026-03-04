SCHEME ?= scheme
LIBDIRS = --libdirs src
TESTDIRS = --libdirs .:src

PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
LIBDIR ?= $(PREFIX)/lib/hafod

# Auto-discover Scheme test files
TEST_SCRIPTS := $(wildcard test/test-*.ss)
TEST_TARGETS := $(patsubst test/test-%.ss,test-%,$(TEST_SCRIPTS))

.PHONY: all compile compile-wpo test clean install uninstall test-launcher verify-umbrella $(TEST_TARGETS)

all: compile-wpo

compile:
	@rm -f src/hafod.so src/hafod.wpo
	$(SCHEME) $(LIBDIRS) --compile-imported-libraries --script compile-all.ss

compile-wpo: compile
	$(SCHEME) $(LIBDIRS) --script tools/compile-wpo.ss

# Static pattern rule: test-X runs test/test-X.ss for all discovered test files.
# We use a static pattern rule (not an implicit pattern rule) because GNU Make
# skips implicit rule search for targets declared .PHONY. Static pattern rules
# are explicit rules and work correctly with .PHONY.
$(TEST_TARGETS): test-%: compile
	$(SCHEME) $(TESTDIRS) --script test/test-$*.ss

# Special case: test-launcher uses bash, not Scheme.
# This explicit rule overrides the static pattern rule above.
test-launcher: compile
	bash test/test-launcher.sh

# Umbrella verification: confirm all 791+ symbols are accessible via (import (hafod))
verify-umbrella: compile
	$(SCHEME) $(LIBDIRS) --script tools/verify-umbrella.ss

test: compile $(TEST_TARGETS) test-launcher verify-umbrella

clean:
	find src -name '*.so' -delete 2>/dev/null || true
	find src -name '*.wpo' -delete 2>/dev/null || true
	find test -name '*.so' -delete 2>/dev/null || true
	rm -f tools/wpo-boot.so tools/wpo-boot.wpo

install: compile-wpo
	install -d $(DESTDIR)$(BINDIR)
	install -d $(DESTDIR)$(LIBDIR)/src
	cp -r src/hafod src/hafod.ss src/hafod.so $(DESTDIR)$(LIBDIR)/src/
	sed 's|HAFOD_ROOT="$$(cd "$$BINDIR/.." \&\& pwd)"|HAFOD_ROOT="$(LIBDIR)"|' \
		bin/hafod > $(DESTDIR)$(BINDIR)/hafod
	chmod +x $(DESTDIR)$(BINDIR)/hafod
	install -m 644 bin/hafod.sps $(DESTDIR)$(BINDIR)/hafod.sps
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 644 doc/hafod.1 $(DESTDIR)$(PREFIX)/share/man/man1/hafod.1
	@if command -v scsh >/dev/null 2>&1; then \
		echo "Note: scsh already installed at $$(command -v scsh), skipping symlink"; \
	else \
		ln -sf hafod $(DESTDIR)$(BINDIR)/scsh; \
		echo "Created scsh -> hafod symlink in $(DESTDIR)$(BINDIR)"; \
	fi

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/hafod $(DESTDIR)$(BINDIR)/hafod.sps
	@if [ -L $(DESTDIR)$(BINDIR)/scsh ] && [ "$$(readlink $(DESTDIR)$(BINDIR)/scsh)" = "hafod" ]; then \
		rm -f $(DESTDIR)$(BINDIR)/scsh; \
	fi
	rm -f $(DESTDIR)$(PREFIX)/share/man/man1/hafod.1
	rm -rf $(DESTDIR)$(LIBDIR)
