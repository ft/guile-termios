PROJECT = guile-termios
TOPDIR = .
LOAD_PATH = $(TOPDIR)/scheme
TEST_PATH = $(TOPDIR)/tests

GUILE_BINARY = guile
GUILE_CALL = $(GUILE_BINARY) -L $(LOAD_PATH) -C $(LOAD_PATH) --no-auto-compile
GUILD_BINARY ?= guild

CC = cc
PERL_BINARY = perl
HARNESS = tap-harness
TESTDRIVER = ./tools/test-driver

INSTALL = $(GUILE_BINARY) --no-auto-compile ./tools/install
DESTDIR =
PREFIX = /usr/local
DOCDIR = $(PREFIX)/share/doc/$(PROJECT)

CFLAGS  = -Wunsupported-warning -Wunused-variable # -Wunused-toplevel
CFLAGS += -Wunbound-variable -Warity-mismatch -Wduplicate-case-datum
CFLAGS += -Wbad-case-datum -Wformat -L$(LOAD_PATH) $(CFLAGS_ADD)

COMPILE = $(GUILD_BINARY) compile $(CFLAGS)

MODULES  = scheme/termios.scm
MODULES += scheme/termios/with-exceptions.scm
MODULES += scheme/termios/system.scm
MODULES += scheme/termios/frontends.scm
OBJECTS = ${MODULES:.scm=.go}

.SUFFIXES: .scm .go

all: generate compile

.scm.go:
	$(COMPILE) -o $@ $<

generate: gps scheme/termios/system.scm

compile: $(OBJECTS)

gen-platform-specifics.c: tools/gen-gps.scm
	$(GUILE_BINARY) --no-auto-compile ./tools/gen-gps.scm > $@

gps: gen-platform-specifics.c config.h
	$(CC) -o $@ $<

config.h: config.h.in tools/gen-config.h.sh
	C_COMPILER="$(CC)" sh ./tools/gen-config.h.sh

scheme/termios/system.scm: gps
	[ -d scheme/termios ] || mkdir -p scheme/termios
	./gps > $@

clean-byte-compile:
	rm -f scheme/*.go scheme/termios/*.go scheme/test/*.go

clean: clean-byte-compile
	rm -f gps gen-platform-specifics.c scheme/termios/system.scm
	rm -f scheme/test/*~ scheme/termios/*~ scheme/*~ *~
	rm -f config.h
	$(MAKE) -C doc clean

doc:
	$(MAKE) -C doc all

install: all
	$(INSTALL) DESTDIR="$(DESTDIR)" DOCDIR="$(DOCDIR)" PREFIX="$(PREFIX)"

plausible:
	sh $(TEST_PATH)/test-this-terminal.sh

test-suite-verbose:
	@echo "Running the test suite in verbose mode..."
	$(HARNESS) --verbose --exec $(TESTDRIVER) $(TEST_PATH)/*.t

test-suite:
	@echo "Running the test suite in quiet mode..."
	$(HARNESS) --exec $(TESTDRIVER) $(TEST_PATH)/*.t

test: plausible test-suite

test-verbose: plausible test-suite-verbose

.PHONY: all clean clean-byte-compile compile doc generate install plausible test-suite test-suite-verbose test test-verbose
