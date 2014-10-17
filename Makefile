CC = cc
GUILE_BINARY = guile
PERL_BINARY = perl
HARNESS = prove
TESTDRIVER = "/bin/sh ./test-driver"
DOC_PREFIX ?= "/usr/local"

all: gps scheme/termios/system.scm

doc:
	(cd doc && $(MAKE) all;)

gen-platform-specifics.c: gen-gps.scm gen-gps.sh
	GUILE_BINARY="$(GUILE_BINARY)" sh ./gen-gps.sh > $@

gps: gen-platform-specifics.c config.h
	$(CC) -o $@ $<

config.h: config.h.in gen-config.h.sh
	C_COMPILER="$(CC)" sh ./gen-config.h.sh

scheme/termios/system.scm: gps
	[ -d scheme/termios ] || mkdir -p scheme/termios
	./gps > $@

compile:
	$(MAKE) scheme/termios/system.scm
	GUILE_BINARY="$(GUILE_BINARY)" sh ./compile

clean-byte-compile:
	rm -f scheme/*.go scheme/termios/*.go scheme/test/*.go

clean: clean-byte-compile
	rm -f gps gen-platform-specifics.c scheme/termios/system.scm
	rm -f scheme/test/*~ scheme/termios/*~ scheme/*~ *~
	rm -f config.h

install-doc:
	GUILE_BINARY="$(GUILE_BINARY)" sh ./install documentation $(DOC_PREFIX)

install:
	GUILE_BINARY="$(GUILE_BINARY)" sh ./install

plausible:
	sh ./tests/test-this-terminal.sh

test-suite-verbose:
	@echo "Running the test suite in verbose mode..."
	GUILE_BINARY="$(GUILE_BINARY)" PERL_BINARY="$(PERL_BINARY)" $(HARNESS) --verbose --color --merge --exec $(TESTDRIVER) ./tests/*.t

test-suite:
	@echo "Running the test suite in quiet mode..."
	GUILE_BINARY="$(GUILE_BINARY)" PERL_BINARY="$(PERL_BINARY)" $(HARNESS) --color --merge --exec $(TESTDRIVER) ./tests/*.t

test: plausible test-suite

test-verbose: plausible test-suite-verbose

.PHONY: all clean clean-byte-compile compile doc install plausible test-suite test-suite-verbose test test-verbose
