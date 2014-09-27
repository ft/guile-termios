CC = cc
GUILE_BINARY = guile
PERL_BINARY = perl
HARNESS = prove
TESTDRIVER = "/bin/sh ./test-driver"

all: gps scheme/termios/system.scm

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
	rm -Rf scheme/*.go scheme/termios/*.go scheme/test/*.go

clean: clean-byte-compile
	rm -f gps gen-platform-specifics.c
	rm -f scheme/test/*~ scheme/termios/*~ scheme/*~ *~
	rm -f config.h

install:
	GUILE_BINARY="$(GUILE_BINARY)" sh ./install

plausible:
	sh ./tests/test-this-terminal.sh

test-suite:
	$(HARNESS) --verbose --color --merge --exec $(TESTDRIVER) ./tests/*.t

test: plausible test-suite

.PHONY: all clean clean-byte-compile compile install plausible test-suite test
