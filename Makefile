CC = cc

all: gps scheme/termios/system.scm

gen-platform-specifics.c: gen-gps.scm gen-gps.sh
	sh ./gen-gps.sh > $@

gps: gen-platform-specifics.c
	$(CC) -o $@ $<

scheme/termios/system.scm: gps
	[ -d scheme/termios ] || mkdir -p scheme/termios
	./gps > $@

compile:
	$(MAKE) scheme/termios/system.scm
	sh ./compile

clean-byte-compile:
	rm -Rf scheme/*.go scheme/termios/*.go

clean: clean-byte-compile
	rm -Rf gps gen-platform-specifics.c scheme/termios *~ scheme/*~

test:
	sh ./test-this-terminal.sh

.PHONY: all clean clean-byte-compile compile test
