CC = cc

all: gps scheme/termios/system.scm

gen-platform-specifics.c: gen-gps.scm gen-gps.sh
	sh ./gen-gps.sh > $@

gps: gen-platform-specifics.c
	$(CC) -o $@ $<

scheme/termios/system.scm: gps
	[ -d scheme/termios ] || mkdir -p scheme/termios
	./gps > $@

clean:
	rm -Rf gps gen-platform-specifics.c scheme/termios *~ scheme/*~

test:
	sh ./test-this-terminal.sh

.PHONY: all clean test
