MAN = guile-termios.3
PDF = guile-termios.pdf
HTML = guile-termios.html

COMMONOPTIONS = --self-contained --number-sections --normalize --smart --toc

TEX_META = -V date='\today' --highlight-style=monochrome
HTML_META = -c manual.css

DISPATCH_TEX_META = -V author="Frank Terbeck"
DISPATCH_TEX_META += -V title="guile-termios(1) Manual"
DISPATCH_HTML_META = $(DISPATCH_TEX_META) --highlight-style=monochrome
DISPATCH_MAN_META = -V title="guile-termios" -V section="1" -V date="Nov, 2012"

all: man html pdf
man: $(MAN)
pdf: $(PDF)
html: $(HTML)

guile-termios.3: guile-termios.mdwn
	pandoc -t man $(COMMONOPTIONS) $(DISPATCH_MAN_META) -o $@ guile-termios.mdwn
guile-termios.pdf: guile-termios.mdwn
	pandoc --template template.latex $(COMMONOPTIONS) $(TEX_META) $(DISPATCH_TEX_META) -o $@ guile-termios.mdwn
guile-termios.html: guile-termios.mdwn
	pandoc -t html5 $(HTML_META) $(COMMONOPTIONS) $(DISPATCH_HTML_META) -o $@ guile-termios.mdwn

clean:
	rm -f $(HTML) $(PDF) $(MAN) *~

.PHONY: all man html pdf clean
