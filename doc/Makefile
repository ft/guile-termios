MAN = guile-termios.7
PDF = guile-termios.pdf
HTML = guile-termios.html
INFO = guile-termios.info

COMMONOPTIONS = --self-contained --number-sections --toc -f markdown

TEX_META = --highlight-style=monochrome
HTML_META = -c manual.css --highlight-style=monochrome

all: man html pdf info
man: $(MAN)
pdf: $(PDF)
html: $(HTML)
info: $(INFO)

guile-termios.7: guile-termios.mdwn
	pandoc -t man $(COMMONOPTIONS) -o $@ guile-termios.mdwn
guile-termios.pdf: guile-termios.mdwn
	pandoc --template template.latex $(COMMONOPTIONS) $(TEX_META) -o $@ guile-termios.mdwn
guile-termios.html: guile-termios.mdwn
	pandoc -t html5 $(HTML_META) $(COMMONOPTIONS) -o $@ guile-termios.mdwn
guile-termios.texi: guile-termios.mdwn
	pandoc --template template.texinfo -t texinfo $(INFO_META) $(COMMONOPTIONS) -o $@ guile-termios.mdwn
guile-termios.info: guile-termios.texi
	makeinfo $< -o $@

clean:
	rm -f $(HTML) $(PDF) $(MAN) $(INFO) *.texi *~

.PHONY: all man html info pdf clean
