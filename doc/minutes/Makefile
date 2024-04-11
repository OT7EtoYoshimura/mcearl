.POSIX:
.SUFFIXES: .md .pdf
.PHONY: all clean

all: 02-21.pdf 02-22.pdf 02-29.pdf 03-07.pdf 03-13.pdf 03-25.pdf 04-03.pdf

clean:
	rm -rf *.pdf

.md.pdf:
	pandoc $< -o $@
