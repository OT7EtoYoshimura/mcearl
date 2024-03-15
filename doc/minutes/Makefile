.POSIX:
.SUFFIXES: .md .pdf
.PHONY: all

all: 02-21.pdf 02-22.pdf 02-29.pdf 03-07.pdf 03-13.pdf

.md.pdf:
	pandoc $< -o $@
