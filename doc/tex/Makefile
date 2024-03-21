.POSIX:
.SUFFIXES: .tex .pdf
.PHONY: all clean

all: project-plan.pdf research-plan.pdf architecture-design.pdf

clean:
	rm -f out/*

.tex.pdf:
	pdflatex -output-directory=out/ $<
	pdflatex -output-directory=out/ $<
	ln -s out/$@
