.POSIX:
.SUFFIXES: .puml .png
.PHONY: all clean

all: c1-mcearl.png c2-mcearl-v1.png c2-mcearl-v2.png c3-origin.png c3-server-v1.png c3-server-v2.png sequence.png

clean:
	rm -rf *.png
.puml.png:
	plantuml $<
