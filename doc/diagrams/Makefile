.POSIX:
.SUFFIXES: .puml .png
.PHONY: all clean

all:  c1-mcearl.png c2-mcearl.png c3-origin.png c3-server.png sequence.png

clean:
	rm -rf *.png
.puml.png:
	plantuml $<
