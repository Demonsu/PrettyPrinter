
include Makefile.inc

.PHONY: all
all: printer

printer: f2jprinter.sf
	$(F2J) -c $< $(F2JFLAGS)

test: f2jprinter.sf
	$(F2J) -r $< $(F2JFLAGS)

clean:
	rm -f *.java *.class

