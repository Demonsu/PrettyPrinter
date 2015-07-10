
include Makefile.inc

.PHONY: all
all: printer

printer: xmlprinter.sf
	$(F2J) -c $< $(F2JFLAGS)

test: xmlprinter.sf
	$(F2J) -r $< $(F2JFLAGS)

clean:
	rm -f *.java *.class

