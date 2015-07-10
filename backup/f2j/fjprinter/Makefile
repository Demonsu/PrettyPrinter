
include Makefile.inc

.PHONY: all
all: printer

printer: fjprinter.sf
	$(F2J) -c $< $(F2JFLAGS)

test: fjprinter.sf
	$(F2J) -r $< $(F2JFLAGS)

clean:
	rm -f *.java *.class

