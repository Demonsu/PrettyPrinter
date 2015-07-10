
include Makefile.inc

.PHONY: all
all: printer

printer: treeprinter.sf
	$(F2J) -c $< $(F2JFLAGS)

test: treeprinter.sf
	$(F2J) -r $< $(F2JFLAGS)

test2: treeprinter2.sf
	$(F2J) -r $< $(F2JFLAGS)

clean:
	rm -f *.java *.class

