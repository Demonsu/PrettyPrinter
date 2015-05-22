
include Makefile.inc

.PHONY: all
all: printer

printer: PrettyPrinterLib2.sf
	$(F2J) -c $< $(F2JFLAGS)

test: PrettyPrinterLib2.sf
	$(F2J) -r $< $(F2JFLAGS)

clean:
	rm -f *.java *.class

