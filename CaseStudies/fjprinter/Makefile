
# Path to f2j compiler
F2J      = f2j

# Default flags to compiler
F2JFLAGS = -m stack --inline


printer: 
	python include.py --include ../../PrettyPrintingLib/ --output fjprinter.sf TestFJPrinter.sf

test: fjprinter.sf
	$(F2J) -r $< $(F2JFLAGS)

clean:
	rm -f *.java *.class

