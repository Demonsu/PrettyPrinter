
# Path to f2j compiler
F2J      = f2j

# Default flags to compiler
F2JFLAGS = -m stack --inline


printer: 

	python include.py --include ../../PrettyPrintingLib/ --output xmlprinter.sf XMLToDocument.sf 


test: xmlprinter.sf
	$(F2J) -r $< $(F2JFLAGS)

clean:
	rm -f *.java *.class

