# Path to f2j compiler
F2J      = f2j

# Default flags to compiler
F2JFLAGS = -m stack --inline

test: f2jprinter.sf
	$(F2J) -r $< $(F2JFLAGS)

printer: 
	python include.py --include ../../PrettyPrintingLib/ --output f2jprinter2.sf TestPrettyPrinter.sf 


clean:
	rm -f *.java *.class

