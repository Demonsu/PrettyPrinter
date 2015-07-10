
# Path to f2j compiler
F2J      = f2j

# Default flags to compiler
F2JFLAGS = -m stack --inline


printer: 
	python include.py --include ../../PrettyPrintingLib/ --output treeprinter.sf TreeToDocument.sf

printer2: 
	python include.py --include ../../PrettyPrintingLib/ --output treeprinter2.sf TreeToDocument2.sf

printer3: 
	python include.py --include ../../PrettyPrintingLib/ --output treeprinter3.sf TreeToDocument3.sf

test: treeprinter.sf
	$(F2J) -r $< $(F2JFLAGS)

test2: treeprinter2.sf
	$(F2J) -r $< $(F2JFLAGS)

test3: treeprinter3.sf
	$(F2J) -r $< $(F2JFLAGS)

clean:
	rm -f *.java *.class

