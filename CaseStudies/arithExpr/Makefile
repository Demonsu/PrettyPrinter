# Path to f2j compiler
F2J      = f2j

# Default flags to compiler
F2JFLAGS = -m stack --inline

test: arithprinter.sf
	$(F2J) -r $< $(F2JFLAGS)

testall: pp.sf
	$(F2J) -r $< $(F2JFLAGS)

printer: 
	python include.py --include ../../PrettyPrintingLib/ --output arithprinter.sf TestArithPrinter.sf 

pp: 
	python include.py --include ~/Desktop/FParser/src/ ../../PrettyPrintingLib/ --output pp.sf TestWhole.sf 



clean:
	rm -f *.java *.class

