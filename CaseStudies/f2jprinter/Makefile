# Path to f2j compiler
F2J      = f2j

# Default flags to compiler
F2JFLAGS = -m stack --inline

test: f2jprinter.sf
	$(F2J) -r $< $(F2JFLAGS)

printer: 
	python include.py --include ../../PrettyPrintingLib/ --output f2jprinter.sf TestPrettyPrinter.sf 

pp:
	python include.py  --include ~/Desktop/FParser/src/ ~/Desktop/PrettyPrinter/PrettyPrintingLib/ --output pp.sf TestWhole.sf 

testall:pp.sf
	$(F2J) -r $< $(F2JFLAGS)

clean:
	rm -f *.java *.class

