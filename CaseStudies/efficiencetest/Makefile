# Path to f2j compiler
F2J      = f2j

# Default flags to compiler
F2JFLAGS = -m stack --inline

test2: text_2.sf
	$(F2J) -r $< 

test3: text_3.sf
	$(F2J) -r $< 

text2: 
	python include.py --include ../../PrettyPrintingLib/ --output text_2.sf nesttext_lib2.sf

text3: 
	python include.py --include ../../PrettyPrintingLib/ --output text_3.sf nesttext_lib3.sf


clean:
	rm -f *.java *.class

