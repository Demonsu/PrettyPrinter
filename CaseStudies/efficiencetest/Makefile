# Path to f2j compiler
F2J      = f2j

# Default flags to compiler
F2JFLAGS = -m stack --inline

test2: 
	 time java -classpath .:/home/xiafan/Desktop/fcore/runtime/runtime.jar Text_2$$


test3: 
	time java -classpath .:/home/xiafan/Desktop/fcore/runtime/runtime.jar Text_3$$

text2: 
	python include.py --include ../../PrettyPrintingLib/ --output text_2.sf nesttext_lib2.sf
	f2j -c -m stack text_2.sf 


text3: 
	python include.py --include ../../PrettyPrintingLib/ --output text_3.sf nesttext_lib3.sf
	f2j -c -m stack text_3.sf 



clean:
	rm -f *.java *.class

