directory = src/main/scala/parsers/

compiler.jar: $(directory)*.scala
	sbt assembly

clean:
	rm -rf *.jar *.s