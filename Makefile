directory = src/main/scala/parsers/

compiler.jar: $(directory)Ast.scala $(directory)Parser.scala $(directory)RenamingPass.scala $(directory)SemanticPass.scala
	sbt assembly

clean:
	rm -rf *.jar