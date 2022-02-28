package parsers

import parsers.Parser.parse

import java.io.File
import scala.collection.mutable.ListBuffer

class Main {
  def main(args: Array[String]): Unit = {
    val errors: ListBuffer[String] = ListBuffer()
    if (args.length == 0 || !args(0).endsWith(".wacc")) println("Please pass a .wacc file to be parsed")
    else {
      val program = parse(new File(args(0)))
      if (program.isSuccess) {
        SemanticPass.traverse(RenamingPass.rename(program.get, errors), errors)
        if (errors.nonEmpty) {
          errors.foreach(println(_))
          sys.exit(200)
        } else {
          sys.exit(0)
        }
      } else {
        println(program)
        sys.exit(100)
      }
    }
  }
}
