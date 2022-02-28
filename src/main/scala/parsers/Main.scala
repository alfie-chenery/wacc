package parsers

import parsers.Parser.parse

import java.io.File
import scala.collection.mutable.ListBuffer

class Main {
  def main(args: Array[String]): Unit = {

    if (args.length == 0 || !args(0).endsWith(".wacc")) {
      println("Please pass a .wacc file to be parsed")
      sys.exit()
    }

    // Run compiler frontend
    val errors: ListBuffer[String] = ListBuffer()
    val program = parse(new File(args(0)))

    if (!program.isSuccess) {
      println(program)
      sys.exit(100)
    }

    val renamedProgram = RenamingPass.rename(program.get, errors)

    SemanticPass.traverse(renamedProgram, errors)

    if (errors.nonEmpty) {
      errors.foreach(println(_))
      sys.exit(200)
    }
    val ALL_AVAILABLE_REGISTERS = ListBuffer[Int](0, 1, 2, 3, 4)
    // CodeGen.traverse(renamedProgram, RegisterAllocator(ALL_AVAILABLE_REGISTERS))

    // Run compiler backend
    // only gets here if hasn't exited

    sys.exit(0)

  }
}
