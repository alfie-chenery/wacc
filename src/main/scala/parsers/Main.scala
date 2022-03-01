package parsers

import parsers.Parser.parse

import java.io.{File, FileWriter}
import scala.collection.mutable.ListBuffer

object Main {
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

    // Run compiler backend
    // only gets here if hasn't exited
    val filename = args(0).split('/').last
    val outputFilename = filename.split('.')(0) + ".s"
    val fw = new FileWriter(outputFilename)
    val cg = CodeGen
    cg.traverse(renamedProgram, new RegisterAllocator())
    fw.write(cg.compile())
    fw.close()

    sys.exit(0)
  }
}
