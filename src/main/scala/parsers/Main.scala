package parsers

import parsers.Parser.{`<expr>`, parse}

import java.io.{File, FileWriter}
import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]): Unit = {
//    try {
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
        for (error <- errors) {
          if (error.startsWith("Syntax error")) sys.exit(100)
          println(error)
        }
        errors.foreach(println(_))
        sys.exit(200)
      }

      // Run compiler backend
      // only gets here if hasn't exited
      val filename = args(0).split('/').last
      val outputFilename = filename.split('.')(0) + ".s"
      val fw = new FileWriter(outputFilename)
      var failed = false
      try {
        fw.write(CodeGen.compile(renamedProgram))
      } catch {
        case c: RuntimeException =>
          failed = true
          println(s"""Errors detected during compilation! Exit code 200 returned.
               |${c.toString}""".stripMargin)
      }
      fw.close()
      if (failed) sys.exit(200) else sys.exit(0)
//    } catch {
//      case e: Exception =>
//    }
  }
}
