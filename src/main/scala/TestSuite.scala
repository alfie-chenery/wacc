import parsers.Parser._
import parsers.RenamingPass._
import parsers.SemanticPass._

import java.io.File
import scala.collection.mutable.ListBuffer
import scala.sys.process._

object TestSuite {
  def main(args: Array[String]): Unit = {
    val validDir: String = "../wacc_examples/valid"
    otherThanMain(new File(validDir), 0)
  }

  def otherThanMain(dir: File, expectedReturn: Int): Unit = {
    try {
      val files: List[File] = dir.listFiles().toList
      println(files)
      var returnVal = -1
      for (currFile <- files) {
        if (currFile.isFile) {
  //        val program = parse(currFile)
  //        if (program.isSuccess) {
          try {
  //            val errors: ListBuffer[String] = ListBuffer()
  //            val renamedProgram = rename(program.get, errors)
  //            traverse(renamedProgram, errors)
  //            errors.foreach(println(_))
  //          "python runTest.py ./refCompile currFile".!!
  //          if (errors.nonEmpty) returnVal = 100
  //          else {
            s"python3 runTest.py ./refCompile $currFile".!!
            val source = scala.io.Source.fromFile("out.txt")
            val lines = try source.mkString finally source.close()
            if (lines.contains("success")) {
              println("success")
              returnVal = 0
            } else if (lines.contains("failed")) {
              returnVal = 123 //
              println("failed")
  //                sys.exit()
            }
  //          }
          } catch {
            case e: Exception => //e.printStackTrace()
          }
  //        } else {
  //          println(program)
  //          returnVal = 200
  //        }
          println(currFile.toString + ": " + (if (returnVal == expectedReturn) "Passed" else "Failed"))
          println(returnVal)
  //        assert(returnVal == expectedReturn)
        } else {
          otherThanMain(currFile, expectedReturn)
        }
      }
    } catch {
      case e =>
    }
  }

}
