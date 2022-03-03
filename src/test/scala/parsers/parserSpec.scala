package parsers

import org.scalatest.flatspec.AnyFlatSpec

class parserSpec extends AnyFlatSpec{
  import Parser._
  import java.io.File
  import scala.collection.mutable.ListBuffer

  def runFrontEnd(dir: File, expectedReturn: Int): Unit = {
    val files: List[File] = dir.listFiles().toList
    println(files)
    var returnVal = -1
    for (currFile <- files) {
      if (currFile.isFile) {
        val program = parse(currFile)
        if (program.isSuccess) {
          try {
            val errors: ListBuffer[String] = ListBuffer()
            val renamedProgram = RenamingPass.rename(program.get, errors)
            SemanticPass.traverse(renamedProgram, errors)
            errors.foreach(println(_))
            if (errors.nonEmpty) returnVal = 100
            else returnVal = 0
          } catch {
            case e: Exception => e.printStackTrace()
          }
        } else {
          println(program)
          returnVal = 200
        }
        println(currFile.toString + ": " + (if (returnVal == expectedReturn) "Passed" else "Failed"))
        assert(returnVal == expectedReturn)
      } else {
        runFrontEnd(currFile, expectedReturn)
      }
    }
  }

  val testDir = "src/test/scala/parsers/"

  behavior of "valid programs"
  they should "parse basic programs" in {
    runFrontEnd(new File(testDir + "valid/basic/"), 0)
  }
  /*
  they should "parse array programs" in {
    runFrontEnd(new File(testDir + "valid/array/"), 0)
  }
   */
  /*
  they should "parse advanced programs" in {
    runFrontEnd(new File(testDir + "valid/advanced/"), 0)
  }
   */
  they should "parse expression programs" in {
    runFrontEnd(new File(testDir + "valid/expressions/"), 0)
  }
  they should "parse function programs" in {
    runFrontEnd(new File(testDir + "valid/function/"), 0)
  }
  they should "parse if programs" in {
    runFrontEnd(new File(testDir + "valid/if/"), 0)
  }
  they should "parse IO programs" in {
    runFrontEnd(new File(testDir + "valid/IO/"), 0)
  }
  /*
  they should "parse pair programs" in {
    runFrontEnd(new File(testDir + "valid/pairs/"), 0)
  }
  */
  /*
  they should "parse runtimeErr programs" in {
    runFrontEnd(new File(testDir + "valid/runtimeErr/"), 0)
  }
  */
  they should "parse scope programs" in {
    runFrontEnd(new File(testDir + "valid/scope/"), 0)
  }
  they should "parse sequence programs" in {
    runFrontEnd(new File(testDir + "valid/sequence/"), 0)
  }
  they should "parse variable programs" in {
    runFrontEnd(new File(testDir + "valid/variables/"), 0)
  }
  they should "parse while programs" in {
    runFrontEnd(new File(testDir + "valid/while/"), 0)
  }

  behavior of "syntax errors"
  they should "be thrown by expressions" in {
    runFrontEnd(new File(testDir + "invalid/syntaxErr/expressions/"), 200)
  }
  /*
  they should "be thrown by functions" in {
    runFrontEnd(new File(testDir + "invalid/syntaxErr/function/"), 200)
  }
  */
  they should "be thrown by if statements" in {
    runFrontEnd(new File(testDir + "invalid/syntaxErr/if/"), 200)
  }
  they should "be thrown by pairs" in {
    runFrontEnd(new File(testDir + "invalid/syntaxErr/pairs/"), 200)
  }
  they should "be thrown by print" in {
    runFrontEnd(new File(testDir + "invalid/syntaxErr/print/"), 200)
  }
  /*
  they should "be thrown by variables" in {
    runFrontEnd(new File(testDir + "invalid/syntaxErr/variables/"), 200)
  }
  */
  /*
  they should "be thrown by while" in {
    runFrontEnd(new File(testDir + "invalid/syntaxErr/while/"), 200)
  }
  */
  they should "be thrown by array" in {
    runFrontEnd(new File(testDir + "invalid/syntaxErr/array/"), 200)
  }
  /*
  they should "be thrown by basic programs" in {
    runFrontEnd(new File(testDir + "invalid/syntaxErr/basic/"), 200)
  }
  */
  they should "be thrown by sequence" in {
    runFrontEnd(new File(testDir + "invalid/syntaxErr/sequence/"), 200)
  }

  behavior of "semantic errors"
  /*
  they should "be thrown by exit" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/exit/"), 100)
  }
  */
  they should "be thrown by expressions" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/expressions/"), 100)
  }
  /*
  they should "be thrown by functions" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/function/"), 100)
  }
  */
  they should "be thrown by if statements" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/if/"), 100)
  }
  /*
  they should "be thrown by IO" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/IO/"), 100)
  }
  */
  they should "contain multiple" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/multiple/"), 100)
  }
  they should "be thrown by pairs" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/pairs/"), 100)
  }
  they should "be thrown by print" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/print/"), 100)
  }
  /*
  they should "be thrown by read" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/read/"), 100)
  }
  */
  they should "be thrown by scope" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/scope/"), 100)
  }
  they should "be thrown by variables" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/variables/"), 100)
  }
  they should "be thrown by while" in {
    runFrontEnd(new File(testDir + "invalid/semanticErr/while/"), 100)
  }

}
