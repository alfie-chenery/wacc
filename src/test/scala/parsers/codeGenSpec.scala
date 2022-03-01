package parsers

import org.scalatest.flatspec.AnyFlatSpec
import parsers.Parser.parse
import scala.sys.process._
import java.io.File
import scala.collection.mutable.ListBuffer

class codeGenSpec extends AnyFlatSpec {

  /* e.g.
  prompt> make
  prompt> ./compile PATH/FILENAME1.wacc
  prompt> arm-linux-gnueabi-gcc -o FILENAME1 -mcpu=arm1176jzf-s -mtune=arm1176jzf-s FILENAME1.s
  prompt> qemu-arm -L /usr/arm-linux-gnueabi/ FILENAME1
   */

  "make".!!

  val baseTestDir = "src/test/scala/parsers"
  val testDir = "valid/IO/print"
  val filename = "print"

  s"./compile $baseTestDir/$testDir/$filename.wacc".!!

  s"arm-linux-gnueabi-gcc -o $filename -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $filename.s".!!

  s"qemu-arm -L /usr/arm-linux-gnueabi/ $filename".!!

//  behavior of ""

}
