package parsers
import scala.collection.mutable.ListBuffer

class RegisterAllocator(availableRegisters: ListBuffer[Int]) {

  val allRegisters = List(4, 5, 6, 7, 8, 9, 10, 11, 12)
  val initialAvailable = availableRegisters.clone()
  //TODO check .clone does a deep copy, ie modifying availableRegs doesnt modify initialAvailable
  //reserved registers
  val retReg = "r0"
  val sp = "r13"
  val linkReg = "r14"
  val pc = "r15"

  //if no list buffer in constructor, assume we have access to all registers
  /*
  def this() = {
    this(allRegisters.to(ListBuffer))
  }
   */


  def next(): String = {
    val reg = "r" + availableRegisters.head.toString
    availableRegisters.remove(0)
    reg
  }

  def restore(): Unit = {
    val used = initialAvailable.diff(availableRegisters)
    availableRegisters.addAll(used)
  }

  def getAvailable: ListBuffer[Int] = {
    availableRegisters
  }


}
