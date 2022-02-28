package parsers

import scala.collection.mutable.ListBuffer

//companion object to store global values needed by all RegisterAllocators
object RegisterAllocatorGlobals{
  //reserved registers
  val retReg = "r0"
  //by convention r1, r2, r3 are reserved
  val sp = "r13"
  val linkReg = "r14"
  val pc = "r15"

  //unreserved registers we have access to use in our code generator
  val allRegisters = List(4, 5, 6, 7, 8, 9, 10, 11, 12)
}

class RegisterAllocator(availableRegs: ListBuffer[Int]) {
  import parsers.RegisterAllocatorGlobals._

  //Default constructor
  private var availableRegisters: ListBuffer[Int] = availableRegs
  private val initialAvailable:   ListBuffer[Int] = availableRegisters.clone()

  //Auxiliary constructor: if no list buffer in constructor, assume we have access to all registers
  def this() = {
    this(allRegisters.to(ListBuffer))
  }


  def next(): String = {
    val reg = "r" + availableRegisters.head.toString
    availableRegisters.remove(0)
    reg
  }

  def restore(): Unit = {
    availableRegisters = initialAvailable.clone()
  }

  def getAvailable: ListBuffer[Int] = {
    availableRegisters
  }


}
