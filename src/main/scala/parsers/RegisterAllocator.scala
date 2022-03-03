package parsers

import scala.collection.mutable.ListBuffer
import parsers.RegisterAllocator._
import parsers.Assembly._

//companion object to store global values needed by all RegisterAllocators
object RegisterAllocator{
  //unreserved registers we have access to use in our code generator
  val allRegisters = List(reg(4), reg(5), reg(6), reg(7), reg(8), reg(9), reg(10)) // reserve 11 for pushing/popping
//  val allRegisters = List(reg(4), reg(5), reg(6), reg(7), reg(8), reg(9), reg(10), reg(11))
//  val allRegisters = List(reg(4), reg(5), reg(6), reg(7), reg(8), reg(9), reg(10), reg(11), reg(12))
}

class RegisterAllocator(private var availableRegisters: ListBuffer[Register]) {

  //Default constructor
  private val initialAvailable:   ListBuffer[Register] = availableRegisters.clone()

  //Auxiliary constructor: if no list buffer in constructor, assume we have access to all registers
  def this() = {
    this(allRegisters.to(ListBuffer))
  }


  def next: Register = {
    availableRegisters.head
  }

  def nextRm: Register = {
    availableRegisters.remove(0)
  }

  def restore(): Unit = {
    availableRegisters = initialAvailable.clone()
  }
  def restore(reg: Register): Unit = {
    availableRegisters += reg
  }

  def getAvailable: ListBuffer[Register] = {
    availableRegisters
  }

  def isEmpty: Boolean = {
    availableRegisters.isEmpty
  }

  def isOnLastReg: Boolean = {
    availableRegisters.size == 1
  }

  def getLast: Register = {
    if (!isEmpty) availableRegisters.last else initialAvailable.last
  }

}
