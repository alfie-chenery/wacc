package parsers

import parsers.Assembly.reg

import scala.collection.mutable.ListBuffer
import parsers.RegisterAllocator.allRegisters

//companion object to store global values needed by all RegisterAllocators
object RegisterAllocator{
  //unreserved registers we have access to use in our code generator
  val allRegisters = List(reg(4), reg(5), reg(6), reg(7), reg(8), reg(9), reg(10), reg(11), reg(12))
}

class RegisterAllocator(private var availableRegisters: ListBuffer[Int]) {

  //Default constructor
  private val initialAvailable:   ListBuffer[Int] = availableRegisters.clone()

  //Auxiliary constructor: if no list buffer in constructor, assume we have access to all registers
  def this() = {
    this(allRegisters.to(ListBuffer))
  }


  def next(): String = {
    "r" + availableRegisters.remove(0).toString
  }

  def restore(): Unit = {
    availableRegisters = initialAvailable.clone()
  }

  def getAvailable: ListBuffer[Int] = {
    availableRegisters
  }


}
