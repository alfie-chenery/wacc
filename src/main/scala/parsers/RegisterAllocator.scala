package parsers

import scala.collection.mutable.ListBuffer
import parsers.RegisterAllocator._
import parsers.Assembly._

//companion object to store global values needed by all RegisterAllocators
object RegisterAllocator{
  //unreserved registers we have access to use in our code generator
  val allRegisters = List(reg(4), reg(5), reg(6), reg(7), reg(8), reg(9), reg(10), reg(11))
  val vfpRegisters = List(vfpReg(4), vfpReg(5), vfpReg(6), vfpReg(7), vfpReg(8), vfpReg(9), vfpReg(10), vfpReg(11))
}

class VfpAllocator(private var availableRegisters: ListBuffer[Register]){
  //Default constructor
  private val initialAvailable:   ListBuffer[Register] = availableRegisters.clone()

  //Auxiliary constructor: if no list buffer in constructor, assume we have access to all registers
  def this() = {
    this(vfpRegisters.to(ListBuffer))
  }


  def next: Register = {
    availableRegisters.head
  }

  def nextRm: Register = {
    availableRegisters.remove(0)
  }

  /** Restores availableRegisters to state when this RA was first initialised */
  def restore(): Unit = {
    availableRegisters = initialAvailable.clone()
  }

  def getAvailable: ListBuffer[Register] = {
    availableRegisters
  }

  def isEmpty: Boolean = {
    availableRegisters.isEmpty
  }

  def size: Int = {
    availableRegisters.size
  }
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

  /** Restores availableRegisters to state when this RA was first initialised */
  def restore(): Unit = {
    availableRegisters = initialAvailable.clone()
  }

  def getAvailable: ListBuffer[Register] = {
    availableRegisters
  }

  def isEmpty: Boolean = {
    availableRegisters.isEmpty
  }

  def size: Int = {
    availableRegisters.size
  }

}
