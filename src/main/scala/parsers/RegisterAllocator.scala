package parsers

import scala.collection.mutable.ListBuffer
import parsers.RegisterAllocator._
import parsers.Assembly._

//companion object to store global values needed by all RegisterAllocators
object RegisterAllocator{
  //unreserved registers we have access to use in our code generator
  val allTempRegisters = List(tReg4, tReg5, tReg6, tReg7, tReg8, tReg9, tReg10, tReg11)
  val allScratchRegisters = List(reg4, reg5, reg6, reg7, reg8, reg9, reg10, reg11)
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
    this(allTempRegisters.to(ListBuffer))
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
