package parsers

import scala.collection.mutable

// TODO remove use of nullOp
object Assembly {


  sealed trait Mnemonic
  case class funcName(name: String) extends Mnemonic {
    override def toString: String = name + ":"
  }
  case object LTORG extends Mnemonic {
    override def toString: String = ".ltorg"
  }

  /**
   * Data definition directive that initialises 1+ 4-byte ints
   * @param size
   */
  case class DWord(size: Int) extends Mnemonic {
    override def toString: String = ".word " + size.toString
  }

  /**
   * String definition directive for an ascii string (won't automatically append a sentinel /0 character)
   * @param string
   */
  case class DAscii(string: String) extends Mnemonic {
    override def toString: String = s""".ascii "$string" """
  }

  //TODO: to string has conditional comma
  case class LDR(r: Register, o2: Operand, suffix: Suffix) extends Mnemonic {
    override def toString: String = {
      val o2String = if (!o2.isInstanceOf[regShift]) o2.toString.replace('#', '=') else o2.toString
      "LDR" + suffix.toString + " " + r.toString + ", " + o2String
    }
  }
  case class STR(rd: Register, rn: Register) extends Mnemonic {
    override def toString: String = {
      "STR " + rd.toString + ", " + rn.toString
    }
  }
  case class PUSH(r: Register) extends Mnemonic {
    override def toString: String = "PUSH {" + r.toString + "}"
  }
  case class POP(r: Register) extends Mnemonic {
    override def toString: String = "POP {" + r.toString + "}"
  }
  case class SUB(o1: Operand, o2: Operand, o3: Operand) extends Mnemonic {
    override def toString: String = "SUB " + o1.toString + ", " + o2.toString + ", " + o3.toString
  }

  case class ADD(o1: Operand, o2: Operand, o3: Operand) extends Mnemonic {
    override def toString: String = "ADD " + o1.toString + ", " + o2.toString + ", " + o3.toString
  }
  case class MOV(rd: Register, o2: Operand, suffix: Suffix) extends Mnemonic {
    override def toString: String = "MOV" + suffix.toString + " " + rd.toString + ", " + o2.toString
  }
  case class BL(label: String, suffix: Suffix) extends Mnemonic {
    override def toString: String = "BL" + suffix.toString + " " + label
  }
  case class CMP(r: Register, o2: Operand) extends Mnemonic {
    override def toString: String = "CMP " + r.toString + ", " + o2.toString
  }
  case class STRB(rd: Register, rn: Register) extends Mnemonic {
    override def toString: String = {
      "STRB " + rd.toString + ", " + rn.toString
    }
  }
  case class AND(rd: Register, rn: Register, rm: Register) extends Mnemonic {
    override def toString: String = "AND " + rd.toString + ", " + rn.toString + ", " + rm.toString
  }
  case class ORR(rd: Register, rn: Register, rm: Register) extends Mnemonic {
    override def toString: String = "ORR " + rd.toString + ", " + rn.toString + ", " + rm.toString
  }
  case class ADDS(rd: Register, rn: Register, rm: Register) extends Mnemonic {
    override def toString: String = "ADDS " + rd.toString + ", " + rn.toString + ", " + rm.toString
  }
  case class SUBS(rd: Register, rn: Register, rm: Register) extends Mnemonic {
    override def toString: String = "SUBS " + rd.toString + ", " + rn.toString + ", " + rm.toString
  }
  case class MULTS(rd: Register, rn: Register, rm: Register) extends Mnemonic {
    override def toString: String = "MULTS " + rd.toString + ", " + rn.toString + ", " + rm.toString
  }
  case class SMULL(rdLo: Register, rdHi: Register, rm: Register, rs: Register) extends Mnemonic {
    override def toString: String =
      "SMULL " + rdLo.toString + ", " + rdHi.toString + ", " + rm.toString + ", " + rs.toString
  }
  case class EOR(rd: Register, o2: Operand, o3: Operand) extends Mnemonic {
    override def toString: String = "EOR " + rd.toString + ", " + o2.toString + ", " + o3.toString
  }
  case class RSBS(rd: Register, o2: Operand, o3: Operand) extends Mnemonic {
    override def toString: String = "RSBS " + rd.toString + ", " + o2.toString + ", " + o3.toString
  }
  case class B(label: String, suffix: Suffix) extends Mnemonic {
    override def toString: String = "B" + suffix.toString + " " + label
  }

  sealed trait Operand
  case class imm(i: Int) extends Operand {
    override def toString: String = "#" + i.toString
  }
  case class immc(c: String) extends Operand {
    override def toString: String = "#'" + c + "'"
  }
  case class label(l: String) extends Operand {
    override def toString: String = "=" + l
  }
  case class regShift(reg: Register, shift: Int, update: Boolean) extends Register {
    override def toString: String = "[" + reg.toString + ", #" + shift + "]" + (if (update) "!" else "")
  }
  case class regVal(reg: Register) extends Register {
    override def toString: String = "[" + reg.toString + "]"
  }
  case class asr(reg: Register, shift: Int) extends Operand {
    override def toString: String = reg.toString + ", ASR #" + shift
  }
  case class lsl(reg: Register, shift: Int) extends Operand {
    override def toString: String = reg.toString + ", LSL #" + shift
  }
  case object nullOp extends Operand {
    override def toString: String = ""
  }

  sealed trait Register extends Operand
  case object RetReg extends Register{
    override def toString: String = "r0"
  }
  case object reg1 extends Register { // todo: check that it doesn't have a special name? / rename if necessary
    override def toString: String = "r1" // should it be included in scratchRegisters or not?
  }                                      // or maybe a separate trait should be made for these, and
  case object reg2 extends Register {    // both put under another trait with same scope as former 'reg'
    override def toString: String = "r2" // todo: same as ^
  }
  case object SP extends Register{
    override def toString: String = "sp"
  }
  case object LinkReg extends Register{
    override def toString: String = "lr"
  }
  case object PC extends Register{
    override def toString: String = "pc"
  }
  sealed trait TempReg extends Register // to be used as temporary registers before reg allocation
  case object tReg4 extends TempReg{
    override def toString: String = "r4"
  }
  case object tReg5 extends TempReg{
    override def toString: String = "r5"
  }
  case object tReg6 extends TempReg{
    override def toString: String = "r6"
  }
  case object tReg7 extends TempReg{
    override def toString: String = "r7"
  }
  case object tReg8 extends TempReg{
    override def toString: String = "r8"
  }
  case object tReg9 extends TempReg{
    override def toString: String = "r9"
  }
  case object tReg10 extends TempReg{
    override def toString: String = "r10"
  }
  case object tReg11 extends TempReg{
    override def toString: String = "r11"
  }
  sealed trait ScratchReg extends Register
  case object reg4 extends ScratchReg{
    override def toString: String = "r4"
  }
  case object reg5 extends ScratchReg{
    override def toString: String = "r5"
  }
  case object reg6 extends ScratchReg{
    override def toString: String = "r6"
  }
  case object reg7 extends ScratchReg{
    override def toString: String = "r7"
  }
  case object reg8 extends ScratchReg{
    override def toString: String = "r8"
  }
  case object reg9 extends ScratchReg{
    override def toString: String = "r9"
  }
  case object reg10 extends ScratchReg{
    override def toString: String = "r10"
  }
  case object reg11 extends ScratchReg{
    override def toString: String = "r11"
  }

  sealed trait Suffix extends Operand
  case object LE extends Suffix {
    override def toString: String = "LE"
  }
  case object LT extends Suffix {
    override def toString: String = "LT"
  }
  case object GE extends Suffix {
    override def toString: String = "GE"
  }
  case object GT extends Suffix {
    override def toString: String = "GT"
  }
  case object CS extends Suffix {
    override def toString: String = "CS"
  }
  case object NE extends Suffix {
    override def toString: String = "NE"
  }
  case object EQ extends Suffix {
    override def toString: String = "EQ"
  }
  case object SB extends Suffix {
    override def toString: String = "SB"
  }
  case object VS extends Suffix {
    override def toString: String = "VS"
  }
  case object Base extends Suffix {
    override def toString: String = ""
  }

}
