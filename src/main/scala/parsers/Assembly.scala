package parsers

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
    override def toString: String = "LDR" + suffix.toString + " " + r.toString + ", " + o2.toString
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

  case class STR(rd: Register, rn: Register) extends Mnemonic {
    override def toString: String = {
      "SUB " + rd.toString + ", " + rn.toString
    }
  }
  case class ADD(o1: Operand, o2: Operand, o3: Operand) extends Mnemonic {
    override def toString: String = "ADD " + o1.toString + ", " + o2.toString + ", " + o3.toString
  }
  case class MOV(rd: Register, o2: Operand, suffix: Suffix) extends Mnemonic {
    override def toString: String = "MOV" + suffix.toString + " " + rd.toString + ", " + o2.toString
  }
  case class BL(label: String) extends Mnemonic {
    override def toString: String = "BL " + label
  }
  case class CMP(r: Register, o2: Operand) extends Mnemonic {
    override def toString: String = "CMP " + r.toString + ", " + o2.toString
  }
  case class BEQ(label: String) extends Mnemonic {
    override def toString: String = "BEQ " + label
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
  case class BLVS(label: String) extends Mnemonic {
    override def toString: String = "BLVS " + label
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
  case class BLEQ(label: String) extends Mnemonic {
    override def toString: String = "BLEQ " + label
  }
  case class EOR(rd: Register, o2: Operand, o3: Operand) extends Mnemonic {
    override def toString: String = "EOR " + rd.toString + ", " + o2.toString + ", " + o3.toString
  }
  case class RSBS(rd: Register, o2: Operand, o3: Operand) extends Mnemonic {
    override def toString: String = "RSBS " + rd.toString + ", " + o2.toString + ", " + o3.toString
  }
  case class B(label: String) extends Mnemonic {
    override def toString: String = "B" + label
  }

  sealed trait Operand
  case class imm(i: Int) extends Operand {
    override def toString: String = "#" + i.toString
  }
  case class immc(c: Char) extends Operand {
    override def toString: String = "#'" + c.toString + "'"
  }
  case class label(l: String) extends Operand {
    override def toString: String = "=" + l
  }
  case class regShift(reg: Register, shift: Int) extends Register {
    override def toString: String = "[" + reg.toString + ", #" + shift + "]"
  }
  case class regVal(reg: Register) extends Register {
    override def toString: String = "[" + reg.toString + "]"
  }
  case object nullOp extends Operand {
    override def toString: String = ""
  }

  sealed trait Register extends Operand
  case object RetReg extends Register{
    override def toString: String = "r0"
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
  case class reg(num: Int) extends Register{
    override def toString: String = "r" + num.toString
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
  case object NE extends Suffix {
    override def toString: String = "NE"
  }
  case object EQ extends Suffix {
    override def toString: String = "EQ"
  }
  case object SB extends Suffix {
    override def toString: String = "SB"
  }
  case object Base extends Suffix { // todo: is there any way of overloading function definitions instead?
    override def toString: String = ""
  }

}
