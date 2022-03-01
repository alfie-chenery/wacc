package parsers

// TODO remove use of nullOp
object Assembly {

  case class funcName(name: String){
    override def toString: String = name + ":"
  }

  sealed trait Mnemonic
  //TODO: to string has conditional comma
  case class LDR(r: Register, o2: Operand, suffix: Suffix) extends Mnemonic {
    override def toString: String = "LDR" + suffix.toString + r.toString + ", " + o2.toString
  }
  case class PUSH(r: Register) extends Mnemonic {
    override def toString: String = "PUSH{" + r.toString + "}"
  }
  case class POP(r: Register) extends Mnemonic {
    override def toString: String = "POP{" + r.toString + "}"
  }
  case class SUB(o1: Operand, o2: Operand, o3: Operand) extends Mnemonic {
    override def toString: String = {
      var ret: String = "SUB " + o1.toString + ", " + o2.toString
      o3 match{
        case nullOp =>
        case _ => ret + ", " + o3.toString
      }
      ret
    }
  }
  case class STR(rd: Register, rn: Register, offset: Operand) extends Mnemonic {
    override def toString: String = {
      var ret: String = "SUB " + rd.toString + ", [" + rn.toString
      offset match{
        case nullOp =>
        case _ => ret + ", " + offset.toString
      }
      ret += "]"
      ret
    }
  }
  case class ADD(o1: Operand, o2: Operand, o3: Operand) extends Mnemonic {
    override def toString: String = "ADD " + o1.toString + ", " + o2.toString + ", " + o3.toString
  }
  case class MOV(rd: Register, o2: Operand, suffix: Suffix) extends Mnemonic {
    override def toString: String = "MOV" + suffix.toString + " " + rd.toString + " ," + o2.toString
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
  case class STRB(rd: Register, rn: Register, offset: Operand) extends Mnemonic {
    override def toString: String = {
      var ret: String = "SUB " + rd.toString + ", [" + rn.toString
      offset match {
        case nullOp =>
        case _ => ret + ", " + offset.toString
      }
      ret += "]"
      ret
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
  case class label(l: String) extends Operand {
    override def toString: String = "=" + label
  }
  case object nullOp extends Operand {
    override def toString: String = ""
  }

  sealed trait Register extends Operand
  case object RetReg extends Register{
    override def toString: String = "r0"
  }
  case object SP extends Register{
    override def toString: String = "r13"
  }
  case object LinkReg extends Register{
    override def toString: String = "r14"
  }
  case object PC extends Register{
    override def toString: String = "r15"
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
  case object Base extends Suffix {
    override def toString: String = ""
  }

  object imm {
    def apply(i: Int): imm = new imm(i)
  }

  object label {
    def apply(l: String): label = new label(l)
  }

  object reg {
    def apply(num: Int): reg = new reg(num)
  }

  object LDR {
    def apply(r: Register, o2: Operand, suffix: Suffix): LDR = new LDR(r, o2, suffix)
  }

  object PUSH {
    def apply(r: Register): PUSH = new PUSH(r)
  }

  object POP{
    def apply(r: Register): POP = new POP(r)
  }

  object SUB{
    def apply(o1: Operand, o2: Operand, o3: Operand): SUB = new SUB(o1, o2, o3)
  }

  object STR{
    def apply(rd: Register, rn: Register, offset: Operand): STR = new STR(rd, rn, offset)
  }

  object ADD{
    def apply(o1: Operand, o2: Operand, o3: Operand): ADD = new ADD(o1, o2, o3)
  }

  object MOV{
    def apply(rd: Register, o2: Operand, suffix: Suffix): MOV = new MOV(rd, o2, suffix)
  }

  object BL{
    def apply(label: String): BL = new BL(label)
  }

  object CMP{
    def apply(r: Register, o2: Operand): CMP = new CMP(r, o2)
  }

  object BEQ{
    def apply(label: String): BEQ = new BEQ(label)
  }

  object STRB{
    def apply(rd: Register, rn: Register, offset: Operand): STRB = new STRB(rd, rn, offset)
  }


  object AND{
    def apply(rd: Register, rn: Register, rm: Register): AND = new AND(rd, rn, rm)
  }

  object ORR{
    def apply(rd: Register, rn: Register, rm: Register): ORR = new ORR(rd, rn, rm)
  }

  object BLVS{
    def apply(label: String): BLVS = new BLVS(label)
  }

  object ADDS{
    def apply(rd: Register, rn: Register, rm: Register): ADDS = new ADDS(rd, rn, rm)
  }

  object SUBS{
    def apply(rd: Register, rn: Register, rm: Register): SUBS = new SUBS(rd, rn, rm)
  }

  object MULTS{
    def apply(rd: Register, rn: Register, rm: Register): MULTS = new MULTS(rd, rn, rm)
  }

  object BLEQ{
    def apply(label: String): BLEQ = new BLEQ(label)
  }

  object EOR{
    def apply(rd: Register, o2: Operand, o3: Operand): EOR = new EOR(rd, o2, o3)
  }

  object RSBS{
    def apply(rd: Register, o2: Operand, o3: Operand): RSBS = new RSBS(rd, o2, o3)
  }


}
