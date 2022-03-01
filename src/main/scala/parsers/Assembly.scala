package parsers

object Assembly {

  import parsley.implicits.zipped.{Zipped2, Zipped3}

  sealed trait mnemonic
  //TODO: to string has conditional comma
  case class LDR(r: register, o2: operand) extends mnemonic {
    override def toString: String = "LDR " + r.toString + ", " + o2.toString
  }
  case class PUSH(r: register) extends mnemonic {
    override def toString: String = "PUSH{" + r.toString + "}"
  }
  case class POP(r: register) extends mnemonic {
    override def toString: String = "POP{" + r.toString + "}"
  }
  case class SUB(o1: operand, o2: operand, o3: operand) extends mnemonic {
    override def toString: String = {
      var ret: String = "SUB " + o1.toString + ", " + o2.toString
      o3 match{
        case nullOp() => ???
        case _ => ret + ", " + o3.toString
      }
      ret
    }
  }
  case class STR(rd: register, rn: register, offset: operand) extends mnemonic {
    override def toString: String = {
      var ret: String = "SUB " + rd.toString + ", [" + rn.toString
      offset match{
        case nullOp() => ???
        case _ => ret + ", " + offset.toString
      }
      ret += "]"
      ret
    }
  }
  case class ADD(o1: operand, o2: operand, o3: operand) extends mnemonic {
    override def toString: String = "ADD " + o1.toString + ", " + o2.toString + ", " + o3.toString
  }
  case class MOV(rd: register, o2: operand) extends mnemonic {
    override def toString: String = "MOV " + rd.toString + " ," + o2.toString
  }
  case class BL(label: String) extends mnemonic {
    override def toString: String = "BL " + label
  }
  case class CMP(r: register, o2: operand) extends mnemonic {
    override def toString: String = "CMP " + r.toString + ", " + o2.toString
  }
  case class LDRNE(r: register, o2: operand) extends mnemonic {
    override def toString: String = "LDRNE " + r.toString + ", " + o2.toString
  }
  case class LDREQ(r: register, o2: operand) extends mnemonic {
    override def toString: String = "LDREQ " + r.toString + ", " + o2.toString
  }
  case class BEQ(label: String) extends mnemonic {
    override def toString: String = "BEQ " + label
  }
  case class STRB(rd: register, rn: register, offset: operand) extends mnemonic {
    override def toString: String = {
      var ret: String = "SUB " + rd.toString + ", [" + rn.toString
      offset match {
        case nullOp() =>
        case _ => ret + ", " + offset.toString
      }
      ret += "]"
      ret
    }
  }
  case class LDRSB(r: register, o2: operand) extends mnemonic {
    override def toString: String = "LDRSB " + r.toString + ", " + o2.toString
  }
  case class AND(rd: register, rn: register, rm: register) extends mnemonic {
    override def toString: String = "AND " + rd.toString + ", " + rn.toString + ", " + rm.toString
  }
  case class ORR(rd: register, rn: register, rm: register) extends mnemonic {
    override def toString: String = "ORR " + rd.toString + ", " + rn.toString + ", " + rm.toString
  }
  case class MOVEQ(rd: register, o2: operand) extends mnemonic {
    override def toString: String = "MOVEQ " + rd.toString + " ," + o2.toString
  }
  case class MOVNE(rd: register, o2: operand) extends mnemonic {
    override def toString: String = "MOVNE " + rd.toString + " ," + o2.toString
  }
  case class MOVGT(rd: register, o2: operand) extends mnemonic {
    override def toString: String = "MOVGT " + rd.toString + " ," + o2.toString
  }
  case class MOVLE(rd: register, o2: operand) extends mnemonic {
    override def toString: String = "MOVLE " + rd.toString + " ," + o2.toString
  }
  case class MOVGE(rd: register, o2: operand) extends mnemonic {
    override def toString: String = "MOVGE " + rd.toString + " ," + o2.toString
  }
  case class MOVLT(rd: register, o2: operand) extends mnemonic {
    override def toString: String = "MOVLT " + rd.toString + " ," + o2.toString
  }
  case class BLVS(label: String) extends mnemonic {
    override def toString: String = "BLVS " + label
  }
  case class ADDS(rd: register, rn: register, rm: register) extends mnemonic {
    override def toString: String = "ADDS " + rd.toString + ", " + rn.toString + ", " + rm.toString
  }
  case class SUBS(rd: register, rn: register, rm: register) extends mnemonic {
    override def toString: String = "SUBS " + rd.toString + ", " + rn.toString + ", " + rm.toString
  }
  case class MULTS(rd: register, rn: register, rm: register) extends mnemonic {
    override def toString: String = "MULTS " + rd.toString + ", " + rn.toString + ", " + rm.toString
  }
  case class BLEQ(label: String) extends mnemonic {
    override def toString: String = "BLEQ " + label
  }
  case class EOR(rd: register, o2: operand, o3: operand) extends mnemonic {
    override def toString: String = "EOR " + rd.toString + ", " + o2.toString + ", " + o3.toString
  }
  case class RSBS(rd: register, o2: operand, o3: operand) extends mnemonic {
    override def toString: String = "RSBS " + rd.toString + ", " + o2.toString + ", " + o3.toString
  }

  sealed trait operand
  case class imm(i: Int) extends operand {
    override def toString: String = "#" + i.toString
  }
  case class label(l: String) extends operand {
    override def toString: String = "=" + label
  }
  case class nullOp() extends operand {
    override def toString: String = ""
  }

  sealed trait register extends operand
  case class retReg() extends register{
    override def toString: String = "r0"
  }
  case class sp() extends register{
    override def toString: String = "r13"
  }
  case class linkReg() extends register{
    override def toString: String = "r14"
  }
  case class pc() extends register{
    override def toString: String = "r15"
  }
  case class reg(num: Int) extends register{
    override def toString: String = "r" + num.toString
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

  object retReg {
    def apply(): retReg = new retReg
  }

  object sp {
    def apply(): sp = new sp
  }

  object linkReg {
    def apply(): linkReg = new linkReg
  }

  object pc {
    def apply(): pc = new pc
  }

  object LDR {
    def apply(r: register, o2: operand): LDR = new LDR(r, o2)
  }

  object PUSH {
    def apply(r: register): PUSH = new PUSH(r)
  }

  object POP{
    def apply(r: register): POP = new POP(r)
  }

  object SUB{
    def apply(o1: operand, o2: operand, o3: operand): SUB = new SUB(o1, o2, o3)
  }

  object STR{
    def apply(rd: register, rn: register, offset: operand): STR = new STR(rd, rn, offset)
  }

  object ADD{
    def apply(o1: operand, o2: operand, o3: operand): ADD = new ADD(o1, o2, o3)
  }

  object MOV{
    def apply(rd: register, o2: operand): MOV = new MOV(rd, o2)
  }

  object BL{
    def apply(label: String): BL = new BL(label)
  }

  object CMP{
    def apply(r: register, o2: operand): CMP = new CMP(r, o2)
  }

  object LDRNE{
    def apply(r: register, o2: operand): LDRNE = new LDRNE(r,o2)
  }

  object LDREQ{
    def apply(r: register, o2: operand): LDREQ = new LDREQ(r, o2)
  }

  object BEQ{
    def apply(label: String): BEQ = new BEQ(label)
  }

  object STRB{
    def apply(rd: register, rn: register, offset: operand): STRB = new STRB(rd, rn, offset)
  }

  object LDRSB{
    def apply(r: register, o2: operand): LDRSB = new LDRSB(r, o2)
  }

  object AND{
    def apply(rd: register, rn: register, rm: register): AND = new AND(rd, rn, rm)
  }

  object ORR{
    def apply(rd: register, rn: register, rm: register): ORR = new ORR(rd, rn, rm)
  }

  object MOVEQ{
    def apply(rd: register, o2: operand): MOVEQ = new MOVEQ(rd, o2)
  }

  object MOVNE{
    def apply(rd: register, o2: operand): MOVNE = new MOVNE(rd, o2)
  }

  object MOVGT{
    def apply(rd: register, o2: operand): MOVGT = new MOVGT(rd, o2)
  }

  object MOVLE{
    def apply(rd: register, o2: operand): MOVLE = new MOVLE(rd, o2)
  }

  object MOVGE{
    def apply(rd: register, o2: operand): MOVGE = new MOVGE(rd, o2)
  }

  object MOVLT{
    def apply(rd: register, o2: operand): MOVLT = new MOVLT(rd, o2)
  }

  object BLVS{
    def apply(label: String): BLVS = new BLVS(label)
  }

  object ADDS{
    def apply(rd: register, rn: register, rm: register): ADDS = new ADDS(rd, rn, rm)
  }

  object SUBS{
    def apply(rd: register, rn: register, rm: register): SUBS = new SUBS(rd, rn, rm)
  }

  object MULTS{
    def apply(rd: register, rn: register, rm: register): MULTS = new MULTS(rd, rn, rm)
  }

  object BLEQ{
    def apply(label: String): BLEQ = new BLEQ(label)
  }

  object EOR{
    def apply(rd: register, o2: operand, o3: operand): EOR = new EOR(rd, o2, o3)
  }

  object RSBS{
    def apply(rd: register, o2: operand, o3: operand): RSBS = new RSBS(rd, o2, o3)
  }

}
