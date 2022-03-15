package parsers

import parsers.CodeGen._
import parsers.Assembly._
import parsers.RegisterAllocator._
import parsers.preDefinedFuncs._

class MathLib {

  //TODO: add references to branched functions

  val mathFuncs = List("acos", "asin", "atan", "atan2", "cos", "cosh", "sin", "sinh",
    "tanh", "exp", "frexp", "ldexp", "log", "log10", "modf", "pow",
    "sqrt", "ceil", "fabs", "floor", "fmod")

  def asin(ra: RegisterAllocator) {
    var r = ra.next
    val condLabel = nextBranchIndex
    val bodyLabel = nextBranchIndex
    intOverflow()
    divByZeroError()
    runtimeError()
    printString()
    pow(ra)
    fact(ra)
    if (!labels.contains("asin")){
      val r = ra.next
      labels("asin") = List(
        PUSH(LinkReg),
        SUB(SP, SP, imm(12)),
        LDR(r, regShift(SP, 16, false), Base),
        STR(r, regShift(SP, 8, false)),
        LDR(r, imm(-1), Base),
        STR(r, regShift(SP, 4, false)),
        LDR(r, imm(3), Base),
        STR(r, regVal(SP)),
        B(condLabel, Base)
      )
      val r1 = ra.next
      labels(condLabel) = List(
        LDR(r, regVal(SP), Base),
        LDR(r1, imm(100), Base),
        CMP(r, r1),
        MOV(r, imm(1), LE),
        MOV(r, imm(0), GT),
        CMP(r, imm(1)),
        B(bodyLabel, EQ),
        LDR(r, regShift(SP, 8, false), Base),
        MOV(RetReg, r, Base),
        ADD(SP, SP, imm(12)),
        POP(PC),
        POP(PC),
        LTORG
      )
      val r2 = ra.next
      labels(bodyLabel) = List(
        SUB(SP, SP, imm(16)),
        LDR(r, regShift(SP, 16, false), Base),
        STR(r, regShift(SP, -4, true)),
        LDR(r, regShift(SP, 36, false), Base),
        STR(r, regShift(SP, -4, true)),
        BL("pow", Base),
        ADD(SP, SP, imm(8)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 12, false)),
        LDR(r, regShift(SP, 16, false), Base),
        STR(r, regShift(SP, -4, true)),
        BL("fact", Base),
        ADD(SP, SP, imm(4)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 8, false)),
        LDR(r, regShift(SP, 12, false), Base),
        LDR(r1, regShift(SP, 8, false), Base),
        MOV(RetReg, r, Base),
        MOV(reg(1), r1, Base),
        BL("p_check_divide_by_zero", Base),
        BL("__aeabi_idiv", Base),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 4, false)),
        LDR(r, regShift(SP, 20, false), Base),
        LDR(r1, regShift(SP, 4, false), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regVal(SP)),
        LDR(r, regShift(SP, 24, false), Base),
        LDR(r1, regVal(SP), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", VS),
        STR(r, regShift(SP, 24, false)),
        LDR(r, regShift(SP, 20, false), Base),
        LDR(r1, imm(-1), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 20, false)),
        LDR(r, regShift(SP, 16, false), Base),
        LDR(r1, imm(2), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 16, false)),
        ADD(SP, SP, imm(16))
      )
    }
  }

  def acos(ra: RegisterAllocator): Unit ={
    divByZeroError()
    intOverflow()
    runtimeError()
    printString()
    pow(ra)
    fact(ra)
    val condLabel = nextBranchIndex
    val bodyLabel = nextBranchIndex
    if (!labels.contains("acos")){
      val r = ra.next
      labels("acos") = List(
        PUSH(LinkReg),
        SUB(SP, SP, imm(12)),
        LDR(r, imm(1), Base),
        STR(r, regShift(SP, 8, false)),
        LDR(r, imm(-1), Base),
        STR(r, regShift(SP, 4, false)),
        LDR(r, imm(2), Base),
        STR(r, regVal(SP)),
        B(condLabel, Base)
      )
      val r1 = ra.next
      labels(condLabel) = List(
        LDR(r, regVal(SP), Base),
        LDR(r1, imm(100), Base),
        CMP(r, r1),
        MOV(r, imm(1), LE),
        MOV(r, imm(0), GT),
        CMP(r, imm(1)),
        B(bodyLabel, EQ),
        LDR(r, regShift(SP, 8, false), Base),
        MOV(RetReg, r, Base),
        ADD(SP, SP, imm(12)),
        POP(PC),
        POP(PC),
        LTORG
      )
      val r2 = ra.next
      labels(bodyLabel) = List(
        SUB(SP, SP, imm(16)),
        LDR(r, regShift(SP, 16, false), Base),
        STR(r, regShift(SP, -4, true)),
        LDR(r, regShift(SP, 36, false), Base),
        STR(r, regShift(SP, -4, true)),
        BL("pow", Base),
        ADD(SP, SP, imm(8)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 12, false)),
        LDR(r, regShift(SP, 16, false), Base),
        STR(r, regShift(SP, -4, true)),
        BL("fact", Base),
        ADD(SP, SP, imm(4)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 8, false)),
        LDR(r, regShift(SP, 12, false), Base),
        LDR(r1, regShift(SP, 8, false), Base),
        MOV(RetReg, r, Base),
        MOV(reg(1), r1, Base),
        BL("p_check_divide_by_zero", Base),
        BL("__aeabi_idiv", Base),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 4, false)),
        LDR(r, regShift(SP, 20, false), Base),
        LDR(r1, regShift(SP, 4, false), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regVal(SP)),
        LDR(r, regShift(SP, 24, false), Base),
        LDR(r1, regVal(SP), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", VS),
        STR(r, regShift(SP, 24, false)),
        LDR(r, regShift(SP, 20, false), Base),
        LDR(r1, imm(-1), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 20, false)),
        LDR(r, regShift(SP, 16, false), Base),
        LDR(r1, imm(2), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 20, false)),
        ADD(SP, SP, imm(16))
      )
    }
  }

  def atan(ra:RegisterAllocator): Unit ={
    printString()
    runtimeError()
    divByZeroError()
    acos(ra)
    asin(ra)
    if(!labels.contains("atan")){
      val r = ra.next
      val r1 = ra.next
      labels("atan") = List(
        PUSH(LinkReg),
        SUB(SP, SP, imm(12)),
        LDR(r, regShift(SP, 16, false), Base),
        STR(r, regShift(SP, -4, true)),
        BL("sin", Base),
        ADD(SP, SP, imm(4)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP,8, false)),
        LDR(r, regShift(SP, 16, false), Base),
        STR(r, regShift(SP, -4, true)),
        BL("cos", Base),
        ADD(SP, SP, imm(4)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 4, false)),
        LDR(r, regShift(SP, 8, false), Base),
        LDR(r1, regShift(SP, 4, false), Base),
        MOV(RetReg, r, Base),
        MOV(reg(1), r1, Base),
        BL("p_check_divide_by_zero", Base),
        BL("__aeabi_idiv", Base),
        MOV(r, RetReg, Base),
        STR(r, regVal(SP)),
        LDR(r, regVal(SP), Base),
        MOV(RetReg, r, Base),
        ADD(SP, SP, imm(12)),
        POP(PC),
        POP(PC),
        LTORG
      )
    }
  }

  def pow(ra: RegisterAllocator): Unit ={
    if(!labels.contains("pow")){
      printString()
      runtimeError()
      intOverflow()
      val condLabel = nextBranchIndex
      val bodyLabel = nextBranchIndex
      val r = ra.next
      labels("pow") = List(
        PUSH(LinkReg),
        SUB(SP, SP, imm(8)),
        LDR(r, imm(1), Base),
        STR(r, regShift(SP, 4, false)),
        LDR(r, imm(0), Base),
        STR(r, regVal(SP)),
        B(condLabel, Base)
      )
      val r1 = ra.next
      labels(condLabel) = List(
        LDR(r, regVal(SP), Base),
        LDR(r1, regShift(SP, 16, false), Base),
        CMP(r, r1),
        MOV(r, imm(1), LT),
        MOV(r, imm(0), GE),
        CMP(r, imm(1)),
        B(bodyLabel, EQ),
        LDR(r, regShift(SP, 4, false), Base),
        MOV(RetReg, r, Base),
        ADD(SP, SP, imm(8)),
        POP(PC),
        POP(PC),
        LTORG
      )
      labels(bodyLabel) = List(
        LDR(r, regShift(SP, 4, false), Base),
        LDR(r1, regShift(SP, 12, false), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 4, false)),
        LDR(r, regVal(SP), Base),
        LDR(r1, imm(1), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", VS),
        STR(r, regVal(SP))
      )
    }
  }

  def fact(ra: RegisterAllocator): Unit ={
    printString()
    runtimeError()
    intOverflow()
    val condLabel = nextBranchIndex
    val bodyLabel = nextBranchIndex
    if(!labels.contains("fact")){
      val r = ra.next
      labels("fact") = List(
        PUSH(LinkReg),
        SUB(SP, SP, imm(8)),
        LDR(r, imm(1), Base),
        STR(r, regShift(SP, 4, false)),
        LDR(r, imm(1), Base),
        STR(r, regVal(SP)),
        B(condLabel, Base)
      )
      val r1 = ra.next
      labels(condLabel) = List(
        LDR(r, regVal(SP), Base),
        LDR(r1, regShift(SP, 12, false), Base),
        CMP(r, r1),
        MOV(r, imm(1), LE),
        MOV(r, imm(0), GT),
        CMP(r, imm(1)),
        B(bodyLabel, EQ),
        LDR(r, regShift(SP, 4, false), Base),
        MOV(RetReg, r, Base),
        ADD(SP, SP, imm(8)),
        POP(PC),
        POP(PC),
        LTORG
      )
      labels(bodyLabel) = List(
        LDR(r, regShift(SP, 4, false), Base),
        LDR(r1, regVal(SP), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 4, false)),
        LDR(r, regVal(SP), Base),
        LDR(r1, imm(1), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", VS),
        STR(r, regVal(SP))
      )
    }
  }

}
