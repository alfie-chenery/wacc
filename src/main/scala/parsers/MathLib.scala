package parsers

import parsers.CodeGen._
import parsers.Assembly._
import parsers.RegisterAllocator._
import parsers.preDefinedFuncs._
import parsers.Ast._
import parsers.SemanticPass.checkExprType

import scala.collection.mutable.ListBuffer

object MathLib {

  def sin(ra: RegisterAllocator, va: VfpAllocator): Unit = {
    val condLabel = nextBranchIndex
    val bodyLabel = nextBranchIndex
    intOverflow()
    divByZeroError()
    runtimeError()
    printString()
    pow(ra, va)

    if (!labels.contains("sin")){
      val r = ra.next
      val funcCode = new ListBuffer[Mnemonic]()
      funcCode += PUSH(LinkReg)
      funcCode += SUB(SP, SP, imm(12))
      funcCode += LDR(r, regShift(SP, 16, update = false), Base)
      funcCode += STR(r, regShift(SP, 8, update = false))
      funcCode += LDR(r, imm(-1), Base)
      funcCode += STR(r, regShift(SP, 4, update = false))
      funcCode += LDR(r, imm(3), Base)
      funcCode += STR(r, regVal(SP))
      funcCode += B(condLabel, Base)

      val r1 = ra.next

      funcCode += funcName(bodyLabel)
      funcCode += SUB(SP, SP, imm(16))
      funcCode += LDR(r, regShift(SP, 16, update = false), Base)
      funcCode += STR(r, regShift(SP, -4, update = true))
      funcCode += LDR(r, regShift(SP, 36, update = false), Base)
      funcCode += STR(r, regShift(SP, -4, update = true))
      funcCode += BL("pow", Base)
      funcCode += ADD(SP, SP, imm(8))
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 12, update = false))
      funcCode += LDR(r, regShift(SP, 16, update = false), Base)
      funcCode += STR(r, regShift(SP, -4, update = true))
      funcCode += BL("fact", Base)
      funcCode += ADD(SP, SP, imm(4))
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 8, update = false))
      funcCode += LDR(r, regShift(SP, 12, update = false), Base)
      funcCode += LDR(r1, regShift(SP, 8, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += MOV(reg1, r1, Base)
      funcCode += BL("p_check_divide_by_zero", Base)
      funcCode += BL("__aeabi_idiv", Base)
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 4, update = false))
      funcCode += LDR(r, regShift(SP, 20, update = false), Base)
      funcCode += LDR(r1, regShift(SP, 4, update = false), Base)
      funcCode += SMULL(r, r1, r, r1)
      funcCode += CMP(r1, asr(r, 31))
      funcCode += BL("p_throw_overflow_error", NE)
      funcCode += STR(r, regVal(SP))
      funcCode += LDR(r, regShift(SP, 24, update = false), Base)
      funcCode += LDR(r1, regVal(SP), Base)
      funcCode += ADDS(r, r, r1)
      funcCode += BL("p_throw_overflow_error", VS)
      funcCode += STR(r, regShift(SP, 24, update = false))
      funcCode += LDR(r, regShift(SP, 20, update = false), Base)
      funcCode += LDR(r1, imm(-1), Base)
      funcCode += SMULL(r, r1, r, r1)
      funcCode += CMP(r1, asr(r, 31))
      funcCode += BL("p_throw_overflow_error", NE)
      funcCode += STR(r, regShift(SP, 20, update = false))
      funcCode += LDR(r, regShift(SP, 16, update = false), Base)
      funcCode += LDR(r1, imm(2), Base)
      funcCode += ADDS(r, r, r1)
      funcCode += BL("p_throw_overflow_error", NE)
      funcCode += STR(r, regShift(SP, 16, update = false))
      funcCode += ADD(SP, SP, imm(16))
      funcCode += funcName(condLabel)
      funcCode += LDR(r, regVal(SP), Base)
      funcCode += LDR(r1, imm(100), Base)
      funcCode += CMP(r, r1)
      funcCode += MOV(r, imm(1), LE)
      funcCode += MOV(r, imm(0), GT)
      funcCode += CMP(r, imm(1))
      funcCode += B(bodyLabel, EQ)
      funcCode += LDR(r, regShift(SP, 8, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += ADD(SP, SP, imm(12))
      funcCode += POP(PC)
      funcCode += POP(PC)
      funcCode += LTORG

      labels("sin") = funcCode.toList
    }
  }

  def cos(ra: RegisterAllocator, va: VfpAllocator): Unit ={
    divByZeroError()
    intOverflow()
    runtimeError()
    printString()
    pow(ra, va)
    fact(ra, va)

    val condLabel = nextBranchIndex
    val bodyLabel = nextBranchIndex
    if (!labels.contains("cos")){
      val r = ra.next
      val funcCode = new ListBuffer[Mnemonic]()
      funcCode += PUSH(LinkReg)
      funcCode += SUB(SP, SP, imm(12))
      funcCode += LDR(r, imm(1), Base)
      funcCode += STR(r, regShift(SP, 8, update = false))
      funcCode += LDR(r, imm(-1), Base)
      funcCode += STR(r, regShift(SP, 4, update = false))
      funcCode += LDR(r, imm(2), Base)
      funcCode += STR(r, regVal(SP))
      funcCode += B(condLabel, Base)

      val r1 = ra.next
      val r2 = ra.next
      funcCode += funcName(bodyLabel)
      funcCode += SUB(SP, SP, imm(16))
      funcCode += LDR(r, regShift(SP, 16, update = false), Base)
      funcCode += STR(r, regShift(SP, -4, update = true))
      funcCode += LDR(r, regShift(SP, 36, update = false), Base)
      funcCode += STR(r, regShift(SP, -4, update = true))
      funcCode += BL("pow", Base)
      funcCode += ADD(SP, SP, imm(8))
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 12, update = false))
      funcCode += LDR(r, regShift(SP, 16, update = false), Base)
      funcCode += STR(r, regShift(SP, -4, update = true))
      funcCode += BL("fact", Base)
      funcCode += ADD(SP, SP, imm(4))
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 8, update = false))
      funcCode += LDR(r, regShift(SP, 12, update = false), Base)
      funcCode += LDR(r1, regShift(SP, 8, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += MOV(reg1, r1, Base)
      funcCode += BL("p_check_divide_by_zero", Base)
      funcCode += BL("__aeabi_idiv", Base)
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 4, update = false))
      funcCode += LDR(r, regShift(SP, 20, update = false), Base)
      funcCode += LDR(r1, regShift(SP, 4, update = false), Base)
      funcCode += SMULL(r, r1, r, r1)
      funcCode += CMP(r1, asr(r, 31))
      funcCode += BL("p_throw_overflow_error", NE)
      funcCode += STR(r, regVal(SP))
      funcCode += LDR(r, regShift(SP, 24, update = false), Base)
      funcCode += LDR(r1, regVal(SP), Base)
      funcCode += ADDS(r, r, r1)
      funcCode += BL("p_throw_overflow_error", VS)
      funcCode += STR(r, regShift(SP, 24, update = false))
      funcCode += LDR(r, regShift(SP, 20, update = false), Base)
      funcCode += LDR(r1, imm(-1), Base)
      funcCode += SMULL(r, r1, r, r1)
      funcCode += CMP(r1, asr(r, 31))
      funcCode += BL("p_throw_overflow_error", NE)
      funcCode += STR(r, regShift(SP, 20, update = false))
      funcCode += LDR(r, regShift(SP, 16, update = false), Base)
      funcCode += LDR(r1, imm(2), Base)
      funcCode += ADDS(r, r, r1)
      funcCode += BL("p_throw_overflow_error", NE)
      funcCode += STR(r, regShift(SP, 20, update = false))
      funcCode += ADD(SP, SP, imm(16))

      funcCode += funcName(condLabel)
      funcCode += LDR(r, regVal(SP), Base)
      funcCode += LDR(r1, imm(100), Base)
      funcCode += CMP(r, r1)
      funcCode += MOV(r, imm(1), LE)
      funcCode += MOV(r, imm(0), GT)
      funcCode += CMP(r, imm(1))
      funcCode += B(bodyLabel, EQ)
      funcCode += LDR(r, regShift(SP, 8, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += ADD(SP, SP, imm(12))
      funcCode += POP(PC)
      funcCode += POP(PC)
      funcCode += LTORG

      labels("cos") = funcCode.toList
    }
  }

  def tan(ra: RegisterAllocator, va: VfpAllocator): Unit ={
    printString()
    runtimeError()
    divByZeroError()
    cos(ra, va)
    sin(ra, va)
    if(!labels.contains("tan")){
      val r = ra.next
      val r1 = ra.next
      val funcCode = new ListBuffer[Mnemonic]()

      funcCode += PUSH(LinkReg)
      funcCode += SUB(SP, SP, imm(12))
      funcCode += LDR(r, regShift(SP, 16, update = false), Base)
      funcCode += STR(r, regShift(SP, -4, update = true))
      funcCode += BL("sin", Base)
      funcCode += ADD(SP, SP, imm(4))
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP,8, update = false))
      funcCode += LDR(r, regShift(SP, 16, update = false), Base)
      funcCode += STR(r, regShift(SP, -4, update = true))
      funcCode += BL("cos", Base)
      funcCode += ADD(SP, SP, imm(4))
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 4, update = false))
      funcCode += LDR(r, regShift(SP, 8, update = false), Base)
      funcCode += LDR(r1, regShift(SP, 4, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += MOV(reg1, r1, Base)
      funcCode += BL("p_check_divide_by_zero", Base)
      funcCode += BL("__aeabi_idiv", Base)
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regVal(SP))
      funcCode += LDR(r, regVal(SP), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += ADD(SP, SP, imm(12))
      funcCode += POP(PC)
      funcCode += POP(PC)
      funcCode += LTORG

      labels("tan") = funcCode.toList
    }
  }

  def pow(ra: RegisterAllocator, va: VfpAllocator): Unit ={
    if(!labels.contains("pow")){
      printString()
      runtimeError()
      intOverflow()
      val condLabel = nextBranchIndex
      val bodyLabel = nextBranchIndex
      val r = ra.next
      val r1 = ra.next

      val funcCode = new ListBuffer[Mnemonic]()
      funcCode += PUSH(LinkReg)
      funcCode += SUB(SP, SP, imm(8))
      LDR(r, imm(1), Base)
      STR(r, regShift(SP, 4, update = false))
      LDR(r, imm(0), Base)
      STR(r, regVal(SP))
      B(condLabel, Base)

      funcCode += funcName(bodyLabel)
      funcCode += LDR(r, regShift(SP, 4, update = false), Base)
      funcCode += LDR(r1, regShift(SP, 12, update = false), Base)
      val v = va.next
      val v1 = va.next
      funcCode += FMSR(v, r)
      funcCode += FMSR(v1, r1)
      funcCode += FMUL(v, v1, v)
      funcCode += FCMP(v1, asr(v, 31))
      funcCode += FMRS(r1, v1)
      funcCode += FMRS(r, v)
      funcCode += BL("p_throw_overflow_error", NE)
      funcCode += STR(r, regShift(SP, 4, update = false))
      funcCode += LDR(r, regVal(SP), Base)
      funcCode += LDR(r1, imm(1), Base)
      funcCode += ADDS(r, r, r1)

      funcCode += funcName(condLabel)
      funcCode += LDR(r, regVal(SP), Base)
      funcCode += LDR(r1, regShift(SP, 16, update = false), Base)
      funcCode += CMP(r, r1)
      funcCode += MOV(r, imm(1), LT)
      funcCode += MOV(r, imm(0), GE)
      funcCode += CMP(r, imm(1))
      funcCode += B(bodyLabel, EQ)
      funcCode += LDR(r, regShift(SP, 4, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += ADD(SP, SP, imm(8))
      funcCode += POP(PC)
      funcCode += POP(PC)
      funcCode += LTORG

      labels("pow") = funcCode.toList
    }
  }

  def fact(ra: RegisterAllocator, va: VfpAllocator): Unit ={
    printString()
    runtimeError()
    intOverflow()
    val condLabel = nextBranchIndex
    val bodyLabel = nextBranchIndex
    if(!labels.contains("fact")){
      val r = ra.next

      val funcCode = new ListBuffer[Mnemonic]

      funcCode += PUSH(LinkReg)
      funcCode += SUB(SP, SP, imm(8))
      funcCode += LDR(r, imm(1), Base)
      funcCode += STR(r, regShift(SP, 4, update = false))
      funcCode += LDR(r, imm(1), Base)
      funcCode += STR(r, regVal(SP))
      funcCode += B(condLabel, Base)

      val r1 = ra.next

      funcCode += funcName(bodyLabel)
      funcCode += LDR(r, regShift(SP, 4, update = false), Base)
      funcCode += LDR(r1, regVal(SP), Base)
      funcCode += SMULL(r, r1, r, r1)
      funcCode += CMP(r1, asr(r, 31))
      funcCode += BL("p_throw_overflow_error", NE)
      funcCode += STR(r, regShift(SP, 4, update = false))
      funcCode += LDR(r, regVal(SP), Base)
      funcCode += LDR(r1, imm(1), Base)
      funcCode += ADDS(r, r, r1)
      funcCode += BL("p_throw_overflow_error", VS)
      funcCode += STR(r, regVal(SP))

      funcCode += funcName(condLabel)
      funcCode += LDR(r, regVal(SP), Base)
      funcCode += LDR(r1, regShift(SP, 12, update = false), Base)
      funcCode += CMP(r, r1)
      funcCode += MOV(r, imm(1), LE)
      funcCode += MOV(r, imm(0), GT)
      funcCode += CMP(r, imm(1))
      funcCode += B(bodyLabel, EQ)
      funcCode += LDR(r, regShift(SP, 4, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += ADD(SP, SP, imm(8))
      funcCode += POP(PC)
      funcCode += POP(PC)
      funcCode += LTORG

      labels("fact") = funcCode.toList
    }
  }

  def fabs(ra: RegisterAllocator, va: VfpAllocator): Unit ={
    if(!labels.contains("fabs")){
      intOverflow()
      runtimeError()
      printString()
      val cond1Label = nextBranchIndex
      val cond2Label = nextBranchIndex
      val r = ra.next
      val r1 = ra.next

      val funcCode = new ListBuffer[Mnemonic]

      funcCode += PUSH(LinkReg)
      funcCode += LDR(r, regShift(SP, 4, update = false), Base)
      funcCode += LDR(r1, imm(0), Base)
      funcCode += CMP(r, r1)
      funcCode += MOV(r, imm(1), LT)
      funcCode += MOV(r, imm(0), GE)
      funcCode += CMP(r, imm(0))
      funcCode += B(cond1Label, EQ)
      funcCode += LDR(r, regShift(SP, 4, update = false), Base)
      funcCode += LDR(r1, imm(-1), Base)
      funcCode += SMULL(r, r1, r, r1)
      funcCode += CMP(r1, asr(r, 31))
      funcCode += BL("p_throw_overflow_error", NE)
      funcCode += STR(r, regShift(SP, 4, update = false))
      funcCode += LDR(r, regShift(SP, 4, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += POP(PC)
      funcCode += B(cond2Label, Base)

      funcCode += funcName(cond1Label)
      funcCode += LDR(r, regShift(SP, 4, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += POP(PC)

      funcCode += funcName(cond2Label)
      funcCode += POP(PC)
      funcCode += LTORG

      labels("fabs") = funcCode.toList
    }
  }

  def sqrt(ra: RegisterAllocator, va: VfpAllocator): Unit ={
    if(!labels.contains("sqrt")){
      printString()
      runtimeError()
      intOverflow()
      divByZeroError()
      val condLabel = nextBranchIndex
      val bodyLabel = nextBranchIndex
      val r = ra.next
      val r1 = ra.next
      val funcCode = new ListBuffer[Mnemonic]

      funcCode += PUSH(LinkReg)
      funcCode += SUB(SP, SP, imm(20))
      funcCode += LDR(r,regShift(SP, 24, update = false), Base)
      funcCode += LDR(r1, imm(2), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += MOV(reg1, r1, Base)
      funcCode += BL("p_check_divide_by_zero", Base)
      funcCode += BL("__aeabi_idiv", Base)
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 16, update = false))
      funcCode += LDR(r, regShift(SP, 16, update = false), Base)
      funcCode += STR(r, regShift(SP, 12, update = false))
      funcCode += LDR(r, regShift(SP, 24, update = false), Base)
      funcCode += LDR(r1, regShift(SP, 12, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += MOV(reg1, r1, Base)
      funcCode += BL("p_check_divide_by_zero", Base)
      funcCode += BL("__aeabi_idiv", Base)
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 8, update = false))
      funcCode += LDR(r, regShift(SP, 12,update = false), Base)
      funcCode += LDR(r1, regShift(SP, 8, update = false), Base)
      funcCode += ADDS(r, r, r1)
      funcCode += BL("p_throw_overflow_error", VS)
      funcCode += STR(r, regShift(SP, 4, update = false))
      funcCode += LDR(r, regShift(SP, 4, update = false), Base)
      funcCode += LDR(r, imm(2), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += MOV(reg1, r1, Base)
      funcCode += BL("p_check_divide_by_zero", Base)
      funcCode += BL("__aeabi_idiv", Base)
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 16, update = false))
      funcCode += LDR(r, regShift(SP, 12, update = false), Base)
      funcCode += LDR(r1, regShift(SP, 16, update = false), Base)
      funcCode += SUBS(r, r, r1)
      funcCode += BL("p_throw_overflow_error", VS)
      funcCode += STR(r, regVal(SP))
      funcCode += B(condLabel, Base)

      funcCode += funcName(bodyLabel)
      funcCode += SUB(SP, SP, imm(8))
      funcCode += LDR(r, regShift(SP, 24, update = false), Base)
      funcCode += STR(r, regShift(SP, 20, update = false))
      funcCode += LDR(r, regShift(SP, 32, update = false), Base)
      funcCode += LDR(r1, regShift(SP, 20, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += MOV(reg1, r1, Base)
      funcCode += BL("p_check_divide_by_zero", Base)
      funcCode += BL("__aeabi_idiv", Base)
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 4, update = false))
      funcCode += LDR(r, regShift(SP, 20, update = false), Base)
      funcCode += LDR(r1, regShift(SP, 4, update = false), Base)
      funcCode += ADDS(r, r, r1)
      funcCode += BL("p_throw_overthrow_error", VS)
      funcCode += STR(r, regVal(SP))
      funcCode += LDR(r, regVal(SP), Base)
      funcCode += LDR(r1, imm(2), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += MOV(reg1, r1, Base)
      funcCode += BL("p_check_divide_by_zero", Base)
      funcCode += BL("__aeabi_idiv", Base)
      funcCode += MOV(r, RetReg, Base)
      funcCode += STR(r, regShift(SP, 24, update = false))
      funcCode += ADD(SP, SP, imm(8))

      funcCode += funcName(condLabel)
      funcCode += LDR(r, regVal(SP), Base)
      funcCode += LDR(r1, imm(0), Base)
      funcCode += CMP(r, r1)
      funcCode += MOV(r, imm(1), NE)
      funcCode += MOV(r, imm(0), EQ)
      funcCode += CMP(r, imm(1))
      funcCode += B(bodyLabel, EQ)
      funcCode += LDR(r, regShift(SP, 16, update = false), Base)
      funcCode += MOV(RetReg, r, Base)
      funcCode += ADD(SP, SP, imm(20))
      funcCode += POP(PC)
      funcCode += POP(PC)
      funcCode += LTORG

      labels("sqrt") = funcCode.toList
    }
  }

}
