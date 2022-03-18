package parsers

import parsers.CodeGen._
import parsers.Assembly._
import parsers.RegisterAllocator._
import parsers.preDefinedFuncs._
import parsers.Ast._
import parsers.SemanticPass.checkExprType

import scala.collection.mutable.ListBuffer

object MathLib {

  def sin(ra: RegisterAllocator, va: VfpAllocator, expr: Expr, code: ListBuffer[Mnemonic]): Unit = {
    val condLabel = nextBranchIndex
    val bodyLabel = nextBranchIndex
    intOverflow()
    divByZeroError()
    runtimeError()
    printString()
    //todo: correct expr pass
    pow(ra, va, expr, code)
    fact(ra, va, expr, code)
    if (!labels.contains("sin")){
      val r = traverseExpr(expr, ra, va, code)
      labels("sin") = List(
        PUSH(LinkReg),
        SUB(SP, SP, imm(12)),
        LDR(r, regShift(SP, 16, update = false), Base),
        STR(r, regShift(SP, 8, update = false)),
        LDR(r, imm(-1), Base),
        STR(r, regShift(SP, 4, update = false)),
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
        LDR(r, regShift(SP, 8, update = false), Base),
        MOV(RetReg, r, Base),
        ADD(SP, SP, imm(12)),
        POP(PC),
        POP(PC),
        LTORG
      )
      val r2 = ra.next
      labels(bodyLabel) = List(
        SUB(SP, SP, imm(16)),
        LDR(r, regShift(SP, 16, update = false), Base),
        STR(r, regShift(SP, -4, update = true)),
        LDR(r, regShift(SP, 36, update = false), Base),
        STR(r, regShift(SP, -4, update = true)),
        BL("pow", Base),
        ADD(SP, SP, imm(8)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 12, update = false)),
        LDR(r, regShift(SP, 16, update = false), Base),
        STR(r, regShift(SP, -4, update = true)),
        BL("fact", Base),
        ADD(SP, SP, imm(4)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 8, update = false)),
        LDR(r, regShift(SP, 12, update = false), Base),
        LDR(r1, regShift(SP, 8, update = false), Base),
        MOV(RetReg, r, Base),
        MOV(reg(1), r1, Base),
        BL("p_check_divide_by_zero", Base),
        BL("__aeabi_idiv", Base),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 4, update = false)),
        LDR(r, regShift(SP, 20, update = false), Base),
        LDR(r1, regShift(SP, 4, update = false), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regVal(SP)),
        LDR(r, regShift(SP, 24, update = false), Base),
        LDR(r1, regVal(SP), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", VS),
        STR(r, regShift(SP, 24, update = false)),
        LDR(r, regShift(SP, 20, update = false), Base),
        LDR(r1, imm(-1), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 20, update = false)),
        LDR(r, regShift(SP, 16, update = false), Base),
        LDR(r1, imm(2), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 16, update = false)),
        ADD(SP, SP, imm(16))
      )
    }
  }

  def cos(ra: RegisterAllocator, va: VfpAllocator, expr: Expr, code: ListBuffer[Mnemonic]): Unit ={
    divByZeroError()
    intOverflow()
    runtimeError()
    printString()
    //todo: correct expr pass
    pow(ra, va, expr, code)
    fact(ra, va, expr, code)
    val condLabel = nextBranchIndex
    val bodyLabel = nextBranchIndex
    if (!labels.contains("cos")){
      val r = traverseExpr(expr, ra, va, code)
      labels("cos") = List(
        PUSH(LinkReg),
        SUB(SP, SP, imm(12)),
        LDR(r, imm(1), Base),
        STR(r, regShift(SP, 8, update = false)),
        LDR(r, imm(-1), Base),
        STR(r, regShift(SP, 4, update = false)),
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
        LDR(r, regShift(SP, 8, update = false), Base),
        MOV(RetReg, r, Base),
        ADD(SP, SP, imm(12)),
        POP(PC),
        POP(PC),
        LTORG
      )
      val r2 = ra.next
      labels(bodyLabel) = List(
        SUB(SP, SP, imm(16)),
        LDR(r, regShift(SP, 16, update = false), Base),
        STR(r, regShift(SP, -4, update = true)),
        LDR(r, regShift(SP, 36, update = false), Base),
        STR(r, regShift(SP, -4, update = true)),
        BL("pow", Base),
        ADD(SP, SP, imm(8)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 12, update = false)),
        LDR(r, regShift(SP, 16, update = false), Base),
        STR(r, regShift(SP, -4, update = true)),
        BL("fact", Base),
        ADD(SP, SP, imm(4)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 8, update = false)),
        LDR(r, regShift(SP, 12, update = false), Base),
        LDR(r1, regShift(SP, 8, update = false), Base),
        MOV(RetReg, r, Base),
        MOV(reg(1), r1, Base),
        BL("p_check_divide_by_zero", Base),
        BL("__aeabi_idiv", Base),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 4, update = false)),
        LDR(r, regShift(SP, 20, update = false), Base),
        LDR(r1, regShift(SP, 4, update = false), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regVal(SP)),
        LDR(r, regShift(SP, 24, update = false), Base),
        LDR(r1, regVal(SP), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", VS),
        STR(r, regShift(SP, 24, update = false)),
        LDR(r, regShift(SP, 20, update = false), Base),
        LDR(r1, imm(-1), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 20, update = false)),
        LDR(r, regShift(SP, 16, update = false), Base),
        LDR(r1, imm(2), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 20, update = false)),
        ADD(SP, SP, imm(16))
      )
    }
  }

  def tan(ra: RegisterAllocator, va: VfpAllocator, expr: Expr, code: ListBuffer[Mnemonic]): Unit ={
    printString()
    runtimeError()
    divByZeroError()
    //todo: correct expr pass
    cos(ra, va, expr, code)
    sin(ra, va, expr, code)
    if(!labels.contains("tan")){
      val r = traverseExpr(expr, ra, va, code)
      val r1 = ra.next
      labels("tan") = List(
        PUSH(LinkReg),
        SUB(SP, SP, imm(12)),
        LDR(r, regShift(SP, 16, update = false), Base),
        STR(r, regShift(SP, -4, update = true)),
        BL("sin", Base),
        ADD(SP, SP, imm(4)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP,8, update = false)),
        LDR(r, regShift(SP, 16, update = false), Base),
        STR(r, regShift(SP, -4, update = true)),
        BL("cos", Base),
        ADD(SP, SP, imm(4)),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 4, update = false)),
        LDR(r, regShift(SP, 8, update = false), Base),
        LDR(r1, regShift(SP, 4, update = false), Base),
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

  def pow(ra: RegisterAllocator, va: VfpAllocator, expr1: Expr, expr2: Expr,  code: ListBuffer[Mnemonic]): Unit ={
    if(!labels.contains("pow")){
      printString()
      runtimeError()
      intOverflow()
      val condLabel = nextBranchIndex
      val bodyLabel = nextBranchIndex
      val r = ra.next
      val r1 = traverseExpr(expr1, ra, va, code)

      val funcCode = new ListBuffer[Mnemonic]()
      funcCode += PUSH(LinkReg)
      funcCode += SUB(SP, SP, imm(8))
      LDR(r, imm(1), Base)
      STR(r, regShift(SP, 4, update = false))
      LDR(r, imm(0), Base)
      STR(r, regVal(SP))
      B(condLabel, Base)

      val t = checkExprType(expr1, expr1, new ListBuffer[String])
      funcCode += funcName(bodyLabel)
      funcCode += LDR(r, regShift(SP, 4, update = false), Base)
      funcCode += LDR(r1, regShift(SP, 12, update = false), Base)

      if (t == WInt) {
        funcCode += SMULL(r, r1, r, r1)
        funcCode += CMP(r1, asr(r, 31))
        funcCode += BL("p_throw_overflow_error", NE)
        funcCode += STR(r, regShift(SP, 4, update = false))
        funcCode += LDR(r, regVal(SP), Base)
        funcCode += LDR(r1, imm(1), Base)
        funcCode += ADDS(r, r, r1)
        funcCode += BL("p_throw_overflow_error", VS)
        funcCode += STR(r, regVal(SP))
        funcCode += funcName(bodyLabel)
      }else{
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
      }

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

  def fact(ra: RegisterAllocator, va: VfpAllocator, expr: Expr, code: ListBuffer[Mnemonic]): Unit ={
    printString()
    runtimeError()
    intOverflow()
    val condLabel = nextBranchIndex
    val bodyLabel = nextBranchIndex
    if(!labels.contains("fact")){
      val r = traverseExpr(expr, ra, va, code)
      labels("fact") = List(
        PUSH(LinkReg),
        SUB(SP, SP, imm(8)),
        LDR(r, imm(1), Base),
        STR(r, regShift(SP, 4, update = false)),
        LDR(r, imm(1), Base),
        STR(r, regVal(SP)),
        B(condLabel, Base)
      )
      val r1 = ra.next
      labels(condLabel) = List(
        LDR(r, regVal(SP), Base),
        LDR(r1, regShift(SP, 12, update = false), Base),
        CMP(r, r1),
        MOV(r, imm(1), LE),
        MOV(r, imm(0), GT),
        CMP(r, imm(1)),
        B(bodyLabel, EQ),
        LDR(r, regShift(SP, 4, update = false), Base),
        MOV(RetReg, r, Base),
        ADD(SP, SP, imm(8)),
        POP(PC),
        POP(PC),
        LTORG
      )
      labels(bodyLabel) = List(
        LDR(r, regShift(SP, 4, update = false), Base),
        LDR(r1, regVal(SP), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 4, update = false)),
        LDR(r, regVal(SP), Base),
        LDR(r1, imm(1), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", VS),
        STR(r, regVal(SP))
      )
    }
  }

  def fabs(ra: RegisterAllocator, va: VfpAllocator, expr: Expr, code: ListBuffer[Mnemonic]): Unit ={
    if(!labels.contains("fabs")){
      intOverflow()
      runtimeError()
      printString()
      val cond1Label = nextBranchIndex
      val cond2Label = nextBranchIndex
      val r = traverseExpr(expr, ra, va, code)
      val r1 = ra.next
      labels("fabs") = List(
        PUSH(LinkReg),
        LDR(r, regShift(SP, 4, update = false), Base),
        LDR(r1, imm(0), Base),
        CMP(r, r1),
        MOV(r, imm(1), LT),
        MOV(r, imm(0), GE),
        CMP(r, imm(0)),
        B(cond1Label, EQ),
        LDR(r, regShift(SP, 4, update = false), Base),
        LDR(r1, imm(-1), Base),
        SMULL(r, r1, r, r1),
        CMP(r1, asr(r, 31)),
        BL("p_throw_overflow_error", NE),
        STR(r, regShift(SP, 4, update = false)),
        LDR(r, regShift(SP, 4, update = false), Base),
        MOV(RetReg, r, Base),
        POP(PC),
        B(cond2Label, Base)
      )
      labels(cond1Label) = List(
        LDR(r, regShift(SP, 4, update = false), Base),
        MOV(RetReg, r, Base),
        POP(PC)
      )
      labels(cond2Label)= List(
        POP(PC),
        LTORG
      )
    }
  }

  def sqrt(ra: RegisterAllocator, va: VfpAllocator, expr: Expr, code: ListBuffer[Mnemonic]): Unit ={
    if(!labels.contains("sqrt")){
      printString()
      runtimeError()
      intOverflow()
      divByZeroError()
      val condLabel = nextBranchIndex
      val bodyLabel = nextBranchIndex
      val r = traverseExpr(expr, ra, va, code)
      val r1 = ra.next
      labels("sqrt") = List(
        PUSH(LinkReg),
        SUB(SP, SP, imm(20)),
        LDR(r,regShift(SP, 24, update = false), Base),
        LDR(r1, imm(2), Base),
        MOV(RetReg, r, Base),
        MOV(reg(1), r1, Base),
        BL("p_check_divide_by_zero", Base),
        BL("__aeabi_idiv", Base),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 16, update = false)),
        LDR(r, regShift(SP, 16, update = false), Base),
        STR(r, regShift(SP, 12, update = false)),
        LDR(r, regShift(SP, 24, update = false), Base),
        LDR(r1, regShift(SP, 12, update = false), Base),
        MOV(RetReg, r, Base),
        MOV(reg(1), r1, Base),
        BL("p_check_divide_by_zero", Base),
        BL("__aeabi_idiv", Base),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 8, update = false)),
        LDR(r, regShift(SP, 12,update = false), Base),
        LDR(r1, regShift(SP, 8, update = false), Base),
        ADDS(r, r, r1),
        BL("p_throw_overflow_error", VS),
        STR(r, regShift(SP, 4, update = false)),
        LDR(r, regShift(SP, 4, update = false), Base),
        LDR(r, imm(2), Base),
        MOV(RetReg, r, Base),
        MOV(reg(1), r1, Base),
        BL("p_check_divide_by_zero", Base),
        BL("__aeabi_idiv", Base),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 16, update = false)),
        LDR(r, regShift(SP, 12, update = false), Base),
        LDR(r1, regShift(SP, 16, update = false), Base),
        SUBS(r, r, r1),
        BL("p_throw_overflow_error", VS),
        STR(r, regVal(SP)),
        B(condLabel, Base)
      )
      labels(condLabel) = List(
        LDR(r, regVal(SP), Base),
        LDR(r1, imm(0), Base),
        CMP(r, r1),
        MOV(r, imm(1), NE),
        MOV(r, imm(0), EQ),
        CMP(r, imm(1)),
        B(bodyLabel, EQ),
        LDR(r, regShift(SP, 16, update = false), Base),
        MOV(RetReg, r, Base),
        ADD(SP, SP, imm(20)),
        POP(PC),
        POP(PC),
        LTORG
      )
      labels(bodyLabel) = List(
        SUB(SP, SP, imm(8)),
        LDR(r, regShift(SP, 24, update = false), Base),
        STR(r, regShift(SP, 20, update = false)),
        LDR(r, regShift(SP, 32, update = false), Base),
        LDR(r1, regShift(SP, 20, update = false), Base),
        MOV(RetReg, r, Base),
        MOV(reg(1), r1, Base),
        BL("p_check_divide_by_zero", Base),
        BL("__aeabi_idiv", Base),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 4, update = false)),
        LDR(r, regShift(SP, 20, update = false), Base),
        LDR(r1, regShift(SP, 4, update = false), Base),
        ADDS(r, r, r1),
        BL("p_throw_overthrow_error", VS),
        STR(r, regVal(SP)),
        LDR(r, regVal(SP), Base),
        LDR(r1, imm(2), Base),
        MOV(RetReg, r, Base),
        MOV(reg(1), r1, Base),
        BL("p_check_divide_by_zero", Base),
        BL("__aeabi_idiv", Base),
        MOV(r, RetReg, Base),
        STR(r, regShift(SP, 24, update = false)),
        ADD(SP, SP, imm(8))
      )
    }
  }

}
