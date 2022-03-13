package parsers

import parsers.CodeGen._
import parsers.Assembly._
import parsers.RegisterAllocator._
import parsers.preDefinedFuncs._

class MathLib {

  val mathFuncs = List("acos", "asin", "atan", "atan2", "cos", "cosh", "sin", "sinh",
    "tanh", "exp", "frexp", "ldexp", "log", "log10", "modf", "pow",
    "sqrt", "ceil", "fabs", "floor", "fmod")

  def mathCodeGen(ident: String, value: Double, ra: RegisterAllocator) ={
    ident match{
      case "asin" =>
        var r = ra.next
        val condLabel = nextBranchIndex
        val bodyLabel = nextBranchIndex
        intOverflow()
        divByZeroError()
        runtimeError()
        printString()
        //work out how to add fabs code
        if (!labels.contains("asin")){
          labels("asin") = List(PUSH(LinkReg),
            SUB(SP, SP, imm(24)),
            LDR(r, imm(1), Base),
            STR(r, regShift(SP, 20, false)),
            LDR(r, regShift(SP, 28, false), Base),
            STR(r, regShift(SP, 16, false)),
            LDR(r, imm(1), Base),
            STR(r, regShift(SP, 12, false)),
            LDR(r, imm(1), Base),
            STR(r, regShift(SP, 8, false)),
            LDR(r, regShift(SP, 28, false), Base),
            STR(r, regShift(SP, 4, false)),
            LDR(r, regShift(SP, 12, false), Base),
            STR(r, regShift(SP, -4, true)),
            BL("fabs", Base),
            ADD(SP, SP, imm(4)),
            MOV(r, RetReg, Base),
            STR(r, regVal(SP)),
            B(condLabel, Base)
          )
          val r1 = ra.next
          val r2 = ra.next
          labels(condLabel) = List(LDR(r, regVal(SP), Base),
            //this won't work
            LDR(r1, label("0.0000001"), Base),
            CMP(r, r1),
            MOV(r, imm(1), GT),
            MOV(r, imm(0), LE),
            LDR(r1, regShift(SP, 20, false), Base),
            LDR(r2, imm(100), Base),
            CMP(r1, r2),
            MOV(r1, imm(1), LT),
            MOV(r2, imm(0), GE),
            AND(r, r, r1),
            CMP(r, imm(1)),
            B(bodyLabel, EQ),
            LDR(r, regShift(SP, 16, false), Base),
            MOV(RetReg, r, Base),
            ADD(SP, SP, imm(24)),
            POP(PC),
            POP(PC),
            LTORG
          )
          val r3 = ra.next
          labels(bodyLabel) = List(LDR(r, regShift(SP, 8, false), Base),
            LDR(r1, imm(2), Base),
            LDR(r2, regShift(SP, 20, false), Base),
            SMULL(r1, r2, r1, r2),
            CMP(r2, asr(r1, 31)),
            BL("p_throw_overflow_error", NE),
            LDR(r2, imm(2), Base),
            LDR(r3, regShift(SP, 20, false), Base),
            SMULL(r2, r3, r2, r3),
            CMP(r3, asr(r2, 31)),
            BL("p_throw_overflow_error", NE),
            LDR(r3, imm(1), Base),
            ADDS(r2, r2, r3),
            BL("p_throw_overflow_error", VS),
            SMULL(r1, r2, r1, r2),
            CMP(r2, asr(r1, 31)),
            BL("p_throw_overflow_error", NE),
            SMULL(r, r1, r, r1),
            CMP(r1, asr(r, 31)),
            BL("p_throw_overflow_error", NE),
            STR(r, regShift(SP, 8, false)),
            LDR(r, regShift(SP, 4, false), Base),
            LDR(r1, imm(-1), Base),
            SMULL(r, r1, r, r1),
            CMP(r1, asr(r, 31)),
            BL("p_throw_overflow_error", NE),
            LDR(r1, regShift(SP, 28, false), Base),
            SMULL(r, r1, r, r1),
            CMP(r1, asr(r, 31)),
            BL("p_throw_overflow_error", NE),
            LDR(r1, regShift(SP, 28, false), Base),
            SMULL(r, r1, r, r1),
            CMP(r1, asr(r, 31)),
            BL("p_throw_overflow_error", NE),
            STR(r, regShift(SP, 4, false)),
            LDR(r, regShift(SP, 4, false), Base),
            LDR(r1, regShift(SP, 8, false), Base),
            MOV(RetReg, r, Base),
            MOV(reg(1), r1, Base),
            BL("p_check_divide_by_zero", Base),
            BL("__aeabi_idiv", Base),
            MOV(r, RetReg, Base),
            STR(r, regShift(SP, 12, false)),
            LDR(r, regShift(SP, 16, false), Base),
            LDR(r1, regShift(SP, 12, false), Base),
            ADDS(r, r, r1),
            BL("p_throw_overflow_error", VS),
            STR(r, regShift(SP, 16, false)),
            LDR(r, regShift(SP, 20, false), Base),
            LDR(r1, imm(1), Base),
            ADDS(r, r, r1),
            BL("p_throw_overflow_error", VS),
            STR(r, regShift(SP, 20, false)),
            LDR(r, regShift(SP, 12, false), Base),
            STR(r, regShift(SP, -4, true)),
            BL("fabs", Base),
            ADD(SP, SP, imm(4)),
            MOV(r, RetReg, Base),
            STR(r, regVal(SP))
          )
        }
      case "fabs" =>
    }
  }

}
