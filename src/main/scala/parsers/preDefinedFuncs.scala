package parsers

import parsers.Assembly._
import parsers.CodeGen._

object preDefinedFuncs {

  def printLn(): Unit = {
    val int_msg = s"msg_$getDataMsgIndex"
    if (!labels.contains("p_print_ln")) {
      data(int_msg) = List(
        DWord(1),
        DAscii("\\0")
      )
      labels("p_print_ln") =
        List(PUSH(LinkReg),
          LDR(RetReg, label(int_msg), Base),
          ADD(RetReg, RetReg, imm(4)),
          BL("puts", Base),
          MOV(RetReg, imm(0), Base),
          BL("fflush", Base),
          POP(PC)
        )
    }
  }

  def printString(): Unit = {
    if (!labels.contains("p_print_string")) {
      val str_format_msg = s"msg_$getDataMsgIndex"
      data(str_format_msg) = List(
        DWord(5),
        DAscii("%.*s\\0")
      )
      labels("p_print_string") =
        List(PUSH(LinkReg),
          LDR(reg(1), regVal(RetReg), Base),
          ADD(reg(2), RetReg, imm(4)),
          LDR(RetReg, label(str_format_msg), Base),
          ADD(RetReg, RetReg, imm(4)),
          BL("printf", Base),
          MOV(RetReg, imm(0), Base),
          BL("fflush", Base),
          POP(PC)
        )
    }
  }

  def printBool(): Unit = {
    if (!labels.contains("p_print_bool")) {
      val bool_true_msg = s"msg_$getDataMsgIndex"
      data(bool_true_msg) = List(
        DWord(5),
        DAscii("true\\0")
      )
      val bool_false_msg = s"msg_$getDataMsgIndex"
      data(bool_false_msg) = List(
        DWord(6),
        DAscii("false\\0")
      )
      labels("p_print_bool") =
        List(PUSH(LinkReg),
          CMP(RetReg, imm(0)),
          LDR(RetReg, label(bool_true_msg), NE),
          LDR(RetReg, label(bool_false_msg), EQ),
          ADD(RetReg, RetReg, imm(4)),
          BL("printf", Base),
          MOV(RetReg, imm(0), Base),
          BL("fflush", Base),
          POP(PC))
    }
  }

  def printInt(): Unit = {
    if (!labels.contains("p_print_int")) {
      val int_msg = s"msg_$getDataMsgIndex"
      data(int_msg) =
        List(DWord(3), DAscii("%d\\0"))

      labels("p_print_int") =
        List(PUSH(LinkReg),
          MOV(reg(1), RetReg, Base),
          LDR(RetReg, label(int_msg), Base),
          ADD(RetReg, RetReg, imm(4)),
          BL("printf", Base),
          MOV(RetReg, imm(0), Base),
          BL("fflush", Base),
          POP(PC))
    }
  }

  def checkNullPointer(): Unit = {
    runtimeError()
    val null_msg: String = s"msg_$getDataMsgIndex"
    val pair_check_null_pointer = "p_check_null_pointer"
    if (!labels.contains(pair_check_null_pointer)) {
      data(null_msg) =
        List(DWord(50),
          DAscii("NullReferenceError: dereference a null reference\\n\\0"))
      labels(pair_check_null_pointer) =
        List(PUSH(LinkReg),
          CMP(RetReg, imm(0)),
          LDR(RetReg, label(null_msg), Base),
          BL("p_throw_runtime_error", EQ),
          POP(PC)
        )
    }
  }

  def printReference(): Unit = {
    if (!labels.contains("p_print_reference")) {
      val ptr_format_msg = s"msg_$getDataMsgIndex"
      data(ptr_format_msg) = List(
        DWord(3),
        DAscii("%p\\0")
      )
      labels("p_print_reference") =
        List(PUSH(LinkReg),
          MOV(reg(1), RetReg, Base),
          LDR(RetReg, label(ptr_format_msg), Base),
          ADD(RetReg, RetReg, imm(4)), //value of 4 is not dependent on array's type
          BL("printf", Base),
          MOV(RetReg, imm(0), Base),
          BL("fflush", Base),
          POP(PC)
        )
    }
  }

  def divByZeroError(): Unit = {
    if (!labels.contains("p_check_divide_by_zero")) {
      val int_msg = s"msg_$getDataMsgIndex"
      data(int_msg) =
        List(DWord(45),
          DAscii("DivideByZeroError: divide or modulo by zero\\n\\0"))
      labels("p_check_divide_by_zero")=
        List(PUSH(LinkReg),
          CMP(reg(1), imm(0)),
          LDR(RetReg, label(int_msg), EQ),
          BL("p_throw_runtime_error", EQ),
          POP(PC))
      runtimeError()
    }
  }

  def runtimeError(): Unit = {
    if (!labels.contains("p_throw_runtime_error"))
      labels("p_throw_runtime_error") =
        List(BL("p_print_string", Base),
          MOV(RetReg, imm(-1), Base),
          BL("exit", Base))
    printString()
  }

  def intOverflow(): Unit = {
    if (!labels.contains("p_throw_overflow_error")) {
      val int_msg = s"msg_$getDataMsgIndex"
      data(int_msg) =
        List(DWord(83),
          DAscii("OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n\\0"))
      labels("p_throw_overflow_error") =
        List(LDR(RetReg, label(int_msg), Base),
          BL("p_throw_runtime_error", Base))
    }
    runtimeError()
  }

}
