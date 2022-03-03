package parsers

import parsers.SemanticPass.{checkExprType, st}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object CodeGen{
  /**
   * Code Generation pass
   */
   import parsers.Ast._
   import parsers.Assembly._

  // each of these maps represent a section of the output code that can be appended to
  // linkedHashMaps, since data and labels should follow a specific order
  val data = new mutable.LinkedHashMap[String, List[Mnemonic]]
  val labels = new mutable.LinkedHashMap[String, List[Mnemonic]]
  val variableLocation = new mutable.LinkedHashMap[String, Register]
  var currentShift = 0

  /**
   * Returns the next available 'msg' index in data
   */
  def getDataMsgIndex: Int = {
    data.size
  }

  /**
   * value used for conditional branch labels L0, L1 etc
   */
  var branchIndex: Int = -1
  def nextBranchIndex: String = {
    branchIndex += 1
    "L" + branchIndex.toString
  }

  def traverse(node: AstNode, ra: RegisterAllocator, code: ListBuffer[Mnemonic]): Unit = {
    println(node)
    node match {
      case Program(funcs, stat) =>
        for (func <- funcs) {
          traverse(func, ra, code)
        }

        code += funcName("main")
        code += PUSH(LinkReg)
        val assignments = assignmentsInScope(stat)
        currentShift = assignments
        if (assignments > 0) {
          // 1024 is the maximum immediate value because of shifting
          for (_ <- 0 until assignments / 1024) code += SUB(SP, SP, imm(1024))
          code += SUB(SP, SP, imm(assignments % 1024))
        }

        traverse(stat, ra, code)
        if (assignments > 0) {
          code += ADD(SP, SP, imm(assignments % 1024))
          for (_ <- 0 until assignments / 1024) code += ADD(SP, SP, imm(1024))
        }

        code += LDR(RetReg, imm(0), Base)
        code += POP(PC)
        code += LTORG

      case Func((_, Ident(name)), ParamList(params), stat) =>
        val assignments = assignmentsInScope(stat)
        currentShift = assignments + 4
        for (param <- params) {
          variableLocation += (param.ident.ident -> regShift(SP, currentShift, update = false))
          currentShift += typeSize(param._type)
        }
        code += funcName(name)
        code += PUSH(LinkReg)
        if (assignments > 0) code += SUB(SP, SP, imm(assignments))
        traverse(stat, ra, code)
        if (assignments > 0) code += ADD(SP, SP, imm(assignments))
        code += POP(PC)
        code += LTORG

      case Param(_type, ident) => ???

      // <Stat>
      case Decl(PairType(t1, t2), Ident(ident), NewPair(fst, snd)) =>
        code += LDR(RetReg, imm(8), Base)
        code += BL("malloc")
        code += MOV(ra.next, RetReg, Base)
        val reg1 = ra.nextRm
        val fstReg = traverseExpr(fst, ra, code)
        code += LDR(RetReg, imm(typeSize(t1)), Base)
        code += BL("malloc")
        code += STR(fstReg, regVal(RetReg))
        code += STR(RetReg, regVal(reg1))
        val sndReg = traverseExpr(snd, ra, code)
        code += LDR(RetReg, imm(typeSize(t2)), Base)
        code += BL("malloc")
        code += STRB(ra.next, regVal(RetReg))
        code += STR(RetReg, regShift(reg1, 4, update = false))
        code += STR(reg1, regVal(SP))
        // TODO this location probably needs to be changed
        variableLocation += (ident -> regVal(SP))
        ra.restore()
      case Decl(ArrayType(_type), Ident(ident), ArrayLiter(exprs)) =>
        code += LDR(RetReg, imm(exprs.length * typeSize(_type) + 4), Base)
        code += BL("malloc")
        val reg =ra.nextRm
        code += MOV(reg, RetReg, Base)
        var location = 4
        for (expr <- exprs) {
          val ret = traverseExpr(expr, ra, code)
          code += STR(ret, regShift(reg, location, update = false))
          location += typeSize(_type)
        }
        code += LDR(ra.next, imm(exprs.size), Base)
        code += STR(ra.next, regVal(reg))
        code += STR(reg, regVal(SP))
        // TODO this location probably needs to be changed
        variableLocation += (ident -> regVal(SP))
        ra.restore()
      case Decl(WInt, Ident(ident), rhs) =>
        val r = ra.next
        traverseExpr(rhs, ra, code)
        //if (ret != RetReg) code += LDR(r, ret, Base)
        currentShift -= 4
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift, update = false)
        variableLocation += (ident -> location)
        code += STR(r, location)
      case Decl(WBool, Ident(ident), rhs) =>
        val r = ra.next
        traverseExpr(rhs, ra, code)
        //if (ret != RetReg) code += MOV(r, ret, Base)
        currentShift -= 1
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift, update = false)
        variableLocation += (ident -> location)
        code += STRB(r, location)
      case Decl(WChar, Ident(ident), rhs) =>
        val r = ra.next
        traverseExpr(rhs, ra, code)
        currentShift -= 1
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift, update = false)
        variableLocation += (ident -> location)
        code += STRB(r, location)
      case Decl(WString, Ident(ident), rhs) =>
        val r = ra.next
        traverseExpr(rhs, ra, code)
        currentShift -= 4
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift, update = false)
        variableLocation += (ident -> location)
        code += STR(r, location)
      //case Decl

      case Assign(Ident(ident), rhs) =>
        if (st(Ident(ident))._2 == WInt) code += STR(traverseExpr(rhs, ra, code), variableLocation(ident))
        else code += STRB(traverseExpr(rhs, ra, code), variableLocation(ident))
      case Assign(lhs, rhs) => ???

      case Free(expr) => ???

      case Read(lhs: AstNode) =>
        val _type: Type = SemanticPass.checkExprType(lhs, node, new ListBuffer[String])
        val t = _type match {
          case WChar => "p_read_char"
          case WInt => "p_read_int"
        }
        if (!labels.contains(t)) {
          val read_msg = s"msg_$getDataMsgIndex"
          // TODO remove massive duplication
          t match {
            case "p_read_char" =>
              data(read_msg) = List(
                DWord(4),
                DAscii(" %c\\0")
              )
              labels("p_read_char") =
                List(PUSH(LinkReg),
                  MOV(reg(1), RetReg, Base),
                  LDR(RetReg, label(read_msg), Base),
                  ADD(RetReg, RetReg, imm(4)),
                  BL("scanf"),
                  POP(PC))
            case "p_read_int" =>
              data(read_msg) = List(
                DWord(3),
                DAscii("%d\\0")
              )
              labels("p_read_int") =
                List(PUSH(LinkReg),
                  MOV(reg(1), RetReg, Base),
                  LDR(RetReg, label(read_msg), Base),
                  ADD(RetReg, RetReg, imm(4)),
                  BL("scanf"),
                  POP(PC))
          }
        }
        code += ADD(ra.next, SP, imm(0))
        code += MOV(RetReg, ra.next, Base)
        code += BL(t)

      case Print(expr: AstNode) =>
        //TODO: escape escape characters somehow in data strings? when they get written to the file it treats them literally
        val ret = traverseExpr(expr, ra, code)
        // TODO change this so it doesn't match explicit types
        SemanticPass.checkExprType(expr, expr, new ListBuffer[String]) match {
          case WString =>
            printString()
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next, ret, SB)
            code += MOV(RetReg, ra.next, Base)
            code += BL("p_print_string")


          case WBool =>
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
                  BL("printf"),
                  MOV(RetReg, imm(0), Base),
                  BL("fflush"),
                  POP(PC))
            }
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next, ret, SB)
            code += MOV(RetReg, ra.next, Base)
            code += BL("p_print_bool")

          case WInt =>
            if (!labels.contains("p_print_int")) {
              val int_msg = s"msg_$getDataMsgIndex"
              data(int_msg) =
                List(DWord(3), DAscii("%d\\0"))

              labels("p_print_int") =
                List(PUSH(LinkReg),
                  MOV(reg(1), RetReg, Base),
                  LDR(RetReg, label(int_msg), Base),
                  ADD(RetReg, RetReg, imm(4)),
                  BL("printf"),
                  MOV(RetReg, imm(0), Base),
                  BL("fflush"),
                  POP(PC))
            }
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next, ret, SB)
            code += MOV(RetReg, ra.next, Base)
            code += BL("p_print_int")

          case WChar =>
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next, ret, Base)
            code += MOV(RetReg, ra.next, Base)
            code += BL("putchar")


          case ArrayType(_type) =>
            //printing an array variable prints its address
            printReference()
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next, ret, SB)
            code += MOV(RetReg, ra.next, Base)
            code += BL("p_print_reference")


          //TODO: implement all other print types
          /*
          case ArrayElem(ident: Ident, expr: List[Expr]) => ???
          case PairLiter => ???
          case Ident(ident: String) => ???
          case ParensExpr(expr: Expr) => ??? // is this actually a possibility or are these removed in the previous passes?
           */

        }
        //TODO: find all functions that are branched to and add (???)
        //TODO: add global messages that can be added (???)

      case Println(expr) =>
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
              BL("puts"),
              MOV(RetReg, imm(0), Base),
              BL("fflush"),
              POP(PC)
            )
        }
        traverse(Print(expr), ra, code)
        code += BL("p_print_ln")

      case Return(expr) =>
        code += MOV(RetReg, traverseExpr(expr, ra, code), Base)
        code += POP(PC)

      case Exit(expr) =>
        code += MOV(RetReg, traverseExpr(expr, ra, code), Base)
        code += BL("exit")

      case IfElse(cond, stat_true, stat_false) =>
        // TODO add stack pointer changes for new scopes
        val reg = traverseExpr(cond, ra, code)
        code += CMP(reg, imm(0))
        val fun1 = nextBranchIndex
        val fun2 = nextBranchIndex
        code += BEQ(fun1)
        traverse(stat_true, ra, code)
        code += B(fun2)
        code += funcName(fun1)
        traverse(stat_false, ra, code)
        code += funcName(fun2)

      case While(cond, stat) =>
        // TODO add stack changes for scoping
        val condLabel = nextBranchIndex
        val bodyLabel = nextBranchIndex
        code += B(condLabel)
        code += funcName(bodyLabel)
        traverse(stat, ra, code)
        code += funcName(condLabel)
        traverse(cond, ra, code)
        val reg = traverseExpr(cond, ra, code)
        code += CMP(reg, imm(1))
        code += BEQ(bodyLabel)

      case Scope(stat) => traverse(stat, ra, code)

      case Combine(stats) =>
        for (stat <- stats) {
          traverse(stat, ra, code)
        }
      //TODO - potentially make an eval function to evaluate expr1 to a intLiter
      // or make traverse of intLitter(x) return x.toString and nothing else
      // so that the expr and intLiter cases can be combined
      // ie case Greater(expr1, expr2) handles if expr1 and expr2 are already intLiter

      case _ =>
      }
    }

  def traverseExpr(node: AstNode, ra: RegisterAllocator, code: ListBuffer[Mnemonic]): Register = {
    node match {
      case IntLiter(x) =>
        code += LDR(ra.next, imm(x), Base)
        ra.next
      case Negate(IntLiter(x)) => traverseExpr(IntLiter(-x), ra, code)
      case BoolLiter(b) =>
        code += MOV(ra.next, imm(if (b) 1 else 0), Base)
        ra.next
      case CharLiter(c) =>
        // todo: is this the correct solution or does it only solve one case?
        val _c = if (c=="\\u0000") imm(0) else immc(c)
        code += MOV(ra.next, _c, Base)
        ra.next
      case StrLiter(s) =>
        val int_msg = s"msg_$getDataMsgIndex"
        data(int_msg) = List(DWord(s.length), DAscii(s))
        code += LDR(ra.next, label(int_msg), Base)
        ra.next
      case PairLiter => ???
      case Ident(x) =>
        // TODO this will probably need to change when pairs and arrays are implemented
        if (typeSize(st(Ident(x))._2) == 4) code += LDR(ra.next, variableLocation(x), Base)
        else code += LDR(ra.next, variableLocation(x), SB)
        ra.next
      case ArrayElem(Ident(x), elems) =>
        // TODO this wouldn't work for multi-dimensional arrays
        val reg1 = ra.nextRm
        code += ADD(reg1, SP, traverseExpr(elems.head, ra, code))
        ra.restore()
        reg1
      case ParensExpr(expr) => traverseExpr(expr, ra, code)
      case Call(Ident(name), ArgList(args)) =>
        var totalSize = 0
        for (arg <- args) {
          val size = typeSize(checkExprType(arg, arg, new ListBuffer[String]))
          totalSize += size
          val reg = traverseExpr(arg, ra, code)
          if (size == 4) code += STR(reg, regShift(SP, -size, update = true))
          else code += STRB(reg, regShift(SP, -size, update = true))
        }
        code += BL(name)
        if (totalSize > 0) code += ADD(SP, SP, imm(totalSize))
        code += MOV(ra.next, RetReg, Base)
        RetReg

      case And(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = ra.nextRm
        }
        var res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next
        }
        print(ra.next)
        code += AND(res1, res1, res2)
        ra.restore()
        reg1

      // TODO factor out repeated code
      case Or(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        var res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next
        }
        code += ORR(res1, res1, res2)
        ra.restore()
        reg1

        // TODO check if reg1 should be moved with LDR or LDRSB
      case Greater(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
        phonyCaseCompare(code, GT, reg1, ra.next)
        ra.restore()
        reg1

      case GreaterEq(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
        phonyCaseCompare(code, GE, reg1, ra.next)
        ra.restore()
        reg1

      case Less(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
        phonyCaseCompare(code, LT, reg1, ra.next)
        ra.restore()
        reg1

      case LessEq(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
        phonyCaseCompare(code, LE, reg1, ra.next)
        ra.restore()
        reg1

      case Eq(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
        phonyCaseCompare(code, EQ, reg1, ra.next)
        ra.restore()
        reg1

      case NotEq(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
        phonyCaseCompare(code, NE, reg1, ra.next)
        ra.restore()
        reg1

      case Plus(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
        code += ADDS(reg1, reg1, ra.next)
        intOverflow()
        code += BLVS("p_throw_overflow_error")
        ra.restore()
        reg1

      case Minus(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
        code += SUBS(reg1, reg1, ra.next)
        intOverflow()
        code += BLVS("p_throw_overflow_error")
        ra.restore()
        reg1

      case Mult(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
        code += SMULL(reg1, ra.next, reg1, ra.next)
        code += CMP(ra.next, asr(reg1, 31))
        intOverflow()
        code += BLNE("p_throw_overflow_error")
        ra.restore()
        reg1
      case Div(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
        code += MOV(RetReg, reg1, Base)
        code += MOV(reg(1), ra.next, Base)
        divByZeroError()
        code += BL("p_check_divide_by_zero")
        code += BL("__aeabi_idiv")
        code += MOV(reg1, RetReg, Base)
        ra.restore()
        reg1

      case Mod(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
        code += MOV(RetReg, reg1, Base)
        code += MOV(reg(1), ra.next, Base)
        divByZeroError()
        code += BL("p_check_divide_by_zero")
        code += BL("__aeabi_idivmod")
        code += MOV(reg1, reg(1), Base)
        ra.restore()
        reg1

      case Negate(expr) =>
        val reg = traverseExpr(expr, ra, code)
        if (!reg.isInstanceOf[reg]) code += LDR(reg, reg, Base)
        intOverflow()
        code += RSBS(reg, reg, imm(0))
        code += BLVS("p_throw_overflow_error")
        reg

      case Not(expr) =>
        val reg = traverseExpr(expr, ra, code)
        if (!reg.isInstanceOf[reg]) code += LDR(reg, reg, SB)
        code += EOR(reg, reg, imm(1))
        reg

      case Chr(expr) =>
        val reg = traverseExpr(expr, ra, code)
        code += MOV(RetReg, reg, Base)
        code += BL("putchar")
        reg

      case Ord(expr) =>
        val reg = traverseExpr(expr, ra, code)
        // TODO implement this
        //code += MOV(reg, immc(expr.asInstanceOf[Char]), Base)
        reg

      case Len(expr) =>
        val reg = traverseExpr(expr,ra,code)
        code += LDR(reg, regVal(reg), Base)
        ra.next

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
          BL("printf"),
          MOV(RetReg, imm(0), Base),
          BL("fflush"),
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
          BL("printf"),
          MOV(RetReg, imm(0), Base),
          BL("fflush"),
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
          BLEQ("p_throw_runtime_error"),
          POP(PC))
      runtimeError()
    }
  }

  def runtimeError(): Unit = {
    if (!labels.contains("p_throw_runtime_error"))
      labels("p_throw_runtime_error") =
        List(BL("p_print_string"),
          MOV(RetReg, imm(-1), Base),
          BL("exit"))
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
          BL("p_throw_runtime_error"))
    }
    runtimeError()
  }

  def traverseBinaryExpr(expr1: Expr, expr2: Expr, ra: RegisterAllocator, code: ListBuffer[Mnemonic]): Unit = {
    val res1 = traverseExpr(expr1, ra, code)
    val reg1 = ra.nextRm
    if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
    val res2 = traverseExpr(expr2, ra, code)
    if (!res2.isInstanceOf[reg]) code += LDR(ra.next, res2, SB)
  }

  def phonyCaseCompare(code: ListBuffer[Mnemonic], suffix1: Suffix, reg1: Register, reg2: Register): Unit = {
    /**
     * Function to factor out repeated code for comparison expressions
     * Can be seen as a case containing all comparison cases as sub cases,
     * But this isn't reflected in the current AST implementation hence phony
     * The main case in traverse should ensure reg1 and reg2 store the correct
     * values before calling this function
     */
    var suffix2: Suffix = null
    suffix1 match{
      case GT =>
        suffix2 = LE
      case GE =>
        suffix2 = LT
      case LT =>
        suffix2 = GE
      case LE =>
    suffix2 = GT
      case EQ =>
        suffix2 = NE
      case NE =>
        suffix2 = EQ
    }

    code += CMP(reg1, reg2)
    code += MOV(reg1, imm(1), suffix1)
    code += MOV(reg1, imm(0), suffix2)
  }

  def compile(node: AstNode, ra: RegisterAllocator = new RegisterAllocator()): String = {
    val sb = new StringBuilder()
    val code: ListBuffer[Mnemonic] = ListBuffer()
    traverse(node, ra, code)

    if (data.nonEmpty) {
      sb.append(".data\n\n")
      for ((k, body) <- data) {
        sb.append("\n" + k + ":\n\t")
        for (line <- body) {
          sb.append(line + "\n\t")
        }
      }
    }
    sb.append("\n.text\n\n")
    sb.append(".global main\n\n")

    for (line <- code) {
      sb.append((if (!line.isInstanceOf[funcName]) "\t" else "") + line.toString + "\n")
    }
    // todo: the formatting might fail once IfElse is implemented...
    for((k,v) <- labels){
      sb.append("\n" + k + ":\n\t")
      for (line <- v) {
        sb.append(line + "\n\t")
      }
    }
    sb.toString()
  }

  // TODO add more to this map
  def typeSize(_type: Type): Int = {
    _type match {
      case WInt => 4
      case WBool => 1
      case WChar => 1
      case WString => 4
      case PairType(_, _) => 4
      case ArrayType(_) => 4
    }
  }
  def assignmentsInScope(stats: Stat): Int = {
    var size = 0
    stats match {
      case Decl(_type, _, _) => size += typeSize(_type)
      case Combine(stats) =>
        for (stat <- stats) {
          stat match {
            case Decl(_type, _, _) => size += typeSize(_type)
            case _ =>
          }
        }
    }
    size
  }

  def add(map: mutable.LinkedHashMap[String, String], key: String, value: String): Unit = {
    /**
     * Adds new key value pair to map only if the key is not already present
     */
    if(!map.contains(key)){
      map += (key -> value)
    }
  }

  def update(map: mutable.HashMap[String, String], key: String, data: String): Unit = {
    /**
     * Appends data to the value found at key in map
     * If key is not in map, it is added with data as its value
     */
    map(key) = map.getOrElse(key, "") + data
  }

}