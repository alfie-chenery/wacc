package parsers

import parsers.SemanticPass.st

import scala.collection.mutable
import scala.collection.immutable
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
  // todo: refactor so that maps/buffers automatically indent/format strings ?
  // todo: reformat to use instruction ADT instead of strings

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

  //TODO make use of availableRegs to replace
  def traverse(node: AstNode, ra: RegisterAllocator, code: ListBuffer[Mnemonic]): Unit = {
    node match {
      case Program(funcs, stat) =>
        code += funcName("main")
        code += PUSH(LinkReg)
        val assignments = assignmentsInScope(stat)
        currentShift = assignments
        if (assignments > 0) {
          // 1024 is the maximum immediate value because of shifting
          for (i <- 0 until assignments / 1024) code += SUB(SP, SP, imm(1024))
          code += SUB(SP, SP, imm(assignments % 1024))
        }
        for (func <- funcs) {
          traverse(func, ra, code)
        }

        traverse(stat, ra, code)
        if (assignments > 0) {
          code += ADD(SP, SP, imm(assignments % 1024))
          for (i <- 0 until assignments / 1024) code += ADD(SP, SP, imm(1024))
        }

        code += LDR(RetReg, imm(0), Base)
        code += POP(PC)
        code += LTORG

      case Func((_type, Ident(name)), ParamList(params), stat) =>
        code += funcName(name)
        code += PUSH(LinkReg)
        val assignmentSize = assignmentsInScope(stat)
        if (assignmentSize > 0) code += SUB(SP, SP, imm(assignmentSize))
        traverse(stat, ra, code)
        if (assignmentSize > 0) code += ADD(SP, SP, imm(assignmentSize))
        code += POP(LinkReg)
        code += LTORG

      case Param(_type, ident) => ???

      // <Stat>
      case Decl(PairType(t1, t2), ident, PairLiter) => ???
      case Decl(WInt, Ident(ident), rhs) =>
        val r = ra.next()
        traverseExpr(rhs, ra, code)
        //if (ret != RetReg) code += LDR(r, ret, Base)
        currentShift -= 4
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift)
        variableLocation += (ident -> location)
        code += STR(r, location)
      case Decl(WBool, Ident(ident), rhs) =>
        val r = ra.next()
        traverseExpr(rhs, ra, code)
        //if (ret != RetReg) code += MOV(r, ret, Base)
        currentShift -= 1
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift)
        variableLocation += (ident -> location)
        code += STRB(r, location)
      case Decl(WChar, Ident(ident), rhs) =>
        val r = ra.next()
        traverseExpr(rhs, ra, code)
        currentShift -= 1
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift)
        variableLocation += (ident -> location)
        code += STRB(r, location)
      case Decl(WString, Ident(ident), rhs) =>
        //todo: string length
        val r = ra.next()
        traverseExpr(rhs, ra, code)
        currentShift -= 4
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift)
        variableLocation += (ident -> location)
        code += STR(r, location)
      //case Decl

      case Assign(Ident(ident), rhs) =>
        if (st(Ident(ident))._2 == WInt) code += STR(traverseExpr(rhs, ra, code), variableLocation(ident))
        else code += STRB(traverseExpr(rhs, ra, code), variableLocation(ident))
      case Assign(lhs, rhs) => ???

      case Free(expr) => ???

      case Read(lhs: AstNode) =>
        // todo: needs to account for register availability (?)
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
                DWord(3),
                DAscii("%d\\0")
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
        code += ADD(ra.next(), SP, imm(0))
        code += MOV(RetReg, ra.next(), Base)
        code += BL(t)

      case Print(expr: AstNode) =>
        //TODO: escape escape characters somehow in data strings? when they get written to the file it treats them literally
        val ret = traverseExpr(expr, ra, code)
        // TODO change this so it doesn't match explicit types
        SemanticPass.checkExprType(expr, expr, new ListBuffer[String]) match {
          case WString =>
            val t = "p_print_string"
            if (!labels.contains(t)) {
              val str_format_msg = s"msg_$getDataMsgIndex"
              data(str_format_msg) = List(
                DWord(5),
                DAscii("%.*s\\0")
              )
              labels(t) =
                List(PUSH(LinkReg),
                  // todo: fill in this line
                  ADD(reg(2), RetReg, imm(4)),
                  LDR(RetReg, label(str_format_msg), Base),
                  ADD(RetReg, RetReg, imm(4)),
                  BL("printf"),
                  MOV(RetReg, imm(0), Base),
                  BL("fflush"),
                  POP(PC)
                )
            }
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next(), ret, SB)
            code += MOV(RetReg, ra.next(), Base)
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
                  BL("ffllush"),
                  POP(PC))
            }
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next(), ret, SB)
            code += MOV(RetReg, ra.next(), Base)
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
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next(), ret, SB)
            code += MOV(RetReg, ra.next(), Base)
            code += BL("p_print_int")

          case WChar =>
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next(), ret, Base)
            code += MOV(RetReg, ra.next(), Base)
            code += BL("putchar")

          //TODO: implement all other print types
          /*
          case PairLiter => ???
          case Ident(ident: String) => ??? // ?
          case ArrayElem(ident: Ident, expr: List[Expr]) => ???
          case ParensExpr(expr: Expr) => ??? // is this actually a possibility or are these removed in the previous passes?
           */

        }
        //TODO: find all functions that are branched to and add (???)
        //TODO: add global messages that can be added (???)
        //TODO: r4 might not be available?

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


      case Exit(expr) =>
        code += MOV(RetReg, traverseExpr(expr, ra, code), Base)
        code += BL("exit")

      case IfElse(cond, stat_true, stat_false) =>
        // TODO add stack pointer changes for new scopes
        val reg = ra.next()
        traverse(cond, ra, code)
        code += CMP(reg, imm(0))
        val newScope = new RegisterAllocator(ra.getAvailable)
        traverse(stat_true, newScope, code)
        newScope.restore()
        code += funcName(nextBranchIndex)
        traverse(stat_false, newScope, code)
        code += funcName(nextBranchIndex)


      case While(cond, stat) =>
        // TODO add stack changes for scoping
        val condLabel = nextBranchIndex
        val bodyLabel = nextBranchIndex
        code += B(condLabel)
        code += funcName(bodyLabel)
        traverse(stat, ra, code)
        code += funcName(condLabel)
        traverse(cond, ra, code)
        code += BEQ(bodyLabel)


      case Scope(stat) => traverse(stat, ra, code)

      case Combine(stats) =>
        for (stat <- stats) {
          traverse(stat, ra, code)
        }

      case Greater(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        code += LDR(reg1, label(x.toString), Base)
        code += LDR(reg2, label(y.toString), Base)
        phonyCaseCompare(code, GT, reg1, reg2)

      case GreaterEq(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        code += LDR(reg1, label(x.toString), Base)
        code += LDR(reg2, label(y.toString), Base)
        phonyCaseCompare(code, GE, reg1, reg2)

      case Less(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        code += LDR(reg1, label(x.toString), Base)
        code += LDR(reg2, label(y.toString), Base)
        phonyCaseCompare(code, LT, reg1, reg2)

      case LessEq(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        code += LDR(reg1, label(x.toString), Base)
        code += LDR(reg2, label(y.toString), Base)
        phonyCaseCompare(code, LE, reg1, reg2)

      case Eq(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        code += LDR(reg1, label(x.toString), Base)
        code += LDR(reg2, label(y.toString), Base)
        phonyCaseCompare(code, EQ, reg1, reg2)

      case NotEq(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        code += LDR(reg1, label(x.toString), Base)
        code += LDR(reg2, label(y.toString), Base)
        phonyCaseCompare(code, NE, reg1, reg2)


//      case Greater(expr1, expr2) =>
//        traverse(expr1, ra)
//        traverse(expr2, ra)
      //TODO - potentially make an eval function to evaluate expr1 to a intLiter
      // or make traverse of intLitter(x) return x.toString and nothing else
      // so that the expr and intLiter cases can be combined
      // ie case Greater(expr1, expr2) handles if expr1 and expr2 are already intLiter

      case Plus(IntLiter(x), IntLiter(y)) =>
        code += ADDS(reg(4), reg(5), reg(5))
        code += BLVS("p_throw_overflow_error")
        code += MOV(RetReg, reg(4), Base)
        //todo

      case _ =>
      }
    }

  def traverseExpr(node: AstNode, ra: RegisterAllocator, code: ListBuffer[Mnemonic]): Register = {
    node match {
      case IntLiter(x) =>
        code += LDR(ra.next(), imm(x), Base)
        ra.next()
      case Negate(IntLiter(x)) => traverseExpr(IntLiter(-x), ra, code)
      case BoolLiter(b) =>
        code += MOV(ra.next(), imm(if (b) 1 else 0), Base)
        ra.next()
      case CharLiter(c) =>
        code += MOV(ra.next(), immc(c), Base)
        ra.next()
      case StrLiter(s) =>
        val int_msg = s"msg_$getDataMsgIndex"
        data(int_msg) = List(DWord(s.length), DAscii(s))
        code += LDR(ra.next(), label(int_msg), Base)
        ra.next()
      case PairLiter => ???
      case Ident(x) =>
        code += LDR(ra.next(), variableLocation(x), Base)
        ra.next()
      case ArrayElem(Ident(x), elems) => ???
      case ParensExpr(expr) => traverseExpr(expr, ra, code)
      case Call(Ident(name), args) =>
        // TODO deal with arguments
        code += BL(name)
        RetReg

        // TODO reduce register use
      case And(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm()
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next(), res2, SB)
        code += AND(reg1, reg1, ra.next())
        ra.restore()
        reg1

      // TODO factor out repeated code
      case Or(expr1, expr2) =>
        val res1 = traverseExpr(expr1, ra, code)
        val reg1 = ra.nextRm()
        if (!res1.isInstanceOf[reg]) code += LDR(reg1, res1, SB)
        val res2 = traverseExpr(expr2, ra, code)
        if (!res2.isInstanceOf[reg]) code += LDR(ra.next(), res2, SB)
        code += ORR(reg1, reg1, ra.next())
        ra.restore()
        reg1

      // TODO binary and unary ops
    }
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
    code += MOV(RetReg, reg1, Base)
  }

  //TODO add actual IO to file, presumably file passed as parameter
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
  val typeSize: immutable.Map[Type, Int] = Map[Type, Int](
    WInt -> 4,
    WBool -> 1,
    WChar -> 1,
    WString -> 4)
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