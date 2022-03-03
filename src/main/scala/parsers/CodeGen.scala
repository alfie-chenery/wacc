package parsers

import parsers.SemanticPass.{checkExprType, checkType, st}

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

      // <Stat>
      case Decl(PairType(t1, t2), Ident(ident), NewPair(fst, snd)) =>
        code += LDR(RetReg, imm(8), Base)
        code += BL("malloc", Base)
        code += MOV(ra.next, RetReg, Base)
        val reg1 = ra.nextRm
        val fstReg = traverseExpr(fst, ra, code)
        code += LDR(RetReg, imm(typeSize(t1)), Base)
        code += BL("malloc", Base)
        code += STR(fstReg, regVal(RetReg))
        code += STR(RetReg, regVal(reg1))
        val sndReg = traverseExpr(snd, ra, code)
        code += LDR(RetReg, imm(typeSize(t2)), Base)
        code += BL("malloc", Base)
        code += STRB(ra.next, regVal(RetReg))
        code += STR(RetReg, regShift(reg1, 4, update = false))
        code += STR(reg1, regVal(SP))
        // TODO this location probably needs to be changed
        variableLocation += (ident -> regVal(SP))
        ra.restore()
      case Decl(ArrayType(_type), Ident(ident), ArrayLiter(exprs)) =>
        val ret = traverseExpr(ArrayLiter(exprs), ra, code)
        code += STR(ret, regVal(SP))
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

      case Assign(Ident(ident), rhs) =>
        if (typeSize(st(Ident(ident))._2) == 4) code += STR(traverseExpr(rhs, ra, code), variableLocation(ident))
        else code += STRB(traverseExpr(rhs, ra, code), variableLocation(ident))
      case Assign(ArrayElem(ident, expr), rhs) =>
        val ret = traverseExpr(rhs, ra, code)
        ra.nextRm
        val arr = traverseExpr(ArrayElem(ident, expr), ra, code)
        code += STR(ret, arr)
        //TODO: remove duplication
      case Assign(FstPair(expr), rhs) =>
        /*
        val fst = traverseExpr(FstPair(expr), ra, code)
        val reg = traverseExpr(rhs, ra, code)
        var r = ra.next
        code += LDR(r, regVal(SP), Base)
        code += MOV(RetReg, r, Base)
        code += BL("p_check_null_pointer", Base)
        code += LDR(r, regVal(r), Base)
        code += STR(reg, regVal(r))
        runtimeError()
         */
        val null_msg: String = s"msg_$getDataMsgIndex"
        data(null_msg) =
          List(DWord(50),
            DAscii("NullReferenceError: dereference a null reference\\n\\0"))
        val pair_check_null_pointer = "p_check_null_pointer"
        if (!labels.contains(pair_check_null_pointer)) {
          labels(pair_check_null_pointer) =
            List(PUSH(LinkReg),
              CMP(RetReg, imm(0)),
              LDR(RetReg, label(null_msg), Base),
              BL("p_throw_runtime_error", EQ),
              POP(PC)
            )
        }
      case Assign(SndPair(expr), rhs) =>
        val reg = traverseExpr(rhs, ra, code)
        var r = ra.next
        code += LDR(r, regVal(SP), Base)
        code += MOV(RetReg, r, Base)
        code += BL("p_check_null_pointer", Base)
        code += LDR(r, regShift(r, 4, update = false), Base)
        code += STRB(reg, regVal(r))
        runtimeError()
        val null_msg: String = s"msg_$getDataMsgIndex"
        data(null_msg) =
          List(DWord(50),
            DAscii("NullReferenceError: dereference a null reference\\n\\0"))
        val pair_check_null_pointer = "p_check_null_pointer"
        if (!labels.contains(pair_check_null_pointer)) {
          labels(pair_check_null_pointer) =
            List(PUSH(LinkReg),
              CMP(RetReg, imm(0)),
              LDR(RetReg, label(null_msg), Base),
              BL("p_throw_runtime_error", EQ),
              POP(PC)
            )
        }

      case Free(expr) =>
        val r = ra.next
        code += LDR(traverseExpr(node, ra, code),SP, Base)
        code += MOV(RetReg, r, Base)
        val free_msg: String = s"msg_$getDataMsgIndex"
        data(free_msg) =
          List(DWord(50),
            DAscii("NullReferenceError: dereference a null reference\\n\\0"))
        runtimeError()
        val t = checkExprType(expr, node, new ListBuffer[String])
        t match{
          case PairType(_, _) =>
            code += BL("p_free_pair", Base)
            val pair_free_msg = "p_free_pair"
            if (!labels.contains(pair_free_msg)) {
              labels(pair_free_msg) =
                List(PUSH(LinkReg),
                  CMP(RetReg, imm(0)),
                  LDR(RetReg, label(free_msg), EQ),
                  B("p_throw_runtime_error", EQ),
                  PUSH(RetReg),
                  LDR(RetReg, RetReg, Base),
                  BL("free", Base),
                  LDR(RetReg, SP, Base),
                  LDR(RetReg, regShift(RetReg, 4, update = false), Base),
                  BL("free", Base),
                  POP(RetReg),
                  BL("fflush", Base),
                  POP(PC)
                )
            }
          case ArrayType(_) =>
            code += BL("p_free_array", Base)
            val neg_index_msg = s"msg_$getDataMsgIndex"
            data(neg_index_msg) =
              List(DWord(44),
                DAscii("ArrayIndexOutOfBoundsError: negative index\\n\\0"))
            val large_index_msg = s"msg_$getDataMsgIndex"
            data(large_index_msg) =
              List(DWord(45),
                DAscii("ArrayIndexOutOfBoundsError: index too large\\n\\0"))
            code += BL("p_free_array", Base)
            val array_free_msg = "p_free_array"
            if (!labels.contains(array_free_msg)) {
              labels(array_free_msg) =
                List(PUSH(LinkReg),
                  CMP(RetReg, imm(0)),
                  LDR(RetReg, label(free_msg), EQ),
                  B("p_throw_runtime_error",EQ),
                  BL("free", Base),
                  POP(PC)
                )
            }
            val array_bounds_msg = "p_check_array_bounds"
            if (!labels.contains(array_bounds_msg)) {
              labels(array_bounds_msg) =
                List(PUSH(LinkReg),
                  CMP(RetReg, imm(0)),
                  LDR(RetReg, label(neg_index_msg), LT),
                  BL("p_throw_runtime_error", LT),
                  LDR(reg(1), reg(1), Base),
                  CMP(RetReg, reg(1)),
                  LDR(RetReg, label(free_msg), CS),
                  BL("p_throw_runtime_error", CS),
                  POP(PC)
                )
            }
        }

      case Read(lhs: AstNode) =>
        val _type: Type = SemanticPass.checkExprType(lhs, node, new ListBuffer[String])
        val t = _type match {
          case WChar => "p_read_char"
          case WInt => "p_read_int"
        }
        if (!labels.contains(t)) {
          val read_msg = s"msg_$getDataMsgIndex"
          t match {
            case "p_read_char" =>
              data(read_msg) = List(
                DWord(4),
                DAscii(" %c\\0")
              )
            case "p_read_int" =>
              data(read_msg) = List(
                DWord(3),
                DAscii("%d\\0")
              )
          }
          labels(t) =
            List(PUSH(LinkReg),
              MOV(reg(1), RetReg, Base),
              LDR(RetReg, label(read_msg), Base),
              ADD(RetReg, RetReg, imm(4)),
              BL("scanf", Base),
              POP(PC))
        }
        code += ADD(ra.next, SP, imm(0))
        code += MOV(RetReg, ra.next, Base)
        code += BL(t, Base)


      case Print(expr: AstNode) =>
        //TODO: escape escape characters somehow in data strings? when they get written to the file it treats them literally
        val ret = traverseExpr(expr, ra, code)
        // TODO change this so it doesn't match explicit types
        SemanticPass.checkExprType(expr, expr, new ListBuffer[String]) match {
          case WString =>
            printString()
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next, ret, SB)
            code += MOV(RetReg, ra.next, Base)
            code += BL("p_print_string", Base)

          case PairType(t1, t2) =>


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
                  BL("printf", Base),
                  MOV(RetReg, imm(0), Base),
                  BL("fflush", Base),
                  POP(PC))
            }
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next, ret, SB)
            code += MOV(RetReg, ra.next, Base)
            code += BL("p_print_bool", Base)

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
                  BL("printf", Base),
                  MOV(RetReg, imm(0), Base),
                  BL("fflush", Base),
                  POP(PC))
            }
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next, ret, SB)
            code += MOV(RetReg, ra.next, Base)
            code += BL("p_print_int", Base)

          case WChar =>
            if (!ret.isInstanceOf[reg]) code += LDR(ra.next, ret, Base)
            code += MOV(RetReg, ra.next, Base)
            code += BL("putchar", Base)

          case ArrayType(_type) =>
            if(_type == WChar){
              //arrays of chars may be treated as strings
              printString()
              code += LDR(ra.next, regVal(SP), Base)
              code += MOV(RetReg, ra.next, Base)
              code += BL("p_print_string", Base)
            } else {
              //printing an array variable prints its address
              printReference()
              if (!ret.isInstanceOf[reg]) code += LDR(ra.next, ret, SB)
              code += MOV(RetReg, ra.next, Base)
              code += BL("p_print_reference", Base)
            }
          case PairType(_, _) =>
            val r = ra.next
            code += LDR(r, regVal(SP), Base)
            code += MOV(RetReg, r, Base)
            BL("p_print_reference", Base)
            val p_msg = s"msg_$getDataMsgIndex"
            data(p_msg) = List(
              DWord(3),
              DAscii("%p\\0")
            )
            val print_ref_msg = "p_print_reference"
            if (!labels.contains(print_ref_msg)) {
              labels(print_ref_msg) =
                List(PUSH(LinkReg),
                  MOV(reg(1), RetReg, Base),
                  LDR(RetReg, label(p_msg), Base),
                  ADD(RetReg, RetReg, imm(4)),
                  BL("printf", Base),
                  MOV(RetReg, imm(0), Base),
                  BL("fflush", Base),
                  POP(PC)
                )
            }

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
              BL("puts", Base),
              MOV(RetReg, imm(0), Base),
              BL("fflush", Base),
              POP(PC)
            )
        }
        traverse(Print(expr), ra, code)
        code += BL("p_print_ln", Base)

      case Return(expr) =>
        code += MOV(RetReg, traverseExpr(expr, ra, code), Base)
        code += POP(PC)

      case Exit(expr) =>
        code += MOV(RetReg, traverseExpr(expr, ra, code), Base)
        code += BL("exit", Base)

      case IfElse(cond, stat_true, stat_false) =>
        // TODO add stack pointer changes for new scopes
        val reg = traverseExpr(cond, ra, code)
        code += CMP(reg, imm(0))
        val fun1 = nextBranchIndex
        val fun2 = nextBranchIndex
        code += B(fun1, EQ)
        traverse(stat_true, ra, code)
        code += B(fun2, Base)
        code += funcName(fun1)
        traverse(stat_false, ra, code)
        code += funcName(fun2)

      case While(cond, stat) =>
        // TODO add stack changes for scoping
        val condLabel = nextBranchIndex
        val bodyLabel = nextBranchIndex
        code += B(condLabel, Base)
        code += funcName(bodyLabel)
        traverse(stat, ra, code)
        code += funcName(condLabel)
        traverse(cond, ra, code)
        val reg = traverseExpr(cond, ra, code)
        code += CMP(reg, imm(1))
        code += B(bodyLabel, EQ)

      case Scope(stat) => traverse(stat, ra, code)

      case Combine(stats) =>
        for (stat <- stats) {
          traverse(stat, ra, code)
        }

      case _ =>
      }
    }

  def traverseExpr(node: AstNode, ra: RegisterAllocator, code: ListBuffer[Mnemonic]): Register = {
    val spill = ra.size==2 // defines whether we are in a register spill state
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
      case ArrayLiter(exprs) =>
        val _type = checkExprType(exprs.head, exprs.head, new ListBuffer[String])
        code += LDR(RetReg, imm(exprs.length * typeSize(_type) + 4), Base)
        code += BL("malloc", Base)
        val ret =ra.nextRm
        code += MOV(ret, RetReg, Base)
        var location = 4
        for (expr <- exprs) {
          val ret = traverseExpr(expr, ra, code)
          code += STR(ret, regShift(ret, location, update = false))
          location += typeSize(_type)
        }
        code += LDR(ra.next, imm(exprs.size), Base)
        code += STR(ra.next, regVal(ret))
        ra.restore()
        ret
      case PairLiter => ???
      case FstPair(expr) =>
        traverseExpr(expr, ra, code)
      case SndPair(expr) =>
        traverseExpr(expr, ra, code)
      case Ident(x) =>
        // TODO this will probably need to change when pairs and arrays are implemented
        if (typeSize(st(Ident(x))._2) == 4) code += LDR(ra.next, variableLocation(x), Base)
        else code += LDR(ra.next, variableLocation(x), SB)
        ra.next
      case ArrayElem(Ident(x), elems) =>
        // TODO this wouldn't work for multi-dimensional arrays
        val arr = variableLocation(x)
        val arrReg = arr match {
          case regVal1: regVal => regVal1.reg
          case _ => arr
        }
        val arrLoc = ra.nextRm
        code += ADD(arrLoc, arrReg, imm(0))
        if (!labels.contains("p_check_array_bounds")) {
          val negMessage = s"msg_$getDataMsgIndex"
          data(negMessage) = List(
            DWord(44),
            DAscii("ArrayIndexOutOfBoundsError: negative index\\n\\0")
          )
          val largeMessage = s"msg_$getDataMsgIndex"
          data(largeMessage) = List(
            DWord(45),
            DAscii("ArrayIndexOutOfBoundsError: index too large\\n\\0")
          )
          runtimeError()
          labels("p_check_array_bounds") =
            List(PUSH(LinkReg),
              CMP(RetReg, imm(0)),
              LDR(RetReg, label(negMessage), LT),
              BL("p_throw_runtime_error", LT),
              LDR(reg(1), regVal(reg(1)), Base),
              CMP(RetReg, reg(1)),
              LDR(RetReg, label(largeMessage), CS),
              BL("p_throw_runtime_error", CS),
              POP(PC)
            )
        }
        for (elem <- elems) {
          val ret = traverseExpr(elem, ra, code)
          code += LDR(arrLoc, regVal(arrLoc), Base)
          code += MOV(RetReg, ret, Base)
          code += MOV(reg(1), arrLoc, Base)
          // TODO this constants should probably changed based on the size of the things in the array
          code += BL("p_check_array_bounds", Base)
          code += ADD(arrLoc, arrLoc, imm(4))
          code += ADD(arrLoc, arrLoc, lsl(ret, 2))
        }
        ra.restore()
        regVal(arrLoc)
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
        code += BL(name, Base)
        if (totalSize > 0) code += ADD(SP, SP, imm(totalSize))
        code += MOV(ra.next, RetReg, Base)
        RetReg

      case And(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1)
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next
        }
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          code += AND(res2, res2, res1)
        } else {
          code += AND(res1, res1, res2)
        }
        ra.restore()
        reg1

      // TODO factor out repeated code
      case Or(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1)
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next
        }
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          code += ORR(res2, res2, res1)
        } else {
          code += ORR(res1, res1, res2)
        }
        ra.restore()
        reg1

        // TODO check if reg1 should be moved with LDR or LDRSB
      case Greater(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1)
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next
        }
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, GT, res1, res2)
        } else {
          phonyCaseCompare(code, GT, reg1, ra.next)
        }
        ra.restore()
        reg1


      case GreaterEq(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1)
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next
        }
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, GE, res1, res2)
        } else {
          phonyCaseCompare(code, GE, reg1, ra.next)
        }
        ra.restore()
        reg1

      case Less(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1)
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next
        }
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, LT, res1, res2)
        } else {
          phonyCaseCompare(code, LT, reg1, ra.next)
        }
        ra.restore()
        reg1

      case LessEq(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1)
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next
        }
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, LE, res1, res2)
        } else {
          phonyCaseCompare(code, LE, reg1, ra.next)
        }
        ra.restore()
        reg1

      case Eq(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1)
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next
        }
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, EQ, res1, res2)
        } else {
          phonyCaseCompare(code, EQ, reg1, res2)
        }
        ra.restore()
        reg1

      case NotEq(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1)
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next
        }
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, NE, res1, res2)
        } else {
          phonyCaseCompare(code, NE, reg1, ra.next)
        }
        ra.restore()
        reg1


      /**
       * @return a register containing the result of expr1 PLUS expr2
       *
       * Note on 'spill' state:
       * If there are only 2 remaining registers in the RegisterAllocator, this function will
       * operate in a 'spill' state; upon computing the first expression its value will be pushed to
       * the stack and the register freed for use when computing expr2.
       */
      case Plus(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1) // now that res1 is on the stack, its old reg is free to be overwritten
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next // res2 now contains the result of expr 2
        }
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          code += ADDS(res2, res1, res2)
        } else { // needs separate ADD cases, since the res1 or res2 will be the lower register address depending on whether we're in a spill state
          code += ADDS(res1, res1, res2)
        }
        intOverflow()
        code += BL("p_throw_overflow_error", VS)
        ra.restore()
        reg1

      case Minus(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1) // now that res1 is on the stack, its old reg is free to be overwritten
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next // res2 now contains the result of expr 2
        }
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          code += SUBS(res2, res1, res2)
        } else { // needs separate ADD cases, since the res1 or res2 will be the lower register address depending on whether we're in a spill state
          code += SUBS(res1, res1, res2)
        }
        intOverflow()
        code += BL("p_throw_overflow_error", VS)
        ra.restore()
        reg1

      case Mult(expr1, expr2) =>
        println(expr1)
        println(ra.getAvailable)
        var res1 = traverseExpr(expr1, ra, code)
        println(ra.getAvailable)
        println("\n")
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1) // now that res1 is on the stack, its old reg is free to be overwritten
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next // res2 now contains the result of expr 2
        }
        if (spill) {
          println("got to spill case")
          res1 = ra.getAvailable(1)
          code += POP(res1)
          code += SMULL(res1, res2, res1, res2)
          code += CMP(res2, asr(res1, 31))
        } else { // needs separate ADD cases, since the res1 or res2 will be the lower register address depending on whether we're in a spill state
          code += SMULL(res1, res2, res1, res2)
          code += CMP(res2, asr(res1, 31))
        }
        intOverflow()
        code += BL("p_throw_overflow_error", NE)
        ra.restore()
        reg1

      case Div(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1) // now that res1 is on the stack, its old reg is free to be overwritten
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next // res2 now contains the result of expr 2
        }
        if (spill) {
          println("got to spill case")
          res1 = ra.getAvailable(1)
          code += POP(res1)
          code += MOV(RetReg, res1, Base) // <-- todo?
          code += MOV(reg(1), res2, Base)
          divByZeroError()
          code += BL("p_check_divide_by_zero", Base)
          code += BL("__aeabi_idiv", Base)
          code += MOV(res1, RetReg, Base)
        } else { // needs separate ADD cases, since the res1 or res2 will be the lower register address depending on whether we're in a spill state
          code += MOV(RetReg, res1, Base)
          code += MOV(reg(1), res2, Base)
          divByZeroError()
          code += BL("p_check_divide_by_zero", Base)
          code += BL("__aeabi_idiv", Base)
          code += MOV(res1, RetReg, Base)
        }
        ra.restore()
        reg1

      case Mod(expr1, expr2) =>
        var res1 = traverseExpr(expr1, ra, code)
        val reg1 = if (spill) ra.next else ra.nextRm
        if (!res1.isInstanceOf[reg]) {
          code += LDR(reg1, res1, SB)
          res1 = reg1
        }
        if (spill) code += PUSH(res1) // now that res1 is on the stack, its old reg is free to be overwritten
        var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), code)
        if (!res2.isInstanceOf[reg]) {
          code += LDR(ra.next, res2, SB)
          res2 = ra.next // res2 now contains the result of expr 2
        }
        if (spill) {
          println("got to spill case")
          res1 = ra.getAvailable(1)
          code += POP(res1)
          code += MOV(RetReg, reg1, Base) // <-- todo?
          code += MOV(reg(1), ra.next, Base)
          divByZeroError()
          code += BL("p_check_divide_by_zero", Base)
          code += BL("__aeabi_idivmod", Base)
          code += MOV(reg1, reg(1), Base)
        } else { // needs separate ADD cases, since the res1 or res2 will be the lower register address depending on whether we're in a spill state
          code += MOV(RetReg, res1, Base)
          code += MOV(reg(1), res2, Base)
          divByZeroError()
          code += BL("p_check_divide_by_zero", Base)
          code += BL("__aeabi_idivmod", Base)
          code += MOV(res1, reg(1), Base)
        }
        ra.restore()
        reg1

      case Negate(expr) =>
        val reg = traverseExpr(expr, ra, code)
        if (!reg.isInstanceOf[reg]) code += LDR(reg, reg, Base)
        intOverflow()
        code += RSBS(reg, reg, imm(0))
        code += BL("p_throw_overflow_error", VS)
        reg

      case Not(expr) =>
        val reg = traverseExpr(expr, ra, code)
        if (!reg.isInstanceOf[reg]) code += LDR(reg, reg, SB)
        code += EOR(reg, reg, imm(1))
        reg

      case Chr(expr) =>
        val reg = traverseExpr(expr, ra, code)
        reg

      case Ord(expr) =>
        val reg = traverseExpr(expr, ra, code)
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
          BL("printf", Base),
          MOV(RetReg, imm(0), Base),
          BL("fflush", Base),
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
     *
     *@param suffix1  : The suffix of the positive branch of the compare.
     *                For example, when pattern matching a Greater than comparison, suffix1 should be GT
     *                Since this function should only be called in comparison cases, for suffix1 in {Base,CS,SB,VS}
     *                the output is undefined
     */
    var suffix2: Suffix = null
    suffix1 match {
      case GT => suffix2 = LE
      case GE => suffix2 = LT
      case LT => suffix2 = GE
      case LE => suffix2 = GT
      case EQ => suffix2 = NE
      case NE => suffix2 = EQ

      //All other cases should not be passed into this function as they arent compare suffixes.
      //To suppress warnings a catch all case is added, but the generated code would not function as expected.
      case _ =>
        suffix2 = Base
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
          sb.append(line.toString + "\n\t")
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
        sb.append(line.toString + "\n\t")
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