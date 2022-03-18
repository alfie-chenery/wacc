package parsers

import parsers.SemanticPass.{checkExprType, st}
import parsers.preDefinedFuncs._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object CodeGen{

  import parsers.Assembly._
  import parsers.Ast._
  import parsers.MathLib._

  // each of these maps represent a section of the output code that can be appended to
  // linkedHashMaps, since data and labels should follow a specific order
  val data = new mutable.LinkedHashMap[String, List[Mnemonic]]
  val labels = new mutable.LinkedHashMap[String, List[Mnemonic]]
  val variableLocation = new mutable.LinkedHashMap[String, Register]
  var currentShift = 0

  val mathFuncs = List("sin", "cos", "tan", "pow", "fact", "fabs", "sqrt")

  /** Returns the next available 'msg' index in data */
  def getDataMsgIndex: Int = {
    data.size
  }

  /** Value used for conditional branch labels L0, L1 etc */
  var branchIndex: Int = -1
  def nextBranchIndex: String = {
    branchIndex += 1
    "L" + branchIndex.toString
  }

  /** value used to increase offset when reading multiple times */
  var readCounter: Int = 0 //will be set at start of new scopes
  def getReadOffset: Int = {
    val result = 4 * readCounter //4 bytes * number of reads so far
    readCounter -= 1
    result

  }

  def traverse(node: AstNode, ra: RegisterAllocator, va: VfpAllocator, code: ListBuffer[Mnemonic]): Unit = {
    node match {
      case Program(funcs, stat) =>
        for (func <- funcs) {
          traverse(func, ra, va, code)
        }

        readCounter = readsInScope(stat)

        code += funcName("main")
        code += PUSH(LinkReg)
        val assignments = assignmentsInScope(stat)
        currentShift = assignments
        if (assignments > 0) {
          // 1024 is the maximum immediate value because of shifting
          for (_ <- 0 until assignments / 1024) code += SUB(SP, SP, imm(1024))
          code += SUB(SP, SP, imm(assignments % 1024))
        }

        traverse(stat, ra, va, code)
        if (assignments > 0) {
          code += ADD(SP, SP, imm(assignments % 1024))
          for (_ <- 0 until assignments / 1024) code += ADD(SP, SP, imm(1024))
        }
        code += LDR(RetReg, imm(0), Base)
        code += POP(PC)
        code += LTORG

      case Func((_, Ident(name)), ParamList(params), stat) =>
        val assignments = assignmentsInScope(stat)
        currentShift = assignments
        for (param <- params) {
          currentShift += typeSize(param._type)
          variableLocation += (param.ident.ident -> regShift(SP, currentShift, update = false))
        }
        currentShift = assignments
        readCounter = readsInScope(stat)
        code += funcName(name)
        code += PUSH(LinkReg)
        if (assignments > 0) code += SUB(SP, SP, imm(assignments))
        traverse(stat, ra, va, code)
        if (assignments > 0) code += ADD(SP, SP, imm(assignments))
        code += POP(PC)
        code += LTORG

      // <Stat>
      case Decl(PairType(_, _), Ident(ident), expr) =>
        // TODO this location probably needs to be changed
        currentShift -= 4
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift, update = false)
        variableLocation += (ident -> location)
        val reg1 = traverseExpr(expr, ra, va, code)
        code += STR(reg1, location)
        ra.restore()
      case Decl(ArrayType(_), Ident(ident), ArrayLiter(exprs)) =>
        val ret = traverseExpr(ArrayLiter(exprs), ra, va, code)
        variableLocation += (ident -> regVal(SP))
        code += STR(ret, regVal(SP))
        ra.restore()
      case Decl(Number, Ident(ident), rhs) =>
        val r = ra.next
        traverseExpr(rhs, ra, va, code)
        currentShift -= 4
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift, update = false)
        variableLocation += (ident -> location)
        code += STR(r, location)
      case Decl(WBool, Ident(ident), rhs) =>
        val r = ra.next
        traverseExpr(rhs, ra, va, code)
        currentShift -= 1
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift, update = false)
        variableLocation += (ident -> location)
        code += STRB(r, location)
      case Decl(WChar, Ident(ident), rhs) =>
        val r = ra.next
        traverseExpr(rhs, ra, va, code)
        currentShift -= 1
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift, update = false)
        variableLocation += (ident -> location)
        code += STRB(r, location)
      case Decl(WString, Ident(ident), rhs) =>
        val r = ra.next
        traverseExpr(rhs, ra, va, code)
        currentShift -= 4
        val location = if (currentShift == 0) regVal(SP) else regShift(SP, currentShift, update = false)
        variableLocation += (ident -> location)
        code += STR(r, location)

      case Assign(Ident(ident), rhs) =>
        if (typeSize(st(Ident(ident))._2) == 4) code += STR(traverseExpr(rhs, ra, va, code), variableLocation(ident))
        else code += STRB(traverseExpr(rhs, ra, va, code), variableLocation(ident))
      case Assign(ArrayElem(ident, expr), rhs) =>
        val ret = traverseExpr(rhs, ra, va, code)
        ra.nextRm
        val arr = traverseExpr(ArrayElem(ident, expr), ra, va, code)
        code += STR(ret, arr)
        //TODO: remove duplication
      case Assign(FstPair(expr), rhs) =>
        val ret = traverseExpr(rhs, ra, va, code)
        ra.nextRm
        val fst = traverseExpr(FstPair(expr), ra, va, code)
        code.remove(code.length-1)
        // TODO probably factor out this check and not call checkExprType
        if (typeSize(checkExprType(rhs, rhs, new ListBuffer[String])) == 4) code += STR(ret, regVal(fst))
        else code += STRB(ret, regVal(fst))
        checkNullPointer()
        ra.restore()

      case Assign(SndPair(expr), rhs) =>
        val ret = traverseExpr(rhs, ra, va, code)
        ra.nextRm
        val snd = traverseExpr(SndPair(expr), ra, va, code)
        code.remove(code.length-1)
        if (typeSize(checkExprType(rhs, rhs, new ListBuffer[String])) == 4) code += STR(ret, regVal(snd))
        else code += STRB(ret, regVal(snd))
        checkNullPointer()
        ra.restore()

      case Free(expr) =>
        code += MOV(RetReg, traverseExpr(expr, ra, va, code), Base)
        val free_msg: String = s"msg_$getDataMsgIndex"
        // TODO This should probably call check null pointer
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
                  LDR(RetReg, regVal(RetReg), Base),
                  BL("free", Base),
                  LDR(RetReg, regVal(SP), Base),
                  LDR(RetReg, regShift(RetReg, 4, update = false), Base),
                  BL("free", Base),
                  POP(RetReg),
                  BL("free", Base),
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
                  LDR(reg1, reg1, Base),
                  CMP(RetReg, reg1),
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
          case _ =>
            throw RuntimeException(s"Incompatible type: ${_type.toString} with call to Read")
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
              MOV(reg1, RetReg, Base),
              LDR(RetReg, label(read_msg), Base),
              ADD(RetReg, RetReg, imm(4)),
              BL("scanf", Base),
              POP(PC))
        }

        code += ADD(ra.next, SP, imm(getReadOffset))
        code += MOV(RetReg, ra.next, Base)
        code += BL(t, Base)


      case Print(PairLiter) =>
        printReference()
        val ret = traverseExpr(PairLiter, ra, va, code)
        code += MOV(RetReg, ret, Base)
        code += BL("p_print_reference", Base)
      case Print(expr: AstNode) =>
        val ret = traverseExpr(expr, ra, va, code)
        // TODO change this so it doesn't match explicit types
        SemanticPass.checkExprType(expr, expr, new ListBuffer[String]) match {
          case WString =>
            printString()
            if (!ret.isInstanceOf[TempReg]) code += LDR(ra.next, ret, SB)
            code += MOV(RetReg, ra.next, Base)
            code += BL("p_print_string", Base)

          case WBool =>
            printBool()
            if (!ret.isInstanceOf[TempReg]) code += LDR(ra.next, ret, SB)
            code += MOV(RetReg, ra.next, Base)
            code += BL("p_print_bool", Base)

          case WInt =>
            printInt()
            if (!ret.isInstanceOf[TempReg]) code += LDR(ra.next, ret, SB)
            code += MOV(RetReg, ra.next, Base)
            code += BL("p_print_int", Base)

          case WChar =>
            if (!ret.isInstanceOf[TempReg]) code += LDR(ra.next, ret, Base)
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
              if (!ret.isInstanceOf[TempReg]) code += LDR(ra.next, ret, SB)
              code += MOV(RetReg, ra.next, Base)
              code += BL("p_print_reference", Base)
            }
          case PairType(_, _) =>
            printReference()
            val r = ra.next
            code += MOV(RetReg, r, Base)
            code += BL("p_print_reference", Base)

        }

      case Println(expr) =>
        printLn()
        traverse(Print(expr), ra, va, code)
        code += BL("p_print_ln", Base)

      case Return(expr) =>
        code += MOV(RetReg, traverseExpr(expr, ra, va, code), Base)
        code += POP(PC)

      case Exit(expr) =>
        code += MOV(RetReg, traverseExpr(expr, ra, va, code), Base)
        code += BL("exit", Base)

      case IfElse(cond, stat_true, stat_false) =>
        // TODO add stack pointer changes for new scopes
        val reg = traverseExpr(cond, ra, va, code)
        code += CMP(reg, imm(0))
        val fun1 = nextBranchIndex
        val fun2 = nextBranchIndex
        code += B(fun1, EQ)
        traverse(stat_true, ra, va, code)
        code += B(fun2, Base)
        code += funcName(fun1)
        traverse(stat_false, ra, va, code)
        code += funcName(fun2)

      case While(cond, stat) =>
        // TODO add stack changes for scoping
        val condLabel = nextBranchIndex
        val bodyLabel = nextBranchIndex
        code += B(condLabel, Base)
        code += funcName(bodyLabel)
        traverse(stat, ra, va, code)
        code += funcName(condLabel)
        traverse(cond, ra, va, code)
        val reg = traverseExpr(cond, ra, va, code)
        code += CMP(reg, imm(1))
        code += B(bodyLabel, EQ)

      case Scope(stat) => traverse(stat, ra, va, code)

      case Combine(stats) =>
        for (stat <- stats) {
          traverse(stat, ra, va, code)
        }


      case _ =>
      }
    }

  def traverseExpr(node: AstNode, ra: RegisterAllocator, va: VfpAllocator, code: ListBuffer[Mnemonic]): Register = {
    val spill = ra.size==2 // defines whether we are in a register spill state
    node match {
      case IntLiter(x) =>
        code += LDR(ra.next, imm(x), Base)
        ra.next
      case Negate(IntLiter(x)) => traverseExpr(IntLiter(-x), ra, va, code)
      case BoolLiter(b) =>
        code += MOV(ra.next, imm(if (b) 1 else 0), Base)
        ra.next
      case CharLiter(c) =>
        // todo: is this the correct solution or does it only solve one case?
        val _c = if (c == "\\u0000") imm(0) else immc(c)
        code += MOV(ra.next, _c, Base)
        ra.next
      case StrLiter(s) =>
        val int_msg = s"msg_$getDataMsgIndex"
        data(int_msg) = List(DWord(s.replace("\\", "").length), DAscii(s))
        code += LDR(ra.next, label(int_msg), Base)
        ra.next
      case ArrayLiter(exprs) =>
        val _type = checkExprType(exprs.head, exprs.head, new ListBuffer[String])
        code += LDR(RetReg, imm(exprs.length * typeSize(_type) + 4), Base)
        code += BL("malloc", Base)
        val r1 = ra.nextRm
        code += MOV(r1, RetReg, Base)
        var location = 4
        for (expr <- exprs) {
          val ret = traverseExpr(expr, ra, va, code)
          code += STR(ret, regShift(r1, location, update = false))
          location += typeSize(_type)
        }
        code += LDR(ra.next, imm(exprs.size), Base)
        code += STR(ra.next, regVal(r1))
        ra.restore()
        r1
      case PairLiter =>
        code += LDR(ra.next, imm(0), Base)
        ra.next

      // TODO implement these correctly
      case FstPair(expr) =>
        val ret = traverseExpr(expr, ra, va, code)
        code += MOV(RetReg, ret, Base)
        checkNullPointer()
        code += BL("p_check_null_pointer", Base)
        code += LDR(ret, regVal(ret), Base)
        val suffix = if (typeSize(checkExprType(expr, expr, new ListBuffer[String])) == 4) Base
        else SB
        code += LDR(ret, regVal(ret), suffix)
        ret
      case SndPair(expr) =>
        val ret = traverseExpr(expr, ra, va, code)
        code += MOV(RetReg, ret, Base)
        checkNullPointer()
        code += BL("p_check_null_pointer", Base)
        code += LDR(ret, regShift(ret, 4, update = false), Base)
        val suffix = if (typeSize(checkExprType(expr, expr, new ListBuffer[String])) == 4) Base
        else SB
        code += LDR(ret, regVal(ret), suffix)
        ret
      case NewPair(fst, snd) =>
        code += LDR(RetReg, imm(8), Base)
        code += BL("malloc", Base)
        code += MOV(ra.next, RetReg, Base)
        val r1 = ra.nextRm
        val fstReg = traverseExpr(fst, ra, va, code)
        code += LDR(RetReg, imm(typeSize(checkExprType(fst, fst, new ListBuffer[String]))), Base)
        code += BL("malloc", Base)
        code += STR(fstReg, regVal(RetReg))
        code += STR(RetReg, regVal(r1))
        val sndReg = traverseExpr(snd, ra, va, code)
        code += LDR(RetReg, imm(typeSize(checkExprType(snd, snd, new ListBuffer[String]))), Base)
        code += BL("malloc", Base)
        // TODO this should be STRB for chars
        code += STR(ra.next, regVal(RetReg))
        code += STR(RetReg, regShift(r1, 4, update = false))
        r1
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
              LDR(reg1, regVal(reg1), Base),
              CMP(RetReg, reg1),
              LDR(RetReg, label(largeMessage), CS),
              BL("p_throw_runtime_error", CS),
              POP(PC)
            )
        }
        for (elem <- elems) {
          val ret = traverseExpr(elem, ra, va, code)
          code += LDR(arrLoc, regVal(arrLoc), Base)
          code += MOV(RetReg, ret, Base)
          code += MOV(reg1, arrLoc, Base)
          // TODO this constants should probably changed based on the size of the things in the array
          code += BL("p_check_array_bounds", Base)
          code += ADD(arrLoc, arrLoc, imm(4))
          val _type = typeSize(checkExprType(ArrayElem(Ident(x), elems), ArrayElem(Ident(x), elems), new ListBuffer[String]))
          val loc = if (_type == 4) lsl(ret, 2) else ret
          code += ADD(arrLoc, arrLoc, loc)
        }
        ra.restore()
        regVal(arrLoc)
      case ParensExpr(expr) => traverseExpr(expr, ra, va, code)
      case Call(Ident(name), ArgList(args)) =>
        if (!mathFuncs.contains(name)) {
          var totalSize = 0
          for (arg <- args) {
            val size = typeSize(checkExprType(arg, arg, new ListBuffer[String]))
            totalSize += size
            val reg = traverseExpr(arg, ra, va, code)
            if (size == 4) code += STR(reg, regShift(SP, -size, update = true))
            else code += STRB(reg, regShift(SP, -size, update = true))
          }
          code += BL(name, Base)
          if (totalSize > 0) code += ADD(SP, SP, imm(totalSize))
          code += MOV(ra.next, RetReg, Base)
        }else{
          name match {
            case "sin" => sin(ra, va)
            case "cos" => cos(ra, va)
            case "tan" => tan(ra, va)
            case "pow" => pow(ra, va)
            case "fact" => fact(ra, va)
            case "fabs" => fabs(ra, va)
            case "sqrt" => if (checkExprType(args.head, node, new ListBuffer[String]) == WInt) sqrtInt(ra, va) else sqrt(ra, va)
          }
        }
        RetReg

      case And(expr1, expr2) =>
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)

        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          code += AND(res2, res2, res1)
        } else {
          code += AND(res1, res1, res2)
        }
        ra.restore()
        r1

      // TODO factor out repeated code
      case Or(expr1, expr2) =>
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)

        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          code += ORR(res2, res2, res1)
        } else {
          code += ORR(res1, res1, res2)
        }
        ra.restore()
        r1

        // TODO check if r1 should be moved with LDR or LDRSB
      case Greater(expr1, expr2) =>
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)

        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, GT, res1, res2)
        } else {
          phonyCaseCompare(code, GT, r1, ra.next)
        }
        ra.restore()
        r1


      case GreaterEq(expr1, expr2) =>
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)

        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, GE, res1, res2)
        } else {
          phonyCaseCompare(code, GE, r1, ra.next)
        }
        ra.restore()
        r1

      case Less(expr1, expr2) =>
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)

        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, LT, res1, res2)
        } else {
          phonyCaseCompare(code, LT, r1, ra.next)
        }
        ra.restore()
        r1

      case LessEq(expr1, expr2) =>
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)

        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, LE, res1, res2)
        } else {
          phonyCaseCompare(code, LE, r1, ra.next)
        }
        ra.restore()
        r1

      case Eq(expr1, expr2) =>
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)

        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, EQ, res1, res2)
        } else {
          phonyCaseCompare(code, EQ, r1, res2)
        }
        ra.restore()
        r1

      case NotEq(expr1, expr2) =>
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)

        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          phonyCaseCompare(code, NE, res1, res2)
        } else {
          phonyCaseCompare(code, NE, r1, ra.next)
        }
        ra.restore()
        r1


      /**
       * @return a register containing the result of expr1 PLUS expr2
       *
       * Note on 'spill' state:
       * If there are only 2 remaining registers in the RegisterAllocator, this function will
       * operate in a 'spill' state; upon computing the first expression its value will be pushed to
       * the stack and the register freed for use when computing expr2.
       */
      case Plus(expr1, expr2) =>
        val t1 = checkExprType(expr1, node, new ListBuffer[String])
        val t2 = checkExprType(expr2, node, new ListBuffer[String])
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)
        ra.next
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          if (t1 == WInt && t2 == WInt) {
            code += ADD(res2, res1, res2)
          }else{
            val v = va.next
            val v1 = va.next
            code += FMSR(v, res1)
            code += FMSR(v1, res2)
            code += FADD(v, v, v1)
            code += FMRS(res2, v)
          }
        } else { // needs separate ADD cases, since the res1 or res2 will be the lower register address depending on whether we're in a spill state
          if (t1 == WInt && t2 == WInt) {
            code += ADD(res1, res1, res2)
          }else{
            val v = va.next
            val v1 = va.next
            code += FMSR(v, res1)
            code += FMSR(v1, res2)
            code += FADD(v, v, v1)
            code += FMRS(res1, v)
          }
        }
        intOverflow()
        code += BL("p_throw_overflow_error", VS)
        ra.restore()
        r1

      case Minus(expr1, expr2) =>

        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)
        val t1 = checkExprType(expr1, node, new ListBuffer[String])
        val t2 = checkExprType(expr2, node, new ListBuffer[String])
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          if (t1 == WInt && t2 == WInt) {
            code += SUB(res2, res1, res2)
          }else{
            val v = va.next
            val v1 = va.next
            code += FMSR(v, res1)
            code += FMSR(v1, res2)
            code += FSUB(v, v, v1)
            code += FMRS(res2, v)
          }
        } else {
          val v = va.next
          val v1 = va.next
          code += FMSR(v, res1)
          code += FMSR(v1, res2)
          code += FSUB(v, v, v1)
          code += FMRS(res1, v)
        }
        intOverflow()
        code += BL("p_throw_overflow_error", VS)
        ra.restore()
        r1

      case Mult(expr1, expr2) =>
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)
        val t1 = checkExprType(expr1, node, new ListBuffer[String])
        val t2 = checkExprType(expr2, node, new ListBuffer[String])
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          if (t1 == WInt && t2 == WInt) {
            code += SMULL(res1, res2, res1, res2)
            code += CMP(res2, asr(res1, 31))
          }else{
            val v = va.next
            val v1 = va.next
            code += FMSR(v, res1)
            code += FMSR(v1, res2)
            FMUL(v, v, v1)
            FCMP(v1, asr(v, 31))
            code += FMRS(res1, v)
            code += FMRS(res2, v1)
          }
        } else {
          if (t1 == WInt && t2 == WInt) {
            code += SMULL(res1, res2, res1, res2)
            code += CMP(res2, asr(res1, 31))
          }else{
            val v = va.next
            val v1 = va.next
            code += FMSR(v, res1)
            code += FMSR(v1, res2)
            FMUL(v, v, v1)
            FCMP(v1, asr(v, 31))
            code += FMRS(res1, v)
            code += FMRS(res2, v1)
          }
        }
        intOverflow()
        code += BL("p_throw_overflow_error", NE)
        ra.restore()
        r1

      case Div(expr1, expr2) =>
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)
        val t1 = checkExprType(expr1, node, new ListBuffer[String])
        val t2 = checkExprType(expr2, node, new ListBuffer[String])
        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          if (t1 == WInt && t2 == WInt) {
            code += MOV(RetReg, res1, Base)
            code += MOV(reg1, res2, Base)
            divByZeroError()
            code += BL("p_check_divide_by_zero", Base)
            code += BL("__aeabi_idiv", Base)
            code += MOV(res1, RetReg, Base)
          }else{
            val v = va.next
            val v1 = va.next
            code += FMSR(v, res1)
            code += FMSR(v1, res2)
            code += FDIV(v, v, v1)
            code += FMRS(res1, v)
          }
        } else { // needs separate ADD cases, since the res1 or res2 will be the lower register address depending on whether we're in a spill state
          if (t1 == WInt && t2 == WInt) {
            code += MOV(RetReg, res1, Base)
            code += MOV(reg1, res2, Base)
            divByZeroError()
            code += BL("p_check_divide_by_zero", Base)
            code += BL("__aeabi_idiv", Base)
            code += MOV(res1, RetReg, Base)
          }else{
            val v = va.next
            val v1 = va.next
            FMSR(v, res1)
            FMSR(v1, res2)
            FDIV(v, v, v1)
            FMRS(res1, v)
          }
        }
        ra.restore()
        r1

      case Mod(expr1, expr2) =>
        var (r1, res1, res2) = traverseBinExpr(expr1, expr2, ra, va, code, spill)

        if (spill) {
          res1 = ra.getAvailable(1)
          code += POP(res1)
          code += MOV(RetReg, r1, Base) // <-- todo?
          code += MOV(reg1, ra.next, Base)
          divByZeroError()
          code += BL("p_check_divide_by_zero", Base)
          code += BL("__aeabi_idivmod", Base)
          code += MOV(r1, reg1, Base)
        } else { // needs separate ADD cases, since the res1 or res2 will be the lower register address depending on whether we're in a spill state
          code += MOV(RetReg, res1, Base)
          code += MOV(reg1, res2, Base)
          divByZeroError()
          code += BL("p_check_divide_by_zero", Base)
          code += BL("__aeabi_idivmod", Base)
          code += MOV(res1, reg1, Base)
        }
        ra.restore()
        r1

      case Negate(expr) =>
        val t = checkExprType(expr, node, new ListBuffer[String])
        val reg = traverseExpr(expr, ra, va, code)
        if (!reg.isInstanceOf[TempReg]) code += LDR(reg, reg, Base)
        intOverflow()
        if (t == WInt) {
          code += RSBS(reg, reg, imm(0))
          code += BL("p_throw_overflow_error", VS)
        }else{
          val v = va.next
          val v1 = va.next
          FMSR(v, reg)
          FLDM(v1, imm(-1))
          FMUL(v, v, v1)
          FMRS(reg, v)
        }
        reg

      case Not(expr) =>
        val reg = traverseExpr(expr, ra, va, code)
        if (!reg.isInstanceOf[TempReg]) code += LDR(reg, reg, SB)
        code += EOR(reg, reg, imm(1))
        reg

      case Chr(expr) =>
        val reg = traverseExpr(expr, ra, va, code)
        reg

      case Ord(expr) =>
        val reg = traverseExpr(expr, ra, va, code)
        reg

      case Len(expr) =>
        val reg = traverseExpr(expr, ra, va, code)
        code += LDR(reg, regVal(reg), Base)
        ra.next

    }
  }

  def traverseBinExpr(expr1: Expr, expr2: Expr, ra: RegisterAllocator, va: VfpAllocator, code: ListBuffer[Mnemonic], spill: Boolean): (Register,Register,Register) = {
    /** Function to reduce duplication in dealing with binary expressions */
    var res1 = traverseExpr(expr1, ra, va, code)
    val r1 = if (spill) ra.next else ra.nextRm
    if (!res1.isInstanceOf[TempReg]) {
      code += LDR(r1, res1, SB)
      res1 = r1
    }
    if (spill) code += PUSH(res1)
    var res2 = traverseExpr(expr2, new RegisterAllocator(ra.getAvailable), new VfpAllocator(va. getAvailable), code)
    if (!res2.isInstanceOf[TempReg]) {
      code += LDR(ra.next, res2, SB)
      res2 = ra.next
    }

    (r1, res1, res2)
  }


  def phonyCaseCompare(code: ListBuffer[Mnemonic], suffix1: Suffix, r1: Register, r2: Register): Unit = {
    /**
     * Function to factor out repeated code for comparison expressions
     * Can be seen as a case containing all comparison cases as sub cases,
     * But this isn't reflected in the current AST implementation hence phony
     * The main case in traverse should ensure r1 and r2 store the correct
     * values before calling this function
     *
     * */
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

    code += CMP(r1, r2)
    code += MOV(r1, imm(1), suffix1)
    code += MOV(r1, imm(0), suffix2)
  }

  def compile(node: AstNode, ra: RegisterAllocator = new RegisterAllocator(), va: VfpAllocator = new VfpAllocator()): String = {
    val sb = new StringBuilder()
    var code: ListBuffer[Mnemonic] = ListBuffer()
    traverse(node, ra, va, code)
    // todo: refactor compilation so that a file is created and written to (without a sb)
    //  and the file is deleted if an error is detected

    // todo: refactor so that optimizations can be performed on the intermediate representation?
    //  perhaps registers are the only objects that need to be redefined, for now... (?)

    // CFG optimisations operate only on main program, not data or helper functions
    val cfg = new ControlFlowGraph(code)
//    println(s"Original CFG: $cfg")
    // replaces all TempRegs with ScratchRegs in the code according to regMapping
    LiveAnalysis.liveVariableAnalysis(cfg)
//    println(s"CFG after live variable analysis: $cfg")

    code = cfg.toAssembly

    if (data.nonEmpty) {
      sb.append(".data\n\n") // todo: could these not be added as mnemonic labels for consistency?
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
    // todo: the formatting might fail once IfElse is implemented... (?)
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
      case WFloat => 4
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

  def readsInScope(stats: AstNode): Int = {
    var size = 0
    stats match {
//      case Program(_,stat) => readsInScope(stat)
//      case Func(_,_, stat) => readsInScope(stat)
//      case Read(_) => size += 1
      case Combine(stats) =>
        for (stat <- stats) {
          stat match {
            case Read(_) => size += 1
            case _ =>
          }
        }
    }
    size - 1
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