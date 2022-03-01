package parsers

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import parsers.Assembly.retReg

object CodeGen{
  /**
   * Code Generation pass
   */
   import parsers.Ast._

  // each of these maps represent a section of the output code that can be appended to
  // linkedHashMaps, since data and labels should follow a specific order
  val data = new mutable.LinkedHashMap[String, String]
  val labels = new mutable.LinkedHashMap[String, String]
  val errors = new mutable.LinkedHashMap[String, String]
  // todo: refactor so that maps/buffers automatically indent/format strings ?
  // todo: reformat to use instruction ADT instead of strings

  /**
   * Returns the next available 'msg' index in data
   */
  def getDataMsgIndex: Int = {
    data.size
  }

  //TODO make use of availableRegs to replace
  def traverse(node: AstNode, ra: RegisterAllocator): String = {
    node match {
      case Program(funcs, stat) =>
        var program = "main:\n\t" +
          "PUSH {lr}\n\t"

        val assignmentSize = assignmentsInScope(stat)
        if (assignmentSize > 0) program += "SUB sp, sp, #" + assignmentSize + "\n\t"

        for (func <- funcs) {
          program += traverse(func, ra)
        }
        program += traverse(stat, ra)

        if (assignmentSize > 0) program += "ADD sp, sp, #" + assignmentSize + "\n\t"

        program += "LDR " + retReg + ", =0\n\t" +
          "POP {pc}\n\t" +
          ".ltorg"
        program

      case Func((_type, Ident(name)), ParamList(params), stat) =>
        // TODO deal with params
        var program = name + ":\n\t" +
          "PUSH {lr}\n\t"
          val assignmentSize = assignmentsInScope(stat)
          if (assignmentSize > 0) program += "SUB sp, sp, #" + assignmentSize + "\n\t"
          program += traverse(stat, ra)
          if (assignmentSize > 0) program += "ADD sp, sp, #" + assignmentSize + "\n\t"
          program += "POP {lr}\n\t" +
            ".ltorg"
        program

      case Param(_type, ident) => ???

      // <Stat>
      case Skip => ""

      case Decl(PairType(t1, t2), ident, PairLiter) => ???
      case Decl(_type, ident, rhs) =>
        // TODO this is not gonna work (see many variables example)
        val reg = ra.next()
        s""" SUB sp, sp, #4
           | LDR $reg, =${traverse(rhs,ra)}
           | STR $reg, [sp]
           | ADD sp,
           |""".stripMargin

      case Assign(lhs, rhs) => ???

      case Read(lhs: AstNode) =>
        // todo: needs to account for register availability (?)
        val _type: Type = SemanticPass.checkExprType(lhs, node, new ListBuffer[String])
        val t = _type match {
          case WChar => "p_read_char"
          case WInt => "p_read_int"
        }
        if (!labels.contains(t)) {
          val read_msg = s"msg_$getDataMsgIndex"
          t match {
            case "p_read_char" =>
              data(read_msg) =
                            """.word 3
                              |.ascii	"%d\0" """.stripMargin
              labels("p_read_char") =
                s"""PUSH {lr}
                   |MOV r1, r0
                   |LDR r0, =$read_msg
                   |ADD r0, r0, #4
                   |BL scanf
                   |POP {pc}
                   |""".stripMargin
            case "p_read_int" =>
              data(read_msg) =
                """.word 4
                  |.ascii " %c\0" """.stripMargin
              labels("p_read_int") =
                s"""PUSH {lr}
                   |MOV r1, r0
                   |LDR r0, =$read_msg
                   |ADD r0, r0, #4
                   |BL scanf
                   |POP {pc}""".stripMargin
          }
        }
        s""" ADD r4, sp, #0
          | MOV r0, r4
          | BL $t""".stripMargin

      case Free(expr) => ???

      case Print(expr: AstNode) =>
        expr match {
          case StrLiter(str_val: String) =>
            val t = "p_print_string"
            val str_val_msg = s"msg_$getDataMsgIndex"
            data(str_val_msg) =
              s""".word ${str_val.length}

                 |.ascii "$str_val" """.stripMargin
            if (!labels.contains(t)) {
              val str_format_msg = s"msg_$getDataMsgIndex"
              data(str_format_msg) =
                """.word 5
                  |.ascii "%.*s\0" """.stripMargin
              data(t) =
                s"""PUSH {lr}
                   |LDR r1, [r0]
                   |ADD r2, r0, #4
                   |LDR r0, =$str_format_msg
                   |ADD r0, r0, #4
                   |BL printf
                   |MOV r0, #0
                   |BL fflush
                   |POP {pc}""".stripMargin
            }
            s"""LDR r4, $str_val_msg
                  |MOV r0, r4
                  |BL p_print_string""".stripMargin
          case BoolLiter(bool_val: Boolean) =>
            if (!labels.contains("p_print_bool")) {
              val bool_true_msg = s"msg_$getDataMsgIndex"
              val bool_false_msg = s"msg_$getDataMsgIndex"
              data(bool_true_msg) =
                """.word 5
                  |.ascii "true\0" """.stripMargin
              data(bool_false_msg) =
                """.word 6
                  |.ascii "false\0" """.stripMargin
              labels("p_print_bool") =
                s"""PUSH {lr}
                   |CMP r0, #0
                   |LDRNE r0, =${bool_true_msg}
                   |LDREQ r0, =${bool_false_msg}
                   |ADD r0, r0, #4
                   |BL printf
                   |MOV r0, #0
                   |BL fflush
                   |POP {pc}""".stripMargin
            }
            s""" MOV r4, #${if (bool_val) 1 else 0}
               | MOV r0, r4
               | BL p_print_bool""".stripMargin

          case IntLiter(int_val: Int) =>
            if (!labels.contains("p_print_int")) {
              val int_msg = s"msg_$getDataMsgIndex"
              labels("p_print_int") =
                s""" |PUSH {lr}
                   |MOV r1, r0
                   |LDR r0, =$int_msg
                   |ADD r0, r0, #4
                   |BL printf
                   |MOV r0, #0
                   |BL fflush
                   |POP {pc}""".stripMargin
            }
            s""" MOV r4, #$int_val
               | MOV r0, r4
               | BL p_print_bool""".stripMargin

          case CharLiter(char_val: Char) =>
            s"""MOV r4, #'$char_val'
               |MOV r0, r4
               |BL putchar""".stripMargin

          //TODO: implement all other print types
          case PairLiter => ???
          case Ident(ident: String) => ??? // ?
          case ArrayElem(ident: Ident, expr: List[Expr]) => ???
          case ParensExpr(expr: Expr) => ??? // is this actually a possibility or are these removed in the previous passes?

        }
        //TODO: find all functions that are branched to and add (???)
        //TODO: add global messages that can be added (???)
        //TODO: r4 might not be available?

      case Println(expr) =>
        val _p = traverse(Print(expr), ra)
        val println_msg = getDataMsgIndex
        if (!labels.contains("p_print_ln")) {
          data("println_msg") =
            """.word 1
              |.ascii "\0" """.stripMargin
          labels("p_print_ln") =
            s"""PUSH {lr}
               |LDR r0, =$println_msg
               |ADD r0, r0, #4
               |BL puts
               |MOV r0, #0
               |BL fflush
               |POP {pc}""".stripMargin
        }
        _p + s"""BL p_print_ln"""

      case Return(expr) => ??? //todo

      case Exit(expr) =>
        val reg = ra.next()
        s""" LDR $reg = ${traverse(expr, ra)}
           | MOV r0, $reg
           | BL exit""".stripMargin

      case IfElse(cond, stat_true, stat_false) =>
        // TODO add stack pointer changes for new scopes
        val reg = ra.next()
        traverse(cond, ra)
        var result = "CMP " + reg + ", #0\n"
        val newScope = new RegisterAllocator(ra.getAvailable)
        traverse(stat_true, newScope)
        newScope.restore()
        result += "L0:\n"
        traverse(stat_false, newScope)
        result + "L1:\n"


      case While(cond, stat) =>
        // TODO add stack changes for scoping
        "B L0" +
          "L1:" + traverse(stat, ra) +
        "L0" + traverse(cond, ra)
        //conditional branch back to L1

      case Scope(stat) => traverse(stat, ra)

      case Combine(stats) =>
        // TODO this could be refactored
        val statements = ""
        val newScope = new RegisterAllocator(ra.getAvailable)
        for (stat <- stats) {
          statements + traverse(stat, newScope)
          newScope.restore() //TODO check this implementation
        }
        statements

      case Or(BoolLiter(_), BoolLiter(_)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        "MOV " + reg1 + ", #1" +
          "MOV " + reg2 + ", #0" +
          "ORR " + reg1 + ", " + reg1 + ", " + reg2 +
          "MOV " + retReg + ", " + reg1

      case Or(expr1, expr2) =>
        traverse(expr1, ra)
        traverse(expr2, ra)
        val reg1 = ra.next()
        val reg2 = ra.next()
        "ORR " + reg1 + ", " + reg1 + ", " + reg2 +
          "MOV " + retReg + ", " + reg2

      case And(BoolLiter(_), BoolLiter(_)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        "MOV " + reg1 + ", #1" +
          "MOV " + reg2 + ", #0" +
          "AND " + reg1 + ", " + reg1 + ", " + reg2 +
          "MOV " + retReg + ", " + reg1

      case And(expr1, expr2) =>
        traverse(expr1, ra)
        traverse(expr2, ra)
        val reg1 = ra.next()
        val reg2 = ra.next()
        "AND " + reg1 + ", " + reg1 + ", " + reg2 +
          "MOV " + retReg + ", " + reg1

      case Greater(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        "LDR " + reg1 + " =" + x.toString +
          "LDR " + reg2 + " =" + y.toString +
          phonyCaseCompare("Greater", reg1, reg2)

      case GreaterEq(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        "LDR " + reg1 + " =" + x.toString +
          "LDR " + reg2 + " =" + y.toString +
          phonyCaseCompare("GreaterEq", reg1, reg2)

      case Less(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        "LDR " + reg1 + " =" + x.toString +
          "LDR " + reg2 + " =" + y.toString +
          phonyCaseCompare("Less", reg1, reg2)

      case LessEq(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        "LDR " + reg1 + " =" + x.toString +
          "LDR " + reg2 + " =" + y.toString +
          phonyCaseCompare("LessEq", reg1, reg2)

      case Eq(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        "LDR " + reg1 + " =" + x.toString +
          "LDR " + reg2 + " =" + y.toString +
          phonyCaseCompare("Eq", reg1, reg2)

      case NotEq(IntLiter(x), IntLiter(y)) =>
        val reg1 = ra.next()
        val reg2 = ra.next()
        "LDR " + reg1 + " =" + x.toString +
          "LDR " + reg2 + " =" + y.toString +
          phonyCaseCompare("NotEq", reg1, reg2)


//      case Greater(expr1, expr2) =>
//        traverse(expr1, ra)
//        traverse(expr2, ra)
      //TODO - potentially make an eval function to evaluate expr1 to a intLiter
      // or make traverse of intLitter(x) return x.toString and nothing else
      // so that the expr and intLiter cases can be combined
      // ie case Greater(expr1, expr2) handles if expr1 and expr2 are already intLiter

      case Plus(IntLiter(x), IntLiter(y)) =>
        "ADDS r4, r4, r5" +
        "BLVS p_throw_overflow_error" +
        "MOV r0, r4" //todo

      case _ => ""
      }
    }

  def phonyCaseCompare(subcase: String, reg1: String, reg2: String): String = {
    /**
     * Function to factor out repeated code for comparison expressions
     * Can be seen as a case containing all comparison cases as sub cases,
     * But this isn't reflected in the current AST implementation hence phony
     * The main case in traverse should ensure reg1 and reg2 store the correct
     * values before calling this function
     */
    var postfix1 = ""
    var postfix2 = ""
    subcase match{
      case "Greater" =>
        postfix1 = "GT"
        postfix2 = "LE"
      case "GreaterEq" =>
        postfix1 = "GE"
        postfix2 = "LT"
      case "Less" =>
        postfix1 = "LT"
        postfix2 = "GE"
      case "LessEq" =>
        postfix1 = "LE"
        postfix2 = "GT"
      case "Eq" =>
        postfix1 = "EQ"
        postfix2 = "NE"
      case "NotEq" =>
        postfix1 = "NE"
        postfix2 = "EQ"
    }

    "CMP " + reg1 + ", " + reg2 +
    "MOV" + postfix1 + " " + reg1 + ", #1" +
    "MOV" + postfix2 + " " + reg1 + ", #0" +
    "MOV " + retReg + ", " + reg1
  }

  //TODO add actual IO to file, presumably file passed as parameter
  def compile(): String ={
    val sb = new StringBuilder()
    sb.append(".data\n\t")
    for((k,v) <- data){
      sb.append(k + ":\n\t" + v)
    }
    sb.append(".text\n\t")
    sb.append(".global main\n\t")
    for((k,v) <- labels){
      sb.append(k + ":\n\t" + v)
    }
    sb.toString()
  }

  // TODO add more to this map
  val typeSize: immutable.Map[Type, Int] = Map[Type, Int](WInt -> 4, WBool -> 1, WChar -> 1)
  def assignmentsInScope(stats: Stat): Int = {
    var size = 0;
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

  def add(map: mutable.HashMap[String, String], key: String, value: String): Unit = {
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