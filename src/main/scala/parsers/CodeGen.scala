package parsers

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object CodeGen{
  /**
   * Code Generation pass
   */
   import parsers.Ast._

  // each of these maps represent a section of the output code that can be appended to
  val data = new mutable.HashMap[String, String]
  val functions: mutable.Set[String] =  mutable.Set()
  // todo: refactor so that maps/buffers automatically indent/format strings

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

        program += "LDR " + ra.retReg + ", =0\n\t" +
        "POP {pc}\n\t" +
        ".ltorg"
        program

      case Func((_type, Ident(name)), ParamList(params), stat) =>
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
      case Skip => ???

      case Decl(PairType(t1, t2), ident, PairLiter) => ???
      case Decl(_type, ident, rhs) =>
        // TODO this is not gonna work (see many variables example)
        val reg = ra.next()
        "SUB sp, sp, #4" +
        "LDR " + reg +", =" + traverse(rhs, ra) + ////////////////////////////////////////
        "STR " + reg + ", [sp]" +
        "ADD sp, sp, #4"

      case Assign(lhs, rhs) => ???

      case Read(lhs) =>

        val _type: Type = SemanticPass.checkExprType(lhs, node, new ListBuffer[String])

        val t = _type match {
        // todo: refactor? kinda don't like how the way it has side effects
          case WChar =>
//            data(s"$msg") =
            data(s"msg") =
              """.word 3
                |.ascii	"%d\0" """.stripMargin
            "p_read_int"

          case WInt =>
//            data(s"$msg") =
            data(s"msg") =
              """.word 4
                |.ascii " %c\0" """.stripMargin
            "p_read_bool"
        }

        functions += t

        s""" ADD r4, sp, #0
          | MOV r0, r4
          | BL $t""".stripMargin

      case Free(expr) => ???

      case Print(expr) =>
        val _type: Type = SemanticPass.checkExprType(expr, node, new ListBuffer[String])
        _type match{
          case WString =>
            functions += "p_print_string"
            // TODO call the string function
            ""
          case WBool =>
            functions += "p_print_bool"
            """ MOV r4, #0
              | MOV r0, r4
              | BL p_print_bool""".stripMargin
            // todo: r4 might not be available
            //TODO: all other print types
            //TODO: find all functions that are branched to and add
            //TODO: add global messages that can be added
        }

      case Println(expr) =>
        functions += "p_print_ln"
        traverse(Print(expr), ra) + "BL p_print_ln" // formatting

      case Return(expr) => ??? //todo

      case Exit(expr) =>
        val reg = ra.next()
        s"""LDR $reg = ${traverse(expr, ra)}
           |MOV r0, r4
           |BL exit""".stripMargin

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
        //TODO GOT TO HERE WITH RA

      case Scope(stat) => traverse(stat, ra)

      case Combine(stats) =>
        // TODO this could be refactored
        val statements = ""
        for (stat <- stats) {
          statements + traverse(stat, ra)
        }
        statements

      // TODO this is almost certainly missing something
      case Or(BoolLiter(_), BoolLiter(_)) =>
        "MOV r4, #1" +
          "MOV r5, #0" +
          "ORR r4, r4, r5" +
          "MOV r0, r4"

      case Or(expr1, expr2) =>
        traverse(expr1, ra)
        traverse(expr2, ra)
        "ORR r4, r4, r5" +
          "MOV r0, r4"
      //TODO: print function but only if not already there

      // TODO this is almost certainly missing something
      case And(BoolLiter(_), BoolLiter(_)) =>
        "MOV r4, #1" +
          "MOV r5, #0" +
          "AND r4, r4, r5" +
          "MOV r0, r4"

      case And(expr1, expr2) =>
        traverse(expr1, ra)
        traverse(expr2, ra)
        "AND r4, r4, r5" +
          "MOV r0, r4"
      //TODO: print function but only if not already there

      case Greater(expr1, expr2) =>
        // TODO this is almost certainly missing something
        traverse(expr1, ra)
        traverse(expr2, ra)

      case Less(IntLiter(x), IntLiter(y)) =>
        "LDR r4 =" + x.toString +
                "LDR r5 =" + y.toString +
                "CMP r4, r5" +
                "MOVLT r4, #1" +
                "MOVGE r4, #0" +
                "MOV r0, r4"

      case LessEq(IntLiter(x), IntLiter(y)) =>
        "LDR r4 =" + x.toString +
                "LDR r5 =" + y.toString +
                "CMP r4 r5" +
                "MOVLE r4 #1" +
                "MOVGT r4 #0" +
                "MOV r0, r4"


      case Plus(IntLiter(x), IntLiter(y)) =>
        "ADDS r4, r4, r5" +
        "BLVS p_throw_overflow_error" +
        "MOV r0, r4" //todo



      }
    }

  //TODO add actual IO to file, presumably file passed as parameter
  def writeToFile(filename: String): Unit ={
    val sb = new StringBuilder()
    sb.append(".data\n\t")
    for((k,v) <- data){
      sb.append(k + ":\n\t" + v)
    }

    sb.append(".text\n\t")

    sb.append(".global main\n\t")
//    for((k,v) <- functions){
//      sb.append(k + ":\n\t" + v)
//    }
  }

  // TODO in future this could be transformed to use the datatype
  // TODO make messages variables
  val standardFunctions: immutable.Map[String, String] = Map(
    "p_read_int" ->
      s""" PUSH {lr}
        | MOV r1 r0
        | LDR r0 =msg_0
        | ADD r0, r0, #4
        | BL scanf
        | POP {pc}""".stripMargin,

    "p_read_char"->
      s""" PUSH {lr}
        | MOV r1, r0
        | LDR r0 =msg_0
        | ADD r0, r0, #4
        | BL scanf
        | POP {pc}""".stripMargin,

    "p_print_string" ->
      s""" PUSH {lr}
        | LDR r1, [r0]
        | ADD r2, r0, 4
        | LDR r0, =msg_0
        | ADD r0, r0, #4
        | BL printf
        | MOV r0 #0
        | BL fflush
        | POP {pc}""".stripMargin,

    "p_print_ln" ->
      s""" PUSH {LR}
        | LDR r0, =msg_0
        | ADD r0, r0, #4
        | BL puts
        | MOV r0, #0
        | BL fflush
        | POP {pc}
        |""".stripMargin,

  "p_print_ln" ->
    s""" PUSH {LR}
       | LDR r0, =msg_0
       | ADD r0, r0, #4
       | BL puts
       | MOV r0, #0
       | BL fflush
       | POP {pc}
       |""".stripMargin,

  "p_read_int" ->
    s"""  PUSH {lr}
        | MOV r1, r0
        | LDR r0, =msg_0
        | ADD r0, r0, #4
        | BL scanf
        | POP {pc}""".stripMargin,

  "p_read_char" ->
    s"""  PUSH {lr}
        | MOV r1, r0
        | LDR r0, =msg_0
        | ADD r0, r0, #4
        | BL scanf
        | POP {pc}
    """.stripMargin

  )


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
          }
        }
    }
    size
  }

  def pop(availableRegs: ListBuffer[Int]): String = {
    /**
     * pops the head of availableRegs and returns this register as a string, prepended with r
     */
    val reg = availableRegs.head
    availableRegs.remove(0)
    "r" + reg
  }

  def peek(availableRegs: ListBuffer[Int]): String = {
    /**
     * returns the head of availableRegs as a string prepended with r, without modifying availableRegs
     */
    val reg = availableRegs.head
    "r" + reg
  }

  def drop(n: Int, availableRegs: ListBuffer[Int]): ListBuffer[Int] = {
    /**
     * returns the ListBuffer obtained by dropping the first n elements
     */
    var result = availableRegs.clone()
    for (_ <- 1 to n) {
      result = result.tail
    }
    result
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
