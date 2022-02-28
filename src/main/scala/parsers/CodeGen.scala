package parsers

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
  val text = new mutable.HashMap[String, String]
  val main = new mutable.ListBuffer[String]
  val functions = new mutable.HashMap[String, String]
  // todo: refactor so that maps/buffers automatically indent/format strings

  //TODO make use of availableRegs to replace
  def traverse(node: AstNode, ra: RegisterAllocator): String = {
    node match {
      case Program(funcs, stat) =>

        main += "main:\n\tPUSH {lr}"
        funcs.foreach(traverse(_, availableRegs))
        traverse(stat, availableRegs)
        main +=
          "LDR r0, =0\n\t" +
          "POP {pc}\n\t" +
          ".ltorg"

      case Func((_type, Ident(name)), ParamList(params), stat) =>
        update(functions, name, ":\n\t")
//        for (param <- params) {
//          traverse(param, availableRegs)
//        }
        traverse(stat, availableRegs)

      case Param(_type, ident) =>

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

      case Read(lhs) => ???

      case Free(expr) => ???

      case Print(expr) =>
        val _type: Type = SemanticPass.checkExprType(expr, node, new ListBuffer[String])
        _type match{
          case WString =>
            "LDR r1, [r0]" +
            "ADD r2, r0, #4" +
            "LDR r0, =msg_3" +
            "ADD r0, r0, #4" +
            "BL printf" +
            "MOV r0, #0" +
            "BL fflush"
          case WBool =>
            add(functions, "p_print_bool:",
                "PUSH {lr}" +
                "CMP r0, #0" +
                "LDRNE r0, =msg_0" +
                "LDREQ r0, =msg_1" +
                "ADD r0, r0, #4" +
                "BL printf" +
                "MOV r0, #0" +
                "BL fflush" +
                "POP {pc}")

            "MOV r4, #0" + // todo: r4 might not be available
            "MOV r0, r4" +
            "BL p_print_bool"
            //TODO: all other print types
          //TODO: find all functions that are branched to and add
          //TODO: add global messages that can be added
        }

      case Println(expr) => ??? // todo: mostly same as print

      case Return(expr) => ??? //todo

      case Exit(expr) =>
        val reg = ra.next()
        "LDR " + reg + " =" + traverse(expr, ra) +
        "MOV r0, r4" +
        "BL exit"

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

      case Or(BoolLiter(_), BoolLiter(_)) =>
        main += "MOV r4, #1" +
          "MOV r5, #0" +
          "ORR r4, r4, r5" +
          "MOV r0, r4"

      case Or(expr1, expr2) =>
        traverse(expr1, ra)
        traverse(expr2, ra)
        "ORR r4, r4, r5" +
          "MOV r0, r4"
      //TODO: print function but only if not already there

      case And(BoolLiter(_), BoolLiter(_)) =>
        main += "MOV r4, #1" +
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
        traverse(expr1, availableRegs)
        traverse(expr2, availableRegs)

      case Less(IntLiter(x), IntLiter(y)) =>
        main += "LDR r4 =" + x.toString +
                "LDR r5 =" + y.toString +
                "CMP r4, r5" +
                "MOVLT r4, #1" +
                "MOVGE r4, #0" +
                "MOV r0, r4"

      case LessEq(IntLiter(x), IntLiter(y)) =>
        main += "LDR r4 =" + x.toString +
                "LDR r5 =" + y.toString +
                "CMP r4 r5" +
                "MOVLE r4 #1" +
                "MOVGT r4 #0" +
                "MOV r0, r4"
      }
    }

  //TODO add actual IO to file, presumably file passed as parameter
  def writeToFile(): Unit ={
    val sb = new StringBuilder()
    sb += ".data\n\t"
    for((k,v) <- data){
      sb += k + ":\n\t" + v
    }
    //write to file here
    sb.setLength(0) //clear string builder

    sb += ".text\n\t"
    for((k,v) <- text){
      sb += k + ":\n\t" + v
    }
    //write to file here
    sb.setLength(0) //clear string builder

    sb += ".global main\n\t"
    for((k,v) <- functions){
      sb += k + ":\n\t" + v
    }
    //write to file here
  }

  //helper functions to make code cleaner

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
      map += (key,value)
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