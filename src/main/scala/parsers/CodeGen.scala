package parsers

import parsers.Ast._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object CodeGen{
  /**
   * Code Generation pass
   */
  // contains available registers
  val regs = new mutable.ListBuffer[Int]
  // each of these buffers represent a section of the output code that can be appended to
  val data = new mutable.ListBuffer[String]
  val text = new mutable.ListBuffer[String]
//  val main = new mutable.ListBuffer[String]
  val functions = new mutable.ListBuffer[String]

  // initialise buffers with respective label names
  data += ".data"
  text += ".text"
  functions += ".global main"

  def traverse(node: AstNode): Unit = {
    node match {
      case Program(funcs, stat) =>

//        main += "main:\n\tPUSH {lr}"
        funcs.foreach(traverse(_))
        traverse(stat)
//        main +=
//          "LDR r0, =0\n\t" +
//          "POP {pc}\n\t" +
//          ".ltorg"

      case Func((name, _), ParamList(params), stat) =>
        for (param <- params) {
          functions += name
          functions += ":\n\t"
          //traverse(param)
        }
        traverse(stat)

      case Param(_type, ident) =>

      // <Stat>
      case Skip =>
      case Decl(PairType(t1, t2), ident, PairLiter) =>
      case Decl(_type, ident, rhs) =>

      case Assign(lhs, rhs) =>
      case Read(lhs) =>
      case Free(expr) =>
      case Print(expr) =>

      case Println(expr) =>
      case Return(expr) =>
      case Exit(expr) =>
      case IfElse(cond, stat_true, stat_false) =>
        traverse(stat_true)
        traverse(stat_false)
      case While(cond, stat) =>
        traverse(stat)
      case Scope(stat) => traverse(stat)
      case Combine(stats) => stats.foreach(traverse(_))
      case _ =>
    }
  }

}