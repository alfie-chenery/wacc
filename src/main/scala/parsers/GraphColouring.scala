package parsers

import parsers.Assembly.{ADD, ADDS, AND, B, BL, CMP, DAscii, DWord, EOR, LDR, LTORG, MOV, MULTS, Mnemonic, ORR, POP, PUSH, RSBS, Register, SMULL, STR, STRB, SUB, SUBS, funcName}

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object GraphColouring {
  /**
   * @param code : compiled mnemonic IR assembly with temporary registers
   * @return compiled assembly with allocated registers
   */
//  def optimize(code: ListBuffer[Mnemonic]): ListBuffer[Mnemonic] = {
//    ???
//  }

  //  val nodes: mutable.Set[CFGNode] = new mutable.HashSet[CFGNode]

  val nodes: ListBuffer[CFGNode] = new ListBuffer[CFGNode]()
  val stCFG: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()
  //    val nodes: mutable.LinkedHashMap[Int, CFGNode] = new mutable.LinkedHashMap[Int, CFGNode]()

//  def buildCFG(code: ListBuffer[Mnemonic]): ListBuffer[CFGNode] = {
  def buildCFG(code: ListBuffer[Mnemonic]): Unit = {
    var i = 0
    for (m <- code) { // refactor to be functional?
      m match {
        case LTORG =>
        case DWord(_) =>
        case DAscii(_) =>
        case funcName(label) =>
          stCFG(label) = i
          nodes.append(new CFGNode(i, m, m.uses, m.defs, new ListBuffer[Int]))
          i+=1
        case _ =>
          nodes.append(new CFGNode(i, m, m.uses, m.defs, new ListBuffer[Int]))
          i+=1
      }
    }
    // MID: now 'nodes' should be populated (in order) with every instruction in the .text section.
    //  We should then be able to start at the beginning of code and follow the flow of execution

    succGenerator(0)

    // MID: all nodes should now have appropriate succ values (+uses, defs)
    //  next, we can begin our analysis.

    for (n <- nodes) {
      println(n)
    }

    //    val out = new ListBuffer[CFGNode]
    //    for ((m, i) <- code.zipWithIndex) { // refactor to be functional?
    //      out += new CFGNode(i, m, m.uses, m.defs, succs(m))
    //    }

  }

  def succGenerator(i: Int): Unit = {
    val c: CFGNode = nodes(i)
    c.succs ++ (
      c.instruction match {
        //      case Assembly.funcName(name) => ???
        //      case Assembly.LTORG => ???
        //      case Assembly.DWord(size) => ???
        //      case Assembly.DAscii(string) => ???
        case LDR(_, _, _) | STR(_, _) | PUSH(_) | POP(_) | SUB(_, _, _) |
             ADD(_, _, _) | MOV(_, _, _) | CMP(_, _) | STRB(_, _) |
             AND(_, _, _) | ORR(_, _, _) | ADDS(_, _, _) | SUBS(_, _, _) |
             MULTS(_, _, _) | SMULL(_, _, _, _) | EOR(_, _, _) |
             RSBS(_, _, _) | funcName(_) => List(i + 1) // should I be considering the error functions? maybe later? // funcName?
        case B(label, _) => List(stCFG(label), i + 1)
        case BL(label, _) => List(stCFG(label), i + 1)
        // todo: EOF case? tbh it should be fine, the match will just return empty
      })
    for (s <- c.succs) {
      succGenerator(s)
    }
  }


  //  def buildCFG(code: ListBuffer[Mnemonic]): ListBuffer[CFGNode] = {
  //    val out = new ListBuffer[CFGNode]
  //    for ((m, i) <- code.zipWithIndex) { // refactor to be functional?
  //      out += new CFGNode(i, m, m.uses, m.defs, succs(m))
  //    }
  //  }
}

class CFGNode(val id: Int, val instruction: Mnemonic, var uses: ListBuffer[Register], var defs: ListBuffer[Register], var succs: ListBuffer[Int])

