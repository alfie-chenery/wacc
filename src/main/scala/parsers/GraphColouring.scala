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
          nodes.append(new CFGNode(i, m, m.uses, m.defs, new mutable.HashSet[Int]))
          i+=1
        case _ =>
          nodes.append(new CFGNode(i, m, m.uses, m.defs, new mutable.HashSet[Int]))
          i+=1
      }
    }
    // MID: now 'nodes' should be populated (in order) with every instruction in the .text section.
    //  We should then be able to start at the beginning of code and follow the flow of execution

    succGenerator(0)

    // MID: all nodes should now have appropriate succ values (+uses, defs)
    //  next, we can begin our analysis.

    println(stCFG)
    println(List("id", "instruction", "uses", "defs", "succs"))
    for (n <- nodes) {
      println(List(n.id, n.instruction, n.uses, n.defs, n.succs))
    }

    println("got here 1")
    // maps from node (instr.) index to set of livein/liveout regs
    val liveIn: mutable.HashMap[Int, mutable.Set[Register]] = new mutable.HashMap[Int, mutable.Set[Register]]()
    val liveOut: mutable.HashMap[Int, mutable.Set[Register]] = new mutable.HashMap[Int, mutable.Set[Register]]()

    for (n <- nodes) {
      liveIn(n.id) = mutable.Set[Register]()
      liveOut(n.id) = mutable.Set[Register]()
    }

    var prevLiveIn: mutable.HashMap[Int, mutable.Set[Register]] = mutable.HashMap()
    var prevLiveOut: mutable.HashMap[Int, mutable.Set[Register]] = mutable.HashMap()

    do {
      prevLiveIn = liveIn
      prevLiveOut = liveOut
      println(nodes.reverse)
      for (n <- nodes.reverse) {
        liveIn(n.id) = n.uses.union(liveOut(n.id).diff(n.defs)) // todo: check
        liveOut(n.id) = n.succs.foldLeft(mutable.Set[Register]())((a: mutable.Set[Register], i: Int) => liveIn(i).union(a))
      }
    } while (prevLiveIn != liveIn && prevLiveOut != liveOut)

    println(liveIn)
    println(liveOut)

  }

  def succGenerator(i: Int): Unit = {
    nodes(i)
    nodes(i).succs = (
      nodes(i).instruction match {
//      case Assembly.funcName(name) => ???
//      case Assembly.LTORG => ???
//      case Assembly.DWord(size) => ???
//      case Assembly.DAscii(string) => ???
        case LDR(_, _, _) | STR(_, _) | PUSH(_) | POP(_) | SUB(_, _, _) |
             ADD(_, _, _) | MOV(_, _, _) | CMP(_, _) | STRB(_, _) |
             AND(_, _, _) | ORR(_, _, _) | ADDS(_, _, _) | SUBS(_, _, _) |
             MULTS(_, _, _) | SMULL(_, _, _, _) | EOR(_, _, _) |
             RSBS(_, _, _) | funcName(_) =>
          if (i+1<nodes.size) mutable.HashSet(i+1) else mutable.HashSet() // should I be considering the error functions? maybe later? // funcName?
        case B(label, _) =>
          if (stCFG.contains(label)) mutable.HashSet(i+1, stCFG(label)) else mutable.HashSet(i+1) // if label not found, means function is external and it is fine for regs to be overwritten
        case BL(label, _) =>
          if (stCFG.contains(label)) mutable.HashSet(i+1, stCFG(label)) else mutable.HashSet(i+1)
        case _ => mutable.HashSet() // catches EOF
      })
    for (s <- nodes(i).succs) {
      println(s"going to: " + s)
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

class CFGNode(val id: Int, val instruction: Mnemonic, var uses: mutable.Set[Register], var defs: mutable.Set[Register], var succs: mutable.Set[Int])

