package parsers

import parsers.Assembly._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class CFGNode(val id: Int,
              var instruction: Mnemonic,
              var succs: mutable.Set[Int]) {
  override def toString: String = s"CFGNode($id, $instruction, $succs)"
}

class ControlFlowGraph(val code: ListBuffer[Mnemonic]) {

  val nodes: ListBuffer[CFGNode] = new ListBuffer[CFGNode]()

  private val stCFG: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()


//  def buildCFG(code: ListBuffer[Mnemonic]): ListBuffer[CFGNode] = {

  // generates node skeletons
  code.zipWithIndex.foreach{ case (n, i) =>
    n match {
      case funcName(label) =>
        stCFG(label) = i
      case _ =>
    }
    nodes.append(new CFGNode(i, n, new mutable.HashSet[Int]))
  }

  // MID: now 'nodes' should be populated (in order) with every instruction in the .text section.
  //  We should then be able to start at the beginning of code and follow the flow of execution

  succGenerator(nodes)

  // MID: all nodes should now have appropriate succ values


  override def toString: String = toAssembly.toString()

  def toAssembly: ListBuffer[Mnemonic] = {
    nodes.map(c => c.instruction)
  }

  def succGenerator(nodes: ListBuffer[CFGNode]): Unit = {
    // program control flow analysis begins at the first line of code (0) in main
    // A queue is used instead of recursion to prevent stack overflow on branch-heavy programs
    val q: mutable.Queue[Int] = mutable.Queue(0)
    while (q.nonEmpty) {
      val i = q.dequeue()
      nodes(i).succs =
        nodes(i).instruction match {
          case LDR(_, _, _) | STR(_, _) | PUSH(_) | POP(_) | SUB(_, _, _) |
               ADD(_, _, _) | MOV(_, _, _) | CMP(_, _) | STRB(_, _) |
               AND(_, _, _) | ORR(_, _, _) | ADDS(_, _, _) | SUBS(_, _, _) |
               MULTS(_, _, _) | SMULL(_, _, _, _) | EOR(_, _, _) |
               RSBS(_, _, _) | funcName(_) =>
            if (i + 1 < nodes.size) mutable.HashSet(i + 1) else mutable.HashSet() // should I be considering the error functions? maybe later? // funcName?
          case B(label, _) =>
            // if label not found, means function is external and it is fine for regs to be overwritten
            if (stCFG.contains(label)) mutable.HashSet(i + 1, stCFG(label)) else mutable.HashSet(i + 1)
          case BL(label, _) =>
            if (stCFG.contains(label)) mutable.HashSet(i + 1, stCFG(label)) else mutable.HashSet(i + 1)
          case _ => mutable.HashSet() // catches EOF, data, etc.
        }
      // enqueues only the current node's successors that have not yet been visited
      nodes(i).succs.filter(nodes(_).succs.isEmpty).foreach(q.enqueue)
    }
  }

}
