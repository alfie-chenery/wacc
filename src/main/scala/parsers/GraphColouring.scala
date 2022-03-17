package parsers

import parsers.Assembly.{ADD, ADDS, AND, B, BL, CMP, DAscii, DWord, EOR, LDR, LTORG, MOV, MULTS, Mnemonic, ORR, POP, PUSH, RSBS, Register, SMULL, STR, STRB, SUB, SUBS, TempReg, funcName}

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object GraphColouring {
  /**
   * @param code : compiled mnemonic IR assembly with temporary TempRegs
   * @return compiled assembly with allocated TempRegs
   */

  val nodes: ListBuffer[CFGNode] = new ListBuffer[CFGNode]()
  val stCFG: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()

//  def buildCFG(code: ListBuffer[Mnemonic]): ListBuffer[CFGNode] = {
  def buildCFG(code: ListBuffer[Mnemonic]): Unit = {

    // generates node skeletons
    var i = 0
    for (n <- code) { // refactor to be functional?
      n match {
        case LTORG =>
        case DWord(_) =>
        case DAscii(_) =>
        case funcName(label) =>
          stCFG(label) = i
          nodes.append(new CFGNode(i, n, new mutable.HashSet[TempReg], new mutable.HashSet[TempReg], new mutable.HashSet[Int]))
          i+=1
        case _ =>
          nodes.append(new CFGNode(i, n, new mutable.HashSet[TempReg], new mutable.HashSet[TempReg], new mutable.HashSet[Int]))
          i+=1
      }
    }

    // assign defs and uses for each node
    for (n <- nodes) {
      n.instruction match {
        case funcName(name) =>
        case LTORG =>
        case DWord(size) =>
        case DAscii(string) =>
        case LDR(r, o2, suffix) =>
          n.defs = mutable.HashSet(r).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(o2).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
        case STR(rd, rn) =>
          n.uses = mutable.HashSet(rd, rn).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
        case PUSH(r) =>
          n.uses = mutable.HashSet(r).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
        case POP(r) =>
          n.defs = mutable.HashSet(r).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
        case SUB(o1, o2, o3) =>
          // can be rewritten as a match?
          n.defs = mutable.HashSet(o1).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(o2, o3).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
        case ADD(o1, o2, o3) =>
          n.defs = mutable.HashSet(o1).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(o2, o3).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
        case MOV(rd, o2, suffix) =>
          n.defs = mutable.HashSet(rd).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(o2).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
        case BL(label, suffix) =>
        case CMP(r, o2) =>
          n.uses = mutable.HashSet(r, o2).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
        case STRB(rd, rn) =>
          n.uses = mutable.HashSet(rd, rn).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
        case AND(rd, rn, rm) =>
          n.defs = mutable.HashSet(rd).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(rn, rm).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
        case ORR(rd, rn, rm) =>
          n.defs = mutable.HashSet(rd).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(rn, rm).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
        case ADDS(rd, rn, rm) =>
          n.defs = mutable.HashSet(rd).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(rn, rm).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])

        case SUBS(rd, rn, rm) =>
          n.defs = mutable.HashSet(rd).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(rn, rm).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])

        case MULTS(rd, rn, rm) =>
          n.defs = mutable.HashSet(rd).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(rn, rm).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])

        case SMULL(rdLo, rdHi, rm, rs) =>
          n.defs = mutable.HashSet(rdLo, rdHi).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(rm, rs).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])

        case EOR(rd, o2, o3) =>
          n.defs = mutable.HashSet(rd).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(o2, o3).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])

        case RSBS(rd, o2, o3) =>
          n.defs = mutable.HashSet(rd).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])
          n.uses = mutable.HashSet(o2, o3).filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])

        case B(label, suffix) =>
      }
    }

    // MID: now 'nodes' should be populated (in order) with every instruction in the .text section.
    //  We should then be able to start at the beginning of code and follow the flow of execution

    succGenerator()

    // MID: all nodes should now have appropriate succ values (+uses, defs)
    //  next, we can begin our analysis.

    println(stCFG)
    println(List("id", "instruction", "uses", "defs", "succs"))
    for (n <- nodes) {
      println(List(n.id, n.instruction, n.uses, n.defs, n.succs))
    }

    // maps from node (instr.) index to set of livein/liveout regs
    val liveIn: mutable.HashMap[Int, mutable.Set[TempReg]] = new mutable.HashMap[Int, mutable.Set[TempReg]]()
    val liveOut: mutable.HashMap[Int, mutable.Set[TempReg]] = new mutable.HashMap[Int, mutable.Set[TempReg]]()

    for (n <- nodes) {
      liveIn(n.id) = mutable.Set[TempReg]()
      liveOut(n.id) = mutable.Set[TempReg]()
    }

    var prevLiveIn: mutable.HashMap[Int, mutable.Set[TempReg]] = mutable.HashMap()
    var prevLiveOut: mutable.HashMap[Int, mutable.Set[TempReg]] = mutable.HashMap()

    do {
      prevLiveIn = liveIn
      prevLiveOut = liveOut
      println(nodes.reverse)
      for (n <- nodes.reverse) {
        liveIn(n.id) = n.uses.union(liveOut(n.id).diff(n.defs))
        liveOut(n.id) = n.succs.foldLeft(mutable.Set[TempReg]())((a: mutable.Set[TempReg], i: Int) => liveIn(i).union(a))
      }
    } while (prevLiveIn != liveIn && prevLiveOut != liveOut)

    println(liveIn)
    println(liveOut)

  }

  def succGenerator(): Unit = {
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
            if (i+1<nodes.size) mutable.HashSet(i+1) else mutable.HashSet() // should I be considering the error functions? maybe later? // funcName?
          case B(label, _) =>
            // if label not found, means function is external and it is fine for regs to be overwritten
            if (stCFG.contains(label)) mutable.HashSet(i+1, stCFG(label)) else mutable.HashSet(i+1)
          case BL(label, _) =>
            if (stCFG.contains(label)) mutable.HashSet(i+1, stCFG(label)) else mutable.HashSet(i+1)
          case _ => mutable.HashSet() // catches EOF, data, etc.
        }
      // enqueues only the current node's successors that have not yet been visited
      nodes(i).succs.filter(nodes(_).succs.isEmpty).foreach(q.enqueue)
    }
  }
}

class CFGNode(val id: Int,
              val instruction: Mnemonic,
              var uses: mutable.Set[TempReg],
              var defs: mutable.Set[TempReg],
              var succs: mutable.Set[Int])

