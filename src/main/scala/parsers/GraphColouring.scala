package parsers

import parsers.Assembly.{ADD, ADDS, AND, B, BL, CMP, DAscii, DWord, EOR, LDR, LTORG, MOV, MULTS, Mnemonic, ORR, POP, PUSH, RSBS, Register, SMULL, STR, STRB, SUB, SUBS, ScratchReg, TempReg, asr, funcName, lsl, regShift, regVal}

import scala.collection.immutable.{HashSet, ListMap}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object GraphColouring {
  // todo: refactor into separate control flow intermediate representation pass
  /**
   * @param code : compiled mnemonic IR assembly with temporary TempRegs
   * @return compiled assembly with allocated TempRegs
   */

  val nodes: ListBuffer[CFGNode] = new ListBuffer[CFGNode]()
  val stCFG: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()

  def graphColouring(interferes: mutable.Map[TempReg, mutable.Set[TempReg]]): mutable.Map[TempReg, ScratchReg] = {
    // Uses a greedy approximation algorithm that runs in linear time
    val tempAllocation: mutable.Map[TempReg, ScratchReg] = mutable.HashMap()
    val available: List[ScratchReg] = RegisterAllocator.allScratchRegisters
    for ((tempReg, adj) <- ListMap(interferes.toSeq.sortBy(_._2.size):_*)) { // uses sorted tempRegs by degree
      var i: Int = 0
      while (adj.exists(a => if (tempAllocation.contains(a)) tempAllocation(a) == available(i) else false)) {
        i += 1
      }
      tempAllocation(tempReg) = available(i)
      // todo: spillage..?
    }
    tempAllocation
  }

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
          i += 1
        case _ =>
          nodes.append(new CFGNode(i, n, new mutable.HashSet[TempReg], new mutable.HashSet[TempReg], new mutable.HashSet[Int]))
          i += 1
      }
    }

    def tempReg(s: mutable.HashSet[Any]): mutable.HashSet[TempReg] = s.map {
      case regVal(r) => r
      case regShift(r, _, _) => r
      case asr(r, _) => r
      case lsl(r, _) => r
      case r => r
    }.filter(_.isInstanceOf[TempReg]).map(_.asInstanceOf[TempReg])

    // todo: refactor into separate function?
    // assign defs and uses for each node
    for (n <- nodes) {
      n.instruction match {
        case LDR(r, o2, _) =>
          n.defs = tempReg(mutable.HashSet(r))
          n.uses = tempReg(mutable.HashSet(o2))
        case STR(rd, rn) =>
          n.uses = tempReg(mutable.HashSet(rd, rn))
        case PUSH(r) =>
          n.uses = tempReg(mutable.HashSet(r))
        case POP(r) =>
          n.defs = tempReg(mutable.HashSet(r))
        case SUB(o1, o2, o3) =>
          n.defs = tempReg(mutable.HashSet(o1))
          n.uses = tempReg(mutable.HashSet(o2, o3))
        case ADD(o1, o2, o3) =>
          n.defs = tempReg(mutable.HashSet(o1))
          n.uses = tempReg(mutable.HashSet(o2, o3))
        case MOV(rd, o2, _) =>
          n.defs = tempReg(mutable.HashSet(rd))
          n.uses = tempReg(mutable.HashSet(o2))
        case CMP(r, o2) =>
          n.uses = tempReg(mutable.HashSet(r, o2))
        case STRB(rd, rn) =>
          n.uses = tempReg(mutable.HashSet(rd, rn))
        case AND(rd, rn, rm) =>
          n.defs = tempReg(mutable.HashSet(rd))
          n.uses = tempReg(mutable.HashSet(rn, rm))
        case ORR(rd, rn, rm) =>
          n.defs = tempReg(mutable.HashSet(rd))
          n.uses = tempReg(mutable.HashSet(rn, rm))
        case ADDS(rd, rn, rm) =>
          n.defs = tempReg(mutable.HashSet(rd))
          n.uses = tempReg(mutable.HashSet(rn, rm))
        case SUBS(rd, rn, rm) =>
          n.defs = tempReg(mutable.HashSet(rd))
          n.uses = tempReg(mutable.HashSet(rn, rm))
        case MULTS(rd, rn, rm) =>
          n.defs = tempReg(mutable.HashSet(rd))
          n.uses = tempReg(mutable.HashSet(rn, rm))
        case SMULL(rdLo, rdHi, rm, rs) =>
          n.defs = tempReg(mutable.HashSet(rdLo, rdHi))
          n.uses = tempReg(mutable.HashSet(rm, rs))
        case EOR(rd, o2, o3) =>
          n.defs = tempReg(mutable.HashSet(rd))
          n.uses = tempReg(mutable.HashSet(o2, o3))
        case RSBS(rd, o2, o3) =>
          n.defs = tempReg(mutable.HashSet(rd))
          n.uses = tempReg(mutable.HashSet(o2, o3))
        case _ =>
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

    // todo: Refactor into its own function?

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

    println("liveIn:")
    println(liveIn)
    println("liveOut:")
    println(liveOut)

    // Interference analysis:
    val interferes: mutable.Map[TempReg, mutable.Set[TempReg]] = mutable.HashMap[TempReg, mutable.Set[TempReg]]()
    for (t <- RegisterAllocator.allTempRegisters) {
      for (n <- nodes) {
        if (liveOut(n.id).contains(t)) {
          interferes(t) = liveOut(n.id)
        }
      }
    }

    println("interferes:")
    println(interferes)
    val tempAllocation: mutable.Map[TempReg, ScratchReg] = graphColouring(interferes)
    println("tempAllocation:")
    println(tempAllocation)

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

class CFGNode(val id: Int,
              val instruction: Mnemonic,
              var uses: mutable.Set[TempReg],
              var defs: mutable.Set[TempReg],
              var succs: mutable.Set[Int])

