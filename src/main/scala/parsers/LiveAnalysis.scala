package parsers

import parsers.Assembly._

import scala.annotation.tailrec
//import parsers.ControlFlowGraph.CFGNode

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object LiveAnalysis {

  class LVACFGNode(override val id: Int,
                   instruction: Mnemonic,
                   var uses: mutable.Set[TempReg],
                   var defs: mutable.Set[TempReg],
                   succs: mutable.Set[Int]) extends CFGNode(id, instruction, succs)

//  def tempReg(s: mutable.HashSet[Operand]): mutable.HashSet[TempReg] = s.map(e => tempReg2(e) match {
//    case Some(value) => value
//    case None =>
//  })
  def tempReg(s: mutable.HashSet[Operand]): mutable.HashSet[TempReg] = s.map(tempReg2).collect {
    case Some(value) => value
  }

  @tailrec
  def tempReg2(o: Operand): Option[TempReg] = {
    o match {
      case imm(i) => None
      case immc(c) => None
      case label(l) => None
      case asr(reg, shift) => tempReg2(reg)
      case lsl(reg, shift) => tempReg2(reg)
      case Assembly.nullOp => None
      case suffix: Suffix => None
      case register: Register => register match {
        case regShift(reg, shift, update) => tempReg2(reg)
        case regVal(reg) => tempReg2(reg)
        case Assembly.RetReg => None
        case Assembly.reg1 => None
        case Assembly.reg2 => None
        case Assembly.SP => None
        case Assembly.LinkReg => None
        case Assembly.PC => None
        case reg: ScratchReg => None
        case reg: TempReg => Some(reg)
      }
    }
  }

  /**
   * @param cfg : ControlFlowGraph
   * @return a mapping from temporary registers to scratch registers, optimised with graph colouring
   */
  def registerAllocation(cfg: ControlFlowGraph): mutable.Map[TempReg, ScratchReg] = {
    val nodes: ListBuffer[LVACFGNode] = cfg.nodes.map(c => new LVACFGNode(c.id, c.instruction, mutable.HashSet[TempReg](), mutable.HashSet[TempReg](), c.succs))

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

    // maps from node (instr.) index to set of livein/liveout regs
    val liveIn, liveOut: mutable.HashMap[Int, mutable.Set[TempReg]] = new mutable.HashMap[Int, mutable.Set[TempReg]]()

    for (n <- nodes) {
      liveIn(n.id) = mutable.Set[TempReg]()
      liveOut(n.id) = mutable.Set[TempReg]()
    }

    var prevLiveIn, prevLiveOut: mutable.HashMap[Int, mutable.Set[TempReg]] = mutable.HashMap()

    do {
      prevLiveIn = liveIn.clone()
      prevLiveOut = liveOut.clone()
      for (n <- nodes.reverse) {
        liveIn(n.id) = n.uses.union(liveOut(n.id).diff(n.defs))
        liveOut(n.id) = n.succs.foldLeft(mutable.Set[TempReg]())((a: mutable.Set[TempReg], i: Int) => liveIn(i).union(a))
      }
    } while (prevLiveIn != liveIn && prevLiveOut != liveOut)

//    for (n <- nodes) {
//      println(s"${n.instruction} LI ${liveIn(n.id)} LO ${liveOut(n.id)} uses ${n.uses} defs ${n.defs}")
//    }

    // Interference analysis:
    val interferes: mutable.Map[TempReg, mutable.Set[TempReg]] = mutable.HashMap[TempReg, mutable.Set[TempReg]]()
    for (t <- RegisterAllocator.allTempRegisters) {
      interferes(t) = mutable.HashSet[TempReg]()
    }
    for (t <- RegisterAllocator.allTempRegisters) {
      for (n <- nodes) {
        if (liveOut(n.id).contains(t)) {
          interferes(t) = interferes(t).union(liveOut(n.id))
        }
      }
    }

//    println(s"interference map: $interferes")
    val tempAllocation: mutable.Map[TempReg, ScratchReg] = graphColouring(interferes)
//    println(s"register allocation: $tempAllocation")

    tempAllocation
  }

  // translate an arbitrary register and its arguments into their scratch reg equivalent, if applicable
  def translate(r: Register, a: mutable.Map[TempReg, ScratchReg]): Register = {
    r match {
      case regShift(reg, shift, update) => regShift(translate(reg, a), shift, update)
      case regVal(reg) => regVal(translate(reg, a))
      case reg: TempReg => a(reg)
      case reg: ScratchReg => reg
      case RetReg | Assembly.reg1 | Assembly.reg2 | SP | LinkReg | PC => r
    }
  }

  // translate an arbitrary operand and its arguments into their scratch reg equivalents, if appropriate
  def translate(o: Operand, a: mutable.Map[TempReg, ScratchReg]): Operand = {
    o match {
      case register: Register => translate(register, a)
      case asr(reg, shift) => asr(translate(reg, a), shift)
      case lsl(reg, shift) => lsl(translate(reg, a), shift)
      case _ => o
    }
  }

  // translate an arbitrary mnemonic and its arguments into their scratch register equivalents, if appropriate
  def translate(mnemonic: Mnemonic, a: mutable.Map[TempReg, ScratchReg]): Mnemonic = {
    mnemonic match {
      case LDR(r, o2, suffix) => LDR(translate(r, a), translate(o2, a), suffix)
      case STR(rd, rn) => STR(translate(rd, a), translate(rn, a))
      case PUSH(r) => PUSH(translate(r, a))
      case POP(r) => POP(translate(r, a))
      case SUB(o1, o2, o3) => SUB(translate(o1, a), translate(o2, a), translate(o3, a))
      case ADD(o1, o2, o3) => ADD(translate(o1, a), translate(o2, a), translate(o3, a))
      case MOV(rd, o2, suffix) => MOV(translate(rd, a), translate(o2, a), suffix)
      case CMP(r, o2) => CMP(translate(r, a), translate(o2, a))
      case STRB(rd, rn) => STRB(translate(rd, a), translate(rn, a))
      case AND(rd, rn, rm) => AND(translate(rd, a), translate(rn, a), translate(rm, a))
      case ORR(rd, rn, rm) => ORR(translate(rd, a), translate(rn, a), translate(rm, a))
      case ADDS(rd, rn, rm) => ADDS(translate(rd, a), translate(rn, a), translate(rm, a))
      case SUBS(rd, rn, rm) => SUBS(translate(rd, a), translate(rn, a), translate(rm, a))
      case MULTS(rd, rn, rm) => MULTS(translate(rd, a), translate(rn, a), translate(rm, a))
      case SMULL(rdLo, rdHi, rm, rs) => SMULL(translate(rdLo, a), translate(rdHi, a), translate(rm, a), translate(rs, a))
      case EOR(rd, o2, o3) => EOR(translate(rd, a), translate(o2, a), translate(o3, a))
      case RSBS(rd, o2, o3) => RSBS(translate(rd, a), translate(o2, a), translate(o3, a))
      case i => i
    }
  }

  def liveVariableAnalysis(cfg: ControlFlowGraph): Unit = {
    val a: mutable.Map[TempReg, ScratchReg] = registerAllocation(cfg)
    for (n <- cfg.nodes) {
      n.instruction = translate(n.instruction, a)
    }
  }

  // Graph colouring algorithm that uses a greedy approximation algorithm running in linear time
  def graphColouring(interferes: mutable.Map[TempReg, mutable.Set[TempReg]]): mutable.Map[TempReg, ScratchReg] = {
    val tempAllocation: mutable.Map[TempReg, ScratchReg] = mutable.HashMap()
    val available: List[ScratchReg] = RegisterAllocator.allScratchRegisters
    for ((tempReg, adj) <- ListMap(interferes.toSeq.sortBy(_._2.size): _*)) { // uses sorted tempRegs by degree
      var i: Int = 0
      while (adj.exists(a => if (tempAllocation.contains(a)) tempAllocation(a) == available(i) else false)) {
        i += 1
      }
      tempAllocation(tempReg) = available(i)
      // todo: spillage..?
    }
    tempAllocation
  }
}