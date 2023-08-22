package wacc.decompiler.CFgenerate

import scala.collection.mutable.ListBuffer

object ControlFlowGraph {
  import FlowBlocks._
  import wacc.back.IR._

  // The main function for generate control flow graph
  def generateAllGraph(instruction: List[Instruction]): List[FlowGraph] = {
    val flowGraphs: List[FlowGraph] = divideSections(instruction)
    flowGraphs.map {
      case v@MainFG() => generateGraph(v)
      case v@UserFunc() => generateGraph(v)
      case v => v
    }
  }

  private object JumpCall {
    def unapply(ins: Instruction): Option[String] = ins match {
      case Branch(opr) => Some(opr)  // OneWay
      case Beq(opr)    => Some(opr)    // TwoWays
      case Bne(opr)    => Some(opr)
      case Bl(opr)     => Some(opr)    // CallBlocks
      case Bleq(opr)   => Some(opr)
      case Blvs(opr)   => Some(opr)
      case Bllt(opr)   => Some(opr)
      case Blge(opr)   => Some(opr)
      case Blne(opr)   => Some(opr)
      case _ => None
    }
  }

  // Take in a set of instructions within one function,
  // and create a corresponding control flow graph, return the head of graph
  private def generateGraph(flowGraph: FlowGraph): FlowGraph = {
    val instructions = flowGraph.ins

    var currentLabel = "main"
    val insInBlock: ListBuffer[Instruction] = ListBuffer.empty
    val allCFBlock: ListBuffer[CFBlock] = ListBuffer.empty

    // First generate loose connected by string blocks
    for (ins <- instructions.tail) ins match {
      case Label(v) =>  // Fall Through
        if (insInBlock.nonEmpty) {
          allCFBlock.addOne(createCFBlock(currentLabel, v, insInBlock.toList))
        }
        currentLabel = v
        insInBlock.clear()
        insInBlock.addOne(Label(v))

      case JumpCall(v) =>
        allCFBlock.addOne(createCFBlock(currentLabel, v, insInBlock.toList :+ ins))
        insInBlock.clear()
        currentLabel = ""

      case v =>
        insInBlock.addOne(v)
    }
    // Return block
    allCFBlock.addOne(createCFBlock(currentLabel, "ret", insInBlock.toList))


    val tightBlocks = CFBlocksIntoSubs(allCFBlock)
    flowGraph.nodes = tightBlocks
    flowGraph.nodes.foreach(pointBack)
    flowGraph.mainHead = flowGraph.nodes.head
    flowGraph
  }

  // Connect all blocks which point to their parent in labelfrom
  private def pointBack(cfBlock: CFBlock): Unit = {
    cfBlock match {
      case OneWayBlock(next) => next.labelFrom = next.labelFrom :+ cfBlock
      case TwoWayBlock(nextTrue, nextFalse) =>
        nextTrue.labelFrom = nextTrue.labelFrom :+ cfBlock
        nextFalse.labelFrom = nextFalse.labelFrom :+ cfBlock
      case CallBlock(_, nextFall) => nextFall.labelFrom = nextFall.labelFrom :+ cfBlock
      case FallBlock(next) => next.labelFrom = next.labelFrom :+ cfBlock
      case ReturnBlock() => None
      case _ => throw new Exception("Not supposed to be here")
    }
  }

  // Find the block with the corresponding label
  private def findBlock(label: String, allCFBlock: List[CFBlock]): CFBlock = {
    val res = allCFBlock.find(x => x.label == label)
    res match {
      case Some(v) => v
      case None => throw new Exception("Cannot find the label")
    }
  }

  //Find the next fall through block
  private def nextBlock(curBlock: CFBlock, allCFBlocks: List[FlowBlocks.CFBlock]): CFBlock = {
    var res: CFBlock = null
    for (Seq(ins1, ins2) <- allCFBlocks.sliding(2)) {
      if (ins1 eq curBlock) {
        res = ins2
      }
    }
    if (res == null) {
      throw new Exception("Laji")
    }
    res
  }

  // Fit all blocks into their correct subclass, and connect all blocks together
  private def CFBlocksIntoSubs(allCFBlock : ListBuffer[CFBlock]): List[CFBlock] = {
    val catBlocks = allCFBlock.map(categorise)
    catBlocks.map {
      case ReturnBlock() => None
      case v@OneWayBlock(_) => v.next = findBlock(v.labelTo, catBlocks.toList)
      case v@FallBlock(_) => v.next = findBlock(v.labelTo, catBlocks.toList)
      case v@CallBlock(_, _) => v.nextFall = nextBlock(v, catBlocks.toList)
      case v@TwoWayBlock(_, _) =>
        v.nextTrue = findBlock(v.labelTo, catBlocks.toList)
        v.nextFalse = nextBlock(v, catBlocks.toList)
    }
    catBlocks.toList
  }

  // Divide IR into different sections
  private def divideSections(instructions: List[Instruction]): List[FlowGraph] = {
    val allSections:ListBuffer[FlowGraph]  = ListBuffer.empty
    val currentSection: ListBuffer[Instruction] = ListBuffer.empty
    var currentGraph: FlowGraph = new FlowGraph
    var firstData = true

    for (Seq(ins, ins1) <- instructions.sliding(2)) {
      ins match {
        case DotData =>
          if (currentSection.isEmpty) {
            currentGraph = new MainDataSection
          } else {
            currentGraph.ins = currentSection.toList
            currentSection.clear()
            allSections.addOne(currentGraph)
            currentGraph = new SubDataSection
            if (firstData) {
              currentGraph = new MainDataSection
              firstData = false
            }
          }
        case DotText =>
          if (currentSection.isEmpty) {
            currentGraph = new MainFG
          } else {
            ins1 match {
              case DotGlobal("main") =>
                currentGraph.ins = currentSection.toList
                currentSection.clear()
                allSections.addOne(currentGraph)
                currentGraph = new MainFG

              case _ =>
                currentGraph.ins = currentSection.toList
                currentSection.clear()
                allSections.addOne(currentGraph)
                currentGraph = new ProgFunc
            }
          }
        case _ =>
            currentSection.addOne(ins)
      }
    }
    if (instructions.last != DotData || instructions.last != DotText) {
      currentSection.addOne(instructions.last)
    }
    currentGraph.ins = currentSection.toList
    allSections.addOne(currentGraph)

    // Assume main after data
    val mainPos = allSections.indexWhere{
      case MainFG() => true
      case _ => false
    }

    allSections.insertAll(mainPos + 1, divideMainFunction(allSections(mainPos)))
    allSections.remove(mainPos)
    allSections.toList
  }

  // Divide the main .text into main and its subfunctions
  private def divideMainFunction(mainFlow: FlowGraph): List[FlowGraph] = {
    val allSections: ListBuffer[FlowGraph] = ListBuffer.empty
    val curIns: ListBuffer[Instruction] = ListBuffer.empty
    var curFG = new FlowGraph
    val localLabel = "\\.L.+".r

    for (ins <- mainFlow.ins.tail) {
      ins match {
        case DotGlobal(_) => None
        case Label(localLabel()) => curIns.addOne(ins)
        case Label("main") =>
          if (curIns.nonEmpty) {
            curFG.ins = curIns.toList
            allSections.addOne(curFG)
            curIns.clear()
          }
          curFG = new MainFG
          curIns.addOne(ins)
        case Label(_) =>
          if (curIns.nonEmpty) {
            curFG.ins = curIns.toList
            allSections.addOne(curFG)
            curIns.clear()
          }
          curFG = new UserFunc
          curIns.addOne(ins)
        case _ => curIns.addOne(ins)
      }
    }
    curFG.ins = curIns.toList
    allSections.addOne(curFG)

    allSections.toList
  }

  // Probably not TODO,
  //  Optimise Jmp by replacing consecutive jumps with one jump
  private def peephole(CFBlock: CFBlock) : CFBlock= {
    ReturnBlock()
  }
}

