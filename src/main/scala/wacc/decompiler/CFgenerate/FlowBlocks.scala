package wacc.decompiler.CFgenerate

import wacc.back.IR._
import wacc.decompiler.L2IR

import scala.collection.mutable.ListBuffer

object FlowBlocks {
  // Abstract data structure for control block,
  // Since there is only maximum two way conditionals

  class CFBlock{
    var label: String = ""
    var labelTo: String = ""
    var labelFrom: List[CFBlock] = List.empty
    var blockInstructions: List[Instruction] = List.empty
    var blockL2Ins: List[L2IR.L2ins] = List.empty
    val blockType: String = "CFBlock"
    var labelFromBools: ListBuffer[Boolean] = ListBuffer.empty

    // This variable is used in high level code generator, when the visiter visit the block,
    // this will be set to true
    var checked = false

    override def toString: String =
      "name (" + blockType + ") : " + label + ", to " + labelTo + " \n with instructions: " + blockInstructions.mkString(" \n")

    override def hashCode(): Int = blockInstructions.hashCode()
  }

  def createCFBlock(currentLabel: String, dest: String, instructions: List[Instruction]): CFBlock = {
    val b = new CFBlock
    b.label = currentLabel
    b.labelTo = dest
    b.blockInstructions = instructions
    b
  }

  // Turn blocks into categorised sub blocks
  def categorise(src: CFBlock): CFBlock = {
    var subBlock: CFBlock = new CFBlock
    val lastIns: Instruction = src.blockInstructions.last
    if (src.labelTo == "ret") {
      subBlock = ReturnBlock()
    } else lastIns match {
      case _: Branch =>
        subBlock = OneWayBlock(null)
      case _: Beq | _: Bne =>
        subBlock = TwoWayBlock(null, null)
      case _: Bl  | _: Bleq | _: Blvs | _: Bllt | _:Blge | _:Blne =>
        subBlock = CallBlock(src.labelTo, null)
      case _ =>
        subBlock = FallBlock(null)
    }

    subBlock.label = src.label
    subBlock.labelTo = src.labelTo
    subBlock.blockL2Ins = src.blockL2Ins
    subBlock.blockInstructions = src.blockInstructions
    subBlock
  }

  case class OneWayBlock(var next: CFBlock) extends CFBlock {
    var visitedNext = false
    override val blockType: String = "OneWayBlock"
  }
  case class TwoWayBlock(var nextTrue: CFBlock, var nextFalse: CFBlock) extends CFBlock {
    var visitedTrue = false
    var visitedFalse = false
    override val blockType: String = "TwoWayBlock"
  }
  case class CallBlock(var nextFunc: String, var nextFall: CFBlock) extends CFBlock {
    var visitedNext = false
    override val blockType: String = "CallBlock"
  }
  case class ReturnBlock() extends CFBlock {
    override val blockType: String = "Return Block"
  }
  case class FallBlock(var next: CFBlock) extends CFBlock {
    var visitedNext = false
    override val blockType: String = "Fall Block"
  }
  // Contain the entire If else block include from condition to the exit node
  case class OneWayIfElseBlock(var head: CFBlock, var next: CFBlock) extends CFBlock {
    var trueExec: List[CFBlock] = List.empty
    var falseExec: List[CFBlock] = List.empty
    override val blockType: String = "IfElseBlock"
  }
  // Contain the entire while block including exit node
  case class OneWayWhileBlock(var cond: CFBlock, var next: CFBlock) extends CFBlock {
    var body: List[CFBlock] = List.empty
    override val blockType: String = "WhileBlock"
  }

  object OneDirectBlock {
    def unapply(cFBlock: CFBlock): Option[CFBlock] = cFBlock match {
      case OneWayBlock(opr) => Some(opr)
      case CallBlock(_, opr) => Some(opr)
      case FallBlock(opr) => Some(opr)
      case OneWayWhileBlock(_, opr) => Some(opr)
      case OneWayIfElseBlock(_, opr) => Some(opr)
      case _ => None
    }
  }
}
