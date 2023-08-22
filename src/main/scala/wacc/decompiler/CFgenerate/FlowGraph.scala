package wacc.decompiler.CFgenerate

import wacc.back.IR.Instruction
import wacc.decompiler.CFgenerate.FlowBlocks._


class FlowGraph {
  var mainHead: CFBlock = new ReturnBlock
  var nodes: List[CFBlock] = List.empty
  var ins: List[Instruction] = List.empty



  override def toString: String =
    "Start at block: " +  mainHead.label +  "\n" +
      "With Blocks: " + nodes.map(v => v.label + "(" + v.blockType + ")").mkString(", ")
}

sealed trait TextSection extends FlowGraph
sealed trait DataSection extends FlowGraph

case class MainDataSection() extends DataSection
case class SubDataSection() extends DataSection

case class MainFG() extends TextSection
case class UserFunc() extends TextSection
case class ProgFunc() extends TextSection
