package wacc.decompiler.HighLevelCodeGenerator

import wacc.decompiler.HighLevelCodeGenerator.HighLevelCodeGen.addingDelimiterForCodesInAScope
import wacc.decompiler.HighLevelCodeGenerator.TransExp.transBoolExp
import wacc.decompiler.L2IR.JCond


object TransCFBlock{
  import wacc.decompiler.CFgenerate.FlowBlocks._
  import wacc.decompiler.HighLevelCodeGenerator.HighLevelCodeGen.transL2Instructions
  import wacc.decompiler.HighLevelCodeGenerator.TransLoop.transLoop
  import wacc.decompiler.HighLevelCodeGenerator.TransCondition.{transCondition, getIfFollow}

  def setChecked(b: CFBlock): Unit = {
    b.checked = true
  }

  def transCFBlock(b: CFBlock): List[String] = {
    if (!b.checked) { // If b has not been checked by the visitor
      setChecked(b) // Meaning visitor has visited this block
      b match {
        case FallBlock(next) => 
          transL2Instructions(b.blockL2Ins) ::: transCFBlock(next)

        case CallBlock(_, nextFall) => 
          transL2Instructions(b.blockL2Ins) ::: transCFBlock(nextFall)

        // twoWayBlock in transCFBlock means the visitor is going to tranform an if-else statement
        case twoWayBlock@TwoWayBlock(nextTrue, nextFalse) => 
          val ifFollowBlock:Option[CFBlock] = getIfFollow(nextTrue, nextFalse)
          if (ifFollowBlock.isEmpty) {
            // ifFollowBlock is empty means two ways starting from this block will never meet
            transCondition(twoWayBlock)
          } else {
            setChecked(ifFollowBlock.get)
            transCondition(twoWayBlock) :::
            transCFBlock(ifFollowBlock.get)
        }

        case v@OneWayWhileBlock(cond, next) =>
          // Cond here is two way
          // v.body contains a list of blocks
          transL2Instructions(cond.blockL2Ins.init) :::
          ("while" + transBoolExp(cond.blockL2Ins.last.asInstanceOf[JCond].boolExp) ::
          "do" ::
          v.body.map(x => addingDelimiterForCodesInAScope(transCFBlock(x))) :::
          List("done")).mkString("\n") ::
          transCFBlock(next)

        case v@OneWayIfElseBlock(cond, next) =>
          // Head here is two way
          // v.trueExec contains a list of blocks in then{}
          // v.falseExec contains a list of blocks in else{}
          transL2Instructions(cond.blockL2Ins.init) :::
          ("if " + transBoolExp(cond.blockL2Ins.last.asInstanceOf[JCond].boolExp) ::
          "then" ::
          v.trueExec.map(x => addingDelimiterForCodesInAScope(transCFBlock(x))) ::
          "else" ::
          v.falseExec.map(x => addingDelimiterForCodesInAScope(transCFBlock(x))) ::
          "fi" ::
          List.empty).mkString("\n") ::
          transCFBlock(next)

        // Assume While procedure is always consisted of one one-way block pointing to another two-way block
        // next should be a two-way block and is the condition of the while-loop
        case OneWayBlock(next) =>
          transL2Instructions(b.blockL2Ins) ::: transCFBlock(next)
        //          setChecked(next)
//          // next is always expected to be a two-way block
//          transL2Instructions(b.blockL2Ins.dropRight(1)) ::: // The last L2 instruction in a one-way block should always be a JUnc() which should be ignored
//          transLoop(next) ::: transCFBlock(next.asInstanceOf[TwoWayBlock].nextFalse)

        case ReturnBlock() => 
          transL2Instructions(b.blockL2Ins)
      }
    } else {
      List.empty
    }
  }
}
