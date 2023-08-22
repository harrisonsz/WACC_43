package wacc.decompiler.HighLevelCodeGenerator

object TransCondition{
  import wacc.decompiler.CFgenerate.FlowBlocks._
  import wacc.decompiler.L2IR._
  import wacc.decompiler.HighLevelCodeGenerator.HighLevelCodeGen.
  {transL2Instructions,addingDelimiterForCodesInAScope}
  import wacc.decompiler.HighLevelCodeGenerator.TransExp.transBoolExp
  import wacc.decompiler.HighLevelCodeGenerator.TransCFBlock.transCFBlock
  import scala.collection.mutable.ArrayBuffer

  // In the current version of WACC, condition only has if-else so nextTrue and nextFalse can only lead to 
  // a same block which is the if-follow block or each of them should always lead to a return block.
  // When this function returns None, it means the program exits from an if-else statement or the function
  // returns from an if-else statment
  def getIfFollow(nextTrue: CFBlock, nextFalse: CFBlock): Option[CFBlock] = {
    var cur:Option[CFBlock] = Some(nextTrue)
    val followingBlocks = getFollowingBlocks(nextFalse)
    while (cur != None) {
      if (followingBlocks.exists(x => x eq cur.get)) { 
        // Using eq as I want to check if the references of two block are equal
        return cur
      }
      cur = goNextBlock(cur.get)
    }
    None
  }

  def getFollowingBlocks(start: CFBlock): List[CFBlock] = {
    var cur:Option[CFBlock] = goNextBlock(start)
    val blocks = ArrayBuffer[CFBlock]()
    while (cur != None) {
      blocks.addOne(cur.get)
      cur = goNextBlock(cur.get)
    }
    blocks.toList
  }
  
  def goNextBlock(currentBlock: CFBlock): Option[CFBlock] = currentBlock match {
    case FallBlock(next) => Some(next)
    case OneWayBlock(next) => Some(next)
    case TwoWayBlock(nextTrue, nextFalse) => Some(nextFalse)
    case CallBlock(nextFunc, nextFall) => Some(nextFall)
    case ReturnBlock()   => None
  }

  def transCondition(b: TwoWayBlock): List[String] = {
    // Removing the last one because the last one is expected to be the JCond
    transL2Instructions(b.blockL2Ins.dropRight(1)) :::
    ("if " + transBoolExp(b.blockL2Ins.last.asInstanceOf[JCond].boolExp) ::
    "then" ::
    addingDelimiterForCodesInAScope(transCFBlock(b.nextTrue)) ::
    "else" ::
    addingDelimiterForCodesInAScope(transCFBlock(b.nextFalse)) ::
    "fi" ::
    List.empty).mkString("\n") ::
    List.empty
  }
}
