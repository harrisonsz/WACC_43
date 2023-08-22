package wacc.decompiler.ControlFlowAnalysis

import wacc.decompiler.CFgenerate.FlowBlocks._
import wacc.decompiler.CFgenerate._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object CFAnalysis {
  // After analysis, the list of nodes of graph is no longer reliable
  def cFAnalysis(flowGraphs: List[FlowGraph]): List[FlowGraph] = {
    flowGraphs.map{
      case MainFG() => cFAnalysisEach(_)
      case UserFunc() => cFAnalysisEach(_)
      case v => v
      }
    flowGraphs
  }

  def cFAnalysisEach(flowGraph: FlowGraph): FlowGraph = {
    flowGraph.mainHead = identifyOneBlock(flowGraph.mainHead)
    identifyLater(flowGraph.mainHead)
    flowGraph
  }

  @tailrec
  private def identifyOneBlock(cFBlock: CFBlock): CFBlock = {
    var res: CFBlock = cFBlock
    cFBlock match {
      case TwoWayBlock(_, _) =>
        res = findStructure(res)
        identifyOneBlock(res)
      case _ => res
    }
  }

  // Identify structure and only connect the head, tail should be connected in find
  @tailrec
  private def identifyLater(cFBlock: CFBlock): Unit =
   cFBlock match {
      case _: ReturnBlock  =>
      case TwoWayBlock(_, _) => throw new Exception("At this point this block should be one way")
      case lastBlock@OneDirectBlock(next) =>
        next match {
          case v@TwoWayBlock(_, _) =>
            lastBlock match {
              case cBlock@OneWayBlock(_) =>
                cBlock.next = findStructure(v)
                identifyLater(cFBlock)
              case cBlock@CallBlock(_, _) =>
                cBlock.nextFall = findStructure(v)
                identifyLater(cFBlock)
              case cBlock@FallBlock(_) =>
                cBlock.next = findStructure(v)
                identifyLater(cFBlock)
              case cBlock@OneWayIfElseBlock(_, _) =>
                cBlock.next = findStructure(v)
                identifyLater(cFBlock)
              case cBlock@OneWayWhileBlock(_, _) =>
                cBlock.next = findStructure(v)
                identifyLater(cFBlock)
              case _ => throw new Exception("Only expecting one way block here")
            }
          case _: ReturnBlock =>
          case v@OneDirectBlock(_) => identifyLater(v)
        }
    }

  private def findStructure(block: CFBlock): CFBlock = {
    val trueExec: ListBuffer[CFBlock] = ListBuffer.empty
    val falseExec: ListBuffer[CFBlock] = ListBuffer.empty
    block match {
      case v@TwoWayBlock(trueBlock, falseBlock) =>
        val trueForWhileBody = findAllIfPointBack(v, direction = true, trueExec)
        val falseForWhileBody = findAllIfPointBack(v, direction = false, falseExec)
        if (trueForWhileBody) {
          val res = OneWayWhileBlock(block, falseBlock)
          res.body = trueExec.toList
          res
        } else if (falseForWhileBody) {
          val res = OneWayWhileBlock(block, trueBlock)
          res.body = falseExec.toList
          res
        } else {
          val (l1, l2) = cutUtilMeetingPoint(trueExec, falseExec)
          val res = OneWayIfElseBlock(block, falseExec.last)
          res.trueExec = l1
          res.falseExec = l2
          res
        }
      case _ => throw new Exception("This has to be two way")
    }
  }

  private def notContainWithRef(v: CFBlock, falseExec: ListBuffer[CFBlock]): Boolean = {
    falseExec.forall(x => x ne v)
  }

  private def cutUtilMeetingPoint(trueExec: ListBuffer[CFBlock], falseExec: ListBuffer[CFBlock]): (List[CFBlock], List[CFBlock]) = {
    var lastBlock: CFBlock = ReturnBlock()
    (trueExec.takeWhile(v => {
      lastBlock = v
      notContainWithRef(v, falseExec)
    }).toList,
    {
      falseExec.addOne(lastBlock)
      falseExec.takeWhile(v => v != lastBlock).toList})
  }

  // May encounter two way block in the middle which needs to be dealt, return true if point back to block
  // Does not contain head of while
  private def findAllIfPointBack(block: CFBlock, direction: Boolean, exec: ListBuffer[CFBlock]): Boolean = {
    var lastBlock = block
    var currentBlock = block match {
      case TwoWayBlock(nextTrue, nextFalse) => if (direction) {nextTrue} else nextFalse
      case _ => throw new Exception("Only two way block passed in as head")
    }
    while (currentBlock match {
      case _ : ReturnBlock =>
        exec.addOne(currentBlock)
        false
      case v@TwoWayBlock(_, _) =>
        val res = identifyOneBlock(v)
        lastBlock match {
          case v@OneWayBlock(_) => v.next = res
          case v@CallBlock(_, _) => v.nextFall = res
          case v@FallBlock(_) => v.next = res
          case v@OneWayIfElseBlock(_, _) => v.next = res
          case v@OneWayWhileBlock(_, _) => v.next = res
          case v@TwoWayBlock(_, _) => // Only when lastblock was head
            if (direction) {v.nextTrue = res} else {v.nextFalse = res}
        }
        currentBlock = res
        true
      case OneDirectBlock(next) =>
        if (next eq block) {
          exec.addOne(currentBlock)
          return true
        }
        true
    }) {
      exec.addOne(currentBlock)
      lastBlock = currentBlock
      currentBlock = currentBlock match {
        case OneDirectBlock(next) => next
        case _ => throw new Exception("Only One Direct Block")
      }
    }
    false
  }
}
