package wacc.decompiler.DataFlow

import wacc.decompiler.CFgenerate.FlowBlocks._
import wacc.decompiler.L2IR.L2ins
import scala.collection.mutable.ListBuffer
import DataFlowEquations._
import wacc.decompiler.L2IR._
import scala.collection.mutable.Set
import OpUnapply._
import wacc.decompiler.CFgenerate.FlowGraph

object DataFlowOptimisation {

  def clearAllVisited(fg: FlowGraph): Unit = {
    fg.nodes.foreach{
      case x: OneWayBlock => 
        x.asInstanceOf[OneWayBlock].visitedNext = false
      case x: TwoWayBlock =>
        val z = x.asInstanceOf[TwoWayBlock]
        z.visitedFalse = false
        z.visitedTrue = false
      case x: CallBlock =>
        x.asInstanceOf[CallBlock].visitedNext = false
      case x: FallBlock =>
        x.asInstanceOf[FallBlock].visitedNext = false
      case _ => None

    }
  }

  def optGraphDataFlow(fg: FlowGraph): FlowGraph = {
    var prev = fg.nodes.map(_.blockL2Ins)
    while ({
      fg.nodes.foreach(x => x.labelFromBools = ListBuffer.fill(x.labelFrom.size)(false))
      clearAllVisited(fg)
      fg.nodes.foreach(x => x.blockL2Ins = deadRegElim(x))
      fg.nodes.foreach(x => x.labelFromBools = ListBuffer.fill(x.labelFrom.size)(false))
      clearAllVisited(fg)
      fg.nodes.foreach(x => x.blockL2Ins = regCopyProp(x))
      //applyPostOrder(fg.mainHead, x => x.blockL2Ins = regCopyProp(x))
      val cur = fg.nodes.map(_.blockL2Ins)
      val a = !prev.equals(cur)
      prev = cur
      a
    }) ()
    //fg.nodes.foreach(x => x.blockL2Ins = deadRegElim(x))
    // fg.nodes.foreach(x => x.blockL2Ins = regCopyProp(x))
    // fg.nodes.foreach(x => x.blockL2Ins = deadRegElim(x))
    // fg.nodes.foreach(x => x.blockL2Ins = regCopyProp(x))
    // fg.nodes.foreach(x => x.blockL2Ins = deadRegElim(x))
    // fg.nodes.foreach(x => x.blockL2Ins = regCopyProp(x))
    // fg.nodes.foreach(x => x.blockL2Ins = deadRegElim(x))
    fg
  }

  def applyPostOrder(cur: CFBlock, func: CFBlock => Unit): Unit = {
    cur match {
      case OneDirectBlock(next) =>
        applyPostOrder(next, func)
      case TwoWayBlock(nextTrue, nextFalse) =>
        applyPostOrder(nextTrue, func)
        applyPostOrder(nextFalse, func)
      case _ =>
    }
    func(cur)
  }

  def deadRegElim(block: CFBlock): List[L2ins] = {
    val result: ListBuffer[L2ins] = ListBuffer.from(block.blockL2Ins)
    var len = result.size
    var i = 0
    var bi = 0
    while (i < len) {
      result(i) match {
        case assign(l2Id, _) =>
          if (duChain(block, bi, l2Id).isEmpty) {
            result.remove(i)
            len -= 1
          } else {
            i += 1
          }
        case _ => 
          i += 1
      }
      bi += 1
    }
    result.toList
  }

  def regCopyProp(block: CFBlock): List[L2ins] = {
    val result: ListBuffer[L2ins] = ListBuffer.from(block.blockL2Ins)
    val len = result.size
    var i = 0
    while (i < len) {
      val usedRegs: Set[L2Id] = result(i) match {
        case assign(idx, exp) => 
          getUsedRegInExp(exp)
        case JCond(l2Id, boolExp) => 
          getUsedRegInExp(boolExp)
        case call(funcBlock, args) =>
          Set.from(args.map(getUsedRegInExp(_).toList).flatten)
        case _ => 
          Set.empty
      }
      for (reg <- usedRegs) {
        var prop = false
        val ud = udChain(block, i, reg).toList
        ud match {
          case List((b, ix)) =>
            prop = true
            val ins = b.blockL2Ins(ix)
            ins match {
              case assign(_, aExp) =>
                val rhs = getUsedRegInExp(aExp)
                if (!rhs.isEmpty && !rhs.map(xClear((b, ix), (block, i), _)).reduce(_ && _)) {
                  prop = false
                }
              case _ => throw new Exception("ud must return defines")
            }
            if (prop) {
              result(i) = replaceReg(result(i), reg, ins.asInstanceOf[assign].aExp)
            }
          case _ => None
        }
      }
      i += 1
    }
    result.toList
  }

  def replaceReg(exp: L2ins, prev: L2Id, update: Exp): L2ins = {
    exp match {
      case assign(id, aExp) => 
        assign(id, replaceRegInExp(aExp, prev, update))
      case JCond(l2Id, boolExp) => 
        JCond(l2Id, replaceRegInExp(boolExp, prev, update).asInstanceOf[BoolExp])
      case JUnc() => exp
      case call(funcBlock, args) => 
        call(funcBlock, args.map(replaceRegInExp(_, prev, update)))
      case ret(ariExp) => 
        ret(replaceRegInExp(ariExp, prev, update).asInstanceOf[AriExp])
      case _ =>
        throw new Exception("Unexpected Exp: " + exp)
    }
  }

  def replaceRegInExp(exp: Exp, prev: L2Id, update: Exp): Exp = (exp, exp) match {
    case (BinaryOperator(opr1, opr2), BinaryOperatorType(bOp)) =>
      bOp(replaceRegInExp(opr1, prev, update), replaceRegInExp(opr2, prev, update))
    case (UnaryOperator(opr), UnaryOperatorType(uOp)) =>
      uOp(replaceRegInExp(opr, prev, update))
    case (L2Id(_), L2Id(_)) =>
      if (exp.asInstanceOf[L2Id].equals(prev)) {
        update
      } else {
        exp
      }
    case (L2Imm(_), L2Imm(_)) =>
      exp
    case _ =>
      throw new Exception("Unexpected Exp: " + exp)
  }

  def getUsedRegInExp(exp: Exp): Set[L2Id] = {
    val result: Set[L2Id] = Set.empty
    exp match {
      case OpUnapply.BinaryOperator(opr1, opr2) =>
        for (opr <- List(opr1, opr2)) {
          opr match {
            case _: L2Id =>
              result.addOne(opr.asInstanceOf[L2Id])
            case _: L2Imm =>
              None
            case _ =>
              result.addAll(getUsedRegInExp(opr)) 
          }
        }
      case OpUnapply.UnaryOperator(opr) =>
        opr match {
          case _: L2Id =>
            result.addOne(opr.asInstanceOf[L2Id])
          case _: L2Imm =>
            None
          case _ =>
            result.addAll(getUsedRegInExp(exp))
        }
      case _: L2Id =>
        result.addOne(exp.asInstanceOf[L2Id])
      case _: L2Imm =>
        None
      case _ =>
        throw new Exception("UnExpected Exp: " + exp)
    }
    result
  }
}
