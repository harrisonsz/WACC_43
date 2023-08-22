package wacc.decompiler.DataFlow

import wacc.decompiler.CFgenerate.FlowBlocks._
import wacc.decompiler.L2IR._

import scala.collection.mutable.Set
import wacc.back.IR

import scala.collection.mutable

object DataFlowEquations {

  def duChain(block: CFBlock, i: Int, id: L2Id): Set[(CFBlock, Int)] = {
    val result: Set[(CFBlock, Int)] = Set.empty
    val liveOut = liveOutBlock(block)
    val reachOut = reachOutBlock(block)
     val used = usedBlock(block)
     val defAfter = killDefBlock(block).filter{case ((idx, _), x) => id.equals(idx) && x > i}
     var min = block.blockL2Ins.size
     if (defAfter.nonEmpty) {
       min = defAfter.map{case (_, ix) => ix}.min + 1
     }
     val useInBlock = used.filter{case ((idx, _), ix) => idx.equals(id) && ix > i && ix < min}
       .map{case (_, ix) => ix}
     result.addAll(List.fill(useInBlock.size)(block).zip(useInBlock))
     if (reachOut.contains(((id, block), i))) {
       result.addAll(liveOut.filter{case ((idx, _), _) => idx.equals(id)}
         .map{case ((_, b), ix) => (b, ix)})
     }
    result
  }

  def udChain(block: CFBlock, i: Int, id: L2Id): Set[(CFBlock, Int)] = {
    val localExpose = localExposedUse(block)
    if (localExpose.contains(((id, block), i))) {
      val reachIn = reachInBlock(block)
      reachIn.filter{case ((idx, _), _) => idx.equals(id)}.map{case ((_, b), ix) => (b, ix)}
    } else {
      val defBlock = killDefBlock(block)
      val defBefore = defBlock.filter{case ((idx, _), ix) => idx.equals(id) && ix < i}
      if (defBefore.isEmpty) {
        Set.empty
      } else {
        val max = defBefore.maxBy{_._2}
        Set((max._1._2, max._2))
      }
    }
  }

  def xClear(from: (CFBlock, Int), to: (CFBlock, Int), id: L2Id): Boolean = {
    val allPaths = findPaths(from._1, to._1).filter(_.isDefined).map{case value => value.get}
    val allDef = allPaths.map(x => x.slice(1, x.size)).map(_.map(killDefBlock(_))).flatten.flatten
    val at = allDef.map{case ((idx, _), _) => !idx.equals(id)}
    val atClear = at.isEmpty || at.reduce(_ && _)
    killDefBlock(from._1).filter{case ((idx, _), ix) => idx.equals(id) && ix >= from._2}
      .addAll(killDefBlock(to._1)
        .filter{case ((idx, _), ix) => idx.equals(id) && ix < to._2}).isEmpty && 
    atClear
  }

  def findPaths(from: CFBlock, to: CFBlock): List[Option[List[CFBlock]]] = {
    if (from.equals(to)) return List(Some(List(to)))
    from match {
      case ReturnBlock() => List(None)
      case TwoWayBlock(nextTrue, nextFalse) => 
        val l = (nextTrue.equals(from), nextFalse.equals(from)) match {
          case (true, false) => List(nextFalse)
          case (false, true) => List(nextTrue)
          case (false, false) => List(nextTrue, nextFalse)
          case _ => throw new Exception("Unexpected two way both point to self")
        }
        l.map{
          case next => 
            val ath = findPaths(next, to)
            ath.map{
              case None => None
              case Some(value) => Some(from :: value)
            }
        }.flatten
      case BlockUnapply.SingleWayBlock(next) => 
        val ath = findPaths(next, to)
        ath.map{
          case None => None
          case Some(value) => Some(from :: value)
        }
    }
  }

  def reachInBlock(block: CFBlock): Set[((L2Id, CFBlock), Int)] = {
    if (!block.blockInstructions.isEmpty && 
        block.blockInstructions.head.isInstanceOf[IR.Label] && 
        !block.label.startsWith(".L")) {
      return Set.empty
    }
    val result: Set[((L2Id, CFBlock), Int)] = Set.empty
    for (i <- block.labelFrom.indices) {
      if (!block.labelFromBools(i)) {
        block.labelFromBools(i) = true
        result.addAll(reachOutBlock(block.labelFrom(i)))
      }
    }
    //block.labelFrom.foreach(x => result.addAll(reachOutBlock(x)))
    result
  }

  def reachOutBlock(block: CFBlock): Set[((L2Id, CFBlock), Int)] = {
    val killDef = killDefBlock(block)
    reachInBlock(block).filterNot{case ((id, _), _) => 
      killDef.map{case ((id, _), _) => id}.contains(id)}.addAll(localAvailDef(killDef))
  }

  def liveInBlock(block: CFBlock): Set[((L2Id, CFBlock), Int)] = {
    val temp1 = localExposedUse(block)
    localExposedUse(block).addAll(liveOutBlock(block)
      .filterNot{case ((id, _), _) => 
        killDefBlock(block).map{case ((id, _), _) => id}.contains(id)})
  }

  def liveOutBlock(block: CFBlock): Set[((L2Id, CFBlock), Int)] = block match {
    case _: ReturnBlock => 
      Set.empty
    case _: OneWayBlock =>
      if (block.asInstanceOf[OneWayBlock].visitedNext) {
        Set.empty
      } else {
        block.asInstanceOf[OneWayBlock].visitedNext = true
        liveInBlock(block.asInstanceOf[OneWayBlock].next)
      }
    case _: TwoWayBlock =>
      val cast = block.asInstanceOf[TwoWayBlock]
      (cast.visitedTrue, cast.visitedFalse) match {
        case (true, true) =>
          Set.empty
        case (true, false) =>
          cast.visitedFalse = true
          liveInBlock(block.asInstanceOf[TwoWayBlock].nextFalse)
        case (false, true) =>
          cast.visitedTrue = true
          liveInBlock(block.asInstanceOf[TwoWayBlock].nextTrue)
        case (false, false) =>
          cast.visitedTrue = true
          cast.visitedFalse = true
          liveInBlock(block.asInstanceOf[TwoWayBlock].nextTrue)
          .addAll(liveInBlock(block.asInstanceOf[TwoWayBlock].nextFalse))
      }
    case _: CallBlock =>
      if (block.asInstanceOf[CallBlock].visitedNext) {
        Set.empty
      } else {
        block.asInstanceOf[CallBlock].visitedNext = true
        liveInBlock(block.asInstanceOf[CallBlock].nextFall)
      }
    case _: FallBlock =>
      if (block.asInstanceOf[FallBlock].visitedNext) {
        Set.empty
      } else {
        block.asInstanceOf[FallBlock].visitedNext = true
        liveInBlock(block.asInstanceOf[FallBlock].next)
      }
  }

  def killDefBlock(block: CFBlock): Set[((L2Id, CFBlock), Int)] = {
    val result: Set[((L2Id, CFBlock), Int)] = Set.empty
    for (i <- 0 until block.blockL2Ins.size) {
      block.blockL2Ins(i) match {
        case assign(l2Id, aExp) => 
          result.addOne(((l2Id, block), i))
        case _ => None
      }
    }
    result
  }

  def localAvailDef(defBlock: Set[((L2Id, CFBlock), Int)]): Set[((L2Id, CFBlock), Int)] = {
    val result: Set[((L2Id, CFBlock), Int)] = Set.empty
    result.addAll(defBlock.groupBy(_._1).map{case (t, l) => l.maxBy(_._2)}.toSet)
  }

  private def matchExp(exp: Exp, i: Int, block: CFBlock): mutable.Set[((L2Id, CFBlock), Int)] = {
    val result: mutable.Set[((L2Id, CFBlock), Int)] = mutable.Set.empty
    exp match {
      case OpUnapply.BinaryOperator(opr1, opr2) =>
        List(opr1, opr2).foreach {
          case opr@(_: L2Id) =>
            result.addOne(((opr.asInstanceOf[L2Id], block), i))
          case _: L2Imm => None
          case opr =>
            result.addAll(matchExp(opr, i, block))
        }
        result
      case OpUnapply.UnaryOperator(opr) =>
        opr match {
          case _: L2Id =>
            result.addOne(((opr.asInstanceOf[L2Id], block), i))
          case _: L2Imm => Set.empty
          case _ => 
            result.addAll(matchExp(opr, i, block))
        }
      case _: L2Imm =>
        Set.empty
      case _: L2Id =>
        result.addOne(((exp.asInstanceOf[L2Id], block), i))

      case _ => throw new Exception("Met unexpected Exp")
    }
  }

  def usedBlock(block: CFBlock): Set[((L2Id, CFBlock), Int)] = {
    val result: Set[((L2Id, CFBlock), Int)] = Set.empty
    for (i <- 0 until block.blockL2Ins.size) {
      block.blockL2Ins(i) match {
        case assign(l2Id, exp) => 
          result.addAll(matchExp(exp, i, block))
        case JCond(l2Id, boolExp) => 
          result.addAll(matchExp(boolExp, i, block))
        case call(funcBlock, args) => 
          args.map(matchExp(_, i, block)).foreach(result.addAll(_))
          result
        case ret(exp) => 
          result.addAll(matchExp(exp, i, block))
        case _ => None
      }
    }
    result
  }

  def localExposedUse(block: CFBlock): Set[((L2Id, CFBlock), Int)] = {
    val result: Set[((L2Id, CFBlock), Int)] = Set.empty
    val allUsed = usedBlock(block)
    val allDef = killDefBlock(block)
    for ((t, i) <- allUsed) {
      if (allDef.forall{case (tx, ix) => !t.equals(tx) || ix > i}) {
        result.addOne((t, i))
      }
    }
    result
  }
}
