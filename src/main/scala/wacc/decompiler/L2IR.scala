package wacc.decompiler
import wacc.back.IR.Instruction
import wacc.decompiler.CFgenerate.FlowBlocks.CFBlock
object L2IR {
  sealed trait Exp

  sealed trait AriExp extends Exp

  sealed trait UniOP extends Exp
  case class NegL2(opr: AriExp) extends UniOP with AriExp {
    override def toString(): String = {
      "-" + opr.toString()
    }
  }
  case class LenL2(opr: AriExp) extends UniOP with AriExp {
    override def toString(): String = {
      "len" + opr.toString()
    }
  }
  case class OrdL2(opr: AriExp) extends UniOP with AriExp {
    override def toString(): String = {
      "ord" + opr.toString()
    }
  }
  case class ChrL2(opr: AriExp) extends UniOP with AriExp {
    override def toString(): String = {
      "chr" + opr.toString()
    }
  }

  sealed trait BiOp extends Exp
  case class Add(val opr1: AriExp, val opr2: AriExp) extends BiOp with AriExp {
    override def toString(): String = {
      opr1.toString() + " + " + opr2.toString()
    }
  }
  case class Sub(val opr1: AriExp, val opr2: AriExp) extends BiOp with AriExp {
    override def toString(): String = {
      opr1.toString() + " - " + opr2.toString()
    }
  }
  case class Mul(val opr1: AriExp, val opr2: AriExp) extends BiOp with AriExp {
    override def toString(): String = {
      opr1.toString() + " * " + opr2.toString()
    }
  }
  case class Div(val opr1: AriExp, val opr2: AriExp) extends BiOp with AriExp {
    override def toString(): String = {
      opr1.toString() + " / " + opr2.toString()
    }
  }
  case class Mod(val opr1: AriExp, val opr2: AriExp) extends BiOp with AriExp {
    override def toString(): String = {
      opr1.toString() + " % " + opr2.toString()
    }
  }

  sealed trait BoolExp extends Exp
  case class EqL2(opr1: Exp, opr2: Exp) extends BoolExp with BiOp {
    override def toString(): String = {
      opr1.toString() + " == " + opr2.toString()
    }
  }
  case class NEqL2(opr1: Exp, opr2: Exp)extends BoolExp with BiOp {
    override def toString(): String = {
      opr1.toString() + " != " + opr2.toString()
    }
  }
  case class GTL2(opr1: Exp, opr2: Exp) extends BoolExp with BiOp {
    override def toString(): String = {
      opr1.toString() + " > " + opr2.toString()
    }
  }
  case class LTL2(opr1: Exp, opr2: Exp) extends BoolExp with BiOp {
    override def toString(): String = {
      opr1.toString() + " < " + opr2.toString()
    }
  }
  case class GEL2(opr1: Exp, opr2: Exp) extends BoolExp with BiOp {
    override def toString(): String = {
      opr1.toString() + " >= " + opr2.toString()
    }
  }
  case class LEL2(opr1: Exp, opr2: Exp) extends BoolExp with BiOp {
    override def toString(): String = {
      opr1.toString() + " <= " + opr2.toString()
    }
  }
  case class AndL2(opr1: BoolExp, opr2: BoolExp) extends BoolExp with BiOp {
    override def toString(): String = {
      opr1.toString() + " && " + opr2.toString()
    }
  }
  case class OrL2(opr1: BoolExp, opr2: BoolExp) extends BoolExp with BiOp {
    override def toString(): String = {
      opr1.toString() + " || " + opr2.toString()
    }
  }
  case class Not(opr1: BoolExp) extends BoolExp with UniOP {
    override def toString(): String = {
      "!" + opr1.toString()
    }
  }

  case class L2Id(val name: String) extends AriExp with BoolExp {
    override def equals(that: Any): Boolean = that match {
      case _: L2Id =>
        that.asInstanceOf[L2Id].name == this.name
      case _ => false
    }
    override def toString(): String = {
      "R" + name
    }
  }
  case class L2Imm(value: Int) extends AriExp with BoolExp {
    override def toString(): String = {
      value.toString()
    }
  }

  sealed trait L2ins
  case class assign(l2Id: L2Id, aExp: Exp) extends L2ins
  case class JUnc() extends L2ins
  case class JCond(l2Id: L2Id, boolExp: BoolExp) extends L2ins
  case class call(funcBlock: String, args: List[Exp]) extends AriExp with L2ins
  case class ret(ariExp: AriExp) extends L2ins
  case class push(ariExp: AriExp) extends L2ins
  case class pop(l2Id: L2Id) extends L2ins
}

object L2Utils {
  import L2IR._

  // TODO, what supposed to be accomplished by dataflow and control flow analysis
  def convertIRtoL2(instructions: List[Instruction]): List[L2ins] = {
    List.empty
  }
}
