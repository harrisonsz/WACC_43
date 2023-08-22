package wacc.decompiler.DataFlow

import wacc.decompiler.L2IR._

object OpUnapply {
  object BinaryOperator{
    def unapply(exp: Exp): Option[(Exp, Exp)] = exp match {
      case Add(opr1, opr2) => Some(opr1, opr2)
      case Sub(opr1, opr2) => Some(opr1, opr2)
      case Mul(opr1, opr2) => Some(opr1, opr2)
      case Div(opr1, opr2) => Some(opr1, opr2)
      case EqL2(opr1, opr2) => Some(opr1, opr2)
      case NEqL2(opr1, opr2)=> Some(opr1, opr2)
      case GTL2(opr1, opr2) => Some(opr1, opr2)
      case LTL2(opr1, opr2) => Some(opr1, opr2)
      case GEL2(opr1, opr2) => Some(opr1, opr2)
      case LEL2(opr1, opr2) => Some(opr1, opr2)
      case AndL2(opr1, opr2) => Some(opr1, opr2)
      case OrL2(opr1, opr2) => Some(opr1, opr2)
      case _ => None
    }
  }
  object BinaryOperatorType{
    def unapply(exp: Exp): Option[(Exp, Exp) => Exp] = exp match {
      case Add(opr1, opr2) => Some(Add.asInstanceOf[(Exp, Exp) => Exp])
      case Sub(opr1, opr2) => Some(Sub.asInstanceOf[(Exp, Exp) => Exp])
      case Mul(opr1, opr2) => Some(Mul.asInstanceOf[(Exp, Exp) => Exp])
      case Div(opr1, opr2) => Some(Div.asInstanceOf[(Exp, Exp) => Exp])
      case EqL2(opr1, opr2) => Some(EqL2.asInstanceOf[(Exp, Exp) => Exp])
      case NEqL2(opr1, opr2)=> Some(NEqL2.asInstanceOf[(Exp, Exp) => Exp])
      case GTL2(opr1, opr2) => Some(GTL2.asInstanceOf[(Exp, Exp) => Exp])
      case LTL2(opr1, opr2) => Some(LTL2.asInstanceOf[(Exp, Exp) => Exp])
      case GEL2(opr1, opr2) => Some(GEL2.asInstanceOf[(Exp, Exp) => Exp])
      case LEL2(opr1, opr2) => Some(LEL2.asInstanceOf[(Exp, Exp) => Exp])
      case AndL2(opr1, opr2) => Some(AndL2.asInstanceOf[(Exp, Exp) => Exp])
      case OrL2(opr1, opr2) => Some(OrL2.asInstanceOf[(Exp, Exp) => Exp])
      case _ => None
    }
  }

  object UnaryOperator{
    def unapply(exp: Exp): Option[Exp] = exp match {
      case ChrL2(opr) => Some(opr)
      case LenL2(opr) => Some(opr)
      case NegL2(opr) => Some(opr)
      case OrdL2(opr) => Some(opr)
      case Not(opr) => Some(opr)
      case _ => None
    }
  }

  object UnaryOperatorType{
    def unapply(exp: Exp): Option[Exp => Exp] = exp match {
      case ChrL2(opr) => Some(ChrL2.asInstanceOf[Exp => Exp])
      case LenL2(opr) => Some(LenL2.asInstanceOf[Exp => Exp])
      case NegL2(opr) => Some(NegL2.asInstanceOf[Exp => Exp])
      case OrdL2(opr) => Some(OrdL2.asInstanceOf[Exp => Exp])
      case Not(opr) => Some(Not.asInstanceOf[Exp => Exp])
      case _ => None
    }
  }
}
