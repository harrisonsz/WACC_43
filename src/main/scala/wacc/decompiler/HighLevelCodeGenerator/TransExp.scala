package wacc.decompiler.HighLevelCodeGenerator

import wacc.decompiler.L2IR._

object AriExpBinaryOperator{
  def unapply(ariExp: AriExp): Option[(AriExp, AriExp)] = ariExp match {
    case Add(opr1, opr2) => Some(opr1, opr2)
    case Sub(opr1, opr2) => Some(opr1, opr2)
    case Mul(opr1, opr2) => Some(opr1, opr2)
    case Div(opr1, opr2) => Some(opr1, opr2)
    case _ => None
  }
}

object AriExpUnaryOperator{
  def unapply(ariExp: AriExp): Option[AriExp] = ariExp match {
    case ChrL2(opr) => Some(opr)
    case LenL2(opr) => Some(opr)
    case NegL2(opr) => Some(opr)
    case OrdL2(opr) => Some(opr)
    case _ => None
  }
}

object BoolExpBinaryOperator{
  def unapply(boolExp: BoolExp): Option[(Exp, Exp)] = boolExp match {
    case EqL2(opr1, opr2) => Some(opr1, opr2)
    case NEqL2(opr1, opr2)=> Some(opr1, opr2)
    case GTL2(opr1, opr2) => Some(opr1, opr2)
    case LTL2(opr1, opr2) => Some(opr1, opr2)
    case GEL2(opr1, opr2) => Some(opr1, opr2)
    case LEL2(opr1, opr2) => Some(opr1, opr2)
    case AndL2(opr1, opr2) => Some(opr1, opr2)
    case OrL2(opr1, opr2) => Some(opr1, opr2)
    case _  => None
  }
}

object TransExp{
  def transAriExp(aExp: AriExp): String = {
    val s = aExp match {
      case AriExpBinaryOperator(opr1, opr2) => transAriExp(opr1) + transAriOp(aExp) + transAriExp(opr2)
      case AriExpUnaryOperator(opr)         => transAriOp(aExp) + transAriExp(opr)
      case L2Id(name)                       => name
      case L2Imm(value)                     => value.toString()
      case call(name, args)            => "call " + name + "(" + args.map(_.toString()).mkString(", ") + ")"
      case _                                => throw new Exception("Unexpcted expression!!!")
    }
    if (!aExp.isInstanceOf[call]) { // parentheses are not required if the right value is call
      "(" + s + ")"
    } else {
      s
    } 
  }

  def transAriOp(aExp: AriExp): String = aExp match {
    case Add(_, _) => "+"
    case Sub(_, _) => "-"
    case Mul(_, _) => "*"
    case Div(_, _) => "/"
    case NegL2(_)  => "-"
    case OrdL2(_)  => "ord "
    case ChrL2(_)  => "chr "
    case LenL2(_)  => "len "
    case _         => throw new Exception("Unexpected operator!!!")
  }

  def transBoolExp(bExp: BoolExp): String = {
    val s = bExp match {
      case BoolExpBinaryOperator(opr1, opr2) => transBoolExp(opr1.asInstanceOf[BoolExp]) + transBoolOp(bExp) + transBoolExp(opr2.asInstanceOf[BoolExp])
      case Not(opr1) => transBoolOp(bExp) + transBoolExp(opr1)
      case L2Id(name) => name
      case L2Imm(value) => value.toString()
      case _          => throw new Exception("Unknown bool expression!!!")
    }
    "(" + s + ")"
  }

  def transBoolOp(bExp: BoolExp): String = bExp match {
    case EqL2(_, _) => "=="
    case NEqL2(opr1, opr2) => "!="
    case GTL2(_, _) => ">"
    case LTL2(_, _) => "<"
    case GEL2(_, _) => ">="
    case LEL2(_, _) => "<="
    case AndL2(_, _)=> "&&"
    case OrL2(_, _) => "||"
    case Not(_)     => "!"
    case _ => throw new Exception("Unexpected bool operator!!!")
  }
}
