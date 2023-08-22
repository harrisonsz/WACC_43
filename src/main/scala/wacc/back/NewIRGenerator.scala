package wacc.back

import scala.collection.mutable.ArrayBuffer

object TempGenerator {

    import wacc.front.SymbolTable.SymbolTable
    import wacc.front.IdentifierObject._

    var counter = -1

    def getNewTemp(symbolTable: SymbolTable): String = {
      counter += 1
      symbolTable.add("@" + counter, new VarObject(null))
      "@" + counter
    }

    def getTemp(): String = {
      "@" + counter
    }
  }

object GenerateNewIR {
  import wacc.front.ast._
  import wacc.front.SymbolTable._
  import wacc.back.NewIR._

  val tempGenerator = TempGenerator

  def transProgram(prog: Program): List[Instruction] = {
    List.empty
  }

  def loadArrElem(id: Ident, exprs: List[Expr], symbolTable: SymbolTable): (Var, List[Instruction]) = id match {
    case Ident(v) =>
      var prevId = Var(v)
      val result: ArrayBuffer[Instruction] = ArrayBuffer.empty
      for (i <- 0 to exprs.size - 2) {
        val exprT = transExpr(exprs(i), symbolTable)
        result.addAll(exprT._2)
        result.addOne(IndexedCpFromL(Var(tempGenerator.getNewTemp(symbolTable)), prevId, exprT._1))
        prevId = Var(tempGenerator.getTemp())
      }
      (prevId, result.toList)
  }

  def transStat(stat: Stat, symbolTable: SymbolTable): List[Instruction] = stat match {
    case Assign(lv, rv) => 
      lv match {
        case ArrayElem(id, exprs) =>
          val loadResult = loadArrElem(id, exprs, symbolTable)
          val exprT = transExpr(exprs.last, symbolTable)
          val rvT = transRvalue(rv, symbolTable)
          loadResult._2 :::
          exprT._2 ::: 
          rvT._2 ::: 
          IndexedCpIntoL(loadResult._1, exprT._1, rvT._1) ::
          List.empty
      } 
  }

  def transRvalue(rv: Rvalue, symbolTable: SymbolTable): (Var, List[Instruction]) = rv match {
    case _ => (Var(""), List.empty)
  }

  def genericBOp(expr1: Expr, expr2: Expr, bop: BOp, symbolTable: SymbolTable): (Var, List[Instruction]) = (transExpr(expr1, symbolTable), transExpr(expr2, symbolTable)) match {
        case ((id1, l1), (id2, l2)) =>
          (id1, l1 ::: l2 ::: List(AssignBOp(id1, id1, id2, bop)))
  }

  def genericUOp(expr: Expr, uop: UOp, symbolTable: SymbolTable): (Var, List[Instruction]) = transExpr(expr, symbolTable) match {
    case (id, l) =>
      (id, l ::: List(AssignUOp(id, id, uop)))
  }

  def transExpr(expr: Expr, symbolTable: SymbolTable): (Var, List[Instruction]) = expr match {
    case Ident(v) => 
      (Var(v), List.empty)
    case IntLiter(v) => 
      val id = Var(tempGenerator.getNewTemp(symbolTable))
      (id, List(Copy(id, Imm(v))))
    case BoolLiter(true) =>
      val id = Var(tempGenerator.getNewTemp(symbolTable))
      (id, List(Copy(id, Imm(1))))
    case BoolLiter(false) =>
      val id = Var(tempGenerator.getNewTemp(symbolTable))
      (id, List(Imm(0)))
    case CharLiter(c) =>
      val id = Var(tempGenerator.getNewTemp(symbolTable))
      (id, List(Imm(c.toInt)))
    case Add(expr1, expr2) =>
      genericBOp(expr1, expr2, AddIR, symbolTable)
    case Sub(expr1, expr2) =>
      genericBOp(expr1, expr2, SubIR, symbolTable)
    case Mul(expr1, expr2) =>
      genericBOp(expr1, expr2, MulIR, symbolTable)
    case Div(expr1, expr2) =>
      genericBOp(expr1, expr2, DivIR, symbolTable)
    case Mod(expr1, expr2) =>
      genericBOp(expr1, expr2, ModIR, symbolTable)
    case Lt(expr1, expr2) =>
      genericBOp(expr1, expr2, LtIR, symbolTable)
    case Lte(expr1, expr2) =>
      genericBOp(expr1, expr2, LteIR, symbolTable)
    case Gt(expr1, expr2) =>
      genericBOp(expr1, expr2, GtIR, symbolTable)
    case Gte(expr1, expr2) =>
      genericBOp(expr1, expr2, GteIR, symbolTable)
    case Eq(expr1, expr2) =>
      genericBOp(expr1, expr2, EqIR, symbolTable)
    case Neq(expr1, expr2) =>
      genericBOp(expr1, expr2, NeqIR, symbolTable)
    case And(expr1, expr2) =>
      genericBOp(expr1, expr2, AndIR, symbolTable)
    case Or(expr1, expr2) =>
      genericBOp(expr1, expr2, OrIR, symbolTable)
    case StrLiter(v) =>
       val id = Var(tempGenerator.getNewTemp(symbolTable))
       (id, List(CpStr(id, v)))
    case ChrUOp(expr) =>
      genericUOp(expr, ChrIR, symbolTable) 
    case NegUOp(expr) =>
      genericUOp(expr, NegIR, symbolTable)
    case NotUOp(expr) =>
      genericUOp(expr, NotIR, symbolTable)
    case OrdUOp(expr) =>
      genericUOp(expr, OrdIR, symbolTable)
    case LenUOp(expr) =>
      genericUOp(expr, LenIR, symbolTable)
    case ArrayElem(id, exprs) => 
      loadArrElem(id, exprs, symbolTable)
    case PairLiter() =>
      val id = Var(tempGenerator.getNewTemp(symbolTable))
      (id, List(Copy(id, Imm(0))))
  }
}
