package wacc.back

object TransExpression {
  import wacc.front.SymbolTable._
  import IRGenerator._
  import wacc.front.ast._
  import wacc.back.IR._
  import wacc.back.PreDefinedFunctions._
  import wacc.back.FunctionsPool.addingFunc
  import wacc.back.StringCounter.getStringCounter
  import wacc.front.TypeChecker.getExpType
  import wacc.back.RegOperator._
  import wacc.back.TransArray._
  import scala.math.abs

   // The final result of expr will always be mov into the regs.head
  def transExpr(expr: Expr, regs: List[RId], symbolTable: SymbolTable): List[Instruction] = (expr, regs) match {
    case (Ident(v), r1 :: rest) =>
      loadValueOfVariable(r1, v, symbolTable) ::
        List.empty
    case (IntLiter(v), r1 :: rest) => {
      if (checkImm(v)) {
        Mov(r1, Imm(v))
      } else {
        Load(r1, Constant(v.toString()))
      }
    } ::
      List.empty
    case (BoolLiter(true), r1 :: rest) => Mov(r1, Imm(1)) :: List.empty
    case (BoolLiter(false), r1 :: rest) => Mov(r1, Imm(0)) :: List.empty
    case (CharLiter(c), r1 :: rest) => Mov(r1, Imm(c.toInt)) :: List.empty
    case (PairLiter(), r1 :: rest) => Mov(r1, Imm(0)) :: List.empty
    case (Add(expr1, expr2), r1 :: rest) =>
      addingFunc(ErrorOverFlow)
      genericBinaryOp(AddIns, expr1, expr2, regs, symbolTable) :::
        Blvs("_errOverflow") ::
        List.empty
    case (Sub(expr1, expr2), r1 :: rest) =>
      addingFunc(PreDefinedFunctions.ErrorOverFlow)
      genericBinaryOp(SubIns, expr1, expr2, regs, symbolTable) :::
        Blvs("_errOverflow") ::
        List.empty
    case (Mul(expr1, expr2), r1 :: rest) =>
      genericBinaryOp(MulIns, expr1, expr2, regs, symbolTable)
    case (Div(expr1, expr2), r1 :: rest) =>
      divMod(expr1, expr2, regs, symbolTable, DIV)
    case (Mod(expr1, expr2), r1 :: rest) =>
      divMod(expr1, expr2, regs, symbolTable, MOD)
    case (Lt(expr1, expr2), r1 :: rest) =>
      genericBinaryOp(LTIns, expr1, expr2, regs, symbolTable)
    case (Lte(expr1, expr2), r1 :: rest) =>
      genericBinaryOp(LEIns, expr1, expr2, regs, symbolTable)
    case (Gt(expr1, expr2), r1 :: rest) =>
      genericBinaryOp(GTIns, expr1, expr2, regs, symbolTable)
    case (Gte(expr1, expr2), r1 :: rest) =>
      genericBinaryOp(GEIns, expr1, expr2, regs, symbolTable)
    case (Eq(expr1, expr2), r1 :: rest) =>
      genericBinaryOp(EqIns, expr1, expr2, regs, symbolTable)
    case (Neq(expr1, expr2), r1 :: rest) =>
      genericBinaryOp(NeqIns, expr1, expr2, regs, symbolTable)
    case (And(expr1, expr2), r1 :: rest) =>
      genericBinaryOp(AndIns, expr1, expr2, regs, symbolTable)
    case (Or(expr1, expr2), r1 :: rest) =>
      genericBinaryOp(OrIns, expr1, expr2, regs, symbolTable)
    case (StrLiter(v), r1 :: rest) =>
      stringPool.get(v) match {
        case None =>
          val stringCounter = getStringCounter()
          stringPool.addOne((v, stringCounter))
          dataList.appendAll(DotWord(v.length()) :: Label(".L.str" + stringCounter) ::
            DotAsciz(addingSlashBeforeQuotation(v)) :: List.empty)
          List(Load(r1, Constant(".L.str" + stringCounter)))
        case Some(value) =>
          val stringCounter = value
          List(Load(r1, Constant(".L.str" + stringCounter)))
      }
    case (NegUOp(expr), r1 :: rest) =>
      addingFunc(ErrorOverFlow)
      transExpr(expr, regs, symbolTable) :::
        Rsbs(r1, r1, Imm(0)) ::
        Blvs("_errOverflow") ::
        List.empty
    case (NotUOp(expr), r1 :: rest) =>
      transExpr(expr, regs, symbolTable) :::
        Cmp(r1, Imm(1)) ::
        MovEq(r1, Imm(0)) ::
        MovNeq(r1, Imm(1)) ::
        List.empty
    case (OrdUOp(expr), r1 :: rest) =>
      transExpr(expr, regs, symbolTable)
    case (ChrUOp(expr), r1 :: rest) =>
      transExpr(expr, regs, symbolTable)
    case (LenUOp(expr), r1 :: rest) =>
      transExpr(expr, regs, symbolTable) :::
        Load(r1, ImmOffset(r1, Imm(-4))) ::
        List.empty
    case (arrElem@ArrayElem(id, exprs), regs) =>
      regs match {
        case r1 :: Nil =>
          val exprType = getExpType(arrElem, symbolTable).get
          loadValueOfVariable(r1, id.v, symbolTable) ::
            Push(regs) ::
            stackArrElemLoadStoreHelper(regs, symbolTable, exprs, exprType, null, LDR)
        case r1 :: r2 :: rest =>
          val exprType = getExpType(arrElem, symbolTable).get
          loadValueOfVariable(r1, id.v, symbolTable) ::
            arrElemLoadStoreHelper(r1, r2 :: rest, symbolTable, exprs, exprType, null, LDR)
        case Nil =>
          throw new Exception("Should not arrive at situation with no register left!!!")
      }
    case (_, Nil) =>
      throw new Exception("Should not arrive at situation with no register left!!!")
  }

  def constantFolding(bOp: (RId, RId, RId) => Instruction, expr1: Expr, expr2: Expr, regs: List[RId], symbolTable: SymbolTable): List[Instruction] = (expr1, expr2) match {
    case (IntLiter(ex1), IntLiter(ex2)) =>
      bOp match {
        case AddIns => transExpr(IntLiter(ex1 + ex2) (1, 1), regs, symbolTable)
        case SubIns => transExpr(IntLiter(ex1 - ex2) (1, 1), regs, symbolTable)
        case MulIns => transExpr(IntLiter(ex1 * ex2) (1, 1), regs, symbolTable)
        case LTIns => transExpr(BoolLiter(ex1 < ex2) (1, 1), regs, symbolTable)
        case LEIns => transExpr(BoolLiter(ex1 <= ex2) (1, 1), regs, symbolTable)
        case GTIns => transExpr(BoolLiter(ex1 > ex2) (1, 1), regs, symbolTable)
        case GEIns => transExpr(BoolLiter(ex1 >= ex2) (1, 1), regs, symbolTable)
        case EqIns => transExpr(BoolLiter(ex1 == ex2) (1, 1), regs, symbolTable)
        case NeqIns => transExpr(BoolLiter(ex1 != ex2) (1, 1), regs, symbolTable)
        case _ => throw new Exception("should not have other bOps")
      }
    case (CharLiter(ex1), CharLiter(ex2)) =>
      bOp match {
        case LTIns => transExpr(BoolLiter(ex1 < ex2) (1, 1), regs, symbolTable)
        case LEIns => transExpr(BoolLiter(ex1 <= ex2) (1, 1), regs, symbolTable)
        case GTIns => transExpr(BoolLiter(ex1 > ex2) (1, 1), regs, symbolTable)
        case GEIns => transExpr(BoolLiter(ex1 >= ex2) (1, 1), regs, symbolTable)
        case EqIns => transExpr(BoolLiter(ex1 == ex2) (1, 1), regs, symbolTable)
        case NeqIns => transExpr(BoolLiter(ex1 != ex2) (1, 1), regs, symbolTable)
        case _ => throw new Exception("should not have other bOps")
      }
    case (BoolLiter(ex1), BoolLiter(ex2)) =>
      bOp match {
        case EqIns => transExpr(BoolLiter(ex1 == ex2) (1, 1), regs, symbolTable)
        case NeqIns => transExpr(BoolLiter(ex1 != ex2) (1, 1), regs, symbolTable)
        case AndIns => transExpr(BoolLiter(ex1 && ex2) (1, 1), regs, symbolTable)
        case OrIns => transExpr(BoolLiter(ex1 || ex2) (1, 1), regs, symbolTable)
        case _ => throw new Exception("should not have other bOps")
      }
    case (StrLiter(ex1), StrLiter(ex2)) =>
      bOp match {
        case EqIns => transExpr(BoolLiter(ex1 == ex2) (1, 1), regs, symbolTable)
        case NeqIns => transExpr(BoolLiter(ex1 != ex2) (1, 1), regs, symbolTable)
        case _ => throw new Exception("should not have other bOps")
      }
    case _ => List.empty
  }

  def regularGenericBinaryOp(bOp: (RId, RId, RId) => Instruction, expr1: Expr, expr2: Expr, regs: List[RId], symbolTable: SymbolTable): List[Instruction] = regs match {
    case r1 :: Nil =>
      transExpr(expr2, regs, symbolTable) :::
        Push(regs) ::
        transExpr(expr1, regs, symbolTable) :::
        Pop(List(RId(LR))) ::
        bOp(r1, r1, RId(LR)) :: {
        bOp match {
          case MulIns =>
            addingFunc(ErrorOverFlow)
            Cmp(RId(LR), ASR(r1, Imm(31))) ::
              Blne("_errOverflow") ::
              List.empty
          case _ => List.empty
        }
      }
    case r1 :: r2 :: rest =>
      transExpr(expr1, regs, symbolTable) :::
        transExpr(expr2, r2 :: rest, symbolTable) :::
        bOp(r1, r1, r2) :: {
        bOp match {
          case MulIns =>
            addingFunc(ErrorOverFlow)
            Cmp(r2, ASR(r1, Imm(31))) ::
              Blne("_errOverflow") ::
              List.empty
          case _ => List.empty
        }
      }
    case Nil =>
      throw new Exception("Should not arrive at situation with no register left!!!")
  }

  def genericBinaryOp(bOp: (RId, RId, RId) => Instruction, expr1: Expr, expr2: Expr, regs: List[RId], symbolTable: SymbolTable): List[Instruction] = regs match {
    case r1 :: Nil =>
      if (OptimiseFold) {
        val optimisedInstructions = constantFolding(bOp, expr1, expr2, regs, symbolTable)
        if (optimisedInstructions.isEmpty) {
          regularGenericBinaryOp(bOp, expr1, expr2, regs, symbolTable)
        } else {
          optimisedInstructions
        }
      } else {
        regularGenericBinaryOp(bOp, expr1, expr2, regs, symbolTable)
      }
    case r1 :: r2 :: rest =>
      if (OptimiseFold) {
        val optimisedInstructions = constantFolding(bOp, expr1, expr2, regs, symbolTable)
        if (optimisedInstructions.isEmpty) {
          regularGenericBinaryOp(bOp, expr1, expr2, regs, symbolTable)
        } else {
          optimisedInstructions
        }
      } else {
        regularGenericBinaryOp(bOp, expr1, expr2, regs, symbolTable)
      }
    case Nil =>
      throw new Exception("Should not arrive at situation with no register left!!!")
  }

  def divMod(expr1: Expr, expr2: Expr, regs: List[RId], symbolTable: SymbolTable, mode: Int): List[Instruction] = regs match {
    case r1 :: rest =>
      assert(mode == DIV || mode == MOD)
      addingFunc(ErrorDivZero)
      allCallerSave() :: // save R0-R3
        transExpr(expr1, RId(0) :: regs, symbolTable) :::
        transExpr(expr2, RId(1) :: regs, symbolTable) :::
        Cmp(RId(1), Imm(0)) ::
        Bleq("_errDivZero") ::
        Bl("__aeabi_idivmod") ::
        Mov(r1, RId(mode)) ::
        allCallerRestore() ::
        List.empty
    case Nil =>
      throw new Exception("Should not arrive at situation with no register left!!!")
  }

  def checkImm(num: Int): Boolean = {
    if (num >= -256 && num <= 255) return true
    var n = abs(num)
    while ((n & 1) == 0) {
      n = n >> 1
    }
    return n <= 255
  }
}
