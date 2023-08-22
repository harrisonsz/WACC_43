package wacc.back

object TransIO {
  import wacc.back.IR._
  import wacc.front.SymbolTable._
  import wacc.front.ast._
  import wacc.back.IRGenerator._
  import wacc.back.RegOperator._
  import wacc.back.FunctionsPool.addingFunc
  import wacc.back.PreDefinedFunctions._
  import wacc.front.TypeChecker.getExpType
  
    // The value in regs.head should be stored in the stack position of lv
  def transStoreReadValue(lv: Lvalue, regs: List[RId], symbolTable: SymbolTable): List[Instruction] = (lv, regs) match {
    case (Fst(lv), r1 :: rest) =>
      Push(List(r1)) ::
        transLvalue(lv, regs, symbolTable) :::
        Pop(List(RId(LR))) ::
        Store(r1, ImmOffset(RId(LR), Imm(0))) ::
        List.empty
    case (Snd(lv), r1 :: rest) =>
      Push(List(r1)) ::
        transLvalue(lv, regs, symbolTable) :::
        Pop(List(RId(LR))) ::
        Store(r1, ImmOffset(RId(LR), Imm(4))) ::
        List.empty
    case (_, r1 :: regs) =>
      val id = getIdFromLv(lv)
      storeValueOfVariable(r1, id.v, symbolTable) ::
        List.empty
    case (_, Nil) =>
      throw new Exception("Shouldn't arrvie at a situation with no register left!!!")
  }

  def getPrintType(expr: Expr, symbolTable: SymbolTable): Instruction = {
    getExpType(expr, symbolTable) match {
      case Some(IntType) =>
        addingFunc(PrintInt)
        Bl("_printi")
      case Some(CharType) =>
        addingFunc(PrintChar)
        Bl("_printc")
      case Some(BoolType) =>
        addingFunc(PrintBool)
        Bl("_printb")
      case Some(StringType) =>
        addingFunc(PrintString)
        Bl("_prints")
      case Some(ArrayType(CharType)) =>
        addingFunc(PrintString)
        Bl("_prints")
      case Some(_) =>
        addingFunc(PrintPair)
        Bl("_printp")
      case None => throw new Exception("getExpType() here should always return Some()")
    }
  }
}
