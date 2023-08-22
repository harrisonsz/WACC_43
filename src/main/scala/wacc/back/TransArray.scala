package wacc.back

object TransArray {
  import wacc.front.SymbolTable._
  import IRGenerator._
  import wacc.front.ast._
  import wacc.back.IR._
  import wacc.back.TransExpression.transExpr
  import wacc.back.FunctionsPool.addingFunc
  import wacc.back.PreDefinedFunctions._
  import wacc.back.RegOperator._
  import wacc.front.SemanticChecker.getLeftValueType

  // When storing a char element in an array, strb should be used, otherwise, using str
  def storeOneElementInArray(t: Type, rt: RId, rn: RId, i: Int): Instruction = t match {
    case ArrayType(CharType) =>
      addingFunc(ArrayStore)
      StoreByte(rt, ImmOffset(rn, Imm(i)))
    case _ => Store(rt, ImmOffset(rn, Imm(i * 4)))
  }

  // When storing a char array, the memory allocated should be 1 byte for each element, otherwise, 
  // 4 bytes for each element
  def allocateMemoForArray(t: Type, size: Int): Instruction = t match {
    case ArrayType(CharType) => Mov(RId(0), Imm(size + 4))
    case _ => Mov(RId(0), Imm((size + 1) * 4))
  }

  def arrElemLoadStoreHelper(arrPtr: RId, regs: List[RId], symbolTable: SymbolTable, exprs: List[Expr], exprType: Type, rv: Rvalue, mode: Int): List[Instruction] = (exprs, regs, mode) match {
    // need extra case for only 1 register left
    case (expr :: Nil, r1 :: rRest, LDR) =>
      transExpr(expr, regs, symbolTable) :::
        checkPushForLoad(arrPtr, regs) :::
        Mov(RId(3), arrPtr) ::
        Mov(RId(10), r1) :: {
        exprType match {
          case CharType =>
            addingFunc(ArrayLoadByte)
            Bl("_arrLoadB")
          case _ =>
            addingFunc(ArrayLoad)
            Bl("_arrLoad")
        }
      } ::
        Mov(arrPtr, RId(3)) ::
        checkPopForLoad(arrPtr, regs) :::
        List.empty
    case (expr :: Nil, r1 :: Nil, STR) => // if only one register left
      transExpr(expr, regs, symbolTable) :::
        Push(regs) ::
        transRvalue(exprType, rv, regs, symbolTable) :::
        Pop(List(RId(LR))) ::
        checkPushForStore(arrPtr, regs) :::
        Mov(RId(3), arrPtr) ::
        Mov(RId(10), RId(LR)) ::
        Mov(RId(8), r1) :: {
        exprType match {
          case CharType =>
            addingFunc(ArrayStoreByte)
            Bl("_arrStoreB")
          case _ =>
            addingFunc(ArrayStore)
            Bl("_arrStore")
        }
      } ::
        checkPopForStore(arrPtr, regs) :::
        List.empty
    case (expr :: Nil, r1 :: r2 :: rest, STR) =>
      transExpr(expr, regs, symbolTable) :::
        transRvalue(exprType, rv, r2 :: rest, symbolTable) :::
        checkPushForStore(arrPtr, regs) :::
        Mov(RId(3), arrPtr) ::
        Mov(RId(10), r1) ::
        Mov(RId(8), r2) :: {
        exprType match {
          case CharType =>
            addingFunc(ArrayStoreByte)
            Bl("_arrStoreB")
          case _ =>
            addingFunc(ArrayStore)
            Bl("_arrStore")
        }
      } ::
        checkPopForStore(arrPtr, regs) :::
        List.empty
    case (expr :: rest, r1 :: rRest, _) =>
      transExpr(expr, regs, symbolTable) :::
        checkPushForLoad(arrPtr, regs) :::
        Mov(RId(3), arrPtr) ::
        Mov(RId(10), r1) :: {
        exprType match {
          case CharType =>
            addingFunc(ArrayLoadByte)
            Bl("_arrLoadB")
          case _ =>
            addingFunc(ArrayLoad)
            Bl("_arrLoad")
        }
      } ::
        Mov(r1, RId(3)) ::
        checkPopForLoad(arrPtr, regs) :::
        arrElemLoadStoreHelper(r1, arrPtr :: rRest, symbolTable, rest, exprType, rv, mode) :::
        Mov(arrPtr, r1) :: // store return address in regs.head
        List.empty
    case (Nil, _, _) => throw new Exception("Expected expression")
    case (_, Nil, _) => throw new Exception("No more resgisters, SHOULD NOT HAPPEN!!!")
  }

  def stackArrElemLoadStoreHelper(regs: List[RId], symbolTable: SymbolTable, exprs: List[Expr], exprType: Type, rv: Rvalue, mode: Int): List[Instruction] = (exprs, regs, mode) match {
    case (expr :: Nil, r1 :: Nil, LDR) =>
      transExpr(expr, regs, symbolTable) :::
        Pop(List(RId(LR))) ::
        checkPushForLoad(null, regs) :::
        Mov(RId(3), RId(LR)) ::
        Mov(RId(10), r1) :: {
        exprType match {
          case CharType =>
            addingFunc(ArrayLoadByte)
            Bl("_arrLoadB")
          case _ =>
            addingFunc(ArrayLoad)
            Bl("_arrLoad")
        }
      } ::
        Mov(r1, RId(3)) ::
        checkPopForLoad(null, regs) :::
        List.empty
    case (expr :: Nil, r1 :: Nil, STR) =>
      transExpr(expr, regs, symbolTable) :::
        Push(regs) ::
        transRvalue(exprType, rv, regs, symbolTable) :::
        Push(regs) ::
        Mov(r1, RId(SP)) ::
        checkPushForStore(null, regs) :::
        Load(RId(3), ImmOffset(r1, Imm(8))) ::
        Load(RId(10), ImmOffset(r1, Imm(4))) ::
        Load(RId(8), ImmOffset(r1, Imm(0))) :: {
        exprType match {
          case CharType =>
            addingFunc(ArrayStoreByte)
            Bl("_arrStoreB")
          case _ =>
            addingFunc(ArrayStore)
            Bl("_arrStore")
        }
      } ::
        checkPopForStore(null, regs) :::
        AddIns(RId(SP), RId(SP), Imm(12)) ::
        List.empty
    case (expr :: rest, r1 :: Nil, _) =>
      transExpr(expr, regs, symbolTable) :::
        Pop(List(RId(LR))) ::
        checkPushForLoad(null, regs) :::
        Mov(RId(3), RId(LR)) ::
        Mov(RId(10), r1) :: {
        exprType match {
          case CharType =>
            addingFunc(ArrayLoadByte)
            Bl("_arrLoadB")
          case _ =>
            addingFunc(ArrayLoad)
            Bl("_arrLoad")
        }
      } ::
        Mov(r1, RId(3)) ::
        checkPopForLoad(null, regs) :::
        Push(regs) ::
        stackArrElemLoadStoreHelper(regs, symbolTable, rest, exprType, rv, mode)
    case (_, r1 :: r2 :: rest, _) =>
      throw new Exception("Should not run in stack machine mode when having more than one register left")
    case (Nil, _, _) =>
      throw new Exception("Illegal arrElem")
    case (_, Nil, _) =>
      throw new Exception("Shouldn't arrvie at a situation with no register left!!!")
  }

  def transArrElemStore(arrElem: ArrayElem, regs: List[RId], symbolTable: SymbolTable, rv: Rvalue): List[Instruction] = (arrElem, regs) match {
    // need extra case for only 1 register left
    case (ArrayElem(id, exprs), r1 :: Nil) =>
      loadValueOfVariable(r1, id.v, symbolTable) ::
        Push(regs) ::
        stackArrElemLoadStoreHelper(regs, symbolTable, exprs, getLeftValueType(arrElem, symbolTable).get, rv, STR)
    case (ArrayElem(id, exprs), r1 :: r2 :: rest) =>
      loadValueOfVariable(r1, id.v, symbolTable) ::
        arrElemLoadStoreHelper(r1, r2 :: rest, symbolTable, exprs, getLeftValueType(arrElem, symbolTable).get, rv, STR)
    case (_, Nil) => throw new Exception("No more resgisters, SHOULD NOT HAPPEN!!!")
  }
}
