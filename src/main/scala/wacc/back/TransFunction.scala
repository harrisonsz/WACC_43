package wacc.back

object TransFunction {
  import wacc.front.SymbolTable._
  import wacc.front.ast._
  import wacc.front.IdentifierObject._
  import wacc.back.IRGenerator._
  import wacc.back.IR._
  import wacc.back.RegOperator._

   // n is the number of push before body codes after pushings of arguments
  def addingStakcPosForEveryArg(funcObj: FuncObject, n: Int): Unit = {
    funcObj.paraList.zip(funcObj.paraList.indices.toList).foreach {
      case (p, i) =>
        // Setting the checkedIR as true means it can be found by lookupAll in IRGenerator
        // Parameters are wrapped with []
        funcObj.symbolTable.lookup("[" + p.id.v + "]").get.asInstanceOf[VarObject].checkedIR = true
        funcObj.symbolTable.lookup("[" + p.id.v + "]").get.asInstanceOf[VarObject].stackPos = Some(-((n - 1) + (funcObj.paraList.size - i)))
    }
  }

  // It reset all variables recursively
  def resetAllVarInSymbolTable(symbolTable: SymbolTable): Unit = {
    symbolTable.dict.values.foreach{x => x match {
      case v: VarObject => v.checkedIR = false; v.regPos = None; v.stackPos = None
      case ifObj: IfObject => resetAllVarInSymbolTable(ifObj.ifSymbolTable); resetAllVarInSymbolTable(ifObj.elseSymbolTable)
      case whileObj: WhileObject => resetAllVarInSymbolTable(whileObj.symbolTable)
      case subObj: SubProgObject => resetAllVarInSymbolTable(subObj.symbolTable)
      case _: ProgObject => throw new Exception("There should not be ProgObj in FuncObj")
      case _: FuncObject => throw new Exception("This function should only be called in transFunc() and there should not be FuncObj in FuncObj")
    }}
  }

  def transFunc(func: Func, funcObj: FuncObject): List[Instruction] = {
    if (OptimiseERA) {
      // If we want to know which registers needed to be pushed at the start, we need to get the body codes. 
      // If we want to get the body codes, the number of registers pushed at the start should be known because positions of arguments
      // is calculated with the number of registers being pushed at the start.
      // So we need to give fake positions to arguments and generating a fake body codes to count the number of registers will be pushed
      // at the start of the body
      addingStakcPosForEveryArg(funcObj, 42) // 42 is just a filler, it is filling rubbish positions for arguments
      funcObj.symbolTable.getEncSymTable().cleanStackCounter()
      val fakeBodyCodes = transStats(func.stats, GeneralRegs, funcObj.symbolTable)
      clearAllScopeCounters(funcObj.symbolTable.getEncSymTable())
      resetAllVarInSymbolTable(funcObj.symbolTable)
      funcUsedCalleeSavedRegs = getUsedCalleeSavedRegs(fakeBodyCodes)
      addingStakcPosForEveryArg(funcObj, funcUsedCalleeSavedRegs.size + 2) // 2 refers to fp and lr
    } else {
      addingStakcPosForEveryArg(funcObj, 9) // r4, r5, r6, r7, r8, r10, r12, fp and lr are pushed after arguments are pushed
    }
    // Cleaning the stack counter of the main program before transforming the function 
    // because which will affect the allocation of stack position while transforming the
    // function
    funcObj.symbolTable.getEncSymTable().cleanStackCounter()
    val funcAssemblyCodes = transStats(func.stats, GeneralRegs, funcObj.symbolTable)
    Label("wacc_" + func.id.v) ::
      Push(List(RId(FP), RId(LR))) ::
      (if (OptimiseERA) {
        val regs = getUsedCalleeSavedRegs(funcAssemblyCodes)
        if (!regs.isEmpty) {
          List(Push(regs))
        } else {
          List.empty
        }
      } else {
        List(allCalleeSave())
      }) :::
      Mov(RId(FP), RId(SP)) :: // Updating fp by the sp
      funcAssemblyCodes :::
      restoreSP(funcObj.symbolTable.getStackPos()) ::: // Restoring the SP
      Mov(RId(SP), RId(FP)) :: // Resetting the stack pointer
      Pop(List(RId(FP), RId(PC))) ::
      Customize(".ltorg") ::
      List.empty
  }
}
