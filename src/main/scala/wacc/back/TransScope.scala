package wacc.back

object BranchCounter {
  var branchCounter = -1;

  def getBranchCounter() : Int = {
    branchCounter += 1
    branchCounter
  }

  def getJustBranchCounter() : Int = {
    branchCounter
  }
}

object TransScope {
  import wacc.front.SymbolTable._
  import IRGenerator._
  import wacc.front.ast._
  import wacc.back.IR._
  import wacc.back.TransExpression.transExpr
  import BranchCounter.{getBranchCounter, getJustBranchCounter}

  def transSubProgram(sp: SubProgram, symbolTable: SymbolTable, regs: List[RId]): List[Instruction] = {
    transStats(sp.stats, regs, symbolTable) ::: restoreSP(symbolTable.getStackPos())
  }

  def transIf(ifStat: If, ifSymbolTable: SymbolTable, elseSymbolTable: SymbolTable, regs: List[RId]): List[Instruction] = {
    // allow if statements without else
    if (ifStat.exprFalse.isEmpty) {
      regs match {
        case r1 :: rest =>
          ifStat.cond match {
            case BoolLiter(true) =>
              transStats(ifStat.exprTrue, regs, ifSymbolTable) ::: restoreSP(ifSymbolTable.getStackPos())
            case BoolLiter(false) => List()
            case _ =>
              val condInstructions = transExpr(ifStat.cond, regs, ifSymbolTable.getEncSymTable())
              val trueCounter = getBranchCounter()
              val trueBlockInstructions = transStats(ifStat.exprTrue, regs, ifSymbolTable)
              condInstructions ::: List(Cmp(r1, Imm(0)), Beq(".L" + trueCounter)) :::
                trueBlockInstructions ::: restoreSP(ifSymbolTable.getStackPos()) :::
                List(Label(".L" + trueCounter))
          }
        case Nil =>
          throw new Exception("Should not arrive at situation with no register left!!!")
      }
    } else {
      regs match {
        case r1 :: rest =>
          // control-flow optimisation
          ifStat.cond match {
            case BoolLiter(true) =>
              transStats(ifStat.exprTrue, regs, ifSymbolTable) ::: restoreSP(ifSymbolTable.getStackPos())
            case BoolLiter(false) =>
              transStats(ifStat.exprFalse, regs, ifSymbolTable) ::: restoreSP(ifSymbolTable.getStackPos())
            case _ =>
              val condInstructions = transExpr(ifStat.cond, regs, ifSymbolTable.getEncSymTable())
              val trueCounter = getBranchCounter()
              val trueBlockInstructions = List(Label(".L" + trueCounter)) ::: transStats(ifStat.exprTrue, regs, ifSymbolTable)
              val falseCounter = getBranchCounter()
              val falseBlockInstructions = List(Label(".L" + falseCounter)) ::: transStats(ifStat.exprFalse, regs, elseSymbolTable)
              condInstructions :::
                List(Cmp(r1, Imm(1)), Beq(".L" + trueCounter), Branch(".L" + falseCounter)) :::
                trueBlockInstructions :::
                restoreSP(ifSymbolTable.getStackPos()) :::
                Branch(".L" + getBranchCounter()) ::
                falseBlockInstructions :::
                restoreSP(elseSymbolTable.getStackPos()) :::
                Branch(".L" + getJustBranchCounter()) ::
                List(Label(".L" + getJustBranchCounter()))
          }
        case Nil =>
          throw new Exception("Should not arrive at situation with no register left!!!")
      }
    }
  }

  def transWhile(whileStat: While, symbolTable: SymbolTable, regs: List[RId]): List[Instruction] = regs match {
    case r1 :: rest =>
      symbolTable.clearScopeCounter()
      // control-flow optimisation
      val loopInstructions = transStats(whileStat.stats, regs, symbolTable)
      val condCounter = getBranchCounter()
      whileStat.cond match {
        case BoolLiter(false) => List()
        case BoolLiter(true) =>
          Label(".L" + condCounter) :: loopInstructions ::: List(Branch(".L" + condCounter))
        case _ =>
          val condInstructions = transExpr(whileStat.cond, regs, symbolTable.getEncSymTable())
          Label(".L" + condCounter) ::
            condInstructions :::
            List(Cmp(r1, Imm(0)), Beq(".L" + getBranchCounter())) :::
            loopInstructions :::
            restoreSP(symbolTable.getStackPos()) :::
            Branch(".L" + condCounter) ::
            List(Label(".L" + getJustBranchCounter()))
      }

    case Nil => 
      throw new Exception("Should not arrive at situation with no register left!!!")
  }
}
