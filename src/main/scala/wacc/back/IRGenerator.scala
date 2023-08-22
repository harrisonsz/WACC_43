package wacc.back

import wacc.front.ast._
import wacc.front.SemanticChecker._
import wacc.front.IdentifierObject._
import wacc.back.IR._

import scala.collection.immutable.{::, List, Nil}

object InstructionUsingReg{
  def unapply(i: Instruction): Option[RId] = i match {
    case Mov(rd, _) => Some(rd)
    case MovLT(rd, _) => Some(rd)
    case MovLE(rd, _) => Some(rd)
    case MovGT(rd, _) => Some(rd)
    case MovGE(rd, _) => Some(rd)
    case MovEq(rd, _) => Some(rd)
    case MovNeq(rd, _) => Some(rd)
    case Load(rd, _)  => Some(rd)
    case LoadByte(rd, _) => Some(rd)
    case LoadSignedByte(rd, _) => Some(rd)
    case _          => None
  }
}

object StringCounter {
  var stringCounter = -1;

  def getStringCounter() : Int = {
    stringCounter += 1
    stringCounter
  }
}

object IRGenerator {

  import wacc.back.TransScope._
  import FunctionsPool.{addingDefOfFuncs, addingFunc}
  import IR._
  import wacc.front.SymbolTable._
  import PreDefinedFunctions._
  import wacc.front.TypeChecker._
  import RegOperator._
  import TransExpression.transExpr
  import wacc.back.TransArray._
  import wacc.back.TransFunction.transFunc
  import wacc.back.TransIO._

  import scala.collection.mutable.{HashMap, ListBuffer}


  val dataList: ListBuffer[Instruction] = ListBuffer(DotData)

  val stringPool: HashMap[String, Int] = HashMap.empty

  // The flag of Efficient Register Allocation, there will be optimisation in assembly code if 
  // it is true.
  var OptimiseERA = false 
  var OptimiseFold = false

  // Another global mutable variable, it is a list recording the general registers used in every
  // function, used when meeting the return statements, it should be updated at the start of every
  // transFunc() 
  var funcUsedCalleeSavedRegs: List[RId] = List.empty

  def generateIR(program: Program, progObject: ProgObject): List[Instruction] = {
    clearAllScopeCounters(progObject.symbolTable)
    val mainAssemblyCodes = transStats(program.stats, GeneralRegs, progObject.symbolTable)
    DotText :: DotGlobal("main") :: Label("main") ::
      Push(List(RId(FP), RId(LR))) ::
      (if (OptimiseERA) {
        val regs = getUsedCalleeSavedRegs(mainAssemblyCodes)
        if (!regs.isEmpty) {
          List(Push(regs))
        } else {
          List.empty
        }
      } else {
        List(allCalleeSave())
      }) :::
      Mov(RId(FP), RId(SP)) :: // Updating fp by the sp
      mainAssemblyCodes :::
      Mov(RId(0), Imm(0)) ::
      restoreSP(progObject.symbolTable.getStackPos()) :::
      (if (OptimiseERA) { // Restoring the registers
        val regs = getUsedCalleeSavedRegs(mainAssemblyCodes)
        if (!regs.isEmpty) {
          List(Pop(regs))
        } else {
          List.empty
        }
      } else {
        List(allCalleeRestore())
      }) :::
      Pop(List(RId(FP), RId(PC))) ::
      program.funcs.flatMap((x: Func) => transFunc(x, progObject.symbolTable.lookup("(" + x.id.v + ")").get.asInstanceOf[FuncObject])) :::
      addingDefOfFuncs() :::
      dataList.toList
  }

  def transStats(stats: List[Stat], regs: List[RId], symbolTable: SymbolTable): List[Instruction] = stats match {
    case Nil => List.empty
    case head :: next => (head, regs) match {
      case (Exit(expr), rd :: rest) =>
        transExpr(expr, regs, symbolTable) :::
          Mov(RId(0), rd) ::
          Bl("exit") ::
          List.empty

      case (Skip, _) =>
        transStats(next, regs, symbolTable)

      case (Init(t, ident, rv), rd :: Nil) =>
        // When a base type variable is initialized, the value rv will be pushed into the stack and the relative stack
        // position will be recorded in the symbol table
        symbolTable.lookupAll(ident.v).get.asInstanceOf[VarObject].stackPos = Some(symbolTable.getAndIncStackPos()) // Modify the stack position of a variable
        symbolTable.lookupAll(ident.v).get.asInstanceOf[VarObject].checkedIR = true
        transRvalue(t, rv, regs, symbolTable) :::
          Push(List(rd)) ::
          transStats(next, regs, symbolTable)

      case (Init(t, ident, rv), rd :: rest) =>
        if (OptimiseERA) {
          // If there are registers available, we store the rv into a register
          symbolTable.lookupAll(ident.v).get.asInstanceOf[VarObject].regPos = Some(rd)
          symbolTable.lookupAll(ident.v).get.asInstanceOf[VarObject].checkedIR = true
          transRvalue(t, rv, rest, symbolTable) :::
            Mov(rd, rest.head) ::
            transStats(next, rest, symbolTable)
        } else {
          symbolTable.lookupAll(ident.v).get.asInstanceOf[VarObject].stackPos = Some(symbolTable.getAndIncStackPos()) // Modify the stack position of a variable
          symbolTable.lookupAll(ident.v).get.asInstanceOf[VarObject].checkedIR = true
          transRvalue(t, rv, regs, symbolTable) :::
            Push(List(rd)) ::
            transStats(next, regs, symbolTable)
        }

      case (Assign(lv, rv), rd :: rest) =>
        // Assume only assigning variables before more implementations
        lv match {
          case arrElem@ArrayElem(id, exprs) =>
            transArrElemStore(arrElem, regs, symbolTable, rv) :::
              transStats(next, regs, symbolTable)
          case Ident(v) =>
            transRvalue(getLeftValueType(lv, symbolTable).get, rv, regs, symbolTable) :::
              storeValueOfVariable(rd, v, symbolTable) ::
              transStats(next, regs, symbolTable)

          case p@(Fst(_) | Snd(_)) => regs match {
            case r1 :: rest =>
              val v = p match {
                case Fst(v) => v
                case Snd(v) => v
                case _ => throw new Exception("Should not arrive here")
              }
              val pos = p match {
                case Fst(v) => 0
                case Snd(v) => 4
                case _ => throw new Exception("Should not arrive here")
              }
              addingFunc(ErrorNull)
              transLvalue(v, regs, symbolTable) :::
                Cmp(r1, Imm(0)) ::
                Bleq("_errNull") ::
                Push(List(r1)) ::
                transRvalue(IntType, rv, regs, symbolTable) :::
                Pop(List(RId(LR))) ::
                Load(RId(LR), ImmOffset(RId(LR), Imm(pos))) ::
                Store(r1, ImmOffset(RId(LR), Imm(0))) ::
                Mov(r1, RId(LR)) ::
                transStats(next, regs, symbolTable)
            case Nil =>
              throw new Exception("Shouldn't arrvie at a situation with no register left!!!")
          }
        }

      case (Print(expr), rd :: rest) =>
        transExpr(expr, regs, symbolTable) :::
          Mov(RId(0), rd) ::
          getPrintType(expr, symbolTable) ::
          transStats(next, regs, symbolTable)

      case (Println(expr), rd :: rest) =>
        addingFunc(PrintNewLine)
        transExpr(expr, regs, symbolTable) :::
          Mov(RId(0), rd) ::
          getPrintType(expr, symbolTable) ::
          Bl("_println") ::
          transStats(next, regs, symbolTable)

      case (Free(expr), rd :: rest) => getExpType(expr, symbolTable) match {
        case Some(PairType(_, _)) =>
          addingFunc(FreePair)
          transExpr(expr, regs, symbolTable) :::
            callerRegsSave(regs) :::
            Mov(RId(0), rd) ::
            Bl("_freepair") ::
            callerRegsRestore(regs) :::
            transStats(next, regs, symbolTable)
        case Some(Null) =>
          addingFunc(FreePair)
          transExpr(expr, regs, symbolTable) :::
            callerRegsSave(regs) :::
            Mov(RId(0), rd) ::
            Bl("_freepair") ::
            callerRegsRestore(regs) :::
            transStats(next, regs, symbolTable)
        case _ =>
          transExpr(expr, regs, symbolTable) :::
            SubIns(rd, rd, Imm(4)) ::
            callerRegsSave(regs) :::
            Mov(RId(0), rd) ::
            Bl("free") ::
            callerRegsRestore(regs) :::
            transStats(next, regs, symbolTable)
      }

      case (Return(expr), rd :: rest) =>
        transExpr(expr, regs, symbolTable) :::
          Mov(RId(0), rd) ::
          restoreSP(symbolTable.getAllStackPos()) ::: // Restoring the SP
          Mov(RId(SP), RId(FP)) :: // Resetting the stack pointer
          (if (OptimiseERA) {
            Pop(funcUsedCalleeSavedRegs)
          } else {
            allCalleeRestore()
          }) ::
          Pop(List(RId(FP), RId(PC))) ::
          List.empty

      case (ifStat: If, _) =>
        val ifObj = symbolTable.lookup("if" + "{" + symbolTable.getNum() + "}").get.asInstanceOf[IfObject]
        transIf(ifStat, ifObj.ifSymbolTable, ifObj.elseSymbolTable, regs) :::
          transStats(next, regs, symbolTable)

      case (whileStat: While, _) =>
        val whileObj = symbolTable.lookup("while" + "{" + symbolTable.getNum() + "}").get.asInstanceOf[WhileObject]
        transWhile(whileStat, whileObj.symbolTable, regs) :::
          transStats(next, regs, symbolTable)

      case (sp: SubProgram, _) =>
        val spObj = symbolTable.lookup("sub" + "{" + symbolTable.getNum() + "}").get.asInstanceOf[SubProgObject]
        transSubProgram(sp, spObj.symbolTable, regs) :::
          transStats(next, regs, symbolTable)

      case (Read(lv), rd :: rest) =>
        val readIns = getLeftValueType(lv, symbolTable) match {
          case Some(IntType) =>
            addingFunc(ReadInt)
            Bl("_readi")
          case Some(CharType) =>
            addingFunc(ReadChar)
            Bl("_readc")
          case Some(_) =>
            throw new Exception("Only Int and Char type variables can be read(There is not expected to reach)")
          case None => throw new Exception("There is not expected to reach")
        }
        transLvalue(lv, regs, symbolTable) :::
          callerRegsSave(regs) :::
          Mov(RId(0), rd) ::
          readIns ::
          Mov(rd, RId(0)) ::
          callerRegsRestore(regs) :::
          transStoreReadValue(lv, regs, symbolTable) :::
          transStats(next, regs, symbolTable)

      case (_, Nil) => throw new Exception("No more resgisters, SHOULD NOT HAPPEN!!!")
    }
  }

  // It reset all scope counters in all symbol tables
  // It should only be called at the start of generateIR()
  def clearAllScopeCounters(symbolTable: SymbolTable): Unit = {
    symbolTable.clearScopeCounter()
    symbolTable.dict.toList.map(_._2).foreach {
      case funcObj: FuncObject => clearAllScopeCounters(funcObj.symbolTable)
      case subObj: SubProgObject => clearAllScopeCounters(subObj.symbolTable)
      case ifObj: IfObject => clearAllScopeCounters(ifObj.ifSymbolTable); clearAllScopeCounters(ifObj.elseSymbolTable)
      case whileObj: WhileObject => clearAllScopeCounters(whileObj.symbolTable)
      case _ =>
    }
  }

  // The value in rs will be the value of v, whether the value of v is stored in a register or stack
  def storeValueOfVariable(rs: RId, v: String, symbolTable: SymbolTable): Instruction = {
    val stackPos = symbolTable.lookupAllInIRCheck(v).get.asInstanceOf[VarObject].stackPos
    val regPos = symbolTable.lookupAllInIRCheck(v).get.asInstanceOf[VarObject].regPos

    if (OptimiseERA) {
      (stackPos, regPos) match {
        case (Some(pos), None) =>
          Store(rs, ImmOffset(RId(FP), Imm(pos * -4)))
        case (None, Some(reg)) =>
          Mov(reg, rs)
        case (Some(_), Some(_)) => throw new Exception("Variable should not have both regPos and stackPos")
        case (None, None) => throw new Exception("Variable should have been stored somewhere!!!")
      }
    } else {
      Store(rs, ImmOffset(RId(FP), Imm(stackPos.get * -4)))
    }
  }

  // The value of v will be in rd, whether the value of v is stored in a register or stack
  def loadValueOfVariable(rd: RId, v: String, symbolTable: SymbolTable): Instruction = {
    val stackPos = symbolTable.lookupAllInIRCheck(v).get.asInstanceOf[VarObject].stackPos
    val regPos = symbolTable.lookupAllInIRCheck(v).get.asInstanceOf[VarObject].regPos

    if (OptimiseERA) {
      (stackPos, regPos) match {
        case (Some(pos), None) =>
          Load(rd, ImmOffset(RId(FP), Imm(pos * -4)))
        case (None, Some(reg)) =>
          Mov(rd, reg)
        case (Some(_), Some(_)) => throw new Exception("Variable should not have both regPos and stackPos")
        case (None, None) => throw new Exception("Variable should have been stored somewhere!!!")
      }
    } else {
      Load(rd, ImmOffset(RId(FP), Imm(stackPos.get * -4)))
    }
  }

  // The value of Lvalue should be in the regs.head
  def transLvalue(lv: Lvalue, regs: List[RId], symbolTable: SymbolTable): List[Instruction] = (lv, regs) match {
    case (Fst(v), r1 :: rest) =>
      addingFunc(ErrorNull)
      transLvalue(v, regs, symbolTable) :::
        Cmp(r1, Imm(0)) ::
        Bleq("_errNull") ::
        Load(r1, ImmOffset(r1, Imm(0))) ::
        Load(r1, ImmOffset(r1, Imm(0))) ::
        List.empty
    case (Snd(v), r1 :: rest) =>
      addingFunc(ErrorNull)
      transLvalue(v, regs, symbolTable) :::
        Cmp(r1, Imm(0)) ::
        Bleq("_errNull") ::
        Load(r1, ImmOffset(r1, Imm(4))) ::
        Load(r1, ImmOffset(r1, Imm(0))) ::
        List.empty
    case (_, r1 :: rest) =>
      val id = getIdFromLv(lv)
      loadValueOfVariable(r1, id.v, symbolTable) ::
        List.empty
    case (_, Nil) =>
      throw new Exception("Shouldn't arrvie at a situation with no register left!!!")
  }

  // Malloc and push the stored value address onto stack
  def allocateValForPair(rd: RId, r1: RId, expr: Expr, rest: List[RId], symbolTable: SymbolTable): List[Instruction] = {
    callerRegsSave(rest) :::
      Mov(RId(0), Imm(4)) ::
      Bl("malloc") ::
      Mov(rd, RId(0)) ::
      callerRegsRestore(rest) :::
      transExpr(expr, r1 :: rest, symbolTable) :::
      Store(r1, ImmOffset(rd, Imm(0))) ::
      Push(List(rd)) ::
      List.empty
  }

  // The final value is expected to be in the regs.head
  def transRvalue(t: Type, rv: Rvalue, regs: List[RId], symbolTable: SymbolTable): List[Instruction] = (rv, regs) match {
    case (ArrayLiter(exprs), r1 :: Nil) =>
      addingFunc(ArrayStore)
      addingFunc(ArrayStoreByte)
      // This procedure will need two registers
      callerRegsSave(regs) :::
        allocateMemoForArray(t, exprs.size) ::
        Bl("malloc") ::
        Mov(RId(LR), RId(0)) ::
        callerRegsRestore(regs) :::
        AddIns(RId(LR), RId(LR), Imm(4)) ::
        Mov(r1, Imm(exprs.size)) ::
        Store(r1, ImmOffset(RId(LR), Imm(-4))) ::
        exprs.zipWithIndex.flatMap(exprIndexPair => {
          Push(List(RId(LR))) ::
            transExpr(exprIndexPair._1, regs, symbolTable) :::
            Pop(List(RId(LR))) ::
            storeOneElementInArray(t, r1, RId(LR), exprIndexPair._2) ::
            List.empty
        }) :::
        Mov(r1, RId(LR)) ::
        List.empty
    case (ArrayLiter(exprs), rd :: r1 :: rest) =>
      addingFunc(ArrayStore)
      addingFunc(ArrayStoreByte)
      // This procedure will need two registers
      callerRegsSave(regs) :::
        allocateMemoForArray(t, exprs.size) ::
        Bl("malloc") ::
        Mov(r1, RId(0)) ::
        callerRegsRestore(regs) :::
        AddIns(r1, r1, Imm(4)) ::
        Mov(rd, Imm(exprs.size)) ::
        Store(rd, ImmOffset(r1, Imm(-4))) ::
        exprs.zipWithIndex.flatMap(exprIndexPair => {
          transExpr(exprIndexPair._1, rd :: rest, symbolTable) :::
            storeOneElementInArray(t, rd, r1, exprIndexPair._2) ::
            List.empty
        }) :::
        Mov(rd, r1) ::
        List.empty

    case (NewPair(expr1, expr2), r1 :: Nil) =>
      // This procedure will need two registers 
      val allocateLv = allocateValForPair(RId(LR), r1, expr1, List.empty, symbolTable)
      val allocateRv = allocateValForPair(RId(LR), r1, expr2, List.empty, symbolTable)
      val mallocPair =
        callerRegsSave(regs) :::
          Mov(RId(0), Imm(8)) ::
          Bl("malloc") ::
          Mov(RId(LR), RId(0)) ::
          callerRegsRestore(regs) :::
          Pop(List(r1)) ::
          Store(r1, ImmOffset(RId(LR), Imm(4))) ::
          Pop(List(r1)) ::
          Store(r1, ImmOffset(RId(LR), Imm(0))) ::
          Mov(r1, RId(LR)) ::
          List.empty
      allocateLv ::: allocateRv ::: mallocPair

    case (NewPair(expr1, expr2), rd :: r1 :: rest) =>
      // This procedure will need two registers 
      val allocateLv = allocateValForPair(rd, r1, expr1, rest, symbolTable)
      val allocateRv = allocateValForPair(rd, r1, expr2, rest, symbolTable)
      val mallocPair =
        callerRegsSave(regs) :::
          Mov(RId(0), Imm(8)) ::
          Bl("malloc") ::
          Mov(rd, RId(0)) ::
          callerRegsRestore(regs) :::
          Pop(List(r1)) ::
          Store(r1, ImmOffset(rd, Imm(4))) ::
          Pop(List(r1)) ::
          Store(r1, ImmOffset(rd, Imm(0))) ::
          List.empty
      allocateLv ::: allocateRv ::: mallocPair

    case (Call(id, args), rd :: rest) =>
      callerRegsSave(regs) :::
      args.flatMap(expr =>
        transExpr(expr, regs, symbolTable) :::
          Store(rd, PreIndexed(RId(SP), Imm(-4))) ::
          List.empty) ::: // Pushing all arguments into the stack
      Bl("wacc_" + id.v) ::
      Mov(rd, RId(0)) ::
      callerRegsRestore(regs) :::
      AddIns(RId(SP), RId(SP), Imm(args.size * 4)) ::
      List.empty

    case (Fst(v), rd :: rest) =>
      addingFunc(ErrorNull)
      transLvalue(v, regs, symbolTable) :::
        Cmp(rd, Imm(0)) ::
        Bleq("_errNull") ::
        Load(rd, ImmOffset(rd, Imm(0))) ::
        Load(rd, ImmOffset(rd, Imm(0))) ::
        List.empty

    case (Snd(v), rd :: rest) =>
      addingFunc(ErrorNull)
      transLvalue(v, regs, symbolTable) :::
        Cmp(rd, Imm(0)) ::
        Bleq("_errNull") ::
        Load(rd, ImmOffset(rd, Imm(4))) ::
        Load(rd, ImmOffset(rd, Imm(0))) ::
        List.empty

    case (expr: Expr, rs) => transExpr(rv.asInstanceOf[Expr], rs, symbolTable)

    case (_, Nil) => throw new Exception("Registers should never run OUTTTTTT!!!!!")
  }


  // The SP need to be restored before conventinal pop instructions, using 996 because the maximum number of bits of immediates in arm11
  // is 3
  def restoreSP(sc: Int): List[Instruction] = {
    val totalRestore = sc * 4
    List.fill(totalRestore / 996)(AddIns(RId(SP), RId(SP), Imm(996))) :::
      AddIns(RId(SP), RId(SP), Imm(totalRestore % 996)) ::
      List.empty
  }

  def getIdFromLv(lv: Lvalue): Ident = lv match {
    case ArrayElem(id, expr) => id
    case id@Ident(v) => id
    case Fst(lv) => getIdFromLv(lv)
    case Snd(lv) => getIdFromLv(lv)
  }

  def addingSlashBeforeQuotation(s: String): String = {
    s.map[String](x => {
      if (x == '"') {
        "\\" + x
      } else {
        x.toString
      }
    }).fold("")((s1, s2) => s1.concat(s2))
  }
 
}
