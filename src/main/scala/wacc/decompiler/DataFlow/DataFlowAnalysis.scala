package wacc.decompiler.DataFlow

import wacc.back.RegOperator
import wacc.decompiler.CFgenerate.{MainFG, UserFunc}

object DataFlowAnalysis {
  import wacc.back.IR._
  import wacc.decompiler.CFgenerate.FlowGraph
  import wacc.decompiler.L2IR._

  private val divModIns: List[Instruction] = List(Cmp(RId(1), Imm(0)), 
    Bleq("_errDivZero"), Bl("__aeabi_idivmod"))

  def transFlowGraphs(flowGraphs: List[FlowGraph]): List[FlowGraph] = {
    flowGraphs.foreach{
      case fg: MainFG =>
        val idGenerator = new IdGenerator
        val symbolTable = new SimpleSymbolTable
        fg.nodes.foreach(cfb => cfb.blockL2Ins = iRtoL2IR(cfb.blockInstructions, idGenerator, symbolTable))
      case fg: UserFunc =>
        val idGenerator = new IdGenerator
        val symbolTable = new SimpleSymbolTable
        fg.nodes.foreach(cfb => cfb.blockL2Ins = iRtoL2IR(cfb.blockInstructions, idGenerator, symbolTable))
      case _ =>
        None 
    }
    flowGraphs
  }

  def genericAssBOp(rd: RId, rn: RId, op2: Operand2, bOp: (Exp, Exp) => BiOp, idGenerator: IdGenerator, symbolTable: SimpleSymbolTable): List[L2ins] = {
    if (rn.id == RegOperator.SP) {
      return List.empty
    }
    val rnVal = transOp2(rn, symbolTable)
    val op2Val = transOp2(op2, symbolTable)
    symbolTable.contains(rd) match {
      case true => 
        List(assign(transOp2(rd, symbolTable).asInstanceOf[L2Id], bOp(rnVal, op2Val)))
      case false =>
        val rdVal = idGenerator.getId()
        symbolTable.add(rd, rdVal)
        List(assign(L2Id(rdVal), bOp(rnVal, op2Val)))
    }
  }

  def iRtoL2IR(instructions: List[Instruction], idGenerator: IdGenerator, symbolTable: SimpleSymbolTable): List[L2ins] = {
    instructions match {
      case Nil => List.empty
      //case Mov(RId(0), _) :: Bl("exit") :: rest =>
      //  iRtoL2IR(rest, idGenerator, symbolTable)
      // case Mov(RId(0), op2) :: Bl("_printi") :: rest =>
      //   call()
      case head :: rest =>
        head match {
          case Mov(_, RId(RegOperator.SP)) =>
            iRtoL2IR(rest, idGenerator, symbolTable)
          case Mov(rd, op2) => 
            val op2Val = transOp2(op2, symbolTable)
            symbolTable.contains(rd) match {
              case true => 
                assign(transOp2(rd, symbolTable).asInstanceOf[L2Id], op2Val) ::
                iRtoL2IR(rest, idGenerator, symbolTable)
              case false =>
                val rdVal = idGenerator.getId()
                symbolTable.add(rd, rdVal)
                assign(L2Id(rdVal), op2Val) ::
                iRtoL2IR(rest, idGenerator, symbolTable)
            }
          case AddIns(rd, rn, op2) => 
            genericAssBOp(rd, rn, op2, Add.asInstanceOf[(Exp, Exp) => BiOp], idGenerator, symbolTable) :::
            iRtoL2IR(rest, idGenerator, symbolTable)
          case SubIns(rd, rn, op2) => 
            genericAssBOp(rd, rn, op2, Sub.asInstanceOf[(Exp, Exp) => BiOp], idGenerator, symbolTable) :::
            iRtoL2IR(rest, idGenerator, symbolTable)
          case MulIns(rd, rm, rs) => 
            genericAssBOp(rd, rm, rs, Mul.asInstanceOf[(Exp, Exp) => BiOp], idGenerator, symbolTable) :::
            iRtoL2IR(rest, idGenerator, symbolTable)
          case AndIns(rd, rn, op2) => 
            genericAssBOp(rd, rn, op2, AndL2.asInstanceOf[(Exp, Exp) => BiOp], idGenerator, symbolTable) :::
            iRtoL2IR(rest, idGenerator, symbolTable)
          case Cmp(RId(1), Imm(0)) =>
            if (instructions.startsWith(divModIns)) {
              instructions(3) match {
                case Mov(rd, RId(0)) =>
                  genericAssBOp(rd, RId(0), RId(1), Div.asInstanceOf[(Exp, Exp) => BiOp], idGenerator, symbolTable) :::
                  iRtoL2IR(instructions.slice(4, instructions.size), idGenerator, symbolTable)
                case Mov(rd, RId(1)) =>
                  genericAssBOp(rd, RId(0), RId(1), Mod.asInstanceOf[(Exp, Exp) => BiOp], idGenerator, symbolTable) :::
                  iRtoL2IR(instructions.slice(4, instructions.size), idGenerator, symbolTable)
                case _ =>
                  throw new Exception("Should not happen")
              }
            } else {
              List.empty // not implemented yet
            }
          case Push(regs) => 
            iRtoL2IR(rest, idGenerator, symbolTable)
          case Pop(regs) => 
            iRtoL2IR(rest, idGenerator, symbolTable)
          case Label(name) => 
            iRtoL2IR(rest, idGenerator, symbolTable)
          case Blvs("_errOverflow") =>
            iRtoL2IR(rest, idGenerator, symbolTable)
        }
    } 
  }

  def transOp2(op2: Operand2, symbolTable: SimpleSymbolTable): AriExp = op2 match {
    case Imm(v) => L2Imm(v)
    case reg@RId(id) => 
      symbolTable.get(reg) match {
        case None => 
          println(symbolTable.symbolTable)
          throw new Exception("can't find name for rn: r" + id)
        case Some(value) => L2Id(value)
      }
  }
}
