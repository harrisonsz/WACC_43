package wacc.decompiler.DataFlow

import wacc.back.RegOperator
import wacc.decompiler.CFgenerate.{MainFG, UserFunc}
import wacc.decompiler.CFgenerate.FlowBlocks

object L2IRGenerator {
  import wacc.back.IR._
  import wacc.decompiler.CFgenerate.FlowGraph
  import wacc.decompiler.L2IR._

  private val divModIns: List[Instruction] = List(Cmp(RId(1), Imm(0)), 
    Bleq("_errDivZero"), Bl("__aeabi_idivmod"))
  
  def getMainFG(flowGraphs: List[FlowGraph]): MainFG = {
    flowGraphs.find(_.isInstanceOf[MainFG]) match {
      case None => throw new Exception("MainFG not found")
      case Some(value) => value.asInstanceOf[MainFG]
    }
  }

  def transFlowGraphs(flowGraphs: List[FlowGraph]): List[FlowGraph] = {
    flowGraphs.foreach{
      case fg: MainFG =>
        fg.nodes.foreach(cfb => cfb.blockL2Ins = iRtoL2IR(cfb, cfb.blockInstructions))
        DataFlowOptimisation.optGraphDataFlow(fg)
      case fg: UserFunc =>
        fg.nodes.foreach(cfb => cfb.blockL2Ins = iRtoL2IR(cfb, cfb.blockInstructions))
        DataFlowOptimisation.optGraphDataFlow(fg)
      case _ =>
        None 
    }
    flowGraphs
  }

  def genericAssBOp(rd: RId, rn: RId, op2: Operand2, bOp: (Exp, Exp) => BiOp): List[L2ins] = {
    if (rn.id == RegOperator.SP) {
      return List.empty
    }
    val rnVal = transOp2(rn)
    val op2Val = transOp2(op2)
    List(assign(transOp2(rd).asInstanceOf[L2Id], bOp(rnVal, op2Val)))
  }

  def iRtoL2IR(cfb: FlowBlocks.CFBlock, instructions: List[Instruction]): List[L2ins] = {
    instructions match {
      case Nil => List.empty
      case Cmp(opr1, opr2) :: rest =>
        if (opr1.equals(RId(1)) && opr2.equals(Imm(0)) && instructions.startsWith(divModIns)) {
          instructions(3) match {
            case Mov(rd, RId(0)) =>
              genericAssBOp(rd, RId(0), RId(1), Div.asInstanceOf[(Exp, Exp) => BiOp]) :::
              iRtoL2IR(cfb, instructions.slice(4, instructions.size))
            case Mov(rd, RId(1)) =>
              genericAssBOp(rd, RId(0), RId(1), Mod.asInstanceOf[(Exp, Exp) => BiOp]) :::
              iRtoL2IR(cfb, instructions.slice(4, instructions.size))
            case _ =>
              throw new Exception("Should not happen")
          }
        } else {
          rest match {
            case Beq(_) :: rrest =>
              JCond(null, EqL2(transOp2(opr1), transOp2(opr2))) ::
              iRtoL2IR(cfb, rrest)
            case Bne(_) :: rrest =>
              JCond(null, NEqL2(transOp2(opr1), transOp2(opr2))) ::
              iRtoL2IR(cfb, rrest)
          }
        }
      case Mov(RId(0), opr) :: Bl(s) :: rest =>
        if (List("_printi", "_prints", "_printc", "_printb", "_printp").contains(s)) {
          cfb.asInstanceOf[FlowBlocks.CallBlock].nextFall.blockInstructions match {
            case Bl("_println") :: _ =>
              call("println", List(transOp2(opr))) ::
              iRtoL2IR(cfb, rest)
            case _ =>
              call("print", List(transOp2(opr))) ::
              iRtoL2IR(cfb, rest)
          }
        } else {
          throw new Exception("Decompiler currently only supports function print")
        }
      case head :: rest =>
        head match {
          case EqIns(rd, rn, op2) => 
            assign(transOp2(rd).asInstanceOf[L2Id], EqL2(transOp2(rn), transOp2(op2))) ::
            iRtoL2IR(cfb, rest)
          case Mov(_, RId(RegOperator.SP)) =>
            iRtoL2IR(cfb, rest)
          case Mov(rd, op2) => 
            val op2Val = transOp2(op2)
            assign(transOp2(rd).asInstanceOf[L2Id], op2Val) ::
            iRtoL2IR(cfb, rest)
          case Load(rd, amode2) => 
            assign(transOp2(rd).asInstanceOf[L2Id], transAMode2(amode2)) ::
            iRtoL2IR(cfb, rest)
          case AddIns(rd, rn, op2) => 
            genericAssBOp(rd, rn, op2, Add.asInstanceOf[(Exp, Exp) => BiOp]) :::
            iRtoL2IR(cfb, rest)
          case SubIns(rd, rn, op2) => 
            genericAssBOp(rd, rn, op2, Sub.asInstanceOf[(Exp, Exp) => BiOp]) :::
            iRtoL2IR(cfb, rest)
          case MulIns(rd, rm, rs) => 
            genericAssBOp(rd, rm, rs, Mul.asInstanceOf[(Exp, Exp) => BiOp]) :::
            iRtoL2IR(cfb, rest)
          case AndIns(rd, rn, op2) => 
            genericAssBOp(rd, rn, op2, AndL2.asInstanceOf[(Exp, Exp) => BiOp]) :::
            iRtoL2IR(cfb, rest)
          case Cmp(RId(1), Imm(0)) =>
            if (instructions.startsWith(divModIns)) {
              instructions(3) match {
                case Mov(rd, RId(0)) =>
                  genericAssBOp(rd, RId(0), RId(1), Div.asInstanceOf[(Exp, Exp) => BiOp]) :::
                  iRtoL2IR(cfb, instructions.slice(4, instructions.size))
                case Mov(rd, RId(1)) =>
                  genericAssBOp(rd, RId(0), RId(1), Mod.asInstanceOf[(Exp, Exp) => BiOp]) :::
                  iRtoL2IR(cfb, instructions.slice(4, instructions.size))
                case _ =>
                  throw new Exception("Should not happen")
              }
            } else {
              List.empty // not implemented yet
            }
          case Push(regs) => 
            iRtoL2IR(cfb, rest)
          case Pop(regs) => 
            iRtoL2IR(cfb, rest)
          case Label(name) => 
            iRtoL2IR(cfb, rest)
          case Blvs("_errOverflow") =>
            iRtoL2IR(cfb, rest)
          case Branch(name) => 
            iRtoL2IR(cfb, rest)
          case Bl("_println") =>
            iRtoL2IR(cfb, rest)
        }
    } 
  }

  def transOp2(op2: Operand2): AriExp = op2 match {
    case Imm(v) => L2Imm(v)
    case reg@RId(id) => L2Id(id.toString())
  }

  def transAMode2(amode: AMode2) : L2Id = amode match {
    case Constant(label) => L2Id(label)
    case ImmOffset(rn, imm) => L2Id("[" + rn.id + ", " + imm.v + "]")
    case _ => throw new Exception("Other addressing mode not supported yet")
  }
}
