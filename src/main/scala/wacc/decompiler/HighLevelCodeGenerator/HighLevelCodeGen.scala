package wacc.decompiler.HighLevelCodeGenerator

import wacc.decompiler.L2IR._
import wacc.decompiler.CFgenerate.UserFunc

object HighLevelCodeGen {

  import wacc.decompiler.CFgenerate.{FlowGraph, MainFG}
  import wacc.decompiler.HighLevelCodeGenerator.TransExp._
  import wacc.decompiler.HighLevelCodeGenerator.TransCFBlock.transCFBlock

  def convertFlowGraphToWACC(fgs: List[FlowGraph]): String = {
    val mainFG: MainFG = getMainFG(fgs)
    val funcFGs = getUserFuncFGs(fgs)
    "begin\n" +
    funcFGs.map(transFunc(_)).mkString("\n") +
    addingDelimiterForCodesInAScope(transCFBlock(mainFG.mainHead)) + "\n" +
    "end\n" 
  }

  def transL2Instructions(instructions: List[L2ins]): List[String] = {
    val is = instructions.map(transL2ins(_)) 
    if (is.isEmpty) {
      List("skip")
    } else {
      is
    }
  }

  // The codes in a scope are separated by ;\n 
  def addingDelimiterForCodesInAScope(lines: List[String]): String = {
    lines.mkString(";\n")
  }

  def transL2ins(ins: L2ins): String = ins match {
    case assign(l2Id, aExp) => "int " + transAssign(l2Id, aExp)

    case call(name, args) => name + "(" + args.map(_.toString()).mkString(", ") + ")"

    case ret(ariExp) => transRet(ariExp)

    case _ => throw new Exception("Unexpected L2 instruction!!!")
  }

  def transAssign(l2Id: L2Id, exp: Exp): String = exp match {
    case _: AriExp => l2Id.name + " = " + transAriExp(exp.asInstanceOf[AriExp])
    case _: BoolExp => l2Id.name + " = " + transBoolExp(exp.asInstanceOf[BoolExp])
  }
  
  def transRet(ariExp: AriExp): String = {
    "return " + transAriExp(ariExp)
  }

  def getMainFG(flowGraphs: List[FlowGraph]): MainFG = {
    flowGraphs.find(_.isInstanceOf[MainFG]) match {
      case None => throw new Exception("MainFG not found")
      case Some(value) => value.asInstanceOf[MainFG]
    }
  }

  def getUserFuncFGs(fgs: List[FlowGraph]): List[FlowGraph] = {
    fgs.filter(_.isInstanceOf[UserFunc])
  }

  def transFunc(fg: FlowGraph): String = {
    "int " + fg.mainHead.label + "() is" + "\n" +
    addingDelimiterForCodesInAScope(transCFBlock(fg.mainHead)) + "\n" +
    "end" + "\n"
  }
}
