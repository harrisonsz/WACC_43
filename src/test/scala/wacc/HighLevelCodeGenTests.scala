package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.decompiler.HighLevelCodeGenerator.HighLevelCodeGen._
import wacc.decompiler.L2IR._
import wacc.decompiler.CFgenerate.FlowBlocks._
import wacc.decompiler.CFgenerate.MainFG
import wacc.decompiler.CFgenerate.UserFunc

class HighLevelCodeGenTests extends AnyFlatSpec with Matchers {
  "Flowgrah 1" should "be translated correctly" in {
    val fg = new MainFG
    fg.mainHead.blockL2Ins = List(
      assign(L2Id("v1"), L2Imm(3)),
      ret(L2Id("v1"))
    )
    println(convertFlowGraphToWACC(List(fg)))
  }

  "Flowgrah 2" should "be translated correctly" in {
    val fg = new MainFG
    fg.mainHead.blockL2Ins = List(
      assign(L2Id("v1"), L2Imm(3)),
      assign(L2Id("v2"), L2Imm(4)),
      ret(Add(L2Id("v1"), L2Id("v2")))
    )
    println(convertFlowGraphToWACC(List(fg)))
  }

  "Flowgrah 3" should "be translated correctly" in {
    val fg = new MainFG
    val headBlock = TwoWayBlock(null, null)
    headBlock.blockL2Ins = List(
      assign(L2Id("v1"), L2Imm(3)),
      JCond(L2Id("This is nothing"), EqL2(L2Id("v1"), L2Imm(3)))
    )
    val trueBlock = new ReturnBlock()
    val falseBlock = new ReturnBlock()
    trueBlock.blockL2Ins = List.empty
    falseBlock.blockL2Ins = List.empty
    headBlock.nextTrue = trueBlock
    headBlock.nextFalse = falseBlock
    fg.mainHead = headBlock
    println(convertFlowGraphToWACC(List(fg)))
  }

  "Flowgrah 4" should "be translated correctly" in {
    val fg = new MainFG
    val headBlock = TwoWayBlock(null, null)
    headBlock.blockL2Ins = List(
      assign(L2Id("v1"), L2Imm(3)),
      assign(L2Id("v2"), L2Imm(4)),
      JCond(L2Id("This is nothing"), EqL2(L2Id("v1"), L2Id("v2")))
    )
    val printlnBloc = new FallBlock(null)
    printlnBloc.label = "println"
    val trueBlock = new CallBlock("println", new ReturnBlock())
    val falseBlock = new CallBlock("println", new ReturnBlock())
    trueBlock.blockL2Ins = List(
      //call(printlnBloc, List(L2Id("v1")))
      call("println", List(L2Id("v1")))
    )
    falseBlock.blockL2Ins = List(
      //call(printlnBloc, List(L2Id("v2")))
      call("println", List(L2Id("v2")))
    )
    headBlock.nextTrue = trueBlock
    headBlock.nextFalse = falseBlock
    fg.mainHead = headBlock
    println(convertFlowGraphToWACC(List(fg)))
  }

  "Flowgrah 5" should "be translated correctly" in {
    val fg = new MainFG
    val whileHead = OneWayBlock(null)
    whileHead.blockL2Ins = List(
      assign(L2Id("v1"), L2Imm(3)),
      JUnc()
    )
    val whileCondBlock = TwoWayBlock(null, null)
    whileCondBlock.blockL2Ins = List(
      JCond(L2Id("B-612"), EqL2(L2Id("v1"), L2Imm(6)))
    )
    val whileBodyBlock: FallBlock = new FallBlock(null)
    whileBodyBlock.blockL2Ins = List(
      assign(L2Id("v1"), Add(L2Id("v1"), L2Imm(1)))
    )
    val whileFollowBlock = new ReturnBlock()
    fg.mainHead = whileHead
    whileHead.next = whileCondBlock
    whileCondBlock.nextTrue = whileBodyBlock
    whileCondBlock.nextFalse = whileFollowBlock
    whileBodyBlock.next = whileCondBlock
    println(convertFlowGraphToWACC(List(fg)))
  }

  "Flowgrah 6" should "be translated correctly" in {
    val funcFg = new UserFunc
    funcFg.mainHead.blockL2Ins = List(
      assign(L2Id("v1"), L2Imm(3)),
      ret(L2Id("v1"))
    )
    funcFg.mainHead.label = "foo"
    val fg = new MainFG
    fg.mainHead.blockL2Ins = List(
      //assign(L2Id("v2"), call(funcFg.mainHead, List()))
      assign(L2Id("v2"), call("", List()))
    )
    println(convertFlowGraphToWACC(List(fg, funcFg)))
  }
}
