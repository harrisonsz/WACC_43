package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.decompiler.ControlFlowAnalysis.CFAnalysis._
import wacc.decompiler.CFgenerate.FlowBlocks._
import wacc.decompiler.CFgenerate.FlowGraph

class CFAnalysisTests extends AnyFlatSpec with Matchers {
  "A simple if else structure" should "be translated into a if else block" in {
    val ret2 = ReturnBlock()
    val ret1 = OneWayBlock(ret2)
    val ret = OneWayBlock(ret1)

    val b1 = OneWayBlock(ret)
    b1.label = "b1"
    val b10 = OneWayBlock(b1)
    b10.label = "b10"
    val b11 = OneWayBlock(b10)
    b11.label = "b11"

    val b2 = OneWayBlock(ret)
    b2.label = "b2"
    val b20 = OneWayBlock(b2)
    b20.label = "b20"
    val b21 = OneWayBlock(b20)
    b21.label = "b21"

    val b3 = TwoWayBlock(b11, b21)
    val fl = new FlowGraph()
    fl.mainHead = b3
    val res = cFAnalysisEach(fl)
    print(res)
  }

  "A simple while structure" should "be translated into a while block" in {
    val ret2 = ReturnBlock()
    val ret1 = OneWayBlock(ret2)
    val ret = OneWayBlock(ret1)

    val b1 = OneWayBlock(ret)
    b1.label = "b1"
    val b10 = OneWayBlock(b1)
    b10.label = "b10"
    val b11 = OneWayBlock(b10)
    b11.label = "b11"

    val b2 = OneWayBlock(ret)
    b2.label = "b2"
    val b20 = OneWayBlock(b2)
    b20.label = "b20"
    val b21 = OneWayBlock(b20)
    b21.label = "b21"

    val b3 = TwoWayBlock(b11, b21)
    b2.next = b3
    val fl = new FlowGraph()
    fl.mainHead = b3
    val res = cFAnalysisEach(fl)
    print(res)
  }

  "A if contain while" should "be translated into a if else block" in {
    val ret2 = ReturnBlock()
    val ret1 = OneWayBlock(ret2)
    val ret = OneWayBlock(ret1)

    val b1 = OneWayBlock(ret)
    b1.label = "b1"
    val b10 = OneWayBlock(b1)
    b10.label = "b10"
    val b11 = OneWayBlock(b10)
    b11.label = "b11"

    val b2 = OneWayBlock(ret)
    b2.label = "b2"
    val b20 = OneWayBlock(b2)
    b20.label = "b20"
    val b21 = TwoWayBlock(b20, b2)
    b21.label = "b21"

    val b3 = TwoWayBlock(b11, b21)
    b20.next = b21
    val fl = new FlowGraph()
    fl.mainHead = b3
    val res = cFAnalysisEach(fl)
    print(res)
  }
}
