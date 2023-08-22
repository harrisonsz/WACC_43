package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.back.IR._
import wacc.decompiler.CFgenerate.{ControlFlowGraph, FlowGraph}
import wacc.decompiler.{ArmToIR}

class ControlFlowGraphTests extends AnyFlatSpec with Matchers{
  //Basic
  val instructions = " .data\n .text\n .global main\n main:\n  push {fp, lr}\n  push {r4, r8, r10, r12}\n blvs _errOverflow\n mov fp, sp\n  pop {fp, pc}\n .data\n .word 4\n .L._prints_str0:\n"
  val res: List[Instruction] = ArmToIR.convertAllToIR(instructions)
  println(res)
  val flowGraph: List[FlowGraph] = ControlFlowGraph.generateAllGraph(res)
  println(flowGraph)

  //WithLabel
  val instructions1 = " .data\n .text\n .global main\n main:\n  push {fp, lr}\n .L1:\n  push {r4, r8, r10, r12}\n .L2:\n  mov fp, sp\n  pop {fp, pc}\n"
  val res1: List[Instruction] = ArmToIR.convertAllToIR(instructions1)
  println(res1)
  val flowGraph1: List[FlowGraph] = ControlFlowGraph.generateAllGraph(res1)
  flowGraph1.foreach(println)

  //With multi user func
  val instructions2 = " .data\n .text\n .global main\n main:\n  push {fp, lr}\n .L1:\n  push {r4, r8, r10, r12}\n .L2:\n  mov fp, sp\n  pop {fp, pc}\n" +
    " wacc_haha:\n  push {fp, lr}\n .L1:\n  push {r4, r8, r10, r12}\n .L2:\n  mov fp, sp\n  pop {fp, pc}\n" +
    " .data\n .text\n _printss:\n  push {fp, lr}\n .L1:\n  blvs _errOverflow\n push {r4, r8, r10, r12}\n .L2:\n  mov fp, sp\n  pop {fp, pc}\n"
  val res2: List[Instruction] = ArmToIR.convertAllToIR(instructions2)
  println(res2)
  val flowGraph2: List[FlowGraph] = ControlFlowGraph.generateAllGraph(res2)
  println(flowGraph2)


  //WithData at the end
  val instructions3 = " .text\n .global main\n main:\n  push {fp, lr}\n .L1:\n  push {r4, r8, r10, r12}\n .L2:\n  mov fp, sp\n  pop {fp, pc}\n .data\n"
  val res3: List[Instruction] = ArmToIR.convertAllToIR(instructions3)
  println(res3)
  val flowGraph3: List[FlowGraph] = ControlFlowGraph.generateAllGraph(res3)
  flowGraph3.foreach(println)
}
