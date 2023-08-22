package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.back.IR._
import wacc.decompiler.CFgenerate.{ControlFlowGraph, FlowGraph}
import wacc.decompiler.ArmToIR
import wacc.decompiler.DataFlow.L2IRGenerator._
import wacc.decompiler.DataFlow.DataFlowOptimisation._
import wacc.decompiler.CFgenerate.FlowBlocks
import wacc.decompiler.DataFlow.DataFlowEquations.liveOutBlock

import scala.collection.mutable.ListBuffer


class DataFlowTest extends AnyFlatSpec with Matchers{

  val whileIns = ".data\n @ length of .L.str0\n .word 7\n .L.str0:\n .asciz \"flip b!\"\n @ length of .L.str1\n .word 11\n .L.str1:\n .asciz \"end of loop\"\n .text\n .global main\n main:\n push {fp, lr}\n push {r4, r8, r10, r12}\n mov fp, sp\n @ Stack pointer unchanged, no stack allocated variables\n mov r8, #1\n mov r4, r8\n b .L0\n .L1:\n ldr r8, =.L.str0\n push {r8}\n pop {r8}\n mov r8, r8\n mov r0, r8\n @ statement primitives do not return results (but will clobber r0/rax)\n bl _prints\n bl _println\n cmp r4, #1\n movne r8, #1\n moveq r8, #0\n push {r8}\n pop {r8}\n mov r8, r8\n mov r4, r8\n .L0:\n cmp r4, #1\n beq .L1\n ldr r8, =.L.str1\n push {r8}\n pop {r8}\n mov r8, r8\n mov r0, r8\n @ statement primitives do not return results (but will clobber r0/rax)\n bl _prints\n bl _println\n @ Stack pointer unchanged, no stack allocated variables\n mov r0, #0\n pop {r4, r8, r10, r12}\n pop {fp, pc}\n .data\n @ length of .L._prints_str0\n .word 4\n .L._prints_str0:\n .asciz \"%.*s\"\n .text\n _prints:\n push {lr}\n mov r2, r0\n ldr r1, [r0, #-4]\n ldr r0, =.L._prints_str0\n bl printf\n mov r0, #0\n bl fflush\n pop {pc}\n .data\n @ length of .L._println_str0\n .word 0\n .L._println_str0:\n .asciz \"\"\n .text\n _println:\n push {lr}\n ldr r0, =.L._println_str0\n bl puts\n mov r0, #0\n bl fflush\n pop {pc}\n"
  //Basic
  val instructions = " .data\n .text\n .global main\n main:\n  push {fp, lr}\n  push {r4, r8, r10, r12}\n mov r5, #15\n mov r4, r5\n mov r0, r4\n bl _printi\n blvs _errOverflow\n mov fp, sp\n  pop {fp, pc}\n .data\n .word 4\n .L._prints_str0:\n"
  val res: List[Instruction] = ArmToIR.convertAllToIR(whileIns)
  val flowGraph: List[FlowGraph] = ControlFlowGraph.generateAllGraph(res)
  flowGraph.foreach(_.nodes.foreach(x => x.labelFromBools = ListBuffer.fill(x.labelFrom.size)(false)))
  val l2FlowGraph: List[FlowGraph] = transFlowGraphs(flowGraph)
  //println(l2FlowGraph)
  //l2FlowGraph.foreach(_.nodes.foreach( x => println(x.blockL2Ins)))
  //ddl2FlowGraph.foreach(_.nodes.foreach(x => x.blockL2Ins = deadRegElim(x)))
  //l2FlowGraph.foreach(_.nodes.foreach( x => println(x.blockL2Ins)))

  // //WithData at the end
  // val instructions3 = " .text\n .global main\n main:\n  push {fp, lr}\n .L1:\n  push {r4, r8, r10, r12}\n .L2:\n  mov fp, sp\n  pop {fp, pc}\n .data\n"
  // val res3: List[Instruction] = ArmToIR.convertAllToIR(instructions3)
  // //println(res3)
  // val flowGraph3: List[FlowGraph] = ControlFlowGraph.generateAllGraph(res3)
  // flowGraph3.foreach(println)
}
