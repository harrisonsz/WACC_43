package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.back.IR._
import wacc.decompiler.ArmToIR


class ArmToIRTests extends AnyFlatSpec with Matchers {

  " .data" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR(" .data").equals(Some(DotData)))
  }


  " .text" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR(" .text").equals(Some(DotText)))
  }

  " .global main" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR(" .global main").equals(Some(DotGlobal("main"))))
  }

  " .L0:" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR(" .L0:").equals(Some(Label(".L0"))))
  }

  " .L.str0:" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR(" .L.str0:").equals(Some(Label(".L.str0"))))
  }

  " main:" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR(" main:").equals(Some(Label("main"))))
  }

  "  mov fp, r2" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  mov fp, r2").equals(Some(Mov(RId(11), RId(2)))))
  }

  "  mov r1, #21" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  mov r1, #21").equals(Some(Mov(RId(1), Imm(21)))))
  }

  "  mov r1, r2, asr #3" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  mov r1, r2, asr #3").equals(Some(Mov(RId(1), ASR(RId(2), Imm(3))))))
  }

  "  mov fp, sp, lsl #-11" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  mov fp, sp, lsl #-11").equals(Some(Mov(RId(11), LSL(RId(13), Imm(-11))))))
  }

  "  pop {fp, r2, sp}" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  pop {fp, r2, sp}").equals(Some(Pop(List(RId(11), RId(2), RId(13))))))
  }

  "  push {fp, r2, sp}" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  push {fp, r2, sp}").equals(Some(Push(List(RId(11), RId(2), RId(13))))))
  }

  "  adds r1, r2, #3" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  adds r1, r2, #3").equals(Some(AddIns(RId(1), RId(2), Imm(3)))))
  }

  "  smull r1, r2, r3, r2" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  smull r1, r2, r3, r2").equals(Some(MulIns(RId(1), RId(3), RId(2)))))
  }

  "  str r1, [r2, #3]" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  str r1, [r2, #3]").equals(Some(Store(RId(1), ImmOffset(RId(2), Imm(3))))))
  }

  "  str r1, [r2, #3]!" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  str r1, [r2, #3]!").equals(Some(Store(RId(1), PreIndexed(RId(2), Imm(3))))))
  }

  "  str r1, [r2, r3, lsl #3]" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  str r1, [r2, r3, lsl #3]").equals(Some(Store(RId(1), LSLOffset(RId(2), RId(3), Imm(3))))))
  }

  "  strb r1, [r2, #3]!" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  strb r1, [r2, #3]!").equals(Some(StoreByte(RId(1), PreIndexed(RId(2), Imm(3))))))
  }

  "  ldr r1, =32" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  ldr r1, =32").equals(Some(Load(RId(1), Constant("32")))))
  }

  "  bleq main" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  bleq main").equals(Some(Bleq("main"))))
  }

  "  .asciz \"%.*s\"" should "be translated correctly" in {
    assert(ArmToIR.singleArmToIR("  .asciz \"%.*s\"").equals(Some(DotAsciz("\"%.*s\""))))
  }

  val instructions = " .data\n .text\n .global main\n main:\n  push {fp, lr}\n  push {r4, r8, r10, r12}\n  mov fp, #3\n  pop {fp, pc}\n"
  "list of instructions" should "be translated correctly" in {
    assert(ArmToIR.convertAllToIR(instructions) == List(DotData, DotText, DotGlobal("main"), Label("main"), Push(List(RId(11), RId(14))), Push(List(RId(4), RId(8), RId(10), RId(12))),
      Mov(RId(11), Imm(3)), Pop(List(RId(11), RId(15)))))
  }
}
