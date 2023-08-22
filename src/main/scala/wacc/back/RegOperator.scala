package wacc.back

import wacc.back.IR.{Instruction, Pop, Push, RId}
import scala.collection.mutable.ArrayBuffer

object RegOperator {

  // R11 => FP, R13 => SP, R14 => LR, R15 => PC
  val FP = 11 // frame pointer
  val SP = 13 // stack pointer
  val LR = 14 // link register
  val PC = 15 // program counter
  val DIV = 0 // used in divMod
  val MOD = 1 // used in divMod
  val STR = 0 // used in arrElemLoadStoreHelper
  val LDR = 1 // used in arrElemLoadStoreHelper

  // The link register can be used for calculations because it is only used when the branching occurs
  val GeneralRegs: List[RId] = List(RId(4), RId(5), RId(6), RId(7), RId(8), RId(10), RId(12), RId(0), RId(1), RId(2), RId(3))

  val CalleeSavedRegs: List[RId] = List(RId(4), RId(5), RId(6), RId(7), RId(8), RId(10), RId(12))

  val CallerSavedRegs: List[RId] = List(RId(0), RId(1), RId(2), RId(3))

  // Saving all callee-saved registers
  def allCalleeSave(): Instruction = {
    Push(List(RId(4), RId(5), RId(6), RId(7), RId(8), RId(10), RId(12)))
  }

  // Saving all caller-saved registers
  def allCallerSave(): Instruction = {
    Push(List(RId(0), RId(1), RId(2), RId(3)))
  }
  
  def usedCallerSave(unusedRegs: List[RId]): List[Instruction] = {
    val regList = CallerSavedRegs.filter(!unusedRegs.contains(_))
    if (regList.isEmpty) {
      List.empty
    } else {
      List(Push(regList))
    }
  }

  def callerRegsSave(unusedRegs: List[RId]): List[Instruction] = {
    if (IRGenerator.OptimiseERA) {
      usedCallerSave(unusedRegs)
    } else {
      List(allCallerSave())
    }
  }

  // Popping all callee-saved registers
  def allCalleeRestore(): Instruction = {
    Pop(List(RId(4), RId(5), RId(6), RId(7), RId(8), RId(10), RId(12)))
  }

  // Popping all caller-saved registers
  def allCallerRestore(): Instruction = {
    Pop(List(RId(0), RId(1), RId(2), RId(3)))
  }

  def usedCallerRestore(unusedRegs: List[RId]): List[Instruction] = {
    val regList = CallerSavedRegs.filter(!unusedRegs.contains(_))
    if (regList.isEmpty) {
      List.empty
    } else {
      List(Pop(regList))
    }
  }

  def callerRegsRestore(unusedRegs: List[RId]): List[Instruction] = {
    if (IRGenerator.OptimiseERA) {
      usedCallerRestore(unusedRegs)
    } else {
      List(allCallerRestore())
    }
  }

  def allRegs(): List[RId] = {
    List.range(0, 12).map(RId)
  }

  def getUsedCalleeSavedRegs(instructions: List[Instruction]): List[RId] = {
    (instructions.foldLeft(Set.empty[RId]){ case (regs, i) =>
      i match {
        case InstructionUsingReg(rd) if CalleeSavedRegs.contains(rd) => regs + rd
        case _                       => regs
      }
    }).toList.sortWith(_.id < _.id)
  }

  // R1, R3 and R10 are used in the procedure of the load of array element
  // so they should be considered independently
  def getUsedRegsForLoadArrayElem(arrPtr: RId, regs: List[RId]): List[RId] = {
    List(RId(1), RId(3), RId(10)).filter(x => x != arrPtr && !regs.contains(x))
  }

  def checkPushForLoad(arrPtr: RId, regs: List[RId]): List[Instruction] = {
    val regList = getUsedRegsForLoadArrayElem(arrPtr, regs)
    if (regList.isEmpty) {
      List.empty
    } else {
      List(Push(regList))
    }
  }

  def checkPopForLoad(arrPtr: RId, regs: List[RId]): List[Instruction] = {
    val regList = getUsedRegsForLoadArrayElem(arrPtr, regs)
    if (regList.isEmpty) {
      List.empty
    } else {
      List(Pop(regList))
    }
  }

  // R1, R3, R8 and R10 are used in the procedure of the store of array element
  // so they should be considered independently
  def getUsedRegsForStoreArrayElem(arrPtr: RId, regs: List[RId]): List[RId] = {
    List(RId(1), RId(3), RId(8), RId(10)).filter(x => arrPtr != x && !regs.contains(x))
  }

  def checkPushForStore(arrPtr: RId, regs: List[RId]): List[Instruction] = {
    val regList = getUsedRegsForStoreArrayElem(arrPtr, regs)
    if (regList.isEmpty) {
      List.empty
    } else {
      List(Push(regList.toList))
    }
  }

  def checkPopForStore(arrPtr: RId, regs: List[RId]): List[Instruction] = {
    val regList = getUsedRegsForStoreArrayElem(arrPtr, regs)
    if (regList.isEmpty) {
      List.empty
    } else {
      List(Pop(regList.toList))
    }
  }
}
