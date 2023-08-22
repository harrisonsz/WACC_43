package wacc.back

object NewIR {
  sealed trait Operand extends  Instruction
  case class Imm(v: Int) extends Operand
  sealed trait Vars extends Operand
  case class Var(name: String) extends Vars

  sealed trait Instruction
  case class AssignBOp(nameL: Var, oprd1: Operand, oprd2: Operand, op: BOp) extends Instruction
  case class AssignUOp(nameL: Var, oprd: Operand, op: UOp) extends Instruction
  case class Copy(nameL: Var, oprd: Operand) extends Instruction
  case class CpStr(nameL: Var, str: String) extends Instruction
  case class Label(name: String) extends Instruction
  case class UcondJ(label: Label) extends Instruction
  case class TrueCondJ(cond: Var, label: Label) extends Instruction
  case class FalseCondJ(cond: Var, label: Label) extends Instruction
  case class Param(name: Var) extends Instruction
  case class Call(name: Label, n: Int) extends Instruction
  case class IndexedCpFromL(nameL: Var, arrPtr: Var, index: Operand) extends Instruction
  case class IndexedCpIntoL(arrPtr: Var, index: Operand, oprd: Operand) extends Instruction

  sealed trait BOp
  case object AddIR extends BOp
  case object SubIR extends BOp
  case object MulIR extends BOp
  case object DivIR extends BOp
  case object ModIR extends BOp
  case object GtIR extends BOp
  case object LtIR extends BOp
  case object GteIR extends BOp
  case object LteIR extends BOp
  case object EqIR extends BOp
  case object NeqIR extends BOp
  case object AndIR extends BOp
  case object OrIR extends BOp

  sealed trait UOp
  case object NotIR extends UOp
  case object NegIR extends UOp
  case object LenIR extends UOp
  case object OrdIR extends UOp
  case object ChrIR extends UOp
}
