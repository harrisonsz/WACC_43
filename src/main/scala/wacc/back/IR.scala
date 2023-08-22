package wacc.back

object IR{
  sealed trait Operand2
  case class RId(id: Int) extends Operand2
  case class Imm(v: Int) extends Operand2
  case class ASR(rm: RId, imm: Imm) extends Operand2
  case class LSL(rm: RId, imm: Imm) extends Operand2

  sealed trait AMode2
  case class ImmOffset(rn: RId, imm: Imm) extends AMode2
  case class LSLOffset(rn: RId, rm: RId, imm: Imm) extends AMode2
  case class Constant(label: String) extends AMode2
  case class PreIndexed(rn: RId, imm: Imm) extends AMode2

  sealed trait Instruction
  case class Push(regs: List[RId]) extends Instruction
  case class Pop(regs: List[RId]) extends Instruction 
  case class Mov(rd: RId, op2: Operand2) extends Instruction
  case class Label(name: String) extends Instruction
  case class Bl(name: String) extends Instruction
  case class Bleq(name: String) extends Instruction
  case class Cmp(rn: RId, op2: Operand2) extends Instruction
  case class AddIns(rd: RId, rn: RId, op2: Operand2) extends Instruction
  case class SubIns(rd: RId, rn: RId, op2: Operand2) extends Instruction
  case class MulIns(rd: RId, rm: RId, rs: RId) extends Instruction
  case class LTIns(rd: RId, rn: RId, op2: Operand2) extends Instruction
  case class LEIns(rd: RId, rn: RId, op2: Operand2) extends Instruction
  case class GTIns(rd: RId, rn: RId, op2: Operand2) extends Instruction
  case class GEIns(rd: RId, rn: RId, op2: Operand2) extends Instruction
  case class EqIns(rd: RId, rn: RId, op2: Operand2) extends Instruction
  case class NeqIns(rd: RId, rn: RId, op2: Operand2) extends Instruction
  case class AndIns(rd: RId, rn: RId, op2: RId) extends Instruction
  case class OrIns(rd: RId, rn: RId, op2: RId) extends Instruction
  case class MovLT(rd: RId, op2: Operand2) extends Instruction
  case class MovLE(rd: RId, op2: Operand2) extends Instruction
  case class MovGT(rd: RId, op2: Operand2) extends Instruction
  case class MovGE(rd: RId, op2: Operand2) extends Instruction
  case class MovEq(rd: RId, op2: Operand2) extends Instruction
  case class MovNeq(rd: RId, op2: Operand2) extends Instruction
  case class Store(rt: RId, amode2: AMode2) extends Instruction
  case class StoreByte(rt: RId, amode2: AMode2) extends Instruction
  case class Load(rd: RId, amode2: AMode2) extends Instruction
  case class LoadByte(rt: RId, amode2: AMode2) extends Instruction
  case class LoadSignedByte(rt: RId, amode2: AMode2) extends Instruction
  case class Beq(label: String) extends Instruction
  case class Bne(label: String) extends Instruction
  case class Blvs(label: String) extends Instruction
  case class Bllt(label: String) extends Instruction
  case class Blge(label: String) extends Instruction
  case class Blne(label: String) extends Instruction
  case class Branch(name: String) extends Instruction
  case class Rsbs(rd: RId, rn: RId, op2: Operand2) extends Instruction

  case object DotText extends Instruction
  case object DotData extends Instruction
  case class DotWord(size: Int) extends Instruction
  case class DotAsciz(string: String) extends Instruction
  case class DotGlobal(tag: String) extends Instruction
  case class Customize(string: String) extends Instruction
}

