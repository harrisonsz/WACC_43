package wacc.back

object CodeGenerator{
  import IR._
  import Peephole.peepholeOptimize

  // It accecpts the list of IRs and return a String
  def generatingCode(instructions: List[Instruction]): String = {
    instructions.map(transformOneInstruction(_)).mkString("\n") + "\n"
  }

  def transformOneInstruction(instruction: Instruction): String = instruction match {
    case DotData        => " .data"
    case DotText        => " .text"
    case DotGlobal(tag) => " .global" + " " + tag
    case Label(name)    => " " + name + ":"
    case Mov(rd, op2)   => "  mov " + registerName(rd) + ", " + operandName(op2)
    case Pop(regs)      => "  pop " + "{" + regs.map(registerName(_)).mkString(", ") + "}"
    case Push(regs)     => "  push " + "{" + regs.map(registerName(_)).mkString(", ") + "}"
    case Bl(name)       => "  bl " + name
    case AddIns(rd, rn, op2) => "  adds " + registerName(rd) + ", " + registerName(rn) + ", " + operandName(op2)
    case SubIns(rd, rn, op2) => "  subs " + registerName(rd) + ", " + registerName(rn) + ", " + operandName(op2)
    case Rsbs(rd, rn, op2) => "  rsbs " + registerName(rd) + ", " + registerName(rn) + ", " + operandName(op2)
    case MulIns(rd, rm, rs)  => "  smull " + registerName(rd) + ", " + registerName(rs) + ", " + registerName(rm) + ", " + registerName(rs)
    case LTIns(r1, r2, op2) => "  cmp " + registerName(r2) + ", " + operandName(op2) + "\n" +
                                transformOneInstruction(MovLT(r1, Imm(1))) + "\n" +
                                transformOneInstruction(MovGE(r1, Imm(0)))
    case LEIns(r1, r2, op2) => "  cmp " + registerName(r2) + ", " + operandName(op2) + "\n" +
                                transformOneInstruction(MovLE(r1, Imm(1))) + "\n" +
                                transformOneInstruction(MovGT(r1, Imm(0)))
    case GTIns(r1, r2, op2) => "  cmp " + registerName(r2) + ", " + operandName(op2) + "\n" +
                                transformOneInstruction(MovGT(r1, Imm(1))) + "\n" +
                                transformOneInstruction(MovLE(r1, Imm(0)))
    case GEIns(r1, r2, op2) => "  cmp " + registerName(r2) + ", " + operandName(op2) + "\n" +
                                transformOneInstruction(MovGE(r1, Imm(1))) + "\n" +
                                transformOneInstruction(MovLT(r1, Imm(0)))
    case EqIns(r1, r2, op2) => transformOneInstruction(Cmp(r2, op2)) + "\n" +
                               transformOneInstruction(MovEq(r1, Imm(1))) + "\n" +
                               transformOneInstruction(MovNeq(r1, Imm(0)))
    case NeqIns(r1, r2, op2) => transformOneInstruction(Cmp(r2, op2)) + "\n" +
                               transformOneInstruction(MovNeq(r1, Imm(1))) + "\n" +
                               transformOneInstruction(MovEq(r1, Imm(0)))
    case Store(rt, ImmOffset(rn, offset)) => "  str " + registerName(rt) +
     ", [" + registerName(rn) + ", " + operandName(offset) + "]"
    case Store(rt, PreIndexed(rn, offset)) => transformOneInstruction(Store(rt, ImmOffset(rn, offset))) + "!"
    case Store(rt, LSLOffset(rn, rm, offset)) => "  str " + registerName(rt) + ", [" + registerName(rn) + ", " + registerName(rm) + ", lsl " + operandName(offset) + "]"
    case Store(_, Constant(_)) => throw new Exception("cannot store constant")
    case StoreByte(rt, PreIndexed(rn, offset)) =>
      "  strb " + registerName(rt) + ", [" + registerName(rn) + ", " + operandName(offset) + "]!" 
    case StoreByte(rt, ImmOffset(rn, imm)) => "  strb " + registerName(rt) + ", [" + registerName(rn) + ", " + operandName(imm) + "]"
    case StoreByte(rt, LSLOffset(rn, rm, offset)) => "  strb " + registerName(rt) + ", [" + registerName(rn) + ", " + registerName(rm) + ", lsl " + operandName(offset) + "]"
    case Load(rd, Constant(label)) => "  ldr " + registerName(rd) + ", =" + label
    case Load(rd, ImmOffset(rn, imm))       => "  ldr " + registerName(rd) + 
      ", [" + registerName(rn) + ", " + operandName(imm) + "]"
    case Load(rd, PreIndexed(rn, imm)) => transformOneInstruction(Load(rd, ImmOffset(rn, imm))) + "!"
    case Load(rt, LSLOffset(rn, rm, offset)) => "  ldr " + registerName(rt) + ", [" + registerName(rn) + ", " + registerName(rm) + ", lsl " + operandName(offset) + "]"
    case LoadByte(rt, LSLOffset(rn, rm, offset)) => "  ldrb " + registerName(rt) + ", [" + registerName(rn) + ", " + registerName(rm) + ", lsl " + operandName(offset) + "]"
    case LoadSignedByte(rd, ImmOffset(rn, imm)) => "  ldrsb " + registerName(rd) + ", [" + registerName(rn) + ", " + operandName(imm) + "]"
    case Beq(label) => "  beq " + label
    case Bne(label) => "  bne " + label
    case Customize(string) => "  " + string
    case Bleq(name) => "  bleq " + name
    case Blvs(label) => "  blvs " + label
    case Bllt(name) => "  bllt " + name
    case Blge(label) => "  blge " + label
    case Blne(label) => "  blne " + label
    case Cmp(rn, op2) => "  cmp " + registerName(rn) + ", " + operandName(op2)
    case Branch(label) => "  b " + label
    case MovLT(rd, v) => "  movlt " + registerName(rd) + ", " + operandName(v)
    case MovLE(rd, v) => "  movle " + registerName(rd) + ", " + operandName(v)
    case MovGT(rd, v) => "  movgt " + registerName(rd) + ", " + operandName(v)
    case MovGE(rd, v) => "  movge " + registerName(rd) + ", " + operandName(v)
    case MovEq(rd, v) => "  moveq " + registerName(rd) + ", " + operandName(v)
    case MovNeq(rd, v) => "  movne " + registerName(rd) + ", " + operandName(v)
    case DotAsciz(string) => "  .asciz " + "\"" + string + "\""
    case DotWord(size) => "  .word " + size
    case AndIns(rd, rn, op2) => "  and " + registerName(rd) + ", " + registerName(rn) + ", " + operandName(op2)
    case OrIns(rd, rn, op2) => "  orr " + registerName(rd) + ", " + registerName(rn) + ", " + operandName(op2)
    case _ => throw new Exception("Should not arrive here")
  }

  def operandName(op2: Operand2): String = op2 match {
    case rid@RId(id) => registerName(rid)
    case Imm(v)      => "#" + v.toString()
    case ASR(rm, imm) => registerName(rm) + ", asr " + operandName(imm)
    case LSL(rm, imm) => registerName(rm) + ", lsl " + operandName(imm)
  }

  def registerName(rid: RId): String = rid.id match {
    case 11 => "fp"
    case 13 => "sp"
    case 14 => "lr"
    case 15 => "pc"
    case _  => "r" + rid.id.toString()
  }
}
