package wacc.decompiler
import wacc.back.IR._

import scala.util.matching.Regex

object ArmToIR {
  import RegexPatterns._
  import Operand2Regex._
  def convertAllToIR(armCode: String): List[Instruction] = {
    val armLines: List[String] = armCode.split("\\n").toList
    combineIR(noneFilter(armLines.map(singleArmToIR)), 0)
  }

  def noneFilter(list: List[Option[Instruction]]): List[Instruction] = list match{
    case (r1 :: rest) =>
      if (!r1.isEmpty) {
        r1.get :: noneFilter(rest)
      } else {
        noneFilter(rest)
      }
    case Nil => List.empty
  }

  def combineIR(IRs: List[Instruction], index: Int): List[Instruction] = {
    if (index >= IRs.length) {
      return List.empty
    }
    if (index >= IRs.length - 2 && index < IRs.length) {
      return IRs.slice(index, IRs.length)
    }
    IRs(index) match {
      case Cmp(rn, op2) =>
        IRs(index + 1) match {
          case MovLT(rd, Imm(1)) =>
            IRs(index + 2) match {
              case MovGE(rd, Imm(0)) => LTIns(rd, rn, op2) :: combineIR(IRs, index + 3)
              case _ =>  IRs(index) :: IRs(index + 1) :: IRs(index + 2) :: combineIR(IRs, index + 3)
            }
          case MovLE(rd, Imm(1)) =>
            IRs(index + 2) match {
              case MovGT(rd, Imm(0)) => LEIns(rd, rn, op2) :: combineIR(IRs, index + 3)
              case _ => IRs(index) :: IRs(index + 1) :: IRs(index + 2) :: combineIR(IRs, index + 3)
            }
          case MovGT(rd, Imm(1)) =>
            IRs(index + 2) match {
              case MovLE(rd, Imm(0)) => GTIns(rd, rn, op2) :: combineIR(IRs, index + 3)
              case _ => IRs(index) :: IRs(index + 1) :: IRs(index + 2) :: combineIR(IRs, index + 3)
            }
          case MovGE(rd, Imm(1)) =>
            IRs(index + 2) match {
              case MovLT(rd, Imm(0)) => GEIns(rd, rn, op2) :: combineIR(IRs, index + 3)
              case _ => IRs(index) :: IRs(index + 1) :: IRs(index + 2) :: combineIR(IRs, index + 3)
            }
          case MovEq(rd, Imm(1)) =>
            IRs(index + 2) match {
              case MovNeq(rd, Imm(0)) => EqIns(rd, rn, op2) :: combineIR(IRs, index + 3)
              case _ => IRs(index) :: IRs(index + 1) :: IRs(index + 2) :: combineIR(IRs, index + 3)
            }
          case MovNeq(rd, Imm(1)) =>
            IRs(index + 2) match {
              case MovEq(rd, Imm(0)) => EqIns(rd, rn, op2) :: combineIR(IRs, index + 3)
              case _ => IRs(index) :: IRs(index + 1) :: IRs(index + 2) :: combineIR(IRs, index + 3)
            }
          case _ => IRs(index) :: IRs(index + 1) :: combineIR(IRs, index + 2)
      }
      case _ => IRs(index) :: combineIR(IRs, index + 1)
    }
  }

  def singleArmToIR(armLine: String): Option[Instruction] = armLine match {
    case commentReg() => None
    case dotDataReg() => Some(DotData)
    case dotTextReg() => Some(DotText)
    case dotGlobalReg(tag) => Some(DotGlobal(tag))
    case labelReg(name) => Some(Label(name))
    case movReg(rd, op2) => Some(Mov(getRId(rd), stringToOperand2(op2)))
    case popReg(regs) => Some(Pop(getAllRegs(regs)))
    case pushReg(regs) => Some(Push(getAllRegs(regs)))
    case blReg(name) => Some(Bl(name))
    case addInsReg(rn, rm, op2) => Some(AddIns(getRId(rn), getRId(rm), stringToOperand2(op2)))
    case subInsReg(rn, rm, op2) => Some(SubIns(getRId(rn), getRId(rm), stringToOperand2(op2)))
    case rsbsReg(rn, rm, op2) => Some(Rsbs(getRId(rn), getRId(rm), stringToOperand2(op2)))
    case mulInsReg(r1, r2, r3, r4) => Some(MulIns(getRId(r1), getRId(r3), getRId(r2)))
    case strImmOffset(r1, r2, offset) => Some(Store(getRId(r1), ImmOffset(getRId(r2), Imm(toInteger(offset)))))
    case strPreIndexed(r1, r2, offset) => Some(Store(getRId(r1), PreIndexed(getRId(r2), Imm(toInteger(offset)))))
    case strLSLOffset(r1, r2, r3, offset) => Some(Store(getRId(r1), LSLOffset(getRId(r2), getRId(r3), Imm(toInteger(offset)))))
    case strbPreIndexed(r1, r2, offset) => Some(StoreByte(getRId(r1), PreIndexed(getRId(r2), Imm(toInteger(offset)))))
    case strbImmOffset(r1, r2, offset) => Some(StoreByte(getRId(r1), ImmOffset(getRId(r2), Imm(toInteger(offset)))))
    case strbLSLOffset(r1, r2, r3, offset) => Some(StoreByte(getRId(r1), LSLOffset(getRId(r2), getRId(r3), Imm(toInteger(offset)))))
    case loadConstant(r1, constant) => Some(Load(getRId(r1), Constant(constant)))
    case loadImmOffset(r1, r2, offset) => Some(Load(getRId(r1), ImmOffset(getRId(r2), Imm(toInteger(offset)))))
    case loadPreIndexed(r1, r2, offset) => Some(Load(getRId(r1), PreIndexed(getRId(r2), Imm(toInteger(offset)))))
    case loadLSLOffset(r1, r2, r3, offset) => Some(Load(getRId(r1), LSLOffset(getRId(r2), getRId(r3), Imm(toInteger(offset)))))
    case loadByte(r1, r2, r3, offset) => Some(LoadByte(getRId(r1), LSLOffset(getRId(r2), getRId(r3), Imm(toInteger(offset)))))
    case loadSignedByte(r1, r2, offset) => Some(LoadSignedByte(getRId(r1), ImmOffset(getRId(r2), Imm(toInteger(offset)))))
    case beqReg(label) => Some(Beq(label))
    case bneReg(label) => Some(Bne(label))
    case bleqReg(label) => Some(Bleq(label))
    case blvsReg(label) => Some(Blvs(label))
    case blltReg(label) => Some(Bllt(label))
    case blneReg(label) => Some(Blne(label))
    case cmpReg(r1, op2) => Some(Cmp(getRId(r1), stringToOperand2(op2)))
    case branchReg(label) => Some(Branch(label))
    case movLTReg(r1, op2) => Some(MovLT(getRId(r1), stringToOperand2(op2)))
    case movLEReg(r1, op2) => Some(MovLE(getRId(r1), stringToOperand2(op2)))
    case movGTReg(r1, op2) => Some(MovGT(getRId(r1), stringToOperand2(op2)))
    case movGEReg(r1, op2) => Some(MovGE(getRId(r1), stringToOperand2(op2)))
    case movEqReg(r1, op2) => Some(MovEq(getRId(r1), stringToOperand2(op2)))
    case movNeReg(r1, op2) => Some(MovNeq(getRId(r1), stringToOperand2(op2)))
    case dotAscizReg(string) => Some(DotAsciz(string))
    case dotWordReg(size) => Some(DotWord(toInteger(size)))
    case andInsReg(r1, r2, r3) => Some(AndIns(getRId(r1), getRId(r2), getRId(r3)))
    case orInsReg(r1, r2, r3) => Some(AndIns(getRId(r1), getRId(r2), getRId(r3)))
    case customizeReg(string) => Some(Customize(string))
    case v => 
      println(v+ "\n")
      throw new Exception("Illegal ARM instruction!" + v)
  }

  def stringToOperand2(name: String): Operand2 = name match {
    case registerNameReg(rn) => getRId(rn)
    case ImmReg(offset) => Imm(offset.toInt)
    case ASRReg(rn, offset) => ASR(getRId(rn), Imm(offset.toInt))
    case LSLReg(rn, offset) => LSL(getRId(rn), Imm(offset.toInt))
    case _ => throw new Exception("Illegal Operand2!")
  }

  def getRId(name: String): RId = name match {
    case registerNameReg(rn) =>
      rn match {
        case "fp" => RId(11)
        case "sp" => RId(13)
        case "lr" => RId(14)
        case "pc" => RId(15)
        case _  => RId(name.tail.toInt)
      }
    case _ => throw new Exception("Illegal register name")
  }

  def toInteger(name: String): Int = {
    try{
      name.toInt
    } catch {
      case e: Exception => throw new Exception("This is not an valid Integer!")
    }
  }

  def getAllRegs(regs: String): List[RId] = regs match {
    case registerListReg(rn, rest) => getRId(rn) :: getAllRegs(rest)
    case registerNameReg(rn) => List(getRId(rn))
    case _ => throw new Exception("Illegal list of registers!")
  }
}

// A list of regular expression patterns
private object RegexPatterns{
  val dotDataReg: Regex = " *.data".r
  val dotTextReg: Regex = " *.text".r
  val dotGlobalReg: Regex = " *.global +([a-zA-Z0-9]+)".r
  val labelReg: Regex = " *(.+):".r
  val movReg: Regex = " *mov (fp|sp|lr|pc|r\\d+), +(.+)".r
  val popReg: Regex = " *pop +\\{(.+)}".r
  val pushReg: Regex = " *push +\\{(.+)}".r
  val blReg: Regex = " *bl +(.+)".r
  val addInsReg: Regex = " *adds +(.+), +(.+), +(.+)".r
  val subInsReg: Regex = " *subs +(.+), +(.+), +(.+)".r
  val rsbsReg: Regex = " *rsbs +(.+), +(.+), +(.+)".r
  val mulInsReg: Regex = " *smull +(.+), +(.+), +(.+), +(.+)".r
  val strImmOffset: Regex = " *str +(fp|sp|lr|pc|r\\d+), +\\[(fp|sp|lr|pc|r\\d+), +#([+|-]?\\d+)]".r
  val strPreIndexed: Regex = " *str +(fp|sp|lr|pc|r\\d+), +\\[(fp|sp|lr|pc|r\\d+), +#([+|-]?\\d+)]!".r
  val strLSLOffset: Regex = " *str +(fp|sp|lr|pc|r\\d+), +\\[(fp|sp|lr|pc|r\\d+), +(fp|sp|lr|pc|r\\d+), +lsl #([+|-]?\\d+)]".r
  val strbPreIndexed: Regex = " *strb (fp|sp|lr|pc|r\\d+), +\\[(fp|sp|lr|pc|r\\d+), +#([+|-]?\\d+)]!".r
  val strbImmOffset: Regex = " *strb (fp|sp|lr|pc|r\\d+), +\\[(fp|sp|lr|pc|r\\d+), +#([+|-]?\\d+)]".r
  val strbLSLOffset: Regex = " *strb (fp|sp|lr|pc|r\\d+), +\\[(fp|sp|lr|pc|r\\d+), +(fp|sp|lr|pc|r\\d+), +lsl #([+|-]?\\d+)]".r
  val loadConstant: Regex = " *ldr (fp|sp|lr|pc|r\\d+), +=(.+)".r
  val loadImmOffset: Regex = " *ldr (fp|sp|lr|pc|r\\d+), +\\[(fp|sp|lr|pc|r\\d+), #([+|-]?\\d+)]".r
  val loadPreIndexed: Regex = " *ldr (fp|sp|lr|pc|r\\d+), +\\[(fp|sp|lr|pc|r\\d+), #([+|-]?\\d+)]!".r
  val loadLSLOffset: Regex = " *ldr (fp|sp|lr|pc|r\\d+), +\\[(fp|sp|lr|pc|r\\d+), +(fp|sp|lr|pc|r\\d+), +lsl #([+|-]?\\d+)]".r
  val loadByte: Regex = " *ldrb +(fp|sp|lr|pc|r\\d+), +\\[(fp|sp|lr|pc|r\\d+), +(fp|sp|lr|pc|r\\d+), +lsl #([+|-]?\\d+)]".r
  val loadSignedByte: Regex = " *ldrsb (fp|sp|lr|pc|r\\d+), +\\[(fp|sp|lr|pc|r\\d+), +#([+|-]?\\d+)]".r
  val beqReg: Regex = " *beq (.+)".r
  val bneReg: Regex = " *bne (.+)".r
  val customizeReg: Regex = "  *(.+)".r
  val bleqReg: Regex = " *bleq (.+)".r
  val blvsReg: Regex = " *blvs (.+)".r
  val blltReg: Regex = " *bllt (.+)".r
  val blgeReg: Regex = " *blge (.+)".r
  val blneReg: Regex = " *blne (.+)".r
  val cmpReg: Regex = " *cmp +(fp|sp|lr|pc|r\\d+), +(.+)".r
  val branchReg: Regex = " *b +(.+)".r
  val movLTReg: Regex = " *movlt +(fp|sp|lr|pc|r\\d+), +(.+)".r
  val movLEReg: Regex = " *movle +(fp|sp|lr|pc|r\\d+), +(.+)".r
  val movGTReg: Regex = " *movgt +(fp|sp|lr|pc|r\\d+), +(.+)".r
  val movGEReg: Regex = " *movge +(fp|sp|lr|pc|r\\d+), +(.+)".r
  val movEqReg: Regex = " *moveq +(fp|sp|lr|pc|r\\d+), +(.+)".r
  val movNeReg: Regex = " *movne +(fp|sp|lr|pc|r\\d+), +(.+)".r
  val dotAscizReg: Regex = """ *.asciz (".+")""".r
  val dotWordReg: Regex = " *.word (\\d+)".r
  val andInsReg: Regex = " *and +(fp|sp|lr|pc|r\\d+), +(fp|sp|lr|pc|r\\d+), +(fp|sp|lr|pc|r\\d+)".r
  val orInsReg: Regex = " *and +(fp|sp|lr|pc|r\\d+), +(fp|sp|lr|pc|r\\d+), +(fp|sp|lr|pc|r\\d+)".r
  val commentReg: Regex = " *\\@.*".r
}


private object Operand2Regex {
  val registerNameReg: Regex = "(fp|sp|lr|pc|r\\d+)".r
  val ImmReg: Regex = "#([+|-]?\\d+)".r
  val ASRReg: Regex = "(fp|sp|lr|pc|r\\d+), asr #([+|-]?\\d+)".r
  val LSLReg: Regex = "(fp|sp|lr|pc|r\\d+), lsl #([+|-]?\\d+)".r
  val registerListReg: Regex = "(fp|sp|lr|pc|r\\d+), (.+)".r

}