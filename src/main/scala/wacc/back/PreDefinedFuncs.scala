package wacc.back

import wacc.back.IRGenerator._

object PreDefinedFunctions {
  sealed trait PreDefinedFunc
  case object ReadInt        extends PreDefinedFunc
  case object ReadChar       extends PreDefinedFunc
  case object PrintInt       extends PreDefinedFunc
  case object PrintChar      extends PreDefinedFunc
  case object PrintBool      extends PreDefinedFunc
  case object PrintString    extends PreDefinedFunc
  case object PrintPair      extends PreDefinedFunc
  case object PrintNewLine   extends PreDefinedFunc
  case object ErrorDivZero   extends PreDefinedFunc
  case object ErrorOverFlow  extends PreDefinedFunc
  case object ArrayStore     extends PreDefinedFunc
  case object ArrayStoreByte extends PreDefinedFunc
  case object ArrayLoad      extends PreDefinedFunc
  case object ArrayLoadByte  extends PreDefinedFunc
  case object BoundCheck     extends PreDefinedFunc
  case object ErrorNull      extends PreDefinedFunc
  case object FreePair       extends PreDefinedFunc
}

object FunctionsPool {

  import IR._
  import PreDefinedFunctions._
  import RegOperator._

  import scala.collection.mutable

  val allPreDefinedFuncs: List[PreDefinedFunc] = List(ReadInt, ReadChar, PrintInt, PrintChar, PrintBool, PrintString, PrintPair, PrintNewLine,
    ErrorDivZero, ErrorOverFlow, ArrayStore, ArrayStoreByte, ArrayLoad, ArrayLoadByte, BoundCheck, ErrorNull, FreePair)

  val dict = allPreDefinedFuncs.foldLeft(mutable.Map.empty[PreDefinedFunc, Boolean]) { (m, f) => m.addOne(f -> false) }

  def addingFunc(func: PreDefinedFunc): Unit = {
    func match { // Some pre-defined functions call other pre-defined functions
      case ErrorDivZero => addingFunc(PrintString)
      case ErrorOverFlow => addingFunc(PrintString)
      case ErrorNull => addingFunc(PrintString)
      case ArrayLoad => addingFunc(BoundCheck)
      case ArrayLoadByte => addingFunc(BoundCheck)
      case ArrayStore => addingFunc(BoundCheck)
      case ArrayStoreByte => addingFunc(BoundCheck)
      case FreePair => addingFunc(ErrorNull)
      case _ =>
    }
    dict(func) = true
  }

  def addingDefOfFuncs(): List[Instruction] = {
    dict.filter(_._2).foldLeft(List.empty[Instruction]) { (all, p) => all ::: defOfFunc(p._1) }
  }

  def defOfFunc(func: PreDefinedFunc): List[Instruction] = func match {
    case PrintInt => defOfPrinti()
    case PrintChar => defOfPrintc()
    case PrintBool => defOfPrintb()
    case PrintPair => defOfPrintp()
    case PrintString => defOfPrints()
    case PrintNewLine => defOfPrintln()
    case ReadInt => defOfReadi()
    case ReadChar => defOfReadc()
    case FreePair => defOfFreePair()
    case ErrorOverFlow => defOfErrOverflow()
    case ErrorNull => defOferrNull()
    case ErrorDivZero => defOfErrDivZero()
    case ArrayLoad => defOfArrLoad()
    case ArrayLoadByte => defOfArrLoadB()
    case ArrayStore => defOfArrStore()
    case ArrayStoreByte => defOfArrStoreB()
    case BoundCheck => defOfBoundsCheck()
  }

  // It returns the definition of readi which is used to read integer from the input
  def defOfReadi(): List[Instruction] = {
    DotData ::
      DotWord(2) ::
      Label(".L._readi_str0") ::
      DotAsciz("%d") ::
      DotText ::
      Label("_readi") ::
      Push(List(RId(LR))) ::
      Store(RId(0), PreIndexed(RId(SP), Imm(-4))) ::
      Mov(RId(1), RId(SP)) ::
      Load(RId(0), Constant(".L._readi_str0")) ::
      Bl("scanf") ::
      Load(RId(0), ImmOffset(RId(SP), Imm(0))) ::
      AddIns(RId(SP), RId(SP), Imm(4)) ::
      Pop(List(RId(PC))) ::
      List.empty
  }

  // It returns the definition of readc which is used to read character from the input
  def defOfReadc(): List[Instruction] = {
    DotData ::
      DotWord(3) ::
      Label(".L._readc_str0") ::
      DotAsciz(" %c") ::
      DotText ::
      Label("_readc") ::
      Push(List(RId(LR))) ::
      StoreByte(RId(0), PreIndexed(RId(SP), Imm(-1))) ::
      Mov(RId(1), RId(SP)) ::
      Load(RId(0), Constant(".L._readc_str0")) ::
      Bl("scanf") ::
      LoadSignedByte(RId(0), ImmOffset(RId(SP), Imm(0))) ::
      AddIns(RId(SP), RId(SP), Imm(1)) ::
      Pop(List(RId(PC))) ::
      List.empty
  }


  // It returns the definition of printi which is used to print the int
  def defOfPrinti(): List[Instruction] = {
    DotData ::
      DotWord(2) ::
      Label(".L._printi_str0") ::
      DotAsciz("%d") ::
      DotText ::
      Label("_printi") ::
      Push(List(RId(LR))) ::
      Mov(RId(1), RId(0)) ::
      Load(RId(0), Constant(".L._printi_str0")) ::
      Bl("printf") ::
      Mov(RId(0), Imm(0)) ::
      Bl("fflush") ::
      Pop(List(RId(PC))) ::
      List.empty
  }

  // It returns the definition of printc which is used to print the char
  def defOfPrintc(): List[Instruction] = {
    DotData ::
      DotWord(2) ::
      Label(".L._printc_str0") ::
      DotAsciz("%c") ::
      DotText ::
      Label("_printc") ::
      Push(List(RId(LR))) ::
      Mov(RId(1), RId(0)) ::
      Load(RId(0), Constant(".L._printc_str0")) ::
      Bl("printf") ::
      Mov(RId(0), Imm(0)) ::
      Bl("fflush") ::
      Pop(List(RId(PC))) ::
      List.empty
  }

  def defOfPrintb(): List[Instruction] = {
    DotData ::
      DotWord(5) ::
      Label(".L._printb_str0") ::
      DotAsciz("false") ::
      DotWord(4) ::
      Label(".L._printb_str1") ::
      DotAsciz("true") ::
      DotWord(4) ::
      Label(".L._printb_str2") ::
      DotAsciz("%.*s") ::
      DotText ::
      Label("_printb") ::
      Push(List(RId(LR))) ::
      Cmp(RId(0), Imm(0)) ::
      Bne(".L_printb0") ::
      Load(RId(2), Constant(".L._printb_str0")) ::
      Branch(".L_printb1") ::
      Label(".L_printb0") ::
      Load(RId(2), Constant(".L._printb_str1")) ::
      Label(".L_printb1") ::
      Load(RId(1), ImmOffset(RId(2), Imm(-4))) ::
      Load(RId(0), Constant(".L._printb_str2")) ::
      Bl("printf") ::
      Mov(RId(0), Imm(0)) ::
      Bl("fflush") ::
      Pop(List(RId(PC))) ::
      List.empty
  }

  // It returns the definition of prints which can print a string
  def defOfPrints(): List[Instruction] = {
    DotData ::
      DotWord(4) ::
      Label(".L._prints_str0") ::
      DotAsciz("%.*s") ::
      DotText ::
      Label("_prints") ::
      Push(List(RId(LR))) ::
      Mov(RId(2), RId(0)) ::
      Load(RId(1), ImmOffset(RId(0), Imm(-4))) ::
      Bl("printf") ::
      Mov(RId(0), Imm(0)) ::
      Bl("fflush") ::
      Pop(List(RId(PC))) ::
      List.empty
  }


  // It returns the definition of printp which is used to print the non-char array and pair types
  def defOfPrintp(): List[Instruction] = {
    DotData ::
      DotWord(2) ::
      Label(".L._printp_str0") ::
      DotAsciz("%p") ::
      DotText ::
      Label("_printp") ::
      Push(List(RId(LR))) ::
      Mov(RId(1), RId(0)) ::
      Load(RId(0), Constant(".L._printp_str0")) ::
      Bl("printf") ::
      Mov(RId(0), Imm(0)) ::
      Bl("fflush") ::
      Pop(List(RId(PC))) ::
      List.empty
  }

  // It returns the definition of println
  def defOfPrintln(): List[Instruction] = {
    DotData ::
      DotWord(0) ::
      Label(".L._println_str0") ::
      DotAsciz("") ::
      DotText ::
      Label("_println") ::
      Push(List(RId(LR))) ::
      Load(RId(0), Constant(".L._println_str0")) ::
      Bl("puts") ::
      Mov(RId(0), Imm(0)) ::
      Bl("fflush") ::
      Pop(List(RId(PC))) ::
      List.empty
  }

  def defOfErrDivZero(): List[Instruction] = {
    DotData ::
      DotWord(40) ::
      Label(".L._errDivZero_str0") ::
      DotAsciz("fatal error: division or modulo by zero") ::
      DotText ::
      Label("_errDivZero") ::
      Load(RId(0), Constant(".L._errDivZero_str0")) ::
      Bl("_prints") ::
      Mov(RId(0), Imm(255)) ::
      Bl("exit") ::
      List.empty
  }

  def defOfErrOverflow(): List[Instruction] = {
    DotData ::
      DotWord(52) ::
      Label(".L._errOverflow_str0") ::
      DotAsciz("fatal error: integer overflow or underflow occurred") ::
      DotText ::
      Label("_errOverflow") ::
      Load(RId(0), Constant(".L._errOverflow_str0")) ::
      Bl("_prints") ::
      Mov(RId(0), Imm(255)) ::
      Bl("exit") ::
      List.empty
  }

  def defOfArrStore(): List[Instruction] = {
    DotText ::
      Label("_arrStore") ::
      Push(List(RId(LR))) ::
      Cmp(RId(10), Imm(0)) ::
      MovLT(RId(1), RId(10)) ::
      Bllt("_boundsCheck") ::
      Load(RId(LR), ImmOffset(RId(3), Imm(-4))) ::
      Cmp(RId(10), RId(LR)) ::
      MovGE(RId(1), RId(10)) ::
      Blge("_boundsCheck") ::
      Store(RId(8), LSLOffset(RId(3), RId(10), Imm(2))) ::
      Pop(List(RId(PC))) ::
      List.empty
  }

  def defOfArrStoreB(): List[Instruction] = {
    DotText ::
      Label("_arrStoreB") ::
      Push(List(RId(LR))) ::
      Cmp(RId(10), Imm(0)) ::
      MovLT(RId(1), RId(10)) ::
      Bllt("_boundsCheck") ::
      Load(RId(LR), ImmOffset(RId(3), Imm(-4))) ::
      Cmp(RId(10), RId(LR)) ::
      MovGE(RId(1), RId(10)) ::
      Blge("_boundsCheck") ::
      StoreByte(RId(8), LSLOffset(RId(3), RId(10), Imm(0))) ::
      Pop(List(RId(PC))) ::
      List.empty
  }

  def defOfArrLoadB(): List[Instruction] = {
    DotText ::
      Label("_arrLoadB") ::
      Push(List(RId(LR))) ::
      Cmp(RId(10), Imm(0)) ::
      MovLT(RId(1), RId(10)) ::
      Bllt("_boundsCheck") ::
      Load(RId(LR), ImmOffset(RId(3), Imm(-4))) ::
      Cmp(RId(10), RId(LR)) ::
      MovGE(RId(1), RId(10)) ::
      Blge("_boundsCheck") ::
      LoadByte(RId(3), LSLOffset(RId(3), RId(10), Imm(0))) ::
      Pop(List(RId(PC))) ::
      List.empty
  }

  def defOfArrLoad(): List[Instruction] = {
    DotText ::
      Label("_arrLoad") ::
      Push(List(RId(LR))) ::
      Cmp(RId(10), Imm(0)) ::
      MovLT(RId(1), RId(10)) ::
      Bllt("_boundsCheck") ::
      Load(RId(LR), ImmOffset(RId(3), Imm(-4))) ::
      Cmp(RId(10), RId(LR)) ::
      MovGE(RId(1), RId(10)) ::
      Blge("_boundsCheck") ::
      Load(RId(3), LSLOffset(RId(3), RId(10), Imm(2))) ::
      Pop(List(RId(PC))) ::
      List.empty
  }

  def defOfBoundsCheck(): List[Instruction] = {
    DotData ::
      DotWord(42) ::
      Label(".L._boundsCheck_str0") ::
      DotAsciz("fatal error: array index %d out of bounds") ::
      DotText ::
      Label("_boundsCheck") ::
      Load(RId(0), Constant(".L._boundsCheck_str0")) ::
      Bl("printf") ::
      Mov(RId(0), Imm(0)) ::
      Bl("fflush") ::
      Mov(RId(0), Imm(255)) ::
      Bl("exit") ::
      List.empty
  }

  def defOferrNull(): List[Instruction] = {
    DotData ::
      DotWord(45) ::
      Label(".L._errNull_str0") ::
      DotAsciz("fatal error: null pair dereferenced or freed \\n") ::
      DotText ::
      Label("_errNull") ::
      Load(RId(0), Constant(".L._errNull_str0")) ::
      Bl("_prints") ::
      Mov(RId(0), Imm(255)) ::
      Bl("exit") ::
      List.empty
  }

  def defOfFreePair(): List[Instruction] = {
    Label("_freepair") ::
      Push(List(RId(LR))) ::
      Cmp(RId(0), Imm(0)) ::
      Bleq("_errNull") ::
      Mov(RId(1), RId(0)) ::
      allCallerSave() ::
      Load(RId(0), ImmOffset(RId(1), Imm(0))) ::
      Bl("free") ::
      allCallerRestore() ::
      allCallerSave() ::
      Load(RId(0), ImmOffset(RId(1), Imm(4))) ::
      Bl("free") ::
      allCallerRestore() ::
      allCallerSave() ::
      Mov(RId(0), RId(1)) ::
      allCallerRestore() ::
      Bl("free") ::
      Pop(List(RId(PC))) ::
      List.empty
  }
}
