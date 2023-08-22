package wacc.back

object Peephole {
  import IR._

  def peepholeOptimize(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case (push@Push(pushList)) :: rest => // remove redundant push pop
        rest match {
          case (pop@Pop(popList)) :: rest =>
            if (popList.equals(pushList)) {
              peepholeOptimize(rest)
            } else {
              push :: pop :: peepholeOptimize(rest)
            }
          case _ =>
            Push(pushList) :: peepholeOptimize(rest)
        }
      case (add@AddIns(rd, r1@RId(id1), r2)) :: rest => // x = a + a -> x = a << 1
        r2 match {
          case RId(id2) => 
            if (id1 == id2) {
              Mov(rd, LSL(r1, Imm(1))) :: peepholeOptimize(rest)
            } else {
              add :: peepholeOptimize(rest)
            }
          case _ =>
            add :: peepholeOptimize(rest)
        }
      case head :: rest =>
        head :: peepholeOptimize(rest)
      case Nil => 
        List.empty
    }
  }
}
