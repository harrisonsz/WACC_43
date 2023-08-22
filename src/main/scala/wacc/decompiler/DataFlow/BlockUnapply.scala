package wacc.decompiler.DataFlow

import wacc.decompiler.CFgenerate.FlowBlocks._

object BlockUnapply {
  object SingleWayBlock{
    def unapply(block: CFBlock): Option[CFBlock] = block match {
      case OneWayBlock(next) => Some(next)
      case CallBlock(_, nextFall) => Some(nextFall)
      case FallBlock(next) => Some(next)
      case _ => None
    }
  }
}
