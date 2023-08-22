package wacc.decompiler.DataFlow

import wacc.decompiler.L2IR._

class IdGenerator {
  var counter = -1

  def getId(): String = {
    counter = counter + 1
    "local" + counter
  }
}
