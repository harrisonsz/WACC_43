package wacc.decompiler.DataFlow

import scala.collection.mutable.HashMap
import wacc.back.IR._

class SimpleSymbolTable {
  val symbolTable: HashMap[RId, String] = HashMap.empty

  def add(key: RId, value: String): Unit = {
    symbolTable.addOne(key, value)
  }

  def get(key: RId): Option[String] = {
    symbolTable.get(key)
  }

  def contains(key: RId): Boolean = {
    symbolTable.contains(key)
  }
}
