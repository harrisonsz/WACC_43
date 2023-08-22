package wacc.front

object SymbolTable {
  class SymbolTable(encSymTable: SymbolTable = null) {
    import scala.collection.mutable
    import wacc.front.IdentifierObject._

    // It stores all mapping in this symbol table
    val dict = mutable.Map[String, Identifier]()

    // This is the counter used to allocate the position in stack
    var stackCounter = 0

    def getStackPos(): Int = {
      stackCounter
    }

    def cleanStackCounter(): Unit = {
      stackCounter = 0
    }

    def getAllStackPos(): Int = {
      if (encSymTable == null) {
        stackCounter
      } else {
        stackCounter + encSymTable.getAllStackPos()
      }
    }

    def getAndIncStackPos(): Int = {
      stackCounter += 1
      return getAllStackPos()
    }

    // This is the counter used to allocate the name of scopes. 
    // e.g. There are scopes while, if and sub in parallel and in order, their names
    // in the dict will be while{0}, if{1} and sub{2}
    var scopeCounter = -1

    // Return the string of a brand new number for a scope and increase the counter
    def getNum(): String = {
      scopeCounter += 1
      return scopeCounter.toString()
    }

    // This function should only be called in the function clearAllScopeCounters() which will only be called at the start of generateIR()
    def clearScopeCounter(): Unit = {
      scopeCounter = -1
    }

    // Defining this function to encapsulate the class
    def getEncSymTable() = encSymTable

    // Add the mapping (name -> i) into the this symbol table
    def add(name: String, obj: Identifier): Unit = {
      dict.addOne((name, obj))
    }

    // Remove the mapping (name -> i) by name
    def remove(name: String): Unit = {
      dict.remove(name)
    }

    // Search if name exists in this symbol table, if it does,
    // return Some(value), None otherwise
    // [<name>] refer to parameters
    def lookup(name: String): Option[Identifier] = {
        dict.get(name).orElse(dict.get("["+name+"]"))
    }

    // Search up until finish the rearching of the root symbol table
    // and return None or find the name in any table and return Some(value)
    def lookupAll(name: String): Option[Identifier] = {
      var s = this
      while (s != null) {
        val obj = s.lookup(name)
        if (obj.isDefined) {
          return obj
        }
        s = s.getEncSymTable()
      }
      None
    }

    def lookupAllInIRCheck(name: String): Option[Identifier] = {
      var s = this
      while (s != null) {
        val obj = s.lookup(name)
        if (obj.isDefined) {
          if (obj.get.isInstanceOf[VarObject]) {
            if (obj.get.asInstanceOf[VarObject].checkedIR == true) {
              return obj
            }
          } else {
            return obj
          }
        }
        s = s.getEncSymTable()
      }
      None
    }

    def lookUpFunc(funcParam: String) : Boolean = {
      dict.contains("[" + funcParam + "]")
    }
  }
}