package wacc.front

object IdentifierObject {
    import SemanticChecker._
    import SymbolTable._
    import ast._
    import wacc.back.IR.RId

    sealed trait Identifier

    class VarObject(val typ: Type) extends Identifier {
      var checkedIR = false
      var regPos: Option[RId] = None // records which register this variable is stored at
      var stackPos: Option[Int] = None // records the relative pos to the FP where this variable is stored
    }

    class FuncObject(function: Func, parent: SymbolTable) extends Identifier {
      val funcName = function.id.v
      val returnType = function.t
      val paraList = function.params
      var symbolTable: SymbolTable = new SymbolTable(parent)
      symbolTable.add("return", new VarObject(returnType))
      // add all parameters to symbol table
      for (parameter: Param <- paraList) {
        // Functions cannot define the same argument twice
        if (symbolTable.lookup("[" + parameter.id.v + "]").isDefined) {
          errorLogs.addOne((parameter.pos, "Functions cannot define the same argument twice"))
        }
        symbolTable.add("[" + parameter.id.v + "]", new VarObject(parameter.t))
      }
    }

    class ProgObject(val program: Program) extends Identifier {
      var symbolTable: SymbolTable = new SymbolTable()

      // add all function names to the symbol table
      for (func: Func <- program.funcs) {
        val thisFuncObj = new FuncObject(func, symbolTable)
        if (symbolTable.lookupAll("(" + thisFuncObj.funcName + ")").isDefined) {
          errorLogs.addOne((func.pos, "illegal redefinition of function " + thisFuncObj.funcName))
        }
        symbolTable.add("(" + thisFuncObj.funcName + ")", thisFuncObj)
      }
    }

    class IfObject(val ifStat: If, parent: SymbolTable) extends Identifier {
      var ifSymbolTable: SymbolTable = new SymbolTable(parent)
      var elseSymbolTable: SymbolTable = new SymbolTable(parent)
      if (ifStat.exprFalse.isEmpty) {
        elseSymbolTable = new SymbolTable()
      }
    }

    class WhileObject(val whileStat: While, parent: SymbolTable) extends Identifier {
      var symbolTable: SymbolTable = new SymbolTable(parent)
    }

    class SubProgObject(val subStat: SubProgram, parent: SymbolTable) extends Identifier {
      var symbolTable: SymbolTable = new SymbolTable(parent)
    }
  }


