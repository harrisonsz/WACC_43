package wacc.front

object TypeChecker {
  import wacc.front.SemanticChecker._
  import wacc.front.SymbolTable.SymbolTable
  import wacc.front.ast._
  import scala.collection.mutable.ListBuffer

  def twoTypesAreSame(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (ArrayType(t1), ArrayType(t2))           => twoTypesAreSame(t1, t2)
    // PairType(Null, Null) means the pair elements types are missing so it can match to any PairType()
    case (PairType(Null, Null), PairType(_,_))    => true
    case (PairType(_,_), PairType(Null, Null))    => true
    case (PairType(lt1, rt1), PairType(lt2, rt2)) => twoPairElemTypesAreSame(lt1, lt2) && twoPairElemTypesAreSame(rt1, rt2)
    case (PairType(_,_), Null)                    => true
    case (Null, PairType(_,_))                    => true
    case (t1, t2)                                 => t1 == t2
  }

  // Check if lv and rv have the same type, if not adding error messages to errorLogs
  def leftRightSameType(lv: Lvalue, rv: Rvalue, curTable: SymbolTable, assign: Assign, errorlogs: ListBuffer[((Int, Int), String)]): Unit = {
    val leftType = getLeftValueType(lv, curTable)
    if (leftType.isDefined) {
      rValueIsType(leftType.get, rv, curTable)
    } else {
      rv match {
        case Snd(Fst(Ident(_))) | Fst(Snd(Ident(_))) | Fst(Fst(Ident(_))) | Snd(Snd(Ident(_))) =>
          errorLogs.addOne((assign.pos, "Attempting to exchange values between pairs of unknown types\n" +
            "Pair exchange is only legal when the type of at least one of the sides is known or specified"))
        case _ =>
      }
    }
  }

  // Get the type of a variable,
  // it also generates error message is the given identifier is not in the symbol table or it is not a variable
  def getIdentiferType(identifier: Ident, symbolName: String, curTable: SymbolTable): Option[Type] = {
    import IdentifierObject._
    val identObject = curTable.lookupAll(symbolName)
    if (identObject.isEmpty) {
      errorLogs.addOne(identifier.pos,
        "Variable " + symbolName + " has not been defined yet")
      None
    } else if (identObject.get.isInstanceOf[VarObject]) {
      Some(identObject.get.asInstanceOf[VarObject].typ)
    } else {
      errorLogs.addOne(identifier.pos, "Identifier " + symbolName + " is not a variable name")
      None
    }
  }

  // Check if the expr has the same type as the curTable, return true if it is
  def matchReturnType(expr: Expr, curTable: SymbolTable): Boolean = {
    validExpr(expr, curTable)
    val exprType = getExpType(expr, curTable)
    val returnType = getIdentiferType(new Ident("return")(expr.getPosition()), "return", curTable)
    if (exprType.isDefined && exprType.isDefined) {
      return twoTypesAreSame(exprType.get, returnType.get)
    } else {
      return exprType == returnType
    }
  }

  // Gets the type of an expression, the returned type is optional, and it does not produce error log
  def getExpType(exp: Expr, curTable: SymbolTable) : Option[Type] = {
    exp match {
      case IntLiter(v) => Some(IntType)
      case CharLiter(v) => Some(CharType)
      case BoolLiter(v) => Some(BoolType)
      case StrLiter(v) => Some(StringType)
      case Ident(symbolName) =>
        val typ = getIdentiferType(exp.asInstanceOf[Ident], symbolName, curTable)
        if (typ.isEmpty) {
          None
        } else {
          typ
        }
      case ArrayElem(id, expressions) =>
        val typ = getIdentiferType(id, id.v, curTable)
        if (typ.isEmpty) {
          None
        } else {
          var result = Some(typ.get.asInstanceOf[ArrayType].t)
          for (i <- 2 to expressions.size) {
            result = Some(result.get.asInstanceOf[ArrayType].t)
          }
          return result
        }
      case Mul(exp1, exp2) => Some(IntType)
      case Div(exp1, exp2) => Some(IntType)
      case Mod(exp1, exp2) => Some(IntType)
      case Add(exp1, exp2) => Some(IntType)
      case Sub(exp1, exp2) => Some(IntType)
      case NegUOp(exp) => Some(IntType)
      case LenUOp(exp) => Some(IntType)
      case OrdUOp(exp) => Some(IntType)
      case ChrUOp(exp) => Some(CharType)
      case PairLiter() => Some(Null)
      case _ => Some(BoolType)
    }
  }

  // compares the types of two expression, it generates error log
  def sameType(exp1: ast.Expr, exp2: ast.Expr, curTable: SymbolTable) : Unit = {
    val typ1 = getExpType(exp1, curTable)
    val typ2 = getExpType(exp2, curTable)
    if (typ1.isDefined && typ2.isDefined) {
      if (pairNullExps(typ1.get, typ2.get)) {
        // skip if one of them is a pair and another is null
      } else if (typ1.get != typ2.get) {
        errorLogs.addOne(exp1.getPosition(), "These two expressions are not the same type")
      }
    }
  }
}
