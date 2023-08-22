package wacc.front

object IntInfixOperator {
  import ast._
  def unapply(expr: Expr): Option[(Expr, Expr)] = expr match {
    case Add(x, y) => Some((x, y))
    case Sub(x, y) => Some((x, y))
    case Mul(x, y) => Some((x, y))
    case Div(x, y) => Some((x, y))
    case Mod(x, y) => Some((x, y))
    case _ => None
  }
}

object ComparisonOperator {
  import ast._
  def unapply(expr: Expr): Option[(Expr, Expr)] = expr match {
    case Gt(x, y) => Some((x, y))
    case Gte(x, y) => Some((x, y))
    case Lt(x, y) => Some((x, y))
    case Lte(x, y) => Some((x, y))
    case _ => None
  }
}

object SemanticChecker {
  import IdentifierObject._
  import wacc.front.SymbolTable._
  import ast._
  import scala.collection.mutable.ListBuffer
  import wacc.front.TypeChecker._

  // Left two-tuple variable is position and the right string is the error message
  val errorLogs: ListBuffer[((Int, Int), String)] = ListBuffer.empty

  // the "Main" of semantic checker
  def checkingProgram (p: Program): (List[((Int, Int), String)], ProgObject) = {
    errorLogs.clear() //Clearing the information in the errorLogs
    val allFunctions = p.funcs
    val allStats = p.stats
    val wholeProgram = new ProgObject(p)
    allFunctions.foreach(checkingFunction(_, wholeProgram.symbolTable))
    //clear()
    allStats.foreach(checkStat(_, wholeProgram.symbolTable))
    (errorLogs.toList, wholeProgram)
  }

  def checkingFunction(f: Func, parent: SymbolTable): Unit = {
    //clear()
    f.stats.foreach(checkStat(_, parent.lookup("(" + f.id.v + ")").get.asInstanceOf[FuncObject].symbolTable))
  }

  def checkStat (stat: Stat, curTable: SymbolTable): Unit = stat match{
    case Skip                          => //Do nothing

    case Init(t, id, rv)               => checkInit(t, id, rv, curTable)
      

    // assignment must ensure that the value assigned to the identifier is valid
    case assign@Assign(lv, rv)         => leftRightSameType(lv, rv, curTable, assign, errorLogs)

    case Read(lv)                      => readable(lv, curTable)

    case Free(expr)                    => freeable(expr, curTable)

    case Return(expr)                  => 
      if(inMain(curTable)) {
        errorLogs.addOne((stat.asInstanceOf[Return].pos, "The return should not be in the main function"))
      } else if (!matchReturnType(expr, curTable)) {
        errorLogs.addOne((stat.asInstanceOf[Return].pos,
          "The return type should be" + curTable.lookupAll("return").get.asInstanceOf[VarObject].typ.getName()))
      }

    case Exit(expr)                    => rValueIsType(IntType, expr, curTable)

    case Print(expr)                   => validExpr(expr, curTable)

    case Println(expr)                 => validExpr(expr, curTable)

    case If(cond, exprTrue, exprFalse) => 
      rValueIsType(BoolType, cond, curTable)
      val name = "if"+"{"+curTable.getNum()+"}"
      curTable.add(name, new IfObject(stat.asInstanceOf[If], curTable))
      exprTrue.foreach(checkStat(_, curTable.lookup(name).get.asInstanceOf[IfObject].ifSymbolTable))
      exprFalse.foreach(checkStat(_, curTable.lookup(name).get.asInstanceOf[IfObject].elseSymbolTable))

    case While(cond, stats)            => 
      rValueIsType(BoolType, cond, curTable)
      val name = "while"+"{"+curTable.getNum()+"}"
      curTable.add(name, new WhileObject(stat.asInstanceOf[While], curTable))
      stats.foreach(checkStat(_, curTable.lookup(name).get.asInstanceOf[WhileObject].symbolTable))

    case subP @ SubProgram(stats)      => val name = "sub"+"{"+curTable.getNum()+"}"
      curTable.add(name, new SubProgObject(subP, curTable))
      stats.foreach(checkStat(_, curTable.lookup(name).get.asInstanceOf[SubProgObject].symbolTable))
  }

  def checkInit(t: Type, id: Ident, rv: Rvalue, curTable: SymbolTable): Unit = {
    rValueIsType(t, rv, curTable)
    if (curTable.lookup(id.v).isEmpty) { 
      // Checking if the variable has been declared
      curTable.add(id.v, new VarObject(t))
    } else if (curTable.lookup(id.v).isDefined && curTable.lookUpFunc(id.v)) {
      // If the function having the name id.v is existed, we still add the variable 
      // because an argument that has the same name as a function is allowed
      curTable.add(id.v, new VarObject(t))
    } else {
      // Otherwise the variable having the same name is not allowed
      errorLogs.addOne((id.pos, "Variable " + id.v + " has been declared"))
    }
  }

  // Lvalue and Rvalue

  // It returns the type of lv, if the lv is not legal, like the variable is not 
  // defined, error message will be added to the errorLogs, and None is returned.
  // If the type can be any, like (fst fst p), None is returned and no error message
  // is added.
  def getLeftValueType(lv: Lvalue, curTable: SymbolTable): Option[Type] = lv match{
    case Ident(v) => 
      getIdentiferType(lv.asInstanceOf[Ident], v, curTable)

    case ArrayElem(id, expr) => 
      val arrayType = getIdentiferType(id, id.v, curTable)
      if (arrayType.isDefined) {
        arrayType.get match {
          case ArrayType(t) => return Some(t)
          case _            => 
            errorLogs.addOne((id.getPosition(), unexpectedTypeMessage(List("at least 1-dimensional array"))))
        }
      }
      None

    case Fst(id @ Ident(v)) => 
      val pairType = getIdentiferType(id, v, curTable)
      if (pairType.isDefined) {
        pairType.get match {
          case pairT @ PairType(Pair, _) => return Some(PairType(Null, Null)(pairT.pos)) // Nulls are used like a filling
          case PairType(t1, _) => return Some(t1.asInstanceOf[Type])
          case _               => return None // Fst can only be used on pair, the error message is added
                                              // in leftRightSameType()
        }
      }
      return None

    case Snd(id @ Ident(v)) =>
      val pairType = getIdentiferType(id, v, curTable)
      if (pairType.isDefined) {
        pairType.get match {
          case pairT @ PairType(_, Pair) => return Some(PairType(Null, Null)(pairT.pos))
          case PairType(_, t2) => return Some(t2.asInstanceOf[Type])
          case _               => return None // Snd can only be used on pair, the error message is added
                                              // in leftRightSameType()
        }
      }
      return None

    // Following codes are solving the special cases of pair type
    case Fst(Fst(id @ Ident(v))) => 
      val tVar = getIdentiferType(id, v, curTable)
      if (tVar.isDefined) {
        tVar.get match {
          case PairType(Pair, t2) => None
          case _ => errorLogs.addOne(id.getPosition(), unexpectedTypeMessage(List("(pair, any)")))
                    None
        }
      }
      None

    case Snd(Fst(id @ Ident(v))) =>
      val tVar = getIdentiferType(id, v, curTable)
      if (tVar.isDefined) {
        tVar.get match {
          case PairType(Pair, t2) => None
          case _ => errorLogs.addOne(id.getPosition(), unexpectedTypeMessage(List("(pair, any)")))
                    None
        }
      }
      None

    case Fst(Snd(id @ Ident(v))) => 
      val tVar = getIdentiferType(id, v, curTable)
      if (tVar.isDefined) {
        tVar.get match {
          case PairType(t1, Pair) => None
          case _ => errorLogs.addOne(id.getPosition(), unexpectedTypeMessage(List("(any, pair)")))
                    None
        }
      }
      None

    case Snd(Snd(id @ Ident(v))) => 
      val tVar = getIdentiferType(id, v, curTable)
      if (tVar.isDefined) {
        tVar.get match {
          case PairType(t1, Pair) => None
          case _ => errorLogs.addOne(id.getPosition(), unexpectedTypeMessage(List("(any, pair)")))
                    None
        }
      }
      None

    case _: PairElem => None
  }

  def lValueIsType(t: Type, lv: Lvalue, curTable: SymbolTable): Unit = (t, lv) match{
    // Checking if the left element of a pair is type t(a pair)
    case (PairType(_,_), Fst(id @ Ident(v))) => 
      val tVar = getIdentiferType(id, v, curTable)
      if (tVar.isDefined) {
        tVar.get match {
          case PairType(t1, t2) => 
            // Checking if 
            if (t1 != Pair) {
              errorLogs.addOne((id.pos, unexpectedTypeMessage(List("(pair,any)"))))
            }
          case _ => errorLogs.addOne((id.pos, unexpectedTypeMessage(List("pair"))))
        }
      }

    // Checking if the right element of a pair is type t(a pair)
    case (PairType(_,_), Snd(id @ Ident(v))) => 
      val tVar = getIdentiferType(id, v, curTable)
      if (tVar.isDefined) {
        tVar.get match {
          case PairType(t1, t2) => 
            if (t2 != Pair) {
              errorLogs.addOne((id.pos, unexpectedTypeMessage(List("(pair,any)"))))
            }
          case _ => errorLogs.addOne((id.pos, unexpectedTypeMessage(List("pair"))))
        }
      }

    // Checking if id is legal and type t
    case (t, id @ Ident(v)) => 
      val tVar = getIdentiferType(id, v, curTable)
      if (tVar.isDefined) {
        if (tVar.get != t) {
          errorLogs.addOne((id.pos, unexpectedTypeMessage(List(t.getName()))))
        }
      }

    // Checking if id is legal and type t
    case (t, ArrayElem(id @ Ident(v), exprs)) => 
      val tVar = getIdentiferType(id, v, curTable)
      exprs.foreach(validExpr(_, curTable)) // Checking if all expressions are legal
      if (tVar.isDefined) {
        if (tVar.get != t) {
          errorLogs.addOne((id.pos, unexpectedTypeMessage(List(t.getName()))))
        }
      }

    case (_) => // For fst and snd, it will only check the first layer, more than one layers is always accecpted
  }

  def unexpectedTypeMessage(types: List[String]): String = {
    val sb = new StringBuilder("Unexpected type, expected type(s): ")
    sb.append(types.mkString(", ")).toString()
  }

  // Check if rv is type t, otherwise adding error message to errorLogs and doing some expressions checking meanwhile
  def rValueIsType(t: Type, rv: Rvalue, curTable: SymbolTable): Unit = (t, rv) match {
    
    // It checks if all expressions in ArrayLiter are type subT
    case (ArrayType(subT), ArrayLiter(exprs)) =>
      exprs.foreach(rValueIsType(subT, _, curTable))

    case (t, ae @ ArrayElem(id, exprs)) =>
      // Checking all expressions given are valid and int type
      exprs.foreach(validExpr(_, curTable))
      exprs.foreach(checkExpTypeInt(_,curTable))
      val arrayType = getIdentiferType(id, id.v, curTable)
      if (arrayType.isDefined) {
        if (!arrayType.get.isInstanceOf[ArrayType]) {
          errorLogs.addOne((id.getPosition(), unexpectedTypeMessage(List("array"))))
        } else {
          // Checking if the there is problem like x[1][1] but x is a just one-dimensional array
          val dimension = getDimension(arrayType.get)
          if (dimension < exprs.size) {
            errorLogs.addOne((id.getPosition(), unexpectedTypeMessage(List(arrayType.get.getName()+"[]"*exprs.size))))
          }
          val arrayInnerType = getArrayInnerTypeByTimes(arrayType.get, exprs.size)
          (t, arrayInnerType) match {
            case (ep@PairType(et1, et2), p@PairType(t1, t2)) =>
              if (!twoTypesAreSame(ep, p)) {
                errorLogs.addOne((rv.getPosition(),
                  unexpectedTypeMessage(List("(" + et1.getName() + "," + et2.getName() + ")"))))
              }

            case (eType, rType) if (eType == rType)   =>

            case _ =>  errorLogs.addOne((ae.getPosition(), unexpectedTypeMessage(List(t.getName()))))
          }
        }
      }

    case (PairType(lElemType, rElemType), NewPair(lExpr, rExpr)) =>
      exprIsElemType(lElemType, lExpr, curTable) ; exprIsElemType(rElemType, rExpr, curTable)

    case (t, np@NewPair(_, _)) => errorLogs.addOne((np.getPosition(), unexpectedTypeMessage(List(t.getName()))))

    case (t, lv @ Fst(_)) => lValueIsType(t, lv, curTable)
    case (t, lv @ Snd(_)) => lValueIsType(t, lv, curTable)

    case (t, calling@Call(id, exprs)) =>
      val funcIdentifier = curTable.lookupAll("(" + id.v + ")")
      if (funcIdentifier.isDefined) {
        funcIdentifier.get match {
          case func: FuncObject =>
            exprs.foreach(validExpr(_, curTable))
            // Checking if the number of arguments match the definition
            if (exprs.size != func.paraList.size) {
              errorLogs.addOne((id.getPosition(),
                "Unexpected number of arguments, expcted " + func.paraList.size + " argument(s)"))
            }
            exprs.zip(func.paraList).foreach{
              case(e, p) =>
                val exprType = getExpType(e, curTable)
                if (exprType.isDefined) {
                  if (!twoTypesAreSame(exprType.get, p.t)) {
                    // If the type of the expression is not same as it should have been passed in
                    errorLogs.addOne((e.getPosition(), unexpectedTypeMessage(List(p.t.getName()))))
                  }
                } else {
                  errorLogs.addOne((e.getPosition(), "Illegal expression"))
                }
            }
            if (!twoTypesAreSame(t, func.returnType)) {
              errorLogs.addOne((calling.getPosition(), "Unexpected return type, expected type: " + t.getName() + t))
            }
          case _ => errorLogs.addOne((rv.getPosition(), "Invalid calling of function"))
        }
      } else {
        errorLogs.addOne((id.getPosition(), "Undefined function"))
      }

    case _ => validExpr(rv.asInstanceOf[Expr], curTable)
      val exprType = getExpType(rv.asInstanceOf[Expr], curTable)
      if (exprType.isDefined) {
        (exprType.get, t) match {
          case (Null, PairType(_,_)) =>
          case (Null, ArrayType(_))  =>
          case (_) => if (!twoTypesAreSame(exprType.get, t)) {
            if (t.isInstanceOf[ArrayType]) {
              errorLogs.addOne((rv.getPosition(),
                unexpectedTypeMessage(List("array" + "[]" * getDimension(t)))))
            } else {
              errorLogs.addOne((rv.getPosition(), unexpectedTypeMessage(List(t.getName()))))
            }
          }
        }
      }
  }


  // This function return the dimension of the t
  def getDimension(t: Type): Int = t match {
    case at@ArrayType(t) => getDimension(t) + 1
    case _               => 0
  }

  // This function is used to return the inner type of ArrayType
  def getArrayInnerTypeByTimes(t: Type, n: Int): Type = (t, n) match {
    case (t, 0)            => t
    case (ArrayType(t), n) => getArrayInnerTypeByTimes(t, n - 1)
    case (t, n)            => t
  }

  def twoPairElemTypesAreSame(t1: PairElemType, t2: PairElemType): Boolean = (t1, t2) match {
    case(ArrayType(t1), ArrayType(t2)) => twoTypesAreSame(t1, t2)
    case (t1, t2)                      => t1 == t2
  }

  def pairNullExps(typ1: Type, typ2: Type): Boolean = {
    if (typ1 == Null && typ2.isInstanceOf[PairType]) {
      true
    } else if (typ2 == Null && typ1.isInstanceOf[PairType]) {
      true
    } else {
      false
    }
  }

  // Check if expr is type pairElemType, if not adding error message to errorLogs
  def exprIsElemType(pairElemType: PairElemType, expr: Expr, curTable: SymbolTable): Unit = pairElemType match {
    case Pair            => exprIsPair(expr, curTable)
    case ArrayType(t)    => rValueIsType(pairElemType.asInstanceOf[ArrayType], expr, curTable)
    case IntType             => rValueIsType(IntType, expr, curTable)
    case BoolType            => rValueIsType(BoolType, expr, curTable)
    case CharType            => rValueIsType(CharType, expr, curTable)
    case StringType          => rValueIsType(StringType, expr, curTable)
    case Null                => 
  }

  // Check if expr is the pair type, if not adding error messages to errorLogs
  def exprIsPair(expr: Expr, curTable: SymbolTable): Unit = expr match {

    case Ident(v)  => 
      val t = getIdentiferType(expr.asInstanceOf[Ident], v, curTable)
      if (t.isDefined) {
        t.get match {
          case PairType(_, _) => 
          case _              => errorLogs.addOne(expr.getPosition(), unexpectedTypeMessage(List("pair")))
        }
      }

    case ae@ArrayElem(_, _) => rValueIsType(PairType(Null, Null)(0,0), ae, curTable)

    case PairLiter() => 

    case _         => errorLogs.addOne(expr.getPosition(), unexpectedTypeMessage(List("pair")))
  }


  // Check if lv is readable, if not adding error messages to errorLogs
  def readable(lv: Lvalue, curTable: SymbolTable): Unit = {
    val leftType = getLeftValueType(lv, curTable)

    if (leftType.isDefined) {
      leftType.get match {
        // Only char or int can be used in read
        case CharType => 
        case IntType  => 
        case _        => errorLogs.addOne((lv.getPosition(), unexpectedTypeMessage(List("char", "int"))))
      }
    }
  }

  // Check if lv is freeable, if not adding error messages to errorLogs
  def freeable(expr: Expr, curTable: SymbolTable): Unit = {
    expr match {
      case id@Ident(v) => 
        val varType = getIdentiferType(id, v, curTable)
        if (varType.isDefined) {
          varType.get match {
            case PairType(_, _) =>
            case ArrayType(t)   =>  
            case _              => errorLogs.addOne((expr.getPosition(), "Only array or pair can be freed"))
          }
        }
      case _          => errorLogs.addOne((expr.getPosition(), "Only array or pair can be freed"))
    }
  }

  // Check if the curTable is the root symbol table, return true if it is
  def inMain(curTable: SymbolTable): Boolean = {
    // Only in the symbol table of the main program, lookupAll("return") will return None
    return curTable.lookupAll("return").isEmpty
  }


  // Check if the expr is valid
  def validExpr(expr: Expr, curTable: SymbolTable): Unit = {
    expr match {

      // Identifier
      case Ident(symbolName) =>
        if (curTable.lookupAll(symbolName).isEmpty) {
            errorLogs.addOne(expr.asInstanceOf[Ident].pos,
            "Identifier " + symbolName + " has not been defined yet")
        } else if (curTable.lookupAll(symbolName).get.isInstanceOf[VarObject]) {
          // means this is a valid variable, so do nothing
        } else {
          errorLogs.addOne(expr.getPosition(), "Identifier " + symbolName + " is not a variable name")
        }

      // Unary operators
      case NotUOp(thisExpr) => checkExpTypeBool(thisExpr, curTable)
      case NegUOp(thisExpr) => checkExpTypeInt(thisExpr, curTable)
      case LenUOp(thisExpr) => checkExpTypeArray(thisExpr, curTable)
      case OrdUOp(thisExpr) => checkExpTypeChar(thisExpr, curTable)
      case ChrUOp(thisExpr) => checkExpTypeInt(thisExpr, curTable)

      // Binary operators
      case IntInfixOperator(exp1, exp2)  => checkExpTypeInt(exp1, curTable)
                                            checkExpTypeInt(exp2, curTable)
      case ComparisonOperator(exp1, exp2) => checkExpTypeIntOrChar(exp1, curTable)
                                             checkExpTypeIntOrChar(exp2, curTable)
                                             sameType(exp1, exp2, curTable)
      case And(exp1, exp2) => checkExpTypeBool(exp1, curTable)
                              checkExpTypeBool(exp2, curTable)
      case Or(exp1, exp2) => checkExpTypeBool(exp1, curTable)
                             checkExpTypeBool(exp2, curTable)
      case Eq(exp1, exp2) => validExpr(exp1, curTable)
                             validExpr(exp2, curTable)
                             sameType(exp1, exp2, curTable)
      case Neq(exp1, exp2) => validExpr(exp1, curTable)
                              validExpr(exp2, curTable)
                              sameType(exp1, exp2, curTable)
      case ArrayElem(id, expList) => checkExpTypeArrayElem(expr, curTable)
      case PairLiter() => // null is always a valid expression
      case _ =>  // The rest are just liters which do not require check, so do nothing
    }
  }

  def checkExpTypeArray(thisExpr: Expr, curTable: SymbolTable): Unit = {
    thisExpr match {

      case ArrayElem(id, expressions) => 
        val typ = getIdentiferType(id, id.v, curTable)
        expressions.foreach(validExpr(_, curTable))
        if (typ.isDefined) {
          if (!typ.get.asInstanceOf[ArrayType].t.isInstanceOf[ArrayType]) {
            errorLogs.addOne(thisExpr.getPosition(), "This expression is not an array")
          }
        }

      case Ident(v) => 
        val typ = getIdentiferType(thisExpr.asInstanceOf[Ident], v, curTable)
        if (typ.isDefined) {
          if (!typ.get.isInstanceOf[ArrayType]) {
            errorLogs.addOne(thisExpr.getPosition(), "This expression is not an array")
          }
        }

      case _ => errorLogs.addOne(thisExpr.getPosition(), "This expression is not an array")
    }
  }

  def checkExpTypeArrayElem(thisExpr: Expr, curTable: SymbolTable): Unit = {
    thisExpr match {
      case ArrayElem(id, expressions) => 
        getIdentiferType(id, id.v, curTable)
        expressions.foreach(validExpr(_, curTable))
      case _ => errorLogs.addOne(thisExpr.getPosition(), "This expression is not an array element")
    }
  }

  // Check if the given expr returns a BooleanLiter
  def checkExpTypeBool(expr: Expr, curTable: SymbolTable): Unit = {
    expr match {
      case BoolLiter(b) =>
      case Ident(symbolName) =>
        val typ = getIdentiferType(expr.asInstanceOf[Ident], symbolName, curTable)
        if (typ.isEmpty) {

          // just to skip if typ is None because getIdentifierType has already added error log
        } else if (typ.get == BoolType) {
          // since boolean is what this expression should be, do nothing
        } else {
          // means the variable is not a boolean
          errorLogs.addOne(expr.getPosition(), "Identifier " + symbolName + " is not a Boolean type variable")
        }
      case ArrayElem(id, expr) =>
        val typ = getIdentiferType(id, id.v, curTable)
        if (typ.isEmpty) {
        } else if (typ.get.asInstanceOf[ArrayType].t == BoolType) {
        } else {
          errorLogs.addOne(id.getPosition(), "Array " + id.v + " is not a Boolean type array")
        }
        expr.foreach(checkExpTypeInt(_, curTable))
      case NotUOp(expr) => checkExpTypeBool(expr, curTable)
      case Or(exp1, exp2) => checkExpTypeBool(exp1, curTable)
                             checkExpTypeBool(exp2, curTable)
      case And(exp1, exp2) => checkExpTypeBool(exp1, curTable)
                              checkExpTypeBool(exp2, curTable)
      case Eq(exp1, exp2) => validExpr(exp1, curTable)
                             validExpr(exp2, curTable)
                             sameType(exp1, exp2, curTable)
      case Neq(exp1, exp2) => validExpr(exp1, curTable)
                              validExpr(exp2, curTable)
                              sameType(exp1, exp2, curTable)
      case ComparisonOperator(exp1, exp2) => checkExpTypeIntOrChar(exp1, curTable)
                                             checkExpTypeIntOrChar(exp2, curTable)
                                             sameType(exp1, exp2, curTable)
      case _ => errorLogs.addOne(expr.getPosition(), "This expression should return a Boolean")
    }
  }

  // Check if the given expr returns a IntLiter
  def checkExpTypeInt(expr: Expr, curTable: SymbolTable): Unit = {
    expr match {
      case IntLiter(b) =>

      case Ident(symbolName) =>
        val typ = getIdentiferType(expr.asInstanceOf[Ident], symbolName, curTable)
        if (typ.isEmpty) {
          // just to skip if typ is None
        } else if (typ.get == IntType) {
          // since Int is what this expression should be, do nothing
        } else {
          // means the variable is not a int
          errorLogs.addOne(expr.getPosition(), "Identifier " + symbolName + " is not a Int type variable")
        }

      case ArrayElem(id, expr) =>
        val typ = getIdentiferType(id, id.v, curTable)
        if (typ.isEmpty) {
        } else if (typ.get.asInstanceOf[ArrayType].t == IntType) {
          // since Int is what this expression should be, do nothing
        } else {
          // means the variable is not a int
          errorLogs.addOne(id.getPosition(), "Array " + id.v + " is not a Int type array")
        }
        expr.foreach(checkExpTypeInt(_, curTable))

      case IntInfixOperator(exp1, exp2) => checkExpTypeInt(exp1, curTable)
                                           checkExpTypeInt(exp2, curTable)

      case NegUOp(expr) => checkExpTypeInt(expr, curTable)

      case LenUOp(expr) => checkExpTypeArray(expr, curTable)

      case OrdUOp(expr) => checkExpTypeChar(expr, curTable)

      case _ => errorLogs.addOne(expr.getPosition(), "This expression should return a Int")
    }
  }

  // Check if the given expr returns an CharLiter
  def checkExpTypeChar(expr: Expr, curTable: SymbolTable): Unit = {
    expr match {
      case CharLiter(b) =>
      case Ident(symbolName) =>
        val typ = getIdentiferType(expr.asInstanceOf[Ident], symbolName, curTable)
        if (typ.isEmpty) {
          // just to skip if typ is None
        } else if (typ.get == CharType) {
          // since boolean is what this expression should be, do nothing
        } else {
          // means the variable is not a char
          errorLogs.addOne(expr.getPosition(), "Identifier " + symbolName + " is not a Char type variable")
        }
      case ArrayElem(id, expr) =>
        val typ = getIdentiferType(id, id.v, curTable)
        if (typ.isEmpty) {
        } else if (typ.get.asInstanceOf[ArrayType].t == CharType) {
        } else {
          errorLogs.addOne(id.getPosition(), "Array " + id.v + " is not a Char type array")
        }
        expr.foreach(checkExpTypeInt(_, curTable))
      case ChrUOp(exp) => checkExpTypeInt(exp, curTable)
      case _ => errorLogs.addOne(expr.getPosition(), "This expression does not return a Char")
    }
  }

  // Check if the given expr returns an IntLiter or an CharLiter
  def checkExpTypeIntOrChar(expr: Expr, curTable: SymbolTable): Unit = {
    expr match {
      case IntLiter(b) =>

      case CharLiter(b) =>

      case Ident(symbolName) =>
        val typ = getIdentiferType(expr.asInstanceOf[Ident], symbolName, curTable)
        if (typ.isEmpty) {
          // just to skip if typ is None
        } else if (typ.get == CharType || typ.get == IntType) {
          // since boolean or int is what this expression should be, do nothing
        } else {
          // means the variable is not a char or int
          errorLogs.addOne(expr.getPosition(), "Identifier " + symbolName + " is not a Char or Int type variable")
        }

      case ArrayElem(id, expr) =>
        val typ = getIdentiferType(id, id.v, curTable)
        if (typ.isEmpty) {
        } else if (typ.get.asInstanceOf[ArrayType].t == IntType || typ.get.asInstanceOf[ArrayType].t == CharType) {
          // since Int is what this expression should be, do nothing
        } else {
          // means the variable is not a int
          errorLogs.addOne(id.getPosition(), "Array " + id.v + " is not a Int or Char type array")
        }
        expr.foreach(checkExpTypeInt(_, curTable))
      // Unary
      case NegUOp(expr) => checkExpTypeInt(expr, curTable)
      case LenUOp(expr) => checkExpTypeArray(expr, curTable)
      case OrdUOp(expr) => checkExpTypeChar(expr, curTable)
      case ChrUOp(expr) => checkExpTypeInt(expr, curTable)

      // Binary
      case IntInfixOperator(exp1, exp2) => checkExpTypeInt(exp1, curTable)
                                           checkExpTypeInt(exp2, curTable)
                                           
      case _ => errorLogs.addOne(expr.getPosition(), "This expression does not return a Char or Int")
    }
  }


}
