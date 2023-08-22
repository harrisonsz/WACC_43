package wacc.front

import parsley.expr.Prefix

import java.lang

/* Overall abstract tree structure and its companion objects */
object ast{
  import parsley.Parsley
  import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
  import parsley.position.pos

  case class Program (funcs: List[Func], stats: List[Stat]) (val pos: (Int, Int))

  case class Func (t: Type, id: Ident, params: List[Param], stats: List[Stat]) (val pos: (Int, Int))

  case class Param (t: Type, id: Ident) (val pos: (Int, Int))

  sealed trait PairElemType {def getName(): String}
  case object Pair extends PairElemType {def getName() = "pair"}
  
  sealed trait Type {def getName(): String}
  case class ArrayType (t: Type) (val pos: (Int, Int)) extends Type with PairElemType {def getName() = "array"}
  case class PairType  (t1: PairElemType, t2: PairElemType) (val pos: (Int, Int)) extends Type {def getName() = "pair"}
  
  sealed trait BaseType extends Type with PairElemType
  case object IntType    extends BaseType {def getName() = "int"}
  case object BoolType   extends BaseType {def getName() = "bool"}
  case object CharType   extends BaseType {def getName() = "char"}
  case object StringType extends BaseType {def getName() = "string"}

  case object Null extends BaseType {def getName() = "null"}

  sealed trait Rvalue {def getPosition(): (Int, Int) }
  sealed trait Lvalue {def getPosition(): (Int, Int)}

  case class ArrayLiter(exprs: List[Expr]) (val pos: (Int, Int)) extends Rvalue {def getPosition(): (Int, Int) = pos}

  case class NewPair (expr1: Expr, expr2: Expr) (val pos: (Int, Int)) extends Rvalue {def getPosition(): (Int, Int) = pos}

  sealed trait PairElem extends Rvalue with Lvalue
  case class Fst (lv: Lvalue) (val pos: (Int, Int)) extends PairElem {def getPosition(): (Int, Int) = pos}
  case class Snd (lv: Lvalue) (val pos: (Int, Int)) extends PairElem {def getPosition(): (Int, Int) = pos}

  case class Call (id: Ident, args: List[Expr]) (val pos: (Int, Int)) extends Rvalue {def getPosition(): (Int, Int) = pos}

  sealed trait Stat
  case object Skip extends Stat
  case class Init      (t: Type, ident: Ident, rv: Rvalue) (val pos: (Int, Int)) extends Stat
  case class Assign    (lv: Lvalue, rv: Rvalue) (val pos: (Int, Int))            extends Stat
  case class Read      (lv: Lvalue) (val pos: (Int, Int))                        extends Stat
  case class Free      (expr: Expr) (val pos: (Int, Int))                        extends Stat
  case class Return    (expr: Expr) (val pos: (Int, Int))                        extends Stat
  case class Exit      (expr: Expr) (val pos: (Int, Int))                        extends Stat
  case class Print     (expr: Expr) (val pos: (Int, Int))                        extends Stat
  case class Println   (expr: Expr) (val pos: (Int, Int))                        extends Stat
  case class If        (cond: Expr, exprTrue: List[Stat], exprFalse: List[Stat]) (val pos: (Int, Int)) extends Stat
  case class While     (cond: Expr, stats: List[Stat]) (val pos: (Int, Int))                      extends Stat
  case class SubProgram(stats: List[Stat]) (val pos: (Int, Int))                                  extends Stat

  // All get position functions
  sealed trait Expr extends Rvalue {
    def getPosition(): (Int, Int)
  }

  case class IntLiter   (v:scala.Int)    (val pos: (Int, Int)) extends Expr {
    override def getPosition(): (Int, Int) = pos
  }
  case class BoolLiter  (v:scala.Boolean)(val pos: (Int, Int)) extends Expr {
    override def getPosition(): (Int, Int) = pos
  }
  case class CharLiter  (v:scala.Char)   (val pos: (Int, Int)) extends Expr {
    override def getPosition(): (Int, Int) = pos
  }
  case class StrLiter   (v:lang.String)  (val pos: (Int, Int)) extends Expr {
    override def getPosition(): (Int, Int) = pos
  }
  case class PairLiter() (pos: (Int, Int)) extends Expr {
    override def getPosition(): (Int, Int) = pos
  }
  case class Ident      (v:lang.String)  (val pos: (Int, Int)) extends Expr with Lvalue {
    override def getPosition(): (Int, Int) = pos
  }
  case class ArrayElem  (id:Ident, expr:List[Expr]) (val pos: (Int, Int))extends Expr with Lvalue {
    override def getPosition(): (Int, Int) = pos
  }

  sealed trait UnaryOp extends Expr
  case class NotUOp (expr:Expr)(val pos: (Int, Int)) extends  UnaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class NegUOp (expr:Expr)(val pos: (Int, Int)) extends  UnaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class LenUOp (expr:Expr)(val pos: (Int, Int)) extends  UnaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class OrdUOp (expr:Expr)(val pos: (Int, Int)) extends  UnaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class ChrUOp (expr:Expr)(val pos: (Int, Int)) extends  UnaryOp {
    override def getPosition(): (Int, Int) = pos
  }

  sealed trait BinaryOp extends Expr
  case class Mul (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class Div (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class Mod (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class Add (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class Sub (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class Gt  (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class Gte (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class Lt  (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class Lte (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class Eq  (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class Neq (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class And (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }
  case class Or  (exprL: Expr, exprR: Expr)(val pos: (Int, Int)) extends BinaryOp {
    override def getPosition(): (Int, Int) = pos
  }

  // Generic bridges for position
  trait ParserSingletonBridgePos[+A] {
    def con(pos: (Int, Int)): A
    def <#(op: Parsley[_]): Parsley[A] = pos.map(this.con(_)) <* op
  }

  trait ParserBridgePos0[A] extends ParserSingletonBridgePos[A] {
    def apply()(pos: (Int, Int)): A
    override def con(pos: (Int, Int)): A = this.apply()(pos)
  }

  trait ParserBridgePos1[-A, +B] extends ParserSingletonBridgePos[A => B] {
    def apply(x: A)(pos: (Int, Int)): B
    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(this.apply(_) _)
    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
  }

  trait ParserBridgePos2[-A, -B, +C] extends ParserSingletonBridgePos[(A, B) => C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C
    def apply(x: Parsley[A], y: =>Parsley[B]): Parsley[C] =
      pos <**> (x, y).zipped(this.apply(_, _) _)
    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
  }  

  trait ParserBridgePos3[-A, -B, -C, +D] extends ParserSingletonBridgePos[(A, B, C) => D] {
    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
    def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C]): Parsley[D] =
      pos <**> (x, y, z).zipped(this.apply(_, _, _) _)
    override final def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)
  }

  trait ParserBridgePos4[-A, -B, -C, -D, +E] extends ParserSingletonBridgePos[(A, B, C, D) => E] {
    def apply(x: A, y: B, z: C, n: D)(pos: (Int, Int)): E
    def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C], n: Parsley[D]): Parsley[E] =
      pos <**> (x, y, z, n).zipped(this.apply(_, _, _, _) _)
    override final def con(pos: (Int, Int)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
  }

  // Objects using parser bridge pattern
  object PairLiter extends ParserBridgePos0[PairLiter]

  object IntLiter extends ParserBridgePos1[Int, IntLiter]
  object BoolLiter extends ParserBridgePos1[Boolean, BoolLiter]
  object CharLiter extends ParserBridgePos1[Char, CharLiter]
  object StrLiter extends ParserBridgePos1[String, StrLiter]
  object ArrayLiter extends ParserBridgePos1[List[Expr], ArrayLiter]
  object Fst extends ParserBridgePos1[Lvalue, Fst]
  object Snd extends ParserBridgePos1[Lvalue, Snd]
  object Read extends ParserBridgePos1[Lvalue, Read]
  object Free extends ParserBridgePos1[Expr, Free]
  object Return extends ParserBridgePos1[Expr, Return]
  object Exit extends ParserBridgePos1[Expr, Exit]
  object Print extends ParserBridgePos1[Expr, Print]
  object Println extends ParserBridgePos1[Expr, Println]
  object SubProgram extends ParserBridgePos1[List[Stat], SubProgram]

  object ArrayType extends ParserBridgePos1[Type, ArrayType]
  object PairType extends ParserBridgePos2[PairElemType, PairElemType, PairType]

  //object PairLiter extends ParserSingletonBridgePos
  object Ident extends ParserBridgePos1[lang.String, Ident]
  object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]
  object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]
  object Call extends ParserBridgePos2[Ident, List[Expr], Call]
  object Assign extends ParserBridgePos2[Lvalue, Rvalue, Assign]
  object While extends ParserBridgePos2[Expr, List[Stat], While]
  object Param extends ParserBridgePos2[Type, Ident, Param]
  object Program extends ParserBridgePos2[List[Func], List[Stat], Program]

  object NotUOp extends ParserBridgePos1[Expr, NotUOp]
  object NegUOp extends ParserBridgePos1[Expr, NegUOp]
  object LenUOp extends ParserBridgePos1[Expr, LenUOp]
  object OrdUOp extends ParserBridgePos1[Expr, OrdUOp]
  object ChrUOp extends ParserBridgePos1[Expr, ChrUOp]

  object Mul extends ParserBridgePos2[Expr, Expr, Mul]  
  object Div extends ParserBridgePos2[Expr, Expr, Div]
  object Mod extends ParserBridgePos2[Expr, Expr, Mod]
  object Add extends ParserBridgePos2[Expr, Expr, Add]
  object Sub extends ParserBridgePos2[Expr, Expr, Sub]
  object Gt  extends ParserBridgePos2[Expr, Expr, Gt]
  object Gte extends ParserBridgePos2[Expr, Expr, Gte]
  object Lt  extends ParserBridgePos2[Expr, Expr, Lt]
  object Lte extends ParserBridgePos2[Expr, Expr, Lte]
  object Eq  extends ParserBridgePos2[Expr, Expr, Eq]
  object Neq extends ParserBridgePos2[Expr, Expr, Neq]
  object And extends ParserBridgePos2[Expr, Expr, And]
  object Or  extends ParserBridgePos2[Expr, Expr, Or]

  object Init extends ParserBridgePos3[Type, Ident, Rvalue, Init]
  object If extends ParserBridgePos3[Expr, List[Stat], List[Stat], If]

  object Func extends ParserBridgePos4[Type, Ident, List[Param], List[Stat], Func]
}

object Parser{
  import Lexer._
  import Lexer.implicits.implicitSymbol
  import parsley.Parsley
  import Parsley._
  import ast._
  import parsley.combinator._
  import parsley.errors.combinator._
  import parsley.expr.{InfixL, Ops, chain, precedence}
  import parsley.lift.lift2
  
  lazy val program: Parsley[Program] = 
    Program("begin".explain("Program should start with `begin`") ~> many(func), 
    stats.explain("Empty Body is not accepted") <~ 
    "end".explain("Program should end with `end`") <~ 
    eof.explain("Program should be enclosed by `begin` and `end`"))

  lazy val func: Parsley[Func] =
    attempt(lookAhead(typeAtom ~> identifier ~> "(")) ~> 
    Func(typeAtom, Ident(identifier), "(" ~> sepBy(param, ",") <~ ")", "is" ~> funcStatList <~ "end").
    label("Function definition").
    explain("Each path of the function must end with return or exit")

  lazy val param: Parsley[Param] =
    Param(typeAtom, Ident(identifier))

  lazy val statAtom: Parsley[Stat] = 
    ("skip" #> Skip <|>
    init <|>
    assign <|>
    "read" ~> Read(lvalue) <|>
    "free" ~> Free(expr) <|>
    "return" ~> Return(expr) <|>
    "exit" ~> Exit(expr) <|>
    "print" ~> Print(expr) <|>
    "println" ~> Println(expr) <|>
    parseIf <|>
    parseWhile <|>
    subProg).label("Statement")

  lazy val funcEndStat: Parsley[Stat] =
    funcParseIf.label("If statement has proper end") <|> 
    funcSubProg.label("Sub program has proper end") <|> 
    "return" ~> Return(expr) <|> "exit" ~> Exit(expr)

  lazy val funcStatList: Parsley[List[Stat]] =
    lift2(((x: List[Stat], y: Stat) => x :+ y), funcStats, funcEndStat)

  lazy val stats: Parsley[List[Stat]] = 
    sepBy1(statAtom, ";".explain("Statements are seperated by ;"))
  
  lazy val funcStats: Parsley[List[Stat]] =
    manyUntil((statAtom <~ ";"), attempt(lookAhead(funcEndStat ~> notFollowedBy(";"))))

  lazy val funcParseIf: Parsley[If] =
    If("if" ~> expr, "then" ~> funcStatList, ("else" ~> funcStatList <~ "fi")  <|> (pure(List.empty) <~ "fi"))

  lazy val funcSubProg: Parsley[SubProgram] = 
    SubProgram("begin" ~> funcStatList <~ "end")
  
  lazy val subProg: Parsley[SubProgram] = 
    SubProgram("begin" ~> stats <~ "end")

  lazy val parseWhile: Parsley[While] = 
    While("while" ~> expr, "do" ~> stats <~ "done")

  lazy val parseIf: Parsley[If] = 
    If("if" ~> expr, "then" ~> stats, ("else" ~> stats <~ "fi") <|> (pure(List.empty) <~ "fi"))

  lazy val init: Parsley[Init] = 
    Init(typeAtom, Ident(identifier), "=" ~> rvalue)

  lazy val assign: Parsley[Assign] = 
    Assign(lvalue, ("=" ~> rvalue).label("Assignment statement").
      explain("Assignment statement assigns a rvalue to a lvalue in the form \"lvalue = rvalue\""))

  lazy val lvalue: Parsley[Lvalue] =
    attempt(arrayElem) <|>
    Ident(identifier) <|>
    pairElem
  
  lazy val pairElem: Parsley[PairElem] = 
    (Fst("fst" ~> lvalue) <|>
    Snd("snd" ~> lvalue)).label("Pair element")

  lazy val arrayElem: Parsley[ArrayElem] = 
    ArrayElem(Ident(identifier), some("[" ~> expr <~ "]"))

  lazy val rvalue: Parsley[Rvalue] = 
    expr <|>
    arrayLiter <|>
    newPair <|>
    pairElem <|>
    call

  lazy val arrayLiter: Parsley[ArrayLiter] = 
    ArrayLiter("[" ~> sepBy(expr, ",") <~ "]").label("Arrayliter []")
  
  lazy val newPair: Parsley[NewPair] = 
    NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")").label("New pair")

  lazy val call: Parsley[Call] = 
    Call("call" ~> Ident(identifier) <~ "(", sepBy(expr, ",") <~ ")").label("Call to function")

  lazy val typeAtom: Parsley[Type] = 
    attempt(arrayType) <|>
    attempt(pairType) <|>
    baseType

  val baseType: Parsley[BaseType] = 
    (("int" #> IntType) <|> 
    ("bool" #> BoolType) <|> 
    ("char" #> CharType) <|> 
    ("string" #> StringType)).label("type declaration (int bool char string)")
  
  lazy val arrayType: Parsley[ArrayType] =
    chain.postfix1(baseType <|> pairType, ArrayType <# ("[" ~> "]").
      label("Array type"))
  lazy val pairElemType: Parsley[PairElemType] =
    attempt(arrayType) <|> 
    attempt(baseType) <|> 
    ("pair" #> Pair)

  lazy val pairType: Parsley[PairType] =
    PairType("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")

  lazy val intLiter: Parsley[IntLiter] = IntLiter(num)
  
  lazy val boolLiter: Parsley[BoolLiter] = 
    BoolLiter("true" ~> pure(true) <|> "false" ~> pure(false))

  lazy val charLiter: Parsley[CharLiter] = CharLiter(char.map(_.toChar))

  lazy val strLiter: Parsley[StrLiter] = StrLiter(str)

  lazy val atom: Parsley[Expr] = 
    "(" ~> expr <~ ")" <|>
    intLiter <|> 
    boolLiter <|> 
    charLiter <|>
    strLiter <|> 
    (PairLiter <# "null") <|>
    attempt(arrayElem) <|>
    (Ident(identifier))

  lazy val expr: Parsley[Expr] =
    precedence[Expr](atom)(
      Ops(Prefix)(LenUOp <# (lookAhead("len") ~> notFollowedBy(identifier) ~> "len".label("Unary Operator")),
                  OrdUOp <# (lookAhead("ord") ~> notFollowedBy(identifier) ~> "ord".label("Unary Operator")),
                  ChrUOp <# (lookAhead("chr") ~> notFollowedBy(identifier) ~> "chr".label("Unary Operator"))),
      Ops(InfixL)(Mul <# "*".label("Binary Operator"),
                  Div <# "/".label("Binary Operator"),
                  Mod <# "%".label("Binary Operator")),
      Ops(Prefix)(NegUOp <# (lookAhead("-") ~> notFollowedBy(intLiter) ~> "-".label("Unary Operator"))),
      Ops(InfixL)(Add <# "+".label("Binary Operator"),
                  Sub <# "-".label("Binary Operator")),
      Ops(InfixL)(Gt <# ">".label("Binary Operator"),
                  Gte <# ">=".label("Binary Operator"),
                  Lt <# "<".label("Binary Operator"),
                  Lte <# "<=".label("Binary Operator")),
      Ops(InfixL)(Eq <# "==".label("Binary Operator"),
                  Neq <# "!=".label("Binary Operator")),
      Ops(Prefix)(NotUOp <# "!".label("Unary Operator")),
      Ops(InfixL)(And <# "&&".label("Binary Operator")),
      Ops(InfixL)(Or <# "||".label("Binary Operator"))
    ).label("Expression").
    explain("Expression contains integer, boolean, char, string, pair, identifier, " +
      "array element, unary operator and binary operator")

  val parser = fully(program)
}
