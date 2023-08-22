package wacc
import org.scalatest.Inside.inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parsley.Success
import wacc.front.Parser
import wacc.front.ast._

class ParserTests extends AnyFlatSpec with Matchers {
  val finalParser = Parser.parser

  inside (Parser.expr.parse("1")) {case Success(IntLiter(x)) =>
    x should be (1)
  }

  inside (Parser.expr.parse("2147483647")) {case Success(IntLiter(x)) =>
    x should be (2147483647)
  }

  inside (Parser.expr.parse("-2147483648")) {case Success(IntLiter(x)) =>
    x should be (-2147483648)
  }

  inside (Parser.expr.parse("x")) {case Success(Ident(x)) => 
    x should be ("x")
  }

  inside (Parser.expr.parse("len gth")) {case Success(LenUOp(Ident(x))) => 
    x should be ("gth")
  }

  inside (Parser.expr.parse("length")) {case Success(Ident(x)) => 
    x should be ("length")
  }

  // // literals tests --------------------------------------------------------------------------
  // finalParser.parse("1") should be (Success(IntLiter(1)))
  // // finalParser.parse("'x'") should be (Success(CharLiter('x')))
  // finalParser.parse("x") should be (Success(Ident("x")))
  // // finalParser.parse("true") should be (Success(BoolLiter(true)))
  // // finalParser.parse("false") should be (Success(BoolLiter(false)))

  // // white space tests -----------------------------------------------------------------------
  // finalParser.parse(" 23456 ") should be (Success(IntLiter(23456)))
  // finalParser.parse("1 + 2") should be(Success(Add(IntLiter(1), IntLiter(2))))
  // finalParser.parse("1+2 + 3") should be(Success(Add(Add(IntLiter(1), IntLiter(2)), IntLiter(3))))

  // // binary operators tests -----------------------------------------------------------------------
  // finalParser.parse("1+2") should be (Success(Add (IntLiter(1), IntLiter(2))))
  // finalParser.parse("2 - 1") should be (Success(Sub (IntLiter(2), IntLiter(1))))
  // finalParser.parse("1 * 2") should be (Success(Mul (IntLiter(1), IntLiter(2))))
  // finalParser.parse("2 / 1") should be (Success(Div (IntLiter(2), IntLiter(1))))
  // finalParser.parse("10 % 3") should be (Success(Mod (IntLiter(10), IntLiter(3))))
  // finalParser.parse("10 > 3") should be (Success(Gt (IntLiter(10), IntLiter(3))))
  // finalParser.parse("10 >= 3") should be (Success(Gte (IntLiter(10), IntLiter(3))))
  // finalParser.parse("3 == 3") should be (Success(Eq (IntLiter(3), IntLiter(3))))
  // finalParser.parse("3 && 3") should be (Success(And (IntLiter(3), IntLiter(3))))
  // finalParser.parse("xyz > 3") should be (Success(Gt (Ident("xyz"), IntLiter(3))))
  // // finalParser.parse("x + (3 * y)") should be (Success(Add (Ident("x"), (Mul (IntLiter(3),Ident("y"))))))

  // // precedence tests ------------------------------------------------------------------------
  // finalParser.parse("x + 3 * y") should be (Success(Add (Ident("x"), Mul (IntLiter(3),Ident("y")))))
  // finalParser.parse("x >= 3 + y") should be (Success(Gte (Ident("x"), Add (IntLiter(3),Ident("y")))))
  // finalParser.parse("x + 3 || y < 1") should be (Success(Or (Add (Ident("x"),IntLiter(3)), Lt (Ident("y"), IntLiter(1)))))


  // unary operator tests
  /*
  * finalParser.parse("!false") should be (Success(NotUOp(BoolLiter(false))))
  * finalParser.parse("-23") should be (Success(NegUOp(IntLiter(23))))
  *
  */
}
