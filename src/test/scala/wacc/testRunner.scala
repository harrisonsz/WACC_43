package wacc
import org.scalatest._
import parsley.Failure
import parsley.Success

import scala.io.Source
import flatspec._
import matchers._
import wacc.front.Parser
import wacc.front.SemanticChecker._


class testRunner extends AsyncFlatSpec with should.Matchers{

  def parserRunner(fname: String): Int = {
    val fileContents = Source.fromFile(fname).mkString
    return Parser.parser.parse(fileContents) match {
      case Success(x) => checkingProgram(x) match {
        case (Nil, _) => 0
        case (head :: next, _) => 200
      }
      case Failure(msg) => 100
    }
  }

  final val validList: List[String] = Source.fromFile("./valid.txt").getLines().toList
  final val semanticErrList: List[String] = Source.fromFile("./semantic.txt").getLines().toList
  final val syntaxErrList: List[String] = Source.fromFile("./syntax.txt").getLines().toList

  println("Testing valid tests")
  for (fname <- validList) {
    fname should ("exit with exit code " + 0) in {
      parserRunner(fname) should be (0)
    }
  }
  
  println("Testing invalid semantic tests")
  for (fname <- semanticErrList) {
    fname should ("exit with exit code " + 200) in {
      parserRunner(fname) should be (200)
    }
  }

  println("Testing invalid syntax tests")
  for (fname <- syntaxErrList) {
    fname should ("exit with exit code " + 100) in {
      parserRunner(fname) should be (100)
    }
  }
}
