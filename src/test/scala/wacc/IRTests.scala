package wacc
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.back.IR._
import wacc.back.TransExpression.transExpr
import wacc.back.RegOperator.allRegs
import wacc.front.SymbolTable.SymbolTable
import wacc.front.ast

class IRTests extends AnyFlatSpec with Matchers {
  val testTable = new SymbolTable()
  val regs = allRegs()
  
  "An IntLiter" should "have one element in List" in {
    assert(transExpr(ast.IntLiter(1) ((1,1)), regs, testTable).size == 1)
  }

  it should "have correct element" in {
    transExpr(ast.IntLiter(1) ((1,1)), regs, testTable) should be (List(Mov(regs.head, Imm(1))))
  }

  "test_nested_if" should "test correctly" in {
    Main.main(Array("wacc_example/valid/scope/ifNested1.wacc"))
  }
}