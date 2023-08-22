package wacc

import parsley.Failure
import parsley.Success

import scala.io.Source
import wacc.front.SemanticChecker.checkingProgram
import wacc.back.IRGenerator.{OptimiseFold, generateIR}
import wacc.back.CodeGenerator.generatingCode
import wacc.front.Parser

import java.io.File
import java.io.PrintWriter
import wacc.decompiler.ArmToIR
import wacc.back.IR
import wacc.decompiler.CFgenerate.{ControlFlowGraph, FlowGraph}
import wacc.decompiler.ControlFlowAnalysis.CFAnalysis
import wacc.decompiler.DataFlow.DataFlowAnalysis
import wacc.decompiler.HighLevelCodeGenerator.HighLevelCodeGen
import wacc.decompiler.DataFlow.L2IRGenerator

object Main {

  def generateAssemblerFile(code: String, fileName: String): Unit = {
    import wacc.back.IRGenerator.OptimiseERA
    val outPutName = new StringBuilder(fileName).dropRight(5)
    if (OptimiseERA) {
      outPutName.append("_ERA")
    }
    val fileObject = new File(outPutName.append(".s").toString())
    val printWriter = new PrintWriter(fileObject)
    printWriter.write(code)
    printWriter.close()
  }

  def semanticErrorHandler(pos: (Int, Int), errorMessage: String, filepath: String): Unit = {
    val lines = Source.fromFile(filepath).getLines().toArray
    println("Error at " + pos)
    println(errorMessage)
    println("  |" + lines(pos._1-2))
    println("  |" + lines(pos._1-1))
    println("  |" + " " * (pos._2-1) + "^")
    println("  |" + lines(pos._1))
  }

  def main(args: scala.Array[String]): Unit = {

    import wacc.back.IRGenerator.OptimiseERA

    var filepath = args(0)

    var decompile = false

    def isSwitch(s: String) = (s(0) == '-')
    def inputParser(args: List[String]): Unit = args match {
      case path :: Nil => filepath = path

      case opt :: rest if isSwitch(opt) => opt match {
        case "-r" => 
          if (decompile) throw new Exception("-d cannot be used with -r or -f")
          OptimiseERA = true
          inputParser(rest)
        case "-f" => 
          if (decompile) throw new Exception("-d cannot be used with -r or -f")
          OptimiseFold = true
          inputParser(rest)
        case "-d" => 
          if (OptimiseERA || OptimiseERA) {
            throw new Exception("-d cannot be used with -r or -f")
          }
          decompile = true
          inputParser(rest)
        case _    => throw new Exception("Unknown option " + opt)
      }
      case _  => throw new Exception("Unknown operations")
    }
    inputParser(args.toList)

    val file = new File(filepath)
    val fileContents = Source.fromFile(filepath).mkString
    val fileName = file.getName()

    if (decompile) {
      val res1: List[IR.Instruction] = ArmToIR.convertAllToIR(fileContents)
      val flowGraph1: List[FlowGraph] = ControlFlowGraph.generateAllGraph(res1)
      val l2IR: List[FlowGraph] = L2IRGenerator.transFlowGraphs(flowGraph1)
      val cl2IR: List[FlowGraph] = CFAnalysis.cFAnalysis(l2IR)
      val waccStr: String = HighLevelCodeGen.convertFlowGraphToWACC(cl2IR)
      println(waccStr)
    } else {
      Parser.parser.parse(fileContents) match {
        case Success(x) => checkingProgram(x) match {
          case (Nil,symbolTable) => generateAssemblerFile(generatingCode(generateIR(x, symbolTable)), fileName); sys.exit(0)
          case (head :: next, _) => semanticErrorHandler(head._1, head._2, filepath); sys.exit(200)
        }
        case Failure(msg) => {
          println(msg)
          sys.exit(100)
        }
      }
    }
    // println(Parser.parser.parse(fileContents))
  }
}

