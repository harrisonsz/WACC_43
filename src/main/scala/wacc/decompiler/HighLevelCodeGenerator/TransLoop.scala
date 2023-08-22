package wacc.decompiler.HighLevelCodeGenerator

object TransLoop {
    import wacc.decompiler.CFgenerate.FlowBlocks._
    import wacc.decompiler.L2IR._
    import wacc.decompiler.HighLevelCodeGenerator.TransCFBlock.transCFBlock
    import wacc.decompiler.HighLevelCodeGenerator.TransExp.transBoolExp
    import wacc.decompiler.HighLevelCodeGenerator.HighLevelCodeGen.addingDelimiterForCodesInAScope


    def transLoop(condBlock: CFBlock): List[String] = {
        // condBlock is always expected to be a two-way block
        val loopBodyBlock = condBlock.asInstanceOf[TwoWayBlock].nextTrue
        "while " + transBoolExp(condBlock.blockL2Ins.last.asInstanceOf[JCond].boolExp) + " do\n" +
        addingDelimiterForCodesInAScope(transCFBlock(loopBodyBlock)) + "\n" +
        "done" ::
        List.empty
    }
}
