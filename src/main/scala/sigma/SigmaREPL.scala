package sigma

object SigmaREPL {
  def main(args: Array[String]): Unit = {
    if (args.contains("--test")) {
      SigmaTests.runTests()
      System.exit(0)
    } else {
      runInterpreter()
    }
  }

  private def runInterpreter(): Unit = {
      println("Sigma Calculus Interpreter")
      println("Enter expressions or :q to quit")
      
      var continue = true
      while (continue) {
        print("σ> ")
        val input = scala.io.StdIn.readLine()
        
        if (input == ":q") {
          continue = false
        } else {
          SigmaParser.parse(input) match {
            case Right(expr) =>
              val result = SigmaEvaluator.evaluate(expr)
              println(SigmaPrinter.prettyPrint(result))
            case Left(error) =>
              println(s"Parse error: $error")
          }
        }
      }
    
  }
}