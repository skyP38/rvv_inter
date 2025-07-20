package LS

object LSREPL {
  def main(args: Array[String]): Unit = {
    if (args.contains("--test")) {
      LSTests.runTests()
      System.exit(0)
    } else {
      runInterpreter()
    }
  }

  private def runInterpreter(): Unit = {
      println("LS Calculus Interpreter")
      println("Enter expressions or :q to quit")
      
      var continue = true
      while (continue) {
        print("σ> ")
        val input = scala.io.StdIn.readLine()
        
        if (input == ":q") {
          continue = false
        } else {
          LSParser.parse(input) match {
            case Right(expr) =>
              val result = LSEvaluator.evaluate(expr)
              println(LSPrinter.prettyPrint(result))
            case Left(error) =>
              println(s"Parse error: $error")
          }
        }
      }
    
  }
}