package lambda

object LambdaREPL {
  def main(args: Array[String]): Unit = {
    if (args.contains("--test")) {
      LambdaTests.runTests()
      System.exit(0)
    } else {
      runInterpreter()
    }
  }

  private def runInterpreter(): Unit = {
      println("Lambda Calculus Interpreter")
      println("Enter expressions or :q to quit")
      
      var continue = true
      while (continue) {
        print("λ> ")
        val input = scala.io.StdIn.readLine()
        
        if (input == ":q") {
          continue = false
        } else {
          LambdaParser.parse(input) match {
            case Right(expr) =>
              val result = LambdaEvaluator.evaluate(expr)
              println(LambdaPrinter.prettyPrint(result))
            case Left(error) =>
              println(s"Parse error: $error")
          }
        }
      }
    
  }
}