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
      println("Use :t <expr> to show type of expression")
      
      var continue = true
      while (continue) {
        print("λ> ")
        val input = scala.io.StdIn.readLine()
        
        if (input == ":q") {
          continue = false
        } else if (input.startsWith(":t ")) {
          val exprStr = input.drop(3).trim
          LambdaParser.parse(exprStr) match {
            case Right(expr) =>
              TypeInference.infer(expr) match {
                case Right(ty) =>
                  println(LambdaPrinter.prettyPrintType(ty))
                case Left(error) =>
                  println(s"Type error: $error")
              }
            case Left(error) =>
              println(s"Parse error: $error")
          }
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