package sigma

object SigmaTests {
  def runTests(): Unit = {
    println("Running Sigma Calculus Tests")
    println("=" * 30)

    testParser()

    testEvaluation()
    

    println("=" * 30)
    println("Tests completed")
  }

  private def testParser(): Unit = {
    println("\n[Parser Tests]")
    val testCases = List(
      ("x", "Var(x)"),
      ("x y", "App(Var(x),Var(y))"),
      ("σx.x", "Sigma(x,Var(x))"),
      ("(x, y)", "Pair(Var(x),Var(y))"),
      ("π₁(x,y)", "First(Pair(Var(x),Var(y)))"),
      ("σx.σy.x y", "Sigma(x,Sigma(y,App(Var(x),Var(y))))"),
      ("(σx.x) y", "App(Sigma(x,Var(x)),Var(y))"),
      ("(σx.(x, x)) z", "App(Sigma(x,Pair(Var(x),Var(x))),Var(z))"),
      ("π₁((σx.(x, y)) z)", "First(App(Sigma(x,Pair(Var(x),Var(y))),Var(z)))"),
      ("(σf.σx.π₂(f x)) (σa.(a, b)) c", 
       "App(App(Sigma(f,Sigma(x,Second(App(Var(f),Var(x))))),Sigma(a,Pair(Var(a),Var(b)))),Var(c))")
    )
    
    testCases.foreach { case (input, expected) =>
      print(s"Testing parser '$input'... ")
      SigmaParser.parse(input) match {
        case Right(expr) =>
          val actual = expr.toString
          if (actual == expected) {
            println("✓")
          } else {
            println(s"✗ (got '$actual', expected '$expected')")
          }
        case Left(err) =>
          println(s"✗ Parse error: $err")
      }
    }
  }

   private def testEvaluation(): Unit = {
    println("\n[Evaluation Tests]")
    val testCases = List(
      // Базовые примеры
      "x" -> "x",
      "σx.x" -> "σx.x",
      "(σx.x) y" -> "y",
      
      // Работа с парами
      "π₁(x, y)" -> "x",
      "π₂(x, y)" -> "y",
      "(σx.(x, x)) z" -> "(z, z)",
      
      // Комбинированные примеры
      "π₁((σx.(x, y)) z)" -> "z",
      "(σf.σx.π₂(f x)) (σa.(a, b)) c" -> "b",
      
      // α-конверсия
      "(σx.σy.x) y" -> "σx1.y",
      "(σx.σy.x y) y" -> "σx2.y x2"
    )

    testCases.foreach { case (input, expected) =>
      print(s"Testing '$input'... ")
      SigmaParser.parse(input) match {
        case Right(expr) =>
          println(s"\nParsed AST: $expr")
          val result = SigmaEvaluator.evaluate(expr)
          val resultStr = SigmaPrinter.prettyPrint(result)
          if (resultStr == expected) {
            println("✓")
          } else {
            println(s"✗ (got '$resultStr', expected '$expected')")
          }
        case Left(error) =>
          println(s"✗ Parse error: $error")
      }
    }
   }  
}