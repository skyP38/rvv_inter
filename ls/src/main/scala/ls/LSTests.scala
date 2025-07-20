package LS

object LSTests {
  def runTests(): Unit = {
    println("Running LS Calculus Tests")
    println("=" * 30)

    testParser()

    testEvaluation()
    

    println("=" * 30)
    println("Tests completed")
  }

  private def testParser(): Unit = {
    println("\n[Parser Tests]")
    val testCases = List(
      "σx.x" -> "Abs(Var(x),Var(x))",
      "(x, y)" -> "Tuple(List(Var(x), Var(y)))",
      "π1(x,y)" -> "Proj(0,Tuple(List(Var(x), Var(y))))",
      "σx.σy.x y" -> "Abs(Var(x),Abs(Var(y),App(Var(x),Var(y))))",
      "(σx.x) y" -> "App(Abs(Var(x),Var(x)),Var(y))",
      "(σx.(x, x)) z" -> "App(Abs(Var(x),Tuple(List(Var(x), Var(x)))),Var(z))",
      "π1((σx.(x, y)) z)" -> "Proj(0,App(Abs(Var(x),Tuple(List(Var(x), Var(y)))),Var(z)))",
      "(σf.σx.π2(f x)) (σa.(a, b)) c" -> "App(App(Abs(Var(f),Abs(Var(x),Proj(1,App(Var(f),Var(x))))),Abs(Var(a),Tuple(List(Var(a), Var(b))))),Var(c))"
    )
    
    testCases.foreach { case (input, expected) =>
      print(s"Testing parser '$input'... ")
      LSParser.parse(input) match {
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
      "π1(x, y)" -> "x",
      "π2(x, y)" -> "y",
      "(σx.(x, x)) z" -> "(z, z)",
      
      // Комбинированные примеры
      "π1((σx.(x, y)) z)" -> "z",
      "(σf.σx.π2(f x)) (σa.(a, b)) c" -> "b",
      
      // α-конверсия
      "(σx.σy.x) y" -> "σx1.y",
      "(σx.σy.x y) y" -> "σx2.y x2",

      // Сигма-исчисление
      "(x, y)" -> "(x, y)",
      "π1(x, y)" -> "x",
      "π2(x, y)" -> "y",
      "(σx.(x, x)) y" -> "(y, y)",
      "π1((σx.(x, x)) y)" -> "y",
      "π1(π2( (a, b), (c, d) ))" -> "c",
      // Комбинированные тесты
      "(σx.σy.(x, y)) a b" -> "(a, b)",
      "π1((σx.(π2(x), π1(x))) (a, b))" -> "b"
    )

    testCases.foreach { case (input, expected) =>
      print(s"Testing '$input'... ")
      LSParser.parse(input) match {
        case Right(expr) =>
          println(s"\nParsed AST: $expr")
          val result = LSEvaluator.evaluate(expr)
          val resultStr = LSPrinter.prettyPrint(result)
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