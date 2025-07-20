package lambda

object LambdaTests {
  def runTests(): Unit = {
    println("Running Lambda Calculus Tests")
    println("=" * 30)

    testParser()

    testEvaluation()
    testTypeInference()

    println("=" * 30)
    println("Tests completed")
  }

  private def testParser(): Unit = {
    println("\n[Parser Tests]")
    val parserCases = List(
      "x"     -> "Var(x)",
      "x y"   -> "App(Var(x),Var(y))",
      "x y z" -> "App(App(Var(x),Var(y)),Var(z))",
      "λx.x"  -> "Abs(Var(x),Var(x))",
      "(λx.x y) z" -> "App(Abs(Var(x),App(Var(x),Var(y))),Var(z))",
      "λx.λy.x y"  -> "Abs(Var(x),Abs(Var(y),App(Var(x),Var(y))))"
    )
    
    parserCases.foreach { case (input, expected) =>
      print(s"Testing parser '$input'... ")
      LambdaParser.parse(input) match {
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
      "λx.x" -> "λx.x",
      "(λx.x) y" -> "y",
      
      // Подстановка
      "(λx.x y) z" -> "z y",
      "(λx.λy.x y) a" -> "λy.a y",
      
      // α-конверсия (избегание конфликта имен)
      "(λx.λy.x) y" -> "λx1.y",
      "(λx.λy.x y) y" -> "λx2.y x2",
      
      // Множественные аппликации
      "(λx.λy.x y) a b" -> "a b",
      "(λf.λx.f (f x)) (λz.z) w" -> "w",
      
      // Тест на нормальный порядок редукции
      "(λx.y) ((λx.x x) (λx.x x))" -> "y",
      
    )

    testCases.foreach { case (input, expected) =>
      print(s"Testing '$input'... ")
      LambdaParser.parse(input) match {
        case Right(expr) =>
          println(s"\nParsed AST: $expr")
          val result = LambdaEvaluator.evaluate(expr)
          val resultStr = LambdaPrinter.prettyPrint(result)
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

  private def testTypeInference(): Unit = {
    println("\n[Type Inference Tests]")

    // Функция для структурного сравнения типов
    def typeStructureEqual(actual: Type, expected: Type): Boolean = {
      (actual, expected) match {
        case (TyVar(_), TyVar(_)) => true
        case (TyArrow(a1, a2), TyArrow(b1, b2)) => 
          typeStructureEqual(a1, b1) && typeStructureEqual(a2, b2)
        case _ => false
      }
    }

    val testCases = List(
      ("λx.x", "λx.x"),
      ("λx.λy.x", "λx.λy.x"),
      ("λf.λx.f (f x)", "λf.λx.f (f x)"),
      ("0", "λf.λx.x"),
      ("1", "λf.λx.f x"),
      ("2", "λf.λx.f (f x)")
    )
    testCases.foreach { case (input, expectedPattern) =>
      print(s"Testing type of '$input'... ")
      
        val inputType = for {
          expr <- LambdaParser.parse(input)
          typ <- TypeInference.infer(expr)
        } yield typ

        val expectedType = for {
          expr <- LambdaParser.parse(expectedPattern)
          typ <- TypeInference.infer(expr)
        } yield typ

        (inputType, expectedType) match {
          case (Right(inputTy), Right(expectedTy)) =>
            if (typeStructureEqual(inputTy, expectedTy)) {
              println("✓")
            } else {
              println(s"✗ (got '${LambdaPrinter.prettyPrintType(inputTy)}', " +
                    s"expected '${LambdaPrinter.prettyPrintType(expectedTy)}')")
            }
            
          case (Left(err), _) => println(s"✗ Error in input: $err")
          case (_, Left(err)) => println(s"✗ Error in pattern: $err")
        }
    }
  }
}