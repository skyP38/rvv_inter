package lambda

import fastparse.*, SingleLineWhitespace.*

object LambdaParser {
  def variable[$: P]: P[Var] = {
    P( CharIn("a-z").! ).map { name =>
      Var(name)
    }
  }
  
  def abstraction[$: P]: P[Abs] = {
    P( ("λ" | "\\") ~/ variable ~ "." ~ expr ).map { case (v, e) =>
      Abs(v, e)
    }
  }
  
  def application[$: P]: P[LambdaExpr] = {
    P( atom ~ (atom | "(" ~ expr ~ ")").rep ).map { case (head, tail) =>
      val result = tail.foldLeft(head)(App(_, _))
      result
    }
  }
  
  def atom[$: P]: P[LambdaExpr] = {
    P( variable | "(" ~ expr ~ ")" ).map { expr =>
      expr
    }
  }
  
  def expr[$: P]: P[LambdaExpr] = {
    P( abstraction | application ).map { expr =>
      expr
    }
  }
  
  def parse(input: String): Either[String, LambdaExpr] = {
    fastparse.parse(input, expr(_)) match {
      case Parsed.Success(result, _) =>
        Right(result)
      case failure: Parsed.Failure =>
        Left(failure.msg)
    }
  }
}