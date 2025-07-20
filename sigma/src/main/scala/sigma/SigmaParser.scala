package sigma

import fastparse.*, SingleLineWhitespace.*

object SigmaParser {
  def variable[$: P]: P[Var] = P( CharIn("a-z").! ).map(Var.apply)

  def parens[$: P]: P[SigmaExpr] = P( "(" ~ expr ~ ")" )

  def atom[$: P]: P[SigmaExpr] = P( 
    variable | 
    pair | 
    projection | 
    parens
  )

  def pair[$: P]: P[Pair] = {
    P( "(" ~ expr ~ "," ~ expr ~ ")" ).map(Pair.apply)
  }

  def projection[$: P]: P[SigmaExpr] = {
    P( ("π₁" | "fst") ~/ expr ).map(First.apply) |
    P( ("π₂" | "snd") ~/ expr ).map(Second.apply)
  }
  
  def abstraction[$: P]: P[Sigma] = {
    P( ("σ" | "s") ~/ variable ~ "." ~ expr ).map { 
      case (Var(param), e) => Sigma(param, e)
    }
  }
  
  def application[$: P]: P[SigmaExpr] = {
    // Выражение в скобках или атомарное
    def appAtom[$: P]: P[SigmaExpr] = P( parens | atom )
    
    // Цепочка применений
    P( (abstraction | appAtom) ~ (appAtom).rep ).map {
      case (head, tail) => tail.foldLeft(head)(App.apply)
    }
  }
  
  def expr[$: P]: P[SigmaExpr] = {
    P( abstraction | application | atom )
  }
  
  def parse(input: String): Either[String, SigmaExpr] = {
    fastparse.parse(input, expr(_)) match {
      case Parsed.Success(result, _) =>
        Right(result)
      case failure: Parsed.Failure =>
        Left(failure.msg)
    }
  }
}