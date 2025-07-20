package LS

import fastparse.*, SingleLineWhitespace.*

object LSParser {
  // Базовые определения
  def variable[$: P]: P[Var] = P( CharIn("a-z").! ).map(Var.apply)
  
  def parens[$: P]: P[LSExpr] = P( "(" ~ expr ~ ")" )

  // Атомарные выражения
  def atom[$: P]: P[LSExpr] = P( 
    variable | 
    tuple | 
    projection | 
    parens
  )


  def tuple[$: P]: P[Tuple] = {
    P( "(" ~ expr.rep(sep=",", min=2) ~ ")" ).map(es => Tuple(es.toList))
  }


  def projection[$: P]: P[Proj] = {
    P( ("π" ~ CharIn("1-9")).! ~/ (parens | tuple) ).map {
      case (s, e) => Proj(s.drop(1).toInt - 1, e)
    }
  }

  
  def abstraction[$: P]: P[Abs] = {
    P( ("σ" | "s") ~/ variable ~ "." ~ expr ).map { 
      case (v @ Var(param), e) => Abs(v, e) 
    }
  }
  
  // Применение
  def application[$: P]: P[LSExpr] = {
    def appAtom[$: P]: P[LSExpr] = P( 
      parens.map {
        case e: App => e  // Сохраняем применения в скобках как есть
        case e => e       // Другие выражения
      } |
      abstraction | 
      tuple | 
      projection | 
      atom 
    )

    P( appAtom ~ appAtom.rep ).map {
      case (head, tail) => tail.foldLeft(head)(App.apply)
    }
  }
  
  def expr[$: P]: P[LSExpr] = {
    P( abstraction | application | atom )
  }
  
  def parse(input: String): Either[String, LSExpr] = {
    fastparse.parse(input, expr(_)) match {
      case Parsed.Success(result, _) =>
        Right(result)
      case failure: Parsed.Failure =>
        Left(failure.msg)
    }
  }
}