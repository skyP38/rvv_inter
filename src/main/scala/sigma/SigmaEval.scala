package sigma

object SigmaEvaluator {
  private var counter = 0
  
  private def freshName(): String = {
    counter += 1
    s"x$counter"
  }

  // Свободные переменные выражения
  def freeVars(expr: SigmaExpr): Set[String] = expr match {
    case Var(name) => Set(name)
    case Sigma(param, body) => freeVars(body) - param
    case Pair(first, second) => freeVars(first) ++ freeVars(second)
    case First(expr) => freeVars(expr)
    case Second(expr) => freeVars(expr)
    case App(fn, arg) => freeVars(fn) ++ freeVars(arg)
  }
  
  
  // Подстановка [N/x]M
  def substitute(expr: SigmaExpr, x: String, arg: SigmaExpr, bound: Set[String] = Set.empty): SigmaExpr = {
    
   def alphaConvert(e: SigmaExpr, toRename: String, newName: String): SigmaExpr = e match {
      case Var(name) if name == toRename => Var(newName)
      case Var(name) => Var(name)
      case Sigma(param, body) =>
        if (param == toRename) Sigma(param, body)
        else Sigma(param, alphaConvert(body, toRename, newName))
      case Pair(fst, snd) =>
        Pair(alphaConvert(fst, toRename, newName), alphaConvert(snd, toRename, newName))
      case First(expr) =>
        First(alphaConvert(expr, toRename, newName))
      case Second(expr) =>
        Second(alphaConvert(expr, toRename, newName))
      case App(fn, argExpr) =>
        App(alphaConvert(fn, toRename, newName), alphaConvert(argExpr, toRename, newName))
    }

    expr match {
      case Var(name) => 
        if (name == x && !bound.contains(name)) 
          arg 
        else 
          expr

      case Sigma(param, body) =>
        if (param == x) {
          expr
        } else {
          val fvArg = freeVars(arg)
          if (fvArg.contains(param)) {
            val newName = freshName()
            val newBody = alphaConvert(body, param, newName)
            Sigma(newName, substitute(newBody, x, arg, bound + newName))
          } else {
            Sigma(param, substitute(body, x, arg, bound + param))
          }
        }

      case Pair(first, second) =>
        Pair(
          substitute(first, x, arg, bound),
          substitute(second, x, arg, bound)
        )
        
      case First(expr) =>
        First(substitute(expr, x, arg, bound))
        
      case Second(expr) =>
        Second(substitute(expr, x, arg, bound))

      case App(fn, argExpr) =>
        App(
          substitute(fn, x, arg, bound),
          substitute(argExpr, x, arg, bound)
        )
    }
  }

  // Один шаг редукции
  def reduce(expr: SigmaExpr): Option[SigmaExpr] = expr match {
    case App(Sigma(param, body), arg) => 
      Some(substitute(body, param, arg))

    // Редукция для пар
    case First(Pair(first, _)) => Some(first)
    case Second(Pair(_, second)) => Some(second)

    // Рекурсивные случаи
    case Sigma(param, body) =>
      reduce(body).map(reduced => Sigma(param, reduced))
      
    case Pair(first, second) =>
      reduce(first).map(reduced => Pair(reduced, second))
        .orElse(reduce(second).map(reduced => Pair(first, reduced)))
        
    case First(expr) =>
      reduce(expr).map(reduced => First(reduced))
      
    case Second(expr) =>
      reduce(expr).map(reduced => Second(reduced))
    
    case App(fn, arg) =>
      reduce(fn).map(reduced => App(reduced, arg))
        .orElse(reduce(arg).map(reduced => App(fn, reduced)))
    
    case _ => None
  }
 
  // Нормальная форма (полная редукция)
  def evaluate(expr: SigmaExpr): SigmaExpr = {
    reduce(expr) match {
      case Some(reduced) => evaluate(reduced)
      case None => expr
    }
  }
}