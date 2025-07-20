package lambda

object LambdaEvaluator {
  private var counter = 0
  
  private def freshName(): String = {
    counter += 1
    s"x$counter"
  }
  
  // Свободные переменные выражения
  def freeVars(expr: LambdaExpr): Set[String] = expr match {
    case Var(name) => Set(name)
    case Abs(param, body) => freeVars(body) - param.name
    case App(fn, arg) => freeVars(fn) ++ freeVars(arg)
  }
  
  // Подстановка [N/x]M
  def substitute(expr: LambdaExpr, x: String, arg: LambdaExpr, bound: Set[String] = Set.empty): LambdaExpr = {

    def alphaConvert(e: LambdaExpr, toRename: String, newName: String): LambdaExpr = e match {
      case Var(name) if name == toRename => Var(newName)
      case Var(name) => Var(name)
      case Abs(param, body) =>
        if (param.name == toRename) Abs(param, body)
        else Abs(param, alphaConvert(body, toRename, newName))
      case App(fn, arg) =>
        App(alphaConvert(fn, toRename, newName), alphaConvert(arg, toRename, newName))
    }
    
    expr match {
      case Var(name) => 
        if (name == x && !bound.contains(name)) 
          arg 
        else 
          expr

      case Abs(param, body) =>
        if (param.name == x) {
          expr
        } else {
          val fvArg = freeVars(arg)
          if (fvArg.contains(param.name)) {
            val newName = freshName()

            val newBody = alphaConvert(body, param.name, newName)

            Abs(Var(newName), substitute(newBody, x, arg, bound + newName))
          } else {
            Abs(param, substitute(body, x, arg, bound + param.name))
          }
        }

      case App(fn, argExpr) =>
        App(
          substitute(fn, x, arg, bound),
          substitute(argExpr, x, arg, bound)
        )
    }
  }

  // Один шаг β-редукции
  def betaReduce(expr: LambdaExpr): Option[LambdaExpr] = expr match {
    case App(Abs(param, body), arg) => 
      Some(substitute(body, param.name, arg))

    case Abs(param, body) =>
      betaReduce(body).map(reduced => Abs(param, reduced))

    case App(fn, arg) =>
      betaReduce(fn).map(reduced => App(reduced, arg))
      .orElse(betaReduce(arg).map(reduced => App(fn, reduced)))
    
    case _ => None
  }
  
  // Нормальная форма (полная редукция)
  def evaluate(expr: LambdaExpr): LambdaExpr = {
    betaReduce(expr) match {
      case Some(reduced) => evaluate(reduced)
      case None => expr
    }
  }
}

