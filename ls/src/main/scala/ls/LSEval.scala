package LS

object LSEvaluator {
  private var counter = 0
  
  private def freshName(): String = {
    counter += 1
    s"x$counter"
  }

  // Свободные переменные выражения
  def freeVars(expr: LSExpr): Set[String] = expr match {
    case Var(name) => Set(name)
    case Abs(param, body) => freeVars(body) - param.name
    case App(fn, arg) => freeVars(fn) ++ freeVars(arg)
    case Tuple(elements) => elements.flatMap(freeVars).toSet
    case Proj(_, tuple) => freeVars(tuple)
  }
  
  
  // Подстановка [N/x]M
  def substitute(expr: LSExpr, x: String, arg: LSExpr, bound: Set[String] = Set.empty): LSExpr = {
    
   def alphaConvert(e: LSExpr, toRename: String, newName: String): LSExpr = e match {
      case Var(name) if name == toRename => Var(newName)
      case Var(name) => Var(name)
      case Abs(param, body) =>
        if (param.name == toRename) Abs(param, body)
        else Abs(param, alphaConvert(body, toRename, newName))
      case App(fn, arg) =>
        App(alphaConvert(fn, toRename, newName), alphaConvert(arg, toRename, newName))
      case Tuple(elements) =>
        Tuple(elements.map(alphaConvert(_, toRename, newName)))
      case Proj(i, tuple) =>
        Proj(i, alphaConvert(tuple, toRename, newName))
    }

    expr match {
      case Var(name) => 
        if (name == x && !bound.contains(name)) 
          arg 
        else 
          expr

      case Abs(param, body) =>
        if (param.name == x) expr
        else {
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
        App(substitute(fn, x, arg, bound), substitute(argExpr, x, arg, bound))
        
      case Tuple(elements) =>
        Tuple(elements.map(substitute(_, x, arg, bound)))
        
      case Proj(i, tuple) =>
        Proj(i, substitute(tuple, x, arg, bound))
    }
  }

  // Один шаг редукции
  def reduce(expr: LSExpr): Option[LSExpr] = expr match {
    case App(Abs(param, body), arg) => 
      Some(substitute(body, param.name, arg))
      
    case Proj(i, Tuple(elements)) if elements.indices.contains(i) =>
      Some(elements(i))
      
    case Abs(param, body) =>
      reduce(body).map(reduced => Abs(param, reduced))

    case App(fn, arg) =>
      reduce(fn).map(reduced => App(reduced, arg))
        .orElse(reduce(arg).map(reduced => App(fn, reduced)))
        
    case Tuple(elements) =>
      elements.zipWithIndex.collectFirst {
        case (e, i) if reduce(e).isDefined =>
          val newElements = elements.updated(i, reduce(e).get)
          Tuple(newElements)
      }
      
    case Proj(i, tuple) =>
      reduce(tuple).map(reduced => Proj(i, reduced))
    
    case _ => None
  }
 
  def evaluate(expr: LSExpr): LSExpr = {
    reduce(expr) match {
      case Some(reduced) => evaluate(reduced)
      case None => expr
    }
  }
}