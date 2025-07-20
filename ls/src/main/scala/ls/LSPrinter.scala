package LS

object LSPrinter {
  def prettyPrint(expr: LSExpr): String = expr match {
    case Var(name) => name
    case Abs(param, body) => s"σ${param.name}.${prettyPrint(body)}"
    case App(fn, arg) =>
      val fnStr = fn match
        case _: Abs | _: Tuple => s"(${prettyPrint(fn)})"
        case _ => prettyPrint(fn)
      val argStr = arg match
        case _: App | _: Proj => s"(${prettyPrint(arg)})"
        case _ => prettyPrint(arg)
      s"$fnStr $argStr"
    case Tuple(elements) => elements.map(prettyPrint).mkString("(", ", ", ")")
    case Proj(i, t) => s"π$i(${prettyPrint(t)})"
  }
}