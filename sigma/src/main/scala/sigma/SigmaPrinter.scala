package sigma

object SigmaPrinter {
  def prettyPrint(expr: SigmaExpr): String = expr match {
    case Var(name) => name
    case Sigma(param, body) => s"σ$param.${prettyPrint(body)}"
    case Pair(first, second) => s"(${prettyPrint(first)}, ${prettyPrint(second)})"
    case First(expr) => s"π₁${prettyPrint(expr)}"
    case Second(expr) => s"π₂${prettyPrint(expr)}"
    case App(fn, arg) =>
      val fnStr = fn match {
        case _: Sigma => s"(${prettyPrint(fn)})"
        case _ => prettyPrint(fn)
      }
      val argStr = arg match {
        case _: App => s"(${prettyPrint(arg)})"
        case _ => prettyPrint(arg)
      }
      s"$fnStr $argStr"
  }
}