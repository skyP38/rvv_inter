package lambda

object LambdaPrinter {
  def prettyPrint(expr: LambdaExpr): String = expr match
    case Var(name) => name
    case Abs(param, body) => s"λ${param.name}.${prettyPrint(body)}"
    case App(fn, arg) =>
      val fnStr = fn match {
        case _: Abs => s"(${prettyPrint(fn)})"
        case _ => prettyPrint(fn)
      }
      val argStr = arg match {
        case _: App => s"(${prettyPrint(arg)})"
        case _ => prettyPrint(arg)
      }
      s"$fnStr $argStr"
  
  def prettyPrintType(ty: Type): String = Type.prettyPrint(ty)
}