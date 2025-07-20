
package lambda

sealed trait Type
case class TyVar(name: String) extends Type
case class TyArrow(arg: Type, res: Type) extends Type

object Type {
  // Преобразование типа в строку
  def prettyPrint(ty: Type): String = ty match {
    case TyVar(name) => name
    case TyArrow(arg, res) =>
      val argStr = arg match {
        case TyArrow(_, _) => s"(${prettyPrint(arg)})"
        case _ => prettyPrint(arg)
      }
      s"$argStr → ${prettyPrint(res)}"
  }
}