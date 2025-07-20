package lambda

import scala.collection.mutable

object TypeInference {
  private var counter = 0

  def reset(): Unit = { counter = 0 }

  private def freshTyVar(): Type = {
    counter += 1
    TyVar(s"t$counter")
  }

  private def normalizeType(ty: Type): String = {
    val vars = scala.collection.mutable.Map[String, String]()
    var nextId = 1
    
    def normalize(t: Type): String = t match {
      case TyVar(name) =>
        vars.getOrElseUpdate(name, {
          val newName = s"t$counter"
          counter += 1
          newName
        })
      case TyArrow(from, to) =>
        val fromStr = from match {
          case _: TyArrow => s"(${normalize(from)})"
          case _ => normalize(from)
        }
        s"$fromStr → ${normalize(to)}"
    }

    normalize(ty)
  }

  def infer(expr: LambdaExpr, env: Map[String, Type] = Map.empty): Either[String, Type] = {
    reset()
    try {
      Right(inferHelper(expr, env))
    } catch {
      case e: Exception => Left(e.getMessage)
    }
  }

  private def inferHelper(expr: LambdaExpr, env: Map[String, Type]): Type = {
    expr match {
      case Var(name) =>
        env.getOrElse(name, throw new Exception(s"Unbound variable: $name"))
      
      case Abs(param, body) =>
        val paramTy = freshTyVar()
        val newEnv = env + (param.name -> paramTy)
        val bodyTy = inferHelper(body, newEnv)
        TyArrow(paramTy, bodyTy)
      
      case App(fn, arg) =>
        val fnTy = inferHelper(fn, env)
        val argTy = inferHelper(arg, env)
        val resTy = freshTyVar()
        unify(fnTy, TyArrow(argTy, resTy))
        resTy
    }
  }

  private def unify(t1: Type, t2: Type): Unit = (t1, t2) match {
    case (TyVar(a), ty) => unifyVar(a, ty)
    case (ty, TyVar(a)) => unifyVar(a, ty)
    case (TyArrow(a1, a2), TyArrow(b1, b2)) =>
      unify(a1, b1)
      unify(a2, b2)
    case _ => throw new Exception(s"Cannot unify $t1 with $t2")
  }

  private def unifyVar(a: String, ty: Type): Unit = {
    if (TyVar(a) != ty) {
      if (occurs(a, ty)) throw new Exception(s"Occurs check failed: $a in $ty")
      // Реализуем подстановку ty вместо TyVar(a) во всех местах
    }
  }
  
  private def occurs(a: String, ty: Type): Boolean = ty match {
    case null => false
    case TyVar(b) => a == b
    case TyArrow(from, to) => occurs(a, from) || occurs(a, to)
  }
}