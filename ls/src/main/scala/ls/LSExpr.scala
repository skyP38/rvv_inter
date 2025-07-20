package LS

// Определяем типы для лямбда-выражений
sealed trait LSExpr

// Переменная (Variable)
case class Var(name: String) extends LSExpr

// Абстракция (Abstraction) - λx.M
case class Abs(param: Var, body: LSExpr) extends LSExpr

// Применение (Application) - M N
case class App(fn: LSExpr, arg: LSExpr) extends LSExpr

// Кортеж (Tuple) - <M1, M2, ..., Mn>
case class Tuple(elements: List[LSExpr]) extends LSExpr

// Проекция (Projection) - π_i(M) - проекция i-го элемента кортежа
case class Proj(index: Int, tuple: LSExpr) extends LSExpr