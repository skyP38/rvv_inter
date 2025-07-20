package sigma

// Определяем типы для лямбда-выражений
sealed trait SigmaExpr

// Переменная (Variable)
case class Var(name: String) extends SigmaExpr

// Сигма-абстракция (Sigma abstraction) - σx.M
case class Sigma(param: String, body: SigmaExpr) extends SigmaExpr

// Пара (Pair) - (M, N)
case class Pair(first: SigmaExpr, second: SigmaExpr) extends SigmaExpr

// Первая проекция (First projection) - π₁M
case class First(expr: SigmaExpr) extends SigmaExpr

// Вторая проекция (Second projection) - π₂M
case class Second(expr: SigmaExpr) extends SigmaExpr

// Применение (Application) - M N
case class App(fn: SigmaExpr, arg: SigmaExpr) extends SigmaExpr