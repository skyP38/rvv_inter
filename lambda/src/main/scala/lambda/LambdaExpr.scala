package lambda

// Определяем типы для лямбда-выражений
sealed trait LambdaExpr

// Переменная (Variable)
case class Var(name: String) extends LambdaExpr

// Абстракция (Abstraction) - λx.M
case class Abs(param: Var, body: LambdaExpr) extends LambdaExpr

// Применение (Application) - M N
case class App(fn: LambdaExpr, arg: LambdaExpr) extends LambdaExpr